
type encoding = Ascii | Latin1 | Utf8 | Unknown of string

let enc_utf = "UTF-8"
and enc_lat = "LATIN1"
and enc_asc = "ASCII//TRANSLIT"

let to_string = function
  | Ascii -> "ascii"
  | Latin1 -> "latin1"
  | Utf8 -> "utf8"
  | Unknown s -> "Unknown " ^ s

(* Helper functions *)
let is_utf8 s = Text.check s = None

(* Find if the given char is Ascii or Latin1
 * (Only characters commonly used in the french language.) *)
let whatis_char = function
  | '\x00' | '\t' | '\n' | '\r' | '\x20' .. '\x7e' -> Ascii

  (* Non-break space *)
  | '\xa0'

  (* ° *)
  | '\xb0' 

  (* À, Â, Ä, Ç, È É Ê Ë *)
  | '\xc0' | '\xc2' | '\xc4' | '\xc7' | '\xc8' .. '\xcb' 

  (* Î Ô Ö Ù Ü *)
  | '\xce' |  '\xd4' | '\xd6' | '\xd9' | '\xdc'

  (* à â ä *)
  | '\xe0' | '\xe2' | '\xe4'

  (* æ ç è é ê ë *)
  | '\xe6' .. '\xeb' 

  (* î ï ô ö ù û *)
  | '\xee' | '\xef' | '\xf4' | '\xf6' | '\xf9' | '\xfb' -> Latin1

  | _ -> Unknown ""

let s_appears_at s pos len v =
  let vlen = String.length v in
  if pos + vlen > len then false
  else String.sub s pos vlen = v

(* Odd sequences of latin1 characters which are valid but unusual, hence probably UTF8. *)
let latin1_odd_values = [ "\xc2\xa0" ; ] 

let find_oddness s pos len st = match st with
  | `Ascii | `Latin1 -> if List.exists (fun v -> s_appears_at s pos len v) latin1_odd_values then `OddLatin1 else st
  | `OddLatin1 -> st
  | `Unknown -> assert false

(* Iterate over a string s, starting at pos.
 * Invariant : st is Ascii or Latin1 *)
let rec whatis_aux s st pos len =
  if pos >= len then st
  else
    let st' = find_oddness s pos len st in
    
    match (st', whatis_char s.[pos]) with
    | (`Ascii, Ascii) -> whatis_aux s `Ascii (pos+1) len
    | (`Ascii, Latin1) -> whatis_aux s `Latin1 (pos+1) len
    | (`Latin1, (Ascii | Latin1)) -> whatis_aux s `Latin1 (pos+1) len
    | (`OddLatin1, (Ascii | Latin1)) -> whatis_aux s `OddLatin1 (pos+1) len
    | ((`Ascii | `Latin1 | `OddLatin1), Unknown "") -> `Unknown
    | _ -> assert false

(* Test the given string vs Ascii or latin1. *)
let whatis s = whatis_aux s `Ascii 0 (String.length s)

let is_valid encoding s =
  match encoding with
  | Utf8 -> is_utf8 s
  | Ascii -> whatis s = `Ascii
  | Unknown e -> failwith ("Transcode.is_valid: unknown encoding = " ^ e)
  | Latin1 -> 
    begin match whatis s with
      | `Ascii | `Latin1 | `OddLatin1 -> true
      | `Unknown -> false
    end

let unknown_any = Unknown "Cannot determine this encoding."

(*
let details s =
  String.iteri (fun i c -> match whatis_char c with Latin1 -> Printf.printf "Latin1 character at pos #%d: '%c' (char #%d) \n" i c (Char.code c) | _ -> ()) s
*)
    
let guess_encoding ?(warnings=Pipe.stdout) s =
  
  match (whatis s, is_utf8 s) with
  | `Ascii, true -> Ascii
  | `Ascii, false -> assert false (* Ascii is a subset of UTF8 *)

  | (`Latin1 | `OddLatin1), false -> Latin1
  | `Latin1, true -> Pipe.send warnings (Printf.sprintf "Unknown_both = '%s' (choosing UTF).\n%!" s) ; Utf8
  | `OddLatin1, true -> Utf8
    
  | `Unknown, false -> Pipe.send warnings (Printf.sprintf "Unknown any = '%s'\n%!" s) ; unknown_any
  | `Unknown, true -> Utf8


(* Guess a file's encoding. *)
type state =
  { latin1_lines: int ;
    utf_lines: int ;
    current_encoding: encoding }

let init_state =
  { latin1_lines = 0 ;
    utf_lines = 0 ;
    current_encoding = Ascii }

(* Current encoding -> next encoding, according to the current line's encoding. *)
let next_state warn st linenb lineenc =

  let enc = st.current_encoding in

  let next_encoding =
    match (enc, lineenc) with
    | (Unknown _, _) -> enc
    | (_, Unknown s) ->
      warn (Printf.sprintf "*** Warning: line %d, unknown encoding (%s)" linenb s) ;
      enc

    | (Ascii, k) -> k
    | (k, Ascii) -> k

    | (Latin1, Latin1) -> enc
    | (Latin1, Utf8) ->
      warn (Printf.sprintf "*** Warning: line %d, contains Utf characters, although the file was Latin-1 until now." linenb) ;
      Unknown "Mixed latin1 and utf."

    | (Utf8, Latin1) ->
      warn (Printf.sprintf "*** Warning: line %d, contains latin1 characters, although the file was utf until now." linenb) ;
      Unknown "Mixed utf and latin1."
        
    | (Utf8, Utf8) -> Utf8
  in

  { latin1_lines = st.latin1_lines + (if lineenc = Latin1 then 1 else 0) ;
    utf_lines = st.utf_lines + (if lineenc = Utf8 then 1 else 0) ;
    current_encoding = next_encoding }

let guess_file_encoding ?(warnings=Pipe.stdout) filename =
  let warn s = Pipe.send warnings s in
  
  let%lwt state =
    Lwtfile.fold_file ~file:filename ~rm_empty_lines:true
      (fun linenb line state -> Lwt.wrap4 next_state warn state linenb (guess_encoding ~warnings line))
      init_state
  in
  Lwt.return
    (if state.latin1_lines = 0 && state.utf_lines = 0 then Ascii
     else if state.latin1_lines > state.utf_lines then Latin1
     else Utf8)


let to_ascii encoding s =

  match encoding with
  | Ascii -> s
  | Unknown _ -> failwith "Unknown source encoding"
  | Latin1 -> Encoding.recode_string ~src:enc_lat ~dst:enc_asc s
  | Utf8 -> Encoding.recode_string ~src:enc_utf ~dst:enc_asc s

let to_utf encoding s =

  match encoding with
  | Ascii -> s
  | Unknown _ -> failwith "Unknown source encoding"
  | Latin1 -> Encoding.recode_string ~src:enc_lat ~dst:enc_utf s
  | Utf8 -> s

let from_latin1 encoding s =

  match encoding with
  | Ascii -> Encoding.recode_string ~src:enc_lat ~dst:enc_asc s 
  | Unknown _ -> failwith "Unknown source encoding"
  | Latin1 -> s
  | Utf8 -> Encoding.recode_string ~src:enc_lat ~dst:enc_utf s 

let from_utf encoding s =

  match encoding with
  | Ascii -> Encoding.recode_string ~src:enc_utf ~dst:enc_asc s 
  | Unknown _ -> failwith "Unknown source encoding"
  | Latin1 -> Encoding.recode_string ~src:enc_utf ~dst:enc_lat s
  | Utf8 -> s

let utf_to_ascii s = to_ascii Utf8 s
