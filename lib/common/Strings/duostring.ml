type utf = string
type ascii = string

let my_to_ascii s =
  Text.(to_ascii (map
                    (function
                      (* | "ø" -> "oe" *)
                      | "°" -> "o"
                      | c -> c
                    ) s))
                                     

(* Find the rotated candidate with min value, by permuting around spaces. *)
let rec get_min minval len n pos =
  try 
    (* Find next space *)
    let pos = String.index_from n pos ' ' in

    (* Flip around this point *)
    let left = String.sub n 0 pos
    and right = String.sub n (pos+1) (len - pos - 1)
    in

    let candidate = right ^ " " ^ left in    
    get_min (min candidate minval) len n (pos+1)

  with 
  | Not_found -> minval
  | _ -> assert false

let rotated_canonical n =
  assert Transcode.(is_valid Ascii n) ;
  let len = String.length n in
  if len > 0 then get_min n len n 0 else n

let string_score s =
  let sum = ref 0 in
  String.iter (fun c -> sum := !sum + Char.code c) s ;
  !sum

(* let eq_utf s1 s2 = 0 = Text.compare s1 s2 *)

(* Capitalize all words *)
let rec capit_aux s in_word pos len acu =
  if pos >= len then acu
  else
    let char = Text.get s pos in
    let alnum = Text.is_alnum char in
    let acu' =
      if (not in_word) && alnum then
        Text.splice acu pos (pos+1) (Text.capitalize char)
      else acu
    in
    capit_aux s alnum (pos+1) len acu'

let capit s = capit_aux s false 0 (Text.length s) s

(* Remove some punctuation. *)
let remove_punct s =
  let map_char = function
    | "'" -> " "
    | "-" -> " "
    | s -> s
  in
  Text.map map_char s

let normalize s =

  if not Transcode.(is_valid Utf8 s) then failwith "Duostring.normalize" ;

  (* Lowercase *)
  let s = Text.lower s in

  (* Strip & remove multiple spaces. *)
  let s = Text.strip s in

  (* I wish I could use something better to squish spaces. *)
  let filter_char =
    (* Indicate if the next space we find should be kept. *)
    let keep_space = ref false in
    fun c ->
      match (Text.is_space c, !keep_space) with
      | false, false -> keep_space := true ; true
      | false, true -> true
      | true, false -> false
      | true, true -> keep_space := false ; true
  in

  let s = Text.filter filter_char s in

  (* Ad-hoc case *)
  (* let s = Text.replace s ~patt:"--" ~repl:"-" in *) (* Attention dans les logins. *)
  s


type duostring =
  { utf: string ;
    ascii: string }

type duo = duostring

(* Invariant : ascii is the projection of utf. *)
let check duo = duo

(* Beware: æ => ae  not the same length. *)
(*
  if String.length duo.ascii = Text.length duo.utf then duo
  else
    (* More precise test. *)
    let ascii = Text.(filter is_alnum) duo.ascii
    and utf   = Text.(filter is_alnum) duo.utf in

    if String.length ascii = Text.length utf then duo
      else
      begin
        Printf.printf "Duostring inconsistency: ascii = '%s'  utf = '%s'\n%!" duo.ascii duo.utf ;
        assert false
      end
*)
                
let emptyduo =
  { utf = "" ;
    ascii = "" }

let is_empty s = s.ascii = ""

let mkduo s = 
  assert Transcode.(is_valid Utf8 s) ;

  let utf = normalize s in

  (* Normalize a second time, to squish spaces. *)
  let ascii = normalize (remove_punct (my_to_ascii utf)) in

  (* Share strings if identical. *)
  let utf = if ascii = utf then ascii else utf in

  check { utf ; ascii }

let (^^^) d1 d2 =
  check { utf = d1.utf ^ d2.utf ;
          ascii = d1.ascii ^ d2.ascii }

let rec concat ~sep_when_empty ~sep = function
  | [] -> emptyduo
  | [x] -> x
  | x :: xs ->
    let tail = concat ~sep_when_empty ~sep xs in
    if is_empty x && not sep_when_empty then tail
    else x ^^^ sep ^^^ tail

let space =
  let sp = " " in
  { utf = sp ;
    ascii = sp }

let len s = String.length s.ascii
let get_one_ascii s = s.ascii
let get_all_ascii s = [ s.ascii ]
let get_utf s = s.utf
let get_canonical s = [ rotated_canonical s.ascii ]

let capitalize n =
  check { utf = capit n.utf ;
          ascii = capit n.ascii }

let core_merge d1 d2 =
  let _ = check d1
  and _ = check d2 in

  (* Same ascii *)
  if d1.ascii = d2.ascii then
    if string_score d1.utf > string_score d2.utf then Some d1 else Some d2        
  else None

(* Given an ascii position, find the corresponding utf position *)   
let find_utf_pos d pos =
  let rec loop apos upos p =
    if apos >= pos then upos
    else
      match Text.next p with
      | None -> upos
      | Some (uchar, p2) ->
        let delta = String.length (my_to_ascii uchar) in
        assert (delta > 0) ;
        loop (apos+delta) (upos+1) p2
  in
  loop 0 0 (Text.pointer_l d.utf)

let sub d pos len =

  let alen = String.length d.ascii
  and ulen = Text.length d.utf in

  (* ulen may be <= ascii : utf'ae' -> ascii 'a'.'e'   
   *  but we assume no utf character can be longer than its ascii representation.
   *  not sure, however: utf ' + e = é ?
  *)
  if (alen < ulen) then Printf.printf "Curious : utf'%s' is longer than ascii'%s'.\n%!" d.utf d.ascii ;
  assert (alen >= ulen) ;
  (* If this assert fails, it might be because of --   => replace -- by - when building the utf string. *)

  let (pos2, len2) =
    if alen = ulen then (pos, len)
    else
      (* Find the gap *)
      let pos2 = find_utf_pos d pos
      and pos3 = find_utf_pos d (pos+len-1) in
      (pos2, pos3 - pos2 + 1)
  in
  
  check { utf = Text.sub d.utf pos2 len2 ;
          ascii = String.sub d.ascii pos len }

let merge_with_prefix d1 d2 =
  try
    match core_merge d1 d2 with
    | Some v -> Some (v, emptyduo)
    | None ->
      let len1 = String.length d1.ascii
      and len2 = String.length d2.ascii in

      (* Ensure d1 is longer than d2 *)
      let (d1, d2, len1, len2) = if len1 > len2 then (d1, d2, len1, len2) else (d2, d1, len2, len1) in

      if Common.starts_with d2.ascii d1.ascii then
        (* d2 is a strict prefix of d1. We can merge. *)

        (* Scores of the prefix. *)        
        let utfprefix1 = Text.sub d1.utf 0 len2 in

        let score_prefix1 = string_score utfprefix1
        and score_prefix2 = string_score d2.utf in

        let utfprefix = if score_prefix1 > score_prefix2 then utfprefix1 else d2.utf in

        (* The rest *)
        let p1 = check { utf = utfprefix ;
                         ascii = d2.ascii }
        
        and p2 = sub d1 (len2+1) (len1 - len2 - 1) in

        (* Check that it was cut on a space. *)
        match d1.ascii.[len2] with
        | ' ' -> Some (p1, p2)
        | _ -> None (* Cannot cut if this is not a space. *)

      else None
        
  with e -> Printf.printf "*Unexpected exception* in Duostring.merge_with_prefix: %s\n%!" (Printexc.to_string e) ; None

let merge d1 d2 =
  match merge_with_prefix d1 d2 with
  | Some (c,e1) -> Some (c,e1,emptyduo)
  | None ->
    begin match merge_with_prefix d2 d1 with
      | None -> None
      | Some (c,e2) -> Some (c,emptyduo,e2)
    end

(* Applies f to all decompositions of s such that s = s1 + ' ' + s2  with s1 and s2 non empty.
 * f acu s1 s2 
 * (tested) *)
let fold_on_spaces s acu f =

  let asc = get_one_ascii s in
  let slen = String.length asc in
  
  let rec loop i acu =
    if i >= slen then acu
    else
      try
        let pos = String.index_from asc i ' ' in
        let sub1 = sub s 0 pos
        and sub2 = sub s (pos+1) (slen - pos - 1) in
        
        loop (pos+1) (f acu sub1 sub2)
      with Not_found -> acu
  in
  loop 0 acu

let cuts duo = fold_on_spaces duo [] (fun acu s1 s2 -> (s1,s2) :: acu)

(*
let equalsduo s1 s2 = s1.ascii = s2.ascii

let equals_or_empty s1 s2 = is_empty s1 || is_empty s2 || equalsduo s1 s2

(* Indicate if character c1 is the ascii version of utf character s. *)
let alike c1 s =
  let a = Text.to_ascii s in
  assert (String.length a = 1) ;

  if c1 = a.[0] then true
  else
    match c1, a.[0] with
    | ' ', '-' -> true
    | ' ', '\'' -> true
    | _ -> false

let cut ~prefix s =

  assert (Common.starts_with prefix s.ascii) ;

  let len = String.length prefix in

  (* pos1 = ascii, pos2 = utf *)
  let rec go pos1 pos2 =
    if pos1 = len then pos2
    else
      (* Look for the first utf character which corresponds. *)
    if alike s.ascii.[pos1] (Text.get s.utf pos2) then go (pos1+1) (pos2+1)
    else go pos1 (pos2+1)
  in

  let utf_len = go 0 0 in

  let rest = Text.sub s.utf utf_len (Text.length s.utf - utf_len) in

  let left =
    { ascii = prefix ;
      utf   = Text.sub s.utf 0 utf_len }

  and right = mkduo rest 
  in

  (left, right)

let find ?(pos=0) ~search s =

  assert Transcode.(is_valid Ascii search) ;

  let len1 = String.length search 
  and len2 = String.length s.ascii in

  let rec go pos =
    if pos > len2 - len1 then None
    else
    if search = String.sub s.ascii pos len1 then Some (String.sub s.ascii 0 pos)
    else go (pos+1)
  in

  go pos

(*
let inserts s l =
  if is_empty s then l
  else
    try 
      let other = List.find (fun x -> equalsduo x s) l in
      if other == choose s other then l (* Nothing to do *)
      else Common.myrevmap l (fun x -> if x == other then s else x)
    with Not_found -> s :: l

*)
    
*)
