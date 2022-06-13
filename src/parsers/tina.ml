open Angstrom

let (let*) = (>>=)

(* Whitespace *)
let ws = skip_while (function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false)

(* ANAME : any non empty string of letters, digits, primes ' and underscores _ *)

let aname =
  take_while1
    (function
      | 'A'..'Z' | 'a'..'z'  | '0'..'9' | '\'' | '_' -> true
      | _ -> false)
    
  <?> "aname"


let clear_buf buf = take_till (fun _ -> Buffer.clear buf ; true)

(* '{'QNAME'}' : any chain between braces, and in which characters {, }, and \ are prefixed by \  *)

let qname buf =

  char '{' *>
  clear_buf buf *>
            
  scan_state `Unescaped
    begin fun state char -> match (state, char) with
      | `Unescaped, '}' -> None
      | `Unescaped, '\\' -> Some `Escaped
      | `Unescaped, '{' -> Some `Error
      | `Unescaped, _ -> Buffer.add_char buf char ; Some `Unescaped
      | `Escaped, ('{' | '}' | '\\') -> Buffer.add_char buf char ; Some `Unescaped
      | `Escaped, _ -> Some `Error
      | `Error, _ -> None
    end

  >>= (function    
      | `Unescaped -> return (Buffer.contents buf)
      | _ -> fail "Bad-formed qname chain")

  <?> "qname"

let aname_or_qname buf =
  let* ch = ws *> peek_char in
  match ch with
  | '{' -> qname buf
  | _ -> aname

(* lower-case id *)
let lowid = take_while1 (function 'a'..'z' -> true | _ -> false)


let net_element buf loop =
    
  let* id = ws *> lowid <* ws in
  match id with
  | "net" ->
  | "tr" ->
  | "pl" ->

  (* 'nt' <note> ('0'|'1') <annotation> *)
  | "nt" -> aname_or_qname buf
    
  | _ -> fail ("Unknown keyword " ^ id)


(* comment appeler loop en tail-recursif en lui passant un acu ??? *)

(* voir si ('a -> 'b t)  -> ('a t -> 'b t)    existe... *)


let net buf = fix (net_element buf)


let _f buf = ws *> qname buf *> aname


let parser = any_char >>| (fun _ -> assert false)


