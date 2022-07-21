open Angstrom

(* Whitespace *)
let ws = skip_while (function '\x20' | '\x09' -> true | _ -> false)

(* ANAME : any non empty string of letters, digits, primes ' and underscores _ *)
let aname =
  ws *> take_while1
    (function
      | 'A'..'Z' | 'a'..'z'  | '0'..'9' | '\'' | '_' -> true
      | _ -> false)

  <?> "aname" <* ws


let aname_or_qname () =

  (* Local buffer *)
  let buf = Buffer.create 200 in

  let clear_buf = take_till (fun _ -> Buffer.clear buf ; true) in

  (* '{'QNAME'}' : any chain between braces, and in which characters {, }, and \ are prefixed by \  *)

  let qname =

    (ws *> char '{' *>
     clear_buf *>

     scan_state `Unescaped
       begin fun state char -> match (state, char) with
         | `Final, _ -> None
         | `Unescaped, '}' -> Some `Final
         | `Unescaped, '\\' -> Some `Escaped
         | `Unescaped, '{' -> Some `Error
         | `Unescaped, _ -> Buffer.add_char buf char ; Some `Unescaped
         | `Escaped, ('{' | '}' | '\\') -> Buffer.add_char buf char ; Some `Unescaped
         | `Escaped, _ -> Some `Error
         | `Error, _ -> None
       end

     <* ws

     >>= (function    
         | `Final -> return (Buffer.contents buf)
         | _ -> fail "Bad-formed qname chain"))

    <?> "qname"
  in

  let aname_or_qname =
    let* ch = ws *> peek_char_fail in
    match ch with
    | '{' -> qname
    | _ -> aname
  in

  aname_or_qname


