open Petrinet
open Angstrom

type arc = Normal of int (* | Test of int | Inhibitor of int *)

(* Whitespace *)
let ws = skip_while (function '\x20' | '\x09' -> true | _ -> false)

let wstring s = ws *> string s <* ws
let arrow = wstring "->"

let nl = end_of_line

(* ANAME : any non empty string of letters, digits, primes ' and underscores _ *)
let aname =
  ws *> take_while1
    (function
      | 'A'..'Z' | 'a'..'z'  | '0'..'9' | '\'' | '_' -> true
      | _ -> false)

  <?> "aname" <* ws

(* lower-case id *)
let lowid = ws *> take_while1 (function 'a'..'z' -> true | _ -> false) <* ws <?> "lowid"

let is_digit = function '0'..'9' -> true | _ -> false

let int = take_while1 is_digit >>| int_of_string <?> "int"

let marking =
  ws *>

  (let* n = int in
   let* c = peek_char in match c with
   | Some 'K' -> advance 1 *> return (n * 1000)
   | Some 'M' -> advance 1 *> return (n * 1000000)
   | Some 'G' -> advance 1 *> return (n * 1000000000)
   | _ -> return n)

  <* ws

let ignore_comment = skip_while (function '\n' -> false | _ -> true) *> nl

let map_arc (w,nm) =
  match w with
  | Normal i -> (i,nm)

let net name =

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

  let tinput =
    let* name = aname_or_qname in
    
    let* c = peek_char in match c with   
    | Some '*' -> let* w = advance 1 *> ws *> int <* ws in return (Normal w, name)
    | Some '?' -> assert false (* To be implemented *)
    | _ -> return (Normal 1, name)

  and toutput =
    let* _name = aname_or_qname in

    let* c = peek_char in match c with
    | Some '*' -> let* w = advance 1 *> ws *> int <* ws in return (Normal w, name)
    | _ -> return (Normal 1, name)

  in

  let rec net_loop inet =
    ws *>
    let* eof = at_end_of_input in

    if eof then return inet
    else
      let* c = peek_char in match c with
      | Some ('\n' | '\r') -> nl *> net_loop inet
      | Some '#' -> ignore_comment *> net_loop inet
      | _ ->
        begin
          let* id = lowid in match id with

          | "net" -> netname inet
          | "tr" -> tr inet
          | "pl" -> pl inet
          | "nt" -> nt inet

          | _ -> fail ("Unknown keyword " ^ id)
        end

  (* 'tr' <transition> {<tinput> '->' <toutput>} *)
  and tr inet =
    let* itr_name = aname_or_qname in

    let* c = peek_char in match c with
    | Some ('\n' | '\r') -> nl *> net_loop inet
    | Some '#' -> ignore_comment *> net_loop inet
    | _ -> 
      let* (inp,outp) = map3 (many tinput) arrow (many toutput) ~f:(fun inp _ outp -> (inp,outp)) in

      let itr_pre = List.rev_map map_arc inp
      and itr_post = List.rev_map map_arc outp in
      
      let _ = Net.add_tr inet { itr_name ; itr_pre ; itr_post } in
      
      net_loop inet


  (* 'net' <net> *)
  and netname inet =
    let* name = aname_or_qname <* nl in
    Net.set_name inet name ;
    
    net_loop inet

  (* 'nt' <note> ('0'|'1') <annotation> *)
  and nt inet =
    let* _ = aname_or_qname *> (satisfy (function '0' | '1' -> true | _ -> false)) *> aname_or_qname <* nl in
    net_loop inet

  (* 'pl' <place> {(<marking>)} *)
  and pl inet =
    let* name = aname_or_qname in
    let* c = peek_char in

    let* _m = match c with
      | Some '(' -> advance 1 *> marking <* char ')' <* nl
      | _ -> nl *> return 0
    in

    let _ = Net.add_pl inet name in

    net_loop inet

  in

  let init_inet = Net.mk_empty ~name () in
  
  net_loop init_inet >>| Net.close


(* 
 * TODO : qname ne pas utiliser de buffer. Avoir un flag pour savoir s'il y a des escape et garder la chaîne telle quelle.
 * 
 * let* x = bla in next    <- next est situé dans la fonction. Il ne faudrait pas que next construise un parser (pas d'application d'opérateur).
 *
 * TODO : refléchir à l'allocation de parser : tinput buf   doit fabriquer un nouveau parser. C'est trop bête ! 
 *   => Foncteur ou grosse closure avec buf. ?   Voir fichier TODO.
 *
 *   Pour éviter de passer l'acu : faire un parseur qui renvoie une fonction qui attend l'acu ? Peut-être mieux. Chaque fonction est allouée autant de fois que de tokens. 
 *  Réfléchir à l'allocation mémoire... *)
