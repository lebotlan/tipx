open Angstrom

let (let*) = (>>=)

(* Whitespace *)
let ws = skip_while (function '\x20' | '\x09' -> true | _ -> false)

let wstring s = ws *> string s <* ws

let nl = end_of_line

(* ANAME : any non empty string of letters, digits, primes ' and underscores _ *)

let aname =
  ws *> take_while1
    (function
      | 'A'..'Z' | 'a'..'z'  | '0'..'9' | '\'' | '_' -> true
      | _ -> false)
    
  <?> "aname" <* ws


let clear_buf buf = take_till (fun _ -> Buffer.clear buf ; true)

(* '{'QNAME'}' : any chain between braces, and in which characters {, }, and \ are prefixed by \  *)

let qname buf =

  ws *> char '{' *>
  clear_buf buf *>
  
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
      | _ -> fail "Bad-formed qname chain")
    
      <?> "qname"

let aname_or_qname buf =
  let* ch = ws *> peek_char_fail in
  match ch with
  | '{' -> qname buf
  | _ -> aname

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

type arc = Normal of int (* | Test of int | Inhibitor of int *)

let tinput buf =
  let* _name = aname_or_qname buf in

  let* c = peek_char in match c with
  | Some '*' -> let* w = advance 1 *> ws *> int <* ws in return (Normal w)
  | Some '?' -> assert false (* To be implemented *)
  | _ -> return (Normal 1)

let toutput buf =
  let* _name = aname_or_qname buf in

  let* c = peek_char in match c with
  | Some '*' -> let* w = advance 1 *> ws *> int <* ws in return (Normal w)
  | _ -> return (Normal 1)
  

let rec net_loop buf acu =

  ws *>
  
  begin
    (end_of_input *> return acu)

    <|>

    (let* () = nl in net_loop buf acu)
    
    <|>
    
    let* id = lowid in match id with

    | "net" -> netname buf acu
    | "tr" -> tr buf acu
    | "pl" -> pl buf acu
    | "nt" -> nt buf acu

    | _ -> fail ("Unknown keyword " ^ id)
             
  end

(* 'tr' <transition> {<tinput> '->' <toutput>} *)
and tr buf acu =
  let* _name = aname_or_qname buf in

  (nl *> net_loop buf acu)
  <|>
  let* _qqchose = map3 (many (tinput buf)) (wstring "->") (many (toutput buf)) ~f:(fun _inp _ _outp -> ()) in
  net_loop buf acu
    
  
(* 'net' <net> *)
and netname buf acu =
  let* _name = aname_or_qname buf <* nl in
  net_loop buf acu

(* 'nt' <note> ('0'|'1') <annotation> *)
and nt buf acu =
  let* _ = aname_or_qname buf *> (satisfy (function '0' | '1' -> true | _ -> false)) *> aname_or_qname buf <* nl in
  net_loop buf acu

(* 'pl' <place> {(<marking>)} *)
and pl buf acu =
  let* _name = aname_or_qname buf in
  let* c = peek_char in

  let* m = match c with
    | Some '(' -> advance 1 *> marking <* char ')' <* nl
    | _ -> nl *> return 0
  in

  net_loop buf (m :: acu)


let net =
  let buf = Buffer.create 200 in
  net_loop buf [] 


(* TODO FIXME : ne pas utiliser let* utiliser lift3 / map3 plutôt 
 * TODO : le \n a un rôle particulier (pas dans les noms, termine une décl... 
 * TODO : commentaires lignes commençant par # 
 * TODO : retour à la ligne pour terminer une déclaration. 
 *
 * TODO : refléchir à l'allocation de parser : tinput buf   doit fabriquer un nouveau parser. C'est trop bête ! 
 *   => Foncteur ou grosse closure avec buf. ? 
 *
 *   Pour éviter de passer l'acu : faire un parseur qui renvoie une fonction qui attend l'acu ? Peut-être mieux. Chaque fonction est allouée autant de fois que de tokens. 
 *  Réfléchir à l'allocation mémoire... *)
