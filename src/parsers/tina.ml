open Petrinet
open Angstrom
open Names

type arc = Normal of int (* | Test of int | Inhibitor of int *)


let wstring s = ws *> string s <* ws
let arrow = wstring "->"

let nl = end_of_line


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

let ignore_til_eol = skip_while (function '\n' -> false | _ -> true) *> nl

let map_arc (w,nm) =
  match w with
  | Normal i -> (i,nm)

type acu =
  { inet: Net.inet ;
    mutable marking: (int * Net.pl_id) list }

(* 'pl' <place> {(<marking>)} *)
let pl aname_or_qname net_loop acu =
  let* name = aname_or_qname in
  let* c = peek_char in

  let* m = match c with
    | Some '(' -> advance 1 *> marking <* char ')' <* nl
    | _ -> nl *> return 0
  in

  let pl_id = Net.add_pl acu.inet name in

  if m <> 0 then acu.marking <- (m,pl_id) :: acu.marking ;

  net_loop acu
    

let parse_net ?(safe=false) name =

  let aname_or_qname = Names.aname_or_qname () in

  let tinput =
    let* name = aname_or_qname in

    let* c = peek_char in match c with   
    | Some '*' -> let* w = advance 1 *> ws *> int <* ws in return (Normal w, name)
    | Some '?' -> assert false (* To be implemented *)
    | _ -> return (Normal 1, name)

  and toutput =
    let* name = aname_or_qname in

    let* c = peek_char in match c with
    | Some '*' -> let* w = advance 1 *> ws *> int <* ws in return (Normal w, name)
    | _ -> return (Normal 1, name)

  in

  let rec net_loop acu =
    ws *>
    let* eof = at_end_of_input in

    if eof then return acu
    else
      let* c = peek_char in match c with
      | Some ('\n' | '\r') -> nl *> net_loop acu
      | Some '#' -> ignore_til_eol *> net_loop acu
      | _ ->
        begin
          let* id = lowid in match id with

          | "net" -> netname acu
          | "tr" -> tr acu
          | "pl" -> pl aname_or_qname net_loop acu
          | "nt" -> nt acu

          | _ -> fail ("Unknown keyword " ^ id)
        end

  (* 'tr' <transition> {<tinput> '->' <toutput>} *)
  and tr acu =
    let* itr_name = aname_or_qname in

    let* c = peek_char in match c with
    | Some ('\n' | '\r') -> nl *> net_loop acu
    | Some '#' -> ignore_til_eol *> net_loop acu
    | _ -> 
      let* (inp,outp) = map3 (many tinput) arrow (many toutput) ~f:(fun inp _ outp -> (inp,outp)) in

      (* Keep initial ordering. No rev_map *)
      let itr_pre = List.map map_arc inp
      and itr_post = List.map map_arc outp in

      let _ = Net.add_tr acu.inet { itr_name ; itr_pre ; itr_post } in

      net_loop acu


  (* 'net' <net> *)
  and netname acu =
    let* name = aname_or_qname <* nl in
    Net.set_name acu.inet name ;

    net_loop acu

  (* 'nt' <note> ('0'|'1') <annotation> *)
  and nt acu =
    let* _ = aname_or_qname *> (satisfy (function '0' | '1' -> true | _ -> false)) *> aname_or_qname <* nl in
    net_loop acu

  in

  let init_acu = { inet = Net.mk_empty ~name () ; marking = [] } in
  net_loop init_acu
  >>|
  begin fun acu ->
    let net = Net.close ~safe acu.inet in
    let marking = Marking.init ~safe net in
    let marking = List.fold_left (fun marking (m,pl_id) -> Marking.add marking pl_id m) marking acu.marking in
    (net, marking)
  end


let parse_net_places name =

  let aname_or_qname = Names.aname_or_qname () in

  let rec net_loop acu =
    ws *>
    let* eof = at_end_of_input in

    if eof then return acu
    else
      let* c = peek_char in match c with
      | Some ('\n' | '\r') -> nl *> net_loop acu
      | Some '#' -> ignore_til_eol *> net_loop acu
      | _ ->
        begin
          let* id = lowid in match id with

          | "net" | "tr" | "nt" -> ignore_til_eol *> net_loop acu
          | "pl" -> pl aname_or_qname net_loop acu

          | _ -> fail ("Unknown keyword " ^ id)
        end
  in

  let init_acu = { inet = Net.mk_empty ~name () ; marking = [] } in
  net_loop init_acu
  >>|
  begin fun acu ->
    let net = Net.close ~safe:false acu.inet in
    let marking = Marking.init ~safe:false net in
    (net, marking)
  end


(* 
 * Optimisation : qname ne pas utiliser de buffer. Avoir un flag pour savoir s'il y a des escape et garder la chaîne telle quelle.
 * 
 * let* x = bla in next    <- next est situé dans la fonction. Il ne faudrait pas que next construise un parser (pas d'application d'opérateur).
 *
 * Refléchir à l'allocation de parser : tinput buf   doit fabriquer un nouveau parser. C'est trop bête ! 
 *   => Foncteur ou grosse closure avec buf. ? 
 *
 *   Pour éviter de passer l'acu : faire un parseur qui renvoie une fonction qui attend l'acu ? Peut-être mieux. Chaque fonction est allouée autant de fois que de tokens. 
 *  Réfléchir à l'allocation mémoire... *)
