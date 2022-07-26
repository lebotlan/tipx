open Angstrom
open Libtfg
open Petrinet

open Names
open Tfg

(* Missing in Angstrom ? Should be implemented without a closure. *)
let ifthenelse p ~t ~e =
  let* succeeds = (p *> return true) <|> return false in
  if succeeds then t else e

(* Like ifthenelse, but with a binder *)
let mmatch p ~t ~e =
  let* result = (let* r = p in return (Some r)) <|> return None in
  match result with
  | Some r -> t r
  | None -> e

let (!!) = Lazy.force

(* Local fix. Should be included in Angstrom itself. *)
let ( **> ) p1 lp2 = let* _ = p1 in !!lp2

let parse_tfg net =

  let tfg = Tfg.create (Net.all_pl net) in

  let aname_or_qname = Names.aname_or_qname () in

  let rec expr acu = ws *>
                     let* s = aname_or_qname <* ws in
                     ((char '+' *> expr (s :: acu))
                      <|>
                      return (s :: acu))
  in

  let equation = lift3 (fun left op right -> (left, op, right)) aname_or_qname ((string "=") <|> (string "<=")) (expr []) in

  let rec line = lazy

    ( ifthenelse (char '#' <* ws)
        
        ~t:(mmatch (choice [ char 'R' ; char 'A' ] <* string " |-")
              ~t:(fun c ->
                  ( let* (left, op, right) = equation in
                    
                    match c,op,right with
                    | 'A',"=",_    -> add_agg tfg left right ; !!line
                    | 'R',"=",_    -> add_red tfg right left ; !!line                                                
                    | 'R',"<=",[k] -> add_leq tfg left (int_of_string k) ; !!line
                        
                    | _ -> assert false) )
              
              ~e:((string "simplified" *> return tfg)
                  <|>
                  (string "markings" *> return tfg)
                  <|>          
                  (skip_while (function '\n' -> false | _ -> true) *> (skip_while (function '\n' -> true | _ -> false)) **> line)))

        (* mmatch instead of ifthenelse otherwise the recursion is not well founded. *)
        ~e:( mmatch (char '\n') ~t:(fun _ -> !!line) ~e:(return tfg) ) )

  in

  !!line
