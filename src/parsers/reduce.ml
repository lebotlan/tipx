open Angstrom
open Libtfg
open Petrinet

open Names
open Tfg

(* Missing in Angstrom ? Should be implemented without a closure. *)
let ifthenelse p t e =
  let* succeeds = (p *> return true) <|> return false in
  if succeeds then t else e

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
  
  let equation = lift3 (fun left _ right -> (left, right)) aname_or_qname (char '=') (expr []) in
  
  let rec line = lazy

    ( ifthenelse (char '#' <* ws)

        ( (let* c = choice [ char 'R' ; char 'A' ] <* string " |-" in
           let* (left, right) = equation in
           
           match c with
           | 'R' -> add_red tfg right left ; !!line        
           | 'A' -> add_agg tfg left right ; !!line
               
           | _ -> assert false)
          
          <|>
          
          (skip_while (function '\n' -> false | _ -> true) *> (skip_while (function '\n' -> true | _ -> false)) **> line) )
        
        ( (char '\n' **> line)
          <|>
          return tfg ) )

  in

  !!line 

  
