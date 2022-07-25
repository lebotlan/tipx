open Petrinet
open Formula
open Bool


let eval_simple m s = 
  match s with
  | K k -> k
  | P (w,pl_id) -> w * (Marking.get m pl_id)

(* TODO : performance : evaluate DELTA instead of expression 
 *
 *   - large transitions, small expression => eval expression
 *   - small transitions, large expressions => eval delta
 *)

let rec eval_expr m = function
  | [] -> 0
  | s :: rest -> (eval_simple m s) + (eval_expr m rest)

let eval_atom m atom =
  let e1 = eval_expr m atom.left and 
  e2 = eval_expr m atom.right in
  match atom.rel with 
  | NE -> e1 != e2
  | EQ -> e1 = e2
  | LT -> e1 < e2
  | LE -> e1 <= e2

let eval_formula m f = eval_bool (eval_atom m) f

let eval_goal goal m = goal.negates <> (eval_formula m goal.form)

let verdict goal res = goal.negates <> res
