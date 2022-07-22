open Petrinet
open Formula
open Bool


let eval_simple m s = 
  match s with
  | (K k) -> k
  | (P (w,pl_id)) -> w * (Marking.get m pl_id)

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

let eval_formula f m = eval_bool (eval_atom m) f

let verdict goal res = goal.negates <> res
