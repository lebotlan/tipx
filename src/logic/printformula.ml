open Formula
open Petrinet

let simple2s net simple =
  match simple with
  | (K k) -> string_of_int k
  | (P (w, pl_id)) -> (Net.get_pl net pl_id).pl_name ^ (if w = 1 then "" else "*" ^ (string_of_int w))

let expr2s net expr = Common.sep (simple2s net) " " expr 

let atom2s net atom = 
  expr2s net atom.left
  ^ (match atom.rel with
    | NE -> " != "
    | EQ -> " = "
    | LT -> " < "
    | LE -> "  <= ")
  ^ expr2s net atom.right

  let formula2s net formula = Bool.bool2s (atom2s net) formula 

  let goal2s net goal =
    (match goal.negates with
    | true -> "[] "
    | false -> "<> ")
    ^ formula2s net goal.form ^ "\n"
