open Formula

let simple2s plid2s simple =
  let constant2s k = if k < 0 then "(" ^ (string_of_int k) ^ ")" else (string_of_int k) in
  match simple with
  | (K k) -> constant2s k
  | (P (w, pl_id)) -> (if w = 1 then "" else (constant2s w) ^ "*") ^ (plid2s pl_id)

let expr2s plid2s expr = Common.sep (simple2s plid2s) " + " expr 

let atom2s plid2s atom = 
  expr2s plid2s atom.left
  ^ (match atom.rel with
    | NE -> " != "
    | EQ -> " = "
    | LT -> " < "
    | LE -> " <= ")
  ^ expr2s plid2s atom.right

  let formula2s plid2s formula = Bool.bexpr2s (atom2s plid2s) formula 

  let goal2s plid2s goal =
    (match goal.negates with
    | true -> "- "
    | false -> "")
    ^ formula2s plid2s goal.form
