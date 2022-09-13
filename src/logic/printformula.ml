open Formula

let expr2s plid2s expr = 
  let rec aux plid2s pos_acu neg_acu = function
  | [] -> pos_acu, neg_acu
  | (K k) :: rest -> 
      (match k with
      | x when x < 0 -> aux plid2s pos_acu ((string_of_int (-x)) :: neg_acu) rest
      | x when x > 0 -> aux plid2s ((string_of_int x) :: pos_acu) neg_acu rest
      | _ -> aux plid2s pos_acu neg_acu rest)
  | (P (w, pl_id)) :: rest -> 
      (match w with
      | x when x = 1  -> aux plid2s ((plid2s pl_id) :: pos_acu) neg_acu rest
      | x when x = -1 -> aux plid2s pos_acu ((plid2s pl_id) :: neg_acu) rest 
      | x when x > 0  -> aux plid2s (((string_of_int x) ^ "*" ^ (plid2s pl_id)) :: pos_acu) neg_acu rest
      | x when x < 0  -> aux plid2s pos_acu (((string_of_int (-x)) ^ "*" ^ (plid2s pl_id)) :: neg_acu) rest
      | _ -> aux plid2s pos_acu neg_acu rest)
  in
  let pos, neg = aux plid2s [] [] expr in
  (Common.sep (fun s -> s) " + " pos), (Common.sep (fun s -> s) " + " neg)

let atom2s plid2s atom = 
  let aux s1 s2  = match (s1,s2) with 
    | "", "" -> "0"
    | "", s -> s
    | s, "" -> s
    | s, ss -> s ^ " + " ^ ss in

  let left_pos, left_neg = expr2s plid2s atom.left in
  let right_pos, right_neg = expr2s plid2s atom.right in

  (aux left_pos right_neg) ^
  (match atom.rel with
    | NE -> " != "
    | EQ -> " = "
    | LT -> " < "
    | LE -> " <= ")
    ^ (aux right_pos left_neg)
  
  let formula2s plid2s formula = Bool.bexpr2s (atom2s plid2s) formula 

  let goal2s plid2s goal =
    match goal.negates with
    | true -> "- (" ^ formula2s plid2s goal.form ^ ")"
    | false -> formula2s plid2s goal.form
