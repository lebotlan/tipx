open Formula

let expr2s_aux mkmul plid2s expr =

  let rec aux pos_acu neg_acu = function
  | [] -> pos_acu, neg_acu
  | (K k) :: rest -> 
    (match k with
     | x when x < 0 -> aux pos_acu ((string_of_int (-x)) :: neg_acu) rest
     | x when x > 0 -> aux ((string_of_int x) :: pos_acu) neg_acu rest
     | _ -> aux pos_acu neg_acu rest)
  | (P (w, pl_id)) :: rest -> 
    (match w with
     | x when x = 1  -> aux ((plid2s pl_id) :: pos_acu) neg_acu rest
     | x when x = -1 -> aux pos_acu ((plid2s pl_id) :: neg_acu) rest 
     | x when x > 0  -> aux (mkmul (string_of_int x) (plid2s pl_id) :: pos_acu) neg_acu rest
     | x when x < 0  -> aux pos_acu (mkmul (string_of_int (-x)) (plid2s pl_id) :: neg_acu) rest
     | _ -> aux pos_acu neg_acu rest)
  in

  aux [] [] expr
  
let expr2s plid2s expr =
  let pos, neg = expr2s_aux (fun a b -> a ^ "*" ^ b) plid2s expr in
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


(* SMT-lib format *)

let expr2smt plid2s expr = 
  let pos, neg = expr2s_aux (fun a b -> "(* " ^ a ^ " " ^ b ^ ")") plid2s expr in
  (Bool.infix "+" (fun s -> s) pos, Bool.infix "+" (fun s -> s) neg)

let atom2smt plid2s atom = 
  let aux s1 s2  = match (s1,s2) with 
    | "", "" -> "0"
    | "", s -> s
    | s, "" -> s
    | s, ss -> "(+ " ^ s ^ " " ^ ss ^ ")" in

  let left_pos, left_neg = expr2smt plid2s atom.left in
  let right_pos, right_neg = expr2smt plid2s atom.right in

  "(" ^ (match atom.rel with
      | NE -> "distinct "
      | EQ -> "= "
      | LT -> "< "
      | LE -> "<= ")  ^
    
  (aux left_pos right_neg) ^ " " ^
  (aux right_pos left_neg) ^ ")"

let formula2smt plid2s formula = Bool.bexpr2smt (atom2smt plid2s) formula 

let goal2smt plid2s goal =
  match goal.negates with
  | true -> "(not " ^ formula2smt plid2s goal.form ^ ")"
  | false -> formula2smt plid2s goal.form
