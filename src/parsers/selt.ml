open Logic
open Angstrom
open Bool
open Formula
    
let ws = skip_while (function '\x20' | '\x09' -> true | _ -> false)

let op_land = ws *> string "/\\" <* ws
let op_lor  = ws *> string "\\/" <* ws

(*
 *  inner_expr      : (lor_expr) | - expr | what
 *
 *  land_expr : expr [ /\ expr /\ ... ]
 *
 *  lor_expr : land_expr [ \/ land_expr \/ ... ]
 *)

let bool_expr what =

  let rec inner_expr () =
    ws *>

    (* (lor_expr) *)
    (( char '(' *>
       let* lo1 = lor_expr [] in
       char ')' *> return lo1 )
     
     <|>

     (* - expr *)
     ( char '-' *> ws *>
       let* e = inner_expr () in
       return (Not e) )

     <|>

     (* what *)
     let* w = what in
     return (V w))
  

  and land_expr acu =

    (* expr *)
    let* e1 = inner_expr () <* ws in

    (op_land *> land_expr (e1 :: acu))
    <|>
    return (if acu = [] then e1 else And (List.rev (e1 :: acu)))
    

  and lor_expr acu =
    
    (* land_expr *)
    let* le1 = land_expr [] in
    
    (op_lor *> lor_expr (le1 :: acu))
    <|>
    return (if acu = [] then le1 else Or (List.rev (le1 :: acu)))

  in

  lor_expr []

let build_atom e1 op e2 =
  match op with
  | ">=" | "ge" -> { left = e2 ; rel = LE ; right = e1 }
  | ">"  | "gt" -> { left = e2 ; rel = LT ; right = e1 }
  | "<"  | "lt" -> { left = e1 ; rel = LT ; right = e2 }
  | "<=" | "le" -> { left = e1 ; rel = LE ; right = e2 }
  | "="  | "eq" -> { left = e1 ; rel = EQ ; right = e2 }
  | "!=" | "ne" -> { left = e1 ; rel = NE ; right = e2 }
  | _ -> assert false

let rel_ops = List.map string [ ">=" ; "ge" ; ">"  ; "gt" ; "<"  ; "lt" ; "<=" ; "le" ; "="  ; "eq" ; "!=" ; "ne" ]

let rel = ws *> choice ~failure_msg:"Bad operator. Should be <, <=, >, >=, =, !=, ge, gt, lt, le, eq, ne" rel_ops <* ws

let parse_goal get_plid =

  let aname_or_qname = Names.aname_or_qname () in

  let simple = ws *>
               let* id = aname_or_qname in
               match int_of_string_opt id with
               | Some n -> return (K n)
               | None -> return (P (1, get_plid id))
  in

  
  let rec expr acu = ws *>
                     let* s = simple <* ws in
                     ( (char '+' *> expr (s :: acu))
                       <|>
                       return (List.rev acu) )
  in
  
  (* expr REL expr *)
  let atom = lift3 build_atom (expr []) rel (expr []) in
  
  let formula = bool_expr atom in

  
  let* op = ws *> take 2 in
  match op with
  | "[]" -> let* form = formula in return { form ; negates = true }
  | "<>" -> let* form = formula in return { form ; negates = false }
  | _ -> fail ("The formula should begin with <> or [], not " ^ op)



