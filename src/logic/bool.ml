type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or  of 'a bexpr list
                | Not of 'a bexpr
                | True
                | False


let rec bool2s fu = function
  | V x -> fu x
  | And l -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " /\\ " l
  | Or  l  -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " \\/ " l
  | Not e  -> "-(" ^ bool2s fu e ^ ")"
  | True -> "True"
  | False -> "False"

let rec eval_bool fu = function
  | V x -> fu x
  | And l -> List.for_all (eval_bool fu) l
  | Or  l  -> List.exists (eval_bool fu) l
  | Not e  -> not (eval_bool fu e)
  | True -> true
  | False -> false

let and_append vv = function
  | [False] -> [False]
  | acu -> vv :: acu

let cartesian ll =

  let insert prefix = function
    | True -> prefix
    | False -> [False]
    | And l -> List.rev_append l prefix
    | Or _ -> assert false
    | Not _ -> assert false
    | V _ as vv -> and_append vv prefix
  in
  
  let rec loop acu prefix = function
    | [] -> prefix :: acu
            
    | [] :: _ -> acu
      
    | (x1 :: l1) :: rest ->

      let prefix2 = insert prefix x1 in
      let acu2 = loop acu prefix2 rest in
      loop acu2 prefix (l1 :: rest)
  in

  loop [] [] ll
    
    
(*
let rec dnf_aux lit_negation negation = function
  | V x as vv -> if negation then V (lit_negation x) else vv

  | And l -> 
    if negation then
      (* Not (l1 /\ ... /\ ln) <-> (not l1 \/ ... \/ not ln) *)
      dnf_aux lit_negation false (Or l)
    else
      (* DNF(P and Q) <-> (P1 and Q1) or ... or (Pm and Q1) or ... or (Pm and Qn) *)
      (* with (DNF P) = (P1 or ... or Pm) and (DNF Q) = (Q1 or ... or Qn)         *)
      (* TODO *)
      And l

  | Or l -> 
    if negation then 
      (* Not (l1 \/ ... \/ ln) <-> (l1 /\ ... /\ ln) *)
      dnf_aux lit_negation false (And l)
    else 
      (* DNF(l1 /\ ... /\ ln) <-> DNF(l1) /\ ... /\ DNF(ln) *)
      Or (List.map (dnf_aux lit_negation negation) l)
 
  | Not e -> dnf_aux lit_negation (not negation) e 
*)
    

let or_to_list = function
  | Not _ -> assert false
  | Or l -> l
  | aa -> [ aa ]

let make_or = function
  | [] -> False
  | [x] -> x
  | l -> Or l

let make_and = function
  | [] -> True
  | [x] -> x
  | l -> And (List.rev l)

let or_append vv = function
  | [True] -> [True]
  | acu -> vv :: acu

(* acu is a disjunction of cubes. *)
let rec dnf_aux acu = function
  | True -> [True]
  | False -> acu
  | V _ as vv -> or_append vv acu

  | Not _ -> assert false

  | Or l -> List.fold_left dnf_aux acu l

  | And l ->
    let clauses = List.rev_map (fun x -> make_or (dnf_aux [] x)) l in

    let sets = List.rev_map or_to_list clauses in
    let cubes = List.rev_map (fun tuple -> make_and tuple) (cartesian sets) in
    List.rev_append cubes acu

let dnf _lit_negation bexpr = make_or (dnf_aux [] bexpr)
    
