type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or  of 'a bexpr list
                | Not of 'a bexpr

let rec bool2s fu = function
  | V x -> fu x
  | And l -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " /\\ " l
  | Or  l  -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " \\/ " l
  | Not e  -> "-(" ^ bool2s fu e ^ ")"

let rec eval_bool fu = function
  | V x -> fu x
  | And l -> List.for_all (eval_bool fu) l
  | Or  l  -> List.exists (eval_bool fu) l
  | Not e  -> not (eval_bool fu e)

let rec dnf lit_negation negation = function
  | V x ->
    if negation then
      V (lit_negation x)
    else
      V x

  | And l -> 
    if negation then
      (* Not (l1 /\ ... /\ ln) <-> (l1 \/ ... \/ ln) *)
      dnf lit_negation false (Or l)
    else
      (* DNF(P and Q) <-> (P1 and Q1) or ... or (Pm and Q1) or ... or (Pm and Qn) *)
      (* with (DNF P) = (P1 or ... or Pm) and (DNF Q) = (Q1 or ... or Qn)         *)
      (* TODO *)
      And l

  | Or l -> 
    if negation then 
      (* Not (l1 \/ ... \/ ln) <-> (l1 /\ ... /\ ln) *)
      dnf lit_negation false (And l)
    else 
      (* DNF(l1 /\ ... /\ ln) <-> DNF(l1) /\ ... /\ DNF(ln) *)
      Or (List.map (dnf lit_negation negation) l)
 
  | Not e -> dnf lit_negation (not negation) e 

