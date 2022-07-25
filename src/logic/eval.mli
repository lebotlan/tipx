open Petrinet
open Formula

val eval_formula: Marking.t -> formula -> bool

(* Indicates if we have found what we are looking for (goal, taking into account 'negates') *)
val eval_goal: goal -> Marking.t -> bool

(* verdict goal (result of eval goal)   tells if the initial formula is true or false *)
val verdict: goal -> bool -> bool
