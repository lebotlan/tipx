open Petrinet
open Formula

val eval_formula: formula -> Marking.t -> bool

val verdict: goal -> bool -> bool