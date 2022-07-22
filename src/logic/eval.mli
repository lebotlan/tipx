open Petrinet
open Formula

val eval_goal: goal -> Marking.t -> bool

val verdict: goal -> bool -> bool