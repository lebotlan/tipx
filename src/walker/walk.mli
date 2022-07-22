open Petrinet
open Logic

type state =
  { marking: Marking.t ;
    steps: int }

type walk_result =
  | Bingo of state
  | Deadlock of state
  | Timeout of state

val state2s: state -> string
val result2s: walk_result -> string

val result2verdict: walk_result -> Formula.goal -> string

(* timeout in seconds *)
val sprinter: ?seed:int -> ?timeout:int -> Net.t -> Marking.t -> (Marking.t -> bool) -> walk_result
    
