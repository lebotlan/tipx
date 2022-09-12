open Pipe
open Petrinet
open Logic

type state =
  { marking: Marking.t ;
    steps: int ;
    seed: int }

type walk_result =
  | Bingo of state
  | Deadlock of state
  | Timeout of state
  | Maxstep of state

val state2s: state -> string
val result2s: walk_result -> string

val result2verdict: walk_result -> Formula.goal -> string

type stat =
  { steps: int ;
    time: float ;

    (* Number of steps per second, since last stat *)
    rate: int }

val stat2s: stat -> string

(* timeout in seconds 
 * max_steps n:  if n < 0, max_steps is ignored.
 *               if n >= 0, the walker stops with result Maxstep after n steps. *)
val sprinter: ?seed:int -> ?timeout:int -> ?max_steps:int -> ?stats:stat ws -> Net.t -> Marking.t -> (Marking.t -> bool) -> walk_result
    
val stat_stdout: stat ws
