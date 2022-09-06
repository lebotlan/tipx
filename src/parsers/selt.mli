open Logic
open Petrinet
    
val parse_goal: (string -> Net.pl_id) -> Formula.goal Angstrom.t

val parse_goals: (string -> Net.pl_id) -> Formula.goal list Angstrom.t

