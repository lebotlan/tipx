open Petrinet
open Logic

type path = string

val read_net: path -> (Net.t * Marking.t) Lwt.t
val read_goal: Net.t -> path -> Formula.t Lwt.t


