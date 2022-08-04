open Petrinet
open Logic
open Libtfg

type path = string

val read_net: ?safe:bool -> path -> (Net.t * Marking.t) Lwt.t

val read_goal: Net.t -> path -> Formula.t Lwt.t

val read_goal_to_project: Tfg.t -> path -> Formula.t Lwt.t

val read_tfg: Net.t -> path -> Tfg.t Lwt.t


