open Petrinet
open Logic
open Libtfg

type path = string

val read_net: ?safe:bool -> path -> (Net.t * Marking.t) Lwt.t

(* Return a marking = 0 *)
val read_net_places: path -> (Net.t * Marking.t) Lwt.t

type source = Net of Net.t | Tfg of Tfg.t

val read_goals: source -> path -> Formula.t list Lwt.t

val sread_goals: source -> string -> Formula.t list Lwt.t

val read_tfg: Net.t -> path -> Tfg.t Lwt.t


