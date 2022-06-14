(* open Petrinet *)

type path = string

(* val read: path -> (Net.t * Marking.t) Lwt.t *)

val read: path -> int list Lwt.t

