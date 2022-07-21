open Petrinet

type path = string

val read: path -> (Net.t * Marking.t) Lwt.t



