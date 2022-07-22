open Petrinet

(* net net-name allocates a new local buffer and returns a parser. *)
val parse_net: ?safe:bool -> string -> (Net.t * Marking.t) Angstrom.t
