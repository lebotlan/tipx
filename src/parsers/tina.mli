(* open Petrinet *)

(* val net: (Net.t * Marking.t) Angstrom.t *)

(* net () allocates a new local buffer and returns a parser. *)
val net: unit -> int list Angstrom.t


