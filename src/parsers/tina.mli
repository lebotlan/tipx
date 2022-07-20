open Petrinet

(* val net: (Net.t * Marking.t) Angstrom.t *)

(* net net-name allocates a new local buffer and returns a parser. *)
val net: string -> Net.net Angstrom.t


