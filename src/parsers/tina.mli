open Petrinet

(* net net-name allocates a new local buffer and returns a parser. *)
val parse_net: ?safe:bool -> string -> (Net.t * Marking.t) Angstrom.t

(* Parse a net, but takes only the set of places. 
 * (The resulting net has no transition). *)
val parse_net_places: string -> (Net.t * Marking.t) Angstrom.t
