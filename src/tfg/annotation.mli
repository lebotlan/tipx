open Tfg

(* Literal identifier (index in a cube) *)
type lit_id = int

(* Coef of the node in a literal *)
type coef = int

(* Label is a pair composed of a lit_id and a coeff *)
type label = lit_id * coef

(* Annotation is a mapping node_id <-> labels *)
type annotation (* Abstract type *)

(* Iinitialize a TFG annotation *)
val init: int -> annotation

(* Add the label to a given node_id
 * If the node_id is already associated to the lit_id then it sums the coefficients. *)
val add_label_to_node: annotation -> label -> node_id -> unit

(* Propagate redundancy: child -> parents *)
val propagate_red: annotation -> node_id -> node_id list -> unit

(* Propagate agg: children -> parent *)
val propagate_agg: annotation -> node_id list -> node_id -> unit

val get_labels: annotation -> node_id -> label Seq.t

val annotation2s: tfg -> annotation -> string