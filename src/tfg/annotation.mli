open Tfg

(* Each literal is identified by an int (its index in the cube) *)
type lit_id = int

(* Coef of a node (= variable) in a literal *)
type coef = int

(* A label is used to annotate nodes *)
type label = lit_id * coef

(* Annotation is a mapping node_id --> labels *)
type annotation

(* Initialize a TFG annotation.
 * init n  : n is the number of literals in the cube. *)
val init: int -> annotation

(* Associate the label to the given node_id
 * If the node_id is already associated to the lit_id then it sums the coefficients. 
 * mult is 1 by default - it corresponds to the arc multiplicity
 * The label is multiplied by mult before insertion into node.
 * *)
val add_label_to_node: annotation -> label -> ?mult:int -> node_id -> unit

(* Propagate redundancy: child -> parents *)
val propagate_red: annotation -> node_id -> (int * node_id) list -> unit

(* Propagate agg: children -> parent *)
val propagate_agg: annotation -> node_id list -> node_id -> unit

val get_labels: annotation -> node_id -> label Seq.t

(* The annotation is complete (or shadow-complete) if the projected formula is equivalent to the initial formula. 
 * If uncomplete, then the initial formula can be SAT whereas the projected one is not. *)
val is_complete: annotation -> bool

val annotation2s: tfg -> annotation -> string
