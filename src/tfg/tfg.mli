open Petrinet
open Net


type tfg

type t = tfg

(* Identifiers of nodes in the tfg.
 * When a node is a root, its node_id equals its pl_id in the reduced net. *)
type node_id = int

type node_name = string

(* Places of the reduced net (roots). 
 * Cell number #i must contain place #i. *)
val create: pl array -> tfg


(* Add agglomeration: add_agg tfg X [ Y1 ; ... ; Yn ]     X = Y1 + ... + Yn *)
val add_agg: tfg -> node_name -> node_name list -> unit

(* Add redundancy: add_red tfg [ Y1 ; ... ; YN ] X    X = Y1 + ... + Yn  
 * If a node_name is numeric, it is a constant node. *)
val add_red: tfg -> (int * node_name) list -> node_name -> unit

(* Add X <= k *)
val add_leq: tfg -> node_name -> int -> unit

type node_type =
  (* Variable *)
  | Var of node_name

  (* Any value in interval (unconstrained) *)
  | Intv of int * int

type node

type succ =
  { agg: node list ;
    red: (int * node) list }

type pred = Root | A of node | R of (int * node) list

(* Roots include constant nodes *)
val roots: tfg -> node list

val succ: tfg -> node -> succ

val pred: tfg -> node -> pred

val node_type: node -> node_type

val node_id: node -> node_id

val is_root: tfg -> node -> bool

val get_node: tfg -> node_id -> node

val get_nodename: tfg -> node_id -> node_name

val get_nodenamebrace: tfg -> node_id -> node_name

val get_nodeid: tfg -> node_name -> node_id

val nb_nodes: tfg -> int

val is_empty: tfg -> bool
  
