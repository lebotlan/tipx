(* Unique identifier for places.
 * The identifier is an index in a marking, ordered from 0 to nb_pl - 1 *)
type pl_id = int

(* Unique identifier for transitions. Transitions are ordered from 0 to nb_tr - 1 *)
type tr_id = int

type weight = int

(*

(*** Build a net incrementally ***)

(* Places in an incremental net. *)
type ipl =
  { ipl_id: pl_id ;
    ipl_name: string ;

    ipl_pre:  tr_id list ;
    ipl_post: tr_id list }

(* Transitions in an incremental net. *)
and itr =
  { itr_id: tr_id ;
    itr_name: string ;

    itr_pre:   (weight * pl_id) list ;
    itr_post:  (weight * pl_id) list }


(* Incremental net (mutable) *)
type inet

(* Create empty net *)
val mk_empty: ?name:string -> unit -> inet

val add_pl: inet -> ipl -> unit

val add_tr: inet -> itr -> unit


*)

(*** Immutable net ***)

(* Places *)
type pl =
  { pl_id: pl_id ;
    pl_name: string ;

    pl_pre:  tr list ;
    pl_post: tr list }

(* Transitions *)
and tr =
  { tr_id:  tr_id ;
    tr_name: string ;

    (* Sorted by pl_id *)
    tr_pre:   (weight * pl) list ;
    tr_post:  (weight * pl) list ;
    tr_delta: (weight * pl) list }


(* Dummy transition (used like 'None'). *)
val null_tr : tr


type net

type t = net

(*
val close: inet -> net
*)
  
(* name can be empty *)
val get_name: net -> string

(* The id of each place is its index in the array. *)
(*     val all_pl: net -> pl array *)

(*
val get_pl: net -> pl_id -> pl
*)
  
val nb_pl: net -> int

val nb_tr: net -> int

(* @alloc a new array *)
val all_tr: net -> tr array

val get_tr: net -> tr_id -> tr

val get_pl: net -> pl_id -> pl

(* Argument is the number of places. Used for testing markings only (no transition). *)
val mk_dummy_net: int -> net


(* Small test net *)
val test_net: net
