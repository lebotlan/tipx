(* Unique identifier for places.
 * The identifier is an index in a marking, ordered from 0 to nb_pl - 1 *)
type pl_id = int

(* Unique identifier for transitions. Transitions are ordered from 0 to nb_tr - 1 *)
type tr_id = int

type weight = int

(*** Build a net incrementally ***)

type pl_name = string

type tr_name = string

(* Transitions in an incremental net. *)
type 'a g_itr =
  { itr_name: string ;
    itr_pre:   (weight * 'a) list ;
    itr_post:  (weight * 'a) list }

type itr = pl_name g_itr


(* Incremental net (mutable) *)
type inet

(* Create empty net 
 * only_places: register only places - ignore transitions. *)
val mk_empty: ?only_places:bool -> ?name:string -> unit -> inet

val add_pl: inet -> pl_name -> pl_id

(* If the inet is only-places, it records the places mentioned in the transition
 * and returns 0. *)
val add_tr: inet -> itr -> tr_id

val set_name: inet -> string -> unit

(*** Immutable net ***)

(* Places *)
type pl =
  { pl_id: pl_id ;
    pl_name: string ;

    pl_pre:  tr_id list ;
    pl_post: tr_id list }

(* Transitions *)
and tr =
  { tr_id:  tr_id ;
    tr_name: string ;

    (* Sorted by pl_id *)
    tr_pre:   (weight * pl_id) list ;
    tr_post:  (weight * pl_id) list ;
    tr_delta: (weight * pl_id) list }


(* Dummy transition (used like 'None'). *)
val null_tr : tr


type net

type t = net

(* safe:true : assume the net is safe *)
val close: ?safe:bool -> inet -> net

  
(* name can be empty *)
val get_name: net -> string

val is_safe: net -> bool

(* The id of each place is its index in the array. *)
(*     val all_pl: net -> pl array *)

(*
val get_pl: net -> pl_id -> pl
*)
  
val nb_pl: net -> int

val nb_tr: net -> int

(* @alloc a new array (copy) *)
val all_tr: net -> tr array

(* @alloc a new array (copy) *)
val all_pl: net -> pl array

val get_tr: net -> tr_id -> tr

val get_pl: net -> pl_id -> pl

val get_plname: net -> pl_id -> pl_name

(* Same as get_plname but add braces if contains '-' or '.' (compliant with Walk from the Tina toolbox) *)
val get_plnamebrace: net -> pl_id -> pl_name

val get_plid: net -> pl_name -> pl_id

(* Argument is the number of places. Used for testing markings only (no transition). *)
val mk_dummy_net: int -> net


(* Small test net *)
val test_net: net
