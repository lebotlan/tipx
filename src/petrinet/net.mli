
(* Unique identifier for places.
 * The identifier is its index in the array (see all_pl below). *)
type pl_id = int

(* Unique identifier for transitions.
 * The identifier is its index in the array (see all_tr below). *)
type tr_id = int

type weight = int

type token_count = int
  
(* Initial marking.
 * The array is indexed by places index. *)
type mark = token_count array

type pl =
  { pl_id: id ;
    pl_name: string ;
    pl_in:  (weight * tr_id) list ;
    pl_out: (weight * tr_id) list }

and tr =
  { tr_id:  id ;
    tr_name: string ;
    tr_in:  (weight * pl_id) list ;    
    tr_out: (weight * pl_id) list }

(* A net is mutable *)
type net

type t = net


(*** Read ***)

(* name can be empty *)
val get_name: net -> string

val init_mark: net -> mark
  

(* The id of each place is its index in the array. *)
  val all_pl: net -> pl array

val get_pl: net -> pl_id -> pl
  

(* The id of each transition is its index in the array. *)
  val all_tr: net -> tr array

val get_tr: net -> tr_id -> tr
  


(*** Build ***)


(* Create empty net *)
val mk_empty: ?name:string -> unit -> net

val add_pl: net -> pl -> unit

val add_tr: net -> tr -> unit

