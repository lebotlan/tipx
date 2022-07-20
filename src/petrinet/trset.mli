open Net
    
(* Sets of transitions. *)

(* Mutable *)
type trset


(* Creates an empty set *)
val init: net -> trset

val add: trset -> tr_id -> unit

val remove: trset -> tr_id -> unit

val contains: trset -> tr_id -> bool

val clone: trset -> trset


(* pick trset n  picks a transition in trset, considering transitions starting at 'start'
 * If no transition is in trset, returns null_tr *)
val pick: net -> trset -> start:int -> tr


val equal: trset -> trset -> bool

