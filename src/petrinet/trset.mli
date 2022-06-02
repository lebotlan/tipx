open Net
    
(* Sets of transitions. *)

(* Mutable *)
type trset


(* Creates an empty set *)
val init: net -> trset

val add: trset -> tr -> unit

val remove: trset -> tr -> unit

val contains: trset -> tr -> bool

val clone: trset -> trset


(* pick trset n  picks a transition in trset, using n as a kind of random index. *)
val pick: net -> trset -> random:int -> tr


val equal: trset -> trset -> bool



(* Idée : bit-array pour facilement tester appartenance / suppression  
 * bit-array : Bytes / 64 bits ??? *)
