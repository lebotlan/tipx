
type bitvec

type t = bitvec

val init: int -> t

val set: t -> int -> unit
val unset: t -> int -> unit


(* 0 or 1 *)
val get: t -> int -> int

(* pick bitvec start  returns the first bit that is set, starting at position start.
 * It returns a negative value if none is found. *)
val pick: t -> int -> int

val clone: t -> t

val equal: t -> t -> bool
  
val cmp: t -> t -> int

(* Max number of elements to print *)
val tos: ?max:int -> t -> string
  
