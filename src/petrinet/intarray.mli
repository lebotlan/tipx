
type format = M8 | M16 | M32

type intarray

type t = intarray

(* create fmt size
 * Initially 0 everywhere. *)
val create: format -> int -> t

val tos: ?max:int -> t -> string

(* @noalloc *)
val size: t -> int

val clone: t -> t


(* Cells are numbered from 0. *)

(* @noalloc *)
val get: t -> int -> int


exception Marking_overflow

(* Set or add a value to a cell. 
 * Automatically changes the format if overflow
 *
 * @raise Marking_overflow if a marking overflows M32 or if a safe marking is set to a value > 1.
 *
 * Returns t itself (modified), or a new t if the format has changed.
 *
 * set tab index value  *)

(* @noalloc most of the time
 * @alloc if the array needs to be reformated (overflows the current format). *)
val set: t -> int -> int -> t
val add: t -> int -> int -> t

(* Put 0 everywhere *)                            
val clear: t -> unit

(* @noalloc *)
val cmp: t -> t -> int


