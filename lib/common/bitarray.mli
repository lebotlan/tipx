
(* Extensible bit arrays *)

type bitarray

type t = bitarray

(* Initially, all bits are set to false. 
 * The argument is an indicative starting size (in bits). *)
val create: int -> bitarray

val get: bitarray -> int -> bool
val set: bitarray -> int -> bool -> unit

(* How many bits are set (iterates) *)
val count: bitarray -> int

(* For the following iterators, beware that inv:true may only iterate on a partial subset of cleared bits. *)

(* Fold only on bits that are set (or clear if inv is true). *)
val fold: ?inv:bool -> bitarray -> 'a -> (int -> 'a -> 'a) -> 'a

(* Iter only on bits that are set (or clear if inv is true). *)
val iter: ?inv:bool -> bitarray -> (int -> unit) -> unit

(* Index of the last bit set, -1 if none. *)
val last: bitarray -> int

val clone: bitarray -> bitarray

(* Get all indices in an array. *)
val indices: bitarray -> int array

(* Pretty-printing of the set of bits. *)
val prettytos: ?inv:bool -> bitarray -> string

(* Bit matrix. Rows and columns are automatically added. *)
type bitmatrix

val mcreate: rows:int -> cols:int -> bitmatrix

val mget: bitmatrix -> int -> int -> bool
val mset: bitmatrix -> int -> int -> bool -> unit

val count_row: bitmatrix -> int -> int
