
(* Generic functions to serialize stuff. *)

type 'a serializer = 'a -> Bitstring.bitstring

val (@+) : Bitstring.bitstring -> Bitstring.bitstring -> Bitstring.bitstring

val bobool : bool serializer

(* 63-bit integers *)
val boi : int serializer

(* 64-bits float (double) *)
val bof : float serializer

(* Adaptative integer : 
 * [0-255] : 9 bits
 * [256-63bits] : 64 bits *)
val boa : int serializer

val bostring : string serializer

(* Adds a size indication to the bitstring. *)
(* val bobits : Bitstring.bitstring serializer *)

val bofixedstring : int -> string serializer

val bolist : ('a serializer) -> ('a list serializer)

val boption : ('a serializer) -> ('a option serializer)

val bopair : ('a serializer) -> ('b serializer) -> (('a * 'b) serializer)
(* val botriple : ('a serializer) -> ('b serializer) -> ('c serializer) -> (('a * 'b * 'c) serializer) *)

(* Constants used internally and used by deserialize only. Ignore. *)
val list_mark     : int
val endlist_mark  : int
val tuple_mark    : int
val endtuple_mark : int
