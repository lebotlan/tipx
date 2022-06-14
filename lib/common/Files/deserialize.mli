
(* Generic functions to deserialize serialized stuff. *)

type 'a deserializer = Bitstring.bitstring -> ('a * Bitstring.bitstring)

val boolob : bool deserializer
val iob : int deserializer
val fob : float deserializer
val wfob : float deserializer

(* Adaptative integer : 
 * [0-255] : 9 bits
 * [256-31bits] : 32 bits *)
val aob : int deserializer

val stringob : string deserializer

val fixedstringob : int -> string deserializer

val mapob : ('a deserializer) -> ('a -> 'b) -> ('b deserializer)

val listob : ('a deserializer) -> ('a list deserializer)

val optionob : ('a deserializer) -> ('a option deserializer)

val pairob : ('a deserializer) -> ('b deserializer) -> (('a * 'b) deserializer)

