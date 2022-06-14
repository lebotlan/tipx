(* Dump a 'size'-byte int into a bytes buffer. 
 * size: size in bytes (8 by default) *)
val dump_int: ?size:int -> bytes -> int -> int -> unit

(* Read a 'size'-byte int from a bytes buffer. 
 * size: size in bytes (8 by default) *)
val read_int: ?size:int -> string -> int -> int
