
type t

(* Creates a stat table *)
val mk: unit -> t

(* Records an entry with the given keywords. *)
val set: t -> string list -> unit

(* Prints stats *)
val tos: t -> string
  

(* TODO : also record the entries associated with each keyword. *)
  
  
