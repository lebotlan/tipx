
(* Small convenience layer over hashtables. *)

(* Type of associations between 'a keys and 'b values. *)
type ('a, 'b) assoc
type ('a, 'b) t = ('a, 'b) assoc

(* Creates a new association table. size is used to create the underlying hashtable. *)
val create: ?size:int -> init:('a -> 'b) -> unit -> ('a, 'b) t

(* Gets a value from the table. Create the association if necessary. *)
val get: ('a, 'b) t -> 'a -> 'b

val mem: ('a, 'b) t -> 'a -> bool

(* Like get, but raise Not_found if the association does not exist. *)
val get_existing: ('a, 'b) t -> 'a -> 'b

(* Update a value from the table using the given function. 
 * Create the value first if necessary *)
val update: ('a, 'b) t -> 'a -> ('b -> 'b) -> unit

(* Sets a value *)
val set: ('a, 'b) t -> 'a -> 'b -> unit

(* Remove a binding (it will be re-created if necessary using the init function. *)
val remove: ('a, 'b) t -> 'a -> unit

(* Convenience function that increments/decrements the value. *)
val incr: ('a, int) t -> 'a -> unit
val decr: ('a, int) t -> 'a -> unit

(* Update only if the key is already bound. *)
val update_if_exists: ('a, 'b) t -> 'a -> ('b -> 'b) -> unit

(* Update all keys *)
val update_all: ('a, 'b) t -> ('a -> 'b -> 'b) -> unit

val fold: ('a, 'b) t -> 'c -> ('a -> 'b -> 'c -> 'c) -> 'c

val iter: ('a, 'b) t -> ('a -> 'b -> unit) -> unit

val map: ('a, 'b) t -> ('a -> 'b -> 'c) -> ('a, 'c) t

(* Get the bindings from two assoc tables and map them to a third assoc table. *)
val map2: ('a, 'b) t -> ('a, 'c) t -> ('a -> 'b -> 'c -> 'd) -> ('a, 'd) t

val merge: ('a, 'c) t -> ('a -> 'b -> 'c -> 'c) -> import_from:('a, 'b) t -> unit

val to_list: ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c list

(* Creates an assoc from a list of bindings. A given key may appear several times. *)
val of_list: init:('a -> 'c) -> upd:('a -> 'b -> 'c -> 'c) -> ('a * 'b) list -> ('a, 'c) t

(* Convenience function that returns a sorted list of the results. *)
val sort: ('a, 'b) t -> (('a * 'b) -> ('a * 'b) -> int) -> ('a * 'b) list

(* Compare the pairs using a comparison value 'c. *)
val sort_key: ('a, 'b) t -> (('a * 'b) -> 'c) -> ('a * 'b) list

(* Returns the underlying hashtable (the real one, not a copy). *)
val table: ('a, 'b) t -> ('a, 'b) Hashtbl.t

val size: ('a, 'b) t -> int

val copy: ('a, 'b) t -> ('a, 'b) t


(* Convenience : creates an assoc from a list of arbitrary values and a function that reads the key. 
 * The assoc maps keys to list of values. *)
val of_any_list: 'a list -> ('a -> 'k) -> ('k, 'a list) t


(*** Functorial interface ***)

module type ASSOC =
  sig
    type key
    type 'b assoc
    type 'b t = 'b assoc
    type 'b table

    val create: ?size:int -> init:(key -> 'b) -> unit -> 'b t
    val get: 'b t -> key -> 'b
    val get_existing: 'b t -> key -> 'b
    val set: 'b t -> key -> 'b -> unit
    val update: 'b t -> key -> ('b -> 'b) -> unit
    val incr: int t -> key -> unit
    val decr: int t -> key -> unit
    val update_if_exists: 'b t -> key -> ('b -> 'b) -> unit
    val update_all: 'b t -> (key -> 'b -> 'b) -> unit
    val fold: 'b t -> 'c -> (key -> 'b -> 'c -> 'c) -> 'c
    val iter: 'b t -> (key -> 'b -> unit) -> unit
    val table: 'b t -> 'b table
    val size: 'b t -> int
    val sort: 'b t -> ((key * 'b) -> (key * 'b) -> int) -> (key * 'b) list
    val sort_key: 'b t -> ((key * 'b) -> 'c) -> (key * 'b) list
  end

module Mk_Assoc: functor (Hash: Hashtbl.S) -> ASSOC with type key = Hash.key and type 'b table = 'b Hash.t

 
