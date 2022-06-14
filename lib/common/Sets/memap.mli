(*** Maps of mergeable keys (see Meset). ***)

(* Keys are mergeable. The content of a key can only 'grow'. 
 * Keys are mapped to values. Values can change, this is the 'mutable' part of the object. *)

module type M =
sig

  type key
    
  type 'a map

  module Keyset: Meset.S with type elt = key

  (* tos: Mandatory printer function. Used in x2string functions. 
   * mergev: when keys are merged, associated values are merged too. *)
  val empty: ?compare:(key -> key -> int) -> tos:('a -> string) -> mergev:('a -> 'a -> 'a) -> unit -> 'a map

  (* Like empty, but mergev also receives the key. *)
  val kempty: ?compare:(key -> key -> int) -> tos:('a -> string) -> kmergev:(key -> 'a -> 'a -> 'a) -> unit -> 'a map

  (* Number of bindings == number of keys in this map. *)
  val size: 'a map -> int

  (* Create an empty map from an existing map. *)
  val clear: 'a map -> 'a map

  (* If the map is not empty, returns a binding. *)
  val choose: 'a map -> (key * 'a) option
  
  (* Return the set of keys. *)
  val keys: 'a map -> Keyset.set

  (* Merge a key into the map and updates the binding.
   * If keys are merged together, the associated values are merged too. 
   *
   * ?f: function applied to update the value (gets the merged key and the current value, returns the updated value)
   * ?default: value to be passed to f if the key does not exist yet. (default will be given the argument key)
   *
   * ?force is for the key only.
   *
   * @raise Elt_not_found if the key does not exist and default is not specified.
   * @raise same exceptions than Meset.merge_in *)
  val update: ?exist:bool -> ?force:bool -> ?default:(key -> 'a) -> ?f:(key -> 'a -> 'a) -> 'a map -> key -> 'a map
  val update_k: ?exist:bool -> ?force:bool -> ?default:(key -> 'a) -> ?f:(key -> 'a -> 'a) -> 'a map -> key -> 'a map * key

  (* Derived functions
   * Adds a new binding or replace an existing binding. DOES NOT MERGE VALUES. *)
  val add: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map
  val add_k: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map * key

  (* Add a new bindings. Merge values if existing. *)
  val add_or_merge: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map

  (* See Meset.mem *)
  val find_result2s: 'a map -> (key * 'a) Meset.mem_result -> string
  val find: no:bool -> 'a map -> key -> (key * 'a) Meset.mem_result
  
  (* remove mp key: the key must be physically equal to a key in the map (as returned by find). *)
  val remove: 'a map -> key -> 'a map

  (* val filter: 'a map -> (key -> 'a -> bool) -> 'a map *)

  (* See Set.fold for the meaning of comparekey and compare. *)
  val fold: ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> 'a map -> 'c -> (key -> 'a -> 'c -> 'c) -> 'c

  val lwt_fold: ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> 'a map -> 'c -> (key -> 'a -> 'c -> 'c Lwt.t) -> 'c Lwt.t

  (* to_string *)
  val map2s: ?key2s:(key -> string) -> ?v2s:('a -> string) -> ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> ?title:string -> 'a map -> string

  (* Maps a map. Must provide a merge function. *)
  val mmap: 'a map -> tos:('b -> string) -> mergev:('b -> 'b -> 'b) -> ('a -> 'b) -> 'b map

  (* Maps, with the same type. *)
  val imap: 'a map -> ('a -> 'a) -> 'a map
    
end


module Make : functor (Elt: Meset.ELEMENT) -> M with type key = Elt.elt

