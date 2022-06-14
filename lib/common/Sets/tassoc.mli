
(*** Tree assoc.
 *
 *  An evolution of association lists (or Map.t).
 *  No side-effect. *)

(* Type of associations between 'a keys and 'b values. *)
type ('a, 'b) tassoc
type ('a, 'b) t = ('a, 'b) tassoc

(* Keys are of type 'a list.
 * Usual association lists correspond to singleton keys [x]
 * Lists are typically used to make categories, e.g.  [ "category1" ; "subcategory" ; "subkey" ]
 * The key can be empty. *)

type 'a key = 'a list

(* Reversed *)
type 'a revkey = 'a list
    
(* Creates a new association tree.
 * When looking for a key and a result is always needed (e.g. update), the default value is used. 
 * The default value may depend on the key. 
 *)
val create: ?cmp:('a -> 'a -> int) -> default:('a key -> 'b) -> unit -> ('a, 'b) t


(* TODO: extract sub categories *)

val size: ('a, 'b) t -> int

(* Returns the default value if not found. *)
val get: ('a, 'b) t -> 'a key -> 'b

(* Returns None if not found *)
val oget: ('a, 'b) t -> 'a key -> 'b option

val mem: ('a, 'b) t -> 'a key -> bool

(* Updates a value. Uses the default value if not found. *)
val update: ?only_exist:bool -> ('a, 'b) t -> 'a key -> ('b -> 'b) -> ('a, 'b) t

(* Update all entries. *)
val update_all: ('a, 'b) t -> ('a revkey -> 'b -> 'b) -> ('a, 'b) t

(* t2 = map t1 f
 * default of t2 can be given explicitly, otherwise it is mapped from t1.default *)
val map: ?default:('a key -> 'c) -> ('a, 'b) t -> ('a revkey -> 'b -> 'c) -> ('a, 'c) t

(* Sets (replaces) a value. *)
val set: ('a, 'b) t -> 'a key -> 'b -> ('a, 'b) t

val remove_val: ('a, 'b) t -> 'a key -> ('a, 'b) t

val remove_subtree: ('a, 'b) t -> 'a key -> ('a, 'b) t

val fold: ('a, 'b) t -> 'c -> ('a revkey -> 'b -> 'c -> 'c) -> 'c

val iter: ('a, 'b) t -> ('a revkey -> 'b -> unit) -> unit



(* Get the bindings from two assoc tables and map them to a third assoc table. 
 * revk: indicates whether the function receives the reversed key or normal key. *)
val map2: ?revk:bool -> ('a, 'b) t -> ('a, 'c) t -> ('a key -> 'b -> 'c -> 'd) -> ('a, 'd) t

val merge_into: ?revk:bool -> from:('a, 'b) t -> into:('a,'c) t -> ('a revkey -> 'b -> 'c -> 'c) -> ('a, 'c) t 

val to_list: ('a, 'b) t -> ('a revkey -> 'b -> 'c) -> 'c list

(* Convenience function that returns a sorted list of the results. *)
val sort: ('a, 'b) t -> (('a revkey * 'b) -> ('a revkey * 'b) -> int) -> ('a revkey * 'b) list

(* Compare the pairs using a third value of type 'c. *)
val map_sort: ('a, 'b) t -> (('a revkey * 'b) -> 'c) -> ('a revkey * 'b) list

(* Convenience : creates an assoc from a list of arbitrary values and a function that reads the key. 
 * The assoc maps keys to list of values. *)
val of_any_list: ?cmp:('a -> 'a -> int) -> 'b list -> ('b -> 'a key) -> ('a, 'b list) t
