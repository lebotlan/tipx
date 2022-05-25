open Net

exception Not_fireable of mark * tr

val fireables: net -> mark -> tr_id list

(* Keep only fireable transitions *)
val filter_fireables: net -> mark -> tr_id list -> tr_id list

(* fire net m t 
 *   Fire transition t from marking m.
 *   Returns the reached marking.
 *
 *   ?out: use this array as output. Can be == to m. 
 *
 *  @raise Not_fireable if t is not fireable at m.
 *)
val fire: ?(out:mark) -> net -> mark -> tr_id -> mark


(* Like fire, with out = m 
 * and assume t is fireable at m (not checked). *)
val quick_fire: net -> mark -> tr_id -> unit


