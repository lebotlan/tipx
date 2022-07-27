open Net
open Marking
open Trset
    
exception Not_fireable of tr * marking

(* @noalloc *)
val is_fireable: tr -> marking -> bool


val fireables: net -> marking -> trset



(* fire net m t 
 *   Fire transition t from marking m.
 *   Returns the reached marking (returns out if out is given).
 *
 *   ?out: use this array as output. out can be m. 
 *         if out needs to be reformated (e.g. int8 -> int16), allocates a new marking.
 *         if out is unspecified, allocates a new marking.
 *
 *  @raise Not_fireable if t is not fireable at m.
 *)
val fire: marking -> ?out:marking -> tr -> marking


(* Like fire, with out = m 
 * and assume t is fireable at m (not checked). *)
val quick_fire: marking -> tr -> marking


(* update_fireables n m ts tid 
 *
 *  Prerequisites:
 *    n was in state m0 (not given), whose fireable transitions are ts. 
 *    tid has been fired.
 *    the new marking is m 
 *
 *  ts is updated, it contains the transitions fireable at m.
 *
 * let upd = update_fireables net ()   allocates some data.
 *
 * then, upd m tr t   is @noalloc
 *)
val update_fireables: net -> unit -> marking -> trset -> tr -> unit
