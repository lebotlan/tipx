(* 
 * Map, polymorphic version. 
 * Adapted from map.ml in the ocaml distribution. 
 *)


(* This is the original copyright: *)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* NOTE: If this file is map.mli, do not edit it directly! Instead,
   edit templates/map.template.mli and run tools/sync_stdlib_docs *)

type 'a cmp = 'a -> 'a -> int

type ('key,'a) t
    
val empty: 'key cmp -> ('key,'a) t
val is_empty: ('key,'a) t -> bool
val mem: 'key -> ('key,'a) t -> bool
val add: 'key -> 'a -> ('key,'a) t -> ('key,'a) t
val update: 'key -> ('a option -> 'a option) -> ('key,'a) t -> ('key,'a) t
val singleton: cmp:'key cmp -> 'key -> 'a -> ('key,'a) t
val remove: 'key -> ('key,'a) t -> ('key,'a) t
val merge:
  ('key -> 'a option -> 'b option -> 'c option) ->
  ('key,'a) t -> ('key,'b) t -> ('key,'c) t
val union: ('key -> 'a -> 'a -> 'a option) -> ('key,'a) t -> ('key,'a) t -> ('key,'a) t
val compare: ('a -> 'a -> int) -> ('key,'a) t -> ('key,'a) t -> int
val equal: ('a -> 'a -> bool) -> ('key,'a) t -> ('key,'a) t -> bool
val iter: ('key -> 'a -> unit) -> ('key,'a) t -> unit
val fold: ('key -> 'a -> 'b -> 'b) -> ('key,'a) t -> 'b -> 'b
val for_all: ('key -> 'a -> bool) -> ('key,'a) t -> bool
val exists: ('key -> 'a -> bool) -> ('key,'a) t -> bool
val filter: ('key -> 'a -> bool) -> ('key,'a) t -> ('key,'a) t
val filter_map: ('key -> 'a -> 'b option) -> ('key,'a) t -> ('key,'b) t
val partition: ('key -> 'a -> bool) -> ('key,'a) t -> ('key,'a) t * ('key,'a) t
val cardinal: ('key,'a) t -> int
val bindings: ('key,'a) t -> ('key * 'a) list
val min_binding: ('key,'a) t -> ('key * 'a)
val min_binding_opt: ('key,'a) t -> ('key * 'a) option
val max_binding: ('key,'a) t -> ('key * 'a)
val max_binding_opt: ('key,'a) t -> ('key * 'a) option
val choose: ('key,'a) t -> ('key * 'a)
val choose_opt: ('key,'a) t -> ('key * 'a) option
val split: 'key -> ('key,'a) t -> ('key,'a) t * 'a option * ('key,'a) t
val find: 'key -> ('key,'a) t -> 'a
val find_opt: 'key -> ('key,'a) t -> 'a option
val find_first: ('key -> bool) -> ('key,'a) t -> 'key * 'a
val find_first_opt: ('key -> bool) -> ('key,'a) t -> ('key * 'a) option
val find_last: ('key -> bool) -> ('key,'a) t -> 'key * 'a
val find_last_opt: ('key -> bool) -> ('key,'a) t -> ('key * 'a) option
val map: ('a -> 'b) -> ('key,'a) t -> ('key,'b) t
val mapi: ('key -> 'a -> 'b) -> ('key,'a) t -> ('key,'b) t
