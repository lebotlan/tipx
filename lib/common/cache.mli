
val cache: ('a -> 'b) -> ('a -> 'b)

val cache2: ('a -> 'b -> 'c) -> ('a -> 'b -> 'c)


(* Cache with a buffer of only n values, stored in an array.
 * Tries to be very efficient.
 * You must provide an (efficient) == comparison function. *)

type ('a,'b,'c) qcache

(* An init value is needed to fill the cache arrays. *)
val mk_qcache: eqq:('a -> 'a -> bool) -> size:int -> init:('a*'b) -> ('a,'b,'c) qcache

(* Orig is supposed to specify the function (same orig => same function).
 * Hence, we can reuse the same cached data if the old cache was already initialized with the same orig. *)
val qcached: ('a,'b,'c) qcache -> orig:'c -> ('a -> 'b) -> ('a -> 'b)


