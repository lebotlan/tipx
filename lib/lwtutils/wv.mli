
(* A weak value = a weak array with a single cell. *)
type wv = WV : 'a Weak.t -> wv

(* Convenience: array with a single cell. *)
val weak_ar: 'a -> 'a Weak.t
    
val weak: 'a -> wv

(* Wait for the given weak value to be garbaged collected. 
 * If None, the thread is blocked. *)
val wait_wv: wv option -> unit Lwt.t


(* wv_gen_finalise wv f
 * f is called when the wv is garbaged.
 * f is called at once if wv is already garbaged. *)
val wv_gen_finalise: wv -> (unit -> unit) -> unit

(* wv_finalise wv f arg: when wv is garbage-collected, invoke f arg.
 * Note that arg is stored in a weak array. It can be garbaged meanwhile (and the function will not be called, then). *)
val wv_finalise: ?msg:string -> wv option -> ('a -> unit) -> 'a -> unit

(* Ibid. : both arg1 and arg2 are stored in a weak array. *)
val wv_finalise2: ?msg:string -> wv option -> ('a -> 'b -> unit) -> 'a -> 'b -> unit

