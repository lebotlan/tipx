module Infix:
sig
  (* a >>> b  is like a >> b () , but continues also if the first thread failed.
   * Similar to finalize, except that the exception of the first thread is discarded. *)
  val (>>>): unit Lwt.t -> (unit -> 'b Lwt.t) -> 'b Lwt.t

  (* Like join, but returns unit *)
  val (||>>): 'a Lwt.t -> 'b Lwt.t -> unit Lwt.t
end


(* Like Lwt_unix.yield, to help debugging when it is stuck (happens if it is called within a gtk callback) *)
val yield: unit -> unit Lwt.t

(* Like wrap, but for a function which returns a Lwt value (but may nevertheless raise a standard exception) *)
val lwrap: (unit -> 'a Lwt.t) -> 'a Lwt.t
val lwrap1: ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t
val lwrap2: ('a -> 'b -> 'c Lwt.t) -> 'a -> 'b -> 'c Lwt.t

(* Like join, but for arbitrary types. *)
val join2: 'a Lwt.t -> 'b Lwt.t -> ('a*'b) Lwt.t

val choose2: 'a Lwt.t -> 'a Lwt.t -> 'a Lwt.t

(* exn_guard a b : behaves like b, unless a fails in which case it fails too. 
 * (if a terminates, its value is discarded) *)
val exn_guard: 'a Lwt.t -> 'b Lwt.t -> 'b Lwt.t    

val checksw: Lwt_switch.t -> unit Lwt.t

(*
val lfst: ('a * 'b) Lwt.t -> 'a Lwt.t
val lsnd: ('a * 'b) Lwt.t -> 'b Lwt.t
*)
    
(* Partition, map & filter. *)
type ('a, 'b) choice = No | Left of 'a | Right of 'b

(* Sequential pmp, reversed. *)
val list_pmps: 'a list -> ('a -> ('b,'c) choice Lwt.t) -> ('b list * 'c list) Lwt.t

(* Parallel pmp, same order. *)
val list_pmpp: 'a list -> ('a -> ('b,'c) choice Lwt.t) -> ('b list * 'c list) Lwt.t
    

(* Like wait, but returns a function to wake up the waiting thread. *)     
val fwait: unit -> ('a Lwt.t * ('a -> unit))

(* Returns a thread that is permanently blocked. *)
val blocked: unit -> 'a Lwt.t

(* Like Lwt.async, but guarantees that the thread is not scheduled directly.
 * (Used inside Events consumers, to avoid unexpected InsaneLoops).
 * delay in seconds. *)
val later: ?delay:float -> (unit -> unit Lwt.t) -> unit

(* Convenience, later with argument. *)
val alater: ?delay:float -> ('a -> unit Lwt.t) -> 'a -> unit

(* Like later, but returns immediately Lwt.return_unit *)   
val llater: (unit -> unit Lwt.t) -> unit Lwt.t

(* Convenience, Lwt.async with argument. *)
val aasync: ('a -> unit Lwt.t) -> 'a -> unit

val ignore: 'a Lwt.t -> unit Lwt.t

(* paused f Makes a thread that is paused ; the evaluation of f is triggered only when the (unit -> unit) function is invoked for the first time. *)
val paused: (unit -> 'a Lwt.t) -> ('a Lwt.t * (unit -> unit))

(* Convenience : paused where we ignore the returned thread *)
val ipaused: (unit -> unit Lwt.t) -> (unit -> unit)


val array_iter_s: 'a array -> ('a -> unit Lwt.t) -> unit Lwt.t
val array_fold_s: 'a array -> 'b -> ('a -> 'b -> 'b Lwt.t) -> 'b Lwt.t
val array_map_s: 'a array -> ('a -> 'b Lwt.t) -> 'b array Lwt.t
val array_map_p: 'a array -> ('a -> 'b Lwt.t) -> 'b array Lwt.t

(* Tests if the given thread contains an exception. If so, raise it immediately. Otherwise, return immediately Lwt.return_unit *)
val check_failed: exn Lwt.t -> unit Lwt.t

(* Convenience Lwt.fail_with  with a format *)
val myfail : ('a, unit, string, 'b Lwt.t) format4 -> 'a

(* Convenience Lwt_mutex.with_lock, with an optional mutex. *)
val with_olock : Lwt_mutex.t option -> (unit -> 'a Lwt.t) -> 'a Lwt.t

