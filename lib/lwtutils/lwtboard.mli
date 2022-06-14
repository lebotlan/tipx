(*** Board to send synchronous messages to other lwt-threads ***)
(*

(* Board holding values of type 'a. *)
type 'a t

val create: unit -> 'a t

(* Sends a value. Returns when a consumer takes the value. *)		       
val send: 'a t -> 'a -> unit Lwt.t

(* Wait for a value satisfying the given predicate. 
 * Note that the predicate may be invoked several times on a single value. *)
val waitfor: 'a t -> ('a -> bool) -> 'a Lwt.t

(* Convenience functions *)
val waitforvalue: 'a t -> 'a -> unit Lwt.t

(* The map function is called at most once on an element that returns Some. *)				     
val waitformap: 'a t -> ('a -> 'b option) -> 'b Lwt.t

(* Typical usage:
 * 
 * Thread #1: send `Signal, then waitfor `Answer
 * Thread #2: waitfor `Signal, then send `Answer
 *
 * Note: you do not necessarily need the lwtboard - sometimes, a simple lwt callback is enough.
 *
 *)

*)
