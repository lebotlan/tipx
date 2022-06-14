open Wv

(*** PIPES carrying events. ***)

(* Note: seems similar to React, but not sure the semantics corresponds. 
 *       It happens that a similar library exist in Javascript: rxjs/Observable.  
 *
 * Usual issue: the pipe is connected _after_ the initial value has been sent.
 *              => A pipe holding a value might be useful after all (aka 'signal' ?)
 *              => Or add an option to pipes: when a listener is connected, send it some initial value ???
 *
*)

(* Note also that asynchronous behaviour (time rate limits, async) make use of the Lwt library.
 * Thus, it should be used inside a Lwt main loop. 
 * 
 * Concerning Lwt, callbacks (defined in to_pipe) should use Lwtplus.later instead of Lwt.async 
 * to avoid unexpected InsaneLoops. *)


(* Exception raised when a pipe directly or indirectly feeds itself.
 *    InsaneLoop name    (name of the pipe)
 * (By default, this is considered as bad design: most probably an infinite loop.)
 * The behaviour can be customized though, see 'whenloop' below. *)
exception InsaneLoop of string

(*** A pipe, carrying events of type 'a. ***)

(* We distinguish read & write sockets of the pipe (instead of just using a single read-write pipe)
 * in order to improve the readability of APIs. *)

(* Read socket of the pipe: consumers read values from the pipe through this socket. *)
type 'a rs

(* Write socket of the pipe: producers write values to the pipe through this socket. *)
type 'a ws

type 'a rw =
  { rs: 'a rs ;
    ws: 'a ws }

val getr: 'a ws -> 'a rs


(* Function called when a loop is detected inside a pipe (a pipe feeds itself, possibly indirectly).
 * The function receives the pipe name (for debug),
 *   the call stack of previous recursive events as well as the current event. The list is guaranteed to contain at least one element.
 * It may
 *   - raise InsaneLoop (by default), or any other exception. In this case, propagation of the current event is aborted and exception propagated upwards.
 *   - do not propagate further (None),
 *   - or act as if the given value was received (Some v'), which then replaces the current value. *)
type 'a whenloop = (name:string -> 'a list -> 'a -> 'a option)

(* Makes a new pipe.
 * The name is used to ease debug. *)
val make: name:string -> ?loop:'a whenloop -> unit -> 'a rw

(* A string pipe, goes to stdout (Printf). *)
val stdout: string ws

(* Like stdout, but uses Lwt_io. *)
val lwtstdout: string ws

val null: unit -> 'a ws

(* Replaces previous behaviour. *)
val set_loop: 'a rs -> 'a whenloop -> unit

(*** Note: consider using Lwtplus.later to avoid insane loops. *)

(* Convenience function: ignore loops in whenloop (do not propagate further). *)
val ignore_loop: 'a whenloop

(* Convenience whenloop which stop propagation as soon as a fixpoint is observed.
 * @raise InsaneLoop if recursion limit is reached. If limit=1, InsaneLoop is raised at first loop (if the fixpoint is not observed). *)
val fixloop: ?eq:('a -> 'a -> bool) -> ?limit:int -> unit -> 'a whenloop

(* Sends a message on this pipe. *)
val send: 'a ws -> 'a -> unit

val osend: 'a ws -> 'a option -> unit

(* send in Lwt.wrap2 *)  
val lwtsend: 'a ws -> 'a -> unit Lwt.t

(* send 'later' in a separate Lwt thread, which is guaranteed not to be scheduled immediately.
   See Lwtplus.later *)
val send_later: ?delay:float -> 'a ws -> 'a -> unit

(* Convenience function: new_pipe None returns a new pipe and its sender function.
 * new_pipe Some p returns the read socket associated to p and its sender function. *)
val new_pipe: name:string -> 'a ws option -> 'a rs * ('a -> unit)

val new_rpipe: name:string -> 'a rs option -> 'a rw

(* Note:
 * 
 *   In the following, some functions are used to connect pipes to a consumer (which can be another pipe).
 *   (The consumer can be an existing consumer, or created by the function itself.)
 *
 *   All such functions have an optional argument ?wv, of any (weak) type.
 *   wv is a weak value that will checked for garbage collection
 *   When the weak value is garbage-collected, the connection to the pipe is removed.
 *
 *   This is useful, for instance, when all the connecting pipes of some entity must be removed when the entity is collected.
 *   (Works only if the pipes do not have a pointer towards the entity - otherwise the entity is kept alive by the pipes.
 *    Do not forget to consider also indirect pointers.)
*)

(* Link a pipe to a consumer (that is, a callback).
 * A pipe can be linked to several consumers. 
 * last: add to the end of the queue instead of the beginning. *)
val to_cons: ?wv:wv -> ?last:bool -> 'a rs -> ('a -> unit) -> unit

(* Like to_cons. The lwt callback is launched with async. *)
val to_conslwt: ?wv:wv -> ?last:bool -> 'a rs -> ('a -> unit Lwt.t) -> unit

(* Convenience function: a pipe directly connected to a callback. *)
val new_cb: ?wv:wv -> ('a -> unit) -> 'a ws
val new_lwtcb: ?wv:wv -> ('a -> unit Lwt.t) -> 'a ws

(* Convenience: to_cons with an optional pipe.
 * Does nothing if the pipe is None.  *)
val oto_cons: ?wv:wv -> ?last:bool -> 'a rs option -> ('a -> unit) -> unit

(* Remove a callback added with to_cons. Uses physical equality. 
 * fails if the callback is unknown and check is true. *)
val remove_cb: ?check:bool -> 'a rs -> ('a -> unit) -> unit

(* Like remove_cb: does nothing if the pipe is None. *)
val oremove_cb: ?check:bool -> 'a rs option -> ('a -> unit) -> unit

(* Convenience *)         
val to_unit: ?wv:wv -> 'a rs -> (unit -> unit) -> unit

(* to_pipe pa pb  sends all events from pa to pb. *)
val to_pipe: ?wv:wv -> 'a rs -> 'a ws -> unit
val oto_pipe: ?wv:wv -> 'a rs option -> 'a ws -> unit

(* map_to_pipe pa f pb send all events from pa to pb, mapped by f. *)
val map_to_pipe: ?wv:wv -> 'a rs -> ('a -> 'b) -> 'b ws -> unit
val mapfilter_to_pipe: ?wv:wv -> 'a rs -> ('a -> 'b option) -> 'b ws -> unit

(* Flatten list to another pipe *)
val flatten: ?wv:wv -> ?target:'a ws -> 'a list rs -> 'a rs

(* Wait until an event is received. You may filter by providing a predicate. *)
val wait_on: ?pr:('a -> bool) -> 'a rs -> 'a Lwt.t


(*** Filters ***)

(* Sends only events that are different from the previous event. 
 * (That is, only changes are propagated). *)
val only_change: ?wv:wv -> ?target:'a ws -> ?eq:('a -> 'a -> bool) -> 'a rs -> 'a rs


(*** COMBINATORS ***)

(* All combinators have an optional argument ?target.
 * If target is not specified, those combinators create a new fresh pipe.
 * If target is specified, the combinators send their results to it. *)

val join: ?wv:wv -> ?target:'a ws -> 'a rs -> 'a rs -> 'a rs

val ojoin: ?wv:wv -> ?target:'a ws -> 'a rs option -> 'a rs -> 'a rs

(* Convenience *)
val ujoin: ?wv:wv -> ?target:unit ws -> 'a rs -> 'b rs -> unit rs

(* Map, synchronous: events are immediately produced *)
val map: ?wv:wv -> ?target:'b ws -> 'a rs -> ('a -> 'b) -> 'b rs
val mapfilter: ?wv:wv -> ?target:'b ws -> 'a rs -> ('a -> 'b option) -> 'b rs

(* Builds an 'a pipe whose events will be mapped to an existing 'b pipe. *)
val invmap: ?wv:wv -> 'b ws -> ('a -> 'b) -> 'a ws

val invmapfilter: ?wv:wv -> 'b ws -> ('a -> 'b option) -> 'a ws

(* Partition *)
val partition: ?wv:wv -> ?targeta:'a ws -> ?targetb:'a ws -> 'a rs -> ('a -> bool) -> ('a rs * 'a rs)

(* Map, asynchronous: events are produced when the lwt thread returns *)
val async_map: ?wv:wv -> ?target:'b ws -> 'a rs -> ('a -> 'b Lwt.t) -> 'b rs
val async_mapfilter: ?wv:wv -> ?target:'b ws -> 'a rs -> ('a -> 'b option Lwt.t) -> 'b rs

(* Activable pipe. Use the given reference to activate/disactivate the pipe.
 * Initially active. 
 * Returns a NEW pipe (its input is the argument). 
 * When the pipe is inactive, one may still send value with 'send' (only values received from the pipe argument are discarded). *)
val activable: ?wv:wv -> ?is_on:bool ref -> ?target:'a ws -> 'a rs -> (bool ref * 'a rw)

(* Stateful pipe. 
 * Creates two pipes (or use the given source & target).
 * When a source event 'a is received, update the internal reference with the given function,
 * then send the resulting value on target. 
 * Last argument is the initial state. *)

val stateful: ?wv:wv -> ?target:'b ws -> ?source:'a rs -> update:('b -> 'a -> 'b) -> 'b -> ('a ws * 'b rs)

val toggle: ?wv:wv -> ?target:bool ws -> ?source:'a rs -> bool -> ('a ws * bool rs)

(* Forwards all events from pipe a until predicate is true, then forwards all events from pipe b. *)
val until: ?wv:wv -> ?target:'a ws -> 'a rs -> 'a rs -> ('a -> bool) -> 'a rs


(*** Unimportant functions ***)
val get_name: string -> 'a rs -> string

val mk_new_pipe: ?wv:wv -> string -> ?target:'b ws -> 'a rs -> (('b -> unit) -> 'a -> unit) -> 'b rs

