open Wv

(* A sleep that can be increased once started. 
 * When terminated, returns a value of type 'a. *)
type 'a t

(* Give the initial delay and return value.
 * To start it, use sleep (below). *)
val new_delay: 'a -> float -> 'a t

(* Indicate if this delay is terminated. *)
val is_terminated: 'a t -> bool
     
(* Increase this delay by a positive amount. Optionally modifies the return value.
 * Fails if the delay is already terminated. *)
val increase_delay: ?return:'a -> 'a t -> float -> unit

(* Modifies the return value.            
 * Fails if the delay is already terminated. *)
val mod_value: 'a t -> 'a -> unit
            
(* Wait for the given delay. It can be increased while sleeping.
 * Fails if the delay is already terminated. *)
val sleep: 'a t -> 'a Lwt.t
                

(* Used to control periodic functions. 
 * When paused, there is no lwt cost for waiting.
 * The periodic function will be garbage-collected if the control pipe & control function are garbage-collected. 
 *
 * `Stop definitely stops the periodic function (it detaches it from the control pipe, so that future commands are ignored). 
 * It also fails the ramp functions. *)
type control = [ `Pause | `Run | `Stop ]
val control2s: control -> string

(* Switch that is turned off if ctl receives Stop_fail or Stop_nice *)
val ctl2switch: control Pipe.rs option -> Lwt_switch.t

(* Periodic calls to f. Returns a control function. 
 * The initial state is Run, unless specified otherwise. 
 * The control can also be done using the given pipe. *)
val periodic: ?switch: Lwt_switch.t -> ?init:[`Pause|`Run] -> ?ctl: control Pipe.rs -> period:float -> (unit -> 'a Lwt.t) -> (control -> unit)


(* 'Ramp' function. 
 * Emits float values starting at 'from', increased by 'delta' every 'timestep' seconds.
 * Emits n samples.
 * Returns when the ramp is finished (as soon as the nth sample is emitted). 
 * Can be controlled by ctl (does not pause the current delay, but pauses between steps). *)
val ramp: ?switch : Lwt_switch.t -> ?ctl: control Pipe.rs -> float Pipe.ws -> from:float -> delta:float -> timestep:float -> n:int -> unit -> unit Lwt.t

(* Convenience. *)
val ramp_to: ?switch: Lwt_switch.t -> ?ctl: control Pipe.rs -> float Pipe.ws -> from:float -> tov:float -> timestep:float -> total_delay:float -> unit -> unit Lwt.t

(* Convenience: ramp with integers *)
val iramp: ?switch: Lwt_switch.t -> ?ctl: control Pipe.rs -> int Pipe.ws -> from:int -> delta:int -> timestep:float -> n:int -> unit -> unit Lwt.t

val iramp_to: ?switch: Lwt_switch.t -> ?ctl: control Pipe.rs -> int Pipe.ws -> from:int -> tov:int -> timestep:float -> total_delay:float -> unit -> unit Lwt.t


(* The following function may loop forever.
 * An optional wv object o may be provided to these periodic functions. 
 * It is kept as a weak pointer to o. When o is garbage collected, the periodic function stops (and will be garbage collected). 
 * (For the moment, the periodic function stops only once the current ramp is done.) *)


(* Ramps_seq: execute each ramp one after another.
 * Each ramp is specified by its target value and the time it takes. (target, delay)
 * The sequence may loop. Then, the last element of the sequence 'seq' is followed by the first element of the sequence 'seq' (not the initial value).
 * Emits intermediate values.
 * finish is called at the end of the sequence and in case the sequence is stopped.
*)

val ramps_seq: ?wv:wv -> ?switch: Lwt_switch.t -> ?ctl: control Pipe.rs -> ?to_p:float Pipe.ws -> ?cb:(float -> unit) -> ?finish_cb:(unit -> unit) ->
  from:float -> seq:(float*float) list -> loop:bool -> timestep:float -> unit -> unit Lwt.t
      
(* Convenience: ramps_seq with integers *)
val iramps_seq: ?wv:wv -> ?switch: Lwt_switch.t -> ?ctl: control Pipe.rs -> ?to_p:int Pipe.ws -> ?cb:(int -> unit) -> ?finish_cb:(unit -> unit) ->
  from:int -> seq:(int*float) list -> loop:bool -> timestep:float -> unit -> unit Lwt.t
