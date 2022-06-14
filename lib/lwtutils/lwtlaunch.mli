(* Similar to Lwt.with_value, but with a different API.
 * Usage : Setkeys.( key1 %= value ++ key2 %= value ++ key3 %= value =>> fun ) 
 *
 * Beware, if a thread is called by a gtk-callback, the key can be unset (because the calling thread is the gtk main loop).
*)
module Setkeys:
sig
  (* 'b: type returned by the evaluation of the main thread. *)
  type 'b config

  (* Sets a key/value pair *)
  val (%=): 'a Lwt.key -> 'a -> 'b config

  (* Concat two configs *)
  val (++): 'b config -> 'b config -> 'b config

  (* Launches Lwt.main (if keys hold global preferences, this is the recommended way to start the application) *)
  val (===>): 'a config -> (unit -> 'a Lwt.t) -> 'a

  (* Like ===>, but the argument is not a thread. *)
  val (=>>): 'b config -> (unit -> 'b) -> 'b

  val noconfig: 'a config
end

(* Configure LOG and handle exceptions. 
 * Handle (some) signals if the pipe is given. 
 * Beware, the signal numbers are OCaml Sys's values, not Unix kill values. 
 * 
 * If configure_log is true (true by default),
 *   stdout: log on stdout (false by default)
 *   stderr: log on stderr (true by default). *)
val launch: ?psignals: int Pipe.ws -> ?configure_log:bool -> ?stdout:bool -> ?stderr:bool ->
  appname:string -> run:('b -> 'a Lwt.t) -> 'b -> unit -> 'a Lwt.t


