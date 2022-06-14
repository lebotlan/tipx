(* Run functions in preemptive threads, through Lwt. 
 * A mutex can be used to ensure mutual exclusion of execution. *)

val launch: ?mutex:Lwt_mutex.t -> ('a -> 'b) -> 'a -> 'b Lwt.t

