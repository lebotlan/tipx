(*** Predicate / variables : thread waiting for a predicate to become true ***)

type 'a var

(* Create and initialise a variable *)	
val create: 'a -> 'a var

val read: 'a var -> 'a
		     
(* Sets a variable. May wake up some threads if their predicate becomes true. *)		     
val set: 'a var -> 'a -> unit

(* Cancels all waiting threads on this variable. *)			   
val reset: 'a var -> unit
			   
val wait_for_predicate: 'a var -> ('a -> bool) -> 'a Lwt.t

			      
       
