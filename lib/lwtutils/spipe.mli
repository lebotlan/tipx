(*** Synchronous Pipes ***)

(* Like Pipes, but sending a message expects an answer. *)

(* Synchronous socket.
 *   Reads values of type 'a
 *   Answers with values of type 'b. 
 * 
 * or conversely: 
 *   Sends values of type 'a,
 *   Receives answers of type 'b. 
 *
 * Answers cannot get interleaved (that is, a sender receives the answer to his message, not to another message).
 * Several consumers may receive a message, but only one answer will be received by the sender.
 * *)
type ('a, 'b) ss

val make: name:string -> ?loopa:'a Pipe.whenloop -> ?loopb:'b Pipe.whenloop -> unit -> ('a,'b) ss

val send: ('a,'b) ss -> 'a -> 'b Lwt.t

val to_cons: ?last:bool -> ('a,'b) ss -> ('a -> 'b Lwt.t) -> unit

(* Returns the received value and a function to send the answer. *)
val wait_on: ('a,'b) ss -> ('a * ('b -> unit)) Lwt.t


