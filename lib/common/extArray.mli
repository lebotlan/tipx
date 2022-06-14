
(* Extensible array *)
type 'a t

(* Starting size, default value. *)
val create: int -> 'a -> 'a t

(* Grow the array if necessary, filling new cells with the default value. *)
val set: 'a t -> int -> 'a -> unit

(* If a cell has never been set, it contains the default value. 
 * May raise Failure if index is < 0. *)
val get: 'a t -> int -> 'a

val size: 'a t -> int

val fold: 'a t -> 'b -> (int -> 'a -> 'b -> 'b) -> 'b

(* Called in increasing order. *)
val iter: 'a t -> (int -> 'a -> unit) -> unit

val copy: ?cp:('a -> 'a) -> 'a t -> 'a t



(* Extensible matrix *)
module Mat:
sig
  type 'a mat
  val create: int -> int -> 'a -> 'a mat
  val set: 'a mat -> int -> int -> 'a -> unit
  val get: 'a mat -> int -> int -> 'a

  (* 'Bounding box' of this matrix *)
  val size: 'a mat -> int * int

  (* fold & iter do not necessarily see the complete matrix rectangle. 
   * Some lines may be shorter than others. 
   * (tip: add option ?rectangle:bool if useful) *)
  val fold: 'a mat -> 'b -> (int -> int -> 'a -> 'b -> 'b) -> 'b
  val iter: 'a mat -> (int -> int -> 'a -> unit) -> unit
						      
  val copy: ?cp:('a -> 'a) -> 'a mat -> 'a mat			      
end
