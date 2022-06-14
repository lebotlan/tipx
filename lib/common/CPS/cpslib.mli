
(* 'a, type of acu
 * 'b, type of the rest. *)
type ('a, 'b) kont = unit -> 'a -> 'b

(* Polymorphic token. Contains a cps element which can be used several times
 * (e.g. can be passed to a function). *)
type 'a tok = { it: 'b. ('a, ('a, 'b) kont -> 'b) kont }

(* Builds a start symbol. Initialise the acu with the given value. *)
val mk_start: 'a -> ('a, 'b) kont -> 'b

(* Builds an end symbol. *)
val mk_end: ('a -> 'b) -> ('a, 'b) kont

(* Builds a 0-argument operator on the acu. *)
val mk_op0: ('a1 -> 'a2) -> ('a1, ('a2, 'b) kont -> 'b) kont

(* Builds a 1-argument operator. *)
val mk_op1: ('a1 -> 'b -> 'a2) -> ('a1, 'b -> ('a2, 'c) kont -> 'c) kont

val mk_op2: ('a1 -> 'b -> 'c -> 'a2) -> ('a1, 'b -> 'c -> ('a2, 'd) kont -> 'd) kont
val mk_op3: ('a1 -> 'b -> 'c -> 'd -> 'a2) -> ('a1, 'b -> 'c -> 'd -> ('a2, 'e) kont -> 'e) kont

(* Example

   let start k = mk_start [] k
   let e k = mk_end List.length k
   let push = mk_op1 (fun acu x -> x :: acu)
   let push_add = mk_op2 (fun acu x y -> (x+y) :: acu)

   (start push 10 push 20 push 30 push_add 13 7 e)

*)
