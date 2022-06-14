open Cps

(* TODO LWt *)

(* This module eases the writing of s-function (stack-function),
 * that is, functions poping their arguments from a stack (or from command-line arguments) and pushing their result to the stack.
 *
 * It also eases the writing of a ready-to-run program that executes the instructions found on the command-line.
 *)


(* Values on the stack have type 'a. 
 * Note: 'a can be a union type, e.g.  Int of int | Bool of bool *)
type 'a stack

(* To ease the writing of user operators, the user first creates getters from 'a to 'b. 
 * e.g. a getter from (Int of int | Bool of bool) to int.  A getter may fail if the data is not of the expected dynamic type. 
 * A getter returns an 'a result. *)
type 'a result = Ok of 'a | Failed of string

(* Describes the return type of a user operator. 
 * 'b is the type of the stack. 'a is the type of the returned value. *)
type ('a, 'b) ret_value

(* unit ret_value: the user function returns (), nothing is pushed on the stack. *)
val unit: (unit, 'b) ret_value

(* r0 is equal to unit *)
val r0: (unit, 'b) ret_value

(* r1 cv1: the user function returns x, which is converted with cv1, then pushed on the stack. *)
val r1: ('a -> 'b) -> ('a, 'b) ret_value

(* r2 cv1 cv2: the user function returns (x,y). Converted x is pushed first, then y. Thus, the stack contains y :: x :: ... *)
val r2: ('a1 -> 'b) -> ('a2 -> 'b) -> ('a1 * 'a2, 'b) ret_value

(* r3 cv1 cv2 cv3: the user function returns (x,y,z). Converted x is pushed first, then y, then z. The stack contains z :: y :: x :: ... *)
val r3: ('a1 -> 'b) -> ('a2 -> 'b) -> ('a3 -> 'b) -> ('a1 * 'a2 * 'a3, 'b) ret_value

(* Sordid type used to implement CPS below. *)
type (_, _) acu

(* The type of a s-function definition operating on 'a stack. *)
type 'a def
    
val def: string -> (('b -> 'c stack -> 'c stack * 'b, 'c -> unit result) acu, 'd) kont -> 'd 

val (!->): 'a -> (('b -> 'c stack -> 'c stack * ('d -> 'e), 'c -> 'd result) acu,
                 (('b -> 'c stack -> 'c stack * 'e, 'a) acu, 'f) kont -> 'f) kont

val (!==): 'a -> (((unit -> 'a) -> 'b stack -> 'b stack * 'c, ('c, 'b) ret_value) acu,
                  string -> 'b def) kont


(*

(* A stack machine, handling values of type 'a. *)
type 'a machine = 'a def list

(* An operator tries to operate on the stack. If it fails, it returns an error message. *)
and 'a def = 'a stack -> 'a stack result


(*
   Example.
   Assume int is of type ('a,int) getter 
 *)

   let machine =
     [ ....
     ]

*)
