
(* Values on the stack have type 'a. 
 * Note: 'a can be a union type, e.g.  Int of int | Bool of bool *)
type 'a stack

(* To ease the writing of user operators, the user first creates getters from 'a to 'b. 
 * e.g. getter from (Int of int | Bool of bool) to int.  A getter may fail if the data is not of the expected dynamic type. 
 * A getter returns an 'a result. *)
type 'a result = Ok of 'a | Failed of string

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

type (_, _) acu

val def: string -> 'a -> ('b -> 'c stack -> 'c stack * 'b, 'a) acu

val (+>): ('a -> 'b stack -> 'c stack * ('d -> 'e), 'c -> 'd result) acu -> 'f -> ('a -> 'b stack -> 'c stack * 'e, 'f) acu

type ('b, 'c) def

val (+==): ('a -> 'b stack -> 'c stack * 'd, ('d, 'c) ret_value) acu -> 'a -> string -> ('b, 'c) def



(*



(* The different (dynamic) types of 'a values is represented by a string, e.g "int", "bool". *)
type typ = string

(* A stack machine, handling values of type 'a. *)
type 'a machine = 'a def list

(* An operator tries to operate on the stack. If it fails, it returns an error message. *)
and 'a def = 'a stack -> 'a stack result


(*
   Example.
   Assume int is of type ('a,int) getter 
 *)

   let machine =
     [ def "plus"  int +> int +> ["int"] @== myplus ;
       def "odd"   int +> ["bool"] @== myodd ;
       def "k"     ["int"] @== myk ;
       def "times" int +> int +> "int" @== mytimes ;
     ]

type ('a,'b) wrapper

val def: string -> 'b -> ('d -> 'd, 'b) wrapper

val (+>): ('d -> 'a -> 'd, 'b) wrapper -> 'c -> ('e -> 'a -> 'b -> 'e, 'c) wrapper

(*
(* Create a machine by providing a typing function. *)
val typing: ('a -> 'b) -> ('a,'b) def list -> ('a, 'b) machine

val (<=>): ('a, 'b) machine -> string -> ('b -> ('a, 'b) kont)
val 
                                        


(* Type of operations available in the machine.
 * 'b: type of stack arguments (pops) / return values (pushs)
 * 'c: type of the real operator (... -> 'a list), returns the list of pushs.
 * *)
type ('a, 'b, 'c) op_type =
  {
    (* Command name. Overriding is possible. Dispatch according to stack-pop types. *)
    command: string ;

    (* Number of arguments expected on the command line. *)
    nargs: int ;
    
    (* PRE: pops, in this order. *)
    pops: 'b list ;

    (* POST: pushs, in this order. *)
    pushs: 'b list ;

    (* Wrapper *)
    wrap: 'c -> ('a stack -> 'a stack) }

val add_op: ('a,'b) machine -> ('a,'b,'c) op_type -> 'c -> ('a, 'b) machine


(* Helper to build op_types *)
type ('a,'b,'c) builder

val tt: 'b -> 'b builder
  
val (+>): 'b builder -> 'b -> 'b builder
val (+=>): 'b builder -> 'b list -> (...) wrapper

let plus_type = tt int +> int +=> [ int ]

*)
*)
