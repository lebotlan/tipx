open Cpslib

(* This module eases the writing of s-functions (stack-functions),
 * that is, functions poping their arguments from a stack (or, from command-line arguments) and pushing a result to the stack.
 *
 * It also provides a ready-to-run program that executes in sequence the instructions found on the command-line.
*)

(* First step: instantiate the functor below by providing the necessary types. *)
module type TYPES =
sig
  (* Values on the stack have the type elt.
   *
   * It can be a union type, e.g.  Int of int | Bool of bool *)
  type elt

  (* Optionally, extra information (of type info) can be passed through the s-functions,
   * it can be used to keep global information, such as stats, options, ... 
   *
   * If unneeded, define type info = unit *)
  type info

  val elt2s: elt -> Term2.styled_text
  
end


(* To ease the writing of new operators, the user first creates getters from elt to a type 'a. 
 * e.g. a getter from (Int of int | Bool of bool) to int.  A getter may fail if the data is not of the expected dynamic type. 
 * A getter returns an 'a result. *)
type 'a res = Ok of 'a | Failed of string

exception Error of string

module Mk: functor (Types: TYPES) ->
sig

  type elt = Types.elt
  type info = Types.info

  val elt2s: elt -> Term2.styled_text
                      
  type stack = elt list

  type 'a result = 'a res = Ok of 'a | Failed of string

  (* A user function *)
  type def

  (* A machine contains a list of entries. *)
  type entry =
    | Def of def
    | Title of string
    | Info of string

  (* A stack machine, handling values of type 'a. *)
  type machine = entry list
  
  (* Type of states handled by s-functions. 
   * Basically, a s-function is of type state -> state. 
  *)
  type state =
    { stack: stack ;
      cmd_line: string list ;
      info: info ;
      machine: machine ;
      
      (* Current command been processed. *)
      current: def option }

  (* Sordid type used to implement CPS below. *)
  type (_, _) acu    

  (* Direct definition. One may also define s-functions using CPS style, next. *)
  val raw_def: ?implicit:bool -> name:string -> (state -> state result Lwt.t) -> descr:string -> dtype:string -> entry

  (* Implicit: 
   *     - only a function with at least (exactly?) one cmd-line argument can be implicit
   *     - it will be applied without the function name on the command-line, whenever no explicit function applies,
   *       and provided the cmd-line argument(s) is (are) compatible.
  *)

  (* The following functions are meant to be used in CPS, hence the type are just unreadable. Do not try. 
   * Read the examples instead. *)

  (* Starts a cps definition  def "function-name" ... *)
  val def: string -> (('a -> 'b -> 'b * 'a, 'c -> unit result) acu, 'd) kont -> 'd

  type ('a,'b) wrapper    = 'a -> state -> state * 'b
  
  (* Options *)
  (* val implicit : (('a, 'b) acu, (('a, 'b) acu -> 'c, 'c) kont) kont *)
  val implicit : (('a, 'b) acu, (('a, 'b) acu, 'c) kont -> 'c) kont
  
  (* get_info: the final function receives the info argument at this point. *)
  val get_info: ((('a, 'b -> 'c) wrapper, elt -> 'b result) acu,
                 (((('a, 'c) wrapper, 'd -> info result) acu, 'e) kont) -> 'e) kont

  (* Describes the type of the function e.g.  !-> int !-> int !-> int   where int is of type stack_element -> int result   *)

  (* Shifts the last element into the function arguments. 
   * 'b is the type of the real function. 
   *  The wrapper is of type: (type of real function) -> state -> state * (type of remaining-function == partially applied function) *)
  val ( !-> ): 'z -> ((('b,'e -> 'f) wrapper, elt -> 'e result) acu,
                      ((('b,'f) wrapper, 'z) acu, 'k) kont -> 'k) kont

  (* Like !->, but the argument comes from the command line, not from the stack. *)
  val ( !=> ) : (string -> 'a) -> ((('b,'g -> 'h) wrapper, elt -> 'g result) acu,
                                   ((('b,'h) wrapper, 'i -> 'a) acu, 'j) kont -> 'j) kont

  (* Like !=>, but the reader also receives the info argument *)
  val ( !==> ) : (info -> string -> 'a) -> ((('b,'g -> 'h) wrapper, elt -> 'g result) acu,
                                            ((('b,'h) wrapper, 'i -> 'a) acu, 'j) kont -> 'j) kont                 

  (* Can only be used for the last type (the return type of the function): the function returns a couple (a,b) which pushes two elements on the stack (push a ; push b) *)
  val ( !->> ): ('a -> elt) -> ((('c,'h -> 'i) wrapper, elt -> 'h result) acu,
                                ('j -> elt) -> ((('c,'i) wrapper, 'a * 'j -> elt) acu, 'k) kont -> 'k) kont

  (* Like !->> for a triple (a,b,c) *)
  val ( !->>> ): ('a -> elt) -> ((('c,'h -> 'i) wrapper, elt -> 'h result) acu,
                                 ('j -> elt) -> ('k -> elt) -> ((('c,'i) wrapper, 'a * 'j * 'k -> elt) acu, 'l) kont -> 'l) kont

  (* Similar to !->>, but the function returns a list of values, which are mapped by f and pushed on the stack. *)
  val ( !-::> ): ('a -> elt) -> (( ('b,'c -> 'd) wrapper, elt -> 'c result) acu,
                                 (( ('b, 'd) wrapper, 'a list -> elt) acu, 'e) kont -> 'e) kont


  (* Similar to !->, provide a mapper element -> 'a, then the function will be applied with an argument of type (int -> 'a list),
   * The int argument is the number of argument to be fetched. *)
    (*
val (!-<>): ('a -> 'b result) -> (('c -> 'd -> 'e state * ('f -> 'g), 'e -> 'f result) acu,
                                  (('c -> 'd -> 'e state * 'g, 'a -> (int -> 'b list) result) acu, 'h) kont -> 'h) kont
      *)

  (* No return type: the function returns unit, but nothing is pushed on the stack. 
   * One writes 'nop' as a return type, not !-> nop. *)
  val nop : ((('a,'f -> 'g) wrapper, elt -> 'f result) acu,
             ((('a,'g) wrapper, unit -> elt) acu, 'i) kont -> 'i) kont

  (* Inserts a unit argument. 
   * One writes 'unit' as an argument type, not !-> unit. *)
  val unit :
    unit ->
    ('a -> state -> state * ('b -> 'c), elt -> 'b result) acu ->
    (unit -> ('a -> state -> state * 'c, 'd -> unit result) acu -> 'e) ->
    'e

  (* Terminates the type definition, is followed by the real function (with the expected type) and a description, 
   * e.g.  !-> int !-> int :== succ "Successor" *)

  val ( !== ):  'a -> (((unit -> 'a, 'd) wrapper, 'd -> elt) acu, string -> entry) kont

  (* lwt version *)
  val ( !=% ):  'a -> (((unit -> 'a, 'd Lwt.t) wrapper, 'd -> elt) acu, string -> entry) kont

  (* Like (!==), the real function returns a pair (result-value, new-info).  *)
  val ( !==+ ): 'a -> (((unit -> 'a, 'b * info) wrapper, 'b -> elt) acu, string -> entry) kont

  val ( !=%+ ):  'a -> (((unit -> 'a, ('d * info) Lwt.t) wrapper, 'd -> elt) acu, string -> entry) kont
  
  (* MAP, JOIN machines ????????  pour faire map il faut une bijection 'a -> 'b  et le module STACKCL BIS.*)

  (* Execute the machine with the given command-line arguments.
   * The index is the start index in the array (default:1). 
   * init: initial value of the stack (default:empty) 
   * info: initial value of info
   * warn_not_empty: prints a warning if the stack is not empty at the end. *)
  val exec: machine -> ?init:stack -> string array -> ?index:int -> ?warn_not_empty:bool -> ?show_usage_if_no_args:bool -> info -> unit -> state Lwt.t

  val show_usage: machine -> unit Lwt.t

  (* In order to have a nicer usage message, one may put titles as entries. *)
  val title: string -> entry

  val info: string -> entry

  (* Predefined definitions, that you may insert into your machine. *)

  (* All predefined value can be renamed and have their description changed. *)
  type predef = ?name:string -> ?descr:string -> unit -> entry

  (* Duplicate the topmost stack element. *)
  val dup: predef

  (* Swaps the two topmost stack elements. *)
  val swap: predef

  (* Pops and prints the topmost stack element. *)
  val print: predef

  (* Prints the topmost stack element. No pop. *)
  val peek: predef

  (* Discards the topmost stack element. *)
  val pop: predef

  (* Show uage. *)
  val help: predef
  
end


(* TODO - FUTURE
 *
 *
 *  La pile n'est pas suffisante pour une appli plus complexe (fonctions manipulant plusieurs arguments non homogènes)
 *
 *  Pourquoi pas un dictionnaire (clefs => piles de data)   == environnement ou soupe  : on peut faire pop ou peek sur un nom / push sur un nom
 *
 *  
 * 
 *)
