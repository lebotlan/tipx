(*** Example using the Stackcl library:
 *
 *   We build a stack-based, command-line controled application.
 *   This is called a 'machine'.
 *)

(* Type of the stack elements. *)
type value = Int of int | String of string

(* Instantiate functor. *)
module St = Stackcl.Mk
    (struct
      type elt = value
        
      type info = unit  (* info is not illustrated in these examples. *)

      (* Printer for stack elements. *)
      let elt2s = function
        | Int x -> string_of_int x
        | String s -> "\"" ^ s ^ "\""
    end)
    
open St

(*** A few helper functions. ***)

let id x = x

(* Reads an int from the stack. *)
let int = function
  | Int x -> Ok x
  | _ -> Failed "Not an int"

(* Reads an int from a (string) command-line argument. *)
let cl_int x = try Ok (int_of_string x) with  _ -> Failed "Not an int"

(* Read a string from the command-line. *)
let cl_string x = Ok x

(* Reads any value from the stack. *)
let any x = Ok x

(* Builds an Int, to be put on the stack. *)
let b_int x = Int x

(* Builds a String. *)
let b_string s = String s


(*** Functions to be put in the machine. ***)
let print x = Lwt_io.printf "%s\n" (elt2s x)

let greet () = Lwt_io.printf "Greetings.\n"

let machine =
  [

    title "Push values" ;
    
    (* Push takes a command-line integer and pushes it on the stack. 
     *   !=> takes a command-line argument
     *   !-> builds a return value
     *
     *   ./myapp  push 40 push 30
     *)
    def "push"  !=> cl_int !-> b_int                 !== id  "Pushes the given integer on the stack." ;

    (* For convenience, this version of push is implicit: one may directly write integers on the command-line. 
     *
     *   ./myapp  40 30
     *)
    def "push" implicit !=> cl_int !-> b_int         !== id   "Pushes the given integer on the stack." ;

    (* Likewise, one may directly put strings on the command-line.
     * In case of conflict, the first implicit def that succeeds wins.
     *
     *   ./myapp  40 30 foo
     *)
    def "push" implicit !=> cl_string !-> b_string   !== id   "Pushes the given string on the stack." ;


    title "Print stuff" ;
    
    (* Prints the element on top of the stack. 
     * The stack argument can be of any type. 
     * It consumes a stack element, but does not produce new elements (nop). 
     *
     *   ./myapp  40 30 print foo print print
     * 
     *   prints: 30 "foo" 40
     *
     *   !-> introduces an argument reader (here any)
     *   There is no return value (nop)
     *
     * Notice that 'print' is predefined, so one may just write  print ()  instead of the following line of code.
     *)
    def "print" !-> any  nop                         !=% print "Pops and prints the element on top of the stack." ;

    (*   ./myapp  greet greet greet         displays the greeting message three times. *)
    def "greet" unit nop                             !=% greet "Displays a greeting message." ;
    
    (* Like print,  but pushes back the read element. 
     * peek is predefined *)
    peek () ;

    title "Arithmetic operations" ;
    
    (* 
     *   ./myapp  40 30 add print
     *   ./myapp  100 40 30 add sub 4 mul print         (prints 120)
     *
     *  The two leftmost !-> introduce argument readers.
     *  The last !-> introduces the return value builder.
     *
     *  Note that the construction !-> ... !-> ...  is strongly typed. 
     *  You MUST pass a function with the expected type int -> int -> int   (e.g, +. would not typecheck).
     *)
    def "add" !-> int !-> int !-> b_int              !== ( + )   "Adds the two topmost stack elements. Pushes the result." ;
    def "mul" !-> int !-> int !-> b_int              !== ( * )   "Multiplies the two topmost stack elements. Pushes the result." ;

    (* The arguments are passed to the function according to the 'pop' order.
     * I prefer   100 50 sub  to be interpreted as 100 - 50 (instead of 50 - 100), so I reverse the order of the arguments. *)
    def "sub" !-> int !-> int !-> b_int              !== (fun x y -> y - x)   "Substract the two topmost stack elements. Pushes the result." ;

    (* We take an argument from the command-line. 
     *
     *   ./myapp  10 times 3 print          (prints 30)
     *)
    def "times" !-> int !=> cl_int !-> b_int         !== ( * ) ("Multiplies the top stack element with the given command-line constant.\n" ^
                                                                "Additionally, this is also an example of a multi-line description that, hopefully,\n" ^
                                                                "should be nicely typeset.") ;

    (* This function returns two values, which are pushed on the stack. 
     *
     *   ./myapp  47 10 div print print     (prints 7 4)  
     *)
    def "div"  !-> int !-> int !->> b_int b_int      !== (fun x y -> (y/x, y mod x))   "Pushes the division and remainder of the two topmost stack elements." ;


    title "Misc. operations" ;
    
   (* Dup and pop are predefined, although they could easily be defined from scratch.
    *
    *    ./myapp  15 dup add print          (prints 30)
    *    ./myapp  15 pop                    (the stack is now empty)
    *)
    dup () ;
    pop () ;
    
    (* Although swap is predefined, we may define it like this: *)
    def "swap" !-> any !-> any !->> id id            !== (fun x y -> (x,y)) "Swaps the two topmost stack elements." ;

    (* This function returns a list of elements, to be pushed on the stack. 
     *
     *   ./myapp  dupn 4       (pops and duplicates 4 times the topmost element). *)
    def "dupn" !=> cl_int !-> any !-::> id           !== (fun k x -> List.init k (fun _ -> x)) "Pops and duplicates n times the topmost stack element." ;

    (* Prints the n topmost elements. *)
    (*    def "printn" !=> cl_int                         !== () "Prints the n topmost elements." ; *)
  ]

open Lwtlaunch.Setkeys
let run () =
  let%lwt _ = exec machine Sys.argv () () in
  Lwt.return_unit

let () = noconfig ===> Lwtlaunch.launch ~appname:"teststack" ~run ()

