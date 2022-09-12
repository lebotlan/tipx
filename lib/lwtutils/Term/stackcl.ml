open Cpslib
open Term2

module type TYPES =
sig
  type elt
  type info
  val elt2s: elt -> styled_text
end

type 'a res = Ok of 'a | Failed of string

exception Error of string

module Mk = functor (Types: TYPES) ->
struct

  type elt = Types.elt
  type info = Types.info

  type stack = elt list

  type 'a result = 'a res = Ok of 'a | Failed of string

  type state =
    { stack: stack ;
      cmd_line: string list ;
      info: info ;
      machine: machine ;
      current: def option }

  and def =
    { name: string ;
      app: state -> state Lwt.t ;
      descr: string ;
      implicit: bool ;
      dtype: string }

  and entry =
    | Def of def
    | Title of string
    | Info of string

  and machine = entry list


  type ('b,'e) wrapper = 'b -> state -> state * 'e

  let myerror s = raise (Error s)

  let elt2s = Types.elt2s

  
  let title s = Title s

  let info s = Info s

  let raw_def ?(implicit=false) ~name f ~descr ~dtype =
    let app state =
      match%lwt f state with
      | Ok s -> Lwt.return s
      | Failed s -> myerror s
    in
    Def { name ; app ; descr ; implicit ; dtype }

  (* Generic get_arg from the stack. *)
  let get_arg getter state = match state.stack with
    | [] -> myerror "Empty stack"
    | x :: xs ->
      begin match getter x with
        | Ok v -> v, { state with stack = xs }
        | Failed s -> myerror s
      end

  (* Value stored in the 'last' slot. 
   *   'a acuoption : 'a  is not necessarily the type of a value, it is a conventional type.
  *)

  type _ acuoption =
    (* Expects a unit argument. The initial acu contains Unit_arg (the initial unit will be passed automatically) *)
    | Unit_arg: ('a -> unit result) acuoption

    (* Standard arrow: stores the argument as is. *)
    | Std_arrow: 'a -> 'a acuoption

    (* Return arrow, returning a list. 
     * The type 'a -> 'b does not mean anything. 
     * It is just a way to localize 'a and 'b. *)
    | Ret_list:  string * ('a -> elt list) -> ('a -> elt) acuoption

    (* Command-line argument arrow. *)
    | Cmd_arrow: (string -> 'b) -> ('a -> 'b) acuoption
    | Cmd_info_arrow: (info -> string -> 'b) -> ('a -> 'b) acuoption

    | With_info: ('a -> info result) acuoption
    | With_machine: ('a -> machine result) acuoption

  (* Get-list arrow. *)
  (*  | Get_list: ('a -> 'b result) -> ('a -> (int -> 'b list) result) acuoption *)


  (* acu for CPS construction.
   *   wrapper ('a) : type of the function which maps the user function to a s-function. 
   *   last : type of the last CPS element. If another element is inserted, this last element is shifted into the wrapper.
  *)
  type ('a,'b) acu = {
    a_name: string ;
    a_implicit: bool ;
    wrapper: 'a ;
    last: 'b acuoption ;
    atype: string ;
  }

    (* Generic get_arg from the command-line *)
  let get_cmd_arg reader state = match state.cmd_line with
    | [] ->  myerror "No more command-line arguments."
        
    | x :: xs ->

      let fread = match reader with
        | Cmd_arrow f -> (fun _ x -> f x)
        | Cmd_info_arrow f -> f
        | _ -> assert false
      in
      
      begin match fread state.info x with
        | Ok v -> v, { state with cmd_line = xs }
        | Failed s -> myerror s
      end


  let def a_name =
    mk_start { a_name ;
               a_implicit = false ;
               wrapper = (fun f state -> (state, f)) ;
               last = Unit_arg ;
               atype = "" }

  let implicit () acu k = k () { acu with a_implicit = true }

  let insert (type b) new_last (acu:(_, elt -> b result) acu) =
    match acu.last with
    | Ret_list _ -> assert false (* Not supposed to insert anything after the result. *)

    | Unit_arg ->  { a_name = acu.a_name ;
                         a_implicit = acu.a_implicit ;
                         wrapper =
                           begin fun f ->
                             let f2 = acu.wrapper f in

                             let f3 (state: state) =
                               let (state2, f2b) = f2 state in
                               (state2, f2b (():b))
                             in

                             f3
                           end ;

                         atype = acu.atype ;
                         last = new_last }

    | With_info ->  { a_name = acu.a_name ;
                      a_implicit = acu.a_implicit ;
                      wrapper =
                        begin fun f ->
                          let f2 = acu.wrapper f in

                          let f3 state =
                            let (state2, f2b) = f2 state in
                            (state2, f2b (state.info:b))
                          in

                          f3
                        end ;

                      atype = acu.atype ;
                      last = new_last }

    | With_machine ->  { a_name = acu.a_name ;
                         a_implicit = acu.a_implicit ;
                         wrapper =
                           begin fun f ->
                             let f2 = acu.wrapper f in

                             let f3 state =
                               let (state2, f2b) = f2 state in
                               (state2, f2b (state.machine:b))
                             in

                             f3
                           end ;

                         atype = acu.atype ;
                         last = new_last }

    | Std_arrow last ->
      { a_name = acu.a_name ;
        a_implicit = acu.a_implicit ;
        wrapper =
          begin fun f ->
            let f2 = acu.wrapper f in

            (* f2 state -> state', fun arg1 ... argn -> result
             *                          reads its k first arguments from the stack and gets n next arguments. 
             *                          it returns a result and a new state state2
             * 
             * we must transform it into 
             *
             * f3 state -> state'', fun arg2 ... argn -> result
             *                          reads its k+1 first arguments from the stack and gets n-1 next arguments. 
             *                          it returns a result and a new state state3 *)

            let f3 state =
              let (state2, f2b) = f2 state in
              let (arg1, state3) = get_arg last state2 in
              
              (state3, f2b arg1)
            in

            f3
          end ;

        atype = acu.atype ^ "p" ;
        last = new_last ; }

    | (Cmd_arrow _ | Cmd_info_arrow _) as reader ->
      { a_name = acu.a_name ;
        a_implicit = acu.a_implicit ;
        wrapper =
          begin fun f ->
            let f2 = acu.wrapper f in

            let f3 state =
              let (state2, f2b) = f2 state in
              let (arg1, state3) = get_cmd_arg reader state2 in
              (state3, f2b arg1)
            in

            f3
          end ;

        atype = acu.atype ^ "r" ;
        last = new_last ; }
(*
  | Get_list arg ->
    { a_name = acu.a_name ;
      a_implicit = acu.a_implicit ;
      wrapper =
        begin fun f ->
          let f2 = acu.wrapper f in

          let f3 state =
            let (state2, f2b) = f2 state in

            let cb n = ... in

            let (arg_list, state3) = get_arg_list arg state2 in     -----attention c'est compliqué, on a un cb ici------



            (state3, f2b cb)
          in

          f3
        end ;

      last = new_last ; }
  *)  

  let unit () acu k = k () (insert Unit_arg acu)
  
  let (!->) arg = mk_op0 (fun acu -> insert (Std_arrow arg) acu)

  let (!->>) arg1 = mk_op1 (fun acu arg2 -> insert (Ret_list ("2", (fun (x,y) -> [arg1 x ; arg2 y]))) acu)

  let (!->>>) arg1 = mk_op2 (fun acu arg2 arg3 -> insert (Ret_list ("3", (fun (x,y,z) -> [arg1 x ; arg2 y ; arg3 z]))) acu)

  let (!-::>) arg = mk_op0 (fun acu -> insert (Ret_list ("*", (fun l -> List.map arg l))) acu)

  let nop () acu k = k () (insert (Ret_list ("0", (fun () -> []))) acu)

  let (!=>) arg = mk_op0 (fun acu -> insert (Cmd_arrow arg) acu)

  let (!==>) arg = mk_op0 (fun acu -> insert (Cmd_info_arrow arg) acu)

  let get_info () acu k = k () (insert With_info acu)

  let get_machine () acu k = k () (insert With_machine acu)

  (*let (!-<>) arg = mk_op0 (fun acu -> insert (Get_list arg) acu) *)

  let finish (type a) final_wrap f (acu:(_, a -> elt) acu) descr =

    let f2 = final_wrap (acu.wrapper (fun () -> f)) in

    match acu.last with
    | Unit_arg | Cmd_arrow _ | Cmd_info_arrow _ -> assert false
    | With_info | With_machine (*| Get_list _ *) -> assert false (* You are not supposed to finish a function without a return type. *)

    | Std_arrow last ->

      let wrap_res x s = Lwt.return { s with stack = last x :: s.stack } in

      let f3 state =
        let%lwt (state2, result) = f2 state in
        wrap_res result state2
      in

      Def { app = f3 ;
            implicit = acu.a_implicit ;
            name = acu.a_name ;
            dtype = acu.atype ^ " -> 1" ;
            descr }

    | Ret_list (sym, last) ->

      let wrap_res x s = Lwt.return { s with stack = List.rev_append (last x) s.stack } in

      let f3 state =
        let%lwt (state2, result) = f2 state in
        wrap_res result state2
      in

      Def { app = f3 ;
            implicit = acu.a_implicit ;
            name = acu.a_name ;
            dtype = acu.atype ^ " -> " ^ sym ;
            descr }


  let (!==) arg = mk_end (fun acu descr -> finish (fun g -> (fun st -> Lwt.return (g st))) arg acu descr)

  let (!=%) arg =
    let final_wrap g =
      (fun st -> let (st2, res) = g st in
        let%lwt res = res in
        Lwt.return ( st2, res))
    in
    mk_end (fun acu descr -> finish final_wrap arg acu descr)

  let (!==+) arg =
    let final_wrap g =
      (fun st -> let (st2, (res, info)) = g st in Lwt.return ( { st2 with info }, res))
    in
    mk_end (fun acu descr -> finish final_wrap arg acu descr)

  let (!=%+) arg =
    let final_wrap g =
      (fun st -> let (st2, resi) = g st in
        let%lwt (res, info) = resi in
        Lwt.return ( { st2 with info }, res))
    in
    mk_end (fun acu descr -> finish final_wrap arg acu descr)


  let get_explicit machine =
    let table = Assoc.create ~init:(fun _ -> []) () in
    List.iter (function Title _ | Info _ -> () | Def def -> (* if not def.implicit then *) Assoc.update table def.name (fun l -> def :: l)) (List.rev machine) ;
    table

  let check_empty state =
    match state.stack with
    | [] -> Lwt.return_unit (* All right *)
    | el :: _ ->
      let len = List.length state.stack in

      if len > 1 then
        Styled.(p
                  fmt yellow "*Warning* Final stack is not empty, it still contains %s elements. The topmost element is:\n"
                  (match len with 2 -> "two" | 3 -> "three" | n -> string_of_int n)
                  st (elt2s el)
                  nl
                  e)

      else
        Styled.(p fmt yellow "*Warning* Final stack is not empty, it still contains:\n"
                  st (elt2s el)
                  nl
                  e)

  let rec apply_explicit cmd tries state = function
    | [] -> assert false
    | def :: rest ->

      begin try def.app { state with current = Some def } with
        | e ->
          if rest = [] then
            let s = Printexc.to_string e in

            let msg2 = def.name ^ ": " ^ def.descr in
            
            (* This was the last one. *)
            let msg =
              Printf.sprintf "Command '%s' cannot be applied.\n%s" cmd
                (if tries > 0 then Printf.sprintf "I have tried %d versions of '%s'. None has succeeded. The last one said: %s.\n\n%s" (tries+1) cmd s msg2
                 else "Error is: " ^ s ^ "\n\n" ^ msg2)
            in
            myerror msg

          else apply_explicit cmd (tries+1) state rest
      end

  let rec apply_implicit arg state = function
    | [] -> myerror ("I don't know what to do with '" ^ arg ^ "' found on the command-line.")
    | (Title _ | Info _) :: rest -> apply_implicit arg state rest
    | Def def :: rest ->
      if not def.implicit then apply_implicit arg state rest
      else
        (* Try this one *)
        begin try def.app state with
          | Error _ -> apply_implicit arg state rest
        end

  (*** Pretty-print USAGE ***)

  type len =
    { (* Max size of function names. *)
      len1: int ;

      (* Max size of function dtypes. *)
      len2: int ;

      (* Max size of function descr. *)
      len3: int }

  let max_len len = function
    | Title _ | Info _ -> len
    | Def def ->
      { len1 = max len.len1 (String.length def.name) ;
        len2 = max len.len2 (String.length def.dtype) ;
        len3 = List.fold_left (fun x s -> max x (String.length s)) len.len3 (String.split_on_char '\n' def.descr) }

  let get_len machine = List.fold_left max_len { len1 = 2 ; len2 = 2 ; len3 = 2 } machine
  
  let tolen n s =
    let len = String.length s in
    if len > n then s
    else s ^ (String.make (n - len) ' ')

  let show_def len def =
    let name = tolen len.len1 def.name in

    let margin = String.make (len.len1 + len.len2 + 34) ' ' in    
    let descr = String.concat ("\n" ^ margin) (String.split_on_char '\n' def.descr) in
    
    Styled.(b std "   "
              lwhite name
              dgray "  : "
              lgray (tolen len.len2 def.dtype)
              std "   "
              dcyan (if def.implicit then "(implicit) " else "╌╌╌╌╌╌╌╌╌╌╌")
              dcyan "╌╌╌╌╌╌╌╌╌  "
              cyan descr
              e)

  let repeat n s =
    let rec loop n acu =
      if n <= 0 then acu
      else loop (n-1) (acu ^ s)
    in
    loop n ""
      
  let show_entry len = function
    | Def def -> show_def len def
    | Info s -> Styled.(b lorange ("  " ^ s) nl e)
    | Title s ->
      let title_width = len.len1 + len.len2 + len.len3 in
      
      let line = " -~-~- " ^ s ^ " " in
      let len = String.length line in
      let missing = title_width - len in
  
      let line =
        if missing >= 0 then
          line ^ repeat (missing / 2) "-~" ^ (if missing mod 2 = 0 then "" else "-")
        else line
      in

      Styled.(b nl nl
                magenta line nl
                e)

  let show_usage machine =
    let len = get_len machine in

    let appname = Filename.basename Sys.argv.(0) in
    
    let text = Styled.(b
                         lgray "Usage:" nl
                         std ("  " ^ appname ^ " command1 command2 ... commandn") nl
                         nl
                         std "  It operates on a stack." nl
                         std "  The commands take their arguments from the stack and/or from the command-line." nl
                         nl
                         lgray "  p means 'pop' (stack argument), r means 'reads a command-line argument'" nl
                         lgray "  1 means pushes one value, * means pushes multiple values" nl nl
                         st (Term2.sep (show_entry len) !^"\n" machine)
                         nl nl
                         e)
    in
    Term2.fprints text ;%lwt 

    Lwt.return_unit

  let exec machine ?(init=[]) cl_args ?(index=1) ?(warn_not_empty=true) ?(show_usage_if_no_args=true) info () =

    let init_state =
      { stack = init ;
        machine ;
        cmd_line = Array.to_list (Array.sub cl_args index (Array.length cl_args - index)) ;
        info ;
        current = None }
    in

    if init_state.cmd_line = [] then
      let%lwt () = if show_usage_if_no_args then show_usage machine else Lwt.return_unit in
      Lwt.return init_state

    else
      (* All explicit command names *)
      let explicit_names = get_explicit machine in

      (* Iterate on the command line arguments. *)    
      let rec loop state =
        match state.cmd_line with
        | [] -> 
          (* Finish *)
          let%lwt () = if warn_not_empty then check_empty state else Lwt.return_unit in
          Lwt.return state

        | cmd :: rest -> 
          (*** Process next command ***)

          let%lwt next_state =
            (* Is it a known explicit command ? *)
            if Assoc.mem explicit_names cmd then
              apply_explicit cmd 0 { state with cmd_line = rest } (Assoc.get explicit_names cmd)

            else apply_implicit cmd state machine
          in
          loop next_state

      in
      loop init_state


  (*** Predefined functions ***)
  type predef = ?name:string -> ?descr:string -> unit -> entry

  let any x = Ok x
  let id x = x

  let print_value x = Term2.fprints (elt2s x +^ "\n")

  let dup ?(name="dup") ?(descr="Duplicate the argument on top of the stack.") () =
    def name !-> any !->> id id !== (fun x -> (x,x)) descr

  let swap ?(name="swap") ?(descr="Swaps the two topmost stack elements.") () =
    def name !-> any !-> any !->> id id !== (fun x y -> (x,y)) descr

  let print ?(name="print") ?(descr="Pops and prints the topmost stack element.") () =
    def name !-> any nop !=% print_value descr

  let peek ?(name="peek") ?(descr="Prints the topmost stack element. No pop.") () =
    def name !-> any !-> id !=% (fun x -> print_value x ;%lwt Lwt.return x) descr

  let pop  ?(name="pop") ?(descr="Discards the topmost stack element.") () =
    def name !-> any nop !== (fun _ -> ()) descr

  let help  ?(name="help") ?(descr="Display this help.") () =
    def name get_machine nop !=% (fun m -> show_usage m) descr

end
