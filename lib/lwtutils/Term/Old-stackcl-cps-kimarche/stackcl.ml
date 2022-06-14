open Cps

type 'a stack = 'a list

type 'a result = Ok of 'a | Failed of string

exception Stack_error of string

let stack_error s = raise (Stack_error s)

let get_arg getter = function
  | [] -> stack_error "Empty stack"
  | x :: xs ->
    begin match getter x with
      | Ok v -> v, xs
      | Failed s -> stack_error s
    end

(* 'a acuoption: 'a is the type contained in the option. 
 *                when it is none, it is identified with a unit argument (which is implicitly passed as a first argument of every function)
 * *)

type _ acuoption =
  | Acu_none: ('a -> unit result) acuoption
  | Acu_some: 'a -> 'a acuoption

type ('a,'b) acu = {
  name: string ;
  wrapper: 'a ;
  last: 'b acuoption ; 
}

type ('a, 'b) ret_value =
  { ret_wrap: 'a -> 'b stack -> 'b stack }

type 'b def =
  { app: 'b stack -> 'b stack ;
    descr: string }

let unit = { ret_wrap = (fun () s -> s) }
let r0 = unit
let r1 w1 = { ret_wrap = (fun v1 s -> w1 v1 :: s) }
let r2 w1 w2 = { ret_wrap = (fun (v1,v2) s -> w2 v2 :: w1 v1 :: s) }
let r3 w1 w2 w3 = { ret_wrap = (fun (v1,v2,v3) s -> w3 v3 :: w2 v2 :: w1 v1 :: s) }

let def name = mk_start { name ;
                          wrapper = (fun f stack -> (stack, f)) ;
                          last = Acu_none }

let insert (type a) (type b) arg (acu:(_, a -> b result) acu) =
  match acu.last with
  | Acu_none ->  { name = acu.name ;
                   wrapper =
                     begin fun f ->
                       let f2 = acu.wrapper f in

                       let f3 stack =
                         let (stack2, f2b) = f2 stack in
                         (stack2, f2b (():b))
                       in

                       f3
                     end ;

                   last = Acu_some arg }

  | Acu_some last ->
    { name = acu.name ;
      wrapper =
        begin fun f ->
          let f2 = acu.wrapper f in

          (* f2 stack -> stack', fun arg1 ... argn -> result
           *                          reads its k first arguments from the stack and gets n next arguments. 
           *                          it returns a result and a new stack stack2
           * 
           * we must transform it into 
           *
           * f3 stack -> stack'', fun arg2 ... argn -> result
           *                          reads its k+1 first arguments from the stack and gets n-1 next arguments. 
           *                          it returns a result and a new stack stack3 *)

          let f3 stack =
            let (stack2, f2b) = f2 stack in
            let (arg1, stack3) = get_arg last stack2 in
            (stack3, f2b arg1)
          in

          f3
        end ;

      last = Acu_some arg ; }

let (!->) arg = mk_op0 (fun acu -> insert arg acu)

let finish (type a) (type b) f (acu:(_,(a,b) ret_value) acu) descr =
  match acu.last with
  | Acu_some last ->

    let f2 = acu.wrapper (fun () -> f) in

    let f3 (stack:_ stack) =
      let (stack2, result) = f2 stack in
      last.ret_wrap result (stack2: _ stack)
    in

    { app = f3 ;
      descr }

let (!==) arg = mk_end (fun acu descr -> finish arg acu descr)
