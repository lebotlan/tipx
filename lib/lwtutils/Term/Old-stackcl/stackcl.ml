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

type ('a,'b) acu = {
  name: string ;
  wrapper: 'a ;
  last: 'b ;
}

type ('a, 'b) ret_value =
  { ret_wrap: 'a -> 'b stack -> 'b stack }

type ('b, 'c) def =
  { app: 'b stack -> 'c stack ;
    descr: string }

let unit = { ret_wrap = (fun () s -> s) }
let r0 = unit
let r1 w1 = { ret_wrap = (fun v1 s -> w1 v1 :: s) }
let r2 w1 w2 = { ret_wrap = (fun (v1,v2) s -> w2 v2 :: w1 v1 :: s) }
let r3 w1 w2 w3 = { ret_wrap = (fun (v1,v2,v3) s -> w3 v3 :: w2 v2 :: w1 v1 :: s) }

let def name arg =
  { name ;
    wrapper = (fun f stack -> (stack, f)) ;
    last = arg }

let (+>) acu last =
  { name = acu.name ;
    wrapper =
      begin fun f ->
        let f2 = acu.wrapper f in
        
        (* f2 stack arg1 ... argn   reads its k first arguments from the stack and gets n next arguments. 
         *                          it returns a result and a new stack stack2
         * 
         * we must transform it into 
         *
         * f3 stack arg2 ... argn  reads its k+1 first arguments from the stack and gets n-1 next arguments. 
         *                         it returns a result and a new stack stack3 *)

        let f3 (stack:_ stack) =
          let (stack2, f2b) = f2 stack in
          let (arg1, stack3) = get_arg acu.last (stack2: _ stack) in
          ((stack3:_ stack), f2b arg1)
        in

        f3
      end ;
          
    last ; }
  
let (+==) acu f descr =
  let f2 = acu.wrapper f in

  let f3 (stack:_ stack) =
    let (stack2, result) = f2 stack in
    acu.last.ret_wrap result (stack2: _ stack)
  in

  { app = f3 ;
    descr }



       
