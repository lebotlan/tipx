

type ('a, 'b) kont = unit -> 'a -> 'b

type 'a tok = { it: 'b. ('a, ('a, 'b) kont -> 'b) kont }


let mk_start init = fun k -> k () init

let mk_end f = fun () acu -> f acu

let mk_op0 f = fun () acu k -> k () (f acu)
let mk_op1 f = fun () acu x1 k -> k () (f acu x1)
let mk_op2 f = fun () acu x1 x2 k -> k () (f acu x1 x2)
let mk_op3 f = fun () acu x1 x2 x3 k -> k () (f acu x1 x2 x3)



