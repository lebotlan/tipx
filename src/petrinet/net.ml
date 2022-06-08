type pl_id = int
type tr_id = int

type weight = int

type pl =
  { pl_id: pl_id ;
    pl_name: string ;

    pl_pre:  tr list ;
    pl_post: tr list }

(* Transitions *)
and tr =
  { tr_id:  tr_id ;
    tr_name: string ;

    (* Sorted by pl_id *)
    tr_pre:   (weight * pl) list ;
    tr_post:  (weight * pl) list ;
    tr_delta: (weight * pl) list }

let null_tr =
  { tr_id = -1 ;
    tr_name = "null-transition" ;
    tr_pre = [] ;
    tr_post = [] ;
    tr_delta = [] }


type net =
  { nb_pl: int }

type t = net

let nb_pl net = net.nb_pl

let nb_tr _net = 0

let all_tr _net = []

let get_tr _net _tr_id = null_tr

let get_name _ = ""


(* - extarray

 *  - cache all_tr  all_pl  *)

let mk_dummy_net nb_pl = { nb_pl }
