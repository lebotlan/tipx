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
  { places: pl array ;
    transitions: tr array ;
    name: string }

type t = net

let nb_pl net = Array.length net.places

let nb_tr net = Array.length net.transitions

let all_tr net = Array.copy net.transitions

let get_tr net tr_id = net.transitions.(tr_id)

let get_pl net pl_id = net.places.(pl_id)

let get_name net = net.name


let dummy_place =
  { pl_id = 0 ;
    pl_name = "Dummy place" ;
    pl_pre = [] ;
    pl_post = [] }

let mk_dummy_net nb_pl =
  { places = Array.make nb_pl dummy_place ;
    transitions = [| |] ;
    name = "dummy-net" }



(* Test net *)
let test_net =

  let rec p0 = { pl_id = 0 ; pl_name = "p0" ; pl_pre = [ t0 ] ; pl_post = [ t0 ; t1 ] }
  and p1 = { pl_id = 1 ; pl_name = "p1" ; pl_pre = [] ; pl_post = [ t0 ] }
  and p2 = { pl_id = 2 ; pl_name = "p2" ; pl_pre = [ t0 ; t1 ] ; pl_post = [] }
            
  and t0 = { tr_id = 0 ; tr_name = "t0" ; tr_pre = [ (1,p0) ; (1,p1) ] ; tr_post = [ (1,p0) ; (1,p2) ] ; tr_delta = [ (-1,p1) ; (1,p2) ] }
  and t1 = { tr_id = 1 ; tr_name = "t1" ; tr_pre = [ (1,p0) ] ; tr_post = [ (1,p2) ] ; tr_delta = [ (-1,p0) ; (1,p2) ] }
  in

  { places      = [| p0 ; p1 ; p2 |] ;
    transitions = [| t0 ; t1 |] ;
    name        = "builtin-test-net" }
  
             
