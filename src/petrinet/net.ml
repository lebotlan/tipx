type pl_id = int
type tr_id = int

type pl_name = string
type tr_name = string

type weight = int

(* Init size of tmp arrays and hashtables used in incremental nets. *)
let init_size = 50

type 'a g_itr =
  { itr_name: string ;
    itr_pre:   (weight * 'a) list ;
    itr_post:  (weight * 'a) list }

type itr = pl_name g_itr


type inet =
  { i_name: string ;

    pl_map: (string,pl_id) Hashtbl.t ;
    tr_map: (string,tr_id) Hashtbl.t ;

    mutable pl_count: int ;
    mutable tr_count: int ;

    pl_pre_tbl:  tr_id list ExtArray.t ;
    pl_post_tbl: tr_id list ExtArray.t ;

    tr_def: (pl_id g_itr) ExtArray.t ;
  }

let dummy_tr =
  { itr_name = "Dummy-transition" ;
    itr_pre = [] ;
    itr_post = [] }

let mk_empty ?(name="") () =
  { i_name = name ;
    
    pl_map = Hashtbl.create init_size ;
    tr_map = Hashtbl.create init_size ;
    
    pl_count = 0 ;
    tr_count = 0 ;
    
    pl_pre_tbl  = ExtArray.create init_size [] ;
    pl_post_tbl = ExtArray.create init_size [] ;

    tr_def = ExtArray.create init_size dummy_tr }
    

let add_pl inet pl_name =
  match Hashtbl.find_opt inet.pl_map pl_name with
  | Some i -> i
  | None ->
    let pl_id = inet.pl_count in
    inet.pl_count <- inet.pl_count + 1 ;      
    Hashtbl.add inet.pl_map pl_name pl_id ;
    pl_id

(* Adds the given transition to its pre/post places. *)
let rec insert_in_pl_tbl inet tr_id pl_tbl = function
  | [] -> ()
  | (_, pl_name) :: rest ->

    let pl_id = add_pl inet pl_name in
    let lst = ExtArray.get pl_tbl pl_id in

    if not (List.mem tr_id lst) then ExtArray.set pl_tbl pl_id (tr_id :: lst) ;
    
    insert_in_pl_tbl inet tr_id pl_tbl rest
  

let insert_arc_in_list inet l (w,pl_name) =

  let pl_id = add_pl inet pl_name in

  let rec aux acu = function
    | [] -> (w,pl_id) :: acu
    | (w1,pl_id1) as pp :: rest ->
      if pl_id1 = pl_id then List.rev_append rest ( (w1 + w, pl_id) :: acu )
      else aux (pp :: acu) rest
  in

  aux [] l
  

let add_tr inet itr =

  let tr_id =
    match Hashtbl.find_opt inet.tr_map itr.itr_name with
    | Some i -> i
    | None ->
      let tr_id = inet.tr_count in
      inet.tr_count <- inet.tr_count + 1 ;
      Hashtbl.add inet.tr_map itr.itr_name tr_id ;
      tr_id
  in

  insert_in_pl_tbl inet tr_id inet.pl_post_tbl itr.itr_pre ;
  insert_in_pl_tbl inet tr_id inet.pl_pre_tbl itr.itr_post ;

  let prev_itr = ExtArray.get inet.tr_def tr_id in

  let new_itr =
    if prev_itr == dummy_tr then
      { itr_name = itr.itr_name ;
        itr_pre  = List.rev_map (fun (w,pl_name) -> (w,add_pl inet pl_name)) itr.itr_pre ;
        itr_post = List.rev_map (fun (w,pl_name) -> (w,add_pl inet pl_name)) itr.itr_post }
    else
      begin
        assert (prev_itr.itr_name = itr.itr_name) ;

        { itr_name = itr.itr_name ;
          itr_pre  = List.fold_left (insert_arc_in_list inet) prev_itr.itr_pre itr.itr_pre ;
          itr_post = List.fold_left (insert_arc_in_list inet) prev_itr.itr_post itr.itr_post }
      end
      
  in

  ExtArray.set inet.tr_def tr_id new_itr ;
  
  tr_id


type pl =
  { pl_id: pl_id ;
    pl_name: string ;

    pl_pre:  tr_id list ;
    pl_post: tr_id list }

(* Transitions *)
and tr =
  { tr_id:  tr_id ;
    tr_name: string ;

    (* Sorted by pl_id *)
    tr_pre:   (weight * pl_id) list ;
    tr_post:  (weight * pl_id) list ;
    tr_delta: (weight * pl_id) list }

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

let close _ = assert false



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

  let p0 = { pl_id = 0 ; pl_name = "p0" ; pl_pre = [ 0 ] ; pl_post = [ 0 ; 1 ] }
  and p1 = { pl_id = 1 ; pl_name = "p1" ; pl_pre = [] ; pl_post = [ 0 ] }
  and p2 = { pl_id = 2 ; pl_name = "p2" ; pl_pre = [ 0 ; 1 ] ; pl_post = [] }
            
  and t0 = { tr_id = 0 ; tr_name = "t0" ; tr_pre = [ (1,0) ; (1,1) ] ; tr_post = [ (1,0) ; (1,2) ] ; tr_delta = [ (-1,1) ; (1,2) ] }
  and t1 = { tr_id = 1 ; tr_name = "t1" ; tr_pre = [ (1,0) ] ; tr_post = [ (1,2) ] ; tr_delta = [ (-1,0) ; (1,2) ] }
  in

  { places      = [| p0 ; p1 ; p2 |] ;
    transitions = [| t0 ; t1 |] ;
    name        = "builtin-test-net" }
  
             
