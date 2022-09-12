type pl_id = int
type tr_id = int

type pl_name = string
type tr_name = string

type weight = int

(* Init size of tmp arrays and hashtables used in incremental nets. *)
let init_size = 1000

type 'a g_itr =
  { itr_name: string ;
    itr_pre:   (weight * 'a) list ;
    itr_post:  (weight * 'a) list }

type itr = pl_name g_itr


type inet =
  { mutable i_name: string ;

    pl_map: (string,pl_id) Hashtbl.t ;
    tr_map: (string,tr_id) Hashtbl.t ;

    mutable pl_count: int ;
    mutable tr_count: int ;

    pl_name_tbl: string ExtArray.t ;
    pl_pre_tbl:  tr_id list ExtArray.t ;
    pl_post_tbl: tr_id list ExtArray.t ;

    tr_def: (pl_id g_itr) ExtArray.t ;

    mutable duplicate_transitions: bool ;
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

    pl_name_tbl = ExtArray.create init_size "" ;
    pl_pre_tbl  = ExtArray.create init_size [] ;
    pl_post_tbl = ExtArray.create init_size [] ;

    tr_def = ExtArray.create init_size dummy_tr ;
    
    duplicate_transitions = false }

let set_name inet n = inet.i_name <- n 

let add_pl inet pl_name =
  match Hashtbl.find_opt inet.pl_map pl_name with
  | Some i -> i
  | None ->
    let pl_id = inet.pl_count in
    inet.pl_count <- inet.pl_count + 1 ;      
    Hashtbl.add inet.pl_map pl_name pl_id ;
    
    ExtArray.set inet.pl_name_tbl pl_id pl_name ;
    pl_id

(* Adds the given transition to its pre/post places. *)
let rec insert_in_pl_tbl inet tr_id pl_tbl = function
  | [] -> ()
  | (_, pl_name) :: rest ->

    let pl_id = add_pl inet pl_name in
    let lst = ExtArray.get pl_tbl pl_id in

    (* May introduce doublons, that will be removed after sorting.
     * Checking here if pl_id is in lst is too expansive on big nets. *)
    ExtArray.set pl_tbl pl_id (tr_id :: lst) ;
    
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
        inet.duplicate_transitions <- true ;

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
  { name: string ;

    safe: bool ;
    
    places: pl array ;
    transitions: tr array ;

    pl_map: (string,pl_id) Hashtbl.t}

type t = net

let nb_pl net = Array.length net.places

let nb_tr net = Array.length net.transitions

let all_tr net = Array.copy net.transitions

let all_pl net = Array.copy net.places

let get_tr net tr_id = net.transitions.(tr_id)

let get_pl net pl_id = net.places.(pl_id)

let get_plname net pl_id = (get_pl net pl_id).pl_name

let get_plnamebrace net pl_id =
  let name = get_plname net pl_id in
  if String.contains name '-' || String.contains name '.' then "{" ^ name ^ "}"
  else name

let get_plid net pl_name = Hashtbl.find net.pl_map pl_name

let get_name net = net.name

let is_safe net = net.safe

let remove_duplicates inet l =

  if inet.duplicate_transitions then
    
    let rec aux acu = function
      | [] -> List.rev acu
      | [x] -> aux (x :: acu) []
      | x :: ((y :: _) as rest) -> if x = y then aux acu rest else aux (x :: acu) rest
            
    in
    aux [] (List.sort Stdlib.compare l)

  else l
  
let create_place inet i =
  { pl_id = i ;
    pl_name = ExtArray.get inet.pl_name_tbl i ;
    pl_pre  = remove_duplicates inet (ExtArray.get inet.pl_pre_tbl i) ;
    pl_post = remove_duplicates inet (ExtArray.get inet.pl_post_tbl i) }

let cmp2 (_,i1) (_,i2) = Stdlib.compare i1 i2

let rec compute_delta acu pre post =
  match pre,post with
  | [],[] -> List.rev acu
               
  | (w,pl) :: rest1, [] -> compute_delta ((-w,pl) :: acu) rest1 []

  | [], (w,pl) :: rest2 -> compute_delta ((w,pl) :: acu) [] rest2

  | (w1,pl1) :: rest1, (w2,pl2) :: rest2 ->

    if pl1 = pl2 then
      if w2 = w1 then compute_delta acu rest1 rest2
      else compute_delta ((w2 - w1, pl1) :: acu) rest1 rest2
          
    else if pl1 < pl2 then compute_delta ((-w1, pl1) :: acu) rest1 post
    else compute_delta ((w2, pl2) :: acu) pre rest2

let create_trans inet i =

  let tr = ExtArray.get inet.tr_def i in

  let tr_pre = List.sort cmp2 tr.itr_pre
  and tr_post = List.sort cmp2 tr.itr_post in
  
  { tr_id = i ;
    tr_name = tr.itr_name ;
    tr_pre ;
    tr_post ;
    tr_delta = compute_delta [] tr_pre tr_post }
    
    

let close ?(safe=false) inet =
  { name = inet.i_name ;
    safe ;
    
    places      = Array.init inet.pl_count (create_place inet) ;
    transitions = Array.init inet.tr_count (create_trans inet) ;
    
    pl_map = inet.pl_map
    }


let dummy_place =
  { pl_id = 0 ;
    pl_name = "Dummy place" ;
    pl_pre = [] ;
    pl_post = [] }

let mk_dummy_net nb_pl =
  { name = "dummy-net" ;
    safe = false ;
    places = Array.make nb_pl dummy_place ;
    transitions = [| |] ;
    pl_map = Hashtbl.create 0}



(* Test net *)
let test_net =

  let p0 = { pl_id = 0 ; pl_name = "p0" ; pl_pre = [ 0 ] ; pl_post = [ 0 ; 1 ] }
  and p1 = { pl_id = 1 ; pl_name = "p1" ; pl_pre = [] ; pl_post = [ 0 ] }
  and p2 = { pl_id = 2 ; pl_name = "p2" ; pl_pre = [ 0 ; 1 ] ; pl_post = [] }
            
  and t0 = { tr_id = 0 ; tr_name = "t0" ; tr_pre = [ (1,0) ; (1,1) ] ; tr_post = [ (1,0) ; (1,2) ] ; tr_delta = [ (-1,1) ; (1,2) ] }
  and t1 = { tr_id = 1 ; tr_name = "t1" ; tr_pre = [ (1,0) ] ; tr_post = [ (1,2) ] ; tr_delta = [ (-1,0) ; (1,2) ] }
  and pl_map = Hashtbl.create 3
  in
  begin
  Hashtbl.add pl_map "p0" 0 ;
  Hashtbl.add pl_map "p1" 1 ;
  Hashtbl.add pl_map "p2" 2 ;
  { name        = "builtin-test-net" ;
    safe        = false ;
    places      = [| p0 ; p1 ; p2 |] ;
    transitions = [| t0 ; t1 |] ;
    pl_map = pl_map
    }
  end
  
             
