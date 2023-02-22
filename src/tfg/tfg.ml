open Petrinet
open Net

module X = ExtArray

let init_size = 1000

type node_id = int

type node_name = string

type node_type =
  | Var of node_name
  | Intv of int * int

type node =
  { node_id: node_id ;
    node_type: node_type }

type succ =
  { agg: node list ;
    red: (int * node) list }

type pred = Root | A of node | R of (int * node) list


type tfg =
  { name_map: (string, node_id) Hashtbl.t ;
    
    node_map: node X.t ;

    mutable node_count: int ;
    
    mutable roots: node list ;

    succ: succ X.t ;

    pred: pred X.t }

type t = tfg

let dummy_node =
  { node_id = -1 ;
    node_type = Var "dummy_node" }

let register_node tfg name node =
  
  assert (not (Hashtbl.mem tfg.name_map name)) ;

  Hashtbl.add  tfg.name_map name node.node_id ;
  X.set tfg.node_map node.node_id node ;
  
  ()

let is_empty tfg = tfg.node_count = List.length tfg.roots


let get_node tfg name =
  if Hashtbl.mem tfg.name_map name then X.get tfg.node_map (Hashtbl.find tfg.name_map name)
  else
    begin

      let node_type = match int_of_string_opt name with
        | None -> Var name
        | Some x -> Intv (x,x)
      in
      
      let node = { node_id = tfg.node_count ; node_type } in
      tfg.node_count <- tfg.node_count + 1 ;

      Hashtbl.add tfg.name_map name node.node_id ;
      X.set tfg.node_map node.node_id node ;

      (match node_type with Intv _ -> tfg.roots <- (node :: tfg.roots) | _ -> ()) ;

      node      
    end
  
  
let create roots =

  let name_map = Hashtbl.create init_size
  and node_map = X.create init_size dummy_node
  and succ = X.create init_size { agg = [] ; red = [] }
  and pred = X.create init_size Root in

  let node_count = Array.length roots in

  let tfg = { name_map ; node_map ; roots = [] ; node_count ; succ ; pred } in
  
  let create_init_node i pl =
    assert (i = pl.pl_id) ;

    let node =
      { node_id = i ;
        node_type = Var pl.pl_name }
    in
    
    tfg.roots <- node :: tfg.roots ;
    register_node tfg pl.pl_name node
  in
  
  Array.iteri create_init_node roots ;

  (* Add by default constant nodes equal to 0 and 1 *)
  let _ = get_node tfg "0" in
  let _ = get_node tfg "1" in
  tfg


let add_agg tfg a ll =

  let node_a = get_node tfg a
  and nodes_ll = List.rev_map (get_node tfg) ll in

  let succ_a = X.get tfg.succ node_a.node_id in
  assert (succ_a.agg = []) ;

  X.set tfg.succ node_a.node_id { succ_a with agg = nodes_ll } ;

  List.iter (fun n -> assert (X.get tfg.pred n.node_id = Root) ; X.set tfg.pred n.node_id (A node_a)) nodes_ll ;
  
  ()

let add_red tfg ll p =

  let node_p = get_node tfg p
  and nodes_ll = List.rev_map ((fun (k,n) -> (k,get_node tfg n))) ll in

  assert (X.get tfg.pred node_p.node_id = Root) ;
  X.set tfg.pred node_p.node_id (R nodes_ll) ;

  List.iter (fun (k,n) -> X.update tfg.succ n.node_id (fun succ -> { succ with red = (k,node_p) :: succ.red } ) ) nodes_ll ;

  ()

let add_leq tfg p k =

  let node_p = get_node tfg p in

  let node_k = { node_id = tfg.node_count ; node_type = Intv (k,k) } in
  tfg.node_count <- tfg.node_count + 1 ;  

  X.set tfg.node_map node_k.node_id node_k ;
  tfg.roots <- node_k :: tfg.roots ;

  let node_s = { node_id = tfg.node_count ; node_type = Var (string_of_int tfg.node_count) } in
  tfg.node_count <- tfg.node_count + 1 ;  
  X.set tfg.node_map node_s.node_id node_s ;

  X.set tfg.pred node_p.node_id (A node_k) ;
  X.set tfg.pred node_s.node_id (A node_k) ;
  
  X.set tfg.succ node_k.node_id { agg = [node_p ; node_s] ; red = [] } ;
  
  ()

let roots tfg = tfg.roots

let succ tfg node = X.get tfg.succ node.node_id

let pred tfg node = X.get tfg.pred node.node_id

let node_type node = node.node_type

let node_id node = node.node_id

let is_root tfg node = X.get tfg.pred node.node_id = Root

let get_node tfg node_id = X.get tfg.node_map node_id

let get_nodename tfg node_id =
  match (get_node tfg node_id).node_type with 
  | Var n -> n
  | Intv (a,b) when a = b -> string_of_int a
  | Intv (a,b) -> Printf.sprintf "[%d ; %d]" a b

let get_nodenamebrace tfg node_id =
  let name = get_nodename tfg node_id in
  if String.contains name '-' || String.contains name '.' then "{" ^ name ^ "}"
  else name

let get_nodeid tfg node_name = (X.get tfg.node_map (Hashtbl.find tfg.name_map node_name)).node_id

let nb_nodes tfg = tfg.node_count
