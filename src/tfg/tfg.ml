open Petrinet

open Net

module X = ExtArray

let init_size = 1000

type node_id = int

type node_name = string

type node =
  { node_id: node_id ;
    node_name: node_name }

type succ =
  { agg: node list ;
    red: node list }

type pred = Root | A of node | R of node list


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
    node_name = "dummy_node" }

let register_node tfg node =
  assert (not (Hashtbl.mem tfg.name_map node.node_name)) ;

  Hashtbl.add  tfg.name_map node.node_name node.node_id ;
  X.set tfg.node_map node.node_id node ;
  
  ()

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
        node_name = pl.pl_name }
    in
    
    tfg.roots <- node :: tfg.roots ;
    register_node tfg node
  in
  
  Array.iteri create_init_node roots ;

  tfg

let get_node tfg name =
  if Hashtbl.mem tfg.name_map name then X.get tfg.node_map (Hashtbl.find tfg.name_map name)
  else
    begin
      let node =
        { node_id = tfg.node_count ;
          node_name = name }
      in

      tfg.node_count <- tfg.node_count + 1 ;

      Hashtbl.add tfg.name_map name node.node_id ;
      X.set tfg.node_map node.node_id node ;

      node      
    end


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
  and nodes_ll = List.rev_map (get_node tfg) ll in

  assert (X.get tfg.pred node_p.node_id = Root) ;
  X.set tfg.pred node_p.node_id (R nodes_ll) ;

  List.iter (fun n -> X.update tfg.succ n.node_id (fun succ -> { succ with red = node_p :: succ.red } ) ) nodes_ll ;

  ()

let roots tfg = tfg.roots

let succ tfg node = X.get tfg.succ node.node_id

let pred tfg node = X.get tfg.pred node.node_id

let node_name node = node.node_name

let node_id node = node.node_id

let is_root tfg node = X.get tfg.pred node.node_id = Root

let get_node tfg node_id = X.get tfg.node_map node_id

let nb_nodes tfg = tfg.node_count