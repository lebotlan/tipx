open Tfg
open Petrinet

let node2s node = match node_type node with
  | Var n -> n
  | Intv (a,b) when a = b -> string_of_int a
  | Intv (a,b) -> Printf.sprintf "[%d ; %d]" a b

let knode2s (k,n) =
  if k = 1 then node2s n
  else string_of_int k ^ "." ^ node2s n

let equation2s n2s left right = (node2s left) ^ " = " ^ (Common.sep n2s " + " right)

let tfg2s tfg = 

  (* Roots *)
  let roots = roots tfg in

  (* Bitvec to remember the nodes already explored *)
  let explored_nodes = Bitvec.init (nb_nodes tfg) in
  
  (* Invariant: when loop returns, this node and its dependencies are defined in the acu. *)
  let rec loop acu node = 

    let nodeid = node_id node in
    
    if Bitvec.get explored_nodes nodeid = 1 then acu
    else
      let () = Bitvec.set explored_nodes nodeid in

      (* Succ and pred of the current node *)
      let succ_of_node = succ tfg node in 
      let pred_of_node = pred tfg node in

      (* Ensure dependencies are in the acu. *)
      let acu = List.fold_left loop acu succ_of_node.agg in
      let acu = match pred_of_node with
        | R l -> List.fold_left (fun acu (_,n) -> loop acu n) acu l
        | _ -> acu
      in

      (* Define the agglomeration equation if there is one *)
      let acu =
        if succ_of_node.agg = [] then acu
        else acu ^ "A |- " ^ (equation2s node2s node succ_of_node.agg) ^ "\n"
      in

      (* Define the redundancy equation if there is one *)
      let acu = match pred_of_node with
        | R l -> acu ^ "R |- " ^ equation2s knode2s node l ^ "\n"
        | _ -> acu
      in

      (* See redundancy successors. *)
      List.fold_left loop acu (List.map snd succ_of_node.red)
  in

  List.fold_left loop "" roots
