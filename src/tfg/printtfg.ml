open Tfg
open Petrinet

let equation2s left right = (node_name left) ^ " = " ^ (Common.sep node_name " + " right)

let tfg2s tfg = 

  (* Roots *)
  let roots = roots tfg in

  (* Bitvec to remember the nodes alreay explored *)
  let explored_nodes = Bitvec.init (nb_nodes tfg) in
  
  (* Node to string *)
  let rec node2s node = 
    
    (* Succ and pred of the current node *)
    let succ_of_node = succ tfg node in 
    let pred_of_node = pred tfg node in
    
    (* Only continue if the redundant parent are explored *)
    (* Write redundancy relation when is child            *)
    let (continue,red_str) =
      match pred_of_node with 
      | (R l) -> 
          if List.for_all (fun x -> Bitvec.get explored_nodes (node_id x) = 1) l then
            (* Continue to explore *)
            (true, "R |- " ^ equation2s node l ^ "\n")
          else
            (* Stop exploring *)
            (false, "")
      | _ ->
        (* Continue exploring *)
        (true, "") in

    (* Compute the agglomeration equation if there is one *)
    let agg_str = if continue && List.length succ_of_node.agg > 0
      then "A |- " ^ (equation2s node succ_of_node.agg) ^ "\n"  
      else ""
    in

    if continue then
      begin
        Bitvec.set explored_nodes (node_id node) ;
        (Common.sep node2s "" succ_of_node.agg) ^ (Common.sep node2s "" succ_of_node.red) ^ red_str ^ agg_str
      end
    else
      ""

    in
  
    Common.sep node2s "" roots