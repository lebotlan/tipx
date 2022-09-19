type lit_id = int

(* Node identifier (index in the TFG) *)
(* type node_id = int *)
module SetId = Set.Make(Int)

type coef = int

type label = int * int

(* Annotation *)
type annotation = { 
  labels: ((lit_id,coef) Hashtbl.t) array ;
  mutable complete: bool
}

(* Incomplete projection exception (still sound) *)
(* exception Incomplete_projection *)

(* Dummy hash table *)
let dummy_table = Hashtbl.create 0

(* Initialize a TFG annotation *)
let init nb_nodes = {labels = Array.init nb_nodes (fun _ -> Hashtbl.create 1) ; complete = true}

(* Get the hashtbl associated to the given node_id in the annotation *)
let get_labels annotation node_id =
  let current_table = Array.get annotation.labels node_id in
  if current_table == dummy_table then
    let new_table = Hashtbl.create 1 in
    Array.set annotation.labels node_id new_table ;
    new_table
  else
    current_table

(* Get the coef associated to a node for a given literal *)
let get_coef annotation lit_id node_id =
  match Hashtbl.find_opt (get_labels annotation node_id) lit_id with
  | Some k -> k
  | None -> 0

(* Add the label to a given node_id
 * If the node_id is already associated to the lit_id then it sums the coefficients. *)
let add_label_to_node annotation (lit_id,coef) ?(mult=1) node_id =

  let coef = mult * coef in
  let labels = get_labels annotation node_id in 

  match Hashtbl.find_opt labels lit_id with
  | Some k -> Hashtbl.replace labels lit_id (coef + k)
  | None -> Hashtbl.add labels lit_id coef 

let add_label_to_nodes annotation label knodes = List.iter (fun (mult,n) -> add_label_to_node annotation label ~mult n) knodes

let propagate_red annotation child parents = Hashtbl.iter (fun lit coef -> (add_label_to_nodes annotation (lit,coef) parents)) (Array.get annotation.labels child)

let get_node_literals annotation acu node_id = Hashtbl.fold (fun k _ acu -> if List.mem k acu then acu else k :: acu) (Array.get annotation.labels node_id) acu

let propagate_agg annotation children parent =

  let literals = List.fold_left (get_node_literals annotation) [] children  in
  
  let rec update_candidates max_value acu to_visit candidates lit_id =
    if SetId.is_empty candidates then candidates
    else match to_visit with
      | [] -> acu
      | node :: rest -> let coef = get_coef annotation lit_id node in
        (match max_value with
        | None when (SetId.mem node candidates) -> update_candidates (Some coef) (SetId.add node acu) rest candidates lit_id
        | Some k when (k = coef) && (SetId.mem node candidates) ->  update_candidates (Some k) (SetId.add node acu) rest candidates lit_id
        | Some k when k < coef -> if SetId.mem node candidates then update_candidates (Some k) (SetId.singleton node) rest candidates lit_id
                                  else update_candidates (Some k) (SetId.empty) rest candidates lit_id
        | _ -> update_candidates (Some coef) acu rest candidates lit_id)
  in

  let propagate_min lit_id =
    let aux coef node_id = let current = get_coef annotation lit_id node_id in if current < coef then current else coef in
    add_label_to_node annotation (lit_id, List.fold_left aux Int.max_int children) parent
  in

  if literals != [] then
    let candidates = List.fold_left (update_candidates None SetId.empty children) (SetId.of_list children) literals in

    match SetId.choose_opt candidates with
    | Some node -> (* Propagate red or add label ?*) propagate_red annotation node [ (1,parent) ]
    | None -> (List.iter propagate_min literals ;
               annotation.complete <- false)

  else ()

let get_labels annotation node_id = Hashtbl.to_seq (Array.get annotation.labels node_id)

let is_complete annotation = annotation.complete

(* Annotation to string (only for debug purpose) *)
let annotation2s tfg annotation = 
  let label2s hash = Seq.fold_left (fun acu (lit_id,w) -> acu ^ "( " ^ (string_of_int lit_id ^ " ; " ^ (string_of_int w) ^ " )")) "" (Hashtbl.to_seq hash) in
  let _,st = Array.fold_left (fun (count,acu) node -> if Hashtbl.length node > 0 then ((count+1),(acu ^ (Tfg.get_nodename tfg count) ^ "  <->  " ^ (label2s node) ^ "\n")) else (count+1,acu)) (0, "") annotation.labels
  in st
