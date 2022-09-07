open Logic
open Formula
open Bool
open Tfg
open Annotation
open Petrinet


(* 
 * Clean the printfformula (print node functions)
 * Really usfeull DNF to list ? Code is simpler I hope   
*)

let populate_annotation tfg c =
  let annotation = Annotation.init (nb_nodes tfg) in

  (* Add a literal to the TFG annotation *)
  let add_literal lit_id lit =
    
    let rec add_label ?(mult=1) lit_id = function
    | [] -> ()
    | K k :: rest -> add_label_to_node annotation (lit_id,mult*k) (Tfg.get_nodeid tfg "1") ;  add_label ?mult:(Some mult) lit_id rest
    | P (w,pl) :: rest -> add_label_to_node annotation (lit_id,mult*w) pl ; add_label ?mult:(Some mult) lit_id rest
    in

    add_label ~mult:(-1) lit_id lit.left  ;
    add_label lit_id lit.right ;

    (match lit.rel with 
    | LE -> ()
    | LT -> add_label_to_node annotation (lit_id,-1) (Tfg.get_nodeid tfg (string_of_int 1))
    | EQ | NE -> assert false)
  in

  let rec populate_aux count = function
  | [] -> annotation
  | lit :: rest ->
    begin
      add_literal count lit ;
      populate_aux (count + 1) rest ;
    end
  in
  populate_aux 0 c


let roots2list tfg annotation nb_lit =

  let literals = Array.make nb_lit [] in
  
  let explore_node node =
    let create_simple = 
      match Tfg.node_type node with
        | Var _ -> fun coef -> P (coef,(Tfg.node_id node))
        | Intv (lb,ub) -> fun coef -> if coef < 0 then K (lb * coef) else K (ub * coef)
    in
    Seq.iter (fun (lit_id,coef) -> literals.(lit_id) <- ((create_simple coef) :: literals.(lit_id))) (get_labels annotation (Tfg.node_id node))
  in
  
  List.iter explore_node (Tfg.roots tfg) ;
  Array.to_list (Array.map (fun l -> {left = [K 0] ; rel = LE ; right = l}) literals)


type 'a projected =
  { content: 'a ;
    complete: bool }
   
type projected_goal = {
  p_goal: Formula.t ;
  complete: bool }


(* Project a given cube on a TFG *)
let project_cube tfg c = 
  let annotation = populate_annotation tfg c in
  let visited = Bitvec.init (Tfg.nb_nodes tfg) in

  let rec visit node = 
    let node_id = Tfg.node_id node in
    
    if Bitvec.get visited node_id = 0 then
      begin
        Bitvec.set visited node_id ;

        let succ = Tfg.succ tfg node
        and pred = Tfg.pred tfg node in

        if succ.agg != [] then
          (List.iter visit succ.agg ; propagate_agg annotation (List.map Tfg.node_id succ.agg) node_id) ;

        if succ.red != [] then
          List.iter visit succ.red ;

        match pred with
        | R l -> propagate_red annotation node_id (List.map Tfg.node_id l) ;
        | _ -> () ;
      end
  in

  List.iter visit (Tfg.roots tfg) ;

  (* Build the final formula. *)
  { content = roots2list tfg annotation (List.length c) ;
    complete = is_complete annotation }

let all l f = List.for_all f l

let cubes_to_dnf cubelist =
  { content = list_to_dnf (List.map (fun c -> c.content) cubelist) ;
    complete = all cubelist (fun c -> c.complete) }

(* Project a formula on a TFG *)
let project_formula tfg formula = cubes_to_dnf (List.rev_map (project_cube tfg) (dnf_to_list formula))

(* Project a goal on a TFG *)
(* TODO: fix the use of the negates flag! *)
let project tfg goal =
  let p_form = project_formula tfg (Formula.dnf goal).form in
  
  { p_goal = (Formula.simplify { form = p_form.content ; negates=goal.negates }) ;
    complete = p_form.complete }
  
    
