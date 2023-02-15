open Petrinet
open Parsers
open Logic
open Libtfg
open Term2
open Parse
open Projector
open Walker

(* Default timeout for walkers, in s. *)
let default_timeout = 10

let default_project_timeout = 3600

type bundle =
  { net: Net.t ;
    marking: Marking.t ;
    tfg: Tfg.t option }

type form =
  | Formula of Formula.t
  | Projected of projected_goal

type element =
  | Bundle of bundle
  | Forms of form list

(* Global environment: maps ids to elements. *)
type env =
  { map: (string * element) list ;
    time: float option ;
    verb: bool }

let add_env name elt env = { env with map = (name, elt) :: env.map }

let init_env = { map = [] ; verb = true ; time = None }

let comma s x = if s = "" then x else s ^ ", " ^ x

let form2goal = function
  | Formula g -> g
  | Projected pg -> pg.p_goal

module St = Stackcl.Mk
    (struct
      type elt = element
      type info = env

      let elt2s = function
        | Forms l -> let n = List.length l in Styled.(b lgray (if n = 1 then "a formula" else "a list of " ^ string_of_int n ^ " formulas") e)
                         
        | Bundle b ->
          let extra = if Net.is_safe b.net then "safe" else "" in
          let extra = if b.tfg = None then extra else comma extra "tfg" in
          let extra = if extra = "" then "" else "(" ^ extra ^ ")" in
          Styled.(b lgray ("bundle" ^ extra) e)
    end)

open St

let cl_string x = Ok x

let cl_int x = match int_of_string_opt x with Some i -> Ok i | None -> Failed "Not an int."

let cl_filename x = if Sys.file_exists x then Ok x else Failed "This is not a file."

let cl_env_name env x = if List.mem_assoc x env.map then Ok x else Failed "Not bound in the environment."

let id d = d

let ids x = Ok x

let bind env name elt = ((), add_env name elt env)
let set = bind

let get_bundle env =
  let rec loop = function
    | [] -> failwith "No bundle in current environment."
    | (_, Forms _) :: rest -> loop rest
    | (name, Bundle b) :: _ -> (name, b)
  in
  loop env.map

(* only_places = true  =>  do not parse transitions *)
let load ~safe ~only_places ~tfg env filename =

  (* Read net *)
  let%lwt (net, marking) =
    if only_places then Parse.read_net_places filename
    else Parse.read_net ~safe filename
  in

  (* Read may-be-here TFG *)
  let%lwt tfg =
    if tfg then
      try%lwt
        let%lwt tfg = Parse.read_tfg net filename in
        
        if Tfg.is_empty tfg then Lwt.return_none
        else Lwt.return (Some tfg)
            
      with _ -> Lwt.return_none

    else Lwt.return_none
  in
  
  let bundle = Bundle { net ; marking ; tfg } in    
  Lwt.return (bind env "net" bundle)

let get_pl_reader env =
  let (_,b) = get_bundle env in
  match b.tfg with
  | None -> Net.get_plnamebrace b.net
  | Some tfg -> Tfg.get_nodenamebrace tfg

let form2s env f =

  let reader = get_pl_reader env in
  
  match f with
  | Formula f -> Printformula.goal2s reader f
  | Projected pg -> Printformula.goal2s reader pg.p_goal ^ (if pg.complete then " # complete" else " # uncomplete") ^ Printf.sprintf " (%d / %d)" pg.n_complete_cubes pg.n_cubes


let get env name =
  try List.assoc name env.map
  with Not_found -> failwith (name ^ " is not defined in the environment.")

let get_source env =
  let (_,b) = get_bundle env in
  match b.tfg with
  | None -> Net b.net
  | Some tfg -> Tfg tfg

let get_tfg env = match get_source env with
  | Tfg tfg -> tfg
  | Net _ -> failwith "This bundle has no tfg."  

let mkform l = Forms (List.map (fun f -> Formula f) l)
let mkproj l = Forms (List.map (fun f -> Projected f) l)

let get_forms = function
  | Forms l -> Ok l
  | _ -> Failed "Not a list of formulas"

let read_forms env f = Parse.sread_goals (get_source env) f
let load_forms env f = Parse.read_goals (get_source env) f

let project timeout env l =
  let tfg = get_tfg env in
  
  Common.mymap l
    begin function
      | Formula f -> Projector.project ~timeout tfg f
      | Projected _ -> failwith "Cannot project a formula that is already projected."
    end

let fprint env elt = match elt with
  | Bundle _ -> Term2.fprints (elt2s elt)
  | Forms l ->
    let forms = Common.sep (form2s env) "\n" l in
    Lwt_io.printf "%s\n" forms 

let nl _ = Styled.(p cyan "\n###########################################\n\n" e)

let walk timeout env l =

  let (_,b) = get_bundle env in

  Lwt_list.iter_s
    begin fun form ->
      if env.verb then Styled.(p fmt yellow "\n ## Walking for %d s, testing goal %s\n%!" timeout (form2s env form) e) else Lwt.return_unit ;%lwt

      let predicate = Eval.eval_goal (form2goal form) in
      let result = Walk.(sprinter ~timeout ~stats:(if env.verb then stat_stdout else Pipe.null ()) b.net b.marking predicate) in
      Lwt_io.printf "\n <+> Walk result : %s\n%!" (Walk.result2s result)
    end
    l

let loop_walk env t1 t2 l =

  let (_,b) = get_bundle env in

  Lwt_list.iter_s
    begin fun form ->
      if env.verb then Styled.(p fmt yellow "\n ## Loop-walking for %d s, testing goal %s\n%!" t2 (form2s env form) e) else Lwt.return_unit ;%lwt

      let%lwt () = Lwtplus.yield () in
      
      let predicate = Eval.eval_goal (form2goal form) in

      let glob_timeout = Unix.gettimeofday () +. float_of_int t2 in
      
      let rec loop seed t2 =
        let result = Walk.(sprinter ~seed ~timeout:(min t1 t2) ~stats:(if env.verb then stat_stdout else Pipe.null ()) b.net b.marking predicate) in

        match result with
        | Bingo _ -> Lwt_io.printf "\n <+> Walk result : %s\n%!" (Walk.result2s result)
        | Deadlock st | Timeout st | Maxstep st ->
          let now = Unix.gettimeofday () in
          if now > glob_timeout then Lwt_io.printf "\n <+> Walk result : Global timeout\n%!"
          else loop (st.seed+1) (int_of_float (glob_timeout -. now))
      in

      loop 0 t2
        
    end
    l
  

let to_quiet env = ((), { env with verb = false })

let do_time env =

  let now = Unix.gettimeofday () in
  
  let%lwt () = match env.time with
    | None -> Lwt.return_unit
    | Some start -> Styled.(p fmt orange "# Time: %.2f s\n" (now -. start) e)
  in

  Lwt.return ((), { env with time = Some now })


 
let machine =
  [

    info ("Additionally, a global environment consists in bindings of the form\n\n" ^
          "    name => formula\n" ^
          "    name => bundle (that is: a Petri net, an initial marking, and possibly a tfg).") ;
    
    title "Environment" ;

    def "load" implicit get_info !=> cl_filename nop !=%+ (load ~safe:false ~tfg:true ~only_places:false)
      "Load the given Petri net, put it as a bundle in the environment with the name 'net'. E.g.: load \"file.net\"" ;

    def "tfgload"       get_info !=> cl_filename nop !=%+ (load ~safe:false ~tfg:true ~only_places:true)
      "Load only the places of the given Petri net, and its TFG. Put is as a bundle in the environment." ;
    
    def "rawload"       get_info !=> cl_filename nop !=%+ (load ~safe:false ~tfg:false ~only_places:false)
      "Loads the given Petri net and initial marking, but skips the TFG." ;
    
    def "load-safe"     get_info !=> cl_string nop !=%+ (load ~safe:true ~tfg:true ~only_places:false)      "Like 'load', assuming a safe net." ;
    def "rawload-safe"  get_info !=> cl_string nop !=%+ (load ~safe:true ~tfg:false ~only_places:false)     "Like 'rawload', assuming a safe net." ;

    def "bind" get_info !=> cl_string !-> ids nop !==+ bind "Binds the element on the stack to the given name. E.g.: bind special-net" ;
    def "set"  get_info !=> cl_string !-> ids nop !==+ bind "Synonym to bind." ;

    def "get" implicit get_info !==> cl_env_name !-> id !== get "Gets the element associated to the given identifier in the environment. Pushes it. E.g.: get special-net" ;
    
    title "Formulas" ;

    def "form" get_info !=> cl_string !-> mkform !=% read_forms            "Parse and push the given formula(s) on the stack (as a list). The reference bundle is the last found in the environment." ;
    def "load-forms" get_info !=> cl_string !-> mkform !=% load_forms      "Read formulas from the given file and push them on the stack (as a list). The reference bundle is the last found in the environment." ;

    def "project"             get_info  !-> get_forms !-> mkproj !== (project default_project_timeout) "Projects a list of formulas (popped from the stack). The reference bundle must have a tfg. Pushes the resulting list of formulas." ;
    def "tproject" !=> cl_int get_info  !-> get_forms !-> mkproj !== project                           "Projects with a time limit." ;

    title "Walker" ;

    def "walk" get_info !-> get_forms nop !=% (walk default_timeout)  ("Pops a list of formulas from the stack. Runs a walker sequentially on each formula, with a default timeout of " ^ string_of_int default_timeout ^ "s") ;
    def "twalk" !=> cl_int get_info !-> get_forms nop !=% walk         "Like walk, with the given timeout, in s." ;

    (*    def "steps"  *) (* steps n :  Sur la pile : marquage & seed & steps  (bref, le state) *)

    (* def "init" *)  (* push initial state of the reference bundle. *) 

    def "loop" get_info !=> cl_int !=> cl_int !-> get_forms nop !=% loop_walk ("loop t1 t2: pops a list of formulas. On each formula (taken sequentially), twalk t1. If it deadlocks or " ^
                                                                               "timeouts, restart from init state with the current seed. t2 is a global timeout for each formula.") ;
                                                                               
    (*   def "loop" *)   (* loop timeout1 timeout2  .... reset after each deadlock. *)

(* Idée :
 *
 *  mettre une formule dans l'environnement ('phi')
 *  steps : pop state -> walk -> push state
 *  steps fonctionne avec une ou plusieurs formules ?
 *
 *
*)
    
    
    title "Display" ;

    def "quiet" get_info nop !==+ to_quiet "Quiet mode (prints only necessary information, e.g. verdicts)." ;
      
    print () ;
    def "fprint" get_info !-> ids nop !=% fprint "Full print: print the topmost stack element, with details." ;
    def "nl" get_info nop !=% nl "Prints a blank line (separator)." ;

    title "Others" ;

    def "time" get_info nop !=%+ do_time "Prints the delay since the previous time command. The first time command does not print anything." ;
    dup () ;
    pop () ;
    help () ;
  ]


let run () =
  try%lwt
    let%lwt _ = St.exec machine Sys.argv init_env () in 
    Lwt.return_unit
  with Stackcl.Error msg ->
    Term2.Styled.(p
              nl
              red msg
              nl nl
              e)

open Lwtlaunch.Setkeys
let () = noconfig ===> Lwtlaunch.launch ~appname:"tipx" ~run ()
