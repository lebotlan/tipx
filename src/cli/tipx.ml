open Petrinet
open Parsers
open Logic
open Libtfg
open Term2
open Parse

type bundle =
  { net: Net.t ;
    marking: Marking.t ;
    tfg: Tfg.t option }

type element =
  | Bundle of bundle
  | Forms of Formula.t list

(* Global environment: maps ids to elements. *)
type env =
  { map: (string * element) list }

let add_env name elt env = { map = (name, elt) :: env.map }

let init_env = { map = [] }

let comma s x = if s = "" then x else s ^ ", " ^ x

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

let load ~safe env filename =

  (* Read net *)
  let%lwt (net, marking) = Parse.read_net ~safe filename in

  (* Read may-be-here TFG *)
  let%lwt tfg =
    try%lwt
      let%lwt tfg = Parse.read_tfg net filename in

      if Tfg.is_empty tfg then Lwt.return_none
      else Lwt.return (Some tfg)
        
    with _ -> Lwt.return_none
  in
  
  let bundle = Bundle { net ; marking ; tfg } in    
  Lwt.return (bind env "net" bundle)
      

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

let mkform l = Forms l

let get_forms = function
  | Forms l -> Ok l
  | _ -> Failed "Not a list of formulas"

let read_forms env f = Parse.sread_goals (get_source env) f
let load_forms env f = Parse.read_goals (get_source env) f

let project env l = List.map (Projector.project (get_tfg env)) l

let fprint env elt = match elt with
  | Bundle _ -> Term2.fprints (elt2s elt)
  | Forms l ->
    let forms = Common.sep (Printformula.goal2s (Tfg.get_nodename (get_tfg env))) "\n" l in
    Lwt_io.printf "%s\n" forms 

let machine =
  [

    info ("A global environment consists in bindings of the form\n\n" ^
          "    name => formula\n" ^
          "    name => bundle (that is: a Petri net, an initial marking, and possibly a tfg).") ;
    
    title "Environment" ;

    def "load"      implicit get_info !=> cl_filename nop !=%+ (load ~safe:false)   "Load the given Petri net, put it as a bundle in the environment with the name 'net'" ;
    def "load-safe" get_info !=> cl_string nop !=%+ (load ~safe:true)    "Like 'load', assuming a safe net." ;

    def "bind" get_info !=> cl_string !-> ids nop !==+ bind "Binds the element on the stack to the given name." ;
    def "set"  get_info !=> cl_string !-> ids nop !==+ bind "Synonym to bind." ;

    def "get" implicit get_info !==> cl_env_name !-> id !== get "Gets the element associated to the given identifier in the environment. Pushes it." ;
    
    title "Formulas" ;

    def "form" get_info !=> cl_string !-> mkform !=% read_forms            "Parse and push the given formula(s) on the stack (as a list). The reference bundle is the last found in the environment." ;
    def "load-forms" get_info !=> cl_string !-> mkform !=% load_forms      "Read formulas from the given file and push them on the stack (as a list). The reference bundle is the last found in the environment." ;

    def "project" get_info !-> get_forms !-> mkform !== project "Projects a list of formulas (popped from the stack). The reference bundle must have a tfg. Pushes the resulting list of formulas." ;

    title "Others" ;

    def "fprint" get_info !-> ids nop !=% fprint "Full print: print the topmost stack element, with details." ;

    print () ;
    dup () ;
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
