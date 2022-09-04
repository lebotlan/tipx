open Petrinet
open Parsers
open Logic
open Libtfg
open Term2

type bundle =
  { net: Net.t ;
    marking: Marking.t ;
    tfg: Tfg.t option }

type element =
  | Bundle of bundle
  | Form of Formula.t

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
        | Form _f -> Styled.(b lgray "formula" e)
                         
        | Bundle b ->
          let extra = if Net.is_safe b.net then "safe" else "" in
          let extra = if b.tfg = None then extra else comma extra "tfg" in
          let extra = if extra = "" then "" else "(" ^ extra ^ ")" in
          Styled.(b lgray ("bundle" ^ extra) e)
    end)

open St

let cl_string x = Ok x

let id d = d

let ids x = Ok x

let bind env name elt = ((), add_env name elt env)
let set = bind

let get_bundle env =
  let rec loop = function
    | [] -> failwith "No bundle in current environment."
    | (_, Form _) :: rest -> loop rest
    | (name, Bundle b) :: _ -> (name, b)
  in
  loop env.map

let load ~safe env filename =
  let%lwt (net, marking) = Parse.read_net ~safe filename in
  Lwt.return (bind env "net" (Bundle { net ; marking ; tfg = None }))

let load_tfg env filename =

  let (name, b) = get_bundle env in  
  let%lwt tfg = Parse.read_tfg b.net filename in

  let b2 = { b with tfg = Some tfg } in  
  Lwt.return ((), add_env name (Bundle b2) env)

let get env name =
  try List.assoc name env.map
  with Not_found -> failwith (name ^ " is not defined in the environment.")

let read_form _f = assert false 

let machine =
  [

    info "A global environment consists in bindings of the form" ;
    info "   name => element where element is formula, or a bundle (a Petri net, an initial marking, and possibly a tfg)." ;
    
    title "Environment" ;

    def "load"      get_info !=> cl_string nop !=%+ (load ~safe:false)   "Load the given Petri net, put it as a bundle in the environment with the name 'net'" ;
    def "load-safe" get_info !=> cl_string nop !=%+ (load ~safe:true)    "Like 'load', assuming a safe net." ;

    def "load-tfg"  get_info !=> cl_string nop !=%+ load_tfg             "Load the given tfg. It is inserted in the last bundle put in the environment (fails if there is none)." ;

    def "bind" get_info !=> cl_string !-> ids nop !==+ bind "Binds the element on the stack to the given name." ;
    def "set"  get_info !=> cl_string !-> ids nop !==+ bind "Synonym to bind." ;

    def "get" implicit get_info !=> cl_string !-> id !== get "Gets the element associated to the given identifier in the environment. Pushes it." ;
    
    title "Formulas" ;

    def "form" !=> cl_string !-> id !=% read_form            "Parses and pushes the given formula on the stack." ;

    title "Others" ;

    print () ;
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
