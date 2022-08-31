open Petrinet
open Parsers
open Logic
open Libtfg
open Term2

type env =
  { net: Net.t option ;
    init_marking: Marking.t option ;
    tfg: Tfg.t option }

type data =
  | Marked_net of Net.t * Marking.t
  | Form of Formula.t

module St = Stackcl.Mk
    (struct
      type elt = data
      type info = unit

      let elt2s = function
        | Marked_net (n,_) -> Styled.(b lgray "marked net " cyan (Net.get_name n) (if Net.is_safe n then (bb lgray " (safe)" e) else pempty).b e)
        | Formula f -> Styled.(b lgray "formula" e)
    end)

open St

let cl_string x = Ok x

let id d = d


 --> tfg et net dans INFO


let load ~safe filename =
  let%lwt (net, m) = Parse.read_net ~safe filename in
  Lwt.return (Marked_net (net, m))

let read_form f = 

let machine =
  [    
    title "Config" ;

    def "load"      !=> cl_string !-> id !=% (load ~safe:false)   "Load the given Petri net." ;
    def "load-safe" !=> cl_string !-> id !=% (load ~safe:true)    "Load the given Petri net, assuming a safe net." ;

    def "form"      !=> cl_string !-> id !=% read_form            "Parses and pushes the given formula on the stack." ;

    title "Others" ;

    print () ;
  ]


let run () =
  try%lwt
    let%lwt _ = St.exec machine Sys.argv () () in 
    Lwt.return_unit
  with Stackcl.Error msg ->
    Term2.Styled.(p
              nl
              red msg
              nl nl
              e)

open Lwtlaunch.Setkeys
let () = noconfig ===> Lwtlaunch.launch ~appname:"tipx" ~run ()
