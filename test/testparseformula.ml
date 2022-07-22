(* open Petrinet *)
open Parsers

let explore_selt_formula_file net_file formula_file =
  let%lwt (net,_) = Parse.read_net net_file in 
  let _ = Parse.read_goal net formula_file in
  Lwt_io.printf "Formula file: %s\n" formula_file ;%lwt
  Lwt_io.printf "Formula\n" ;%lwt
  Lwt.return_unit


let run () =
  let nargs = Array.length Sys.argv - 1 in
  
  if nargs < 2 then
    begin
      Lwt_io.printf " Usage : ... bla bla \n"
    end

  else
    match Array.to_list Sys.argv with
    | [] | [_] | [_;_] -> assert false
    | _ :: net_file :: formula_file ::  _ -> explore_selt_formula_file net_file formula_file 


open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testparseformula" ~run ()
