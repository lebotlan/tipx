open Petrinet
open Parsers
open Logic
open Printformula
    
let explore_selt_formula_file net_file formula_file =
  let%lwt (net,_) = Parse.read_net net_file in 
  let%lwt goals = Parse.read_goals (Net net) formula_file in

  Lwt_list.iter_s (fun goal ->  
      Lwt_io.printf "Initial Formula: %s\n" (goal2s (Net.get_plname net) goal) ;%lwt

      let goal = Formula.dnf goal in
      Lwt_io.printf " Dnf formula : %s\n\n" (goal2s (Net.get_plname net) goal) ;%lwt

      Lwt.return_unit)

    goals


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
