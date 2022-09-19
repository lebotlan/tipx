
open Parsers
open Logic
open Printformula
open Libtfg

let explore_selt_formula_file net_file formula_file =
  let%lwt (net,_) = Parse.read_net net_file in 
  let%lwt tfg = Parse.read_tfg net net_file in
  let%lwt goals = Parse.read_goals (Tfg tfg) formula_file in

  Lwt_list.iter_s (fun goal ->  
      Lwt_io.printf "Initial Formula: %s\n" (goal2s (Tfg.get_nodename tfg) goal) ;%lwt
      let goal = Formula.dnf goal in
      Lwt_io.printf "DNF formula: %s\n" (goal2s (Tfg.get_nodename tfg) goal) ;%lwt
      let projected_goal = Projector.project ~timeout:1000 tfg goal in
      Lwt_io.printf "Projected formula: %s\n" (goal2s (Tfg.get_nodename tfg) projected_goal.p_goal) ;%lwt

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

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testprojection" ~run ()
