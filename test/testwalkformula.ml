open Petrinet
open Parsers
open Walker
open Logic  
open Printformula
    
let walk_tina_net_file net_file formula_file =

  let%lwt (net,mark) = Parse.read_net ~safe:true net_file in
  let%lwt goal = Parse.read_goal net formula_file in

  Lwt_io.printf " Read : %d places, %d transitions\n\n" (Net.nb_pl net) (Net.nb_tr net) ;%lwt

  Lwt_io.printf " Initial formula : %s\n\n" (goal2s (Net.get_plname net) goal) ;%lwt

  let goal = Formula.dnf goal in
  Lwt_io.printf " Dnf formula : %s\n\n" (goal2s (Net.get_plname net) goal) ;%lwt
  

  let result = Walk.(sprinter ~timeout:8000 ~seed:3939494 ~stats:stat_stdout net mark (Eval.eval_goal goal)) in
  
  Lwt_io.printf " Walker : %s.\n" (Walk.result2s result) ;%lwt
  Lwt_io.printf " Verdict : %s.\n" (Walk.result2verdict result goal) ;%lwt
  
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
    | _ :: net_file :: formula_file :: _ -> walk_tina_net_file net_file formula_file


open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testwalkformula" ~run ()

