open Petrinet
open Parsers
open Walker
open Logic  

let walk_tina_net_file net_file formula_file =

  let%lwt (net,mark) = Parse.read_net net_file in
  let%lwt goal = Parse.read_goal net formula_file in

  Lwt_io.printf " Read : %d places, %d transitions\n\n" (Net.nb_pl net) (Net.nb_tr net) ;%lwt

  let result = Walk.sprinter ~seed:3939494 ~timeout:6 net mark (Eval.eval_formula goal.form) in
  
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

