open Petrinet
open Parsers

let explore_tina_net_file file =

  let%lwt (net,mark) = Parse.read_net file in

  Lwt_io.printf " Read : %d places, %d transitions\n\n" (Net.nb_pl net) (Net.nb_tr net) ;%lwt
  Lwt_io.printf "%s\n\n" (Printnet.net2s net) ;%lwt
  Lwt_io.printf "Initial marking : \n%s\n\n" (Marking.tos mark) ;%lwt 
  Lwt.return_unit


let run () =
  let nargs = Array.length Sys.argv - 1 in
  
  if nargs < 1 then
    begin
      Lwt_io.printf " Usage : ... bla bla \n"
    end

  else
    match Array.to_list Sys.argv with
    | [] | [_] -> assert false
    | _ :: file :: _ -> explore_tina_net_file file


open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testexploration" ~run ()


(* Perf: Family400 best 2,15s 
                        2,10s   <---- allocate buf and all functions at once
                        2,00s   <---- peek instead of <|> in tr
                        1,42s   <---- peek instead of <|> in net_loop + at_end_of_input instead of end_of_input 
*)
