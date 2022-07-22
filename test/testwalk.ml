open Petrinet
open Parsers
open Walker
    
let walk_tina_net_file file =

  let%lwt (net,mark) = Parse.read_net file in

  Lwt_io.printf " Read : %d places, %d transitions\n\n" (Net.nb_pl net) (Net.nb_tr net) ;%lwt

  let result = Walk.sprinter ~seed:3939494 ~timeout:6 net mark (fun _ -> false) in
  
  Lwt_io.printf " Walker : %s.\n" (Walk.result2s result) ;%lwt
  
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
    | _ :: file :: _ -> walk_tina_net_file file


open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testwalk" ~run ()

