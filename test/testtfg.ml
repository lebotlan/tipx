open Parsers

let explore_reduce_tfg_file file =
  let%lwt (net,_) = Parse.read_net file in 
  let%lwt _ = Parse.read_tfg net file  in
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
    | _ :: file ::  _ -> explore_reduce_tfg_file file 


open Lwtlaunch.Setkeys

let () = noconfig ===> Lwtlaunch.launch ~appname:"Testtfg" ~run ()


