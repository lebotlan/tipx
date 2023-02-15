open Petrinet
open Libtfg
open Angstrom.Buffered

type path = string


let fail_unconsumed msg path u =

  let%lwt () = if msg = "" then Lwt.return_unit else Lwt_io.printf "Error : %s\n%!" msg in
  
  let bigstr = Bigstring.sub u.buf u.off u.len in
  let rest = Bigstring.to_string bigstr in

  Lwt_io.printf "File %s has garbage at the end : << %s >>\n" path rest ;%lwt
  Lwt.fail_with ("Cannot parse file " ^ path)

let read_gen ?(consume_all=true) path parser =

  Lwt_io.(with_file ~mode:Input path
            begin fun inch ->
              let%lwt (unconsumed, result) = Angstrom_lwt_unix.parse parser inch in
              
              match result with
              | Ok v ->
                if consume_all && unconsumed.len > 0 then fail_unconsumed "" path unconsumed
                else Lwt.return v
                          
              | Error msg -> fail_unconsumed msg path unconsumed
            end)
  
let read_net ?safe  path = read_gen path (Tina.parse_net ?safe path)

let read_net_places path = read_gen path (Tina.parse_net_places path)

type source = Net of Net.t | Tfg of Tfg.t

let get_plid = function
  | Net net -> Net.get_plid net
  | Tfg tfg -> Tfg.get_nodeid tfg

let read_goals source path = read_gen path (Selt.parse_goals (get_plid source))

let sread_goals source s = 

  let result = Angstrom.parse_string ~consume:All (Selt.parse_goals (get_plid source)) s in
  match result with
  | Ok v -> Lwt.return v
  | Error msg -> Lwt_io.printf "Error : %s\n" msg ;%lwt Lwt.fail_with "Cannot parse string."

let read_tfg net path = read_gen ~consume_all:false path (Reduce.parse_tfg net)
