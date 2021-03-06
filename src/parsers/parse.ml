open Petrinet
open Angstrom.Buffered

type path = string


let fail_unconsumed path u =
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
                if consume_all && unconsumed.len > 0 then fail_unconsumed path unconsumed
                else Lwt.return v
                          
              | Error msg ->
                Lwt_io.printf "Error : %s\n%!" msg ;%lwt
                fail_unconsumed path unconsumed
            end)
  
let read_net ?safe  path = read_gen path (Tina.parse_net ?safe path)

let read_goal net path = read_gen path (Selt.parse_goal (Net.get_plid net))

let read_tfg net path = read_gen ~consume_all:false path (Reduce.parse_tfg net)
