module Log = Mylog.MkSection (struct let section_name = Mylog.shorten __FILE__ end)
open Log

type ipv4 = int * int * int * int
          
let get_ipv4 () =

  try%lwt
    let lines = Lwt_process.pread_lines ("", [| "hostname" ; "-I" |]) in
    let%lwt addrs = Lwt_stream.fold (fun x y -> y ^ (if y = "" then "" else " ") ^ x) lines "" in
    let addrs = Text.split ~sep:" " addrs in

    (* On ne garde que les IPv4 *)
    let ipv4 = Common.revmapfilter addrs (fun s -> try Scanf.sscanf s "%d.%d.%d.%d%!" (fun a b c d -> Some (a,b,c,d)) with _ -> None) in
    
    match ipv4 with
    | [] -> Lwt.return_none
    | [x] -> Lwt.return_some x
    | x :: _y :: _ -> Lwt.return_some x

  with exn ->
    let%lwt () = log ~exn Error "(get_ipv4 fails)" in
    Lwt.fail exn 
