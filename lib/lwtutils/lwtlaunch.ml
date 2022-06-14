(* let (>>=) key v f = Lwt.with_value key (Some v) f *)

module Setkeys =
struct
  type 'b config = (unit -> 'b) -> 'b
  let (%=) key v = (fun f -> Lwt.with_value key (Some v) f)
  let (++) cf1 cf2 = (fun f -> cf2 (fun () -> cf1 f))
  let (=>>) cf f = cf f
  let (===>) cf f = cf (fun () -> Lwt_main.run (f ()))
  let noconfig = (fun f -> f ())
end

module Log = Mylog.MkSection (struct let section_name = "Main" end)
open Log

let launch ?psignals ?(configure_log=true) ?stdout ?stderr ~appname ~run arg () =

  let () =
    match psignals with
    | None -> ()
    | Some sp ->
      (* Quit if signals are sent in a short delay *)
      let check_signal = Signals.mk_record ~delay:1.1 in

      let send_signal signal =
        Printf.printf "\n\n%!" ;
        alog_f Notice "Received signal %s" (Signals.sname signal) ;
        check_signal signal ;
        
        try Pipe.send sp signal
        with e -> alog_f Error ~exn:e "Exception when handling signal %s" (Signals.sname signal)
      in
      
      let signals = Sys.[ sigint ; sigquit ; sigterm ; sigabrt ; sighup ; sigill ; sigtstp ; sigpipe ] in
      List.iter (fun signal -> ignore(Lwt_unix.on_signal signal send_signal)) signals ;
  in
  
  (* Handle async exceptions too. *)
  Lwt.async_exception_hook :=
    (fun e ->
       Printf.printf "Launch - Async exception hook : %s\n%!" (Printexc.to_string e) ;
       alog_f ~exn:e Fatal "***************************************** %s error (by async hook) ***************************************************\n%s\n"
         appname (Printexc.get_backtrace ())) ;
  
  try%lwt
    let%lwt () = if configure_log then Mylog.full_init ?stdout ?stderr ~appfile:true appname
      else Lwt.return_unit
    in
    run arg
  with e ->
    let%lwt () = log_f ~exn:e Fatal "******************************* %s error ******************************************************\n%s\n"
        appname (Printexc.get_backtrace ())
    in
    Lwt.fail e

