let names = Hashtbl.create 40

let () =
  Hashtbl.add names Sys.sigabrt "sigabrt" ;
  Hashtbl.add names Sys.sigalrm "sigalrm" ;
  Hashtbl.add names Sys.sigbus "sigbus" ;
  Hashtbl.add names Sys.sigchld "sigchld" ;
  Hashtbl.add names Sys.sigcont "sigcont" ;
  Hashtbl.add names Sys.sigfpe "sigfpe" ;
  Hashtbl.add names Sys.sighup "sighup" ;
  Hashtbl.add names Sys.sigill "sigill" ;
  Hashtbl.add names Sys.sigint "sigint" ;
  Hashtbl.add names Sys.sigkill "sigkill" ;
  Hashtbl.add names Sys.sigpipe "sigpipe" ;
  Hashtbl.add names Sys.sigpoll "sigpoll" ;
  Hashtbl.add names Sys.sigprof "sigprof" ;
  Hashtbl.add names Sys.sigquit "sigquit" ;
  Hashtbl.add names Sys.sigsegv "sigsegv" ;
  Hashtbl.add names Sys.sigstop "sigstop" ;
  Hashtbl.add names Sys.sigsys "sigsys" ;
  Hashtbl.add names Sys.sigterm "sigterm" ;
  Hashtbl.add names Sys.sigtrap "sigtrap" ;
  Hashtbl.add names Sys.sigtstp "sigtstp" ;
  Hashtbl.add names Sys.sigttin "sigttin" ;
  Hashtbl.add names Sys.sigttou "sigttou" ;
  Hashtbl.add names Sys.sigurg "sigurg" ;
  Hashtbl.add names Sys.sigusr1 "sigusr1" ;
  Hashtbl.add names Sys.sigusr2 "sigusr2" ;
  Hashtbl.add names Sys.sigvtalrm "sigvtalrm" ;
  Hashtbl.add names Sys.sigxcpu "sigxcpu" ;
  Hashtbl.add names Sys.sigxfsz "sigxfsz" ;
  ()  

let sname id =
  try Hashtbl.find names id
  with Not_found -> "*unknown signal*"

let mk_record ~delay =
  let last = Assoc.create ~size:40 ~init:(fun _ -> 0.0) () in
  fun signal ->
    let now = Unix.gettimeofday () in
    let previous = Assoc.get last signal in

    if (signal = Sys.sigint || signal = Sys.sigquit) && now -. previous <= delay then
      begin
        Printf.printf "\n\nReceiving signal %s twice within %.2fs => EXIT.\n%!" (sname signal) delay ;
        exit 2 ;
      end ;
    
    Assoc.set last signal now ;
    ()
