
module Log = Mylog.MkSection (struct let section_name = Mylog.shorten __FILE__ end)
open Log

(* Get login 
 *   Unix.getlogin () may fail !!?! *)
let getlogin () =

  let login =  
    let open Unix in
    try getenv "USER"
    with exn ->

      alog_f ~exn Warning "Unix.getenv USER failed" ;
      try getlogin ()
      with exn ->

        alog_f ~exn Warning "Unix.getlogin failed" ;
        (getpwuid (getuid ())).pw_name
  in

  (* We had a capitalized login once ?!? 
   * But on next try, the login was lowercase again. *)
  let low = String.lowercase_ascii login in
  if low <> login then alog_f Warning "Unexpected case in login : '%s'" login ;

  low
  


