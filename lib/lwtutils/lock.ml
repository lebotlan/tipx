type t =
  { path: string ;
    mutable locked: bool }

type un_t = string

module Log = Mylog.MkSection (struct let section_name = Mylog.shorten __FILE__ end)
open Log

type owner_info =
  { owner_name  : string ;
    owner_login : string ;
    owner_ip    : string ;
    owner_pid   : int ;
    acquisition_date : float ;
    extra       : string ;
    dir         : string ;
    lockname    : string }

type result =
  | OK of t
  | Failed of un_t * owner_info

let remove msg path =
  try%lwt
    if%lwt Lwtfile.exists ~follow:false path then Lwt_unix.unlink path
    else Lwt.return_unit
  with exn ->
    let%lwt () = log ~exn Error msg in
    Lwt.return_unit

let unsafe_deletion path = remove "Lock.unsafe_deletion failed" path

let release lock =
  if lock.locked then
    begin
      let%lwt () = remove "Lock.release failed" lock.path in
      (lock.locked <- false ;
       Lwt.return_unit)
    end
  else Lwt.fail_with "Lock.release: lock is already released."

let sep = '\030'

let (//) = Filename.concat

let parse dir lockname s =
  Scanf.sscanf s "%s@\030%s@\030%s@\030%d\030%f\030%s"
    (fun owner_name owner_login owner_ip owner_pid date extra ->
       { owner_name ;
         owner_login ;
         owner_ip ;
         owner_pid ;
         acquisition_date = date ;
         extra ;
         dir ;
         lockname })

let acquire ?(may_fail=true) ?(extra="") ~dir ~lockname ~ownername () =

  let filename = ".mutex-" ^ lockname
  and login = Common.getlogin ()
  and pid = Unix.getpid ()
  and date = Unix.gettimeofday () in
  
  let%lwt ip =
    match%lwt Lwt_ip.get_ipv4 () with
    | None -> Lwt.return "ip.none"
    | Some (a,b,c,d) -> Lwt.return (Printf.sprintf "%d.%d.%d.%d" a b c d)
  in
  
  let path = dir // filename
  and content = Printf.sprintf "%s%c%s%c%s%c%d%c%.3f%c%s" ownername sep login sep ip sep pid sep date sep extra
  in

  (* Result is None if all went well,
   * and Some content if the link already exists. *)
  let rec loop i =
    if i >= 2 then
      (* We failed twice to obtain a lock or to read an existing lock. *)
      if may_fail then Lwt.fail_with "Lock.acquire: unexpected behavior."
      else Lwt.return None
        
    else
      try%lwt
        (* Check if the link exists. *)
        if%lwt Lwtfile.exists ~follow:false path then
          let%lwt read = Lwt_unix.readlink path in
          Lwt.return_some (parse dir lockname read)
            
        else          
          (* Try to create the link. *)
          let%lwt () = Lwt_unix.symlink content path in
          
          (* Wait a little. *)
          let%lwt () = Lwt_unix.sleep 0.1 in
          
          (* Re-read the link. *)
          let%lwt read = Lwt_unix.readlink path in

          if read = content then Lwt.return_none
          else
            let%lwt () = log_f Error "Lock.acquire: link %s, read %s instead of %s" path read content in
            let%lwt () = Lwt_unix.unlink path in
            loop (i+1)
          
      with exn ->
        let%lwt () = log ~exn Error "Lock.acquire" in
        loop (i+1)
  in

  match%lwt loop 0 with
  | None ->
    let lock = { path ;
                 locked = true }
    in
    Lwt_main.at_exit (fun () -> if lock.locked then release lock else Lwt.return_unit) ;
    Lwt.return (OK lock)    

  | Some other -> Lwt.return (Failed (path, other))

let with_lock ?(may_fail=true) ?timeout ~dir ~lockname ~ownername f =

  let start_date = Unix.gettimeofday () in  
  
  let rec loop () =
    match%lwt acquire ~may_fail ~dir ~lockname ~ownername () with
    | OK l -> Lwt.return_some l
    | Failed _ ->
      (* Timeout ? *)
      begin match timeout with
        | None -> retry ()
        | Some tm ->
          let now = Unix.gettimeofday () in
          if now -. start_date > tm then
            if may_fail then Lwtplus.myfail "Lock.with_lock: Timeout (%.2fs)" tm
            else Lwt.return_none (* Proceed *)              
          else retry ()
      end

  and retry () =
    (* Wait a little bit, then retry. *)
    let%lwt () = Lwt_unix.sleep 0.1 in
    loop ()
  in

  let%lwt olock = loop () in

  begin
    f ()
  end
  [%finally match olock with None -> Lwt.return_unit | Some l -> release l]

