(**** TEMPLATES for log messages ****)


(*** /!\ ***)
let debugprintf x = Printf.ifprintf stdout x
(*** \!/ ***)

(* let get_tid () = ExtUnix.Specific.gettid () *)


module type TEMPLATE =
sig
  type env
  type chunk = env -> string
  type template = chunk list

  val pid: chunk
  (*  val tid: chunk *)
  val date: chunk
  val millis: chunk
  val section: chunk
  val level: chunk
  val message: chunk

  val (!): string -> chunk
  val sp: chunk
  val dot: chunk
end

module Template =
struct
  type env =
    { env_section: string ;
      env_level: string ;
      date: float ; (* Unix.time () *)
      message: string }

  type chunk = env -> string
  type template = chunk list

  let pid _ = string_of_int (Unix.getpid ())

  (*  let tid _ = string_of_int (get_tid ()) *)

  let date env =
    let open Unix in
    let tm = localtime env.date in
    
    Printf.sprintf "%d.%02d.%02d  %02d:%02d:%02d"
      (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
      
  let millis env =
    let time = env.date in
    let millis = int_of_float (time *. 1000.0) mod 1000 in
    Printf.sprintf "%03d" millis

  let (!) s = (fun (_:env) -> s)
  let sp = !" "
  let dot = !"."
  
  let section env = env.env_section
  let level env = env.env_level
  let message env = env.message

  let new_env ~date ~section ~level ~message =
    { env_section = section ;
      env_level = level ;
      date ;
      message }
end

module T = Template

type template = T.template

(*****************************************************************************************************************************)

(* This module used to be implemented by Lwt_log (now deprecated). *)

module type LOW_LOG =
sig
  (*
  type level = Lwt_log_core.level = Debug | Info | Notice | Warning | Error | Fatal
  type template = Lwt_log_core.template
*)
  exception Logger_closed of string

  type level = Debug | Info | Notice | Warning | Error | Fatal
               
  type logger

  type section

  val new_section: string -> section

  val section_name: section -> string

  val string_of_level: level -> string

  val file: template:T.template -> perm:Unix.file_perm -> file_name:string -> unit -> logger Lwt.t

  (* Make a logger from a channel. The channel will NOT be closed by Low_log.close *)
  val channel: name:string -> template:T.template -> channel: Lwt_io.output_channel -> unit -> logger

  val log: ?exn:exn -> section:section -> logger:logger -> level:level -> date:float -> string -> unit Lwt.t

  (* Init global log level. 
   * If section is specified, holds only for the given section. *)
  val init: ?section:section -> level -> unit

  val close: logger -> unit Lwt.t
end

module Low_log:LOW_LOG =
struct
  exception Logger_closed of string
  
  type level = Debug | Info | Notice | Warning | Error | Fatal

  type section = { section_name : string ;
                   mutable level: level option }

  type logger =
    { name: string ;
      mutable closed: bool ;
      close: (unit -> unit Lwt.t) ;
      (* section, level, date, message lines *)
      output: section -> level -> float -> string list -> unit Lwt.t }

  let global_level = ref Debug
  
  let new_section n =
    { section_name = n ;
      level = None }

  let init ?section level =
    match section with
    | None -> global_level := level
    | Some s -> s.level <- Some level
      
  let section_name s = s.section_name
      
  let string_of_level = function
    | Debug -> "Debug"
    | Info -> "Info"
    | Notice -> "Notice"
    | Warning -> "Warning"
    | Error -> "Error"
    | Fatal -> "Fatal"

  let close logger =
    if logger.closed then Lwt.fail (Logger_closed logger.name)
    else
      begin
        logger.closed <- true ;
        logger.close ()
      end

  (* Mutexes are needed to avoid interleaving of logs. *)
  let mutexes = Assoc.create ~size:40 ~init:(fun _ -> Lwt_mutex.create ()) () 

  (* Flush before and after logging, to avoid interleaving. 
   * Special care: stdout and stderr are often redirected to the same output (terminal, file),
   * hence we flush both of them. *)
  let myflush channel =
    let () = debugprintf "FLUSH-1\n%!" in
    let%lwt () =
      if channel == Lwt_io.stdout || channel == Lwt_io.stderr then
        begin
          let () = debugprintf "FLUSH-1.1a\n%!" in
          (* let () = Printf.eprintf "eFLUSH-1.1a\n%!" in *)

          let open Lwt_io in
          let () = debugprintf "FLUSH-1.1b\n%!" in
          (* let () = Printf.eprintf "eFLUSH-1.1b\n%!" in *)
          flush stderr ;%lwt  (* FIXME *)
          let () = debugprintf "FLUSH-1.1c\n%!" in
          (* let () = debugprintf "eFLUSH-1.1c\n%!" in *)
          flush stdout ;%lwt  (* FIXME *)
          let () = debugprintf "FLUSH-1.1d\n%!" in
          Lwt.return_unit
        end
      else
        (let () = debugprintf "FLUSH-1.2\n%!" in
        Lwt_io.flush channel)
    in
    let () = debugprintf "FLUSH-2\n%!" in
    Lwt.return_unit
  
  (* The name is used to get the same mutex. *)
  let raw_channel ?(close = fun () -> Lwt.return_unit) ~name ~template ~channel () =

    let _mutex = Assoc.get mutexes name in  (* FIXME *)
    
    let output section level date msg =
      let () = debugprintf "OUTPUT: output-1\n%!" in
      (* let%lwt () = Lwt_mutex.lock mutex in  *)
      let () = debugprintf "OUTPUT: output-2\n%!" in
      let%lwt () = myflush channel in
      let () = debugprintf "OUTPUT: output-3\n%!" in
      let env = T.new_env ~section:(section_name section) ~date ~level:(string_of_level level) ~message:(String.concat "" msg) in
      let () = debugprintf "OUTPUT: output-4 : template length = %d\n%!" (List.length template) in
      let%lwt () = Lwt_list.iter_s (fun chunk -> Lwt_io.write channel (chunk env)) template in
      let () = debugprintf "OUTPUT: output-5\n%!" in
      let%lwt () = Lwt_io.write channel "\n" in
      let () = debugprintf "OUTPUT: output-6\n%!" in
      myflush channel ;%lwt
      (* Lwt.return (Lwt_mutex.unlock mutex) ;%lwt *)
      Lwt.return_unit
    in
    
    { name ;
      closed = false ;
      close ;
      output }

  let channel ~name ~template ~channel () = raw_channel ~name ~template ~channel ()

  let file ~template ~perm ~file_name () =
    let%lwt channel = Lwt_io.(open_file ~flags:Unix.[ O_APPEND ; O_CREAT ; O_WRONLY ] ~perm ~mode:output file_name) in
    let close () = Lwt_io.close channel in
    Lwt.return (raw_channel ~close ~name:file_name ~template ~channel ())
  
  let log ?exn ~section ~logger ~level ~date msg =
    let () = debugprintf "LOWLOG: log-1  closed = %b\n%!" logger.closed in
    
    if logger.closed then Lwt.fail (Logger_closed logger.name)
    else
      let do_it =
        match section.level with
        | None -> !global_level <= level
        | Some l -> l <= level
      in

      let () = debugprintf "LOWLOG: do_it = %b\n%!" do_it in
      
      if do_it then
        let msg_list =
          match exn with
          | None -> [msg]
          | Some e -> [Printexc.to_string e ; "  " ; msg]
        in

        let () = debugprintf "LOWLOG: log-2\n%!" in
        
        let%lwt result = logger.output section level date msg_list in
        let () = debugprintf "LOWLOG: log-3\n%!" in
        Lwt.return result
          
      else Lwt.return_unit
            
end


(*
module Low_log:LOW_LOG =
struct
  type level = Lwt_log_core.level = Debug | Info | Notice | Warning | Error | Fatal
  type template = Lwt_log_core.template
  type logger = Lwt_log_core.logger
  type section = Lwt_log_core.section

  let new_section name = Lwt_log.Section.make name
  let name section = Lwt_log.Section.name section
  let string_of_level lvl = Lwt_log_core.string_of_level lvl
  let file ~template ~perm ~file_name () = Lwt_log.file ~template ~mode:`Append ~perm ~file_name ()
  let channel ~template ~channel () = Lwt_log.channel ~template ~close_mode:`Keep ~channel ()
  let log ?exn ~section ~logger ~level msg = Lwt_log_core.log ?exn ~section ~logger ~level msg
  let init min_loglevel = Lwt_log_core.add_rule "*" min_loglevel
  let close logger = Lwt_log_core.close logger
end
*)

(**********************************************************************************************************************)

type section = Low_log.section

type ll = Low_log.level = Debug | Info | Notice | Warning | Error | Fatal

type level = ll

type path = string

type log_destination =
  | Log_file of string * Unix.file_perm
  | Stdout
  | Stderr

type log_config =
  { failsafe_dir: path ;
    directories_perm: Unix.file_perm ;
    get_destination: string -> level -> log_destination list ;
    get_template: log_destination -> level -> Template.template
  }

let init = Low_log.init

let default_template _ = T.[date ; !"." ; millis ; !" [pid " ; pid ; (* !", " ; tid ; *) !"] " ; level ; sp ; section ; sp ; message]

module A = ANSITerminal
open Term

let ctos style templ = List.map (fun chunk -> (fun env -> mytos style (chunk env))) templ

let default_color_template level =

  let level_style, message_style = match level with
    | Debug -> [A.white], [A.white]
    | Info -> [A.green], [A.white]
    | Notice -> [A.green], [A.cyan]
    | Warning -> [A.red], [A.magenta]
    | Error | Fatal -> [A.Bold ; A.red], [A.Bold ; A.red]
  in
  
  (ctos [A.cyan] T.[date ; dot ; millis]) @
  (ctos [A.blue] T.[!" [pid " ; pid ; (* !", " ; tid ; *) !"]"]) @
  (ctos [A.Bold] T.[sp ; section]) @
  (ctos level_style T.[!" [" ; level ; !"]"]) @
  (ctos message_style T.[sp ; message])

let get_default_template = function
  | Stdout -> if Unix.isatty Unix.stdout then default_color_template else default_template
  | Stderr -> if Unix.isatty Unix.stderr then default_color_template else default_template
  | _ -> default_template

let default_logfile = Printf.sprintf "/tmp/mylog-default-%d" (Unix.getpid ())

(* Destinations used before configuration is done. *)
let default_destinations = Lwt.return
    [ Stderr ; 
      Log_file (default_logfile, 0o644) ]

(* mylog section, used to report logging errors. *)
let mylog_section = Low_log.new_section "mylog"

let failsafe_dir = ref "/tmp/"

type configuration_status = Not_configured | Being_configured | Configured of log_config

let config_status = ref Not_configured

(* Queue of logs that were written before configuration. *)
let queue = Queue.create ()

let dest_tostring = function
  | Log_file (f, _) -> "file " ^ f
  | Stdout -> "stdout"
  | Stderr -> "stderr"

(*** Now, beware of error handling! ***)

let failsafe_name = "mylog-errors-" ^ (Common.current_time ()) ^ "-" ^ (string_of_int (Random.int 1000))

let log_failsafe ?exn ~section:_ level msg =
  try
    let logline = Printf.sprintf "*** %s %s: %s %s"
        (Date2s.to_string (Unix.time ())) (Low_log.string_of_level level) msg (Common.option_apply "" exn (fun e -> Printexc.to_string e))
    in
    Printf.printf "%s\n%!" logline ;

    (* No lwt-calls here. We want to log immediately. 
     * We log to a unique output file (to avoid creating zillions of failsafe files. *)
    let outfile = Filename.concat !failsafe_dir failsafe_name in
    let chout = open_out_gen [Open_append ; Open_creat] 0o644 outfile in
    Printf.fprintf chout "%s\n%!" logline ;
    close_out chout ;

    Lwt.return_unit

  with e ->
    (* Total failure. What can we do? *)
    Printf.printf "*** MYLOG FAILURE: logging is just impossible on this damn machine: %s\n%!" (Printexc.to_string e) ;
    Lwt.return_unit

let report_error_1 ?exn msg = log_failsafe ?exn ~section:mylog_section Error msg

let test_log_failsafe = report_error_1

(* We associate a logger status to each pair (dest, level). 
   and a dest_status to each dest. *)
type logger_status =
  { logger: Low_log.logger Lwt.t ;

    (* Indicates if this logger is ready for more messages. *)
    logready: Lwt_mutex.t ;
  }

type dest_status =
  {
    (* Last line that was logged (plus section name) *)
    mutable lastline: string ;
    
    (* Section of last line *)
    mutable lastsection: section option ;

    mutable lastlevel: level ;

    (* Number of consecutive occurrences of the last line. *)
    mutable count: int }

(* May raise exceptions. *)
let create_dest_logger (dest,level) =
  (*  Printf.printf "Creating dest_logger for %s and level %s \n%!" (dest_tostring dest) (Low_log.string_of_level level) ; *)
  let (template, dir_perm) = 
    match !config_status with
    | Not_configured | Being_configured -> (get_default_template dest level, 0o755)
    | Configured cf -> (cf.get_template dest level, cf.directories_perm)
  in

  let logger =
    match dest with
    | Log_file (file_name, perm) ->
      let dirname = Filename.dirname file_name in
      let%lwt () = Lwtfile.mkdir ~parents:true dirname dir_perm in
      Low_log.file ~template ~perm ~file_name ()
        
    | Stdout -> Lwt.return (Low_log.channel ~name:"stdout" ~template ~channel:Lwt_io.stdout ())
    | Stderr -> Lwt.return (Low_log.channel ~name:"stderr" ~template ~channel:Lwt_io.stderr ())
  in

  { logger ;
    logready = Lwt_mutex.create () }

let create_dest_status _dest =
  { lastline = "" ;
    lastsection = None ;
    lastlevel = Info ;
    count = 0 }
                
(* Memoize existing loggers.
 * The table maps (dest, level) to logger_status *)
let dest_to_logger = Assoc.create ~size:16 ~init:create_dest_logger ()

(* Associate dest status to each dest. *)
let dest_to_status = Assoc.create ~size:16 ~init:create_dest_status ()

let create_directory perm dir =
  try%lwt (Lwtfile.mkdir ~parents:true dir) perm
  with e -> report_error_1 ~exn:e "Mylog.create_directory failed"
              
(* Errors when invoking get_dest ? *)
let get_dest_error = ref false

(* early : indicates if this log line was inserted before configuration 
 * (and might therefore  be skipped when the configuration is done, since it has a duplicate in the queue). *)
let log_to_dests early ?exn section level date msg dests =

  let () = debugprintf "MYLOG: log_to_dests-1\n%!" in
  
  (* Logs the message on each destination *)
  let failsafe_log = ref false in

  let%lwt () =
    (* FIXME: iter_p, once debug is finished *)
    Lwt_list.iter_s begin fun dest ->

      let () = debugprintf "MYLOG: log_to_dests-iter-1\n%!" in
      
      try%lwt

        let () = debugprintf "MYLOG: log_to_dests-iter-2\n%!" in
        
        let logger_status = Assoc.get dest_to_logger (dest, level)
        and dest_status = Assoc.get dest_to_status dest in

        let () = debugprintf "MYLOG: log_to_dests-iter-3\n%!" in

        let%lwt logger = logger_status.logger in


        Lwt_mutex.with_lock logger_status.logready
          begin fun () ->
            (* LOCK IS NOW ACQUIRED *)

            let () = debugprintf "MYLOG: log_to_dests-iter-4\n%!" in

            (* We may skip this logline if it is a known early. *)
            let do_log = match (early, !config_status) with
              | false, _ -> true
              | true, (Not_configured | Being_configured) -> true
              | true, Configured _ -> false                     
            in

            let () = debugprintf "MYLOG: log_to_dests-iter-5 do_log = %b\n%!" do_log in

            if do_log then

              let record_lastline = Low_log.section_name section ^ "||" ^ msg in

              let () = debugprintf "MYLOG: log_to_dests-iter-6\n%!" in

              if dest_status.count > 0 && dest_status.lastline = record_lastline then
                (* It is a duplicate line *)
                begin
                  let () = debugprintf "MYLOG: log_to_dests-iter-7.1\n%!" in
                  dest_status.count <- dest_status.count + 1 ;
                  if dest_status.count = 2 then Low_log.log ~section ~logger ~level ~date "[...]"
                  else Lwt.return_unit
                end

              else
                begin
                  let () = debugprintf "MYLOG: log_to_dests-iter-7.2\n%!" in

                  (* Do we have to show the previous duplicates? *)
                  let%lwt () =
                    if dest_status.count > 1 then
                      let repeat_msg = match dest_status.count with
                        | 1 -> "once"
                        | 2 -> "twice"
                        | n -> string_of_int n ^ " times"

                      and lastsection = match dest_status.lastsection with
                        | None -> section
                        | Some s -> s
                      in

                      let () = debugprintf "MYLOG: log_to_dests-iter-7.3\n%!" in
                      let%lwt () = Low_log.log ~section:lastsection ~logger ~date ~level:dest_status.lastlevel (Printf.sprintf "[Previous message repeated %s]" repeat_msg) in
                      let () = debugprintf "MYLOG: log_to_dests-iter-7.4\n%!" in
                      Lwt.return_unit
                    else Lwt.return_unit
                  in

                  let () = debugprintf "MYLOG: log_to_dests-iter-8\n%!" in

                  let%lwt () = Low_log.log ?exn ~section ~logger ~level ~date msg in

                  dest_status.lastline <- record_lastline ;
                  dest_status.lastsection <- Some section ;
                  dest_status.lastlevel <- level ;
                  dest_status.count <- 1 ;

                  Lwt.return_unit
                end

            else Lwt.return_unit
          end

      with e ->
        let () = debugprintf "MYLOG: log_to_dests-EXCEPTION !!!\n%!" in
        let%lwt () = report_error_1 ~exn:e (Printf.sprintf "[PID %d] Cannot log to destination %s" (Unix.getpid ()) (* (get_tid ()) *) (dest_tostring dest)) in
        if not !failsafe_log then 
          begin
            failsafe_log := true ;
            log_failsafe ?exn ~section level msg
          end
        else Lwt.return_unit
    end
      dests
  in
  
  let () = debugprintf "MYLOG: log_to_dests-done\n%!" in

  Lwt.return_unit

(* Report a logging error. *)
let report_error_2 ?exn date msg dests = log_to_dests false ?exn mylog_section Error date msg dests

(* early: indicates if this log line was inserted before configuration. *)
let log_raw early ?exn section level date msg =

  let () = debugprintf "MYLOG: log_raw-1\n%!" in
  
  let%lwt destinations = match !config_status with
    | Not_configured | Being_configured -> default_destinations
    | Configured cf ->
      try Lwt.return (cf.get_destination (Low_log.section_name section) level)
      with e ->
        (* Cannot get the destinations! We have to log this error too (and only once). *)
        let%lwt () =
          let%lwt dests = default_destinations in
          if not !get_dest_error then report_error_2 ~exn:e date "get_destination failed" dests
          else Lwt.return_unit
        in
        get_dest_error := true ;
        default_destinations
  in

  let () = debugprintf "MYLOG: log_raw-2\n%!" in
  
  log_to_dests early ?exn section level date msg destinations


(* Check if logs are awaiting to be dispatched. *)
let rec check_queue () =
  if Queue.is_empty queue then Lwt.return_unit
  else
    (* Needs to be dispatched. *)
    let (exn, section, level, date, msg) = Queue.take queue in
    let%lwt () = log_raw false ?exn section level date msg in
    check_queue ()

(* Main log function *)
let choose_log ?exn section level date msg =

  debugprintf "MYLOG: choose_log  status = %s\n%!"
    (match !config_status with Not_configured -> "not-configured"
                             | Being_configured -> "begin-configured"
                             | Configured _ -> "configured") ;
  
  match !config_status with
  | Not_configured | Being_configured ->
    (* Register this log for further dispatch. *)
    Queue.add (exn, section, level, date, msg) queue ;
    debugprintf "MYLOG: queued.\n%!" ;
    let%lwt result = log_raw true ?exn section level date msg in
    debugprintf "MYLOG: choose_log done.\n%!" ;
    Lwt.return result

  | Configured _ ->
    (* Already configured. Check if the queue needs to be emptied. *)
    let%lwt () = check_queue () in
    log_raw false ?exn section level date msg

(* check_queue is thread-safe: it preserves message ordering. *)
let flush () = Lwt.async check_queue

(* unit Lwt.t ref  to keep the order of asynchronous logs. *)
let async_logs = ref Lwt.return_unit

module MkSection (Section : sig val section_name: string end) =
struct
  type level = Low_log.level
  = Debug | Info | Notice | Warning | Error | Fatal 

  let section = Low_log.new_section Section.section_name

  let log ?exn level msg = choose_log ?exn section level (Unix.gettimeofday ()) msg

  let log_f ?exn level fmt = Printf.ksprintf (fun s -> log ?exn level s) fmt
      
  let alog ?exn level msg =
    let date = Unix.gettimeofday () in
    Queue.add (exn, section, level, date, msg) queue ;
    match !config_status with
    | Not_configured | Being_configured ->
      (* Registered for future dispatch, but also logs it now. *)
      async_logs := Lwt.bind !async_logs (fun () -> log_raw true ?exn section level date ("__" ^ msg)) ;
      Lwt.async (fun () -> !async_logs)      
      
    | Configured _ -> flush ()
      
  let alog_f ?exn level fmt = Printf.ksprintf (fun s -> alog ?exn level s) fmt

  let sync_outlog ?exn level msg = Lwt_preemptive.run_in_main (fun () -> log ?exn level msg)
      
  let sync_outlog_f ?exn level fmt = Printf.ksprintf (fun s -> sync_outlog ?exn level s) fmt
end

module LL = MkSection (struct let section_name = "MyLog" end)


let add_destination new_dest =
  match !config_status with
  | Not_configured | Being_configured -> LL.alog Error "add_destination: logs are not ready."
  | Configured cf ->
    
    let get_destination sn lvl =
      let (mode, dests) = new_dest sn lvl in
      let extra = match mode with `Replace -> false | `Extra -> true in

      if extra then List.rev_append dests (cf.get_destination sn lvl)
      else dests
    in
    
    config_status := Configured { cf with get_destination } ;
    ()




(* Set a default min loglevel -- by default, all.
 * (Because it is just impossible to understand what happens if some logs are discarded BY DEFAULT.) 
 *)
let () = Low_log.init Debug


(* Sets the configuration (once). *)
let init_config config =

  match !config_status with
  | Being_configured | Configured _ ->
    LL.log Error ("init_config: logs are already configured. Cannot call init_config twice.\n\n" ^
                  "   Check carefully your code. If you cannot identify why this happens, it could be because\n" ^
                  "   one of your module has an unexpected dependency to a runnable module (one which configures log and launches something else).\n" ^
                  "   This can cause serious bugs, because the executable that is run is NOT the one you expected.\n\n")

  | Not_configured ->
    config_status := Being_configured ;

    (* Create the necessary directories *)
    let%lwt () = create_directory config.directories_perm config.failsafe_dir in
    begin
      failsafe_dir := config.failsafe_dir ;

      (* Remove the loggers for which the template has changed (so that it will be recreated). *)
      let removable = Assoc.fold dest_to_logger []
          begin fun (dest,level) logger_status acu ->
            if config.get_template dest == default_template || config.get_template dest == default_color_template then acu
            else (dest, level, logger_status.logger) :: acu
          end
      in
      (* We discard the pending log operations (log_op). These log lines are in the Queue anyway. *)
      
      let%lwt () = Lwt_list.iter_p
          begin fun (dest, level, llogger) ->
            Assoc.remove dest_to_logger (dest, level) ; 
            let%lwt logger = llogger in
            try%lwt Low_log.close logger
            with e -> report_error_1 ~exn:e "Mylog.init_config: close logger failed"
          end
          removable
      in
      
      config_status := Configured config ;      
      check_queue ()
    end


open Common.Small

let full_init ?(min_loglevel=Debug) ?(stdout=false) ?(stderr=true) ?(appfile=true) ?(failsafe_dir="/tmp") appname =

  let () = Low_log.init min_loglevel in

  let homedir = Futils.find_homedir () in
  let appfile = if appfile then Some (Log_file (homedir ^ "/.log/" ^ appname, 0o600)) else None in

  let dest = (if stdout then Some Stdout else None) ^:: 
             (if stderr then Some Stderr else None) ^::
             appfile ^:: 
             []
  in

  init_config {
    failsafe_dir ;
    directories_perm = 0o700 ;
    get_destination = (fun _ _ -> dest) ;
    get_template = get_default_template ;
  }

let shorten n = String.capitalize_ascii Filename.(chop_extension (basename n))
