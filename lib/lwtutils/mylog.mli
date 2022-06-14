(*** Log functions, lwt compliant ***)

(* Here are the main ideas:
 *   - Libraries directly invoke log functions to log their messages.
 *   - Libraries are not concerned about where the log messages finally go. 
 *   - Each library is expected to define its own section.
 *   - The main application globally configures log destinations, possibly by filtering on log levels and log sections.
 *
 * Hence, the destinations configuration is global for the whole application. It is done once.
 * For instance, all logs may go to the same file, or set of files, or to the same directory. *)


(* None of these functions may fail directly (presumably), except init_config. 
 * Errors will only be logged, sowehow, without failing the calling program. *)


type ll = Debug | Info | Notice | Warning | Error | Fatal

type level = ll

type section

(*
 * To configure global logging level, you may use:
 *    let () = init Debug
 *
 * To configure logging level for a section only, use:
 *    let () = init ~section Error
 *
 * The section logging level prevails over the global logging level.
 *
 *)
val init: ?section:section -> level -> unit

(* Given a section name, creates a (new) section. Returns the logging functions.  *)
module MkSection (Section : sig val section_name: string end) :
sig
  (* We repeat the type here so that it suffices to open the newly created module. *)
  type level = ll = Debug | Info | Notice | Warning | Error | Fatal 

  val section: section

  (* Logs a message in the current section. *)
  val log: ?exn:exn -> level -> string -> unit Lwt.t

  (* Like log, but takes a format string *)
  val log_f: ?exn:exn -> level -> ('a, unit, string, unit Lwt.t) format4 -> 'a

  (* Asynchronous log (preserves logs ordering). Will be flushed asap. *)
  val alog: ?exn:exn -> level -> string -> unit
  val alog_f: ?exn:exn -> level -> ('a, unit, string, unit) format4 -> 'a

  (* Code executed outside the main-lwt thread should not create any lwt promise,
   * unless using Lwt_preemptive.run_in_main. 
   *
   * Hence, these are the ONLY logging function you may call outside the main thread.
  *)
  val sync_outlog: ?exn:exn -> level -> string -> unit
  val sync_outlog_f: ?exn:exn -> level -> ('a, unit, string, unit) format4 -> 'a
  
end


(***  CONFIGURATION of LOG DESTINATIONS ***)

(* The functions below can be used to configure logging destinations, according to sections and level.
 *
 * If log functions (log, log_f) are invoked before the initial configuration, 
 * these log messages are both sent to a set of default loggers (stdout, /tmp/)
 * and also queued, and will be sent to the expected destinations once configuration is done. *)

(* Full file path. *)
type path = string

(* Possible log destinations *)
type log_destination =
  (* The directories are created if necessary. *)
  | Log_file of path * Unix.file_perm

  | Stdout

  | Stderr

(* Not available (contains a functional value - used as a key in a hashtable. Bad bad bad.)
  | Log_fun of (string -> unit Lwt.t)
*)

module type TEMPLATE =
sig
  type env
  type chunk = env -> string
  type template = chunk list

  val pid: chunk
  (*  val tid: chunk (* Thread id *) *)
  val date: chunk
  val millis: chunk
  val section: chunk
  val level: chunk
  val message: chunk

  val (!): string -> chunk
  val sp: chunk
  val dot: chunk
end

module Template : TEMPLATE

type template = Template.template

(* Some default template *)
val default_template: level -> template

(* Chooses a color template for stdout and stderr. *)
val get_default_template: log_destination -> level -> template

type log_config =
  { (* Failsafe directory to log critical errors when everything else failed. 
     * (Creates the directory if necessary). Prefer an existing directory, though. *)
    failsafe_dir: path ;

    (* If directories need to be created, use these permissions. *)
    directories_perm: Unix.file_perm ;

    (* get_destination  section_name level: indicates where to put logs of this section and this level. 
     * Messages under the logging threshold are discarded, though (see the init function). *)
    get_destination: string -> level -> log_destination list ;

    (* Template to be used according to the given destination (and level)
     * This helps to ensure that a given destination uses similar templates. *)
    get_template: log_destination -> level -> template ;
  }

(* Initial configuration. Shall be called only once. *)
val init_config: log_config -> unit Lwt.t

(* get_destination can be overloaded. 
 * `Replace: replace the previous binding
 * `Extra: add an extra binding. 
 * init_config must have already been invoked (and has returned). 
 * If the returned list is empty, use the *)
val add_destination: (string -> level -> ([`Replace | `Extra] * (log_destination list))) -> unit

val test_log_failsafe: ?exn:exn -> string -> unit Lwt.t

(*** Here is some typical usage:

  Mylog.(
    let dest = [ Stderr ; Log_file ("/tmp/mylogdir/mylogfile", 0o600) ] in
    
    init_config {
      failsafe_dir = "/tmp" ;
      directories_perm = 0o700 ;
      get_destination = (fun _ _ -> dest) ;
      get_template = (fun _ -> default_template) ;
    })
    
*)

(* The above typical example can be invoked with this function: 
 * If appfile is true, a log file ~/.log/appname is created. 
 * string argument is appname. *)
val full_init: ?min_loglevel:level -> ?stdout:bool -> ?stderr:bool ->
  ?appfile:bool -> ?failsafe_dir:path -> string -> unit Lwt.t


(* Shorten file names into a module name.
 * To be used like this:
   module Log = Mylog.MkSection (struct let section_name = Mylog.shorten __FILE__ end)
   open Log
 *)
val shorten: string -> string


