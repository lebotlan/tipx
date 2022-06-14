(* File utils, lwt compliant! *)

type path = FilePath.filename

exception MkDirFailure of string

(* Indicates if the given path is a directory. 
 * Cannot fail, returns false by default. *)
val is_dir: path -> bool Lwt.t

val is_file: ?follow:bool -> path -> bool Lwt.t

(* Indicates if the given path exists. 
 * follow: for symlinks *)
val exists: ?follow:bool -> path -> bool Lwt.t

val is_executable: path -> bool Lwt.t

(* Creates the given directory. Creates the parent directories if necessary. *)
val mkdir: ?parents:bool -> path -> Unix.file_perm -> unit Lwt.t

val umask_from_perm: Unix.file_perm -> string

(* Creates a text file with the given content + the given lines.
 * flags: by default [O_WRONLY ; O_TRUNC ; O_CREAT] *)
val mk_text_file: ?flags:Unix.open_flag list -> ?mode:Unix.file_perm -> path:path -> ?lines:string list -> string -> unit Lwt.t

(* Writes _all_ the expected data. *)
val mywrite: Lwt_unix.file_descr -> bytes -> int -> int -> unit Lwt.t

(* Reads a file content: size, content. *)
val read_file: path -> (int * string) Lwt.t

(* Check that the file systems allows for creating a file of this size. (Removes the file, then.). *)
val check_quota: dir:path -> size:int -> bool Lwt.t

(*** Look for files in a given global-configured dir. ***)

exception File_does_not_exist of path
					      
(* The function returned by find_path gets a relative or absolute path ;
 * If the path is relative, it is searched in dir, if defined, or defaultdir otherwise. 
 * undefined_msg is the error message put into a Failure exception if both keys are undefined. 
 * @raise File_does_not_exist if check_exists is true and the file is not here. *)
val find_path: defaultdir:path Lwt.key -> undefined_msg:string -> unit
	       -> ?check_exists:bool -> ?dir:path Lwt.key -> path -> path


(***  ITERATORS  ***)

(* Opens a file and iter a function over every line. *)

(* rm_empty_lines: should we ignore empty lines? 
 * ignore_regexp: if one regexp matches the line, the line is ignored. 
 * cb function is invoked like this:  cb line_number line acu *)
val fold_file : file:path -> ?rm_empty_lines:bool -> ?flags:Pcre.cflag list -> ?ignore_regexp:string list -> (int -> string -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

val iter_file : file:path -> ?rm_empty_lines:bool -> ?flags:Pcre.cflag list -> ?ignore_regexp:string list -> (int -> string -> unit Lwt.t) -> unit Lwt.t
