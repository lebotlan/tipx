(* Makes an 'archive' of several files, that can be converted from and into a single string. *)

(* Type of an archive.
 * Note that an archive contains all file contents. *)
type arch

(* Archive should only be built if needed. Hence the following useful type. *)
type lazy_arch = arch Lwt.t Lazy.t

type path = Lwtfile.path

(*** Build an archive ***)

(*
 * Note: archives should be built in order to avoid artificial overwrites when extracting them.
 *    => If all files or tgz occur in disjoint subtrees, no problem.
 *    => Otherwise, put higher directories first
 *    => The correct order is A/  A/B   instead of A/B A (which overwrites A)
 *)

val empty: arch

(* Dummy non-empty arch. *)
val dummy: arch Lwt.t

(* Appends the file content to an archive. 
 * path_in_arch: path that is associated to this file in the archive (path used when extracting the file).
 * path_in_arch must be absolute. The root dir (/) will be replaced by the destination dir when the archive is expanded to filesystem. 
 * perm: by default, the current perms of source file. *)
val append_file: arch -> source:path -> path_in_arch:path -> ?perms:int -> unit -> arch Lwt.t

(* Appends a file with the given content. *)
val append_content: arch -> content:string -> path_in_arch:path -> perms:int -> unit -> arch Lwt.t

(* A (virtual) .tgz file. *)
type tgz

(* Appends a directory to the archive. 
 * When the archive is extracted, this directory will be created, empty. *)
val append_dir: arch -> path_in_arch:path -> ?perms:int -> unit -> arch Lwt.t

(* Appends a tgz. *)
val add_tgz: arch -> tgz -> path_in_arch:path -> unit -> arch Lwt.t

(* Creates a .tgz virtual file. 
 * The source paths may contain wildcards or environment variables (it is interpreted by the shell).
 * Spaces or other special characters must be escaped (they are used as such to build the command). 
 * cd dir  is run before building the tar. 
 * mtime: option given as --mtime to tar e.g. ~mtime:"2018-01-01"
 *        as a result, the tgz content does not depend on the file date, only on the file content.
 * *)
val make_tgz: ?mtime:string -> dir:path -> sources:path list -> unit -> tgz Lwt.t


(* Exports an archive to a string. *)
val dump: arch -> string

(* Exports an archive to a compact string by sharing strings. 
 *  share s  returns the id of the shared string. *)
val dump_share: share:(string -> int) -> arch -> string


(* Builds an archive from a string obtained by 'dump'.
 * @raise Failure is the string is not a valid archive. *)
val arch_of_string: string -> arch Lwt.t

(* It is guaranteed that dump (arch_of_string (dump a)) = dump a *)


(* Ditto with a string obtained by dump_share *)
val arch_of_shared: get:(int -> string Lwt.t) -> string -> arch Lwt.t


(*** Write files from an archive ***)

type file_type = File | Tgz | Dir

type file_desc =
  { (* Absolute path (the root dir / will be replaced by the destination dir. *)
    path: path ;

    perm: Unix.file_perm ;

    ftyp: file_type ;

    (* File size *)
    size: int }

(* Read an archive content *)
val content: arch -> file_desc list

val size: arch -> int

(* Get file content
 * @raise Not_found if the file is not in the archive. *)
val file_content: arch -> path -> string

(* Write file to filesystem. 
 * new_path: destination file. By default, path /p is written to root/p 
 * perms: permissions for directories 
 * map_content: receives the old path (before new_path), and the current content. Returns the new content to be written into the destination file. 
 * Returns a pair : (/path in archive, path to the corresponding written file). 
 * map_content is not invoked on tgz *)
val write_file: root:path -> arch -> path -> ?new_path:path -> ?map_content:(path -> string -> string) -> Unix.file_perm -> unit -> (path * path) Lwt.t

type overw_action =
  (* Just overwrite the file/dir. *)
  | Overwrite

  (* Do not overwrite, skip this file/dir. *)
  | Skip

  (* Make a backup of the given file/dir.
   * If Backup None, choose a standard name for the backup.  *)
  | Backup of path option

(* Write all files to filesystem. 
 * new_paths map the absolute archive path to a filesystem path. 
 *           by default, new_paths map /p to root/p 
 * perms: permissions for directories 
 * Returns a list of pairs : (/path in archive, path to the written file). 
 * overwrite: function called before overwriting a file or dir (the root is never backuped, i.e. path_in_arch /)
 *
 * dry_run: if true, do not actually write files, but test for overwritten files/dir. Return the list of files that would have been written.
 *          in dry_run mode, the overwrite action is nevertheless effective.
 * *)
val write_all: root:path -> ?dry_run: bool -> arch -> ?overwrite:(isdir:bool -> abs:path -> arch:path -> overw_action Lwt.t) -> ?new_paths:(path -> path) -> ?map_content:(path -> string -> string) -> Unix.file_perm -> unit -> (path * path) list Lwt.t


(* Maps /path/ to root/path *)
val reparent: root:path -> path -> path

(* Get a signature of this archive *)
val get_sign: arch -> string

(* Returns the files found in the tgz at the given path.
 * Returns the empty list if the path is a file (not a tgz). *)
val tgz_files: arch -> path -> path list Lwt.t

(* Debug *)
val tgz_content: tgz -> string

