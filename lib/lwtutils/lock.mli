
(* Acquire a distributed lock, using a distributed file system such as NFS.
 * Soundness is not guaranteed (since it depends on the underlying file system). *)

(* Type of an acquired lock. *)
type t

(* Type of an unacquired lock. *)
type un_t

(* Result of lock acquisition *)
type result =
  (* OK, the given lock is acquired *)
  | OK of t

  (* Nope, the lock is already owned by the given owner. *)
  | Failed of un_t * owner_info

and owner_info =
    { owner_name  : string ;
      owner_login : string ;
      owner_ip    : string ;
      owner_pid   : int ;
      acquisition_date : float ;
      extra       : string ;
      dir         : string ;
      lockname    : string }

(* Acquire the lock identified by the given name.
 * The lockname must be compatible with a file name (avoid special characters). Can be anything.
 * Ownername is a string identifying the owner in any reasonable way. It does not need to be unique.
 * extra : extra information stored in the lock.
 * The lock file is created in the given directory, which is supposed to be shared between different
 * processes (/tmp for instance) or machines (by a NFS file system for instance). 
 *
 * may_fail: true by default. Raises a Failure exception if unexpected behaviour is observed (e.g. impossible to read the lock, which seems to exist).
 *           otherwise, we pretend to have obtained the lock (unsafe, but better than blocking).
 **)
val acquire : ?may_fail:bool -> ?extra:string -> dir:string -> lockname:string -> ownername:string -> unit -> result Lwt.t

(* @raise Failure when releasing a lock twice *)
val release : t -> unit Lwt.t

(* timeout: if the lock cannot be obtained within this delay, either we fail (if may_fail is true), or we proceed (if may_fail is false). *)
val with_lock: ?may_fail:bool -> ?timeout:float -> dir:string -> lockname:string -> ownername:string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(* Forces deletion of an existing lock. Of course, it is unsafe. 
 * Raises no exception, even if it fails. *)
val unsafe_deletion : un_t -> unit Lwt.t
