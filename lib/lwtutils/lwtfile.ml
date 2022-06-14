type path = FilePath.filename


open Lwt_unix

exception MkDirFailure of string

let is_dir path =
  (* If something fails, we return false by default. *)
  try%lwt
    let%lwt stats = stat path in
    Lwt.return (stats.st_kind = S_DIR)
  with _ -> Lwt.return_false

let is_file ?(follow=true) path =
  try%lwt
    let%lwt stats = (if follow then stat else lstat) path in
    Lwt.return (stats.st_kind = S_REG)
  with _ -> Lwt.return_false
  

let exists ?(follow=true) path =
  try%lwt
    let%lwt _stats = (if follow then Lwt_unix.stat else Lwt_unix.lstat) path in
    Lwt.return_true
  with _ -> Lwt.return_false

let is_executable path =
  try%lwt
    let%lwt () = access path [X_OK] in
    Lwt.return_true
  with _ -> Lwt.return_false

let rec mkdir ?(parents=true) path perm =

  (*  lwt () = Lwt_io.printf "Calling mkdir '%s'\n" path in *)

  let%lwt () = if Common.starts_with "~" path then Lwt.fail (MkDirFailure "Do not use ~ in directory name, expand it by yourself (sorry).") else Lwt.return_unit in

  (* Test path_exists before is_dir, because of a possible race-condition. *)
  let%lwt path_exists = exists path in
  let%lwt path_is_dir = is_dir path in

  if path_is_dir || path = "" then
    (*    lwt () = Lwt_io.printf "%s is already there\n" path in *)
    (* The directory is already there. Nothing to do. *)
    Lwt.return_unit
  else
  if path_exists then
    (* The path already exists but is not a directory. *)
    Lwt.fail (MkDirFailure (path ^ " already exists, but is not a directory."))
  else

    begin
      (* Create parents? *)
      let parent = FilePath.dirname path in
      let%lwt parent_exists = is_dir parent in

      let%lwt () =
        if parent_exists then Lwt.return_unit
        else
        if parents then
          (* Recursive call - well founded recursion <= we make sure the parent is smaller than the current path. *)
          let%lwt () = assert%lwt (String.length parent < String.length path) in
          mkdir ~parents parent perm
        else Lwt.fail (MkDirFailure (parent ^ " is not an existing directory."))
      in

      (*  let%lwt () = Lwt_io.printf "Creating %s\n" path in *)

      try%lwt Lwt_unix.mkdir path perm
      with e ->
        (* Something failed. May be the dir has been created meanwhile (possible race condition, which actually happened! *)
        (*    lwt () = Lwt_io.printf "It failed for %s\n" path in *)
        let%lwt path_is_dir = is_dir path in
        if path_is_dir then Lwt.return_unit else Lwt.fail e
    end

let mk_text_file ?(flags=[O_WRONLY ; O_TRUNC ; O_CREAT]) ?(mode=0o644) ~path ?(lines=[]) content =
  let%lwt file = openfile path flags mode in

  let write_s s = let%lwt _ = write file (Bytes.of_string s) 0 (String.length s) in Lwt.return_unit in
  
  let%lwt _ = write_s content in
  let%lwt () = Lwt_list.iter_s (fun line -> write_s (line ^ "\n")) lines in
  close file

exception File_does_not_exist of path

let find_path ~defaultdir ~undefined_msg () =

  let raise_undefined () = failwith undefined_msg in

  (* Get key value, calls f () if key is undefined or empty. *)
  let read_key key f =
    match Lwt.get key with
    | None -> f ()
    | Some v -> if v = "" then f () else v
  in

  let get_default_dir () = read_key defaultdir raise_undefined in

  let find_dir = function
    | None -> get_default_dir ()
    | Some key -> read_key key get_default_dir
  in

  fun ?(check_exists=true) ?dir path ->
    let final_path =
      if Filename.is_relative path then Filename.concat (find_dir dir) path
      else path (* Absolute *)
    in
    if check_exists && not (Sys.file_exists final_path) then raise (File_does_not_exist final_path) 
    else final_path

let rec mywrite fd bytes offset len =
  if len <= 0 then Lwt.return_unit
  else
    let%lwt written = Lwt_unix.write fd bytes offset len in
    if written = 0 then Lwt.fail_with (Printf.sprintf "Lwtfile.mywrite: wrote 0 bytes instead of %d. It sucks." len)
    else mywrite fd bytes (offset+written) (len-written)

let fold_file ~file ?(rm_empty_lines=false) ?flags ?(ignore_regexp=[]) f acu0 =

  let%lwt ignore_list = Lwt.wrap2 List.map (fun s -> Pcre.regexp ?flags s) ignore_regexp in
  let ignores line =
    if rm_empty_lines && line = "" then true
    else List.exists (fun rex -> Pcre.pmatch ~rex line) ignore_list
  in
  let s_in = Lwt_io.lines_of_file file in

  let%lwt (_, acu) = Lwt_stream.fold_s
      (fun line (linenb, acu) ->
         let%lwt ignorable = Lwt.wrap1 ignores line in
         let%lwt acu = if ignorable then Lwt.return acu else f linenb line acu in
         Lwt.return (linenb + 1, acu))
      s_in (1, acu0)
  in
  Lwt.return acu

let iter_file ~file ?rm_empty_lines ?flags ?ignore_regexp f = fold_file ~file ?rm_empty_lines ?flags ?ignore_regexp (fun i s () -> f i s) ()

let read_file path =
  let%lwt stat = Lwt_unix.stat path in
  let size = stat.st_size in
  let file_content = Bytes.create size in

  let%lwt fd = openfile path [ O_RDONLY ] 0 in
  let%lwt bytes_read = Lwt_unix.read fd file_content 0 size in
  let%lwt () = close fd in
  
  let%lwt () = assert%lwt (bytes_read = size) in
  Lwt.return (bytes_read, Bytes.to_string file_content)
    
let umask_from_perm perms = Printf.sprintf "%o" (perms lxor 0o777)

let (//) = Filename.concat

let check_quota ~dir ~size =

  let testfile = dir // ".THIS-TEST-FILE-CAN-BE-REMOVED" in
  
  begin try%lwt
      let content = String.make size 'a' in      
      let%lwt () = mk_text_file ~path:testfile content in
      Lwt.return_true    
    with _ -> Lwt.return_false
  end
    [%lwt.finally
      try%lwt Lwt_unix.unlink testfile
      with _ -> Lwt.return_unit]

