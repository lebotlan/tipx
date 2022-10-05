type path = Lwtfile.path

type tgz = string

type file_type = File | Tgz | Dir

type file_desc =
  { path: path ;
    perm: Unix.file_perm ;
    ftyp: file_type ;
    size: int }

type file_content = string

type entry = file_desc * file_content
    
(* Reversed *)
type arch = entry list

type lazy_arch = arch Lwt.t Lazy.t

let empty = []

open FilePath
open Lwt_unix

let check_path arch path_in_arch =
  (* New path (path_in_arch) in canonical form. *)
  let new_path = reduce ~no_symlink:true path_in_arch in
  
  let%lwt () =
    if FilePath.is_relative new_path then Lwtplus.myfail "Lwtarchive.append_file: path_in_arch=%s must be an absolute path." path_in_arch 
    else if List.exists (fun (desc, _) -> desc.path = new_path) arch then Lwtplus.myfail "Lwtarchive.append_file: %s is already in the archive." new_path
    else Lwt.return_unit
  in

  Lwt.return new_path

let append_dir arch ~path_in_arch ?(perms=0700) () =
  let%lwt new_path = check_path arch path_in_arch in

  let filedesc =
    { path = new_path ;
      perm = perms ;
      ftyp = Dir ;
      size = 0 }
  in
  
  Lwt.return ((filedesc, "") :: arch)
    
let append_file arch ~source ~path_in_arch ?perms () =
  let%lwt new_path = check_path arch path_in_arch in

  (* Source file *)
  let%lwt stat = Lwt_unix.stat source
  and (bytes_read, file_content) = Lwtfile.read_file source in

  if bytes_read <> stat.st_size then Lwt.fail_with ("Lwtarchive.append_file " ^ source ^ " assert false, inconsistent size.")
  else
    let filedesc =
      { path = new_path ;
        perm = (match perms with None -> stat.st_perm | Some p -> p) ;
        ftyp = File ;
        size = stat.st_size }
    in
    
    Lwt.return ((filedesc, file_content) :: arch)

let append_content arch ~content ~path_in_arch ~perms () =
  let%lwt new_path = check_path arch path_in_arch in

  let filedesc =
    { path = new_path ;
      perm = perms ;
      ftyp = File ;
      size = String.length content }
  in

  Lwt.return ((filedesc, content) :: arch)
  

let add_tgz arch tgz ~path_in_arch () =
  let%lwt new_path = check_path arch path_in_arch in
  
  let filedesc =
    { path = new_path ;
      perm = 0 ;
      ftyp = Tgz ;
      size = String.length tgz }
  in

  Lwt.return ((filedesc, tgz) :: arch)


(* sources may contain empty patterns.
 * This stupid stupid stupid stupid stupid bash will keep the pattern as is when it maps to an empty set. That just totally sucks.
 * We cannot use noglob option because this is not actually bash, this is dash.
 *
 * Thus, we have to filter ourselves the files. Sigh. *)
let filter_source dir source =
  let command = "cd \"" ^ dir ^ "\" && stat -c x " ^ source in
  let process = Lwt_process.open_process_none ~stdout:`Dev_null ~stderr:`Dev_null (Lwt_process.shell command) in
  match%lwt process#status with
  | Unix.WEXITED 0 -> Lwt.return_true
  | _ ->
    Lwt_io.printf "FYI: empty source %s has been discarded.\n" source ;%lwt
    Lwt.return_false

  
let make_tgz ?mtime ~dir ~sources () =
  let get_data _command (procin:Lwt_process.process_in) =
    (* Read all data from stdout. *)
    let%lwt content = Lwt_stream.to_string (Lwt_io.read_chars procin#stdout) in

    (* Lwt_io.printf "make_tgz : content signature = %s\n" (Digest.to_hex (Digest.string content)) >> *)
    
    (* Check status *)
    match%lwt procin#status with
    | Unix.WEXITED 0 -> Lwt.return content
    | Unix.WEXITED n -> Lwt.fail_with (Printf.sprintf "Lwtarchive.make_tgz: tar exit status = %d." n)
    | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> Lwt.fail_with "Lwtarchive.make_tgz: tar process was unexpectedly stopped." 
  in

  let%lwt sources = Lwt_list.filter_p (filter_source dir) sources in
  
  (*   let command = Printf.sprintf "cd \"%s\" && /bin/tar jch %s" dir (String.concat " " sources) in *)

  let mtime = match mtime with None -> "" | Some t -> "--mtime='" ^ t ^ "'" in
  
  (* Alternative, so that files are always processed in the same order, whichever the file system. *)
  let precommand = Printf.sprintf "cd \"%s\" && /usr/bin/find %s -print0" dir (String.concat " " sources) in  
  let command = Printf.sprintf "%s | sort -z | tar jch %s --no-recursion --null -T -" precommand mtime in

  (* Printf.printf "Commande : %s\n%!" command ; *)
  
  (* Damnit! We have to launch find first, to check if it succeeds.
   * set -o pipefail  does not exist in dash. *)
  let pre_process = Lwt_process.open_process_none ~stdout:`Dev_null (Lwt_process.shell precommand) in
  let%lwt () =
    match%lwt pre_process#status with
    | Unix.WEXITED 0 -> Lwt.return_unit
    | Unix.WEXITED n -> Lwt.fail_with (Printf.sprintf "Lwtarchive.make_tgz: find exit status = %d." n)
    | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> Lwt.fail_with "Lwtarchive.make_tgz: find process was unexpectedly stopped." 
  in
  
  Lwt_process.with_process_in (Lwt_process.shell command) (get_data command)

(* Format of the final string structure :
 *    8 means 8 bytes (64 bits)
 *
 *  [header-length]8 [type]8 [header] [file1] [file2] [...]
 *  [header] = ( [path-length]8 [file-offset]8 [file-length]8 [perm]8 [ftyp]8 [path] )*
 *
 *  type = 0x600 unshared strings
 *  type = 0x601 shared strings
 *
 *  file-offset = 0 for file1
 *              = file-length1 for file2
 *              = ...
 *)

let invalid = "Lwtarchive: The given string is an invalid archive."

let ftyp_to_int = function
  | File -> 1
  | Tgz -> 2
  | Dir -> 3

let int_to_ftyp = function
  | 1 -> File
  | 2 -> Tgz
  | 3 -> Dir
  | n -> failwith (Printf.sprintf "%s (ftyp = 0x%x)" invalid n)

(* desc.size: file size
 * size: number of bytes used to represent the file content. Can be =desc.size, or can be =8 for shared values. *)
let bytes_of_desc file_offset desc size =
  let len = String.length desc.path in
  let bytes = Bytes.create (40 + len) in
  Bytehelp.dump_int bytes 0 len ;
  Bytehelp.dump_int bytes 8 file_offset ;
  Bytehelp.dump_int bytes 16 size ;
  Bytehelp.dump_int bytes 24 desc.perm ;
  Bytehelp.dump_int bytes 32 (ftyp_to_int desc.ftyp) ;
  Bytes.blit_string desc.path 0 bytes 40 len ;
  (* Printf.printf "Writing at offset = %d, file size = %d  data-length = %d\n%!" file_offset desc.size size ; *)
  (bytes, file_offset + size)

let rec mk_header size_s file_offset = function
  | [] -> Bytes.empty
  | (desc, content) :: rest ->
    let (buf, new_offset) = bytes_of_desc file_offset desc (size_s content) in
    Bytes.cat buf (mk_header size_s new_offset rest)

let gen_dump arch arch_type map_s size_s =
  let header = mk_header size_s 0 arch in
  let full_header = Bytes.extend header 16 0 in
  Bytehelp.dump_int full_header 0 (Bytes.length header) ;
  Bytehelp.dump_int full_header 8 arch_type ;

  let map_content (_,content) =
    let res = map_s content in
    assert (String.length res = size_s res) ;
    res
  in
  
  String.concat "" ((Bytes.to_string full_header) :: (List.map map_content arch))

let dump arch = gen_dump arch 0x600 (fun s -> s) String.length

let dump_share ~share arch =
  (* Every string is replaced by a 8-byte integer. *)
  let map_s s =
    let bytes = Bytes.create 8 in
    Bytehelp.dump_int bytes 0 (share s) ;
    Bytes.to_string bytes
  in
  gen_dump arch 0x601 map_s (fun _ -> 8)
  

let gen_arch_of_string expected_type map_s s =
  if String.length s < 8 then Lwt.fail_with (invalid ^ " (length)")
  else
    try%lwt
      (* Get header *)
      let header_size = Bytehelp.read_int s 0
      and arch_type = Bytehelp.read_int s 8 in

      if arch_type <> expected_type then Lwt.fail_with "(arch type)"
      else

        let startpos = header_size + 16 in

        let rec loop acu offset =
          if offset >= startpos then Lwt.return (List.rev acu)
          else
            begin
              (* Printf.printf "Reading at offset = %d\n%!" offset ; *)
              let path_length = Bytehelp.read_int s offset
              and file_offset = Bytehelp.read_int s (offset+8)
              and data_length = Bytehelp.read_int s (offset+16)
              and perm = Bytehelp.read_int s (offset+24)
              and ftyp = int_to_ftyp (Bytehelp.read_int s (offset+32))
              in
              let path = String.sub s (offset+40) path_length in
              let desc = { path ; perm ; size = 0 ; ftyp } in
              
              (* Printf.printf "Reading at file-offset = %d, data-length = %d\n%!" file_offset data_length ; *)
              
              let raw_content = String.sub s (startpos + file_offset) data_length in
              let%lwt content = map_s raw_content in

              let desc = { desc with size = String.length content } in
              loop ((desc, content) :: acu) (offset+40+path_length)
            end
        in
        loop [] 16

    with e -> Lwt.fail_with (invalid ^ " " ^ Printexc.to_string e)

let arch_of_string s = gen_arch_of_string 0x600 Lwt.return s

let arch_of_shared ~get s =
  let map_s s = get (Bytehelp.read_int s 0) in
  gen_arch_of_string 0x601 map_s s

let content arch = List.rev_map fst arch

let size arch = List.fold_left (fun acu (fdesc,_) -> acu + fdesc.size) 0 arch

let file_content arch path =
  let (_, ct) = List.find (fun (desc, _) -> desc.path = path) arch in
  ct

let raw_write ?(map_content=fun _ s -> s) content perm oldpath path =

  let%lwt content = Lwt.wrap2 map_content oldpath content in
  
  let%lwt fd = Lwt_unix.openfile path [ O_WRONLY ; O_CREAT ; O_TRUNC ] perm in
  let%lwt () = Lwtfile.mywrite fd (Bytes.of_string content) 0 (String.length content) in
  let%lwt () = Lwt_unix.close fd in
  Lwt.return (oldpath, path)

(* Feed a process stdin with content, and check status *)
let feed name (proc_out:Lwt_process.process_full) content =
  (* Feed content into tar process stdin *)
  let%lwt () = Lwt_io.write_chars proc_out#stdin (Lwt_stream.of_string content) in
  let%lwt () = Lwt_io.close proc_out#stdin in
  
  (* Check status *)
  match%lwt proc_out#status with
  | Unix.WEXITED 0 -> Lwt.return_unit
  | Unix.WEXITED n -> Lwt.fail_with (Printf.sprintf "%s: tar exit status = %d." name n)
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> Lwt.fail_with (name ^ ": tar process was unexpectedly stopped.")

let raw_write_tgz content oldpath path =
  let command = Printf.sprintf "cd \"%s\" && /bin/tar jx" path in
  let put_data proc_out = feed "Lwtarchive.write_tgz" proc_out content in

  let%lwt () = Lwt_process.with_process_full (Lwt_process.shell command) put_data in
  Lwt.return (oldpath, path)

let raw_write_dir perm oldpath path =
  let%lwt () = Lwtfile.mkdir path perm in
  Lwt.return (oldpath, path)

let reparent ~root p =
  try FilePath.reparent "/" root p
  with e ->
    Printf.printf "Reparent error : root = %s, p = %s\n%!" root p ;
    raise e

let write_file ~root arch path ?(new_path=reparent ~root path) ?map_content dperms () =
  let%lwt (desc, ct) = Lwt.wrap2 List.find (fun (desc, _) -> desc.path = path) arch in

  (* Ensure the directory exists *)
  let%lwt () = Lwtfile.mkdir ~parents:true (FilePath.dirname new_path) dperms in

  match desc.ftyp with
  | File -> raw_write ?map_content ct desc.perm desc.path new_path
  | Tgz -> raw_write_tgz ct desc.path new_path
  | Dir -> raw_write_dir desc.perm desc.path new_path

type overw_action = Overwrite | Skip | Backup of path option

let some x = Some x

let rec find_free path num =
  let npath = if num = 0 then path else path ^ string_of_int num in
  if%lwt Lwtfile.exists npath then find_free path (num+1)
  else Lwt.return npath

let check_overwrite dry_run overf path arch_path kont =

  let kont () = if dry_run then Lwt.return_some (arch_path, path) else Lwt.map some (kont arch_path path) in
  
  let%lwt ex = Lwtfile.exists path in
  
  if ex && arch_path <> "/" then
    let%lwt isdir = Lwtfile.is_dir path in
    match%lwt overf ~isdir ~abs:path ~arch:arch_path with
    | Overwrite -> kont ()
    | Skip -> Lwt.return_none
    | Backup bck ->
      let%lwt backup_path = match bck with
        | Some b -> Lwt.return b
        | None -> find_free (path ^ ".backup") 0
      in

      let command = Printf.sprintf "/bin/mv \"%s\" \"%s\"" path backup_path in

      let%lwt () = match%lwt Lwt_unix.system command with
        | Unix.WEXITED 0 -> Lwt.return_unit
        | Unix.WEXITED n -> Lwt.fail_with (Printf.sprintf "Lwtarchive.backup: mv command exits status = %d." n)
        | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> Lwt.fail_with "Lwtarchive.backup: mv command was unexpectedly stopped."
      in
      kont ()
        
  else kont ()

let default_overwrite ~isdir:_ ~abs:_ ~arch:_ = Lwt.return Overwrite

let write_all ~root ?(dry_run=false) arch ?(overwrite=default_overwrite) ?(new_paths=reparent ~root) ?map_content dperms () =  
  Lwt_list.filter_map_s
    begin fun (desc, content) ->
      let%lwt new_path = Lwt.wrap1 new_paths desc.path in
      match desc.ftyp with
      | File ->
        (* Ensure the directory exists *)
        check_overwrite dry_run overwrite new_path desc.path
          (fun oldpath newpath ->
             let%lwt () = Lwtfile.mkdir ~parents:true (FilePath.dirname new_path) dperms in
             raw_write ?map_content content desc.perm oldpath newpath)
          
      | Tgz ->
        (* Ensure the directory exists *)
        check_overwrite dry_run overwrite new_path desc.path
          (fun oldpath newpath ->
             let%lwt () = Lwtfile.mkdir ~parents:true newpath dperms in
             raw_write_tgz content oldpath newpath)
          
      | Dir ->
        check_overwrite dry_run overwrite new_path desc.path
          (fun oldpath newpath ->
             let%lwt () = Lwtfile.mkdir ~parents:true (FilePath.dirname newpath) dperms in
             raw_write_dir desc.perm oldpath newpath)
    end
    
    (List.rev arch)

let digest_entry (desc, content) =
  let open Digest in
  let all = (string content) ^ (string_of_int desc.perm ^ desc.path) in
  string all
  
let get_sign arch =
  Digest.to_hex (List.fold_left (fun s entry -> Digest.string (s ^ digest_entry entry)) "" arch)


let tgz_content tgz = tgz

let tgz_files arch path =
  let%lwt (desc, content) =
    try Lwt.return (List.find (fun (desc, _content) -> desc.path = path) arch)
    with Not_found -> Lwt.fail_with ("Lwtarchive: " ^ path ^ " not in archive.") 
  in
  match desc.ftyp with
  | File -> Lwt.return_nil
  | Dir -> Lwt.return_nil
  | Tgz ->

    (* Tgz file list *)
    let command = "/bin/tar jt" in
    let go proc_out =
      let outstream = Lwt_io.read_lines proc_out#stdout in
      let result = Lwt_stream.to_list outstream in
      let%lwt () = feed "Lwtarchive.tgz_files" proc_out content in
      result
    in
    
    Lwt_process.with_process_full (Lwt_process.shell command) go      

let dummy = append_content empty ~content:"dummy" ~path_in_arch:"/dummy" ~perms:0 ()
    
