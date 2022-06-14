(* Logging module *)
module Log = Mylog.MkSection (struct let section_name = "test_archive.ml" end)
(* open Log *)

open Lwtarchive

let test_share = false

module SH = Shared_vals.Make(Shared_vals.HString)

let share s = SH.(id (share s))

let get i =
  let found = ref None in
  SH.iteri (fun id v _ -> if i = id then found := Some v) ;
  match !found with
  | None -> Lwt.fail_with ("Unknown shared identifier " ^ string_of_int i)
  | Some v -> Lwt.return v

let run () =

  let%lwt arch =
    Lwt_list.fold_left_s (fun arch (p1,p2) -> append_file arch ~source:p1 ~path_in_arch:p2 ()) empty
      [ ("lwtarchive.mli", "/Arch/lwtarchive.mli") ;
        ("lwtarchive.ml", "/Arch/SubDir1/Subdir2/lwtarchive.ml") ;
      ]
  in

  let%lwt tgz = make_tgz ~dir:"Common/" ~sources:["*.mli" ; "Sets/*.mli" ; "Sets/*.ml"] () in
  let%lwt arch = add_tgz arch tgz ~path_in_arch:"/Arch/TAR/GZ/" () in

  let%lwt buf =
    if test_share then Lwt.wrap1 (dump_share ~share) arch
    else Lwt.wrap1 dump arch
  in
  let%lwt () = Lwt_io.printf "Archive dumped. %d bytes.\n%!" (String.length buf) in
  
  let%lwt arch =
    if test_share then arch_of_shared ~get buf
    else arch_of_string buf
  in
  let cont = content arch in

  List.iter (fun cnt -> Printf.printf "File %s  %d bytes  perm = o%o\n%!" cnt.path cnt.size cnt.perm) cont ;

  ignore(Sys.command "rm -rf /tmp/test_archive_dir") ;
  
  let%lwt _ = write_all ~root:"/tmp/test_archive_dir" arch 0o700 () in  
  Lwt.return_unit

open Lwtlaunch.Setkeys

let () =
  noconfig ===> Lwtlaunch.launch ~appname:"test_archive" ~run ()
    
    
