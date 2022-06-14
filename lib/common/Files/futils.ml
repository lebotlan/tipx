open FileUtil

let get_size file =
  let st = stat ~dereference:true file in
  Int64.to_int (byte_of_size (st.size))

let xunit i u = string_of_int i ^ " " ^ u

let show_size n =
  let tb = n lsr 40
  and gb = n lsr 30
  and mb = n lsr 20
  and kb = n lsr 10 in
  
  if tb > 2 then xunit tb "TB"
  else if gb > 2 then xunit gb "GB"
  else if mb > 2 then xunit mb "MB"
  else if kb > 2 then xunit kb "KB"
  else xunit n "B"

let find_homedir () =
  try Sys.getenv "HOME"
  with _ ->
  try
    let uid = Unix.getuid () in
    let entry = Unix.getpwuid uid in
    entry.Unix.pw_dir
  with _ -> "/tmp"
						     
