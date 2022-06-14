open Unix

(* Must be compatible with a file name (no space, no weird character). *)
let to_string date =
  let tm = Unix.localtime date in

  Printf.sprintf "%d-%02d-%02d--%02dH%02dM%02ds"
    (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec


let to_nice_string date =
  let tm = Unix.localtime date in

  Printf.sprintf "%d-%02d-%02d %02dH%02d'%02d"
    (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let to_day_only date =
  let tm = Unix.localtime date in

  Printf.sprintf "%d-%02d-%02d"
    (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday


let time2s tim =
  let open Unix in
  let millis = int_of_float (fst (modf tim) *. 1000.0) in
  let tm = localtime tim in
  Printf.sprintf "%4d-%02d-%02d--%02d.%02d.%02d.%03d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec millis
