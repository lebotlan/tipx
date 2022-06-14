open Common.Small
       
(* Returns the scanned value + the remaining of the input string. *)
type 'a t = string -> ('a * string) option

let scan (sc:'a t) (line:string) =
  match sc line with
  | None -> None
  | Some (v, _) -> Some v

let fmt_scan fmt f = fun line ->
  (* The following line was not easy to find. *)
  try Scanf.sscanf line "%r%[^\004]" (fun buf -> Scanf.bscanf buf fmt f) (fun fres rest -> Some (fres, rest))
  with Scanf.Scan_failure _ | End_of_file -> None

let fm1_scan fmt = fmt_scan fmt id

let int_scan = fm1_scan "%d"
let string_scan = fm1_scan "%s"
let bool_scan = fm1_scan "%b"
let spaces_scan = fmt_scan " " ()

let yes_scan l = Some (l, "")

let union scans line = Common.mapfind (fun sc -> sc line) scans
			   
let kw_scan kw v =
  let lenkw = String.length kw in
  fun s -> 
  if Common.starts_with kw s then
    let ls = String.length s in
    Some (v, String.sub s lenkw (ls - lenkw))
  else None

let map sc f s = Common.option_map (sc s) (fun (v, rest) -> (f v, rest))
			 
let seq sc1 sc2 line =
  match sc1 line with
  | None -> None
  | Some (v1, rest) ->
     begin match sc2 rest with
	   | None -> None
	   | Some (v2, final) -> Some ((v1, v2), final)
     end

let seq3 sc1 sc2 sc3 = map (seq (seq sc1 sc2) sc3) (fun ((a, b), c) -> (a, b, c))


(* Indeed, it is nice. *)
let fmt_seq fmt sc f = map (seq (fmt_scan fmt f) sc) (fun (f1, v1) -> f1 v1)
       
let trim sc = map (seq3 spaces_scan sc spaces_scan) (fun ((), v, ()) -> v)
       
let pair ~sep sc1 sc2 = map (seq3 sc1 (trim (kw_scan sep ())) sc2) (fun (a,(),b) -> (a,b))

(* Reads elements as long as the separator is found. *)			    
let list ?(min_length=0) ~split sc =

  let scan_sep = trim (kw_scan split ()) in
  
  let rec aux acu s =
    (* Try to scan an element *)
    match sc s with
    | None -> if acu = [] then Some ([], s) (* First element not found => empty list *)
	      else None (* Cannot fail after a separator. *)
    | Some (el, rest) ->
       let acu' = el :: acu in
       (* Try to scan the separator *)
       begin match scan_sep rest with
	     | None -> Some (List.rev acu', rest) (* No separator, this is the end of the list *)
	     | Some ((), rest') -> aux acu' rest'
       end
  in

  fun s -> match aux [] s with
	   | None -> None
	   | Some (result, rest) ->
	      if List.length result >= min_length then Some (result, rest)
	      else None
  
let mk_full sc = fun line ->
  match sc line with
  | None -> None
  | Some (v, rest) ->
     if rest = "" then Some (v, "")
     else None

