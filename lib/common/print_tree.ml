
(* Printing context.
 * Acu : output text.
 * margin: the margin to get back exactly under the current position on the next line. 
 * lastmargin: margin used for the last child of a node. The markers are different. *)
type context =
  { acu: string ;
    margin: string ;
    lastmargin: string }

let init_context =
  { acu        = "   " ;
    margin     = "   " ;
    lastmargin = "   " ;}

(* Appends newlines. *)
let rec nl n ctxt =
  if n <= 0 then ctxt
  else nl (n-1) { ctxt with acu = ctxt.acu ^ "\n" ^ ctxt.margin }

(* Adds some text *)
let add ?count ctxt s =
  let spaces = String.make (match count with None -> Text.length s | Some n -> n) ' ' in
  { acu = ctxt.acu ^ s ;
    margin = ctxt.margin ^ spaces ;
    lastmargin = ctxt.lastmargin ^ spaces }

(* Adds a branch *)
let br ?count ctxt label =
  let ctxt = add ?count ctxt label in

  if count = None then
    { acu = ctxt.acu ^ "_" ;
      margin = ctxt.margin ^ "|" ;
      lastmargin = ctxt.margin ^ " " }
  else
    nl 1 { acu = ctxt.acu ;
           margin = ctxt.margin ^ "|" ;
           lastmargin = ctxt.margin ^ " " }

let sprint_tree ?(childs_below=fun _ -> None) tos get_childs tree =

  (* Input : ready to write the current node at the current context position.
   * Output : the last character of acu is the last character of the current node. *)
  let rec loop ctxt node =

    let childs = get_childs node in
    let ctxt = if childs = [] then add ctxt (tos node) else br ?count:(childs_below node) ctxt (tos node) in
    
    let rec iterchilds ct = function
      | [] ->  ct
      | [ch] -> { acu = (loop (add { ct with margin = ct.lastmargin } "___") ch).acu ;
                  margin = ct.margin ;
                  lastmargin = ct.lastmargin }
                
      | ch :: others ->
        let ct2 = nl 2 { acu = (loop (add ct "___") ch).acu ;
                         margin = ct.margin ;
                         lastmargin = ct.lastmargin}
        in
        iterchilds ct2 others
    in
    iterchilds ctxt childs
      
  in
  (loop init_context tree).acu
