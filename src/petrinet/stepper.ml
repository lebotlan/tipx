open Net
open Marking
    
exception Not_fireable of tr * marking

let rec quick_fire_aux m = function
  | [] -> m
  | (w,pl) :: rest ->
    let m2 = Marking.add m pl w in
    quick_fire_aux m2 rest

let quick_fire m tr = quick_fire_aux m tr.tr_delta

let rec is_fireable_aux m = function
  | [] -> true
  | (w,pl) :: rest -> (Marking.get m pl >= w) && is_fireable_aux m rest

let is_fireable tr m = is_fireable_aux m tr.tr_pre

let fire m ?(out=Marking.clone m) tr =
  if not (is_fireable tr m) then raise (Not_fireable (tr,m)) ;
  quick_fire out tr

(* Check for each tr in the list if it is now fireable. Update ts accordingly. *)
let rec check_tr_now_fireable net ts m = function
  | [] -> ()
  | tr_id :: rest ->
    if not (Trset.contains ts tr_id) && is_fireable (get_tr net tr_id) m then Trset.add ts tr_id ;
    check_tr_now_fireable net ts m rest

let rec check_tr_now_disabled net ts m = function
  | [] -> ()
  | tr_id :: rest ->
    if Trset.contains ts tr_id && not (is_fireable (get_tr net tr_id) m) then Trset.remove ts tr_id ;
    check_tr_now_disabled net ts m rest

let rec update_aux net m ts = function
  | [] -> ()
  | (w,pl_id) :: rest ->

    let pl = get_pl net pl_id in
    
    if w >= 0 then check_tr_now_fireable net ts m pl.pl_post
    else check_tr_now_disabled net ts m pl.pl_post  ;

    update_aux net m ts rest
      

let update_fireables net m ts tr = update_aux net m ts tr.tr_delta

let fireables net m =
  
  let ts = Trset.init net in
  Array.iter (fun tr -> if is_fireable tr m then Trset.add ts tr.tr_id) (Net.all_tr net) ;
  ts


