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
let rec check_tr_now_fireable ts m = function
  | [] -> ()
  | tr :: rest ->
    if not (Trset.contains ts tr) && is_fireable tr m then Trset.add ts tr ;
    check_tr_now_fireable ts m rest

let rec check_tr_now_disabled ts m = function
  | [] -> ()
  | tr :: rest ->
    if Trset.contains ts tr && not (is_fireable tr m) then Trset.remove ts tr ;
    check_tr_now_disabled ts m rest

let rec update_aux net m ts = function
  | [] -> ()
  | (w,pl) :: rest ->
    if w >= 0 then check_tr_now_fireable ts m pl.pl_post
    else check_tr_now_disabled ts m pl.pl_post  ;

    update_aux net m ts rest
      

let update_fireables net m ts tr = update_aux net m ts tr.tr_delta

let fireables net m =
  
  let ts = Trset.init net in
  Array.iter (fun tr -> if is_fireable tr m then Trset.add ts tr) (Net.all_tr net) ;
  ts


