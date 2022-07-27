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

type _explored_tr =
  { buffer: Intarray.t ;
    mutable count: int }

(*
let new_explored_tr net =
  { buffer = Intarray.(create M32 (nb_tr net)) ;
    count = 0 }

let max_count = 1 lsl 30

let incr expl =
  expl.count <- expl.count + 1 ;
  if expl.count > max_count then
    begin
      expl.count <- 1 ;
      Intarray.clear expl.buffer
    end
                            
let is_explored  expl tr_id = Intarray.get expl.buffer tr_id = expl.count
                                                                 
let set_explored expl tr_id =
  let res = Intarray.set expl.buffer tr_id expl.count in
  assert (res == expl.buffer) ;
  ()
  *)

(* Check for each tr in the list if it is now fireable. Update ts accordingly. *)
let rec check_tr_now_fireable net (* expl *) ts m = function
  | [] -> ()
  | tr_id :: rest ->
    (* We must not set_explored tr_id unconditionally, because tr_id may still be checked later (with "now_disabled") in case of a connected place with negative delta. *)
    (* if (not (Trset.contains ts tr_id)) && (not (is_explored expl tr_id)) && is_fireable (get_tr net tr_id) m then (set_explored expl tr_id ; Trset.add ts tr_id) ; *)

    if (not (Trset.contains ts tr_id)) && is_fireable (get_tr net tr_id) m then Trset.add ts tr_id ;
    
    check_tr_now_fireable net (* expl *) ts m rest

let rec check_tr_now_disabled net (* expl *) ts m = function
  | [] -> ()
  | tr_id :: rest ->
    (* See above *)
    (* if Trset.contains ts tr_id && (not (is_explored expl tr_id)) && not (is_fireable (get_tr net tr_id) m) then (set_explored expl tr_id ; Trset.remove ts tr_id) ; *)

    if Trset.contains ts tr_id && not (is_fireable (get_tr net tr_id) m) then Trset.remove ts tr_id ;
    
    check_tr_now_disabled net (* expl *) ts m rest

let rec update_aux net (* expl *) m ts = function
  | [] -> ()
  | (w,pl_id) :: rest ->

    let pl = get_pl net pl_id in
    
    if w >= 0 then check_tr_now_fireable net (* expl *) ts m pl.pl_post
    else check_tr_now_disabled net (* expl *) ts m pl.pl_post  ;

    update_aux net (* expl *) m ts rest


let update_fireables net () =
  (* let explored_tr = new_explored_tr net in   *)
  fun m ts tr -> (* incr explored_tr ; *) update_aux net (* explored_tr *) m ts tr.tr_delta


let fireables net m =
  
  let ts = Trset.init net in
  Array.iter (fun tr -> if is_fireable tr m then Trset.add ts tr.tr_id) (Net.all_tr net) ;
  ts


