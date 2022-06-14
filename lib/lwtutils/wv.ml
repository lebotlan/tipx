
type wv = WV : 'a Weak.t -> wv
  
let weak_ar x =
  let ar = Weak.create 1 in
  Weak.set ar 0 (Some x) ;
  ar
  
let weak x = WV (weak_ar x)

let wv_gen_finalise (WV ar) proceed =
  match Weak.get ar 0 with
  (* Note: no race condition here, that is, if Weak.get returns a value, it cannot disappear until our match is finished. *)      
  | None -> proceed () (* Already garbage-collected. *)
  | Some v ->
    (* finalise_last seems more efficient than finalise. It avoids one extra free ride of v. *)
    Gc.finalise_last proceed v

let wait_wv = function
  | None -> Lwtplus.blocked ()
  | Some wv ->
    let (b, wake) = Lwtplus.fwait () in
    wv_gen_finalise wv wake ;
    b

let display = function
  | None -> ()
  | Some m -> Printf.printf "%s\n%!" m

let wv_finalise ?msg wv f arg =
  match wv with
  | None -> ()
  | Some wv ->
    (* Beware, we must not keep the args in the finalization closure. *)
    let w_arg = weak_ar arg in
    
    let proceed () =
      (* Warning: do not use get_copy here, otherwise we will apply the function with fake copied arguments instead of the original ones. *)
      match Weak.get w_arg 0 with
      | Some arg ->
        display msg ;
        f arg
      | _ -> ()
    in
    
    wv_gen_finalise wv proceed

let wv_finalise2 ?msg wv f arg1 arg2 =
  match wv with
  | None -> ()
  | Some wv ->
    (* Beware, we must not keep the args in the finalization closure. *)
    let w_arg1 = weak_ar arg1 
    and w_arg2 = weak_ar arg2 in
    
    let proceed () =
      (* Warning: do not use get_copy here, otherwise we will apply the function with fake copied arguments instead of the original ones. *)
      match Weak.get w_arg1 0, Weak.get w_arg2 0 with
      | Some arg1, Some arg2 ->
        display msg ;
        f arg1 arg2
      | _ -> ()
    in
    
    wv_gen_finalise wv proceed

