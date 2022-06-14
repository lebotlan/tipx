open Common.Small
open Wv
    
type 'a t =
  { mutable terminated: bool ;
    mutable return: 'a ;

    (* Next delay to sleep once the current delay is over. *)
    mutable next_delay: float option ;
  }

let new_delay return dur =
  { terminated = false ;
    return ;
    next_delay = Some dur }

let is_terminated d = d.terminated

let mod_value d v =
  if is_terminated d then failwith "Lwtdelay.mod_value: delay is terminated. " ;
  d.return <- v

let increase_delay ?return delay delta =
  if delta < 0.0 then failwith "Lwtdelay.increase_delay: delay is must be positive." ;
  if is_terminated delay then failwith "Lwtdelay.increase_delay: delay is terminated. " ;
  Common.option_iter return (mod_value delay) ;
  delay.next_delay <- Some (match delay.next_delay with None -> delta | Some x -> x +. delta) ;
  ()

let sleep delay =
  let%lwt () =
    if is_terminated delay then Lwt.fail_with "Lwtdelay.sleep_delay: delay is terminated. "
    else Lwt.return_unit
  in

  let rec loop () = match delay.next_delay with
    | None -> Lwt.return_unit
    | Some d ->
      delay.next_delay <- None ;
      let%lwt () = Lwt_unix.sleep d in
      loop ()
  in
  let%lwt () = loop () in
  delay.terminated <- true ;
  Lwt.return delay.return

type control = [ `Pause | `Run | `Stop ]

let control2s = function
  | `Pause -> "Pause"
  | `Run -> "Run"
  | `Stop -> "Stop"

let ctl2switch ctlo =
  let sw = Lwt_switch.create () in
  Pipe.oto_cons ctlo (function `Stop -> Lwtplus.later (fun () -> Lwt_switch.turn_off sw) | _ -> ()) ;
  sw

let periodic ?switch ?(init=`Run) ?ctl ~period f =

  let state = ref init in

  (* Indicate if a thread is already launched *)
  let is_launched = ref false in

  let rec launch () =
    if !is_launched then ()
    else
      begin
        is_launched := true ;
        Lwt.async
          begin fun () ->
            let%lwt () = Lwt_unix.sleep period in
            is_launched := false ;
            let%lwt () = Lwt.wrap1 Lwt_switch.check switch in
            match !state with
            | `Pause -> Lwt.return_unit (* If Paused, we stop the recursion. *)
            | `Run -> launch () ;
              let%lwt _ = f () in Lwt.return_unit
          end
      end
  in
  if init = `Run then launch () ;

  (* Control function *)
  let rec control = function
    | `Pause -> state := `Pause
    | `Run  -> state := `Run ; launch () (* Works also if `Run is called repeatedly *)
    | `Stop -> state := `Pause ; Pipe.oremove_cb ctl control
  in
  Pipe.oto_cons ctl control ;
  control


let ramp ?switch ?ctl pipe ~from ~delta ~timestep ~n () =

  (* We do not optimize flat ramps, so that they can be paused. *)

  let state = ref `Run in

  let rec aux v n =
    let%lwt () = Lwt.wrap1 Lwt_switch.check switch in
    let%lwt () = Lwt.wrap2 Pipe.send pipe v in

    if n <= 0 then Lwt.return_unit
    else
      begin
        (* Sleep & pause if necessary *)
        let%lwt () = Lwt_unix.sleep timestep in
        (* let () = Printf.printf ":%!" in *)
        let%lwt () = match !state with
          | `Pause ctlp -> let%lwt _ = Pipe.wait_on ~pr:(fun c -> c = `Run) ctlp in Lwt.return_unit
          | _ -> Lwt.return_unit
        in

        if !state = `Stop then Lwt.return_unit
        else aux (v +. delta) (n-1)
      end
  in

  Common.option_iter ctl (fun ctlp -> Pipe.to_cons ctlp (function `Run -> state := `Run
                                                                | `Stop -> state := `Stop
                                                                | `Pause -> state := `Pause ctlp)) ;
  aux from n

let ramp_to ?switch ?ctl pipe ~from ~tov ~timestep ~total_delay () =
  let n = int_of_float (total_delay /. timestep) in
  let delta = (tov -. from) /. (float_of_int n) in
  ramp ?switch ?ctl pipe ~from ~delta ~timestep ~n ()

let iramp ?switch ?ctl pipe ~from ~delta ~timestep ~n () =
  ramp ?switch ?ctl (Pipe.invmap pipe niof) ~from:(foi from) ~delta:(foi delta) ~timestep ~n ()

let iramp_to ?switch ?ctl pipe ~from ~tov ~timestep ~total_delay () =
  ramp_to ?switch ?ctl (Pipe.invmap pipe niof) ~from:(foi from) ~tov:(foi tov) ~timestep ~total_delay ()
    
let ramps_seq ?wv ?switch ?ctl ?to_p ?cb ?(finish_cb=fun () -> ()) ~from ~seq ~loop ~timestep () =

  let pipe = match to_p with
    | None -> (Pipe.make ~name:"ramps_seq_pipe" ()).ws
    | Some p -> p
  in

  Common.option_iter cb (fun cb -> Pipe.to_cons (Pipe.getr pipe) cb) ;
  
  let stopped = ref false in
  Pipe.oto_cons ctl (function `Stop -> stopped := true | _ -> ()) ;

  let finish () =
    let stopped = !stopped in
    finish_cb () ;
    if stopped then Lwt.return_unit
    else Lwt.return_unit
  in
  
  let quit =
    match wv with
    | None -> (fun () -> !stopped)
    | Some (WV ar) ->
      (fun () -> !stopped || not (Weak.check ar 0))
  in
  
  if seq = [] then Lwt.return_unit
  else
    (* Note: a ctl event can only occur inside a ramp (there is no interruption point outside of ramps). *)
    
    let rec aux current = function
      | [] -> if loop then aux current seq else finish ()
      | (target, time) :: rest ->
        let n = max 1 (niof (time /. timestep)) in
        let delta = (target -. current) /. (foi n) in

        (* Abort recursion? *)
        if quit () then finish ()
        else
          let%lwt _ = ramp ?switch ?ctl pipe ~from:current ~delta ~timestep ~n () in
          aux target rest
    in
    aux from seq

let iramps_seq ?wv ?switch ?ctl ?to_p ?cb ?finish_cb ~from ~seq ~loop ~timestep () =
  let to_p = Common.option_map to_p (fun p -> Pipe.invmap p niof)
  and cb = Common.option_map cb (fun cb -> fun r -> cb (niof r)) in
  
  ramps_seq ?wv ?switch ?ctl ?to_p ?cb ?finish_cb ~from:(foi from) ~seq:(List.map (fun (a,b) -> (foi a,b)) seq) ~loop ~timestep ()
