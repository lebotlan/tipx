open Pipe

let sample ?wv ?target ?(prio=fun _ -> 0) ?(only_change=false) ~period ~init pipea =
  let (pipeb, sendb) = new_pipe ~name:(get_name "sample" pipea) target in

  let last_value = ref init
  and last_prio = ref min_int in
  
  let update_last v = if prio v >= !last_prio then (last_value := v ; last_prio := prio v)
  and vsend () =
    last_prio := min_int ;
    Lwt.wrap1 sendb !last_value
  in
  
  let () =
    if only_change then
      begin
        let changed = ref false
        and ctl = Pipe.make ~name:"sample-ctl" () in

        to_cons ?wv pipea (fun v -> changed := true ; update_last v ; Pipe.send ctl.ws `Run) ;
        
        let f () =
          if !changed then
            begin
              changed := false ;
              Pipe.send ctl.ws `Pause ;
              vsend ()
            end
          else
            Lwt.return_unit
        in
        
        let _ctl = Lwtdelay.periodic ~init:`Pause ~ctl:ctl.rs ~period f in
        ()
      end

    else
      (* Inconditional periodic sampling *)
      let _ctl = Lwtdelay.periodic ~period vsend in
      to_cons ?wv pipea update_last
  in
  
  pipeb


let inhibit ?wv ?target ~delay pipea =
  let (pipeb, sendb) = new_pipe ~name:(get_name "inhibit" pipea) target in

  let active = ref true in

  let vsend v =
    active := false ;
    Lwt.async (fun () -> Lwt_unix.sleep (delay v) ;%lwt active := true ; Lwt.return_unit) ;
    sendb v
  in
  
  to_cons ?wv pipea (fun v -> if !active then vsend v) ;
  
  pipeb

(*** Time rate limiter ***)

type ('a, 'c) rate_limit =
  { category: 'a -> 'c option ;
    delay: 'c -> float }

let cst_cat x = { category = (fun _ -> Some ()) ; delay = (fun () -> x) }

(* Status of a category: date of last emission, or indicates that a timer is already running.
 * The timer will send the event in the given reference. *)    
type 'a limit_status = LastEmit of float | Timer of 'a ref

type ('a, 'b) cat =
  { get_delay: 'a -> float option ;
    get_status: 'a -> 'b ;
    set_status: 'a -> 'b -> unit }

let alongtimeago = Unix.gettimeofday () -. 20.0 *. 365.0 *. 24.0 *. 3600.0

let create_cat rl default =
  let table = Assoc.create ~size:4 ~init:(fun _ -> default) () in
  { get_delay = (fun evt -> Common.option_map (rl.category evt) rl.delay) ;
    get_status = (fun evt -> Assoc.get table (Common.option_get (rl.category evt))) ;
    set_status = (fun evt v -> Assoc.set table (Common.option_get (rl.category evt)) v) }

let limit ?wv ?target rate_limit pipe =
  let cat = create_cat rate_limit (LastEmit alongtimeago) in

  mk_new_pipe ?wv "limit" ?target pipe
    begin fun sendb x ->
      match cat.get_delay x with
      | None -> sendb x
      | Some d ->
        begin match cat.get_status x with
          | LastEmit last ->
            let now = Unix.gettimeofday () in
            if now -. last >= d then
              (* this event is clear, we send it at once *)
              ( cat.set_status x (LastEmit now) ;
                sendb x )
            else
              begin
                (* Postpone sending event *)
                let evt_r = ref x in
                cat.set_status x (Timer evt_r) ;
                Lwt.async begin fun () ->
                  let%lwt () = Lwt_unix.sleep (d -. (now -. last)) in
                  let%lwt () = Lwt.wrap2 cat.set_status x (LastEmit (Unix.gettimeofday ())) in
                  Lwt.wrap1 sendb !evt_r
                end
              end

          | Timer evt_r ->
            evt_r := x ; (* Update upcoming event *)
        end
    end

type 'a stable_status =
  (* Delay & expected termination time. *)
  | Sleeping of ('a Lwtdelay.t * float)
  | Awake

let stabilized ?wv ?target rate_limit pipe = 
  let cat = create_cat rate_limit Awake in

  mk_new_pipe ?wv "stabilized" ?target pipe
    begin fun sendb x ->
      match cat.get_delay x with
      | None -> sendb x
      | Some d ->
        begin match cat.get_status x with
          | Awake ->
            let delay = Lwtdelay.new_delay x d in
            let endtime = Unix.gettimeofday () +. d in
            cat.set_status x (Sleeping (delay, endtime)) ;
            Lwt.async begin fun () ->
              let%lwt y = Lwtdelay.sleep delay in
              let%lwt () = Lwt.wrap2 cat.set_status x Awake in
              Lwt.wrap1 sendb y
            end

          | Sleeping (delay, expected_endtime) ->
            let now = Unix.gettimeofday () in
            let new_endtime = now +. d in
            cat.set_status x (Sleeping (delay, new_endtime)) ;
            let delta = new_endtime -. expected_endtime in
            Lwtdelay.increase_delay ~return:x delay delta ;
            ()
        end
    end
