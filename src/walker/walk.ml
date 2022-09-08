open Petrinet
open Logic

type state =
  { marking: Marking.t ;
    steps: int ;
    seed: int }

type walk_result =
  | Bingo of state
  | Deadlock of state
  | Timeout of state
  | Maxstep of state
      
let state2s st = string_of_int st.steps ^ " steps. Marking = " ^ Marking.tos st.marking

let result2s = function
  | Bingo m -> "Bingo : " ^ state2s m
  | Deadlock m -> "Deadlock : " ^ state2s m
  | Timeout m -> "Timeout : " ^ state2s m
  | Maxstep m -> "Maxstep : " ^ state2s m

let result2verdict res goal = 
  match res with 
  | Bingo _ -> string_of_bool (Eval.verdict goal true)
  | _ -> "unknown"

type stat =
  { steps: int ;
    time: float ;
    rate: int }

let stat2s s = Printf.sprintf "%d states, rate = %d steps/s" s.steps s.rate

let foi = float_of_int

let time = Unix.gettimeofday 

let sprinter ?(seed=0) ?timeout ?(max_steps=0) ?(stats=Pipe.null ()) net init_marking p =

  (* Yes, it happens. *)
  if Net.nb_tr net = 0 then

    if p init_marking then Bingo { marking = init_marking ; steps = 0 ; seed }
    else Deadlock { marking = init_marking ; steps = 0 ; seed }

  else
    begin      
      let start = time () in

      let last_date = ref start
      and last_steps = ref 0 in

      let send_stats steps =

        let now = time () in
        let time = now -. start in

        if now > !last_date then
          begin
            let rate = int_of_float (0.5 +. (foi (steps - !last_steps)) /. (now -. !last_date)) in

            (* let () = if (steps / 4000000) <> (!last_steps / 4000000) then  Gc.print_stat stdout in *)

            last_date := now ;
            last_steps := steps ;

            Pipe.send stats { steps ; time ; rate }
          end
      in

      let check_timeout =
        match timeout with
        | None -> fun msteps -> send_stats (max_steps + 1 - msteps) ; true
        | Some t ->
          let t = float_of_int t in
          fun msteps -> send_stats (max_steps + 1 - msteps) ; (time () -. start) < t
      in

      (*  let nb_trans = Net.nb_tr net in *)

      let fireables = Stepper.fireables net init_marking
      and upd = Stepper.update_fireables net () in

      (* msteps = max_steps + 1 - steps *)
      
      let rec loop seed msteps marking =
        if p marking then Bingo { marking ; steps = max_steps + 1 - msteps ; seed }
        else
          let msteps = msteps - 1 in
          if msteps = 0 then Maxstep { marking ; steps = max_steps + 1 - msteps ; seed }
          else                            
            let tr = Trset.pick net fireables ~start:seed in
            if tr == Net.null_tr then Deadlock { marking ; steps = max_steps - msteps ; seed }
            else
              let marking = Stepper.quick_fire marking tr in
              let () = upd marking fireables tr in
              
              (* Update seed *)
              let seed = abs (seed * seed - 13 * seed) in  (* TODO : use linear function *)
              
              if msteps land 0xfffff <> 0 || check_timeout msteps then loop seed (msteps-1) marking else Timeout { marking ; steps = max_steps + 1 - msteps ; seed }
      in

      (* let () = Gc.print_stat stdout in *)

      loop seed (max_steps + 1) (Marking.clone init_marking)
    end
    

let stat_stdout = Pipe.new_cb (fun st -> Printf.printf " ⏲  %s\n%!" (stat2s st))


(* TODO : how to check that our walker takes DIFFERENT paths => record visited markings. *)

(* TODO:  lwt-ize p => perf ? *)
    
