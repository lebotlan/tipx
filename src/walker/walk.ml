open Petrinet
open Logic

type state =
  { marking: Marking.t ;
    steps: int }

type walk_result =
  | Bingo of state
  | Deadlock of state
  | Timeout of state
      
let state2s st = string_of_int st.steps ^ " steps. Marking = " ^ Marking.tos st.marking

let result2s = function
  | Bingo m -> "Bingo : " ^ state2s m
  | Deadlock m -> "Deadlock : " ^ state2s m
  | Timeout m -> "Timeout : " ^ state2s m

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

let sprinter ?(seed=0) ?timeout ?(stats=Pipe.null ()) net init_marking p =

  let start = Unix.time () in

  let last_date = ref start
  and last_steps = ref 0 in
  
  let send_stats steps =

    let now = Unix.time () in
    let time = now -. start in
    
    if now > !last_date then
      begin
        let rate = int_of_float (0.5 +. (foi (steps - !last_steps)) /. (now -. !last_date)) in
        
        last_date := now ;
        last_steps := steps ;
    
        Pipe.send stats { steps ; time ; rate }
      end
  in
  
  let check_timeout =
    match timeout with
    | None -> fun steps -> send_stats steps ; true
    | Some t ->
      let t = float_of_int t in
      fun steps -> send_stats steps ; (Unix.time () -. start) < t
  in

  (*  let nb_trans = Net.nb_tr net in *)
  
  let fireables = Stepper.fireables net init_marking in

  let rec loop seed steps marking =
    if p marking then Bingo { marking ; steps }
    else
      
      let tr = Trset.pick net fireables ~start:seed in
      if tr == Net.null_tr then Deadlock { marking ; steps }
      else
        let marking = Stepper.quick_fire marking tr in
        let () = Stepper.update_fireables net marking fireables tr in

        (* Update seed *)
        let seed = abs (seed * seed - 13 * seed) in
        
        if steps land 0xfffff <> 0 || check_timeout steps then loop seed (steps+1) marking else Timeout { marking ; steps }
  in

  loop seed 0 (Marking.clone init_marking)
    

let stat_stdout = Pipe.new_cb (fun st -> Printf.printf " ⏲  %s\n%!" (stat2s st))
