(*
 LEFT                  RIGHT
   l_in <------------- r_out <-- SEND 'a
   l_out ------------> r_in  --> ANSWER 'b
*)

type ('a, 'b) ss =
  { mutable count: int ; (* Message identifier *)
    l_in:  (int * 'a) Pipe.rs ;
    l_out: (int * 'b) Pipe.ws ;
    r_out: (int * 'a) Pipe.ws ;
    r_in:  (int * 'b) Pipe.rs }

(* Wrap the loop functions with the id extra value. *)
let wrapid loop =
  fun ~name ll (id, v) ->
    let ll = List.map snd ll in
    Common.option_map (loop ~name ll v) (fun w -> (id, w))

let make ~name ?loopa ?loopb () =
  let loopa = Common.option_map loopa wrapid
  and loopb = Common.option_map loopb wrapid in

  let p1 = Pipe.make ~name:(name ^ ".1") ?loop:loopa ()
  and p2 = Pipe.make ~name:(name ^ ".2") ?loop:loopb () in

  { count = 0 ;
    l_in = p1.rs ;
    l_out = p2.ws ;
    r_out = p1.ws ;
    r_in = p2.rs }

let send ss v =
  let id = ss.count in
  ss.count <- ss.count + 1 ;
  (* Wait for the answer before sending the message, to be sure the answer is not lost. *)
  let ans = Pipe.wait_on ~pr:(fun (n, _) -> n = id) ss.r_in in
  let%lwt () = Pipe.lwtsend ss.r_out (id, v) in
  let%lwt (_, w) = ans in
  Lwt.return w

let to_cons ?last ss f =
  Pipe.to_cons ?last ss.l_in
    (fun (id, v) ->
       Lwtplus.later
         (fun () ->
            let%lwt ans = f v in
            Pipe.lwtsend ss.l_out (id, ans)))

let wait_on ss =
  let%lwt (n, v) = Pipe.wait_on ss.l_in in
  let reply x = Lwtplus.later (fun () -> Lwt.return (Pipe.send ss.l_out (n,x))) in
  Lwt.return (v, reply)
