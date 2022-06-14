open Lwt

(* let yield () = Lwt_unix.yield () *)
let yield () = Lwt_unix.sleep 0.02

let ignore x =
  let%lwt _ = x in
  Lwt.return_unit

module Infix = struct
  
  let (>>>) a b =
    try%lwt
      let%lwt () = a in
    b ()
    with _ -> b ()

  let (||>>) a b =
    let%lwt _ = a
    and _ = b in
    Lwt.return_unit
end


let lwrap f = try%lwt f () with e -> Lwt.fail e
let lwrap1 f a = try%lwt f a with e -> Lwt.fail e
let lwrap2 f a b = try%lwt f a b with e -> Lwt.fail e

let checksw sw =
  if Lwt_switch.is_on sw then Lwt.return_unit
  else Lwt.fail Lwt_switch.Off


let aasync f arg = Lwt.async (fun () -> f arg)

let later ?delay f =
  Lwt.async
    (fun () -> let%lwt () = match delay with
         | None -> yield ()
         | Some d -> Lwt_unix.sleep d
       in f ())

let alater ?delay f arg = later ?delay (fun () -> f arg)

let llater f = (later f ; Lwt.return_unit)

type ('a, 'b) choice = No | Left of 'a | Right of 'b

let list_pmps l f =
  let rec loop ((lacu,racu) as acu) = function
    | [] -> Lwt.return acu
    | x :: rest ->
      let%lwt acu' =
        match%lwt f x with
        | No -> Lwt.return acu
        | Left a -> Lwt.return (a :: lacu,racu)
        | Right b -> Lwt.return (lacu, b :: racu)
      in
      loop acu' rest
  in
  loop ([],[]) l

let list_pmpp l f =

  let%lwt l2 = Lwt_list.rev_map_p f l in
  
  let rec loop ((lacu,racu) as acu) = function
    | [] -> acu
    | x :: rest ->
      let acu' =
        match x with
        | No -> acu
        | Left a -> (a :: lacu, racu)
        | Right b -> (lacu, b :: racu)
      in
      loop acu' rest
  in

  Lwt.return (loop ([],[]) l2)


let join2 a b =
  let%lwt va = a
  and vb = b
  in
  return (va, vb)

let choose2 a b = choose [ a ; b ]  

let check_failed a =
  match Lwt.state a with
  | Return e -> Lwt.fail e
  | Sleep -> Lwt.return_unit
  | Fail e -> Lwt.fail e

let myfail fmt = Printf.ksprintf (fun s -> Lwt.fail_with s) fmt

let fwait () =
  let (x, w) = wait () in
  (x, wakeup w)

let blocked () = fst (wait ())

let exn_guard a b =
  let na =
    let%lwt _ = a in
    blocked ()
  in
  choose2 na b
  
let array_map ar f map =
  let l = Array.to_list ar in
  let%lwt l = map f l in
  Lwt.return (Array.of_list l)

let array_map_s ar f = array_map ar f Lwt_list.map_s
let array_map_p ar f = array_map ar f Lwt_list.map_p

let array_fold_s ar acu f =
  let len = Array.length ar in

  let rec loop i acu =
    if i >= len then Lwt.return acu
    else
      let%lwt acu = f ar.(i) acu in
      loop (i+1) acu
  in

  loop 0 acu

let array_iter_s ar f = array_fold_s ar () (fun x () -> f x)

let paused f =
  let wait, resume = fwait () in
  let t =
    let%lwt () = wait in
    f ()
  in
  (t, (fun () -> if Lwt.is_sleeping wait then resume () else ()))

let ipaused f =
  let (t, r) = paused f in
  (* Catch exceptions *)
  let () = Lwt.async (fun () -> t) in
  r

let with_olock omut f = match omut with
  | None -> f ()
  | Some m -> Lwt_mutex.with_lock m f
                

