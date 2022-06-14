open Wv

exception InsaneLoop of string

type 'a whenloop = (name:string -> 'a list -> 'a -> 'a option)

let ignore_loop ~name:_ _ _ = None

let fixloop ?(eq=(=)) ?limit () = fun ~name l v ->
  match l with
  | [] -> assert false
  | x :: _ -> if eq x v then (Printf.printf "Pipe.fixpoint\n%!" ; None)
    else
      begin match limit with
        | None -> Some v
        | Some lim ->
          if List.length l >= lim then raise (InsaneLoop name)
          else Some v
      end

type 'a raw_pipe =
  { mutable output: ('a -> unit) list ;

    (* Stack of recursive calls being processed now by this pipe. 
     * Empty stack  = this pipe is inactive for now.
     * One elements = this pipe is being sending an event.
     * n elements   = this pipe is calling itself recursively. *)
    mutable insend: 'a list ;

    mutable loop: 'a whenloop ;

    name: string }

type 'a rs = 'a raw_pipe

type 'a ws = 'a raw_pipe

type 'a rw =
  { rs: 'a rs ;
    ws: 'a ws }

let getr x = x

let unnamed = "(unnamed)"

let make ~name ?(loop=fun ~name _ _ -> raise (InsaneLoop name)) () =
  let pipe = { output = [] ; insend = [] ; loop ; name } in
  { rs = pipe ;
    ws = pipe }

let set_loop pipe loop = pipe.loop <- loop

let send pipe v =

  let received =
    match pipe.insend with
    | [] -> Some v
    | olds -> pipe.loop ~name:pipe.name olds v (* No need to catch exceptions here, it will be caught by the enclosing recursive call (below). *)
  in

  match received with
  | None -> () (* Do not propagate any further. *)
  | Some v ->
    let previous_stack = pipe.insend in
    pipe.insend <- v :: previous_stack ;

    (* No 'finally' in ocaml? *)
    try
      List.iter (fun f -> f v) pipe.output ;
      pipe.insend <- previous_stack
    with e ->
      pipe.insend <- previous_stack ;
      raise e

let lwtsend pipe v = Lwt.wrap2 send pipe v

let osend pipe = function
  | None -> ()
  | Some x -> send pipe x

let send_later ?delay pipe v = Lwtplus.later ?delay (fun () -> lwtsend pipe v)

let remove_cb ?(check=false) pipe f =
  assert ( (not check) || List.memq f pipe.output) ;
  pipe.output <- List.filter (fun cb -> cb != f) pipe.output

let oremove_cb ?check opipe f = match opipe with
  | None -> ()
  | Some p -> remove_cb ?check p f

    
let to_cons ?wv ?(last=false) pipe f =
  if last then pipe.output <- pipe.output @ [f]
  else pipe.output <- f :: pipe.output ;
  wv_finalise2 wv (remove_cb ~check:false) pipe f

let oto_cons ?wv ?last opipe f = match opipe with
  | None -> ()
  | Some p -> to_cons ?wv ?last p f

let to_conslwt ?wv ?last pipe f = to_cons ?wv ?last pipe (fun x -> Lwt.async (fun () -> f x))

let to_unit ?wv pipe f = to_cons ?wv pipe (fun _ -> f ())

let to_pipe ?wv pa pb = to_cons ?wv pa (send pb)
let oto_pipe ?wv opa pb = oto_cons ?wv opa (send pb)

let map_to_pipe ?wv pa f pb = to_cons ?wv pa (fun e -> send pb (f e))
let mapfilter_to_pipe ?wv pa f pb = to_cons ?wv pa (fun e -> match f e with None -> () | Some x -> send pb x)


let wait_on ?(pr = fun _ -> true) pipe =
  let (res, w) = Lwt.wait () in

  (* Recursive so that it can remove itself from the pipe. *)
  let rec mycb v =
    if pr v then
      begin
        remove_cb pipe mycb ;
        Lwt.async (fun () -> let%lwt () = Lwtplus.yield () in Lwt.return (Lwt.wakeup w v))
      end
    else ()
  in

  to_cons pipe mycb ;
  res

let new_pipe ~name target =
  let p = match target with
    | None -> (make ~name ()).ws
    | Some p -> p
  in
  (p, send p)

let new_rpipe ~name target =
  match target with
    | None -> make ~name ()
    | Some p -> { rs = p ; ws = p }
      
let flatten ?wv ?target pa =
  let (pb, sendb) = new_pipe ~name:"flatten" target in
  to_cons ?wv pa (fun l -> List.iter sendb l) ;
  pb

let new_cb ?wv cb =
  let p = make ~name:"new_cb" () in
  to_cons ?wv p.ws cb ;
  p.ws

let new_lwtcb ?wv cb = new_cb ?wv (fun x -> Lwtplus.alater cb x)

let stdout = new_cb (fun s -> Printf.printf "%s\n%!" s)
let lwtstdout = new_cb (fun s -> Lwt.async (fun () -> Lwt_io.printf "%s\n" s))

let null () = (make ~name:"null" ()).ws

let get_name nm pa =
  if pa.name == unnamed then nm ^ "-."
  else nm ^ "-" ^ pa.name

let get_name2 nm pa pb = get_name (get_name nm pa) pb  

let join ?wv ?target pipea pipeb =
  let (pipec, sendc) = new_pipe ~name:(get_name2 "join" pipea pipeb) target in
  to_cons ?wv pipea sendc ;
  to_cons ?wv pipeb sendc ;
  pipec

let ojoin ?wv ?target opipea pipeb =
  let (pipec, sendc) = new_pipe ~name:"ojoin" target in
  oto_cons ?wv opipea sendc ;
  to_cons ?wv pipeb sendc ;
  pipec  

(* A pipe created by applying a transformation to a first pipe. *)    
let mk_new_pipe ?wv opname ?target pipea app =
  let (pipeb, sendb) = new_pipe ~name:(get_name opname pipea) target in
  to_cons ?wv pipea (app sendb) ;
  pipeb

let only_change ?wv ?target ?(eq=(=)) pipea =
  let previous = ref None in
  mk_new_pipe ?wv "only_change" ?target pipea
    begin fun sendb x ->
      let hide =
        match !previous with
        | None -> false
        | Some old -> eq old x
      in

      if hide then ()
      else
        begin
          previous := Some x ;
          sendb x
        end
    end

let map ?wv ?target pipe f = mk_new_pipe ?wv "map" ?target pipe (fun sendb x -> sendb (f x))
let mapfilter ?wv ?target pipe f = mk_new_pipe ?wv "mapfilter" ?target pipe (fun sendb x -> match f x with None -> () | Some y -> sendb y)

let async_map ?wv ?target pipe f = mk_new_pipe ?wv "async_map" ?target pipe (fun sendb x -> Lwt.async (fun () -> let%lwt y = f x in Lwt.wrap1 sendb y))
let async_mapfilter ?wv ?target pipe f = mk_new_pipe ?wv "async_mapfilter" ?target pipe
    (fun sendb x -> Lwt.async (fun () -> match%lwt f x with None -> Lwt.return_unit
                                                          | Some y -> Lwt.wrap1 sendb y))

let ujoin ?wv ?target pipea pipeb = join ?wv ?target (map pipea (fun _ -> ())) (map pipeb (fun _ -> ()))

let invmap ?wv pipeb f =
  let p = make ~name:"invmap" () in
  to_cons ?wv p.ws (fun v -> send pipeb (f v)) ;
  p.ws

let invmapfilter ?wv pipeb f =
  let p = make ~name:"invmapfilter" () in
  to_cons ?wv p.ws (fun v -> (match f v with Some vv -> send pipeb vv | None -> ())) ;
  p.ws

let partition ?wv ?targeta ?targetb pipe pred =
  let (pipea, senda) = new_pipe ~name:(get_name "partition-1" pipe) targeta
  and (pipeb, sendb) = new_pipe ~name:(get_name "partition-2" pipe) targetb
  in
  to_cons ?wv pipe (fun evt -> if pred evt then senda evt else sendb evt) ;
  (pipea, pipeb)

let activable ?wv ?is_on ?target pipe =
  let active = match is_on with None -> ref true | Some r -> r in
  let p = mapfilter ?wv ?target pipe (fun x -> if !active then Some x else None) in
  (active, { rs = p ; ws = p } )

let stateful ?wv ?target ?source ~update init =
  let state = ref init in

  let in_pipe = new_rpipe ~name:"stateful-in" source
  and (out_pipe, send) = new_pipe ~name:"stateful-out" target in

  to_cons ?wv in_pipe.rs (fun x -> state := update !state x ; send !state) ;
  (in_pipe.ws, out_pipe)

let toggle ?wv ?target ?source init = stateful ?wv ?target ?source ~update:(fun old _ -> not old) init

let until ?wv ?target pipea pipeb p =
  let (pipec, sendc) = new_pipe ~name:(get_name2 "until" pipea pipeb) target in
  let from_b = ref false in
  to_cons ?wv pipea (fun evt -> if not !from_b then (sendc evt ; from_b := p evt)) ;
  to_cons ?wv pipeb (fun evt -> if !from_b then sendc evt) ;
  pipec


