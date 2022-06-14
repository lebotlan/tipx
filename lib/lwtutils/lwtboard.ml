(*

ré-écrire, sans utiliser lwt_sequence.

Parcourir toute la liste à chaque fois. Ce n'est pas si coûteux.

type 'a elt =
  { (* A value on the board. *)
    value: 'a ;

    (* Wakes the thread which has put this elt. *)
    awake: (unit -> unit) ;

    (* Indicate if the elt is still present on the board (used to avoid race-conditions). *)
    mutable present: bool }

type 'a t =
  { mutable seq: 'a elt list ;

    (* Condition used to notify that a new node has been inserted. *)
    cond: 'a elt Lwt_condition.t }

let create () =
  { seq  = [] ;
    cond = Lwt_condition.create () }

let send board v =
  let (hold, awake) = Lwtplus.fwait () in
  let elt =
    { value = v ;
      awake ;
      present = true }
  in
  board.seq <- elt :: board.seq ;
  let%lwt () = Lwt.wrap2 Lwt_condition.broadcast board.cond elt in
  hold

(* Function used with fold on the sequence. flag is the accumulator : None | Some value-found
 * Indicates if the sequence contains a suitable element, satisfying p. *)
let suitable p elt flag =
  match flag with
  | Some _ -> flag
  | None ->
    if elt.present && p elt.value then
      begin
        (* Found ! *)
        elt.present <- false ;
        Lwt_sequence.remove node ;
        elt.awake () ;
        Some elt.value
      end
    else None

let waitfor board p =

  (* Check if a suitable elt is already in the sequence. *)
  let%lwt found = Lwt.return (Lwtplus.fold_node_r (suitable p) board.seq None) in

  (* If not, wait for the condition. *)
  match found with
  | Some v -> Lwt.return v
  | None ->
    let rec loop () =
      let%lwt node = Lwt_condition.wait board.cond in
      begin match%lwt Lwt.wrap3 suitable p node None with
        | None -> loop ()
        | Some v -> Lwt.return v
      end
    in
    loop ()

let waitforvalue board v =
  let%lwt x = waitfor board (fun y -> y = v) in
  let%lwt () = assert%lwt (x = v) in
  Lwt.return_unit

let waitformap board f =
  (* Memorize the function result *)
  let result = ref None in

  let%lwt elt = waitfor board (fun x -> result := f x ; !result <> None) in

  match !result with
  | None -> assert%lwt false
  | Some y -> Lwt.return y

*)
