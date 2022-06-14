type 'a var =
  { mutable value: 'a ;
    mutable listeners: (('a -> bool) * ('a Lwt.u)) list ;
    mutable lock: bool }

let create x =
  { value = x ;
    listeners = [] ;
    lock = false }

let read var = var.value

let register var listen = var.listeners <- listen :: var.listeners
   
let wait_for_predicate var p =
  let%lwt flag = Lwt.wrap1 p var.value in
  if flag then Lwt.return var.value
  else
    begin
      let (thr, wake) = Lwt.wait () in
      register var (p, wake) ;
      thr
    end

let reset var =
  if var.lock then failwith "Lwtvar.set: not supposed to reset a variable within the variable's callback." ;
  List.iter (fun (_, wake) -> Lwt.wakeup_exn wake Lwt.Canceled) var.listeners ;
  var.listeners <- [] ;
  ()

(* Wakeup eligible threads, but asynchronously. We want to wakeup everyone and leave the current function before unblocking other threads. *)
let mywake wake x =
  Lwt.async (fun () ->
      let%lwt () = Lwtplus.yield () in
      Lwt.wakeup wake x ;
      Lwt.return_unit)
    
let set var x =
  if var.lock then failwith "Lwtvar.set: not supposed to set a variable within the variable's callback." ;

  var.lock <- true ;
  var.value <- x ;

  let listeners = var.listeners in

  (* We remove all listeners and insert again those for which the predicate is false.
   * (In order to avoid race conditions : if wakeup happens to execute a concurrent thread, it could try to insert new listeners meanwhile) *)
  var.listeners <- [] ;

  List.iter (fun ((p, wake) as listn) -> if p x then mywake wake x else register var listn) listeners ;
  var.lock <- false ;
  ()
       
