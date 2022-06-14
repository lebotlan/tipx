open Lwt_preemptive

let () = simple_init ()

let launch ?mutex f arg =
  match mutex with 
  | None -> Lwt_preemptive.detach f arg
  | Some m -> Lwt_mutex.with_lock m (fun () -> Lwt_preemptive.detach f arg)

			
