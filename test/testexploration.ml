open Parsers
open Petrinet

open Net

module MSet = Set.Make(Marking)
(* module MSet = Set.Make (struct type t = Marking.t let cmp = Marking.cmp end) *)


(* acu :  list of markings to explore next
 * tr_acu : trset of fireable transitions at the current marking *)
let fire_if_fireable net seen m (acu, trset) tr =
  if Stepper.is_fireable tr m then
    let trset = Trset.clone trset in
    let () = Trset.add trset tr in
      
    let m2 = Stepper.fire m tr in

    let acu = if MSet.mem m2 seen then acu else m2 :: acu in
    
    (* Check update_fireables *)
    let trset0 = Stepper.fireables net m in
    let () = Stepper.update_fireables net m2 trset0 tr in
    let trset1 = Stepper.fireables net m2 in
    let () = assert (Trset.equal trset0 trset1) in
    
    (acu, trset)
           
  else (acu, trset)

let explore net init_marking =

  let rec loop seen = function
    | [] -> seen
    | m :: rest ->
      if MSet.mem m seen then loop seen rest
      else
        let seen = MSet.add m seen in
        let (all,trset) = Array.fold_left (fire_if_fireable net seen m) (rest, Trset.init net) (all_tr net) in
        
        let trset2 = Stepper.fireables net m in
        assert (Trset.equal trset trset2) ;
        
        loop seen all
  in

  let state_space = loop MSet.empty [init_marking] in

  let card = MSet.cardinal state_space in
  Printf.printf "\n\n Net %s has %d state%s.\n\n%!" (get_name net) card (if card > 1 then "s" else "") ;
  ()



let explore_test_net () =

  let m0 = Marking.init test_net in

  let p0 = get_pl test_net 0
  and p1 = get_pl test_net 1 in
  
  let m0 = Marking.add m0 p0 1 in
  let m0 = Marking.add m0 p1 10 in
  
  explore test_net m0

let () = explore_test_net ()
  

let explore_tina_net_file _file =

  let _ = Tina.parser in
  ()



