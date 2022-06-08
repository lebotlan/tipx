open Petrinet

open Net

module MSet = Set.Make(Marking)
(* module MSet = Set.Make (struct type t = Marking.t let cmp = Marking.cmp end) *)


(* acu :  list of markings to explore next
 * tr_acu : trset of fireable transitions at the current marking *)
let rec fire_all_fireables net seen acu trset m = function
  | [] ->
    let trset2 = Stepper.fireables net m in
    assert (Trset.equal trset trset2) ;
    acu
    
  | tr :: rest ->
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

      fire_all_fireables net seen acu trset m rest
      
      
    else fire_all_fireables net seen acu trset m rest

let explore net init_marking =

  let rec loop seen = function
    | [] -> seen
    | m :: rest ->
      if MSet.mem m seen then loop seen rest
      else
        let seen = MSet.add m seen in
        let all = fire_all_fireables net seen rest (Trset.init net) m (all_tr net) in          
        loop seen all
  in

  let state_space = loop MSet.empty init_marking in

  Printf.printf "\n\n Net %s has %d states.\n\n%!" (get_name net) (MSet.cardinal state_space) ;
  ()


