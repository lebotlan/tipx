module HString =
struct
  type t = string
  let equal = Stdlib.(=)
  let hash = Hashtbl.hash
end


module Make =
  functor (V : Hashtbl.HashedType) ->

  struct

    type v = V.t
    
    type sh = v

    module H = Hashtbl.Make (V)
		
    let table = H.create 200

    let idcount = ref 0

    let insertions = ref 0

    let stats ~unitname = Printf.sprintf "%d %s (%d really stored)" !insertions unitname !idcount

    let size () = !idcount

    let tov x = x

    let compare s1 s2 = if s1 == s2 then 0 else Stdlib.compare s1 s2

    let share s =
      incr insertions ;
      if H.mem table s then fst (H.find table s)
      else 
	begin
	  H.add table s (s, !idcount) ;
	  incr idcount ;
	  s
	end

    let id s =
      try snd (H.find table s)
      with Not_found -> assert false (* s has necessarily been recorded *)

    let share_id s = id (share s)

    let share_v s = share s

    let iter f = H.iter (fun _ (s, _) -> f s s) table

    let iteri f = H.iter (fun _ (s, i) -> f i s s) table

    let sh_equals a b =
      if a == b then true
      else
	begin
	  assert (not (V.equal a b)) ;
	  false
	end

    let rec list_equals l1 l2 =
      match l1, l2 with
      | [], [] -> true
      | [], _ | _, [] -> false
      | x1 :: o1, x2 :: o2 -> (sh_equals x1 x2) && list_equals o1 o2

    let list_equals_v = list_equals
							       
  end

