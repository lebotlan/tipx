open Net

let pl2s pl =
  pl.pl_name

let tr2s net tr =

  let rec arc2s net = function
  | [] -> ""
  | (w,pl_id) :: rest -> if w = 1 then (Net.get_pl net pl_id).pl_name ^ " " ^ (arc2s net rest) else ""
in 
tr.tr_name ^ (arc2s net tr.tr_pre) ^ "->" ^ (arc2s net tr.tr_post)  


let net2s net =
  Common.sep (fun x -> pl2s x) "\n" (Array.to_list (Net.all_pl net))
  ^
  Common.sep (fun x -> tr2s net x) "\n" (Array.to_list (Net.all_tr net))