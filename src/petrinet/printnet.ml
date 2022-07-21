open Net

let pl2s pl = "pl " ^ pl.pl_name (* ^ "(" ^ string_of_int pl.pl_id ^ ")" *)

let tr2s net tr =
  
  let arc2s l = Common.sep (fun (w,pl_id) -> (Net.get_pl net pl_id).pl_name ^ (if w = 1 then "" else "*" ^ string_of_int w)) " " l in
  "tr " ^ tr.tr_name ^ " " ^ arc2s tr.tr_pre ^ " -> " ^ arc2s tr.tr_post


let net2s net =
  Common.sep pl2s "\n" (Array.to_list (Net.all_pl net))
  ^ "\n" ^
  Common.sep (tr2s net) "\n" (Array.to_list (Net.all_tr net))
  ^ "\n"
