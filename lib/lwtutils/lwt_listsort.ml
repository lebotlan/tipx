
let rec merge ~cmp l1 l2 = match (l1,l2) with
  | list, [] | [], list -> Lwt.return list    
  | (h1 :: t1), h2 :: t2 ->
    let%lwt res = cmp h1 h2 in
    
    if res < 0 then
      let%lwt tail = merge ~cmp t1 l2 in
      Lwt.return (h1 :: tail)
    else
      let%lwt tail = merge ~cmp l1 t2 in
      Lwt.return (h2 :: tail)

let rec halve = function
  | [] | [_] as t1 -> t1, []
  | h :: t ->
    let t1, t2 = halve t in
    h :: t2, t1

let rec sort ~cmp = function
  | [] | [_] as list -> Lwt.return list
  | list ->
    let l1, l2 = halve list in

    let%lwt l1 = sort ~cmp l1
    and l2 = sort ~cmp l2 in
    
    merge ~cmp l1 l2
