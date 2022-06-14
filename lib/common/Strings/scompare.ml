

let zero = Text.code "0"

let cmp_num_string s1 s2 =

  let rec get_num acu p =
    match Text.next p with
    | None -> (acu, p)
    | Some (c, p') ->
      if Text.is_digit c then get_num (acu * 10 + Text.code c - zero) p'
      else (acu, p)
  in

  let rec loop p1 p2 =
    match Text.next p1, Text.next p2 with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some (c1, p1a), Some (c2, p2a) ->
      if Text.is_digit c1 && Text.is_digit c2 then
          let (k1,p1b) = get_num 0 p1
          and (k2,p2b) = get_num 0 p2 in
          if k1 = k2 then loop p1b p2b
          else Stdlib.compare k1 k2

      else
          if c1 = c2 then loop p1a p2a
          else Text.compare c1 c2
  in

  Text.(loop (pointer_l s1) (pointer_l s2))
