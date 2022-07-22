type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or  of 'a bexpr list
                | Not of 'a bexpr

let rec bool2s fu = function
  | V x -> fu x
  | And l -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " /\\ " l
  | Or  l  -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " \\/ " l
  | Not e  -> "-(" ^ bool2s fu e ^ ")"

let rec eval_bool fu = function
  | V x -> fu x
  | And l -> List.for_all (eval_bool fu) l
  | Or  l  -> List.exists (eval_bool fu) l
  | Not e  -> not ((eval_bool fu) e)
