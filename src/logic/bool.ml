type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or of 'a bexpr list

let rec bool2s fu = function
  | V x -> fu x
  | And l -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " and " l
  | Or l  -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " or " l



