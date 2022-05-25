
(* Boolean composition of 'a atoms. *)

type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or of 'a bexpr list

val bool2s : ('a -> string) -> 'a bexpr -> string
