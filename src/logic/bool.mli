
(* Boolean composition of 'a atoms. *)

type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or  of 'a bexpr list
                | Not of 'a bexpr
                | True
                | False

val bool2s : ('a -> string) -> 'a bexpr -> string

val eval_bool: ('a -> bool) -> 'a bexpr -> bool

(* dnf  v-negation *)
val dnf: ('a -> 'a) -> ?neg:bool -> 'a bexpr -> 'a bexpr
