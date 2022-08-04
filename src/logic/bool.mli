
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

(* Transform a DNF into a list of list *)
val dnf_to_list: 'a bexpr -> 'a list list 

(* Only write list_to_dnf? *)
(* val lit_to_bool: 'a -> 'a bexpr

val cube_list_to_bool: 'a bexpr list -> 'a bexpr

val clause_list_to_bool: 'a bexpr list -> 'a bexpr *)

val list_to_dnf: 'a list list -> 'a bexpr
