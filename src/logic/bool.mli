
(* Boolean composition of 'a atoms. *)

type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or  of 'a bexpr list
                | Not of 'a bexpr
                | True
                | False

val bool2s : ('a -> string) -> 'a bexpr -> string

val eval_bool: ('a -> bool) -> 'a bexpr -> bool

(* dnf  v-negation 
 * Returns a DNF. The result does not contain any Not subexpressions. *)
val dnf: lit_neg:('a -> 'a) -> 'a bexpr -> 'a bexpr

(* A cube is a conjunction of 'a values. *)
type 'a cube = 'a list

(* Transforms a DNF into a (or) list of cubes *)
val dnf_to_list: 'a bexpr -> 'a cube list 

val list_to_dnf: 'a cube list -> 'a bexpr
