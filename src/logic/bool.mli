
(* Boolean composition of 'a atoms. *)

type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or  of 'a bexpr list
                | Not of 'a bexpr
                | True
                | False

val bool2b: bool -> 'a bexpr

val bexpr2s: ('a -> string) -> 'a bexpr -> string

(* Like bexpr2s in smt-lib format *)
val bexpr2smt: ('a -> string) -> 'a bexpr -> string

(* Prints an infix operator & arguments in smt-lib format *)
val infix: string -> ('a -> string) -> 'a list -> string

val eval_bexpr: ('a -> bool) -> 'a bexpr -> bool

(* dnf  v-negation 
 * Returns a DNF. The result does not contain any Not subexpressions. *)
val dnf: lit_neg:('a -> 'a) -> 'a bexpr -> 'a bexpr

(* A cube is a conjunction of 'a values. *)
type 'a cube = 'a list

(* Transforms a DNF into a (or) list of cubes *)
val dnf_to_list: 'a bexpr -> 'a cube list 

val list_to_dnf: 'a cube list -> 'a bexpr


(* map_simplify f : maps literals with f, and then simplifies the resulting expression, e.g. And [ ... ; False ; ... ] -> False  *)
val map_simplify: ('a -> 'a bexpr option) -> 'a bexpr -> 'a bexpr


