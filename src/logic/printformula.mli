open Formula
open Petrinet
open Net

(* 
val simple2s: ?v:int -> ?net:net -> simple -> string

val expr2s: ?v:int -> ?net:net -> expr -> string

val atom2s: ?v:int -> ?net:net -> atom -> string

val formula2s: ?v:int -> ?net:net -> formula -> string
 *)


val atom2s: (pl_id -> string) -> atom -> string

val formula2s: (pl_id -> string) -> formula -> string

val goal2s: (pl_id -> string) -> goal -> string

(* Like goal2s, in smt-lib format *)
val goal2smt: (pl_id -> string) -> goal -> string
  
