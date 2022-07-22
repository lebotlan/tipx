open Formula
open Petrinet
open Net

(* 
val simple2s: ?v:int -> ?net:net -> simple -> string

val expr2s: ?v:int -> ?net:net -> expr -> string

val atom2s: ?v:int -> ?net:net -> atom -> string

val formula2s: ?v:int -> ?net:net -> formula -> string
 *)

val simple2s: net -> simple -> string

val expr2s: net -> expr -> string

val atom2s: net -> atom -> string

val formula2s: net -> formula -> string

val goal2s: net -> goal -> string