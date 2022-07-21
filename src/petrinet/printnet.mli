(* open Marking *)
open Net

(* v : verbosity 
 * net : used to print places/transitions names instead of ids. *)

(*
val marking2s: ?v:int -> ?net:net -> marking -> string

val pl2s: ?v:int -> ?net:net -> pl -> string

val tr2s: ?v:int -> ?net:net -> tr -> string

val net2s: ?v:int -> net -> string

*)

val pl2s: pl -> string

val tr2s: net -> tr -> string

val net2s: net -> string
