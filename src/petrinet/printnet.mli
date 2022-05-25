open Net

(* v : verbosity 
 * net : used to print places/transitions names instead of ids. *)
    
val mark2s: ?v:int -> ?net:net -> mark -> string

val pl2s: ?v:int -> ?net:net -> pl -> string

val tr2s: ?v:int -> ?net:net -> tr -> string

val net2s: ?v:int -> net -> string
  
