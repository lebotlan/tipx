

(* Find signal name.
 * Beware, apparently, OCaml signal id differs from kill -l ids ?!!? *)
val sname: int -> string

(* Record last signal date. 
 * Exits if SIGINT or SIGQUIT is received twice in the given delay (in s). *)
val mk_record: delay:float -> (int -> unit)
                       
  


  
