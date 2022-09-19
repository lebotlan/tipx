open Logic

type projected_goal =
  {
    (* Final formula *)
    p_goal: Formula.t ;

    (* The annotation is complete (or shadow-complete) if the projected formula is equivalent to the initial formula. 
     * If uncomplete, then the initial formula can be SAT whereas the projected one is not. *)
    complete: bool ;

    (* List of projected cubes... *)
    (* ... *)
  }

(* Maps a formula expressed on the initial net to a formula expressed on the reduced net. *)
val project: timeout:int -> Tfg.t -> Formula.t -> projected_goal
  
                                     
