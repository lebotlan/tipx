open Logic

type projected_goal =
  {
    (* Final formula *)
    p_goal: Formula.t ;

    (* The annotation is complete (or shadow-complete) if the projected formula is equivalent to the initial formula. 
     * If uncomplete, then the initial formula can be SAT whereas the projected one is not. *)
    complete: bool ;

    (* Number of cubes in the final formula (before simplification). p_goal may contain less cubes. *)
    n_cubes: int ;

    (* Number of cubes in the final formula (before simplification) that are complete. *)
    n_complete_cubes: int ;

    (* List of projected cubes... *)
    (* ... *)
  }

(* Maps a formula expressed on the initial net to a formula expressed on the reduced net. *)
val project: timeout:int -> Tfg.t -> Formula.t -> projected_goal
  
                                     
