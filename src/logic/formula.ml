open Petrinet
open Net
open Bool

(* Simple expression *)
type simple =
  (* Constant *)
  | K of int

  (* multiplier * place *)
  | P of int * pl_id


(* A sum of simple expressions *)
type expr = simple list

(* Equal, Less than, Less or equal *)
type rel = NE | EQ | LT | LE

type atom =
  { left: expr ;
    rel: rel ;
    right: expr }

type formula = atom bexpr

type goal =
  { form: formula ;
    negates: bool }  (* Are we looking for the formula or for its negation? *)

type t = goal
