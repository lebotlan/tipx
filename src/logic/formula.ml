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
type rel = EQ | LT | LE

type atom =
  { left: expr ;
    rel: rel ;
    right: expr }

type formula = atom bexpr

type t = formula
