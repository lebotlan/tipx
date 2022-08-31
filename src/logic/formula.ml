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

let get_rel = function
  | NE -> (!=)
  | EQ -> (=)
  | LT -> (<)
  | LE -> (<=)

let atom_negation atom =
  match atom.rel with 
  | NE -> {left = atom.left ; rel = EQ ; right = atom.right }
  | EQ -> {left = atom.left ; rel = NE ; right = atom.right }
  | LT -> {left = atom.right ; rel = LE ; right = atom.left }
  | LE -> {left = atom.right ; rel = LT ; right = atom.left }
 
type formula = atom bexpr

type goal =
  { form: formula ;
    negates: bool }  (* Are we looking for the formula or for its negation? *)

type t = goal

let dnf goal =
  { form = Bool.dnf ~lit_neg:atom_negation goal.form ;
    negates = goal.negates }

let is_const_expr l =
  let rec loop k = function
    | [] -> Some k
    | K x :: rest -> loop (k + x) rest
    | P _ :: _ -> None
  in
  loop 0 l

let eval_only_k a =
  match is_const_expr a.left, is_const_expr a.right with
  | Some k1, Some k2 -> Some (bool2b (get_rel a.rel k1 k2))
  | _ -> None

let simplify goal =
  { form = Bool.map_simplify eval_only_k goal.form ;
    negates = goal.negates }
