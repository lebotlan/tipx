open Logic
open Formula
open Bool

(* L'algorithme projete cube par cube, et donc project_formula itére sur les cubes.
 *
 *
 * Créer un id par litéral
 * Associer ces ids aux noeuds correspondant dans une hash map
 * Il faut ajouter le d'(in)équation associé au noeud (eq,le,lt,ge,gt,diff) et la multiplicité k (bien souvent 1)
 *
 * La projection itere sur les noeuds (bottom-up)
 * Pour réaliser la projection sur un noeud il faut que les succésseurs soient déjà projetés.
 *
 * Distinguer les cas: red et agg
 *
 * Red est trivial : on duplique les informations sur les parents.
 *
 * Agglomération : 
 * 1°) On peut faire disparaitre un une contrainte le/lt sur un des fils, si il existe un fils non contraint par un le/lt
 * 2°) Si des fils ont des equations différentes cela rajoute une nouvelle inéquation pour gérer la contrainte mutuelle
 * 3°) Des opérateurs non-linéaires apparaissent seulement quand les tous les neouds ont une borne sup ET inf.
 *
 * A la fin on somme les ids des roots et on récupère les contraintes non linéaires générées par les agglomerations problématiques
*)


let project_cube _tfg cube = cube


(* Dummy function -> to rewrite *)
let project_formula tfg = function
  | Or l -> And (project_cube tfg l)
  | And _ as c -> project_cube tfg c
  | V _ as v -> project_cube tfg v
  | _ -> assert false


let project tfg goal = 
  { form = project_formula tfg goal.form ; 
    negates=goal.negates }


  