open Net
    

(* A marking is a mutable buffer to hold a (positive) marking vector. *)
type marking

type t = marking

(* safe: safe net, places are bounded by 1 *)
val init: ?safe:bool -> net -> marking

val clone: t -> t

val get: t -> pl -> int

(* add m pl x  returns m, modified,  or a new marking. *)
val add: t -> pl -> int -> t

val compare: t -> t -> int


val tos: ?max:int -> t -> string

  (* trouver rapidement les places non nulles (< 0 ou > 0) *)
  

(*

    fireable : comparer les listes ordonnées ?  appliquer un masque ?

    ajouter une donnée privée à pl et tr pour stocker une représentation interne ?

    stocker aussi sous forme de marking ?  pl_in_m : mark ?  permet de faire des masque ou d'être plus efficace  (éviter une liste => tableau avec données compactes ?)


  -> mark abstrait pour pouvoir changer librement de représentation (intarray / sparse)
    quand sparse, trier dans l'ordre des places ! les algos seront plus efficaces


              voir quand changer la représentation du marking (sparse/dense) ? choix au début ? dynamiquement ?

              type bla = Sparse of .. list | Dense of intarray

*)
  
