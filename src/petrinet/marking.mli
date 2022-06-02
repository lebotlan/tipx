open Net
    

(* A marking is a mutable buffer to hold a marking vector. 
 * Marking can be negative. *)
type marking

type t = marking

val init: net -> marking

val clone: t -> t

val get: t -> pl -> int

val add: t -> pl -> int -> unit

val cmp: t -> t -> int

  (* trouver rapidement les places non nulles (< 0 ou > 0) *)
  

(*

  -> un module intarray avec des cases int8 int16 int32 int64 (62 ou 63 en fait)

    fireable : comparer les listes ordonnées ?  appliquer un masque ?

    ajouter une donnée privée à pl et tr pour stocker une représentation interne ?

    stocker aussi sous forme de marking ?  pl_in_m : mark ?  permet de faire des masque ou d'être plus efficace  (éviter une liste => tableau avec données compactes ?)


  -> mark abstrait pour pouvoir changer librement de représentation (intarray / sparse)
    quand sparse, trier dans l'ordre des places ! les algos seront plus efficaces



              voir quand changer la représentation du marking (sparse/dense) ? choix au début ? dynamiquement ?

              type bla = Sparse of .. list | Dense of intarray




*)
  
