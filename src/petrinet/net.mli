
(* Unique identifier for places.
 * The identifier is its index in the array (see all_pl below). *)
type pl_id = int

(* Unique identifier for transitions.
 * The identifier is its index in the array (see all_tr below). *)
type tr_id = int

type weight = int

type token_count = int
  
(* Initial marking.
 * The array is indexed by places index. *)
type mark = token_count array

  -> mark abstrait pour pouvoir changer librement de représentation (intarray / sparse)
    quand sparse, trier dans l'ordre des places ! les algos seront plus efficaces

              "a marking is a mutable buffer to hold a marking."

              voir quand changer la représentation du marking (sparse/dense) ? choix au début ? dynamiquement ?

              type bla = Sparse of .. list | Dense of intarray

  -> un module intarray avec des cases int8 int16 int32 int64 (62 ou 63 en fait)

    fireable : comparer les listes ordonnées ?  appliquer un masque ?

    ajouter une donnée privée à pl et tr pour stocker une représentation interne ?

    stocker aussi sous forme de marking ?  pl_in_m : mark ?  permet de faire des masque ou d'être plus efficace  (éviter une liste => tableau avec données compactes ?)


  -> fonction "optimise"  qui clôt un réseau (on n'ajoutera plus de places/transitions ensuite) => alloue les tableaux.

  -> Annoter les fonctions avec @noalloc   pour indiquer qu'elles n'allouent pas de mémoire.
    
    

type pl =
  { pl_id: id ;
    pl_name: string ;

    (* Sorted by td_id *)
    pl_in:  (weight * tr_id) list ;
    pl_out: (weight * tr_id) list }

and tr =
  { tr_id:  id ;
    tr_name: string ;

    (* Sorted by pl_id *)
    tr_in:  (weight * pl_id) list ;    
    tr_out: (weight * pl_id) list }

(* A net is mutable *)
type net

type t = net


(*** Read ***)

(* name can be empty *)
val get_name: net -> string

val init_mark: net -> mark
  

(* The id of each place is its index in the array. *)
  val all_pl: net -> pl array

val get_pl: net -> pl_id -> pl
  

(* The id of each transition is its index in the array. *)
  val all_tr: net -> tr array

val get_tr: net -> tr_id -> tr
  


(*** Build ***)


(* Create empty net *)
val mk_empty: ?name:string -> unit -> net

val add_pl: net -> pl -> unit

val add_tr: net -> tr -> unit

