open Bi_io

exception Deserialisation_error of string

(* Raise Deserialisation_error *)
val derr: string -> 'a

val dexn: ?msg:string -> exn -> 'a

(* Deserialize a list *)
(* val de_list: tree -> (tree -> 'a) -> 'a list *)
val de_list: (tree -> 'a) -> tree -> 'a list

(* fix *)
val list_of_string: string -> (tree -> 'a) -> 'a list
    
(* "Serialize" a list *)
(* val se_list: 'a list -> ('a -> tree) -> tree *)
val se_list: ('a -> tree) -> 'a list -> tree
                                                
val string_of_list: 'a list -> ('a -> tree) -> string

  

val de_string: tree -> string
val se_string: string -> tree

