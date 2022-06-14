open Bi_io

exception Deserialisation_error of string

let derr msg = raise (Deserialisation_error msg)

let dexn ?(msg="") e = raise (Deserialisation_error (msg ^ ". " ^ Printexc.to_string e))

let de_list map t =
  match t with
  | `Tuple ar -> List.map map (Array.to_list ar)
  | _ -> derr "Not a list"

let list_of_string s map = de_list map (tree_of_string s)

let se_list map l = `Tuple (Array.of_list (List.map map l))

let string_of_list l map = string_of_tree (se_list map l)
    

let se_string s = `String s

let de_string = function
  | `String s -> s
  | _ -> derr "Not a string."


