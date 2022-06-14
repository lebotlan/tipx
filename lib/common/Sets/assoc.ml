type ('a, 'b) assoc =
  { table: ('a, 'b) Hashtbl.t ;
    init: ('a -> 'b) }

type ('a, 'b) t = ('a, 'b) assoc

(* type ('a, 'b) table = ('a, 'b) Hashtbl.t *)

let create ?(size=32) ~init () =
  { table = Hashtbl.create size ;
    init }

let get assoc key =
  try Hashtbl.find assoc.table key
  with Not_found ->
    let value = assoc.init key in
    Hashtbl.add assoc.table key value ;
    value

let mem assoc key = Hashtbl.mem assoc.table key

let remove assoc key = Hashtbl.remove assoc.table key

let set assoc key value = Hashtbl.replace assoc.table key value

let get_existing assoc key = Hashtbl.find assoc.table key

let update assoc key f =
  let old = get assoc key in
  Hashtbl.replace assoc.table key (f old)

let update_if_exists assoc key f =
  try Hashtbl.replace assoc.table key (f (Hashtbl.find assoc.table key))
  with Not_found -> ()

let update_all assoc f =
  Hashtbl.iter (fun key value -> Hashtbl.replace assoc.table key (f key value)) assoc.table

let table assoc = assoc.table

let fold assoc acu f = Hashtbl.fold f assoc.table acu
let iter assoc f = Hashtbl.iter f assoc.table 
let size assoc = Hashtbl.length assoc.table

let map assoc f = 
  let ass2 =
    { table = Hashtbl.create (size assoc) ;
      init  = (fun key -> f key (assoc.init key)) }
  in

  iter assoc (fun key v -> Hashtbl.add ass2.table key (f key v)) ;
  ass2

let map2 ass1 ass2 f =
  let ass3 =
    { table = Hashtbl.create (10 + max (size ass1) (size ass2)) ;
      init  = (fun key -> f key (ass1.init key) (ass2.init key)) }
  in

  (* Keys from ass1. *)
  iter ass1 (fun key v1 -> Hashtbl.add ass3.table key (f key v1 (get ass2 key))) ;

  (* Keys from ass2 that were not in ass1. *)
  iter ass2 (fun key v2 -> if mem ass1 key then () else Hashtbl.add ass3.table key (f key (get ass1 key) v2)) ;

  ass3

let incr assoc key = update assoc key (fun x -> x + 1)
let decr assoc key = update assoc key (fun x -> x - 1)

let to_list map assoc = fold assoc [] (fun a b acu -> map a b :: acu)

let sort assoc cmp = List.sort cmp (to_list (fun a b -> (a,b)) assoc)

let sort_key assoc mkey = sort assoc (fun p1 p2 -> Stdlib.compare (mkey p1) (mkey p2))

let of_list ~init ~upd bindings =
  let assoc = create ~size:(List.length bindings) ~init () in
  List.iter (fun (a,b) -> update assoc a (fun c -> upd a b c)) bindings ;
  assoc

let merge assoc f ~import_from = iter import_from (fun a c -> update assoc a (fun b -> f a c b))

let of_any_list l get_key =
  let tab = create ~init:(fun _ -> []) () in
  List.iter (fun x -> update tab (get_key x) (fun acu -> x :: acu)) l ;
  tab

let copy assoc = 
  { table = Hashtbl.copy assoc.table ;
    init = assoc.init }

(*** Functorial interface ***)

module type ASSOC =
sig
  type key
  type 'b assoc
  type 'b t = 'b assoc
  type 'b table

  val create: ?size:int -> init:(key -> 'b) -> unit -> 'b t
  val get: 'b t -> key -> 'b
  val get_existing: 'b t -> key -> 'b
  val set: 'b t -> key -> 'b -> unit
  val update: 'b t -> key -> ('b -> 'b) -> unit
  val incr: int t -> key -> unit
  val decr: int t -> key -> unit
  val update_if_exists: 'b t -> key -> ('b -> 'b) -> unit
  val update_all: 'b t -> (key -> 'b -> 'b) -> unit
  val fold: 'b t -> 'c -> (key -> 'b -> 'c -> 'c) -> 'c
  val iter: 'b t -> (key -> 'b -> unit) -> unit
  val table: 'b t -> 'b table
  val size: 'b t -> int
  val sort: 'b t -> ((key * 'b) -> (key * 'b) -> int) -> (key * 'b) list
  val sort_key: 'b t -> ((key * 'b) -> 'c) -> (key * 'b) list
end


(* Is there a convenient way to factorise this code? *)

module Mk_Assoc = functor (Hash: Hashtbl.S) ->
struct
  type key = Hash.key
  type 'b assoc =
    { table: 'b Hash.t ;
      init: (key -> 'b) }

  type 'b t = 'b assoc

  type 'b table = 'b Hash.t

  let create ?(size=32) ~init () =
    { table = Hash.create size ;
      init }

  let get assoc key =
    try Hash.find assoc.table key
    with Not_found ->
      let value = assoc.init key in
      Hash.add assoc.table key value ;
      value
  
  let get_existing assoc key = Hash.find assoc.table key

  let set assoc key value = Hash.replace assoc.table key value

  let update assoc key f =
    let old = get assoc key in
    Hash.replace assoc.table key (f old)
  
  let update_if_exists assoc key f =
    try Hash.replace assoc.table key (f (Hash.find assoc.table key))
    with Not_found -> ()

  let update_all assoc f =
    Hash.iter (fun key value -> Hash.replace assoc.table key (f key value)) assoc.table

  let table assoc = assoc.table

  let fold assoc acu f = Hash.fold f assoc.table acu
  let iter assoc f = Hash.iter f assoc.table 
  let size assoc = Hash.length assoc.table

  let incr assoc key = update assoc key (fun x -> x + 1)
  let decr assoc key = update assoc key (fun x -> x - 1)

  let sort assoc cmp = 
    let list = fold assoc [] (fun a b acu -> (a,b) :: acu) in
    List.sort cmp list

  let sort_key assoc mkey = sort assoc (fun p1 p2 -> Stdlib.compare (mkey p1) (mkey p2))

end
