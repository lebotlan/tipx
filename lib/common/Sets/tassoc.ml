type 'a key = 'a list
type 'a revkey = 'a list

(* An assoc is a tree. Nodes are MAPS to children. *)

type ('a, 'b) node =
  { value: 'b option ;
    childs: ('a, ('a,'b) node) Mapp.t }

type ('a, 'b) tassoc =
  { default: 'a key -> 'b ;
    cmp: 'a -> 'a -> int ;
    node: ('a, 'b) node }
    
type ('a, 'b) t = ('a, 'b) tassoc

let empty_node cmp =
  { value = None ;
    childs = Mapp.empty cmp }

let create ?(cmp=Stdlib.compare) ~default () = { default ; cmp ; node = empty_node cmp }

let rec node_size n = Mapp.fold (fun _ n2 s -> s + node_size n2) n.childs (if n.value = None then 0 else 1)
  
let size t = node_size t.node

let rec oget_node n = function
  | [] -> n.value
  | x :: xs ->
    begin match Mapp.find_opt x n.childs with
      | None -> None
      | Some n2 -> oget_node n2 xs
    end

let oget t k = oget_node t.node k

let get t key =
  match oget t key with
  | Some v -> v
  | None -> t.default key

let mem t k = oget t k <> None

let rec set_node cmp n v = function
  | [] -> { n with value = Some v }
  | x :: xs ->
    let upd = function
      | None -> Some (set_node cmp (empty_node cmp) v xs)
      | Some n2 -> Some (set_node cmp n2 v xs)
    in
    
    { n with childs = Mapp.update x upd n.childs }        

let set t k v = { t with node = set_node t.cmp t.node v k }

let update ?(only_exist=false) t k upd =
  match oget t k, only_exist with
  | None, false -> set t k (upd (t.default k))
  | None, true -> t
  | Some v, _ -> set t k (upd v)

let rec update_all_node upd path n =
  let value = match n.value with
    | None -> None
    | Some v -> Some (upd path v)
  in

  { value ;
    childs = Mapp.mapi (fun x n2 -> update_all_node upd (x :: path) n2) n.childs }

let update_all t upd = { t with node = update_all_node upd [] t.node }

let map ?default t upd =

  let default = match default with
    | Some d -> d
    | None -> fun k -> upd k (t.default k)
  in
  
  { default ; cmp = t.cmp ; node = update_all_node upd [] t.node }

let rec fold_node acu path f n =
  let acu = match n.value with
    | None -> acu
    | Some v -> f path v acu
  in  
  Mapp.fold (fun x n2 acu -> fold_node acu (x :: path) f n2) n.childs acu

let fold t acu f = fold_node acu [] f t.node

let iter t f = fold t () (fun key v () -> f key v)


let rec remove_node cmp clean n = function
  | [] -> if clean then empty_node cmp else { n with value = None }
  | x :: xs ->
    let upd = function
      | None -> None
      | Some n2 -> Some (remove_node cmp clean n2 xs)
    in
    { n with childs = Mapp.update x upd n.childs }

let remove clean t key = { t with node = remove_node t.cmp clean t.node key }

let remove_val t key = remove false t key
let remove_subtree t key = remove true t key

let map2 ?(revk=false) t1 t2 f =

  (* Keys from t1 *)
  let t = map t1 (fun rkey v -> let key = List.rev rkey in f (if revk then rkey else key) v (get t2 key)) in

  (* Keys from t2 *)
  fold t2 t begin fun rkey v acu ->
    let key = List.rev rkey in
    match oget t1 key with
    | None ->
      assert (not (mem acu key)) ; (* The key cannot be already there. It was not in t1. It cannot appear twice in t2. *)
      set acu key (f (if revk then rkey else key) (t1.default key) v)
      
    | Some _ -> acu (* Already done *)
  end
  
let merge_into ?(revk=false) ~from ~into f = fold from into (fun rkey v acu -> let key = List.rev rkey in update acu key (fun v0 -> f (if revk then rkey else key) v v0))

let to_list t f = fold t [] (fun rkey v acu -> f rkey v :: acu)

let sort t cmp = List.sort cmp (to_list t (fun k v -> (k,v)))

let map_sort t f = sort t (fun a b -> Stdlib.compare (f a) (f b))

let of_any_list ?cmp l readkey =
  let t0 = create ?cmp ~default:(fun _ -> []) () in 
  List.fold_left (fun t x -> update t (readkey x) (fun l -> x :: l)) t0 l

