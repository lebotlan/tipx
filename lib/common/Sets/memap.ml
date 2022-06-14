module MS = Meset

module type M =
sig
  type key
  type 'a map
  module Keyset: Meset.S with type elt = key
                                
  val empty: ?compare:(key -> key -> int) -> tos:('a -> string) -> mergev:('a -> 'a -> 'a) -> unit -> 'a map
  val kempty: ?compare:(key -> key -> int) -> tos:('a -> string) -> kmergev:(key -> 'a -> 'a -> 'a) -> unit -> 'a map
  val size: 'a map -> int
  val clear: 'a map -> 'a map
  val choose: 'a map -> (key * 'a) option
  val keys: 'a map -> Keyset.set
  val update: ?exist:bool -> ?force:bool -> ?default:(key -> 'a) -> ?f:(key -> 'a -> 'a) -> 'a map -> key -> 'a map
  val update_k: ?exist:bool -> ?force:bool -> ?default:(key -> 'a) -> ?f:(key -> 'a -> 'a) -> 'a map -> key -> 'a map * key
  val add: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map
  val add_k: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map * key
  val add_or_merge: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map
  val find_result2s: 'a map -> (key * 'a) MS.mem_result -> string
  val find: no:bool -> 'a map -> key -> (key * 'a) MS.mem_result
  val remove: 'a map -> key -> 'a map
  (*  val filter: 'a map -> (key -> 'a -> bool) -> 'a map *)
  val fold: ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> 'a map -> 'c -> (key -> 'a -> 'c -> 'c) -> 'c
  val lwt_fold: ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> 'a map -> 'c -> (key -> 'a -> 'c -> 'c Lwt.t) -> 'c Lwt.t
  val map2s: ?key2s:(key -> string) -> ?v2s:('a -> string) -> ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> ?title:string -> 'a map -> string
  val mmap: 'a map -> tos:('b -> string) -> mergev:('b -> 'b -> 'b) -> ('a -> 'b) -> 'b map
  val imap: 'a map -> ('a -> 'a) -> 'a map
end

module Make = functor (Elt: MS.ELEMENT) ->
struct
  type key = Elt.elt

  module S = MS.Make(Elt)
  module Keyset = S

  module M = Map.Make (struct type t = Elt.id  let compare = Stdlib.compare end)

  (* The map uses the first id of the key. 
   * Invariant: { the first id of keys } == { keys of idmap }  *)
  type 'a map = {
    keys: S.set ;    
    v2s: 'a -> string ;
    mergev: key -> 'a -> 'a -> 'a ;
    idmap: 'a M.t ;
    compare: (key -> key -> int) option }

  let keys mp = mp.keys
  
  let raiserror e msg = raise (MS.Error (e, msg))

  let wrap_merge tos mergev =
    (fun key a b -> try mergev key a b with e -> raiserror (MS.User e) ("mergev on " ^ tos a ^ " and " ^ tos b ^ " for element " ^ Elt.elt2s key))
  
  let empty ?compare ~tos ~mergev () =
    { keys = S.empty ;
      v2s = tos ;
      mergev = wrap_merge tos (fun _ a b -> mergev a b) ;
      idmap = M.empty ;
      compare }

  let kempty ?compare ~tos ~kmergev () =
    { keys = S.empty ;
      v2s = tos ;
      mergev = wrap_merge tos kmergev ;
      idmap = M.empty ;
      compare }

  let clear mp =
    { keys = S.empty ;
      v2s = mp.v2s ;
      mergev = mp.mergev ;
      idmap = M.empty ;
      compare = mp.compare }
  
  let get_firstid key =
    match Elt.get_ids key with
    | [] -> raiserror MS.No_identifier (Elt.elt2s key)
    | id :: _ -> id
    | exception e -> raiserror (MS.User e) ("get_ids on " ^ Elt.elt2s key)

  let size mp =
    let res = S.size mp.keys in
    assert (res = M.cardinal mp.idmap) ;
    res

  let update_k ?exist ?force ?default ?(f=fun _ v -> v) mp key =

    let (keys, newkey, previous_value, idmap) =
      match (S.mem ~no:false mp.keys key, exist) with
      | MS.No, Some true -> raiserror MS.Elt_not_found (Elt.elt2s key)
      | MS.Undetermined, Some true -> raiserror MS.Partial (Elt.elt2s key)
      | MS.Yes _, Some false -> raiserror MS.Overwrite (Elt.elt2s key)
                                  
      | (MS.No | MS.Undetermined), _ ->
        begin match default with
          | None -> raiserror MS.Elt_not_found (Elt.elt2s key)
          | Some defv -> (S.merge_in mp.keys key, key, defv key, mp.idmap)
        end
      
      | MS.Yes _, _ ->
        let (keys, newkey) = S.merge_in_e ~exist:true ?force mp.keys key in

        if keys == mp.keys then (keys, newkey, M.find (get_firstid newkey) mp.idmap, mp.idmap)
        else
          
          (* Find keys that have been removed. *)
          match S.mem ~no:false mp.keys newkey with
          | MS.No | MS.Undetermined -> assert false
          | MS.Yes [] -> assert false
          | MS.Yes (ckey1 :: changed_keys) ->

            let oldid1 = get_firstid ckey1 in
            assert (M.mem oldid1 mp.idmap) ;
            let oldv1 = M.find oldid1 mp.idmap
            and idm1  = M.remove oldid1 mp.idmap in
            
            (* Remove these ids. *)
            let (idmap, newv) = List.fold_left
                begin fun (idm, vv) ckey ->
                  let oldid = get_firstid ckey in
                  assert (M.mem oldid idm) ;
                  let vv' = mp.mergev newkey (M.find oldid idm) vv in
                  (M.remove oldid idm, vv')
                end
                (idm1, oldv1) changed_keys
            in
            (keys, newkey, newv, idmap)
    in
    let idmap = M.add (get_firstid newkey) (f newkey previous_value) idmap in

    { keys ;
      v2s    = mp.v2s ;
      mergev = mp.mergev ;
      idmap ;
      compare = mp.compare}, newkey

  let update ?exist ?force ?default ?f mp key = fst (update_k ?exist ?force ?default ?f mp key)

  let add_k ?exist ?force mp key v = update_k ?exist ?force ~default:(fun _ -> v) ~f:(fun _ _ -> v) mp key
                                                   
  let add ?exist ?force mp key v = fst (add_k ?exist ?force mp key v)

  let add_or_merge ?exist ?force mp key v = update ?exist ?force ~default:(fun _ -> v) ~f:(fun newkey oldv -> mp.mergev newkey oldv v) mp key

  
  let find_result2s mp res = match res with
    | MS.No -> "No"
    | MS.Undetermined -> "Undetermined"
    | MS.Yes l -> Printf.sprintf "Yes [ %s ]"
                    (List.fold_left (fun acu (key,v) -> (if acu = "" then "" else acu ^ " ; ") ^ Elt.elt2s key ^ " => " ^ mp.v2s v) "" l)

  let find ~no mp key =
    match S.mem ~no mp.keys key with
    | MS.No -> MS.No
    | MS.Undetermined -> MS.Undetermined
    | MS.Yes keys ->
      MS.Yes (List.map (fun k -> (k, M.find (get_firstid k) mp.idmap)) keys)

  (*
  let filter mp ff =

    let (keys, idmap) = S.fold mp.keys () (fun key acu -> )
    in
    
    { keys ;
      v2s = mp.v2s ;
      mergev = mp.mergev ;
      idmap }
  *)
        
  let remove mp key =
    let keys = S.remove mp.keys key
    and idmap =
      let fid = get_firstid key in
      assert (M.mem fid mp.idmap) ;
      M.remove fid mp.idmap
    in

    { keys ;
      v2s = mp.v2s ;
      mergev = mp.mergev ;
      idmap ;
      compare = mp.compare }

  let choose mp =
    match S.choose mp.keys with
    | None -> None
    | Some stud ->
      begin match find ~no:false mp stud with
        | MS.No -> assert false
        | MS.Undetermined -> assert false
        | MS.Yes [] -> assert false
        | MS.Yes (pair :: _) -> Some pair
      end      

  let mmap mp ~tos ~mergev f =
    { keys = mp.keys ;
      v2s = tos ;
      mergev = wrap_merge tos (fun _ a b -> mergev a b) ;
      idmap = M.map f mp.idmap ;
      compare = mp.compare }

  let imap mp f = { mp with idmap = M.map f mp.idmap }
  
  let choose_compare ck c mp =
    match ck, c with
    | None, None -> mp.compare
    | _ -> c
  
  let fold ?comparekey ?compare mp acu f =
    let compare = choose_compare comparekey compare mp in
    S.fold ?comparekey ?compare mp.keys acu (fun key acu -> f key (M.find (get_firstid key) mp.idmap) acu)

  let lwt_fold ?comparekey ?compare mp acu f =
    let compare = choose_compare comparekey compare mp in
    S.lwt_fold ?comparekey ?compare mp.keys acu (fun key acu -> f key (M.find (get_firstid key) mp.idmap) acu)

  let map2s ?(key2s=Elt.elt2s) ?v2s ?comparekey ?compare ?(title="") mp =
    let compare = choose_compare comparekey compare mp in
    let v2s = match v2s with None -> mp.v2s | Some f -> f in
    
    let init = Printf.sprintf "===================  MAP%s  =================" (if title="" then "" else ": " ^ title) in
    let all =
      fold ?comparekey ?compare mp (init ^ "\n\n")
        begin fun key v acu ->
          acu ^ Printf.sprintf " - %s => %s\n" (key2s key) (v2s v)
        end
    in
    all ^ String.make (String.length init) '='

end
