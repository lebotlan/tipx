module type ELEMENT =
sig
  type elt
  type id
  val get_ids: elt -> id list
      
  type nature
  val nature: id -> nature
    
  val merge: force:bool -> elt -> elt -> elt
  val missing_ids: elt -> nature list

  val elt2s: elt -> string
  val id2s: id -> string
  val nat2s: nature -> string    
end

type 'a mem_result = Yes of 'a list | No | Undetermined

type error =
  | No_identifier
  | Unmergeable of exn
  | Overwrite
  | Lostid of string list
  | Elt_not_found
  | Partial
  | User of exn
    
exception Error of error * string

let error2s = function
  | No_identifier -> "Empty list of identifiers (No_identifier)"
  | Unmergeable e -> "Unmergeable elements (Unmergeable) " ^ (Printexc.to_string e)
  | Overwrite -> "Cannot overwrite element (Overwrite)"
  | Lostid ids -> Printf.sprintf "Removed identifier (Lostid = %s)" (String.concat ", " ids)
  | Elt_not_found -> "Element not found (Elt_not_found)"
  | Partial -> "Element is underspecified (Partial)"
  | User e -> "User exception (User) " ^ (Printexc.to_string e)
    
(* Register *)
let () = Printexc.register_printer
    begin function
      | Error (err, msg) -> Some ("Meset.Error: " ^ error2s err ^ ": " ^ msg)
      | _ -> None
    end

let raiserror e msg = raise (Error (e, msg))

let myfold l a f = List.fold_left f a l
let mymap l f = List.map f l

let l_push l x = if List.mem x l then l else x :: l

module type S =
sig
  type elt
  type set
  val empty: set
  val size: set -> int
  val choose: set -> elt option
  val mem_result2s: elt mem_result -> string
  val mem : no:bool -> set -> elt -> elt mem_result
  val merge_in: ?exist:bool -> ?force:bool -> set -> elt -> set
  val merge_in_e: ?exist:bool -> ?force:bool -> set -> elt -> set * elt
  val remove: set -> elt -> set
  val fold: ?comparekey:(elt -> 'b) -> ?compare:(elt -> elt -> int) -> set -> 'a -> (elt -> 'a -> 'a) -> 'a
  val lwt_fold: ?comparekey:(elt -> 'b) -> ?compare:(elt -> elt -> int) -> set -> 'a -> (elt -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  val set2s: ?comparekey:(elt -> 'b) -> ?compare:(elt -> elt -> int) -> ?title:string -> set -> string
  val filter: set -> (elt -> bool) -> set
end


module Make = functor (Elt: ELEMENT) ->
struct
  module IdMap = Map.Make (struct type t = Elt.id let compare = Stdlib.compare end)

  type elt = Elt.elt

  let mem_result2s = function
    | No -> "No"
    | Undetermined -> "Undetermined"
    | Yes l -> Printf.sprintf "Yes [ %s ]" (List.fold_left (fun acu elt -> (if acu = "" then "" else acu ^ " ; ") ^ Elt.elt2s elt) "" l)
  
  (* 'Signature' of an element: list of id natures and of missing id natures. Both lists are ordered. *)
  type natsig =
    { nats: Elt.nature list ;
      miss: Elt.nature list }

  (* Note: we expect that most objects will have similar natsigs.
   *       natsigs are stored in an association list, which counts the number of elements related to this natsig. *)
  
  type set =
    {
      (* Maps natsigs to the number of elements having that natsig. *)
      natsigs: (natsig * int) list ;
      
      (* Identifier => element *)
      index: elt IdMap.t ;

      size: int ;
    }
    
  let empty =
    { natsigs = [] ;
      index = IdMap.empty ;
      size = 0 }

  let size set = set.size

  let choose set =
    if set.size = 0 then None
    else
      try let (_, el) = IdMap.choose set.index in Some el
      with Not_found -> assert false
  
  let is_included l1 l2 =
    let rec loop = function
      | [] -> true
      | x :: rest -> List.mem x l2 && loop rest
    in
    loop l1

  let list_sub l1 l2 =
    let rem acu elt = List.filter (fun x -> x <> elt) acu in
    List.fold_left rem l1 l2

  let myget_ids elt =
    try
      let ids = Elt.get_ids elt in
      if ids = [] then raiserror No_identifier (Elt.elt2s elt) ;

      (* Remove doublons *)
      List.fold_left l_push [] ids
        
    with e -> raiserror (User e) ("get_ids on " ^ Elt.elt2s elt)

  let mymissing_ids elt =
    try Elt.missing_ids elt
    with e -> raiserror (User e) ("missing_ids on " ^ Elt.elt2s elt)

  let mynature id =
    try Elt.nature id
    with e -> raiserror (User e) ("nature on " ^ Elt.id2s id)

  
  (*** Membership ***)
  let mem ~no set elt =
    
    let ids = myget_ids elt in

    (*    Printf.printf "mem: ids = %s\n%!" (List.fold_left (fun acu id -> acu ^ (id2s id) ^ ", ") "" ids) ; *)
    
    (* Consider all ids. *)
    let elt_found = myfold ids []
        begin fun acu id ->
          if IdMap.mem id set.index then
            (* Do not insert the same element twice. *)
            let elt = IdMap.find id set.index in
            if List.memq elt acu then acu else elt :: acu
          else acu
        end
    in

    match elt_found with
    (* The element is certainly in the set. *)
    | _ :: _ -> Yes elt_found

    | [] ->
      (* The element was not found. 
       * We do not know yet if it may be in the set or if it is certainly not in the set. *)

      if no then
      
        let a_natures = List.map mynature ids in

        (*        Printf.printf "mem: a_natures = %s\n%!" (List.fold_left (fun acu id -> acu ^ (nat2s id) ^ ", ") "" a_natures) ; *)
        
        (* We consider all the natsigs in this set. 
         * We check if the element (A) is distinct from the elements having each natsig. 
         * loop returns No iff A is distinct from all elements. *)
        let rec loop = function
          | [] -> No
          | (natsig, _) :: rest ->
            
            (* all the identifiers'natures of A are included in the missing natures of B *)
            let is_included_ab = is_included a_natures natsig.miss in

(*            Printf.printf "mem: natsig.miss = %s\n%!" (List.fold_left (fun acu id -> acu ^ (nat2s id) ^ ", ") "" natsig.miss) ;
              Printf.printf "is_included_ab = %b\n%!" is_included_ab ; *)
            
            if is_included_ab then
              (* all the identifiers'natures of B are included in the missing natures of A *)
              let is_included_ba = is_included natsig.nats (mymissing_ids elt) in
              if is_included_ba then Undetermined
              else
                (* not is_included_ba => a & b distinct *)
                loop rest
            else
              (* not is_included_ab => a & b distinct *)
              loop rest
        in
        loop set.natsigs

      (* We are not interested in 'No' answer. *)
      else Undetermined

  (* Canonize a list of natures (no doubles, ordered) *)
  let canonize nats =
    let nset = myfold nats
        (Setp.empty Stdlib.compare)
        (fun nset nat -> Setp.add nat nset)
    in
    Setp.elements nset
  
  let get_natsig elt =
    let nats = List.map mynature (myget_ids elt)
    and miss = mymissing_ids elt
    in

(*    Printf.printf "get_natsig: ids = %s\n%!" (List.fold_left (fun acu id -> acu ^ (id2s id) ^ ", ") "" (myget_ids elt)) ;
    Printf.printf "get_natsig: nats = %s\n%!" (List.fold_left (fun acu id -> acu ^ (nat2s id) ^ ", ") "" nats) ;
      Printf.printf "get_natsig: miss = %s\n%!" (List.fold_left (fun acu id -> acu ^ (nat2s id) ^ ", ") "" miss) ; *)
    
    { nats = canonize nats ;
      miss = canonize miss }
  
  (* Decrement the count for natsig in the association list sigs. *)
  let decrement elt sigs =
    let natsig = get_natsig elt in
    
    let rec loop acu = function
      | [] -> assert false (* The natsig was not found. *)
      | (ns, count) as pp :: rest ->
        if natsig = ns then
          if count = 1 then List.rev_append acu rest
          else (ns, count - 1) :: List.rev_append acu rest

        else loop (pp :: acu) rest
    in
    loop [] sigs

  let increment elt sigs =
    let natsig = get_natsig elt in

    let rec loop acu = function
      | [] -> (natsig, 1) :: acu
      | (ns, count) as pp :: rest ->
        if natsig = ns then (ns, count+1) :: List.rev_append acu rest
        else loop (pp :: acu) rest
    in
    loop [] sigs
  
  (*** Remove ***)
  let remove set elt =

    (* Check that the element is indeed in the set. *)
    let () =
    match mem ~no:false set elt with
    | No | Yes [] -> assert false
      
    | Undetermined -> raiserror Elt_not_found (Elt.elt2s elt)

    | Yes [ single ] ->
      if single == elt then () (* ok *)
      else raiserror Elt_not_found (Elt.elt2s elt)
  
    (* If Yes returns 2 elements, that means that the element is not as such in the set,
     * otherwise it would already have been merged.  *)
    | Yes all ->
      List.iter (fun x -> assert (x != elt)) all ;
      raiserror Elt_not_found (Elt.elt2s elt)

    in

    (* New index: remove all the ids pointing to it. *)
    let (_, new_index) = myfold (myget_ids elt) ([], set.index)
        begin fun (old_ids, index) id ->
          if IdMap.mem id index then
            begin
              assert (IdMap.find id index == elt) ;
              (id :: old_ids, IdMap.remove id index)
            end
          else
            begin
              assert (List.mem id old_ids) ; (* The id of the element is not in the index, it means it has already been removed. *)
              (old_ids, index)
            end
        end
    in
    
    { natsigs = decrement elt set.natsigs ;
      size = set.size - 1 ;
      index = new_index }
    
  (* Inserts a new element in the set. *)
  let insert set elt =
    let ids = myget_ids elt in
    
    let new_index = myfold ids set.index
        begin fun index id ->
          if IdMap.mem id index then
            begin
              (* The element is completely new. None of its ids can be in the set. *)
              Printf.printf "Meset.error: the element %s is supposed to be new. Why is its id (%s) already in the set?\n\n%!"
                (Elt.elt2s elt) (Elt.id2s id) ;
              assert false
            end;
          
          IdMap.add id elt index
        end
    in
    
    { natsigs = increment elt set.natsigs ;
      index = new_index ;
      size = set.size + 1 }

  (* Append, not duplicating elements. *)
  (* let rec append_sing l = function
    | [] -> l
    | x :: xs -> append_sing (if List.mem x l then l else x :: l) xs
  *)

  (* e1 = acu (new element), e2 = current elements of the set *)
  let mymerge ?(force=false) (e1, intact) e2 =
    let id1 = myget_ids e1
    and id2 = myget_ids e2 in
    
    let res = try Elt.merge ~force e1 e2 with e -> raiserror (Unmergeable e) (Elt.elt2s e1 ^ " and " ^ Elt.elt2s e2) in
    let ids = myget_ids res in
    
    if is_included id1 ids && is_included id2 ids then ()
    else
      (let disappeared = List.map Elt.id2s (list_sub (List.rev_append id1 id2) ids) in
       raiserror (Lostid disappeared) (Printf.sprintf " merge %s and %s gives %s" (Elt.elt2s e1) (Elt.elt2s e2) (Elt.elt2s res))) ;
    
    (* Intact iff e2 is intact (we do not need to check e1, which is not an element of the set. *)
    (res, intact && res == e2)     
  
  let rec merge_in_e ?exist ?(force=false) set elt =

    (* Check exist *)
    let member =
      match exist with
      | None -> mem ~no:false set elt
      | Some flag ->
        begin match mem ~no:true (* (not flag) *) set elt with
          | No -> if flag then raiserror Elt_not_found (Elt.elt2s elt) else No
          | (Yes _) as res -> if flag then res else raiserror Overwrite (Elt.elt2s elt)
          | Undetermined -> raiserror Partial (Elt.elt2s elt)
        end
    in

    match member with
    | No | Undetermined ->
      (* No other element has a common id. No merging occurs. *)
      (insert set elt, elt)
      
    | Yes all ->
      (* Other elements have a common id. We delete them all, merge them all, and insert the result. *)      
      let (newelt, intact) =
        try myfold all (elt, true) mymerge
        with Error ((Unmergeable _), _) when force ->
          (* Merge failed, but force is true. We force-merge the new element with all previous elements. *)
          let all = mymap all (fun e -> fst (mymerge ~force:true (e, false) elt)) in

          (* force:false  since we are basically merging elements of the set, none should be favored against another. *)
          let newelt = myfold all elt (fun el1 el2 -> fst (mymerge ~force:false (el1, false) el2)) in
          (newelt, false)
      in
      if intact then (set, newelt)
      else
        let newset = myfold all set remove in
        (* newelt may contain new ids which are not in elt, neither in the set ("combined" identifiers, that appear only by combining information).
         * As a consequence, elements mergeable with newelt may exist in the set, but not in 'all'.
         * Thus, we have to merge_in again. *)
        merge_in_e ~force newset newelt

  let merge_in ?exist ?force set elt = fst (merge_in_e ?exist ?force set elt)
  
  let fold ?comparekey ?compare set acu f =

    let scompare = match (compare, comparekey) with
      | (None, None) -> (fun e1 e2 -> Stdlib.compare (myget_ids e1) (myget_ids e2))
      | (Some c, None) -> c
      | (None, Some c) -> (fun e1 e2 -> Stdlib.compare (c e1, myget_ids e1) (c e2, myget_ids e2)) (* c is not necessarily injective *)
      | (Some _, Some _) -> failwith "Meset.fold: ?compare and ?comparekey are exclusive. Do not provide both."
    in

    (* Create an ordered set with each element appearing only once. *)
    let oset = IdMap.fold (fun _ elt os -> Setp.add elt os) set.index (Setp.empty scompare) in

    (* We must get exactly the size of 'set'. *)
    assert (Setp.cardinal oset = set.size) ;
    
    (* Fold over this set. *)
    Setp.fold f oset acu

  let filter set f = fold set empty (fun elt res -> if f elt then merge_in res elt else res)

  let lwt_fold ?comparekey ?compare set acu f =
    fold ?comparekey ?compare set (Lwt.return acu) (fun elt lacu -> Lwt.bind lacu (fun acu -> f elt acu))
                       
  let set2s ?comparekey ?compare ?(title="") set =
    let init = Printf.sprintf "===================  SET%s  =================" (if title="" then "" else ": " ^ title) in
    let all =
      fold ?comparekey ?compare set (init ^ "\n\n")
        begin fun elt acu ->
          acu ^ Printf.sprintf " - %s\n" (Elt.elt2s elt)
        end
    in
    all ^ String.make (String.length init) '='
end

