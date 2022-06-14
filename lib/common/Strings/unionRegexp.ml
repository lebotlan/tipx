type 'a builder = string -> Pcre.substrings -> 'a

type 'a union_regexp =
  { flags: Pcre.icflag ;

    (* Global regexp, union of all its component regexps. *)
    global: Pcre.regexp ;

    filters: (string list * 'a builder) list ;
    cfilters: (Pcre.regexp * 'a builder) list ;
  }

type 'a t = 'a union_regexp

let empty flags =
  { flags = Pcre.cflags flags ;
    global = Pcre.regexp "dummy" ;
    filters = [] ;
    cfilters = [] }

let mk_global iflags filters =
  (* Flatten *)
  let all = List.fold_left (fun acu (pl, _) -> List.rev_append pl acu) [] filters in
  Pcre.regexp_or ~iflags all  

let addb ur mk regs =
  let filters = (regs, mk) :: ur.filters
  and cfilters = (Pcre.regexp_or ~iflags:ur.flags regs, mk) :: ur.cfilters in
  
  { flags = ur.flags ;
    global = mk_global ur.flags filters ;
    filters ;
    cfilters }

let add ur v regs = addb ur (fun _ _ -> v) regs

let union ur1 ur2 =
  let filters = List.rev_append ur1.filters ur2.filters
  and cfilters = List.rev_append ur1.cfilters ur2.cfilters in
  
  { flags = ur1.flags ;
    global = mk_global ur1.flags filters ;
    filters ;
    cfilters }

let create flags bindings = List.fold_left (fun ureg (regs, v) -> add ureg v regs) (empty flags) bindings
let createb flags bindings = List.fold_left (fun ureg (regs, bd) -> addb ureg bd regs) (empty flags) bindings
let createu flags l = create flags [l , ()]

let matches urex s = urex.filters <> [] && Pcre.pmatch ~rex:urex.global s

let (+>) x l = if List.mem x l then l else x :: l

let associated urex s =
  let check_cfilter acu (rex, bd) =
    try bd s (Pcre.exec ~rex s) +> acu
    with Not_found -> acu
  in
  if matches urex s then List.fold_left check_cfilter [] urex.cfilters    
  else []
  
let get_regexps urex = List.fold_left (fun acu (sl, _) -> List.rev_append sl acu) [] urex.filters

let map ur f =

  let bdmap bd = fun s sub -> f (bd s sub) in
  
  { flags = ur.flags ;
    global = ur.global ;
    filters = List.map (fun (sl, bd) -> (sl, bdmap bd)) ur.filters ;
    cfilters = List.map (fun (rx, bd) -> (rx, bdmap bd)) ur.cfilters }


(*


let uniq_cons _ s l = if List.mem s l then l else s :: l
let uniq_append _ l1 l2 = List.fold_left (fun acu s -> uniq_cons () s acu) l2 l1

class ['a] union flags =
  object(self:'selftype)

    (* This one is evaluated when needed. *)
    val mutable global_regexp = None

    (* Associates values to list of (string) regexps. *)     
    val assoc_str = Assoc.create ~init:(fun _ -> []) ()

    (* Associates values to compiled regexps. *)
    val assoc_rex = Assoc.create ~init:(fun _ -> None) ()

    method assoc_str = assoc_str
    method assoc_rex = assoc_rex

    method is_empty = Assoc.size assoc_str = 0

    method add (value:'a) regs =

      let assoc_str = Assoc.copy assoc_str 
      and assoc_rex = Assoc.copy assoc_rex in

      Assoc.update assoc_str value (fun l -> uniq_append () regs l) ;
      Assoc.update assoc_rex value (fun _ -> None) ;

      {< global_regexp = None ;
         assoc_str = assoc_str ;
         assoc_rex = assoc_rex >}

    method union (ureg:'selftype) =

      let assoc_str = Assoc.copy assoc_str 
      and assoc_rex = Assoc.copy assoc_rex in

      Assoc.merge assoc_str uniq_append ~import_from:ureg#assoc_str ;
      Assoc.merge assoc_rex (fun _ _ _ -> None) ~import_from:ureg#assoc_rex ;

      {< global_regexp = None ;
         assoc_str = assoc_str ;
         assoc_rex = assoc_rex >}

    method all_regexps = Assoc.fold assoc_str [] uniq_append

    method get_global =
      match global_regexp with
      | None ->
        let g = Pcre.regexp_or ~flags self#all_regexps in
        global_regexp <- Some g ;
        g

      | Some g -> g

    method get_rex value regs =
      match Assoc.get assoc_rex value with
      | Some r -> r
      | None ->
        let or_reg = Pcre.regexp_or ~flags (Assoc.get assoc_str value) in
        Assoc.update assoc_rex value (fun _ -> Some or_reg) ;
        or_reg

    method associated s =
      Assoc.fold assoc_str [] (fun value regs acu -> if Pcre.pmatch ~rex:(self#get_rex value regs) s then value :: acu else acu)
  end


*)
       
