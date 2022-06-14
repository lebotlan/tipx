
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

    method matches s = if self#is_empty then true else Pcre.pmatch ~rex:self#get_global s
      
    method associated s =
      Assoc.fold assoc_str [] (fun value regs acu -> if Pcre.pmatch ~rex:(self#get_rex value regs) s then value :: acu else acu)
  end


type 'a union_regexp = 'a union

type 'a t = 'a union_regexp

let empty flags = new union flags

let is_empty ureg = ureg#is_empty

let add ureg value regs = ureg#add value regs

let union ureg1 ureg2 = ureg1#union ureg2

let create flags bindings = List.fold_left (fun ureg (v, regs) -> ureg#add v regs) (empty flags) bindings

let matches ureg s = ureg#matches s

let associated ureg s = ureg#associated s

let get_regexps ureg = ureg#all_regexps

