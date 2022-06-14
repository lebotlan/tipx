module type RECORD =
sig
  type t
  type 'a field_type      
  type 'a field
      
  val default: t
  val set: t -> 'a field -> 'a -> t  
  val get: t -> 'a field -> 'a
    
  type a_field = Field: 'a field_type * 'a field -> a_field
    
  val fields: a_field list
  val tos: t -> string
end


module type RECORD_SPEC =
sig
  type 'a field_type
  type 'a field
  val add_field: name:string -> 'a field_type -> 'a -> 'a field

  module Make : functor () -> RECORD with type 'a field_type = 'a field_type and type 'a field = 'a field
end

module type FIELD_TYPE =
sig
  type 'a field_type
  val tos: 'a field_type -> 'a -> string
end

module Make_Spec = functor (FT: FIELD_TYPE) ->
struct
  type 'a field_type = 'a FT.field_type

  type 'a field =
    { mutable read: 'a ; (* Placeholder to read the field value. (Trick borrowed from Lwt keys.) *)
      pos: int ;
      def: 'a ;
      name: string }

  type 'a fd = 'a field

  type a_fd = Field: 'a field_type * 'a fd -> a_fd
    
  let nb = ref 0
  
  let closed = ref false

  let fields = ref []
  
  let add_field ~name ft v =
    if !closed then failwith "Frecord.add_field: spec is closed (Make has already been invoked)"
    else
      begin
        let field =
          { read = v ;
            pos = !nb ;
            def = v ;
            name }
        in
        fields := Field (ft, field) :: !fields ;
        incr nb ;
        field
      end

  module Make = functor () ->
  struct
    let () = closed := true

    let fields = List.rev !fields        
    
    type t = (unit -> unit) array
        
    type 'a field_type = 'a FT.field_type
    type 'a field = 'a fd

    let default = Array.of_list (List.map (fun (Field (_, fd)) -> (fun () -> fd.read <- fd.def)) fields)

    let set r fd v =
      let ar = Array.copy r in
      ar.(fd.pos) <- (fun () -> fd.read <- v) ;
      ar
                     
    let get r fd =
      r.(fd.pos) () ;
      fd.read

    type a_field = a_fd = Field: 'a field_type * 'a field -> a_field

    let field2s t (Field (ft, fd)) = fd.name ^ ": " ^ FT.tos ft (get t fd)
    
    let tos t = Common.sep (field2s t) " ; " fields
  end
end
