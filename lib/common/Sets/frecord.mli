(***  Immutable records with sorts of first-class fields. ***)

(* What you finally get *)
module type RECORD =
sig
  (* Your customized record type *)
  type t

  type 'a field_type
      
  type 'a field

  (* The default record, where all fields have their default values. *)
  val default: t

  val set: t -> 'a field -> 'a -> t
  
  val get: t -> 'a field -> 'a

  (* All fields of t, presented with an existential type. *)
  type a_field = Field: 'a field_type * 'a field -> a_field
    
  val fields: a_field list

  val tos: t -> string
end


(* The module you use to build the record specification. *)
module type RECORD_SPEC =
sig
  type 'a field_type

  type 'a field

  (* add_field ~name ftype default_value
   *   The name is for pretty-printing only (it does not matter if two fields have the same name).
   *   You cannot add more fields once Make has been invoked. *)
  val add_field: name:string -> 'a field_type -> 'a -> 'a field

  module Make : functor () -> RECORD with type 'a field_type = 'a field_type and type 'a field = 'a field
end


(* If you need to apply generic operations on all fields,
 *   (which is likely the case, otherwise you would not need this library),
 * you will need to store some polymorphic information in the fields themselves. 
 * Hence, the field types are parameterized by a functor. *)
module type FIELD_TYPE =
sig
  type 'a field_type
  val tos: 'a field_type -> 'a -> string
end

module Make_Spec : functor (FT: FIELD_TYPE) -> RECORD_SPEC with type 'a field_type = 'a FT.field_type
