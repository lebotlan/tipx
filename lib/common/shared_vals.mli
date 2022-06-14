
(* Shared values are values for which = and == coincide. 
 * That means, equal values are shared in memory. 
 * 
 * Beware that values must not contain mutable data !!!
 *)

module Make : functor (V : Hashtbl.HashedType) ->
  sig

    (* Type of a value *)
    type v = V.t
    
    (* Type of a shared value *)
    type sh

    (* Records a value *)
    val share: v -> sh

    (* Get the associated value *)
    val tov: sh -> v

    (* Check for equality (using ==) *)
    val sh_equals: sh -> sh -> bool

    (* Uses Pervasives.compare *)				  
    val compare: sh -> sh -> int

    (* Get an identifier (numbered from 0 to n-1) *)
    val id: sh -> int

    (* Number of shared values (= n) *)
    val size: unit -> int

    (* Convenient function: shares a value and returns an id. *)
    val share_id: v -> int

    (* Convenient function: shares a value and returns the possibly shared value. *)
    val share_v: v -> v

    (* Iterate on all shared values *)
    val iter: (v -> sh -> unit) -> unit

    (* Convenient function, the extra argument is the id *)
    val iteri: (int -> v -> sh -> unit) -> unit

    (* Compare two lists of shared values. Returns true if they are equal, in the same order. *)
    val list_equals: sh list -> sh list -> bool

    (* Like list_equals, where shared values have been converted to v. *)
    val list_equals_v: v list -> v list -> bool


    val stats: unitname:string -> string

  end

module HString : Hashtbl.HashedType with type t = string
