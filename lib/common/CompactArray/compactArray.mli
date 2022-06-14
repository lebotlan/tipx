(* Array, in a memory-compact layout.
 * (Used to represent very large automata.) *)

module type CONVERTERS =
  sig
    type data

    (* Number of bytes. *)
    val data_size: int

    (* Put the expected data at the given location. Should use at most data_size bytes. *)
    val put_data: string -> int -> data -> unit

    val read_data: string -> int -> data

    (* A special value meaning empty slot. 
     * read_data must return ==empty if data is empty (not only =empty). *)
    val empty: data

  end

module type COMPACT_MAP =
  sig
    type data
    type t

    (* Creates a new array. 
     * cell_length indicates the available number of data slots in each cell (immediately allocated) 
     * reset: true => put empty data in every slot. *)
    val create: reset:bool -> cell_length:int -> length:int -> t

    (* Creates a new array. (An upper bound of) the total number of slots is given (immediately allocated). *)
    val create_slots: reset:bool -> slot_nb: int -> length:int -> t

    (* cell indexes and slot indexes start at 0. *)

    (* If cell_length is 1, use these: *)
    val get: t -> int -> data
    val set: t -> int -> data -> unit

    (*** Otherwise, use these: ***)

    (* Number of slots containing a non-empty value in cell. *)
    val get_cell_slots: t -> int -> int

    (* get_slot cell_index slot_index *)
    val get_slot: t -> int -> int -> data

    (* data must not be empty. *)
    val set_slot: t -> int -> int -> data -> unit

    (* Adds a new slot (or use the first empty one in cell). *)
    val add_slot: t -> int -> data -> unit

    (* Fold plus efficace !!! *)
  end

module MkMap: functor (Convert: CONVERTERS) -> COMPACT_MAP with type data = Convert.data
