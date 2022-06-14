module type CONVERTERS =
  sig
    type data
    val data_size: int
    val put_data: string -> int -> data -> unit
    val read_data: string -> int -> data
    val empty: data
  end

module type COMPACT_MAP =
  sig
    type data
    type t
    val create: cell_length:int -> length:int -> t
    val create_slots: slot_nb: int -> length:int -> t
    val get: t -> int -> data
    val set: t -> int -> data
    val cell_slots: t -> int -> int
    val get_slot: t -> int -> int -> data
    val set_slot: t -> int -> int -> data -> unit
    val add_slot: t -> int -> data -> unit
  end

let intsize = 4

(* Absolute positions in pointers every x cells. *)
let index_block_size = 20000

(* Future optimizations :
 *   - charger les entiers par 32 ou 64 bits
 *   - les pos relatives sur 16 bits (gain : 50%)
 *   - calcul de pos absolue optimisé en C.
 *)

let ug s i = Char.code (String.unsafe_get s i)
let us s i n = String.unsafe_set s i (Char.chr n)

(* À faire en C *)
let get_int s i = (ug s i) lsl 24 + 
		    (ug s (i+1)) lsl 16 + 
		    (ug s (i+2)) lsl 8 + 
		    (ug s (i+3))

let mask = 255

(* À faire en C *)
let set_int s i n =
  us s i (n lsr 24) ;
  us s (i+1) ((n lsr 16) land mask) ;
  us s (i+2) ((n lsr 8) land mask) ;
  us s (i+3) (n land mask) ;
  ()

module MkMap (Convert: CONVERTERS) =
  struct
    type data = Convert.data

    type layout = Fixed of int | Variable 

    type t =
	{ buf: string ;

	  length: int ;

	  layout: layout ;

	  max_slots: int ;
	  
	  (* Used only with variable layout. *)
	  mutable used_slots: int ;

	  (* For each index in [0..length] -> position in string (counted in slots) relative or absolute.
           * positions at indexes such that (index mod index_block_size = 0) are absolute, others are relative.
           * [ 0 (abs) ; 0 (rel) ; 1 (rel) ; 2 (rel) ; ... ]
	   * abs : 0 0 1 3 ...
           *  
	   * (for length, this is just outside the string). *)
	  pointers: string ; }

    let create_buf ~reset slots =
      let buf = String.create (slots * Convert.data_size) in
      if reset then
	for i = 0 to slots do
	  Convert.(put_data buf (i * data_size) empty) ;
	done ;
      buf

    let create ~reset ~cell_length ~length =
      let max_slots = length * cell_length in

      { buf = create_buf reset max_slots ;
	length ;
	layout = Fixed cell_length ;
	used_slots = 0 ;
	max_slots ;
	pointers = "" }

    let create_slots ~reset ~slot_nb ~length =
      { buf = create_buf reset slot_nb ;
	length ;
	layout = Variable ;
	max_slots = slot_nb ;
	used_slots = 0 ;
	pointers = String.make ((slot_nb + 1) * intsize)'\000' }


    let get arr index =
      assert (arr.layout = Fixed 1) ;
      Convert.(read_data arr.buf (index * data_size))

    let set arr index data =
      assert (arr.layout = Fixed 1) ;      
      Convert.(put_data arr.buf (index * data_size) data)

    open Convert

    let rec get_aux pointers mem_pos sum delta =
      if delta = 0 then sum
      else
	let new_pos = mem_pos + int_size in
	get_aux pointers new_pos (sum + get_int pointers new_pos) (delta - 1)

    let get_abs pointers pos =
      let delta = pos mod index_block_size in
      let start_pos = pos - delta in
      let mem_pos = start_pos * int_size in
      get_aux pointers mem_pos (get_int pointers mem_pos) delta


    let get_cell_slots arr index =
      match arr.layout with
      | Fixed n ->

	 let buf = arr.buf in

         (* i = count, pos = index in string buf. *)
	 let rec count i pos =
	   if i = n then n
	   else if read_data buf pos == empty then i
	   else count (i+1) (pos + data_size)
	 in
	 count 0 (index * n * data_size)

      | Variable ->

	 let pos = index * intsize in

	 if (pos+1) mod index_block_size = 0 then
	   begin
	     (* Pas de bol *)
	     let index_n = get_abs pointers pos
	     and index_np1 = get_abs pointers (pos + intsize) in
	     
	     let result = index_np1 - index_n in
	     assert (result >= 0 && result <= arr.max_slots) ;
	     result
	   end

	 else get_int pointers ((pos+1) * int_size)
	 
    let get_pos arr index slot =
      let pos =
	match arr.layout with
	| Fixed n -> index * n + slot
	| Variable -> get_abs arr.pointers (index * intsize)
      in
      assert (pos < arr.max_slots) ;
      pos

    let get_slot arr index slot =
      let pos = get_pos arr index slot in
      read_data arr.buf (pos * data_size)

    let set_slot arr index slot data =
      let pos = get_pos arr index slot in
      put_data arr.buf (pos * data_size) data

    let add_slot arr index data =
      let pos =
	match arr.layout with
	| Fixed n ->
	   let next = get_cell_slots arr index in
	   assert (next < n) ;
	   index * n + next

	| Variable ->
	   arr.used_slots <- arr.used_slots + 1 ;
	   assert (arr.used_slots <= arr.max_slots) ;
	   
      in
      put_data arr.buf (pos * data_size) data

  end
