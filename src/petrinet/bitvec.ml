(* TODO PERF:
 *    - use unsafe versions of Bytes.get Bytes.set
 *    - write pick_loop in C
 *    - write add, remove, contains in C *)


(* Numbered from left to right : b7, b6, b5, ... b0, b15, b14, ... b8, b23, ... *)
type bitvec = bytes

type t = bitvec

let char0 = Char.chr 0

let init size = Bytes.make ((size + 7)/ 8) char0

let clone = Bytes.copy

let tos ?(max=40) bv =
  (* FIXME Seq.take *)
  let result = Seq.fold_left (fun acu b -> acu ^ Printf.sprintf "%02x" (Char.code b)) "" (Bytes.to_seq bv) in
  if String.length result > max then String.sub result 0 max ^ "..." else result

(* Compute the position of the bit in the bytes array: index & mask *)
let get_index n = n lsr 3
let get_mask n = 1 lsl (n mod 8)

(* @noalloc *)
let set bv i =
  let index = get_index i
  and mask = get_mask i in

  let vval = Bytes.get_uint8 bv index in  
  Bytes.set_uint8 bv index (vval lor mask)

(* @noalloc *)
let unset bv i =
  let index = get_index i
  and mask = get_mask i in

  let vval = Bytes.get_uint8 bv index in  
  Bytes.set_uint8 bv index (vval land (lnot mask))

(* @noalloc *)
let get bv i =
  let index = get_index i
  and mask = get_mask i in

  let vval = Bytes.get_uint8 bv index in
  (vval land mask) lsr (i mod 8)

let equal = Bytes.equal
let cmp = Bytes.compare

(* Find the rightmost bit equal to 1 
   @noalloc *)
let rec pick_bit index u8 mask = if u8 land mask = 0 then pick_bit (index+1) u8 (mask lsl 1) else index

(* @noalloc *)
let rec pick_loop len bv start_index index =
  let u8 = Bytes.get_uint8 bv index in
  if u8 = 0 then
    let next_index = (index + 1) mod len in
    
    if next_index = start_index then -1
    else pick_loop len bv start_index next_index

  else
    (* Hit ! *)
    pick_bit (index * 8) u8 1

(* @noalloc *)
let pick bv start =

  let len = Bytes.length bv in
  
  let index = (start / 8) mod len in

  (* Mask for the first byte test *)
  let mask = 0xFF lsl (start mod 8) in

  let first = (Bytes.get_uint8 bv index) land mask in
  if first = 0 then
    let index1 = (index + 1) mod len in
    pick_loop len bv index1 index1
  else
    (* Assumed suboptimal *)
    pick_bit (index * 8) first 1

