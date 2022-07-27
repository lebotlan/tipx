type format = M8 | M16 | M32

type intarray =
  { format: format ;

    (* Maximal value of the format, excluded. *)
    maxv: int ;
    tab: Bytes.t }
    
type t = intarray

let char0 = Char.chr 0

(* @noalloc *)
let sizeof = function
  | M8 -> 1
  | M16 -> 2
  | M32 -> 4

let create fmt n =

  let sof = sizeof fmt in
  
  { format = fmt ;
    maxv = 1 lsl (8 * sof) ;
    tab = Bytes.make (n * sof) char0 }

let clone ar =
  { format = ar.format ;
    maxv = ar.maxv ;
    tab = Bytes.copy ar.tab }

let clear ar = Bytes.fill ar.tab 0 (Bytes.length ar.tab) '\x00'

(* @noalloc *)
let get ar i =
  match ar.format with
  | M8 -> Bytes.get_uint8 ar.tab i
  | M16 -> Bytes.get_uint16_be ar.tab (i*2)
  | M32 ->
    let i4 = i * 4 in
    let hi = Bytes.get_uint16_be ar.tab i4
    and lo = Bytes.get_uint16_be ar.tab (i4+2) in

    hi lsl 16 + lo

let mask16 = 0xffff

(* @noalloc *)
let raw_set ar i v =
  assert (v < ar.maxv) ;
  
  match ar.format with
  | M8 ->  Bytes.set_uint8 ar.tab i v
  | M16 -> Bytes.set_uint16_be ar.tab (i*2) v
  | M32 ->
    let i4 = i * 4 in
    Bytes.set_uint16_be ar.tab i4 (v lsr 16) ;
    Bytes.set_uint16_be ar.tab (i4+2) (v land mask16) ;
    ()

(* @noalloc *)
let size ar = Bytes.length ar.tab / sizeof ar.format

(* @noalloc *)
let rec loop_cmp ar1 ar2 i size =
  if i >= size then 0
  else
    let v1 = get ar1 i
    and v2 = get ar2 i in

    if v1 = v2 then loop_cmp ar1 ar2 (i+1) size
    else Stdlib.compare v1 v2

(* @noalloc *)
let cmp ar1 ar2 =
  let size1 = size ar1
  and size2 = size ar2 in

  (* if ar1.format = ar2.format then Bytes.compare ar1.tab ar2.tab *) (* Unsure Bytes.compare is equivalent to loop_cmp *)
  if size1 = size2 then loop_cmp ar1 ar2 0 size1
  else Stdlib.compare size1 size2

exception Marking_overflow

let format2s = function
  | M8 -> "8b"
  | M16 -> "16b"
  | M32 -> "32b"  

let reformat ar =

  let n = size ar in

  let ar2 =
    match ar.format with
    | M32 -> raise Marking_overflow
    | M16 -> create M32 n
    | M8  -> create M16 n
  in

  Printf.printf "\n [Intarray] Note: reformating array from %s to %s (size %d)\n%!" (format2s ar.format) (format2s ar2.format) n ; 
  
  for i = 0 to n-1 do
    raw_set ar2 i (get ar i) ;
  done ;

  ar2
    

(* @noalloc most of the time
 * @alloc if the array needs to be reformated (overflows the current format). *)
let rec set ar i x =
  if x < ar.maxv then (raw_set ar i x ; ar)
  else set (reformat ar) i x
  

(* @noalloc most of the time *)
let add ar i x = set ar i (x + get ar i)
    
let rec tos_loop acu ar max size i =
  if i >= size then acu
  else if i >= max then acu ^ "..."
  else tos_loop (acu ^ " " ^ string_of_int (get ar i)) ar max size (i+1)
  
let tos ?(max=40) ar =
  tos_loop ("[" ^ format2s ar.format ^ "] ") ar max (size ar) 0
