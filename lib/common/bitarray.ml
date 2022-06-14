
type bitarray = 
  { mutable buffer: bytes ;

    (* Index of the last cell. *)
    mutable last: int }

type t = bitarray

let create n = 
  { buffer = Bytes.make ((n+7) / 8) '\000' ;
    last = 0 }

let clone b =
  { buffer = Bytes.copy b.buffer ;
    last = b.last }

let get_offset_and_mask n = 
  let offset = n lsr 3
  and mask_offset = n mod 8 in
  let mask = 1 lsl mask_offset in
  (offset, mask)

let get sr i =
  if i > sr.last then false
  else
    let (offset, mask) = get_offset_and_mask i in
    0 <> Char.code (Bytes.get sr.buffer offset) land mask

let set sr i value =
  assert (i >= 0) ;
  let len = Bytes.length sr.buffer in
  if i >= 8 * len then
    begin
      let newstring = Bytes.make (max (1 + i/8) (2 * len)) '\000' in
      (*      Printf.printf "blit : %d 0 %d 0 %d\n%!" (String.length !sr) (String.length newstring) len ; *)
      Bytes.blit sr.buffer 0 newstring 0 len ;
      sr.buffer <- newstring ;
    end ;

  let (offset, mask) = get_offset_and_mask i in

  let cell = Char.code (Bytes.get sr.buffer offset) in

  let newcell =
    if value then cell lor mask
    else cell land (mask lxor 0xFF)
  in

  Bytes.set sr.buffer offset (Char.chr newcell) ;

  if i > sr.last then sr.last <- i ;
  ()

(* Fold over bits of a byte *)
let rec fold_bits last pos acu f byte =
  if byte = 0 || pos > last then acu
  else
    let acu' = if byte land 1 = 1 then f pos acu else acu in
    fold_bits last (pos + 1) acu' f (byte lsr 1)

(* Fold over bits of a string *)
let rec fold_aux map_byte last s acu f len pos =
  if pos >= len then acu
  else
    let acu' = fold_bits last (pos lsl 3) acu f (map_byte (Char.code (Bytes.get s pos))) in
    fold_aux map_byte last s acu' f len (pos + 1)

let fold ?(inv=false) sr acu f =
  let map_byte = if inv then fun x -> x lxor 255 else fun x -> x in
  let acu' = fold_aux map_byte sr.last sr.buffer acu f (Bytes.length sr.buffer) 0 in
  acu'

let iter ?inv sr f = fold ?inv sr () (fun n () -> f n)

let count sr = fold sr 0 (fun _ c -> c + 1)

let last sr = fold sr (-1) (fun pos acu -> max pos acu)

let indices sr =
  let size = count sr in
  let res = Array.make size 0 in
  let i = ref 0 in
  iter sr (fun j -> res.(!i) <- j ; incr i) ;
  assert (!i = size) ;
  res


type bitmatrix = 
  { cols: int ;
    arr: bitarray ExtArray.t }

let dummyrow = create 0

let mcreate ~rows ~cols = 
  { cols ;
    arr = ExtArray.create rows dummyrow }

let get_row bm row =
  let r = ExtArray.get bm.arr row in
  if r == dummyrow then
    begin
      let newrow = create bm.cols in
      ExtArray.set bm.arr row newrow ;
      newrow
    end
  else r

let mget bm row col = get (get_row bm row) col

let mset bm row col v = set (get_row bm row) col v

let count_row bm row = count (get_row bm row)


type state =
  { acu: string ;
    interval: (int * int) option }

let init_state =
  { acu = "" ;
    interval = None }

let prettytos ?inv sr =

  let append n st =
    match st.interval with
    | None -> { acu = st.acu ; interval = Some (n,n) }
    | Some (start, last) ->
      if n = last + 1 then
        (* We are still in a sequence *)
        { acu = st.acu ; interval = Some (start, n) }            
      else
        (* The sequence is over *)
        { acu = st.acu ^ (if st.acu = "" then "" else ", ") ^
                (if last = start then string_of_int start
                 else Printf.sprintf "%d-%d" start last) ;
          interval = Some (n, n) }
  in
  
  let result = fold ?inv sr init_state append in

  (* A dummy element to flush the last interval. *)
  (append (-10) result).acu
