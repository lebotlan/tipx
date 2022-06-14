open Common


type 'a deserializer = Bitstring.bitstring -> ('a * Bitstring.bitstring)

(* These functions are the reversed functions of serialize.ml *)
let boolob bits = bitmatch bits with { x : 1 ; other : -1 : bitstring } -> (x, other)
let iob bits    = bitmatch bits with { x : 31 : int ; other : -1 : bitstring } -> (x, other)
let byob bits   = bitmatch bits with { x : 8 ; other : -1 : bitstring } -> (x, other)

let fob bits    = bitmatch bits with { x : 64 : bind(Int64.float_of_bits x) ; other : -1 : bitstring } -> (x, other)
let wfob bits   = bitmatch bits with { x : 32 : bind(Int32.float_of_bits x) ; other : -1 : bitstring } -> (x, other)

(* Gets the first n bits and the rest. *)
let cut bits n = bitmatch bits with { piece1 : n : bitstring ; piece2 : -1 : bitstring } -> (piece1, piece2)

(* Adaptive integer *)
let aob bits = 
  let (flag, other) = boolob bits in
  if flag then iob other else byob other

let bitsob bits =
  let (size, other) = aob bits in
  cut other size

let mapob des f bits =
  let (v, rest) = des bits in
  (f v, rest)

let stringob = mapob bitsob Bitstring.string_of_bitstring


let fixedstringob n bits = bitmatch bits with { s : n * 8 : string ; other : -1 : bitstring } -> (s, other)

(* Deserialize a list *)
let rec listob_aux len map acu bits =
  if len = 0 then (List.rev acu, bits)
  else
    let (v, rest) = map bits in
    listob_aux (len-1) map (v :: acu) rest

let listob map bits =  
  let (readmark, other1) = byob bits in
  let (len, other2) = aob other1 in

  (* Bad marker. This is not a list. *)
  assert (readmark = Serialize.list_mark) ;

  let (l, other3) = listob_aux len map [] other2 in
  let (readendmark, other4) = byob other3 in
  
  (* Bad endmark. Something went wrong. *)
  assert (readendmark = Serialize.endlist_mark) ;

  (l, other4)

let optionob map bits =
  let (flag, other1) = boolob bits in
  if flag then
    let (value, other2) = map other1 in
    (Some value, other2)
  else (None, other1)

(* Deserialize tuples *)
let rec tupleob_aux bits acu = function
  | [] -> (List.rev acu, bits)
  | map1 :: maps ->
      let (v, other) = map1 bits in
      tupleob_aux other (v :: acu) maps

(* mapl is a list of functions to be applied consecutively. *)
let tupleob mapl bits =
  let (readmark, other1) = byob bits in
  
  (* Bad marker. This is not a tuple. *)
  assert (readmark = Serialize.tuple_mark) ;

  let (l, other2) = tupleob_aux other1 [] mapl in
  
  let (readendmark, other3) = byob other2 in
  
  (* Bad endmark. Something went wrong. *)
  assert (readendmark = Serialize.endtuple_mark) ;

  (l, other3)

(* Injection of different types into a single common type. 
 * 5 should be enough for now. *)
type ('a, 'b, 'c, 'd, 'e) inject = A1 of 'a | A2 of 'b | A3 of 'c | A4 of 'd | A5 of 'e

let a1 x = A1 x
let a2 x = A2 x
let a3 x = A3 x
let a4 x = A4 x
let a5 x = A5 x

let pairob map1 map2 bits =
  let mapl = [mapob map1 a1 ; mapob map2 a2] in
  let (vl, rest) = tupleob mapl bits in
  match vl with
  | [ A1 v1 ; A2 v2 ] -> ((v1, v2), rest)
  | _ -> assert false (* Impossible *)
