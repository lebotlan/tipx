open Common

type 'a serializer = 'a -> Bitstring.bitstring

let (@+) a b = Bitstring.concat [a ; b]

let bobool x = [%bitstring {| x : 1 |}]
let boi x = [%bitstring {| Int64.of_int x : 63 |}]

(* bitstring of Byte *)
let boby x = [%bitstring {| x : 8 |}]

let bof x = [%bitstring {| Int64.bits_of_float x : 64 |}]


(* Shortcuts *)
let bos = Bitstring.bitstring_of_string
let nul = Bitstring.empty_bitstring
let concat = Bitstring.concat
let blength = Bitstring.bitstring_length


(* Adaptative integer : 
 * [0-255] : 9 bits
 * [256-63bits] : 64 bits *)
let boa x =
  if 0 <= x && x <= 255 then concat [bobool false ; boby x]
  else concat [bobool true ; boi x]

(* Sized value. *)
let bobits bits = concat [ boa (blength bits) ; bits ]

let bostring s = bobits (Bitstring.bitstring_of_string s)

let bofixedstring n s = [%bitstring {| s : n * 8 : string |}]

(* Markers. *)
let list_mark = 125
let endlist_mark = 126
let tuple_mark = 89
let endtuple_mark = 90

(* Serialize a list *)
let bolist map l = concat ( [boby list_mark ; boa (List.length l)] @ mymap l map @ [boby endlist_mark] )

let boption map = function
  | None -> bobool false
  | Some v -> concat [bobool true ; map v]

(* Serialize a tuple. *)
let botuple l = concat ( (boby tuple_mark) :: l @ [boby endtuple_mark] )

let bopair map1 map2 (a,b) = botuple [map1 a ; map2 b]
let botriple map1 map2 map3 (a,b,c) = botuple [map1 a ; map2 b ; map3 c]

