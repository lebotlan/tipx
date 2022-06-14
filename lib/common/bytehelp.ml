let mask8 = 0xff

let dump_int ?(size=8) bytes offset n =
  (*  Printf.printf "Dump_int size=%d, offset=%d n=%d\n%!" size offset n ; *)
  let rec loop o k n =
    (*    Printf.printf "  >> o = %d, k = %d, n = %d\n%!" o k n ; *)
    if k <= 0 then (assert (n=0) ; ())
    else
      begin
        Bytes.set bytes o (Char.chr (n land mask8)) ;
        loop (o+1) (k-1) (n lsr 8)
      end    
  in
  loop offset size n

let read_int ?(size=8) bytes offset =
  let rec loop acu o k =
    if k <= 0 then acu
    else
      let byt = String.get bytes o in
      loop ((acu lsl 8) + Char.code byt) (o-1) (k-1)
  in
  loop 0 (offset+size-1) size
