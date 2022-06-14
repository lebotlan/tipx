
type t =
  { table: (string, int) Assoc.t ;
    mutable count: int }

let mk () =
  { table = Assoc.create ~init:(fun _ -> 0) () ;
    count = 0 }

let set t l =
  List.iter (Assoc.incr t.table) l ;
  t.count <- t.count + 1 ;
  ()

let tos t =

  let w = Assoc.fold t.table 0 (fun key _ acu -> max acu (String.length key)) in
  
  let s = Assoc.fold t.table "" (fun key n acu -> acu ^ Printf.sprintf " %s : %5d (%2d%%)\n" (Common.tolen w key) n (n * 100 / t.count)) in
  s ^ Printf.sprintf "\n Total %d entries.\n\n" t.count
    
  
