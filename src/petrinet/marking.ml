open Net
open Intarray

(* TODO PERF : Sparse ? *)

type repr =
  
  (* | Sparse of ...  *)
  | Bits of Bitvec.t
  | Dense of Intarray.t


type marking =
  { clone: unit -> marking ;
    get: pl_id -> int ;
    add: pl_id -> int -> marking ;
    tos: ?max:int -> unit -> string ;
    repr: repr }

(* type marking = repr *)

type t = marking


(* @noalloc *)
let compare m1 m2 = match m1,m2 with
  | Bits _, Dense _
  | Dense _, Bits _ -> assert false

  | Bits b1, Bits b2 -> Bitvec.cmp b1 b2
  | Dense ar1, Dense ar2 -> Intarray.cmp ar1 ar2

let rec make repr = match repr with
  | Bits bv ->

    let rec mm =    
      { clone = (fun () -> make (Bits (Bitvec.clone bv))) ;
        get = Bitvec.get bv ;
        add ;
        tos = (fun ?max () -> Bitvec.tos ?max bv) ;
        repr }

    and add pl_id x =
      if x = 0 then mm
      else
        begin match x, Bitvec.get bv pl_id with
          | 1, 0 -> Bitvec.set bv pl_id ; mm
          | -1, 1 -> Bitvec.unset bv pl_id ; mm
          | _ -> raise Intarray.Marking_overflow
        end
    in

    mm

  | Dense ar ->
    let rec mm =
      { clone = (fun () -> make (Dense (Intarray.clone ar))) ;
        get = Intarray.get ar ;
        add ;
        tos = (fun ?max () -> Intarray.tos ?max ar) ;
        repr }

    and add pl_id x =
      if x = 0 then mm
      else
        let ar2 = Intarray.add ar pl_id x in
        if ar == ar2 then mm
        else make (Dense ar2)

    in

    mm

let init ?(safe=false) net =
  let size = nb_pl net in

  if safe then make (Bits (Bitvec.init size))
  else make (Dense (Intarray.create M8 size))


let clone m = m.clone ()

let get m pl_id = m.get pl_id

let add m pl_id x = m.add pl_id x

let tos ?max m = m.tos ?max ()

let compare m1 m2 = compare m1.repr m2.repr
