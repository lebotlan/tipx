open Net
open Intarray

(* TODO PERF : Sparse ? *)

type marking =
  (* | Sparse of ...  *)
  | Bits of Bitvec.t
  | Dense of Intarray.t

type t = marking

let init ?(safe=false) net =
  let size = nb_pl net in

  if safe then Bits (Bitvec.init size)
  else
    Dense (Intarray.create M8 size)

let clone = function
  | Dense ar -> Dense (Intarray.clone ar)
  | Bits b -> Bits (Bitvec.clone b)

let get m pl = match m with
  | Dense ar -> Intarray.get ar pl.pl_id
  | Bits bv -> Bitvec.get bv pl.pl_id

let add m pl x =
  if x = 0 then m
  else match m with
    | Dense ar ->
      let ar2 = Intarray.add ar pl.pl_id x in
      if ar == ar2 then m
      else Dense ar2

    | Bits bv ->
      begin match x, Bitvec.get bv pl.pl_id with
        | 1, 0 -> Bitvec.set bv pl.pl_id ; m
        | -1, 1 -> Bitvec.unset bv pl.pl_id ; m
        | _ -> raise Intarray.Marking_overflow
      end

let compare m1 m2 = match m1,m2 with
  | Bits _, Dense _
  | Dense _, Bits _ -> assert false

  | Bits b1, Bits b2 -> Bitvec.cmp b1 b2
  | Dense ar1, Dense ar2 -> Intarray.cmp ar1 ar2


let tos ?max = function
  | Bits bv -> Bitvec.tos ?max bv
  | Dense ar -> Intarray.tos ?max ar
                  
