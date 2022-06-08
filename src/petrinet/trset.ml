open Net

type trset = Bitvec.t

let init net = Bitvec.init (nb_tr net)

let clone = Bitvec.clone

let add    trs tr   = Bitvec.set   trs tr.tr_id
let remove trs tr   = Bitvec.unset trs tr.tr_id
let contains trs tr = Bitvec.get   trs tr.tr_id <> 0

let equal = Bitvec.equal

let pick net trs ~start =
  let tr_id = Bitvec.pick trs start in
  if tr_id < 0 then null_tr else get_tr net tr_id

