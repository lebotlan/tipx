open Petrinet

(* Generic test of array-like modules *)

type 'a testee =
  { init: int -> 'a ;
    get: 'a -> int -> int ;
    set: 'a -> int -> int -> 'a ;
    clone: 'a -> 'a ;
    cmp: 'a -> 'a -> int ;
    tos: ?max:int -> 'a -> string ;
    cp: int array -> int -> int ; (* Compute postion in array reference *)
    setv: int list }

type 'a tester =
  { testee: 'a testee ;
    size: int ;
    iterations: int }

let mk_test ?(size=20) ?(iterations=25000) testee = { testee ; size ; iterations }

(* Arrays are re-ordered to be compatible with Stdlib.compare on Bitvec. *)

let compute_pos ar i =
  let _n = Array.length ar in
  (i land 0xfffffff8) + ((7 - i) land 0x7)

let get_tab cp ar i = ar.(cp ar i)
let set_tab cp ar i x = ar.(cp ar i) <- x

let atos ar =
  let result = Seq.fold_left (fun acu b -> acu ^ " " ^ Printf.sprintf "%d" b) "" (Array.to_seq ar) in
  if String.length result > 40 then String.sub result 0 40 ^ "..." else result


let check_equal cp tos it t data aref =
  for i = 0 to t.size - 1 do
    let v1 = t.testee.get data i
    and v2 = get_tab cp aref i in

    if v1 <> v2 then (Printf.printf "\n\n  🞮  Testee is not equal to reference array at iteration #%d :\n\n  Testee equals %s\n\n  Reference array is %s\n\n%!" it (tos data) (atos aref) ; assert false)
  done ;
  ()


let run_test t =

  Printf.printf "\n  🏁  Testing size = %d\n\n%!" t.size ;

  let () = Random.init t.size in

  let data = ref (t.testee.init t.size) in
  let dataclone = ref (t.testee.clone !data) in

  let aref = Array.make (t.size+8) 0 in
  let aclone = ref (Array.copy aref) in

  for i = 0 to t.iterations do

    (* Check equal, compare *)

    check_equal t.testee.cp t.testee.tos i t !data aref ;

    let cmp1 = t.testee.cmp !dataclone !data
    and cmp2 = Stdlib.compare !aclone aref in

    if cmp1 <> cmp2 then
      begin
        Printf.printf "\n\n  🞮  Compare give uncomparable results at iteration %d\n\n%!" i ;
        Printf.printf "     cmp1 = %d   comparing  0x%s   and  0x%s\n\n" cmp1 (t.testee.tos !dataclone) (t.testee.tos !data) ;
        Printf.printf "     cmp2 = %d   comparing  %s   and  %s\n\n" cmp2 (atos !aclone) (atos aref) ;
        assert false
      end ;


    (* Clone *)
    if i mod 50 = 0 then
      begin
        dataclone := t.testee.clone !data ;
        aclone := Array.copy aref
      end ;

    (* Print *)
    if i mod 1000 = 0 then Printf.printf "  - Testing  %s\n%!" (t.testee.tos !data) ;

    (* Modify *)

    let index = Random.int t.size in
    let list_index = Random.int (List.length t.testee.setv) in
    let vval = List.nth t.testee.setv list_index in

    (* Printf.printf "  Setting cell #%d to value %d\n%!" index vval ; *)
    
    data := t.testee.set !data index vval ;
    set_tab t.testee.cp aref index vval ;

  done ;

  Printf.printf "\n\n  👍  The test is successful.\n\n%!" ;

  ()



let testee1 =
  let open Bitvec in
  let set bv i v = (if v = 0 then Bitvec.unset bv i else Bitvec.set bv i) ; bv in  
  { init ; get ; set ; clone ; cmp ; setv = [ 0 ; 1 ] ; tos ; cp = compute_pos }


let test_all tst =
  run_test (mk_test ~size:8 tst) ;
  run_test (mk_test ~size:16 tst) ;
  run_test (mk_test ~size:24 tst) ;
  run_test (mk_test ~size:32 tst) ;
  run_test (mk_test ~size:64 tst) ;
  run_test (mk_test ~size:128 tst) ;

  run_test (mk_test ~size:7 tst) ;
  run_test (mk_test ~size:9 tst) ;
  run_test (mk_test ~size:15 tst) ;
  run_test (mk_test ~size:17 tst) ;
  run_test (mk_test ~size:31 tst) ;
  run_test (mk_test ~size:33 tst) ;
  run_test (mk_test ~size:63 tst) ;
  run_test (mk_test ~size:65 tst) ;
  run_test (mk_test ~size:127 tst) ;
  run_test (mk_test ~size:129 tst) ;

  run_test (mk_test ~size:1 tst) ;
  run_test (mk_test ~size:2 tst) ;
  run_test (mk_test ~size:29 tst) ;

  run_test (mk_test ~size:48532 tst) ;
  run_test (mk_test ~size:75437 tst) ;
  ()

let test_all_bitvec () = test_all testee1

open Intarray

let id _ n = n

let testee_intarray fmt =
  let init n = Intarray.create fmt n in   
  { init ; get ; set ; clone ; cmp ; setv = [ 0 ; 1 ; 2 ; 255 ; 256 ; 1000 ; 65535 ; 65536 ] ; tos ; cp = id }

let test_all_intarray () = test_all (testee_intarray M8)


open Net
open Marking

let mk_net size = mk_dummy_net size

let mk_pl pl_id = { pl_id ; pl_name = "test-place" ; pl_pre = [] ; pl_post = [] }

let get t i = get t (mk_pl i)
let set t i x =
  let pl = mk_pl i in
  let z = get t i in
  add (add t pl (-z)) pl x

let cmp = compare

let testee_marking_safe =
  let init size = init ~safe:true (mk_net size) in
  { init ; get ; set ; clone ; cmp ; setv = [ 0 ; 1 ] ; tos ; cp = compute_pos }

let testee_marking_non_safe =
  let init size = init ~safe:false (mk_net size) in
  { init ; get ; set ; clone ; cmp ; setv = [ 0 ; 1 ; 2 ; 255 ; 256 ; 1000 ; 65535 ; 65536 ] ; tos ; cp = id }

let test_all_marking () =
  test_all (testee_marking_safe) ;
  test_all (testee_marking_non_safe) ;
  ()

let () = test_all_marking ()
    

