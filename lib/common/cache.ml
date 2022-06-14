
let cache f =
  let all = Hashtbl.create 10 in

  fun arg ->
    if Hashtbl.mem all arg then Hashtbl.find all arg
    else
      begin
        let result = f arg in
        Hashtbl.add all arg result ;
        result
      end

let cache2 f =
  let cached = cache (fun (x,y) -> f x y) in
  fun x y -> cached (x,y)

(*** QCACHE ***)

type ('a,'b,'c) qcache =
  { keys: 'a array ;
    results: 'b array ;
    (* Number of cells filled in the array (from 0 to filled-1). *)
    mutable filled: int ;

    (* The function been cached. Used to test if the cache can be reused when creating a new cached function. *)
    mutable orig: 'c option ;
    
    eqq: 'a -> 'a -> bool }

let mk_qcache ~eqq ~size ~init =
  let (a,b) = init in
  { keys = Array.make size a ;
    results = Array.make size b ;
    filled = 0 ;
    orig = None ;
    eqq }

let rec look_aux qcache x i =
  if i >= qcache.filled then None
  else if qcache.eqq x qcache.keys.(i) then Some qcache.results.(i)
  else look_aux qcache x (i+1)

let look qcache x = look_aux qcache x 0

let insert qc x r =
  let len = Array.length qc.keys in
  if qc.filled < len then
    begin
      let i = qc.filled in
      qc.filled <- i + 1 ;
      qc.keys.(i) <- x ;
      qc.results.(i) <- r
    end

  else
    (* Recycle... FIFO (?) *)
    begin          
      Array.blit qc.keys 1 qc.keys 0 (len-1) ;
      Array.blit qc.results 1 qc.results 0 (len-1) ;
      qc.keys.(len-1) <- x ;
      qc.results.(len-1) <- r
    end

let same_orig qc oo = match qc.orig with
  | None -> false
  | Some ii -> oo == ii

let qcached qcache ~orig f =

  let () =
    if same_orig qcache orig then ()
    else (qcache.filled <- 0 ; qcache.orig <- Some orig)
  in
  
  fun x ->
  match look qcache x with
  | Some y -> y
  | None ->
    let result = f x in
    insert qcache x result ;
    result
  


