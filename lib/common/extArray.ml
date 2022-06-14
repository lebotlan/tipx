
type 'a t =
    { default_value : 'a ;
      mutable realsize : int ;
      mutable tab : 'a array }

let create n v =
  { default_value = v ;
    realsize = 0 ;
    tab = Array.make n v }

let copy ?(cp=fun x -> x) t = 
  { default_value = t.default_value ;
    realsize = t.realsize ;
    tab = Array.map cp t.tab }

let get t index = 
  if index < 0 then failwith "extArray.get, negative index."
  else if index >= t.realsize then t.default_value
  else t.tab.(index)

let set t index v =

  let len = Array.length t.tab in
  if index >= len then
    begin
      (* Resize *)
      let newarray = Array.make (max (index+1) (2 * len)) t.default_value in
      Array.blit t.tab 0 newarray 0 len ;
      t.tab <- newarray ;
    end ;

  t.tab.(index) <- v ;
  t.realsize <- max t.realsize (index+1) ;
  ()

let size t = t.realsize

let fold t acu f =
  let akk = ref acu in
  for i = 0 to t.realsize - 1 do
    akk := f i t.tab.(i) !akk ;
  done ;
  !akk

let iter t f =
  for i = 0 to t.realsize - 1 do
    f i t.tab.(i) ;
  done
  
module Mat =
  struct
    type 'a mat =
      { lines: ('a t) t ;
	default_line: 'a t }

    let create lines cols v =
      let default_line = create cols v in
      { lines = create lines default_line ;
	default_line }

    let set mat y x v =
      let l = get mat.lines y in

      let l =
	if l == mat.default_line then
	begin
	  let nl = copy l in
	  set mat.lines y nl ;
	  nl
	end
	else l
      in
      set l x v

    let get mat y x = get (get mat.lines y) x

    let size mat =
      let height = size mat.lines in
      let width = fold mat.lines 0 (fun _ l acu -> max acu (size l)) in
      (height, width)

    let fold mat acu f = fold mat.lines acu (fun linenb l acu -> fold l acu (fun col v acu -> f linenb col v acu))

    let iter mat f = iter mat.lines (fun linenb l -> iter l (fun col v -> f linenb col v))

    let copy ?cp mat =
      let copy_line l = if l == mat.default_line then l else copy ?cp l in
      { lines = copy ~cp:copy_line mat.lines ;
	default_line = mat.default_line }
  end
    
