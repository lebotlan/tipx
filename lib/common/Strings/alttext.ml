type utf = string
type ascii = string

(* Cannot be empty.
 * Two cases:
 *    - singleton choices are ascii-only
 *    - multiple choices are atomic (e.g. a single utf character) and contain at least one ascii string. *)
type choice = utf list

type alt = choice list
type t = alt

let empty = [[""]]

let space = [[" "]]


let is_empty_choice choice = List.for_all (fun s -> s = "") choice

let is_empty alt = List.for_all is_empty_choice alt

(* In general, no need to include utf symbols, since their ascii variant should be at the same point too. 
 *
 * All separators are unifiable. *)
let default_seps = [ " " ; "-" ; "'" ]

(* Special utf characters that are mapped to single ascii sequences (no alt). *)
let specials_s = function
  | "°" -> "deg."

  (* Greek letters. To be enriched if necessary. *)
  | "α" -> "alpha"
  | "β" -> "beta"
  | "γ" -> "gamma"
  | "δ" -> "delta"
    
  | s -> s

let map_ascii c =
  let c2 = specials_s c in  
  if Text.is_ascii c2 then c2 else Text.to_ascii c2

(* Special utf characters that are mapped to alternatives.
 *  Empty list means this is not a special. 
 *
 * Returns a choice. It must contain at least one pure-ascii version (not checked, we trust the function). *)
let specials_n = function
  | "ø" -> [ "oe" ; "o" ]
                 
  (*  | "tiret-moisi..." -> [ "--" ; "-" ] *)
  | _ -> []

let union u v = List.fold_left Common.insert_l u v

(* get_choice: applies specials_s and specials_n. Returns a choice or empty list. *)
let get_choice ?(sep=default_seps) c =
  let c2 = map_ascii c in

  (* specials_n invoked on c, not on c2, otherwise some cases of specials_n may be ignored. *)
  let spe = specials_n c in
  let spe = if c2 <> " " && List.mem c2 sep then union [" " ; c2] spe else spe in
    
  match spe, c == c2 with
  | [], true -> []
  | [], false -> [ c ; c2 ]
  | l, true -> Common.insert_l l c
  | l, false -> assert (List.mem c2 l) ; c :: l
  

type build_acu =
  { (* Start position of the current string *)
    current_s: Text.pointer ;

    (* Reversed alt been built. *)
    current_alt: alt }

let init_build_acu point =
  { current_s = point ;
    current_alt = [] }

(* Put the current_string in current_alt, update current_s. *)
let close_build_acu acu point =
    if point == acu.current_s then acu
    else
      let s = Text.chunk acu.current_s point in
      let () = assert (Text.is_ascii s) in

      { current_s = point ;
        current_alt = [s] :: acu.current_alt }

let utf_to_alt ?sep utf =

  let rec loop acu point =
    match Text.next point with
    | None -> List.rev (close_build_acu acu point).current_alt
    | Some (c, next) ->
      begin match get_choice ?sep c with
        | [] -> loop acu next
        | choice ->
          let acu2 = close_build_acu acu point in
          let acu3 = { current_s = next ;
                       current_alt = choice :: acu2.current_alt }
          in
          loop acu3 next
      end
  in

  let start_point = Text.pointer_l utf in
  loop (init_build_acu start_point) start_point


let normalize ?(sep=[" "]) s =

  if not Transcode.(is_valid Utf8 s) then failwith "Alttext.normalize: not valid utf." ;

  (* Lowercase *)
  let s = Text.lower s in

  (* Strip & remove multiple spaces. *)

  let s = Text.strip ~chars:sep s in
  let is_sep x = List.mem x sep in
  
  (* I wish I could use something better to squish spaces. *)
  let filter_char =
    (* Indicate if the next space we find should be kept. *)
    let keep_space = ref false in
    fun c ->
      match (is_sep c, !keep_space) with
      | false, false -> keep_space := true ; true
      | false, true -> true
      | true, false -> false
      | true, true -> keep_space := false ; true
  in

  Text.filter filter_char s

    

let s2alt ?sep ?(norm=true) utf = utf_to_alt ?sep (if norm then normalize ?sep utf else utf)

let altadd alt1 alt2 = alt1 @ alt2

let altadds alt1 s = altadd alt1 (s2alt s)

let rec concat ~sep_when_empty ~sep = function
  | [] -> empty
  | [x] -> x
  | x :: xs ->
    let tail = concat ~sep_when_empty ~sep xs in
    if is_empty x && not sep_when_empty then tail
    else x @ sep @ tail


(* string score, to detect 'richer' utf strings. *)
let score s = Text.fold (fun c acu -> acu + Text.code c) s 0

let best_score cmp = function
  | [] -> assert false
  | [s] -> s
  | s :: rest ->
    fst (Common.myfold rest (s, score s)
           begin fun (s, sco) next ->
             let sco2 = score next in
             if cmp sco sco2 then (s,sco) else (next,sco2)
           end)

(* Choose the filtered (filter), and richest or weakest alternatives, depending on cmp. *)
let choose filter cmp all =
  assert (all <> []) ;
  match List.filter filter all with
  | [] -> best_score cmp all
  | l -> best_score cmp l

let atos filter cmp alt =
  let rec loop acu = function
    | [] -> acu
    | [x] :: rest -> loop (acu ^ x) rest
    | choice :: rest -> loop (acu ^ (choose filter cmp choice)) rest
  in
  loop "" alt

(* Normalize to squish spaces, if some of them appeared by choosing alternatives. *)
let alt2rs alt = normalize (atos (fun s -> not (Text.is_ascii s)) (>) alt)
let alt2ws alt = normalize (atos Text.is_ascii (<) alt)

let get_utf = alt2rs
let get_ascii = alt2ws

let add_choice_len acu choice =
  assert (choice <> []) ;
  let len = List.fold_left (fun m s -> min m (Text.length s)) max_int choice in
  acu + len

let len alt = List.fold_left add_choice_len 0 alt
  
let choice2s = function
  | [] -> assert false
  | [s] -> "(" ^ s ^ ")"
  | l -> "(" ^ Common.sep (fun s -> s) "|" l ^ ")"

let alt2ds alt = Common.sep choice2s "" alt

let find_from n pos sep =
  let len = String.length n in
  let rec loop x =
    if x >= len then raise Not_found
    else if List.mem (String.make 1 n.[x]) sep then x else loop (x+1)
  in
  loop pos

(* Find the rotated candidate with min value, by permuting around spaces. *)
let rec ascii_get_min sep minval len n pos =
  try 
    (* Find next separator *)
    let pos = find_from n pos sep in

    (* Flip around this point *)
    let left = String.sub n 0 pos
    and right = String.sub n (pos+1) (len - pos - 1)
    in

    let candidate = right ^ " " ^ left in    
    ascii_get_min sep (min candidate minval) len n (pos+1)

  with 
  | Not_found -> minval
  | _ -> assert false

let rotated_canonical sep n =
  assert Transcode.(is_valid Ascii n) ;
  let len = String.length n in
  if len > 0 then ascii_get_min sep n len n 0 else n

let product_concat pre = function
  | [] -> assert false (* Empty choice after filter. Bug bug ! *)
  | l -> List.fold_left (fun acu p -> List.fold_left (fun acu x -> Common.insert_l acu (p ^ x)) acu l) [] pre

let get_all_ascii ?(num=8) alt =

  (* Filter *)
  let alt = List.map (List.filter Text.is_ascii) alt in

  (* pre: list of prefixes *)
  let rec loop pre = function
    | [] -> pre
    | [] :: _ -> assert false (* No empty choice *)
    | ( c1 :: _ :: _) :: rest when List.length pre >= num -> loop (product_concat pre [c1]) rest
    | choice :: rest -> loop (product_concat pre choice) rest      
  in

  loop [""] alt 

let get_canonicals ?(sep=default_seps) ?num alt =
  let all = get_all_ascii ?num alt in
  List.map (rotated_canonical sep) (List.fold_left Common.insert_l [] all)


let split_utf utf point nxt =
  let left = Text.(chunk (pointer_l utf) point)
  and right = Text.(chunk nxt (pointer_r utf)) in
  (left,right)

let cut_utf is_sep utf =
  let rec loop acu point =
    match Text.next point with
    | None -> acu
    | Some (c,next) -> loop (if is_sep c then split_utf utf point next :: acu else acu) next
  in
  loop [] (Text.pointer_l utf)

let check_start_utf f utf =
  assert (utf <> "") ;
  f (Text.get utf 0)

let check_start q f alt =
  match alt with
  | [] -> assert false
  | choice :: _ -> q (check_start_utf f) choice

let fnot f x = not (f x)

let insert_split is_sep left right acu =
  assert (not (is_empty left)) ;
  assert (not (is_empty right)) ;

  assert (check_start List.for_all (fnot is_sep) left) ;
  assert (check_start List.for_all (fnot is_sep) right) ;
  
  (left, right) :: acu

let l_add_s elt l = if elt = "" then l else [elt] :: l

let cuts ?(sep=default_seps) alt =

  let is_sep x = List.mem x sep in
  
  (* pre is reversed *)
  let rec loop acu pre post = match post with
    | [] -> acu
    | [] :: _ -> assert false (* Empty choice *)
    | [ascii] as ch :: rest ->
      let acu2 = Common.myfold (cut_utf is_sep ascii) acu (fun acu (left,right) -> insert_split is_sep (List.rev (l_add_s left pre)) (l_add_s right rest) acu) in
      loop acu2 (ch :: pre) rest

    | choice :: rest -> (* A choice is atomic. Is it a separator ? *)
      let acu2 = if List.exists is_sep choice then insert_split is_sep (List.rev pre) rest acu else acu in
      loop acu2 (choice :: pre) rest      
  in

  loop [] [] alt

(* Capitalize all words *)
let rec capit_aux utf in_word point acu =
  match Text.next point with
  | None -> acu
  | Some (char, next) ->
    let alnum = Text.is_alnum char in
    let acu' =
      if (not in_word) && alnum then acu ^ (Text.capitalize char)
      else acu ^ char
    in
    capit_aux utf alnum next acu'

let capitalize utf = capit_aux utf false (Text.pointer_l utf) ""

(* Non empty intersection ? *)
let rec compatible_choice a b = match b with
  | [] -> false
  | b1 :: bs -> List.mem b1 a || compatible_choice a bs

(* Check if x1 is a prefix of x2 *)
let is_prefix x1 x2 = if Common.starts_with x1 x2 then Some (Common.remove_start x1 x2) else None

let which_prefix x1 x2 =
  assert (Text.is_ascii x1 && Text.is_ascii x2) ;

  match is_prefix x1 x2 with
  | Some r -> Some (`Left r)
  | None ->
    match is_prefix x2 x1 with
    | Some r -> Some (`Right r)
    | None -> None

let clean_choice l =
  match List.filter (fun s -> s <> "") l with
  | [] -> None
  | l -> Some l  

let clean alt = List.filter_map clean_choice alt

let left_quotient s acu pre =
  match is_prefix pre s with
  | None -> acu
  | Some rest -> rest :: acu

(* Returns the set { post | s = pre.post with pre in ch } *)
let find_prefix s ch = List.fold_left (left_quotient s) [] ch

let rec find_ok l f = match l with
  | [] -> None
  | x :: xs -> match f x with
    | Some _ as res -> res
    | None -> find_ok xs f

let remove_sep is_sep alt = match clean alt with
  | [] -> []
  | [] :: _ -> assert false

  (* ASCII singleton *)
  | [a] :: rest ->
    let len = String.length a in
    assert (len >= 1) ;
    assert (is_sep (Text.get a 0)) ;
    if len = 1 then rest
    else [String.sub a 1 (len-1)] :: rest

  (* Choice *)
  | ch :: rest ->
    assert (List.exists is_sep ch) ;
    rest

let rec cutable is_sep = function
  | [] -> None (* We do not want empty to be unifiable with something else - it looks like an error. *)
  | [] :: _ -> assert false (* Empty choice *)
  | [""] :: rest -> cutable is_sep rest
  | [a] :: rest ->
    let len = String.length a in
    if is_sep (String.make 1 a.[len-1]) then
      if len = 1 then Some rest else Some ([String.sub a 0 (len-1)] :: rest)
    else None
    
  | ch :: rest -> if List.exists is_sep ch then Some rest else None

let merge ?(sep=default_seps) a b =

  let is_sep x = List.mem x sep in
  
  let a = clean a
  and b = clean b in
  
  (* common is reversed *)
  let rec loop common u v = match u,v with
    | [], [] -> Some (List.rev common, empty, empty)
                  
    | [], v2 when check_start List.exists is_sep v2 -> Some (List.rev common, empty, remove_sep is_sep v2)
    | u2, [] when check_start List.exists is_sep u2 -> Some (List.rev common, remove_sep is_sep u2, empty)

    | [], v2 when None <> cutable is_sep common -> (match cutable is_sep common with None -> assert false | Some rest -> Some (List.rev rest, empty, v2))
    | u2, [] when None <> cutable is_sep common -> (match cutable is_sep common with None -> assert false | Some rest -> Some (List.rev rest, u2, empty))
                                             
    | [], _
    | _, [] -> None (* Not a separator. *)

    | [] :: _, _ -> assert false
    | _, [] :: _ -> assert false (* Empty choice *)

    (* ASCII singleton vs ASCII singleton *)
    | [u2] :: us, [v2] :: vs ->
      begin match which_prefix u2 v2 with
        | None -> None
        | Some (`Left r) -> loop (l_add_s u2 common) us (l_add_s r vs)
        | Some (`Right l) -> loop (l_add_s v2 common) (l_add_s l us) vs
      end

    (* Atomic choice vs ASCII singleton *)
    | uch :: us, [v2] :: vs ->
      (* A prefix of v2 must be compatible with uch *)
      begin match find_prefix v2 uch with
        | [] -> None
        | rests ->
          (* Find which rests are ok. *)
          find_ok rests (fun v2_rest -> loop (uch :: common) us (l_add_s v2_rest vs))
      end

    (* ASCII singleton vs Atomic choice *)
    | [u2] :: us, vch :: vs ->
      begin match find_prefix u2 vch with
        | [] -> None
        | rests -> find_ok rests (fun u2_rest -> loop (vch :: common) (l_add_s u2_rest us) vs)
      end
      
    (* Atomic choice vs Atomic choice *)
    | uch :: us, vch :: vs -> if compatible_choice uch vch then loop (union uch vch :: common) us vs else None

  in

  (* We do not want empty to be mergeable with something else. It looks like an error. *)
  loop [] a b

  

