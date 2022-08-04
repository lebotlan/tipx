type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or  of 'a bexpr list
                | Not of 'a bexpr
                | True
                | False


let rec bool2s fu = function
  | V x -> fu x
  | And l -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " /\\ " l
  | Or  l  -> Common.sep (fun x -> "(" ^ bool2s fu x ^ ")") " \\/ " l
  | Not e  -> "-(" ^ bool2s fu e ^ ")"
  | True -> "True"
  | False -> "False"

let rec eval_bool fu = function
  | V x -> fu x
  | And l -> List.for_all (eval_bool fu) l
  | Or  l  -> List.exists (eval_bool fu) l
  | Not e  -> not (eval_bool fu e)
  | True -> true
  | False -> false

let rec neg_propagation lit_neg neg = function
  | V x as expr -> if neg then V (lit_neg x) else expr
  | And l -> let l = List.rev_map (neg_propagation lit_neg neg) l in if neg then Or  l else And l
  | Or  l -> let l = List.rev_map (neg_propagation lit_neg neg) l in if neg then And l else Or  l
  | Not e -> neg_propagation lit_neg (not neg) e
  | True -> if neg then False else True
  | False -> if neg then True else False

let and_append vv = function
  | [False] -> [False]
  | acu -> vv :: acu

let cartesian ll =

  let insert prefix = function
    | True -> prefix
    | False -> [False]
    | And l -> List.rev_append l prefix
    | Or _ -> assert false
    | Not _ -> assert false
    | V _ as vv -> and_append vv prefix
  in
  
  let rec loop acu prefix = function
    | [] -> prefix :: acu
            
    | [] :: _ -> acu
      
    | (x1 :: l1) :: rest ->

      let prefix2 = insert prefix x1 in
      let acu2 = loop acu prefix2 rest in
      loop acu2 prefix (l1 :: rest)
  in

  loop [] [] ll

let or_to_list = function
  | Not _ -> assert false
  | Or l -> l
  | aa -> [ aa ]

let make_or = function
  | [] -> False
  | [x] -> x
  | l -> Or l

let make_and = function
  | [] -> True
  | [x] -> x
  | l -> And (List.rev l)

let or_append vv = function
  | [True] -> [True]
  | acu -> vv :: acu

(* acu is a disjunction of cubes. *)
let rec dnf_aux acu = function
  | True -> [True]
  | False -> acu
  | V _ as vv -> or_append vv acu

  | Not _ -> assert false

  | Or l -> List.fold_left dnf_aux acu l

  | And l ->
    let clauses = List.rev_map (fun x -> make_or (dnf_aux [] x)) l in

    let sets = List.rev_map or_to_list clauses in
    let cubes = List.rev_map (fun tuple -> make_and tuple) (cartesian sets) in
    List.rev_append cubes acu

let dnf lit_neg ?(neg=false) bexpr = make_or (dnf_aux [] (neg_propagation lit_neg neg bexpr))

let and_to_list l =
  let rec loop acu = function
    | [] -> acu
    | V x :: rest -> loop (x :: acu) rest
    | _ -> assert false
  in
  loop [] l 

let or_to_list l =
  let rec loop acu = function
    | [] -> acu
    | V v :: rest -> loop ([v] :: acu) rest 
    | (And l) :: rest -> loop (and_to_list l :: acu) rest
    | _ -> assert false
    in
    loop [] l

let dnf_to_list = function
    | V x -> [[x]] 
    | And l -> [and_to_list l]
    | Or l -> or_to_list l
    | _ -> assert false

(* let lit_to_bool l = V l

let cube_list_to_bool l = And l

let clause_list_to_bool l = Or l *)

let list_to_dnf l = 
  let rec loop acu = function
    | [] -> acu
    | c :: rest -> loop ((And (List.rev_map (fun x -> V x) c)) :: acu) rest
  in
  Or (loop [] l)
