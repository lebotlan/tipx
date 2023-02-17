type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or  of 'a bexpr list
                | Not of 'a bexpr
                | True
                | False

let bool2b x = if x then True else False

let rec bexpr2s fu = function
  | V x -> fu x
  | And l -> Common.sep (fun x -> "(" ^ bexpr2s fu x ^ ")") " /\\ " l
  | Or  l  -> Common.sep (fun x -> "(" ^ bexpr2s fu x ^ ")") " \\/ " l
  | Not e  -> "- (" ^ bexpr2s fu e ^ ")"
  | True -> "T"
  | False -> "F"

let infix op f = function
  | [] -> ""
  | [x] -> f x
  | l -> "(" ^ op ^ " " ^ (Common.sep f " " l) ^ ")"

let rec bexpr2smt fu = function
  | V x -> fu x
  | And l -> infix "and" (bexpr2smt fu) l
  | Or  l  -> infix "or" (bexpr2smt fu) l
  | Not e  -> "(not " ^ bexpr2smt fu e ^ ")"
  | True -> "true"
  | False -> "false"
    

let rec eval_bexpr fu = function
  | V x -> fu x
  | And l -> List.for_all (eval_bexpr fu) l
  | Or  l  -> List.exists (eval_bexpr fu) l
  | Not e  -> not (eval_bexpr fu e)
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

let dnf ~lit_neg bexpr = make_or (dnf_aux [] (neg_propagation lit_neg false bexpr))

let and_to_list l =
  let rec loop acu = function
    | [] -> acu
    | V x :: rest -> loop (x :: acu) rest
    | _ -> failwith "dnf_to_list: argument is not in DNF (dnf error 3)."
  in
  loop [] l 

let or_to_list l =
  let rec loop acu = function
    | [] -> acu
    | V v   :: rest -> loop ([v] :: acu) rest 
    | And l :: rest -> loop (and_to_list l :: acu) rest
    | _ -> failwith "dnf_to_list: argument is not in DNF (dnf error 2)."
    in
    loop [] l

type 'a cube = 'a list

let dnf_to_list = function
    | V x -> [[x]] 
    | And l -> [and_to_list l]
    | Or l -> or_to_list l
    | _ -> failwith "dnf_to_list: argument is not in DNF (dnf error 1)."

let list_to_dnf l = 
  let rec loop acu = function
    | [] -> acu
    | c :: rest -> loop ((And (List.rev_map (fun x -> V x) c)) :: acu) rest
  in
  Or (loop [] l)


let rec map_simplify f = function
  | (V x) as b -> begin match f x with None -> b | Some bb -> bb end
    
  | Not b -> begin match map_simplify f b with True -> False | False -> True | bb -> Not bb end
               
  | And l -> simplify_and f [] l        
  | Or  l -> simplify_or f [] l
        
  | True -> True
  | False -> False

and simplify_and f acu = function
  | [] ->
    begin match acu with
      | [] -> True
      | [x] -> x
      | l -> And l
    end

  | x :: xs -> 
    begin match map_simplify f x with
      | True -> simplify_and f acu xs
      | False -> False
      | y -> simplify_and f (y :: acu) xs
    end

and simplify_or f acu = function
  | [] ->
    begin match acu with
      | [] -> False
      | [x] -> x
      | l -> Or l
    end

  | x :: xs -> 
    begin match map_simplify f x with
      | False -> simplify_or f acu xs
      | True -> True
      | y -> simplify_or f (y :: acu) xs
    end
