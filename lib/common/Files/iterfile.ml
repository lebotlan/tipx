

let fold_file ~filename ?(empty_lines=false) ?flags ?(ignore_regexp=[]) f acu =


  let ignore_list = List.map (fun s -> Pcre.regexp ?flags s) ignore_regexp in
  let ignores line =
    if empty_lines && line = "" then true
    else
      List.exists (fun rex -> Pcre.pmatch ~rex line) ignore_list
  in

  let chin = open_in filename in

  let acu = ref acu
  and linenb = ref 0 in
  begin try
    while true do
      let line = input_line chin in
      incr linenb ;
      if ignores line then ()
      else
	acu := f !linenb line !acu
    done
  with 
  | End_of_file -> close_in chin 
  | e -> close_in chin ; raise e
  end ;

  !acu

let iter_file ~filename ?empty_lines ?flags ?ignore_regexp f = fold_file ~filename ?empty_lines ?flags ?ignore_regexp (fun i s () -> f i s) ()
