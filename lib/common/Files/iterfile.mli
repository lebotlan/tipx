

(* Opens a file and iter a function over every line. *)

(* empty_lines: should we ignore empty lines? *)

val fold_file : filename:string -> ?empty_lines:bool -> ?flags:Pcre.cflag list -> ?ignore_regexp:string list -> (int -> string -> 'a -> 'a) -> 'a -> 'a

val iter_file : filename:string -> ?empty_lines:bool -> ?flags:Pcre.cflag list -> ?ignore_regexp:string list -> (int -> string -> unit) -> unit

