(*** Regexp unions, which associates values of type 'a to matches. ***)


(* This is some kind of a small, ad-hoc lexer.

   Example:

   let urex = createb [ `ANCHORED ; `CASELESS ]
      [ [ "[0-9]+$" ], (fun s sub -> `Int (int_of_string (Pcre.get_substring sub 0))) ;
        [ "[a-z]+$" ], (fun s _ -> `Alpha s)
      ]
   in

   match associated urex "your-test-string-here" with
   | [] ->  (* No match *)
   | [ `Int n ] -> (* You got an int *)
   | [ `Alpha s ] -> (* You got letters. *)


   
   Pitfalls :
     - `ANCHORED: the regexp must match from the beginning of string
     - do not omit $ at the end

     otherwise, only a part of the string may match the regexp. You obtain this part with get_substring sub 0

*)

type 'a union_regexp

type 'a t = 'a union_regexp

(* Create an empty union_regexp. *)
val empty: Pcre.cflag list -> 'a union_regexp

(* val is_empty: 'a union_regexp -> bool *)

(* Adds the given regexps to the union_regexp. These regexps are associated to value 'a. *)
val add: 'a union_regexp -> 'a -> string list -> 'a union_regexp

type 'a builder = string -> Pcre.substrings -> 'a

(* Like add, but the value is built using the whole string and the matching results. *)
val addb: 'a union_regexp -> 'a builder -> string list -> 'a union_regexp

(* Both unions must use the same cflags (not verified). *)
val union: 'a union_regexp -> 'a union_regexp -> 'a union_regexp

(* Convenience functions *)
val create: Pcre.cflag list -> (string list * 'a) list -> 'a union_regexp
val createb: Pcre.cflag list -> (string list * 'a builder) list -> 'a union_regexp

val createu: Pcre.cflag list -> string list -> unit union_regexp

(* Indicates if the given string matches at least one regexp in the union. *)
val matches: 'a union_regexp -> string -> bool

(* Returns all the 'a values associated to matching regexps. An 'a value may occur at most once in the returned list (compared with =).
 * [] if none matches. *)
val associated: 'a union_regexp -> string -> 'a list

val map: 'a union_regexp -> ('a -> 'b) -> 'b union_regexp


(* Useful regexps *)
    (* to be defined -----
val ur_decimal: float union_regexp
val ur_int: int union_regexp
*)

(* Vintage *)

(* Returns a list of all regexps in this union. *)
val get_regexps: 'a union_regexp -> string list
    
