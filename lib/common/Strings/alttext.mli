(* Text (string) with alternatives,
 *
 *  e.g.   ab-oe-foo
 *         ab--o-foo
 *         ab-ö-foo
 *
 *         ab(-|--)-(oe|o|ö)-foo
 *
 * (This is a subcase of regexps, supposedly more efficicent, and with specific functions.)
 *
 * A path is a string obtained by chosing one of each alternative, e.g.  ab-oe-foo.
 * 
 * Invariants: singleton alternatives are pure ascii: (ab) (-foo)
 *             multiple alternatives are considered atomic (usually a single utf character & its ascii variants): (oe|o|ö)
 *)

type utf = string
type ascii = string

(* Trim and squish separators, change to lowercase. 
 * sep is space only by default *)
val normalize: ?sep:utf list -> utf -> utf

val capitalize: utf -> utf


(* A value of type alt is guaranteed to contain at least one pure-ascii path.
 * (UTF strings are automatically transformed to ascii). *)
type alt
type t = alt

val empty: t

val is_empty: t -> bool

(* Note: utf string is normalized by default - separators are trimmed and squished (spaces only by default) *)
val s2alt: ?sep:utf list -> ?norm:bool -> utf -> t

(* Concat *)
val altadd: t -> t -> t
val altadds: t -> utf -> t

(* Concat with a separator.
 * sep_when_empty: indicates if the separator is inserted when an empty element is appended. *)
val concat: sep_when_empty:bool -> sep:alt -> alt list -> alt


(* A single space. You need it because normalisation, done by s2alt, remove trailing spaces. *)
val space: t

(* Get the richest string representation (choose the alternatives with apparently more information).
 * Here, more information means higher byte values. *)
val alt2rs: t -> utf
val get_utf: t -> utf (* == alt2rs *)

(* Get the weakest string representation, guaranteed ASCII. *)
val alt2ws: t -> ascii
val get_ascii: t -> ascii (* alt2ws *)

(* Get all ascii variants (max: num) *)
val get_all_ascii: ?num:int -> t -> ascii list

(* Length of the shortest path. *)
val len: t -> int 

(* Debug string *)
val alt2ds: t -> utf

(* Returns ascii canonical form of this string, with respect to all rotations around separators and all ascii-alternatives (limited to num). *)
val get_canonicals: ?sep:utf list -> ?num:int -> t -> ascii list

(* cuts: find all decompositions of s such that s = s1 + sep + s2  with s1 and s2 non empty. 
 * Default separators include space ' - *)
val cuts: ?sep:utf list -> t -> (t * t) list

(* space ' -  *)
val default_seps: utf list

(* merge a b returns None if a and b are not mergeable.
 * It returns Some (common, a', b')  where a' or b' is empty.
 *
 * Case b' empty (a' empty is symmetric)
 *   - b is a prefix of a
 *
 *   - common is the (enriched) alt of the common part between a and b.
 *
 *   - a' is the rest, without the cutting separator
 *     (the cut must occur on a separator)
 *
 *  If both elements are equal (or unifiable), a' and b' are empty.
 *)
val merge: ?sep:utf list -> t -> t -> (t * t * t) option



(*
(* Two alts a and b are compatible if there exists a path pa in a and a path pb in b such that
 * ascii(pa) = ascii(pb) *)
val compatible: t -> t -> bool

(* Merge two alts, provided they are compatible. 
 * This creates a new alt with more alternatives. 
 * Returns None if they are not compatible. *)
val merge: t -> t -> t option
*)

