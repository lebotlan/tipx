(* Future:
 * An UTF string does not necessarily have a unique ASCII representation.
 *       e.g.  Romøren  can be  Romoeren   or Romoren.
 *
 * ... A string becomes a regexp (an automaton) (simpler, no cycles) ??
 *)


type utf = string
type ascii = string

(*** ASCII ***)

(* Returns a canonical form of this string, with respect to all rotations. Case-sensitive! *)
val rotated_canonical : ascii -> ascii

(*** UTF ***)

(* Squish spaces, change to lowercase. *)
val normalize: utf -> utf

(* Computes a score, measuring the string information (in terms of accents, special characters, ...) 
 * higher score means richer string. *)
val string_score: string -> int

  
(*** DUOSTRINGS ***)

(* A string stored both in UTF and ascii. *)
type duostring
type duo = duostring

(* Given a UTF string, builds a duostring, by normalizing. *)
val mkduo: utf -> duo

(* Ascii length *)
val len: duo -> int

(* Concatenation *)
val (^^^): duo -> duo -> duo

(* Concat with a separator.
 * sep_when_empty: indicates if the separator is inserted when an empty element is appended. *)
val concat: sep_when_empty:bool -> sep:duo -> duo list -> duo

(* Empty string *)
val emptyduo: duo

val is_empty: duo -> bool

(* A single space *)
val space: duo

val get_one_ascii: duo -> ascii
val get_utf: duo -> utf

(* Upgrade: prepare for alttext *)
val get_all_ascii: duo -> ascii list

 (* Returns an ascii rotated_canonical (see above). *)
(* val get_canonical: duo -> ascii *)

(* Upgrade: now returns a list. *)
val get_canonical: duo -> ascii list

(* Upgrade
 * cuts: find all decompositions of s such that s = s1 + ' ' + s2  with s1 and s2 non empty. *)
val cuts: duo -> (duo * duo) list


(* Like String.sub.
 * Position and length are relative to the ascii version. *)
(* val sub: duo -> int -> int -> duo *)

(* Capitalize all words *)
val capitalize: duo -> duo 

(* Merges two duos, which must have the same ascii part.
 * Keeps the utf part with is the richest (according to its score). 
 * Returns None if ascii parts do not match. *)
(* val merge: duo -> duo -> duo option  *)

(* merge a b returns None if a and b are not mergeable.
 * It returns Some (common, a', b')  where a' or b' is empty.
 *
 * Case b' empty (a' empty is symmetric)
 *   - b is a prefix of a
 *
 *   - common is the enriched string of the common part between a and b.
 *
 *   - a' is the rest, without the cutting separator
 *     (the cut must occur on a separator)
 *
 *   If both elements are equal (or unifiable), a' and b' are empty.
 *)
val merge: duo -> duo -> (duo * duo * duo) option

