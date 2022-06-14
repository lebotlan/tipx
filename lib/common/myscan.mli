
(*** Compositional scanners (of strings). ***)

(* Type of a scanner which scans a string and returns values of types 'a *)
type 'a t

(* Returns Some result if scan succeeds, None if scan failed. 
 * Note that the end of input is not necessarily reached: only a prefix of input can be used to satisfy the scanner. 
 * (Unless the scanner is built using mkfull, see below.) *)
val scan: 'a t -> string -> 'a option

(* Scans according to a format.
 * Usage: fmt_scan format consumer_function
 *        The consumer function returns a value of type 'd.
 *        fmt_scan returns a scanner of type 'd t 
 * Example: fmt_scan "john %d" (fun i -> i)  is a scanner of type int t
*)
val fmt_scan: ('a, Scanf.Scanning.in_channel, _, 'c -> 'd, 'a -> 'e, 'e) format6 -> 'c -> 'd t

(* Like fmt_scan, but for one argument scanners : it returns directly the value. *)
val fm1_scan: ('a, Scanf.Scanning.in_channel, _, ('c -> 'c) -> 'd, 'a -> 'e, 'e) format6 -> 'd t

(* Basic scanners *)			       
val int_scan: int t
val string_scan: string t (* Delimited by a space, like %s in Scanf *)    
val bool_scan: bool t
val spaces_scan: unit t (* Eats 0 or more spaces, see Scanf for details. *)

(* Eats everything. *)
val yes_scan: string t

(* Scans a given string. *)
val kw_scan: string -> 'a -> 'a t

(* Map *)
val map: 'a t -> ('a -> 'b) -> 'b t

(* Succeeds if one of these scanners succeeds *)
val union: 'a t list -> 'a t

(* Sequence *)				  
val seq: 'a t -> 'b t -> ('a * 'b) t

val seq3: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
				   
(* Like a sequence, with a separator. *)				   
val pair: sep:string -> 'a t -> 'b t -> ('a * 'b) t

(* Eats spaces before and after scanning. *)
val trim: 'a t -> 'a t

(* list ~split scan   reads elements with scan, separated by ~split.
 * Succeeds only if at least min_length elements are found (0 by default).
 * split is a keyword, not a pattern. Do not put spaces arount 'split'.
 *
 * May fail if a separator (split) is followed by an unparsable element. *)
val list: ?min_length:int -> split:string -> 'a t -> 'a list t

(* Makes a full scanner from a partial scanner,
 * that is, this parser fails if there are trailing characters. The end of input must be reached. *)
val mk_full: 'a t -> 'a t
		       
			
(* Convenience function: a sequence of a scanf-format and a scanner.
 * fmt_seq format scanner (fun (format-args...) (scanner-value) -> 'd) 
 * Beware, you may need a space at the end of format. 
 *
 * Example: fmt_seq "John %d %d " scan_rest (fun a b rest -> ...) *)
val fmt_seq: ('a, Scanf.Scanning.in_channel, 'b, 'c -> 'f -> 'd, 'a -> 'e, 'e) format6 -> 'f t -> 'c -> 'd t

(* Variants of fmt_seq with 2, 3, ... extra scanners can be written using the API above. *)
    
(* let fmt_seq3 fmt sc1 sc2 f = map (seq3 (fmt_scan fmt f) sc1 sc2) (fun (f1, v1, v2) -> f1 v1 v2) *)
