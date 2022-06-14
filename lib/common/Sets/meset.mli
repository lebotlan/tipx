(*
 * Sets of mergeable elements.
 * Element's information can only grow (by merging), not change (no update).
 * If you need updatable & mergeable elements, use Memap.
 *
 * Each element has one or several identifiers, e.g. for people: login, email, a unique id, ...
 * When two elements have a common identifier, they are merged.
 *
 * Each _element_ is understood as a projection of a "real" immutable _object_.
 * An _element_ only has partial information about the _object_.
 * Merging two elements creates a new element having more information about the real object.
 * Thus, each identifier actually identifies the _object_.
 *)



(* Input signature of the functor Make *)
module type ELEMENT =
sig
  (* The type of (mergeable) elements. 
   * Elements are not compared to each other (we do not use =, nor Pervasives.compare on elements). 
   * It is a very bad idea to have mutable data in elements. *)
  type elt

  (* The type of identifiers. 
   * Identifiers will be compared using Pervasives.compare. *)
  type id

  (* Returns a *non-empty* list of identifiers. *)
  val get_ids: elt -> id list
  
  (* The nature of an identifier. 
   * There should be a small number of different natures.
   * natures will be compared using Pervasives.compare
   *
   * nature is used to determine when an element does _not_ belong to a set. 
   * By default, you can take nature = unit (in which case, the function mem is unable to return No on a non-empty set). *)
  type nature
    
  val nature: id -> nature
    
  (* Merge two elements. 
   * Merge must satisfy a few properties (see invariants, below). 
   * @raise (Error Unmergeable) if merge fails. 
   *
   * merge a b should return the physical value b when possible (i.e. when no information is added to b by merging a). 
   *
   * force:false  merge may fail if the elements are not compatible.
   * force:true   merge force:true a b  should force the merging by giving priority to b, possibly ignoring information from a. 
   *)
  val merge: force:bool -> elt -> elt -> elt

  (* Returns the list of natures of this element's possible missing identifiers.
   * E.g. if the login of this elt is unknown, the list should contain the nature of login ids. 
   * If this element is known to have no email, the nature of email ids can be omitted from the list. 
   *
   * This is used to determine when an element does _not_ belong to a set. 
   * The empty list means that all the identifiers of this object are known.
   *
   * By default, if nature is unit, you must return [ () ], not the empty list. *)

  val missing_ids: elt -> nature list


  (* Expected invariants:
   * Notations: id(x) means (get_ids x)
   *            a & b means (merge a b)
   *
   *        - id(x) <> []                            : get_ids never returns an empty list
   *        - a & b = b & a    
   *        - a & (b & c) = (a & b) & c              : merge is commutative and associative
   *        - a & a = a                              : merge is idempotent
   * 
   *    (I) - id(a) \subset id(a&b)                  : get_ids is consistent with merging
   *
   *
   *    In general, we do not have id(a&b) \subset id(a) \cup id(b)
   *                (new identifiers may appear after merging, e.g.  (Name, first-name) & (group) => id(name + first-name + group)
   *                 such identifiers are called 'composite' identifiers.)
   *)

  (* to_string *)
  val elt2s: elt -> string
  val id2s: id -> string
  val nat2s: nature -> string
    
end

(* Result of member function. *)
type 'a mem_result =
  (* Elements which are projections of the expected object are found in the set. Returns all the matches. 
   * The list cannot be empty.
   * If it contains more than one element, it means that merging the input element in the set will imply merging the elements of the list
   * (possibly more elements, if 'composite' identifiers appear by merging). *)
  | Yes of 'a list

  (* The object is guaranteed not to be in the set. *)
  | No

  (* The object may belong to this set, or not. *)
  | Undetermined


(* Output signature *)
module type S =
sig

  type elt
  type set
  
  val empty: set

  (* Number of elements in the set (greater or equal to the number of _objects_ in the set).      
   * Merged elements count for one. 
   * The set cannot contain two elements which are _known_ to belong to the same object. *)
  val size: set -> int

  (* If the set is not empty, returns an element. *)
  val choose: set -> elt option

  val mem_result2s: elt mem_result -> string

  (* ~no indicates if the "No" answer should be tried. (~no:false is quicker)
   * @raise No_identifier if get_ids returns an empty list. *)
  val mem : no:bool -> set -> elt -> elt mem_result

  (* - An element is known to be in the set iff there exists an element in the set having a common identifier. 
   * - An element A is known _not_ to be in the set iff A is distinct from all the elements in the set.
   * - An element A is said to be distinct from an element B iff: not (    all the identifiers'natures of B are included in the missing natures of A
   *                                                                   and all the identifiers'natures of A are included in the missing natures of B)
   *
   * - Otherwise, the result is Undetermined. *)
  
  

  (* Merges an element in the set.
   *
   * @raise (Error Elt_not_found) if the object is not already in the set and exist is true.
   * @raise (Error Overwrite) if the object is already in the set and exist is false.
   * @raise (Error Partial) if the element is unsufficiently specified (that is, it is undetermined if it belongs to the set) and exist is true or false.
   * To avoid these exceptions, leave 'exist' unspecified.
   *
   * @raise (Error Unmergeable) if merge fails.
   *
   * force:true  in case of conflict, ignore the conflicting information in the set and replace by the information in the given element.
   * force:false (by default) @raise Unmergeable in case of conflict.
   *
   * If merging has no effet (no new information), it returns the same physical set. *)
  val merge_in: ?exist:bool -> ?force:bool -> set -> elt -> set

  (* Convenience: returns also the merged element. *)
  val merge_in_e: ?exist:bool -> ?force:bool -> set -> elt -> set * elt

  (* Remove an element from the set. 
   * It must be an element found with 'mem' (that is, an element which is _as such_ in the set, with physical equality ==). 
   *
   * @raise (Error Elt_not_found) if the element is not as such in the set. *)
  val remove: set -> elt -> set
  
  (* If you want the elements to appear in a given order, you can provide a compare function, 
   * or, simpler, a comparekey function which returns a key used to sort the elements. 
   * The comparekey does not need to be injective. 
   *
   * @raise Failure "compare" if both compare and comparekey are given. *)
  val fold: ?comparekey:(elt -> 'b) -> ?compare:(elt -> elt -> int) -> set -> 'a -> (elt -> 'a -> 'a) -> 'a

  val lwt_fold: ?comparekey:(elt -> 'b) -> ?compare:(elt -> elt -> int) -> set -> 'a -> (elt -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  
  (* to_string *)
  val set2s: ?comparekey:(elt -> 'b) -> ?compare:(elt -> elt -> int) -> ?title:string -> set -> string

  (* Filter *)
  val filter: set -> (elt -> bool) -> set
  
end


module Make : functor (Elt: ELEMENT) -> S with type elt = Elt.elt


type error =
  (* get_ids returns an empty list of identifiers. *)
  | No_identifier
    
  (* Merging two elements failed. *)
  | Unmergeable of exn

  (* Merging an element in the set overwrites an existing element (and ?exist is false). *)
  | Overwrite
    
  (* An identifier was lost after merging (invariant I is broken). *)
  | Lostid of string list

  (* An element is not found in the set. *)
  | Elt_not_found

  (* An element is unsufficiently specified to determine if it belongs to a set or not. *)
  | Partial

  (* Exception occuring in a function provided by the user. *)
  | User of exn
    
(* string: error message. *)
exception Error of error * string
                   
val error2s : error -> string
