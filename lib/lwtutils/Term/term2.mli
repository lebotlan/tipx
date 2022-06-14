(*** Better terminal interface - uses lambda-term ***)


(* Note, TODO v3 : 
 *
 *  - Mécanisme de marges. Pour éviter de le faire à la main.   Lire Format avant tout de même.
 *  - on ne sait jamais si on doit utiliser (b e) ou ^+ etc cie.  Le mélange des niveaux est perturbant.
 * 
 *  - mécanisme pour gérer des listes
 *  - mécanisme pour être polymorphe (mettre dans une liste des continuations différentes ?)
 *
 *  Remplaçable par Inquire (plus simple à utiliser).

 *
 * Il faudrait une librairie qui s'adapte automatiquement :
 *    si on écrit sur un terminal => en couleur
 *    sinon => ascii normal
 *
 * + facilités pour faire des alignements ("tabulations") voire.. des tableaux
 * + facilités pour faire des marges, ou même des boîtes en général avec bordure en haut, à gauche, etc...
 * + facilités pour faire des arbres ??
 *
 *  Ce serait sympa que ce soit de type string bordel. C'est pénible de devoir mettre un type ad-hoc partout.
 *  mais attention, il faut que la concaténation fonctionne...
 *  bref, no trop se...
 *
 *  On veut what mil colors (pas seulement 16 ou 32).
 *
 *
 * This module does not rely on Format, although it seems to provide similar functionalities.
 * As a matter of fact, I have never succeeded in making Format display indented boxes.
 * 
 *   
*)



module LS = LTerm_style

type styled_text = LTerm_text.t

(* Example:
   Styled.(p
             red "Text"
             fmt blue "Here a %d format" 99
           e)

   Styled.p   prints to a terminal
   Styled.b   builds a styled_text

   Beware, there is a compiler performance issue when using fmt in long continuations.
             
*)

val is_empty: styled_text -> bool

val read_line: LTerm_read_line.prompt -> string -> string Lwt.t
val read_password: LTerm_read_line.prompt -> string Lwt.t

val rgb: int -> int -> int -> LS.color

val fprintf : ('a Lwt.t, unit, Zed_utf8.t, unit Lwt.t) format4 -> 'a Lwt.t
val fprints : styled_text -> unit Lwt.t

(* Unstyled *)
val tos: styled_text -> string

val (!^): string -> styled_text
val (^+): string -> styled_text -> styled_text
val (+^): styled_text -> string -> styled_text
val (++): styled_text -> styled_text -> styled_text

(* sep map sp list *)
val sep: ?max:int -> ('a -> styled_text) -> styled_text -> 'a list -> styled_text

module Styled : sig

  (* Builds a style. *)
  val make: ?from:LS.t -> ?bold:bool -> ?ul:bool -> ?fg:LS.color -> unit -> LS.t

  type 'a acu

  type ('a, 'b) block = 'a acu -> ('a acu -> 'b) -> 'b
  type pblock = { b : 'a 'b . ('a, 'b) block }

  type ('a,'b) constr = 'a acu -> string -> ('a acu -> 'b) -> 'b

  (* With printf-like formats. Do not use too much - this is surprisingly expansive. *)
  type ('a,'b,'c) fconstr = 'a acu -> ('c, unit, string, ('a acu -> 'b) -> 'b) format4 -> 'c
    
  val styled : LS.t -> (_,_) constr
  val fstyled : LS.t -> (_,_,_) fconstr

  (* Beware, the compiler gets very slow when fmt is followed by a long continuation. *)
  val fmt: 'a -> ('a -> string -> 'b) -> ('c, unit, string, 'b) format4 -> 'c
           
  (* b = 'builds a styled_text string' *)
  val b: (styled_text acu -> 'a) -> 'a

  (* p = 'prints to stdout' *)
  val p: (unit Lwt.t acu -> 'a) -> 'a
    
  (* end *)
  val e: 'a acu -> 'a

  (* Inserts already-styled text *)
  val st: 'a acu -> styled_text -> ('a acu -> 'b) -> 'b

  (* Empty block *)
  val empty: ('a,'b) block
  val pempty: pblock

  (* New line *)
  val nl: ('a,'b) block
  val pnl: pblock

  (* Creates a block that can be inserted into a sequence. *)
  val bb: (pblock acu -> 'c) -> 'c

  (* No style *)
  val std: (_,_) constr

  (* Current style *)
  val ins: (_,_) constr

  (* Style with a single fg color. *)
  val fg: int -> int -> int -> (_,_) constr
  val ffg: int -> int -> int -> (_,_,_) fconstr

  (* Standard colors *)
  val black: (_,_) constr
  val lwhite: (_,_) constr (* Luminous white *)
  val white: (_,_) constr
  val lgray: (_,_) constr (* Light gray *)
  val dgray: (_,_) constr (* Dark gray *)

  (* l : luminous 
   * d : dark *)
      
  val red: (_,_) constr
  val lred: (_,_) constr
  val dred: (_,_) constr
      
  val green: (_,_) constr
  val lgreen: (_,_) constr
  val dgreen: (_,_) constr
      
  val yellow: (_,_) constr
  val lyellow: (_,_) constr
  val dyellow: (_,_) constr
      
  val blue: (_,_) constr
  val lblue: (_,_) constr
  val dblue: (_,_) constr

  val orange: (_,_) constr
  val lorange: (_,_) constr
  val dorange: (_,_) constr      
      
  val magenta: (_,_) constr
  val lmagenta: (_,_) constr
  val dmagenta: (_,_) constr
      
  val cyan: (_,_) constr
  val lcyan: (_,_) constr
  val dcyan: (_,_) constr
    
end

    
