(* Test de lambda-term *)


(* Idées pour une API moins LOURDINGUE *)

(*
 * - Gestion des styles : proposer des constructeurs.
 * - API sympa pour construire des messages avec plusieurs styles.
 *
 *
 *)


(********************* API  ************************)
module LT = LTerm_text
module LS = LTerm_style

type styled_text = LT.t

module Styled : sig
  val make: ?from:LS.t -> ?bold:bool -> ?ul:bool -> ?fg:LS.color -> unit -> LS.t

  type 'a acu

  type ('a,'b) constr = 'a acu -> string -> ('a acu -> 'b) -> 'b

  (* With printf-like formats. Do not use too much - this is surprisingly expansive. *)
  type ('a,'b,'c) fconstr = 'a acu -> ('c, unit, string, ('a acu -> 'b) -> 'b) format4 -> 'c
    
  val styled : LS.t -> (_,_) constr
  val fstyled : LS.t -> (_,_,_) fconstr

  val fmt: 'a -> ('a -> string -> 'b) -> ('c, unit, string, 'b) format4 -> 'c
           
  (* b = 'builds a string' *)
  val b: (styled_text acu -> 'a) -> 'a

  (* p = 'prints to a term' *)
  val p: LTerm.t -> (unit Lwt.t acu -> 'a) -> 'a
    
  (* end *)
  val e: 'c acu -> 'c

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
      
  val magenta: (_,_) constr
  val lmagenta: (_,_) constr
  val dmagenta: (_,_) constr
      
  val cyan: (_,_) constr
  val lcyan: (_,_) constr
  val dcyan: (_,_) constr
    
end
=
struct

  (* Styles *)
  let make ?(from=LS.none) ?bold ?ul ?fg () =
    { from with
      bold = if bold = None then from.bold else bold ;
      underline = if ul = None then from.underline else ul ;
      foreground = if fg = None then from.foreground else fg }
  

  (* Building styled text *)
  
  type 'a acu = {
    text: styled_text list ;
    final: styled_text -> 'a
  }
  
  type ('a,'b) constr = 'a acu -> string -> ('a acu -> 'b) -> 'b
  type ('a,'b,'c) fconstr = 'a acu -> ('c, unit, string, ('a acu -> 'b) -> 'b) format4 -> 'c
    
  let styled style acu s k = k { acu with text = LT.stylise s style :: acu.text }
  let fstyled style acu fmt = Printf.ksprintf (fun s k -> k { acu with text = LT.stylise s style :: acu.text }) fmt

  let fmt acu cons fmt = Printf.ksprintf (fun s -> cons acu s) fmt
      
  let b k = k { text = [] ; final = (fun s -> s) }

  let p term k = k { text = [] ; final = (fun s -> LTerm.fprints term s) }

  let e acu = acu.final (Array.concat (List.rev acu.text))

  let fg r g b acu = styled (make ~fg:(LS.rgb r g b) ()) acu
  let ffg r g b acu = fstyled (make ~fg:(LS.rgb r g b) ()) acu
  
  let black acu = fg 0 0 0 acu
  let lwhite acu = fg 255 255 255 acu
  let white acu = fg 230 230 230 acu
  let lgray acu = fg 180 180 180 acu
  let dgray acu = fg 80 80 80 acu
      
  let red  acu = fg 200 0 0 acu
  let lred acu = fg 255 0 0 acu
  let dred acu = fg 120 0 0 acu

  let green acu = fg 0 190 0 acu
  let lgreen acu = fg 0 255 0 acu
  let dgreen acu = fg 0 120 0 acu

  let yellow  acu = fg 205 205 0 acu
  let lyellow acu = fg 255 255 0 acu
  let dyellow acu = fg 130 130 0 acu

  let blue acu = fg 0 60 200 acu
  let lblue acu = fg 0 0 255 acu
  let dblue acu = fg 0 0 150 acu

  let magenta acu = fg 205 0 205 acu
  let lmagenta acu = fg 255 0 255 acu
  let dmagenta acu = fg 130 0 130 acu

  let cyan acu = fg 0 195 195 acu
  let lcyan acu = fg 0 255 255 acu
  let dcyan acu = fg 0 130 130 acu

end


(********************* INIT ************************)


open LTerm
open LTerm_geom
open LTerm_style
open Lwt_react

let run = Lwt_main.run

let term = run (Lazy.force stdout)


let () = run (fprintlf term "#Colors: %d, size = %s\n" (colors term) (string_of_size (size term)))

    (* Print colored text. *)
let () = run (Styled.(p term
                        black "= Black =\n"
                        white "= White =\n"
                        lwhite "= Luminous white =\n"
                        lgray "= Light gray =\n"
                        dgray "= Dark gray =\n"

                        red "= Red =\n"
                        lred "= Luminous Red =\n"
                        dred "= Dark Red =\n"

                        green "= Green =\n" 
                        lgreen "= Luminous Green =\n"
                        dgreen "= Dark Green =\n"

                        yellow "= Yellow =\n"
                        lyellow "= Luminous Yellow =\n"
                        dyellow "= Dark Yellow =\n"

                        blue "= Blue =\n"
                        lblue "= Luminous Blue =\n"
                        dblue "= Dark Blue =\n"

                        magenta "= Magenta =\n"
                        lmagenta "= Luminous Magenta =\n"
                        dmagenta "= Dark Magenta =\n"

                        cyan "= Cyan =\n"
                        lcyan "= Luminous Cyan =\n"
                        dcyan "= Dark Cyan =\n"

                        e))

let degrade r g b =
  let rec loop k =
    if k < 0 then Lwt.return_unit
    else
      let x = r * k / 255
      and y = g * k / 255
      and z = b * k / 255
      in
      let%lwt () = Styled.(p term (ffg x y z) " - This text color is %d,%d,%d ! ====\n" x y z e) in
      loop (k-25)
  in
  loop 255

let () = run (degrade 255 0 255)


let redstyle = {
  bold = Some true ;
  underline = None ;
  blink = None ;
  reverse = None ;
  foreground = Some red ;
  background = None ;
}

(********************* PASSWORD ************************)

(* Lourdingue *)
class read_password term = object(self)
  inherit LTerm_read_line.read_password () as super
  inherit [Zed_string.t] LTerm_read_line.term term

  method! stylise last =
    let text, pos = super#stylise last in
    let len = Array.length text in
    (LTerm_text.stylise (Text.repeat len "*") redstyle, pos)
      
  method! send_action = function
    | LTerm_read_line.Break -> () (* Ignore Ctrl+C *)
    | action -> super#send_action action
      
  initializer
    self#set_prompt (S.const (LTerm_text.of_utf8 "Type a password: "))
end


let readp = new read_password term

let tt = Zed_string.to_utf8 (run (readp#run))

let () = run (fprintlf term "Read password : %s \n" tt)



(********************* EDIT SIMPLE ************************)

class read_line term prompt init_string = object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_string.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    self#set_prompt (S.const prompt) ;
    Zed_edit.insert self#context (Zed_rope.of_string (Zed_string.of_utf8 init_string))
end

let bold_red acu = Styled.(styled (make ~fg:LS.(rgb 100 100 100) ~bold:true ())) acu


let readl = new read_line term Styled.(b blue "You may ok "
                                         fmt lred "really %f " 99.0
                                         bold_red " any difference ? "
                                         fmt red "want %b %b" true false 
                                         lblue " to type : " e) "Something here."


let ll = Zed_string.to_utf8 (run readl#run)

let () = run (fprintlf term "Read line : %s \n" ll)
    

