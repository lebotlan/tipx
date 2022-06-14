open Lwt.Infix
module LS = LTerm_style
module LT = LTerm_text

type styled_text = LT.t

let term = Lazy.force LTerm.stdout


let empty = [| |]

let is_empty a = a = empty

let rgb = LS.rgb

let fprintf fmt =
  let%lwt term = term in
  LTerm.fprintf term fmt
    
let fprints s =
  let%lwt term = term in
  LTerm.fprints term s

let tos s = Zed_string.to_utf8 (LTerm_text.to_string s)

let (!^) = LT.of_utf8
let (^+) a b = Array.append !^a b
let (+^) a b = Array.append a !^b
let (++) a b = Array.append a b

let rec sep ?(max=max_int) map sp = function
  | [] -> empty
  | [x] -> map x
  | x :: xs ->
    if max <= 0 then map x ++ sp +^ ".." ++ sp ++ map (Common.list_last xs)
    else map x ++ sp ++ sep ~max:(max-1) map sp xs

module Styled =
struct

  (* Styles *)
  let make ?(from=LS.none) ?bold ?ul ?fg () =
    { from with
      bold = if bold = None then from.bold else bold ;
      underline = if ul = None then from.underline else ul ;
      foreground = if fg = None then from.foreground else fg }

  (* Building styled text *)

  type 'a acu = {
    current_style: LS.t ;
    text: styled_text list ;
    final: styled_text -> 'a
  }

  type ('a, 'b) block = 'a acu -> ('a acu -> 'b) -> 'b
  type pblock = { b : 'a 'b . ('a, 'b) block }
                
  type ('a,'b) constr = 'a acu -> string -> ('a acu -> 'b) -> 'b
  type ('a,'b,'c) fconstr = 'a acu -> ('c, unit, string, ('a acu -> 'b) -> 'b) format4 -> 'c

  let styled style acu s k = k { acu with current_style = style ; text = LT.stylise s style :: acu.text }
  let fstyled style acu fmt = Printf.ksprintf (fun s k -> k { acu with current_style = style ; text = LT.stylise s style :: acu.text }) fmt

  let std acu s k = k { acu with text = LT.of_utf8 s :: acu.text }

  let ins acu s k = k { acu with text = LT.stylise s acu.current_style :: acu.text }

  let st acu stext k = k { acu with text = stext :: acu.text }
  
  let fmt acu cons fmt = Printf.ksprintf (fun s -> cons acu s) fmt

  let b k = k { text = [] ; current_style = LS.none ; final = (fun s -> s) }

  let p k = k { text = [] ; current_style = LS.none ; final = (fun s -> let%lwt term = term in LTerm.fprints term s) }

  let e acu = acu.final (Array.concat (List.rev acu.text))

  let bb k = k { text = [] ; current_style = LS.none ; final = (fun s -> { b = fun acu k' -> k' { acu with text = s :: acu.text }}) }
  
  let empty acu k = k acu
  let pempty = { b = empty }

  let nl acu k = std acu "\n" k
  let pnl = { b = nl }
  
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

  let orange acu = fg 255 162 0 acu
  let lorange acu = fg 255 233 83 acu
  let dorange acu = fg 229 149 12 acu
  
  let green acu = fg 0 190 0 acu
  let lgreen acu = fg 0 255 0 acu
  let dgreen acu = fg 0 120 0 acu

  let yellow  acu = fg 205 205 0 acu
  let lyellow acu = fg 255 255 0 acu
  let dyellow acu = fg 130 130 0 acu

  let blue acu = fg 0 60 200 acu
  let lblue acu = fg 155 200 255 acu
  let dblue acu = fg 0 0 150 acu

  let magenta acu = fg 205 0 205 acu
  let lmagenta acu = fg 255 0 255 acu
  let dmagenta acu = fg 130 0 130 acu

  let cyan acu = fg 0 195 195 acu
  let lcyan acu = fg 0 255 255 acu
  let dcyan acu = fg 0 130 130 acu

end

open Lwt_react

class read_line prompt init_string term = object(self)
  inherit LTerm_read_line.read_line () 
  inherit [Zed_string.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    self#set_prompt (S.const prompt) ;
    Zed_edit.insert self#context (Zed_rope.of_string init_string)
end

let read_line prompt init_string =
  let%lwt term = term in
  (new read_line prompt (Zed_string.of_utf8 init_string) term)#run
  >|= Zed_string.to_utf8

let pwstyle = Styled.make ~bold:true ~fg:(rgb 255 0 255) ()

class read_password prompt term = object(self)
  inherit LTerm_read_line.read_password () as super
  inherit [Zed_string.t] LTerm_read_line.term term

  method! stylise last =
    let text, pos = super#stylise last in
    let len = Array.length text in
    (LTerm_text.stylise (Text.repeat len "*") pwstyle, pos)
      
  method! send_action = function
    (*    | LTerm_read_line.Break -> () (* Ignore Ctrl+C *) *)
    | action -> super#send_action action
      
  initializer
    self#set_prompt (S.const prompt)
end

let read_password prompt =
  let%lwt term = term in
  (new read_password prompt term)#run >|= Zed_string.to_utf8


