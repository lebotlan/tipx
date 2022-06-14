type style = ANSITerminal.style list
    
(* Example of style: [ A.Bold ; A.red ] 
 * mysprintf does not apply the style when stdout is not a tty. *)
val mysprintf: style -> ('a, unit, string) format -> 'a
val dtos:  style -> string -> string

(* Like dtos, but always apply the style. *)
val mytos: style -> string -> string

val default_style: style
  
val err_style: style
val valid_style: style
val dimmed_style: style
val title_style: style
val comment_style: style
val highl_style: style
  
type 'a lwtkont = ('a, unit, string, unit Lwt.t) format4 -> 'a

(* Prints *)
val display: style -> 'a lwtkont

val display_error: 'a lwtkont
val display_valid: 'a lwtkont
val display_title: 'a lwtkont
val display_comment: 'a lwtkont
    
  

(* Set echo OFF or ON on stdin. *)
val set_echo: bool -> unit Lwt.t
