module A = ANSITerminal

type style = ANSITerminal.style list

type 'a lwtkont = ('a, unit, string, unit Lwt.t) format4 -> 'a

let is_color_tty = Unix.isatty Unix.stdout

let mysprintf =
  if is_color_tty then fun style format -> A.sprintf style format
  else fun _style format -> Printf.sprintf format

let mytos style s = A.sprintf style "%s" s
let dtos style s = mysprintf style "%s" s

let err_style = [ A.Bold ; A.red ]
let valid_style = [ A.Bold ; A.green ]
let title_style = [ A.Bold ; A.white ]
let comment_style = [ A.cyan ]
let highl_style = [ A.white ]
let default_style = [ A.default ]
let dimmed_style = [ A.blue ]

let display style fmt = Printf.ksprintf (fun s -> Lwt_io.print (mysprintf style "%s" s)) fmt

let display_error fmt = display err_style fmt
let display_valid fmt = display valid_style fmt
let display_title fmt = display title_style fmt
let display_comment fmt = display comment_style fmt


let set_echo flag =
  let open Lwt_unix in
  let%lwt tio = tcgetattr stdin in
  tcsetattr stdin TCSAFLUSH { tio with c_echo = flag }
