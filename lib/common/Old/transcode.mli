
(*** Maps strings to plain ascii. ***)

(* Known encodings *)
type encoding = Ascii | Latin1 | Utf8 | Unknown of string

(* Print this encoding's name. *)
val to_string : encoding -> string

(* Checks if the given strings is valid in the considered encoding. *)
val is_valid: encoding -> string -> bool

(* Heuristics to guess an encoding, given a string. *)
val guess_encoding : string -> encoding

val guess_file_encoding : filename:string -> encoding

(* Maps a string to a plain-ascii one. *)
val to_ascii : encoding -> string -> string

(* Maps a string to Utf. *)
val to_utf: encoding -> string -> string

(* Maps a string to the given encoding. *)
val from_latin1: encoding -> string -> string
val from_utf: encoding -> string -> string

