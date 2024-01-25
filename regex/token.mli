open! Core

type t = 
| LITERAL of char
| DOT
(* -- Operators *)
| OR
| STAR
| PLUS
| QMARK
(* -- Brackets *)
| LPAREN
| RPAREN
| LBRACE
| RBRACE
| LCLAM
| RCLAM
(* -- Other *)
| COMMA
| DASH
[@@deriving sexp_of]

val to_string        : t -> string
val to_string_option : t option -> string
val eq               : t -> t -> bool
