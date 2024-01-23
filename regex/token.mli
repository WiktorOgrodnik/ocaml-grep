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
(* | LBRACE *)
(* | RBRACE *)
| LCLAM
| RCLAM
(* -- Other *)
| COMMA
[@@deriving sexp_of, eq]

val string_of_token : t -> string
val eq              : t -> t -> bool
