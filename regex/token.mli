open! Core

type t = 
| LITERAL of char
(* -- Operators *)
| OR
(* | STAR *)
(* | PLUS *)
(* -- Brackets *)
| LPAREN
| RPAREN
(* | LBRACE *)
(* | RBRACE *)
[@@deriving sexp_of, eq]

val string_of_token : t -> string
val eq              : t -> t -> bool
