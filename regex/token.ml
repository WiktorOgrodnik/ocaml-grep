open Core

type t = 
| LITERAL of char
(* -- Operators *)
| OR
(* | STAR
| PLUS *)
(* -- Brackets*)
| LPAREN
| RPAREN
(* | LBRACE
| RBRACE *)
[@@deriving sexp_of]

let string_of_token = function
  | LITERAL c -> "LITERAL (" ^ String.make 1 c ^ ")"
  | OR        -> "OR"
  (* | STAR      -> "STAR"
  | PLUS      -> "PLUS" *)
  | LPAREN    -> "LPAREN"
  | RPAREN    -> "RPAREN"
  (* | LBRACE    -> "LBRACE"
  | RBRACE    -> "RBRACE" *)