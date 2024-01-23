open! Core

type t = 
| LITERAL of char
(* -- Operators *)
| OR
| STAR
| PLUS
(* -- Brackets*)
| LPAREN
| RPAREN
(* | LBRACE *)
(* | RBRACE *)
| LCLAM
| RCLAM
(* -- Others*)
| COMMA
[@@deriving sexp_of, eq]

let string_of_token = function
  | LITERAL c -> "LITERAL (" ^ String.make 1 c ^ ")"
  | OR        -> "OR"
  | STAR      -> "STAR"
  | PLUS      -> "PLUS"
  | LPAREN    -> "LPAREN"
  | RPAREN    -> "RPAREN"
  (* | LBRACE    -> "LBRACE" *)
  (* | RBRACE    -> "RBRACE" *)
  | LCLAM     -> "LCLAM"
  | RCLAM     -> "RCLAM"
  | COMMA     -> "COMMA"

let eq a b =
  match a, b with
  | OR, OR
  | PLUS, PLUS
  | STAR, STAR
  | LPAREN, LPAREN
  | RPAREN, RPAREN
  | LCLAM, LCLAM
  | RCLAM, RCLAM
  | COMMA, COMMA -> true
  | LITERAL c1, LITERAL c2 when int_of_char c1 = int_of_char c2 -> true
  | _ -> false
