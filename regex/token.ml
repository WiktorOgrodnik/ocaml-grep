open! Core

type t = 
| LITERAL of char
| DOT
(* -- Operators *)
| OR
| STAR
| PLUS
| QMARK
(* -- Brackets*)
| LPAREN
| RPAREN
| LBRACE
| RBRACE
| LCLAM
| RCLAM
(* -- Others*)
| COMMA
| DASH
[@@deriving sexp_of]

let to_string = function
  | LITERAL c -> "LITERAL (" ^ String.make 1 c ^ ")"
  | DOT       -> "DOT"
  | OR        -> "OR"
  | STAR      -> "STAR"
  | PLUS      -> "PLUS"
  | QMARK     -> "QMRARK"
  | LPAREN    -> "LPAREN"
  | RPAREN    -> "RPAREN"
  | LBRACE    -> "LBRACE"
  | RBRACE    -> "RBRACE"
  | LCLAM     -> "LCLAM"
  | RCLAM     -> "RCLAM"
  | COMMA     -> "COMMA"
  | DASH      -> "DASH"

let to_string_option token =
  match token with
  | Some token -> to_string token
  | None       -> "(No token)!"

let eq a b =
  match a, b with
  | OR, OR
  | DOT, DOT
  | PLUS, PLUS
  | STAR, STAR
  | QMARK, QMARK
  | LPAREN, LPAREN
  | RPAREN, RPAREN
  | LBRACE, LBRACE
  | RBRACE, RBRACE
  | LCLAM, LCLAM
  | RCLAM, RCLAM
  | COMMA, COMMA
  | DASH, DASH -> true
  | LITERAL c1, LITERAL c2 when int_of_char c1 = int_of_char c2 -> true
  | _ -> false
