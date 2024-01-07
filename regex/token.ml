type t = 
| LITERAL of char
(* -- Operators *)
| OR
| STAR
| PLUS
(* -- Brackets*)
| LPAREN
| RPAREN
| LBRACE
| RBRACE

let string_of_token = function
  | LITERAL c -> "LITERAL (" ^ String.make 1 c ^ ")"
  | OR        -> "OR"
  | STAR      -> "STAR"
  | PLUS      -> "PLUS"
  | LPAREN    -> "LPAREN"
  | RPAREN    -> "RPAREN"
  | LBRACE    -> "LBRACE"
  | RBRACE    -> "RBRACE"