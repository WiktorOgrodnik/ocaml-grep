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

val string_of_token: t -> string