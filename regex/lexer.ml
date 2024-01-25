open Core

type t = 
  { input   : string
  ; pos     : int
  ; current : char option }

let advance lxr =
  let is_end = lxr.pos >= String.length lxr.input in
  let next   = match is_end with
  | true  -> None
  | false -> Some (String.get lxr.input lxr.pos)
  in 
  { input   = lxr.input
  ; pos     = lxr.pos + 1
  ; current = next }

let init i =
  let lxr = 
    { input   = i
    ; pos     = 0
    ; current = None } 
  in
  advance lxr

let next_char lxr = match lxr.current with
| None -> (lxr, None)
| Some c -> match c with
  | '|'    -> (advance lxr, Some Token.OR)
  | '*'    -> (advance lxr, Some Token.STAR)
  | '+'    -> (advance lxr, Some Token.PLUS)
  | '('    -> (advance lxr, Some Token.LPAREN)
  | ')'    -> (advance lxr, Some Token.RPAREN)
  | '['    -> (advance lxr, Some Token.LBRACE)
  | ']'    -> (advance lxr, Some Token.RBRACE)
  | '{'    -> (advance lxr, Some Token.LCLAM)
  | '}'    -> (advance lxr, Some Token.RCLAM)
  | ','    -> (advance lxr, Some Token.COMMA)
  | '?'    -> (advance lxr, Some Token.QMARK)
  | '.'    -> (advance lxr, Some Token.DOT)
  | '-'    -> (advance lxr, Some Token.DASH)
  | '^'    -> (advance lxr, Some Token.CARET)
  | '\\'   -> begin
      let lxr = advance lxr in (* skip \ and treat next char as literal *)
      match lxr.current with
      | None   -> (lxr, None) (* backslash on the end of file treat as non existing *)
      | Some c -> (advance lxr, Some (Token.LITERAL c))
    end
  | c      -> (advance lxr, Some (Token.LITERAL c))

let gen_tokens i =
  let lxr = init i in
  let rec gen lxr ts =
    match next_char lxr with
    | (_, None) -> List.rev_append ts [Token.RPAREN]
    | (l, Some t) -> gen l (t :: ts)
  in 
  let tokesn = gen lxr [] in
  match tokesn with
  | Token.RPAREN :: [] -> []
  | tok -> tok
