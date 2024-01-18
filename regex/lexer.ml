open Core

type t = {
  inp: string;
  pos: int;
  ch: char option;
}

let read_char lxr =
  let read_to_end = lxr.pos >= String.length(lxr.inp) in
  let nch =
    if read_to_end then None else Some (String.get lxr.inp lxr.pos) in
  {
    lxr with
    pos = lxr.pos + 1;
    ch = nch
  }

let init i =
  let lxr = {
    inp = i;
    pos = 0;
    ch = None;
  } in
  read_char lxr

let next_char lxr = match lxr.ch with
| None -> (lxr, None)
| Some c -> match c with
  | '|'    -> (read_char lxr, Some Token.OR)
  | '*'    -> (read_char lxr, Some Token.STAR)
  | '+'    -> (read_char lxr, Some Token.PLUS)
  | '('    -> (read_char lxr, Some Token.LPAREN)
  | ')'    -> (read_char lxr, Some Token.RPAREN)
  (* | '['    -> (read_char lxr, Some Token.LBRACE) *)
  (* | ']'    -> (read_char lxr, Some Token.RBRACE) *)
  (* | '{'    -> (read_char lxr, Some Token.LCLAM) *)
  (* | '}'    -> (read_char lxr, Some Token.RCLAM) *)
  | '\\'   -> begin
      let lxr = read_char lxr in (* skip \ and treat next char as literal *)
      match lxr.ch with
      | None   -> (lxr, None) (* backslash on the end of file treat as non existing *)
      | Some c -> (read_char lxr, Some (Token.LITERAL c))
    end
  | c      -> (read_char lxr, Some (Token.LITERAL c))

let gen_tokens i =
  let lxr = init i in
  let rec gen lxr ts =
    match next_char lxr with
    | (_, None) -> List.rev_append ts [Token.RPAREN]
    | (l, Some t) -> gen l (t :: ts)
  in gen lxr []
