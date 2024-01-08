open Base

type t = 
  { tokens  : Token.t list
  ; current : Token.t option
  }

let ( let* ) = Or_error.bind

let advance parser =
  match parser.tokens with
  | t :: ts -> { tokens = ts; current = Some t; }
  | []      -> { tokens = []; current = None  ; }

let init tokens =
  let parser = 
  { tokens  = tokens
  ; current = None
  } in
  advance parser

let parse_until_non_literal parser =
  let rec parse_unl_aux parser xs =
    match parser.current with
    | Some Token.LITERAL x -> parse_unl_aux (advance parser) (Token.LITERAL x :: xs)
    | _                    -> Ok (parser, List.rev xs)
  in
  parse_unl_aux parser []

(* let rec parse parser =
  parse_node parser

and parse_node parser =
  let* parser, tokens = parse_until_non_literal

and parse_node parser =
  match parser.current with
  | Some Token.LPAREN -> parse_sequence
  | _                 -> Error "Nie poprawny node"

and parse_sequence parser =
  match advance parser with
  | Token.LITERAL c -> Ok (parse_until_non_literal parser) **)
  
