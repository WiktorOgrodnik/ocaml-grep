open! Core
open Or_error.Let_syntax

type t = 
  { tokens           : Token.t list
  ; discarded_tokens : Token.t list
  ; current          : Token.t option
  ; idx              : int
  } [@@deriving sexp_of]

let advance parser =
  match parser.tokens, parser.current with
  | [], _            -> { tokens = []
                        ; discarded_tokens = parser.discarded_tokens
                        ; current = None
                        ; idx = parser.idx + 1 }
  | t :: ts, Some cur -> { tokens = ts
                         ; discarded_tokens = cur :: parser.discarded_tokens
                         ; current = Some t
                         ; idx = parser.idx + 1 }
  | t :: ts, None     -> { tokens = ts
                         ; discarded_tokens = parser.discarded_tokens
                         ; current = Some t
                         ; idx = parser.idx + 1 }

(* let back parser =
  match parser.current with
  | None -> failwith "Critical error!"
  | Some cur -> begin
  match parser.discarded_tokens with
  | []       -> { tokens = parser.tokens
                ; discarded_tokens = []
                ; current = None
                ; idx = parser.idx - 1 }
  | t :: ts  -> { tokens = cur :: parser.tokens
                ; discarded_tokens = ts
                ; current = Some t
                ; idx = parser.idx - 1 }
  end *)

(* let get_current parser =
  match parser.current with
  | None -> Or_error.error_string "current not exists"
  | Some current -> Ok current *)

let create_custom_parser tokens current =
  { tokens  = tokens
  ; discarded_tokens = []
  ; current = current
  ; idx = 0
  }

let init tokens =
  advance (create_custom_parser tokens None)

let parse_until_non_literal parser =
  let rec parse_unl_aux parser xs =
    match parser.current with
    | Some Token.LITERAL x -> parse_unl_aux (advance parser) (Token.LITERAL x :: xs)
    | _                    -> Ok (parser, List.rev xs)
  in
  parse_unl_aux parser []

let rec parse_search_for_infix parser =
  let open Token in
  match parser.current with
  | Some OR
  | Some RPAREN -> Ok parser
  | None        -> Or_error.error_string "parse_search_for_infix: None"
  | _           -> parse_search_for_infix (advance parser)

let literal_from_token token =
  match token with
  | Token.LITERAL c -> Ast.LITERAL (Some c)
  | _               -> failwith "token is not literal"

let build_sequence_from_literal_tokens tokens =
  Ok (Ast.SEQUENCE (List.map tokens ~f:literal_from_token))
let rec parse parser =
  (snd (Or_error.ok_exn (parse_group parser)))

and parse_group parser =
  let%bind scout = parse_search_for_infix parser in
  let%bind parser, tree = match scout.current with
  | Some Token.OR     -> parse_alternative parser (advance scout)
  | Some Token.RPAREN -> parse_sequence    parser
  | _ -> Or_error.error_string "parse_node other" in
  Ok (parser, tree)

and parse_alternative parser_left parser_right =
  let%bind _, tree_left = parse_sequence parser_left in
  let%bind parser_right, tree_right = parse_group parser_right in
  Ok (parser_right, Ast.ALTERNATIVE [tree_left; tree_right])

and parse_sequence parser =
  let%bind parser, tokens = parse_until_non_literal parser in
  let%bind tree = build_sequence_from_literal_tokens tokens in
  Ok (parser, tree)
