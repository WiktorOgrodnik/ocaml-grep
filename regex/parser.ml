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
  | [], _             -> { tokens = []
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
    | None                 -> Or_error.error_string "parse_until_non_literal - unexpected end"
    | _                    -> Ok (parser, List.rev xs)
  in
  parse_unl_aux parser []

let find_infix_same_nesting_level parser =
  let rec aux nesting parser =
    match parser.current with
    | Some Token.LPAREN              -> aux (nesting + 1) (advance parser)
    | Some Token.RPAREN              -> aux (nesting - 1) (advance parser)
    | Some Token.OR when nesting = 0 -> Ok (Some Token.OR)
    | None                           -> Ok (None)
    | _                              -> aux nesting (advance parser)
  in aux 0 parser

let rec expect_token parser token =
  match parser.current with
  | Some t when (Token.eq t token) -> Ok parser
  | None -> Or_error.error_string "Token not found!"
  | _    -> expect_token (advance parser) token

let literal_from_token token =
  match token with
  | Token.LITERAL c -> Ast.LITERAL (Some c)
  | _               -> failwith "token is not literal"

let add_literal_tokens_to_sequence tokens existing_sequence =
  match existing_sequence with
  | Ast.SEQUENCE lst -> Ok (Ast.SEQUENCE (List.append lst (List.map tokens ~f:literal_from_token)))
  | _                -> Or_error.error_string "non sequence tree passed to build_sequence"

let add_ast_to_sequence sequence ast =
  match sequence with
  | Ast.SEQUENCE lst -> Ok (Ast.SEQUENCE (List.append lst [ast]))
  | _                -> Or_error.error_string "non sequence tree passed to build_sequence"

let add_ast_to_alternative alternative ast =
  match alternative with
  | Ast.ALTERNATIVE lst -> Ok (Ast.ALTERNATIVE (List.append lst [ast]))
  | _                   -> Or_error.error_string "non alternative tree passed to build_alternative"

let return_group sequence =
  match sequence with
  | Ast.ALTERNATIVE lst
  | Ast.SEQUENCE    lst -> begin match lst with
    | []                -> Or_error.error_string "Empty Sequence or Alternative"
    | x :: []           -> Ok x
    | _                 -> Ok sequence
  end
  | _                   -> Or_error.error_string "Return group: expected Ast.SEQUENCE or Ast.ALTERNATIVE"

let rec parse parser =
  let result = parse_group parser in
  match result with
  | Ok (_, tree) -> tree
  | Error err -> 
    print_endline (Error.to_string_hum err);
    (Ast.SEQUENCE [])

and parse_group parser =
  let%bind token = find_infix_same_nesting_level parser in
  match token with
  | Some Token.OR -> parse_alternative parser
  | None          -> parse_sequence    parser
  | _             -> Or_error.error_string "Parse group: infix operator - unknown token"

and parse_sequence parser =
  let tree = Ast.SEQUENCE [] in
  let rec parse_sequence_aux parser tree =
    let%bind parser, tokens = parse_until_non_literal parser in
    match parser.current with
    | Some Token.LPAREN ->
        let%bind tree              = add_literal_tokens_to_sequence tokens tree  in
        let%bind _                 = expect_token parser Token.RPAREN            in
        let%bind parser, tree_part = parse_group (advance parser)                in
        let%bind tree              = add_ast_to_sequence tree tree_part          in
        parse_sequence_aux (advance parser) tree
    | Some Token.OR (* Only happens when sequence is demaned by alternative *)
    | Some Token.RPAREN ->
        let%bind tree              = add_literal_tokens_to_sequence tokens tree in
        Ok (parser, tree)
    | _ -> Or_error.error_string "Parse sequence: non literal token - unknown token"
  in
  let%bind parser, tree = parse_sequence_aux parser tree in
  let%bind tree         = return_group tree              in
  Ok (parser, tree)

and parse_alternative parser =
  let tree = Ast.ALTERNATIVE [] in
  let rec parse_alternative_aux parser tree =
    let%bind parser, tree_part = parse_sequence parser in
    let%bind tree              = add_ast_to_alternative tree tree_part in
    match parser.current with
    | Some Token.OR -> parse_alternative_aux (advance parser) tree
    | Some Token.RPAREN -> Ok (parser, tree)
    | _                 -> Or_error.error_string "Parse alternative: non literal token - unknown token"
  in
  let%bind parser, tree = parse_alternative_aux parser tree in
  let%bind tree         = return_group tree                 in
  Ok (parser, tree)
