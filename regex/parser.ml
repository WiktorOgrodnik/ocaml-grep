open Core
open Ast
open Or_error.Let_syntax

type t = 
  { tokens           : Token.t list
  ; current          : Token.t option
  } [@@deriving sexp_of]

let advance parser =
  match parser.tokens with
  | []      -> { tokens = []
               ; current = None}
  | t :: ts -> { tokens = ts
               ; current = Some t}

let create_custom_parser tokens current =
  { tokens  = tokens
  ; current = current}

let init tokens =
  advance (create_custom_parser tokens None)

let find_infix_same_nesting_level parser =
  let rec aux nesting parser =
    match parser.current with
    | Some Token.LPAREN              -> aux (nesting + 1) (advance parser)
    | Some Token.RPAREN              -> aux (nesting - 1) (advance parser)
    | Some Token.OR when nesting = 0 -> Ok (Some Token.OR)
    | None                           -> Ok (None)
    | _                              -> aux nesting (advance parser)
  in aux 0 parser

let is_current_token_repeater parser =
  match parser.current with
  | Some Token.STAR
  | Some Token.PLUS -> true
  | _               -> false

let rec expect_token parser token =
  match parser.current with
  | Some t when (Token.eq t token) -> Ok parser
  | None -> Or_error.error_string "Token not found!"
  | _    -> expect_token (advance parser) token

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
    Ast.INVALID

and parse_group parser =
  let%bind token = find_infix_same_nesting_level parser in
  match token with
  | Some Token.OR -> parse_alternative parser
  | None          -> parse_sequence    parser
  | _             -> Or_error.error_string "Parse group: infix operator - unknown token"

and parse_sequence parser =
  let tree = Ast.SEQUENCE [] in
  let rec parse_sequence_aux parser tree =
    match parser.current with
    | Some Token.LITERAL _ ->
        let%bind parser, tree_part = parse_literal parser                                 in
        let%bind tree              = add_ast_to_sequence tree tree_part                   in
        parse_sequence_aux (advance parser) tree
    | Some Token.LPAREN ->
        let%bind _                 = expect_token parser Token.RPAREN                     in
        let%bind parser, tree_part = parse_group (advance parser)                         in
        let%bind tree              = add_ast_to_sequence tree tree_part                   in
        parse_sequence_aux (advance parser) tree
    | Some Token.OR (* Only happens when sequence is demanded by alternative *)
    | Some Token.RPAREN -> Ok (parser, tree)
    | _ -> Or_error.error_string "Parse sequence: unknown token"
  in
  let%bind parser, tree = parse_sequence_aux parser tree in
  let%bind tree         = return_group tree              in
  parse_repeater parser tree

and parse_alternative parser =
  let tree = Ast.ALTERNATIVE [] in
  let rec parse_alternative_aux parser tree =
    let%bind parser, tree_part = parse_sequence parser                  in
    let%bind tree              = add_ast_to_alternative tree tree_part  in
    match parser.current with
    | Some Token.OR     -> parse_alternative_aux (advance parser) tree
    | Some Token.RPAREN -> Ok (parser, tree)
    | _                 -> Or_error.error_string "Parse alternative: unknown token"
  in
  let%bind parser, tree = parse_alternative_aux parser tree in
  let%bind tree         = return_group tree                 in
  parse_repeater parser tree

and parse_literal parser =
  let%bind parser, tree = match parser.current with
  | Some Token.LITERAL c -> Ok (parser, Ast.LITERAL (Some c))
  | _                    -> Or_error.error_string "Parse literal: unknown token"
  in
  parse_repeater parser tree

and parse_repeater parser tree =
  let parser_repeater_aux parser =
    match parser.current with
    | Some Token.STAR -> Ok (parser, Ast.REPEATER (tree, { l = Some 0
                                                         ; r = None }))
    | Some Token.PLUS -> Ok (parser, Ast.REPEATER (tree, { l = Some 1
                                                         ; r = None }))
    | _               -> Or_error.error_string "Parse repeater: unknown token"
  in
  match is_current_token_repeater (advance parser) with
  | true  -> parser_repeater_aux (advance parser)
  | false -> Ok (parser, tree)

;;
