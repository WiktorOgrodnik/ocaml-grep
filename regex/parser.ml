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
               ; current = None }
  | t :: ts -> { tokens = ts
               ; current = Some t }

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
  | Some Token.PLUS
  | Some Token.QMARK
  | Some Token.LCLAM -> true
  | _                -> false

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

let add_number_to_repeater tree number =
  match tree with
  | Ast.REPEATER (t, r) ->
      begin match r.l, r.r with
      | None, None -> Ok (Ast.REPEATER (t, { l = Some number
                                           ; r = None }))
      | Some a, None ->
          if a > number then 
            Or_error.error_string "Parse clam repeater: left bound is greater then right bound"
          else
            Ok (Ast.REPEATER (t, { l = Some a
                                 ; r = Some number }))
      | _ -> Or_error.error_string "Parse clam repeater: to many numbers in this structure"
     end
  | _ -> Or_error.error_string "Parse clam repeater: illeagal Ast passed to the function add_number_to_repeater"

let repeater_is_comma_legal tree =
  match tree with
  | Ast.REPEATER (_, r) ->
      begin match r.r with
      | Some _ -> Or_error.error_string "Parse clam repeater: To many commas in this structure"
      | None   -> Ok(())
      end
  | _ -> Or_error.error_string "Parse clam repeater: illeagal Ast passed to the function repeater_is_comma_legal"

let normalize_clam_repeater tree =
  match tree with
  | Ast.REPEATER (t, r) ->
      begin match r.r with
      | Some _ -> Ok tree
      | None   -> Ok (Ast.REPEATER (t, { l = r.l
                                       ; r = r.l }))
      end
  | _ -> Or_error.error_string "Parse clam repeater: illeagal Ast passed to the function repeater_is_comma_legal"

let return_group sequence =
  match sequence with
  | Ast.ALTERNATIVE lst
  | Ast.SEQUENCE    lst -> begin match lst with
    | []                -> Or_error.error_string "Empty Sequence or Alternative"
    | x :: []           -> Ok x
    | _                 -> Ok sequence
  end
  | _                   -> Or_error.error_string "Return group: expected Ast.SEQUENCE or Ast.ALTERNATIVE"

let rec skip_whitespaces parser =
  match parser.current with
  | Some Token.LITERAL c when Char.is_whitespace c -> skip_whitespaces (advance parser)
  | _ -> parser

let rec parse_group parser =
  let%bind token = find_infix_same_nesting_level parser in
  match token with
  | Some Token.OR -> parse_alternative parser
  | None          -> parse_sequence parser false
  | _             -> Or_error.error_string "Parse group: infix operator - unknown token"

and parse_sequence parser from_or =
  let tree = Ast.SEQUENCE [] in
  let rec parse_sequence_aux parser tree =
    match parser.current with
    | Some Token.DOT
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
  match from_or with
  | true  -> Ok (parser, tree)
  | false -> parse_repeater parser tree

and parse_alternative parser =
  let tree = Ast.ALTERNATIVE [] in
  let rec parse_alternative_aux parser tree =
    let%bind parser, tree_part = parse_sequence parser true             in
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
  | Some Token.DOT       -> Ok (parser, Ast.LITERAL None)
  | _                    -> Or_error.error_string "Parse literal: unknown token"
  in
  parse_repeater parser tree

and parse_repeater parser tree =
  let parser_repeater_aux parser =
    match parser.current with
    | Some Token.STAR   -> Ok (parser, Ast.REPEATER (tree, { l = Some 0
                                                           ; r = None }))
    | Some Token.PLUS   -> Ok (parser, Ast.REPEATER (tree, { l = Some 1
                                                           ; r = None }))
    | Some Token.QMARK  -> Ok (parser, Ast.REPEATER (tree, { l = Some 0
                                                           ; r = Some 1 }))
    | Some Token.LCLAM  ->
        let%bind _ = expect_token parser Token.RCLAM in
        parse_clam_repeater (advance parser) tree
    | _                 -> Or_error.error_string "Parse repeater: unknown token"
  in
  match is_current_token_repeater (advance parser) with
  | true  -> parser_repeater_aux (advance parser)
  | false -> Ok (parser, tree)

and parse_clam_repeater parser tree =
  let tree = Ast.REPEATER (tree, { l = None
                                 ; r = None }) in
  let rec parse_clam_repeater_aux parser tree =
    match parser.current with
    | Some Token.LITERAL _ ->
        let%bind parser, number = parse_number parser                in
        let%bind tree           = add_number_to_repeater tree number in
        parse_clam_repeater_aux parser tree
    | Some Token.COMMA     ->
        let%bind _              = repeater_is_comma_legal tree       in
        parse_clam_repeater_aux (advance parser) tree
    | Some Token.RCLAM     ->
        let%bind tree           = normalize_clam_repeater tree       in
        Ok (parser, tree)
    | _                    -> Or_error.error_string "Parse clam repeater: unknown token"
  in
  parse_clam_repeater_aux parser tree

and parse_number parser =
  let digits_to_number digits =
    let len    = List.length digits                               in
    let seq    = Sequence.to_list (Sequence.range 0 len)          in
    let pows   = List.map seq ~f:(fun a -> Int.( ** ) 10 a)       in 
    let zipp   = List.zip digits pows                             in
    match zipp with
    | Ok a -> Ok (List.fold_right ~f:(fun (a, x) b -> b + a * x) ~init:0 a)
    | Unequal_lengths -> Or_error.error_string "Parse number: zip unequal lengths"
  in
  let rec parse_number_aux parser digits =
    match parser.current with
    | Some (Token.LITERAL d) ->
        begin match Char.get_digit d with
        | Some d -> parse_number_aux (advance parser) (d :: digits)
        | None   -> Or_error.error_string ("Parse number: " ^ String.make 1 d ^ " is not digit")
        end
    | Some Token.RCLAM
    | Some Token.COMMA     -> Ok (parser, digits)
    | _                    -> Or_error.error_string "Parse number: unknown token"
  in
  let%bind parser, digits = parse_number_aux (skip_whitespaces parser) [] in
  let%bind number         = digits_to_number digits    in
  Ok (parser, number)
 
let parse_einv parser =
  let result = parse_group parser in
  match result with
  | Ok (_, tree) -> tree
  | Error err -> 
    print_endline (Error.to_string_hum err);
    Ast.INVALID

let parse parser =
  match parser.current with
  | None   -> Or_error.error_string "Empty pattern!"
  | Some _ ->
    let result = parse_group parser in
    match result with
    | Ok (_, tree) -> Ok tree
    | Error err    -> Error err
