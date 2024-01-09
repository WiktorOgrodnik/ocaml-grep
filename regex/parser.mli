open Core

type t = 
  { tokens           : Token.t list
  ; discarded_tokens : Token.t list
  ; current          : Token.t option
  ; idx              : int
  } [@@deriving sexp_of]

val advance: t -> t
val create_custom_parser: Token.t list -> Token.t option -> t
val init:    Token.t list -> t

val parse_until_non_literal : t -> (t * Token.t list) Or_error.t
val expect_token            : t -> Token.t -> t Or_error.t
val literal_from_token      : Token.t -> Ast.group

val add_literal_tokens_to_sequence : Token.t list -> Ast.group -> Ast.group Or_error.t
val add_ast_to_sequence            : Ast.group -> Ast.group -> Ast.group Or_error.t

val parse                    : t -> Ast.group
val parse_sequence           : t -> (t * Ast.group) Or_error.t
