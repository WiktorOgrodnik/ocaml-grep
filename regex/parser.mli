open Core

type t = 
  { tokens           : Token.t list
  ; current          : Token.t option} 
  [@@deriving sexp_of]

val advance              : t -> t
val create_custom_parser : Token.t list -> Token.t option -> t
val init                 : Token.t list -> t

val expect_token             : t -> Token.t -> t Or_error.t
val add_ast_to_sequence      : Ast.group -> Ast.group -> Ast.group Or_error.t
val add_ast_to_alternative   : Ast.group -> Ast.group -> Ast.group Or_error.t

val return_group             : Ast.group -> Ast.group Or_error.t

val parse                    : t -> Ast.group
val parse_sequence           : t -> (t * Ast.group) Or_error.t
val parse_alternative        : t -> (t * Ast.group) Or_error.t
val parse_literal            : t -> (t * Ast.group) Or_error.t
val parse_repeater           : t -> Ast.group -> (t * Ast.group) Or_error.t
