open Core

type t = 
  { tokens           : Token.t list
  ; current          : Token.t option} 
  [@@deriving sexp_of]

val advance              : t -> t
val create_custom_parser : Token.t list -> Token.t option -> t
val init                 : Token.t list -> t

val find_infix_same_nesting_level : t -> Token.t option Or_error.t
val is_current_token_repeater     : t -> bool
val expect_token                  : t -> Token.t -> t Or_error.t

val add_ast_to_sequence    : Ast.group -> Ast.group -> Ast.group Or_error.t
val add_ast_to_alternative : Ast.group -> Ast.group -> Ast.group Or_error.t
val add_number_to_repeater : Ast.group -> int -> Ast.group Or_error.t

val repeater_is_comma_legal : Ast.group -> unit Or_error.t

val normalize_clam_repeater : Ast.group -> Ast.group Or_error.t
val return_group            : Ast.group -> Ast.group Or_error.t
val skip_whitespaces        : t -> t

val parse_group         : t -> (t * Ast.group) Or_error.t
val parse_sequence      : t -> bool -> (t * Ast.group) Or_error.t
val parse_alternative   : t -> (t * Ast.group) Or_error.t
val parse_literal       : t -> (t * Ast.group) Or_error.t
val parse_repeater      : t -> Ast.group -> (t * Ast.group) Or_error.t
val parse_clam_repeater : t -> Ast.group -> (t * Ast.group) Or_error.t
val parse_number        : t -> (t * int) Or_error.t

val parse_einv               : t -> Ast.group
val parse                    : t -> Ast.group Or_error.t
