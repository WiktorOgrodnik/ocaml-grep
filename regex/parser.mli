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

val add_ast_to_sequence    : Ast.t -> Ast.t -> Ast.t Or_error.t
val add_ast_to_alternative : Ast.t -> Ast.t -> Ast.t Or_error.t
val add_number_to_repeater : Ast.t -> int -> Ast.t Or_error.t

val repeater_is_comma_legal : Ast.t -> unit Or_error.t

val normalize_clam_repeater : Ast.t -> Ast.t Or_error.t
val return_group            : Ast.t -> Ast.t Or_error.t
val skip_whitespaces        : t -> t

val parse_group         : t -> (t * Ast.t) Or_error.t
(* val parse_sequence      : t -> bool -> (t * Ast.t) Or_error.t *)
val parse_alternative   : t -> (t * Ast.t) Or_error.t
(* val parse_literal       : t -> ?allow_repeater:bool -> ?allow_dot:bool -> (t * Ast.t) Or_error.t *)
val parse_repeater      : t -> Ast.t -> (t * Ast.t) Or_error.t
val parse_clam_repeater : t -> Ast.t -> (t * Ast.t) Or_error.t
val parse_number        : t -> (t * int) Or_error.t

val parse_einv               : t -> Ast.t
val parse                    : Token.t list -> Ast.t Or_error.t
