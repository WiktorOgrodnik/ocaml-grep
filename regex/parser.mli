type t = 
  { tokens  : Token.t list
  ; current : Token.t option
  }

val advance: t -> t
val init:    Token.t list -> t

val parse_until_non_literal: t -> t, Token.t list Or_error.t

