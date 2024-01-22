type t

val advance    : t -> t
val init       : string -> t
val next_char  : t -> (t * Token.t option)

val gen_tokens : string -> Token.t list
