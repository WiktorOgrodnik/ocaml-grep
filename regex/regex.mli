open Core

val compile: string -> Ast.t Or_error.t
val search: string -> (string -> (int * int) list -> unit) -> Ast.t -> unit
