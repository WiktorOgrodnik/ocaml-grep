open Core

val search: string -> (string -> (int * int) list -> unit) -> string -> unit Or_error.t
