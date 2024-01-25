val select_from_alt : Ast.t -> Ast.t Choice.t

val search_ast      : string -> Ast.t -> int -> int Choice.t
val search_literal  : string -> Ast.t -> int -> int Choice.t
val search_sequence : string -> Ast.t -> int -> int Choice.t
val search_alt      : string -> Ast.t -> int -> int Choice.t
val search_repeater : string -> Ast.t -> int -> int Choice.t

val search          :  string -> Ast.t -> (int * int) list
