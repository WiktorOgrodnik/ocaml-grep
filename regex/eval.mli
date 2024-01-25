val select_from_alt : Ast.group -> Ast.group Choice.t

val search_ast      : string -> Ast.group -> int -> int Choice.t
val search_literal  : string -> Ast.group -> int -> int Choice.t
val search_sequence : string -> Ast.group -> int -> int Choice.t
val search_alt      : string -> Ast.group -> int -> int Choice.t
val search_repeater : string -> Ast.group -> int -> int Choice.t

val search          :  string -> Ast.group -> (int * int) list
