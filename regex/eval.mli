type state = 
  { text  : string
  ; logic : bool }

val select_from_alt : Ast.t -> Ast.t Choice.t

val search_success  : state -> int -> int Choice.t
val search_failure  : state -> int -> int Choice.t
val search_ast      : Ast.t -> state -> int -> int Choice.t
val search_literal  : Ast.t -> state -> int -> int Choice.t
(* val search_sequence : Ast.t -> state -> int -> int Choice.t *)
val search_alt      : Ast.t -> state -> int -> int Choice.t
val search_repeater : Ast.t -> state -> int -> int Choice.t
val search_negation : Ast.t -> state -> int -> int Choice.t
  
val search          :  string -> Ast.t -> (int * int) list
