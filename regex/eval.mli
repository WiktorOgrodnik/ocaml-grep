val select:  int -> int -> int Bt.t 

val triples: int -> (int * int * int) Bt.t

(* val select_ast_element_from_alt: Ast.group -> Ast.group Bt.t *)

val search:  string -> Ast.group -> (int * int) list

val search_ast      : string -> Ast.group -> int -> int Bt.t
val search_literal  : string -> Ast.group -> int -> int Bt.t
val search_sequence : string -> Ast.group -> int -> int Bt.t
