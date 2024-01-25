open! Core

type t = (* -- Expression*)
| INVALID (* -- Return when error detected *)
| SEQUENCE of t list
| ALTERNATIVE of t list
| LITERAL of literal
| REPEATER of t * repeating
[@@deriving sexp_of]

and repeating = { l : int option
                ; r : int option
                }
[@@deriving sexp_of]

and literal =
| SINGLE of char
| RANGE  of char * char
| ANY
[@@deriving sexp_of]

val to_string   : t -> string
val seq_get_elt : t -> (t * t) option
