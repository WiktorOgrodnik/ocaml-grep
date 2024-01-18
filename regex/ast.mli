open! Core

type group = (* -- Expression*)
| INVALID (* -- Return when error detected *)
| SEQUENCE of group list
| ALTERNATIVE of group list
| LITERAL of char option
| REPEATER of group * repeating
[@@deriving sexp_of]

and repeating = { l : int option
                ; r : int option
                }
[@@deriving sexp_of]
