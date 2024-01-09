open! Core

type group = (* -- Expression*)
| SEQUENCE of group list
| ALTERNATIVE of group list
| LITERAL of char option
[@@deriving sexp_of]

(* and repeating_interval = 
  { from  : int
  ; to_   : int option
  }

and atom =
  { grp : group
  ; rep : repeating_interval option
  } *)