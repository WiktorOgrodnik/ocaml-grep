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

let s_of_sint i =
  match i with
  | None   -> ""
  | Some i -> string_of_int i

let rec to_string ast =
  match ast with
  | INVALID            -> "INVALID"
  | SEQUENCE     xs    -> "SEQUENCE of ("    ^ String.concat ~sep:", " (List.map ~f:to_string xs) ^ ")"
  | ALTERNATIVE  xs    -> "ALTERNATIVE of (" ^ String.concat (List.map ~f:to_string xs) ^ ")"
  | LITERAL      c     -> "LITERAL of " ^ (match c with None -> "?" | Some c -> String.make 1 c)
  | REPEATER    (t, r) -> "REPEATER of " ^ to_string t ^ "(from: " ^ s_of_sint r.l ^ ", to: " ^ s_of_sint r.r ^ ")"

let seq_get_elt seq =
  match seq with
  | SEQUENCE (h :: tl) -> Some (h, SEQUENCE tl)
  | _                  -> None
