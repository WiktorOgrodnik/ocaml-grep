open! Core

type t = (* -- Expression*)
| INVALID (* -- Return when error detected *)
| SEQUENCE of t list
| ALTERNATIVE of t list
| LITERAL of literal
| REPEATER of t * repeating
| NEGATION of t
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

let s_of_sint i =
  match i with
  | None   -> ""
  | Some i -> string_of_int i

let rec to_string ast =
  let to_string_literal lit =
    match lit with
    | SINGLE c     -> String.make 1 c
    | ANY          -> "ANY"
    | RANGE (l, r) -> "RANGE from " ^ String.make 1 l ^ " to " ^ String.make 1 r
  in
  match ast with
  | INVALID            -> "INVALID"
  | SEQUENCE     xs    -> "SEQUENCE of ("    ^ String.concat ~sep:", " (List.map ~f:to_string xs) ^ ")"
  | ALTERNATIVE  xs    -> "ALTERNATIVE of (" ^ String.concat (List.map ~f:to_string xs) ^ ")"
  | LITERAL      c     -> to_string_literal c
  | REPEATER    (t, r) -> "REPEATER of " ^ to_string t ^ "(from: " ^ s_of_sint r.l ^ ", to: " ^ s_of_sint r.r ^ ")"
  | NEGATION     t     -> "NEGATION of (" ^ to_string t ^ ")"

let seq_get_elt seq =
  match seq with
  | SEQUENCE (h :: tl)    -> Some (h, SEQUENCE tl)
  | ALTERNATIVE (h :: tl) -> Some (h, ALTERNATIVE tl)
  | _                     -> None
