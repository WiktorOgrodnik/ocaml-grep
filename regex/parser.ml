type t = 
  { tokens  : Token.t list
  ; current : Token.t option
  ; peek    : Token.t option
  }

(* let rec parse prsr =
  let rec parse_aux parser nodes =
    match  *)