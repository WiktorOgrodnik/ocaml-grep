let find line pattern =
  let rex = Str.regexp pattern in
  match Str.search_forward rex line 0 with
  | exception Not_found -> false
  | _                   -> true
;;

(* let search_old line pattern handler =
  if find line pattern then
    handler line
  else ()

;; *)

let search line handler _ =
  let tokens = Lexer.gen_tokens line in
  List.iter handler (List.map Token.string_of_token tokens)
;;
