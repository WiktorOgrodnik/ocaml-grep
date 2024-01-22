open! Core
open  Or_error.Let_syntax


let search line handler pattern =
  let tokens        = Lexer.gen_tokens pattern  in
  let parser        = Parser.init tokens        in
  let%bind ast_tree = Parser.parse parser       in
  let res           = Eval.search line ast_tree in
  match res with
  | _ :: _ -> handler line res; Ok ()
  | []     -> Ok ()
