open! Core

let compile pattern =
  let tokens = Lexer.gen_tokens pattern in
  Parser.parse tokens

let search line handler compiled_pattern =
  let res           = Eval.search line compiled_pattern in
  match res with
  | _ :: _ -> handler line res
  | []     -> ()
