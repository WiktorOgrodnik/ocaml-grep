open! Core
open Lexer

let examples = Array.of_list
[ "a"
; "hello"
; "(hello)"
; "gr(ae)y"
; "(gr)(ey)"
; "((gr)(ay))"
; "(gr(ey)"
; "gray|grey"
; "(gray|grey)"
; "gr(a|e)y"
; "gr(ab(t|y)ba|baab)wd(a|b)"
; "abb+c"
; "ab(ba)+c"
; "(a|b)b++c"
; "\\++"]

;;

let test_lex_common n =
  let string = Array.get examples n in
  gen_tokens string

let%expect_test "lex_basic_a" =
  let result = test_lex_common 0 in
  print_s [%sexp (result : Token.t list)];
  [%expect{| ((LITERAL a) RPAREN) |}]
