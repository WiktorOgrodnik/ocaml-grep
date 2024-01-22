open! Core

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
; "\\++"
; "\\+*"]

;;

let test_parse_common n =
  let string = Array.get examples n    in
  let tokens = Lexer.gen_tokens string in
  let parser = Parser.init tokens      in
  Parser.parse_einv parser

let%expect_test "parse_basic_a" =
  let result = test_parse_common 0 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (LITERAL (a)) |}]

let%expect_test "parse_basic_hello" =
  let result = test_parse_common 1 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (h)) (LITERAL (e)) (LITERAL (l)) (LITERAL (l)) (LITERAL (o)))) |}]

let%expect_test "parse_basic_(hello)" =
  let result = test_parse_common 2 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (h)) (LITERAL (e)) (LITERAL (l)) (LITERAL (l)) (LITERAL (o)))) |}]

let%expect_test "parse_basic_gr(ae)y" =
  let result = test_parse_common 3 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (g)) (LITERAL (r)) (SEQUENCE ((LITERAL (a)) (LITERAL (e))))
      (LITERAL (y)))) |}]

let%expect_test "parse_basic_(gr)(ey)" =
  let result = test_parse_common 4 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((SEQUENCE ((LITERAL (g)) (LITERAL (r))))
      (SEQUENCE ((LITERAL (e)) (LITERAL (y)))))) |}]

let%expect_test "parse_basic_((gr)(ey))" =
  let result = test_parse_common 5 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((SEQUENCE ((LITERAL (g)) (LITERAL (r))))
      (SEQUENCE ((LITERAL (a)) (LITERAL (y)))))) |}]

let%expect_test "parse_error_(gr(ey)" =
  let result = test_parse_common 6 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    Parse sequence: unknown token
    INVALID |}]

let%expect_test "parse_alternative_gray|grey" =
  let result = test_parse_common 7 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (ALTERNATIVE
     ((SEQUENCE ((LITERAL (g)) (LITERAL (r)) (LITERAL (a)) (LITERAL (y))))
      (SEQUENCE ((LITERAL (g)) (LITERAL (r)) (LITERAL (e)) (LITERAL (y)))))) |}]

(* Przetestujmy jak sparsuje się '(gray|grey)' *)
let%expect_test "parse_alternative_(gray|grey)" =
  let result = test_parse_common 8 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (ALTERNATIVE
     ((SEQUENCE ((LITERAL (g)) (LITERAL (r)) (LITERAL (a)) (LITERAL (y))))
      (SEQUENCE ((LITERAL (g)) (LITERAL (r)) (LITERAL (e)) (LITERAL (y)))))) |}]

let%expect_test "parse_alternative_gr(a|e)y" =
  let result = test_parse_common 9 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (g)) (LITERAL (r)) (ALTERNATIVE ((LITERAL (a)) (LITERAL (e))))
      (LITERAL (y)))) |}]

let%expect_test "parse_alternative_gr(ab(t|y)ba|baab)wd(a|b)" =
  let result = test_parse_common 10 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (g)) (LITERAL (r))
      (ALTERNATIVE
       ((SEQUENCE
         ((LITERAL (a)) (LITERAL (b)) (ALTERNATIVE ((LITERAL (t)) (LITERAL (y))))
          (LITERAL (b)) (LITERAL (a))))
        (SEQUENCE ((LITERAL (b)) (LITERAL (a)) (LITERAL (a)) (LITERAL (b))))))
      (LITERAL (w)) (LITERAL (d)) (ALTERNATIVE ((LITERAL (a)) (LITERAL (b)))))) |}]

let%expect_test "parse_plus_abb+c" =
  let result = test_parse_common 11 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (a)) (LITERAL (b)) (REPEATER (LITERAL (b)) ((l (1)) (r ())))
      (LITERAL (c)))) |}]

let%expect_test "parse_plus_ab(ba)+c" =
  let result = test_parse_common 12 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (a)) (LITERAL (b))
      (REPEATER (SEQUENCE ((LITERAL (b)) (LITERAL (a)))) ((l (1)) (r ())))
      (LITERAL (c)))) |}]

let%expect_test "parse_plus_(a|b)b++c" =
  let result = test_parse_common 13 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    Parse sequence: unknown token
    INVALID |}]

let%expect_test "parse_plus_\\++" =
  let result = test_parse_common 14 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (REPEATER (LITERAL (+)) ((l (1)) (r ()))) |}]

let%expect_test "parse_plus_\\+*" =
  let result = test_parse_common 15 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (REPEATER (LITERAL (+)) ((l (0)) (r ()))) |}]
