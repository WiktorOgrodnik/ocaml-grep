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
; "\\+*"
; "a{2}"
; "(ab){3, 5}"
; "a{1,123}"
; "(abc|nana){5}"
; "ttt{4}"
; "(abc|nana)*"
; "(abv)?"
; "(t.y){2, 10}"
; "t*."
; "[qwerty]"
; "rt[qwerty]p5"
; "pt[abc]{5}"
; "a?"
; "abc{3,4}[0-9]"
; "abc[d-z]"]

;;

let test_parse_common n =
  let string = Array.get examples n    in
  let tokens = Lexer.gen_tokens string in
  let parser = Parser.init tokens      in
  Parser.parse_einv parser

let%expect_test "parse_basic_a" =
  let result = test_parse_common 0 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (LITERAL (SINGLE a)) |}]

let%expect_test "parse_basic_hello" =
  let result = test_parse_common 1 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (SINGLE h)) (LITERAL (SINGLE e)) (LITERAL (SINGLE l))
      (LITERAL (SINGLE l)) (LITERAL (SINGLE o)))) |}]

let%expect_test "parse_basic_(hello)" =
  let result = test_parse_common 2 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (SINGLE h)) (LITERAL (SINGLE e)) (LITERAL (SINGLE l))
      (LITERAL (SINGLE l)) (LITERAL (SINGLE o)))) |}]

let%expect_test "parse_basic_gr(ae)y" =
  let result = test_parse_common 3 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (SINGLE g)) (LITERAL (SINGLE r))
      (SEQUENCE ((LITERAL (SINGLE a)) (LITERAL (SINGLE e))))
      (LITERAL (SINGLE y)))) |}]

let%expect_test "parse_basic_(gr)(ey)" =
  let result = test_parse_common 4 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((SEQUENCE ((LITERAL (SINGLE g)) (LITERAL (SINGLE r))))
      (SEQUENCE ((LITERAL (SINGLE e)) (LITERAL (SINGLE y)))))) |}]

let%expect_test "parse_basic_((gr)(ey))" =
  let result = test_parse_common 5 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((SEQUENCE ((LITERAL (SINGLE g)) (LITERAL (SINGLE r))))
      (SEQUENCE ((LITERAL (SINGLE a)) (LITERAL (SINGLE y)))))) |}]

let%expect_test "parse_error_(gr(ey)" =
  let result = test_parse_common 6 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    Parse sequence - invalid token: (No token)!
    INVALID |}]

let%expect_test "parse_alternative_gray|grey" =
  let result = test_parse_common 7 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (ALTERNATIVE
     ((SEQUENCE
       ((LITERAL (SINGLE g)) (LITERAL (SINGLE r)) (LITERAL (SINGLE a))
        (LITERAL (SINGLE y))))
      (SEQUENCE
       ((LITERAL (SINGLE g)) (LITERAL (SINGLE r)) (LITERAL (SINGLE e))
        (LITERAL (SINGLE y)))))) |}]

(* Przetestujmy jak sparsuje się '(gray|grey)' *)
let%expect_test "parse_alternative_(gray|grey)" =
  let result = test_parse_common 8 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (ALTERNATIVE
     ((SEQUENCE
       ((LITERAL (SINGLE g)) (LITERAL (SINGLE r)) (LITERAL (SINGLE a))
        (LITERAL (SINGLE y))))
      (SEQUENCE
       ((LITERAL (SINGLE g)) (LITERAL (SINGLE r)) (LITERAL (SINGLE e))
        (LITERAL (SINGLE y)))))) |}]

let%expect_test "parse_alternative_gr(a|e)y" =
  let result = test_parse_common 9 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (SINGLE g)) (LITERAL (SINGLE r))
      (ALTERNATIVE ((LITERAL (SINGLE a)) (LITERAL (SINGLE e))))
      (LITERAL (SINGLE y)))) |}]

let%expect_test "parse_alternative_gr(ab(t|y)ba|baab)wd(a|b)" =
  let result = test_parse_common 10 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (SINGLE g)) (LITERAL (SINGLE r))
      (ALTERNATIVE
       ((SEQUENCE
         ((LITERAL (SINGLE a)) (LITERAL (SINGLE b))
          (ALTERNATIVE ((LITERAL (SINGLE t)) (LITERAL (SINGLE y))))
          (LITERAL (SINGLE b)) (LITERAL (SINGLE a))))
        (SEQUENCE
         ((LITERAL (SINGLE b)) (LITERAL (SINGLE a)) (LITERAL (SINGLE a))
          (LITERAL (SINGLE b))))))
      (LITERAL (SINGLE w)) (LITERAL (SINGLE d))
      (ALTERNATIVE ((LITERAL (SINGLE a)) (LITERAL (SINGLE b)))))) |}]

let%expect_test "parse_plus_abb+c" =
  let result = test_parse_common 11 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (SINGLE a)) (LITERAL (SINGLE b))
      (REPEATER (LITERAL (SINGLE b)) ((l (1)) (r ()))) (LITERAL (SINGLE c)))) |}]

let%expect_test "parse_plus_ab(ba)+c" =
  let result = test_parse_common 12 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (SINGLE a)) (LITERAL (SINGLE b))
      (REPEATER (SEQUENCE ((LITERAL (SINGLE b)) (LITERAL (SINGLE a))))
       ((l (1)) (r ())))
      (LITERAL (SINGLE c)))) |}]

let%expect_test "parse_plus_(a|b)b++c" =
  let result = test_parse_common 13 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    Parse sequence - invalid token: PLUS
    INVALID |}]

let%expect_test "parse_plus_\\++" =
  let result = test_parse_common 14 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (REPEATER (LITERAL (SINGLE +)) ((l (1)) (r ()))) |}]

let%expect_test "parse_plus_\\+*" =
  let result = test_parse_common 15 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (REPEATER (LITERAL (SINGLE +)) ((l (0)) (r ()))) |}]

let%expect_test "parse_repeater_ext0" =
  let result = test_parse_common 16 in
  print_s [%sexp (result : Ast.t)];
  [%expect{| (REPEATER (LITERAL (SINGLE a)) ((l (2)) (r (2)))) |}]

let%expect_test "parse_repeater_ext1" =
  let result = test_parse_common 17 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (REPEATER (SEQUENCE ((LITERAL (SINGLE a)) (LITERAL (SINGLE b))))
     ((l (3)) (r (5)))) |}]

let%expect_test "parse_repeater_ext2" =
  let result = test_parse_common 18 in
  print_s [%sexp (result : Ast.t)];
  [%expect{| (REPEATER (LITERAL (SINGLE a)) ((l (1)) (r (123)))) |}]

let%expect_test "parse_repeater_ext3" =
  let result = test_parse_common 19 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (REPEATER
     (ALTERNATIVE
      ((SEQUENCE
        ((LITERAL (SINGLE a)) (LITERAL (SINGLE b)) (LITERAL (SINGLE c))))
       (SEQUENCE
        ((LITERAL (SINGLE n)) (LITERAL (SINGLE a)) (LITERAL (SINGLE n))
         (LITERAL (SINGLE a))))))
     ((l (5)) (r (5)))) |}]

let%expect_test "parse_repeater_ext4" =
  let result = test_parse_common 20 in
  print_s [%sexp (result : Ast.t)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (SINGLE t)) (LITERAL (SINGLE t))
      (REPEATER (LITERAL (SINGLE t)) ((l (4)) (r (4)))))) |}]

let%expect_test "parse_repeater_ext5" =
let result = test_parse_common 21 in
print_s [%sexp (result : Ast.t)];
[%expect{|
  (REPEATER
   (ALTERNATIVE
    ((SEQUENCE
      ((LITERAL (SINGLE a)) (LITERAL (SINGLE b)) (LITERAL (SINGLE c))))
     (SEQUENCE
      ((LITERAL (SINGLE n)) (LITERAL (SINGLE a)) (LITERAL (SINGLE n))
       (LITERAL (SINGLE a))))))
   ((l (0)) (r ()))) |}]

let%expect_test "parse_new1_(abv)?" =
let result = test_parse_common 22 in
print_s [%sexp (result : Ast.t)];
[%expect{|
  (REPEATER
   (SEQUENCE ((LITERAL (SINGLE a)) (LITERAL (SINGLE b)) (LITERAL (SINGLE v))))
   ((l (0)) (r (1)))) |}]

let%expect_test "parse_new2_(t.y){2, 10}" =
let result = test_parse_common 23 in
print_s [%sexp (result : Ast.t)];
[%expect{|
  (REPEATER
   (SEQUENCE ((LITERAL (SINGLE t)) (LITERAL ANY) (LITERAL (SINGLE y))))
   ((l (2)) (r (10)))) |}]

let%expect_test "parse_new3_t*" =
let result = test_parse_common 24 in
print_s [%sexp (result : Ast.t)];
[%expect{| (SEQUENCE ((REPEATER (LITERAL (SINGLE t)) ((l (0)) (r ()))) (LITERAL ANY))) |}]

let%expect_test "parse_new4_[qwerty]" =
let result = test_parse_common 25 in
print_s [%sexp (result : Ast.t)];
[%expect{|
  (ALTERNATIVE
   ((LITERAL (SINGLE q)) (LITERAL (SINGLE w)) (LITERAL (SINGLE e))
    (LITERAL (SINGLE r)) (LITERAL (SINGLE t)) (LITERAL (SINGLE y)))) |}]

let%expect_test "parse_new5_rt_[qwerty]p5" =
let result = test_parse_common 26 in
print_s [%sexp (result : Ast.t)];
[%expect{|
  (SEQUENCE
   ((LITERAL (SINGLE r)) (LITERAL (SINGLE t))
    (ALTERNATIVE
     ((LITERAL (SINGLE q)) (LITERAL (SINGLE w)) (LITERAL (SINGLE e))
      (LITERAL (SINGLE r)) (LITERAL (SINGLE t)) (LITERAL (SINGLE y))))
    (LITERAL (SINGLE p)) (LITERAL (SINGLE 5)))) |}]

let%expect_test "parse_new6_pt[abc]{5}" =
let result = test_parse_common 27 in
print_s [%sexp (result : Ast.t)];
[%expect{|
  (SEQUENCE
   ((LITERAL (SINGLE p)) (LITERAL (SINGLE t))
    (REPEATER
     (ALTERNATIVE
      ((LITERAL (SINGLE a)) (LITERAL (SINGLE b)) (LITERAL (SINGLE c))))
     ((l (5)) (r (5)))))) |}]

let%expect_test "parse_new7_a?" =
let result = test_parse_common 28 in
print_s [%sexp (result : Ast.t)];
[%expect{| (REPEATER (LITERAL (SINGLE a)) ((l (0)) (r (1)))) |}]

let%expect_test "parse_new8_abc{3,4}[0-9]" =
let result = test_parse_common 29 in
print_s [%sexp (result : Ast.t)];
[%expect{|
  (SEQUENCE
   ((LITERAL (SINGLE a)) (LITERAL (SINGLE b))
    (REPEATER (LITERAL (SINGLE c)) ((l (3)) (r (4)))) (LITERAL (RANGE 0 9)))) |}]

let%expect_test "parse_new9_abc[d-z]" =
let result = test_parse_common 30 in
print_s [%sexp (result : Ast.t)];
[%expect{|
  (SEQUENCE
   ((LITERAL (SINGLE a)) (LITERAL (SINGLE b)) (LITERAL (SINGLE c))
    (LITERAL (RANGE d z)))) |}]
