open! Core

let examples = Array.of_list
[ "abc",    "abcabc" 
; "b",      "bbaaaabbbbbb"
; "bc(d)a", "totylkotekstbcdainny tekst bcad"]

;;

let test_parse_common n =
  let pattern, string = Array.get examples n in
  let tokens = Lexer.gen_tokens pattern      in
  let parser = Parser.init tokens            in
  let ast    = Parser.parse parser           in
  let m      = Eval.search string ast        in
  m

let%expect_test "eval_test_1" =
  let result = test_parse_common 0 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{|
    ((3 5) (0 2)) |}]

let%expect_test "eval_test_2" =
  let result = test_parse_common 1 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((11 11) (10 10) (9 9) (8 8) (7 7) (6 6) (1 1) (0 0)) |}]

let%expect_test "eval_test_3" =
  let result = test_parse_common 2 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((12 15)) |}]
