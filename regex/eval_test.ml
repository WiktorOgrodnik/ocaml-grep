open! Core

let examples = Array.of_list
[ "abc",           "abcabc" 
; "b",             "bbaaaabbbbbb"
; "bc(d)a",        "totylkotekstbcdainny tekst bcad"
; "gray|grey",     "tgraygrteygreygruy"
; "gr(u|t|s)(st)", ""
; "gr(u|t|st)",    "grummmgrsttgrsu"
; "b+",            "aabbbbbbbbc"
; "abac*",         "abad"
; "abac*",         "abaccccd"
; "c(ab)+",        "cabbabab"
; "c(ab)+",        "cabababc"
; "c+",            "ca"] 
;;

let test_parse_common n =
  let pattern, string = Array.get examples n     in
  let tokens          = Lexer.gen_tokens pattern in
  let parser          = Parser.init tokens       in
  let ast             = Parser.parse_einv parser in
  Eval.search string ast

let%expect_test "eval_test_1" =
  let result = test_parse_common 0 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((3 5) (0 2)) |}]

let%expect_test "eval_test_2" =
  let result = test_parse_common 1 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((11 11) (10 10) (9 9) (8 8) (7 7) (6 6) (1 1) (0 0)) |}]

let%expect_test "eval_test_3" =
  let result = test_parse_common 2 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((12 15)) |}]

let%expect_test "eval_test_4" =
  let result = test_parse_common 3 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((10 13) (1 4)) |}]

let%expect_test "eval_test_5" =
  let result = test_parse_common 4 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| () |}]

let%expect_test "eval_test_6" =
  let result = test_parse_common 5 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((6 9) (0 2)) |}]

let%expect_test "eval_test_7" =
  let result = test_parse_common 6 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((9 9) (8 9) (7 9) (6 9) (5 9) (4 9) (3 9) (2 9)) |}]

let%expect_test "eval_test_8" =
  let result = test_parse_common 7 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((0 2)) |}]

let%expect_test "eval_test_9" =
  let result = test_parse_common 8 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((0 6)) |}]

let%expect_test "eval_test_10" =
  let result = test_parse_common 9 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((0 2)) |}]

let%expect_test "eval_test_11" =
  let result = test_parse_common 10 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((0 6)) |}]

let%expect_test "eval_test_12" =
  let result = test_parse_common 11 in
  print_s [%sexp (result : (int * int) list)];
  [%expect{| ((0 0)) |}]
