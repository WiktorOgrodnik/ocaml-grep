open! Core
open Parser

let examples = Array.of_list
[ "hello"
; "(hello)"
; "gr(ae)y"
; "(gr)(ey)"
; "((gr)(ay))"
; "(gr(ey)"]
(* ; "gray|grey"
; "(gray|grey)"
; "gr(a|e)y"] *)

;;

let%expect_test "check_parse_until_non_literal_with_literals_only" =
  let tokens =
  [[ Token.LITERAL 'g'
   ; Token.LITERAL 'r'
   ; Token.LITERAL 'a'
   ; Token.LITERAL 'y'
   ; Token.RPAREN]] in
  let parsers = List.map tokens ~f:(init) in
  let results = List.map parsers ~f:parse_until_non_literal in
  print_s [%sexp (results : (t * Token.t list) Or_error.t list)];
  [%expect{|
    ((Ok
      (((tokens ())
        (discarded_tokens ((LITERAL y) (LITERAL a) (LITERAL r) (LITERAL g)))
        (current (RPAREN)) (idx 5))
       ((LITERAL g) (LITERAL r) (LITERAL a) (LITERAL y))))) |}]

let%expect_test "check_parse_until_non_literal" =
  let tokens =
  [[ Token.LITERAL 'g'
   ; Token.LITERAL 'r'
   ; Token.LITERAL 'a'
   ; Token.LITERAL 'y'
   ; Token.OR
   ; Token.LITERAL 'g'
   ; Token.LITERAL 'r'
   ; Token.LITERAL 'e'
   ; Token.LITERAL 'y'
   ; Token.RPAREN]] in
  let parsers = List.map tokens ~f:(init) in
  let results = List.map parsers ~f:parse_until_non_literal in
  print_s [%sexp (results : (t * Token.t list) Or_error.t list)];
  [%expect{|
    ((Ok
      (((tokens ((LITERAL g) (LITERAL r) (LITERAL e) (LITERAL y) RPAREN))
        (discarded_tokens ((LITERAL y) (LITERAL a) (LITERAL r) (LITERAL g)))
        (current (OR)) (idx 5))
       ((LITERAL g) (LITERAL r) (LITERAL a) (LITERAL y))))) |}]

let test_parse_common n =
  let string = Array.get examples n in
  let tokens = Lexer.gen_tokens string in
  let parser = init tokens in
  parse parser

let%expect_test "parse_basic_hello" =
  let result = test_parse_common 0 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (h)) (LITERAL (e)) (LITERAL (l)) (LITERAL (l)) (LITERAL (o)))) |}]

let%expect_test "parse_basic_(hello)" =
  let result = test_parse_common 1 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((SEQUENCE
       ((LITERAL (h)) (LITERAL (e)) (LITERAL (l)) (LITERAL (l)) (LITERAL (o)))))) |}]

let%expect_test "parse_basic_gr(ae)y" =
  let result = test_parse_common 2 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((LITERAL (g)) (LITERAL (r)) (SEQUENCE ((LITERAL (a)) (LITERAL (e))))
      (LITERAL (y)))) |}]

let%expect_test "parse_basic_(gr)(ey)" =
  let result = test_parse_common 3 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((SEQUENCE ((LITERAL (g)) (LITERAL (r))))
      (SEQUENCE ((LITERAL (e)) (LITERAL (y)))))) |}]

let%expect_test "parse_basic_((gr)(ey))" =
  let result = test_parse_common 4 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    (SEQUENCE
     ((SEQUENCE
       ((SEQUENCE ((LITERAL (g)) (LITERAL (r))))
        (SEQUENCE ((LITERAL (a)) (LITERAL (y)))))))) |}]

let%expect_test "parse_error_(gr(ey)" =
  let result = test_parse_common 5 in
  print_s [%sexp (result : Ast.group)];
  [%expect{|
    parse_until_non_literal - unexpected end
    (SEQUENCE ()) |}]