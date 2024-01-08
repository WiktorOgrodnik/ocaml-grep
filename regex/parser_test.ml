open! Core
open Parser

let%expect_test "check_parse_until_non_literal_with_literals_only" =
  let tokens =
  [[ Token.LITERAL 'g'
   ; Token.LITERAL 'r'
   ; Token.LITERAL 'a'
   ; Token.LITERAL 'y']] in
  let parsers = List.map tokens ~f:(init) in
  let results = List.map parsers ~f:parse_until_non_literal in
  print_s [%sexp (results : (t * Token.t list) Or_error.t list)];
  [%expect{|
    ((Ok
      (((tokens ()) (current ()))
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
   ; Token.LITERAL 'y']] in
  let parsers = List.map tokens ~f:(init) in
  let results = List.map parsers ~f:parse_until_non_literal in
  print_s [%sexp (results : (t * Token.t list) Or_error.t list)];
  [%expect{|
    ((Ok
      (((tokens ((LITERAL g) (LITERAL r) (LITERAL e) (LITERAL y)))
        (current (OR)))
       ((LITERAL g) (LITERAL r) (LITERAL a) (LITERAL y))))) |}]