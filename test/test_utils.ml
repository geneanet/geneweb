open Geneweb
open OUnit2

let mutil_contains _ =
  let str = "foo bar" in
  let test t b1 b2 =
    assert_equal b1 (Mutil.contains ~wildcard:false str t)
  ; assert_equal b2 (Mutil.contains ~wildcard:true str t)
  in
  test "foo" true true
; test "baz" false false
; test "foo_b" false true
; test "foo b" true true
; test "foo__b" false false
; test "bar__" false true
; test "r" true true
; test "" true true

let mutil_start_with _ =
  assert_raises (Invalid_argument "start_with")
    (fun () -> Mutil.start_with "foo" (-1) "foo")
; assert_raises (Invalid_argument "start_with")
    (fun () -> Mutil.start_with "foo" 4 "foo")
; assert_bool "Mutil.start_with \"foo\" 0 \"foo\""
    (Mutil.start_with "foo" 0 "foo")
; assert_bool "not (Mutil.start_with \"bar\" 0 \"foo\")"
    (not @@ Mutil.start_with "bar" 0 "foo")
; assert_bool "Mutil.start_with \"\" 0 \"foo\""
    (Mutil.start_with "" 0 "foo")

let _ =
  "Geneweb utils" >:::
  [ "Mutil" >:::
    [ "mutil_contains" >:: mutil_contains
    ; "mutil_start_with" >:: mutil_start_with
    ]
  ]
  |> run_test_tt_main
