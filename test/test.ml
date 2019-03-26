open OUnit2

let _ =
  run_test_tt_main begin
    "Geneweb" >:::
    Test_utils.suite
    @ Test_sosa.suite
  end

