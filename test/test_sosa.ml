open Geneweb
open OUnit2

let suite =
  [ "Sosa" >:::
    [ "int conversion" >:: begin fun _ ->
          assert_equal Sosa.zero (Sosa.of_int 0) ;
          assert_equal Sosa.one (Sosa.of_int 1) ;
        end

    ; "string conversion" >:: begin fun _ ->
        assert_equal Sosa.zero (Sosa.of_string "0") ;
        assert_equal Sosa.one (Sosa.of_string "1") ;
        assert_equal "0" (Sosa.to_string Sosa.zero) ;
        assert_equal "1" (Sosa.to_string Sosa.one) ;
      end

    ; "gen" >:: begin fun _ ->
        let test exp x =
          assert_equal ~printer:string_of_int exp @@
          Sosa.gen (Sosa.of_int x)
        in
        test 1 1 ;
        test 2 2 ;
        test 2 3 ;
        test 3 4 ;
        test 3 5 ;
        test 3 6 ;
        test 3 7 ;
        test 4 8 ;
        test 4 9 ;
        test 4 10 ;
        test 4 11 ;
        test 4 12 ;
        test 4 13 ;
        test 4 14 ;
        test 4 15 ;
      end
    ]
  ]
