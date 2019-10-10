open Geneweb
open OUnit2

let suite =
  [ "Place" >:::
    [ "normalize" >:: begin fun _ ->
          let test exp inp =
            assert_equal ~printer:(fun x -> x) exp (Place.normalize inp)
          in
          test "foo-bar, boobar (baz)" "[foo-bar] - boobar (baz)"
        ; test "[foo-bar - boobar (baz)" "[foo-bar - boobar (baz)"
        ; test "[foo-bar] boobar (baz)" "[foo-bar] boobar (baz)"
        end
    ; "split_suburb" >:: begin fun _ ->
        let test exp inp =
          assert_equal
            ~printer:(fun (a,b) -> Printf.sprintf {|("%s","%s")|} a b)
            exp (Place.split_suburb inp)
        in
        test ("foo-bar", "boobar (baz)") "[foo-bar] - boobar (baz)"
      ; test ("", "boobar (baz)") "boobar (baz)"
      end
    ; "only_suburb" >:: begin fun _ ->
        let test exp inp =
          assert_equal ~printer:(fun s -> s) exp (Place.only_suburb inp)
        in
        test "foo-bar" "[foo-bar] - boobar (baz)"
      ; test "" "boobar (baz)"
      end
    ; "without_suburb" >:: begin fun _ ->
        let test exp inp =
          assert_equal ~printer:(fun s -> s) exp (Place.without_suburb inp)
        in
        test "boobar (baz)" "[foo-bar] - boobar (baz)"
      ; test "boobar (baz)" "boobar (baz)"
      end
    ; "compare_places" >:: begin fun _ ->
        let test exp a b =
          assert_equal ~printer:string_of_int exp (Place.compare_places a b)
        ; assert_equal ~printer:string_of_int (-exp) (Place.compare_places b a)
        in
        test 0 "boobar (baz)" "boobar (baz)"
      ; test (-1) "baz (boobar)" "boobar (baz)"
      ; test (-1) "baz (boobar)" "[foo-bar] - baz (boobar)"
      ; test (-1) "[bar-foo] - baz (boobar)" "[foo-bar] - baz (boobar)"
      ; test (-1) "[foo-bar] - baz (boobar)" "[bar-foo] - boobar (baz)"
      ; test (-1) "[foo-bar] - ebaz (boobar)" "[bar-foo] - éboobar (baz)"
      ; test (-1) "[foo-bar] - baz, boobar, barboo" "[foo-bar] - baz, boobar, barboo, bam"
      end
    ]
  ]
