open Geneweb
open OUnit2

let suite =
  [ "Place" >:::
    [ "normalize" >:: begin fun _ ->
          let test exp inp =
            assert_equal ~printer:(fun x -> x) exp (Place.normalize inp)
          in
          test "foo-bar, boobar (baz)" "[foo-bar] - boobar (baz)"
        end
    ]
  ]
