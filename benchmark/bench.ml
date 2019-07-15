let bench name n fn arg =
  ignore @@ Benchmark.latency1 ~name n (List.map fn) arg

let list =
  [1;2;10;100;1000;10000;100000;1000000;10000000;100000000;1000000000]

let sosa_list =
  List.map Sosa.of_int list

let () =
  bench "Sosa.gen" 1000000L (List.map Sosa.gen) [ sosa_list ]
; bench "Sosa.to_string_sep" 1000000L (List.map @@ Sosa.to_string_sep ",") [ sosa_list ]
; bench "Sosa.to_string" 1000000L (List.map Sosa.to_string) [ sosa_list ]
; bench "Sosa.of_string" 1000000L (List.map Sosa.of_string) [ List.map string_of_int list ]
; bench "Sosa.branches" 1000000L (List.map Sosa.branches) [ sosa_list ]
; bench "PlaceDisplay.normalize" 10000000L Geneweb.PlaceDisplay.normalize
    [ "[foo-bar] - boobar (baz)" ; "[foo-bar] – boobar (baz)" ; "[foo-bar] — boobar (baz)" ]
; bench "Mutil.unsafe_tr" 100000000L (fun s -> Mutil.unsafe_tr 'a' 'b' @@ "a" ^ s)
    [ "aaaaaaaaaa" ; "bbbbbbbbbb" ; "abbbbbbbb" ; "bbbbbbbbba" ; "ababababab" ]
; bench "Mutil.tr" 100000000L (fun s -> Mutil.tr 'a' 'b' @@ "a" ^ s)
    [ "aaaaaaaaaa" ; "bbbbbbbbbb" ; "abbbbbbbb" ; "bbbbbbbbba" ; "ababababab" ]
