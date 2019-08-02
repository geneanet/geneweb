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
; bench "Place.country" 10000L Place.country
    (List.flatten @@ Array.to_list @@ Array.map (fun (_, x) -> Array.to_list x) Place_data.countries)
; begin
  let open Place_data in
  let open Place_types in
  let aux x = List.flatten @@ Array.to_list @@ Array.map (fun (_, x) -> Array.to_list x) x in
  bench "Place.region" 10000L (fun (c, a) -> List.map (Place.region c) a)
    [ Algeria, aux algeria_region
    ; Spain, aux spain_region
    ; France, aux france_region
    ; South_Africa, aux south_africa_region
    ; United_States, aux united_states_region
    ; Canada, aux canada_region
    ; Belgium, aux belgium_region
    ; Austria, aux austria_region
    ; Australia, aux australia_region
    ; Germany, aux germany_region
    ]
end
; begin
  let open Place_data in
  let open Place_types in
  let aux x = List.flatten @@ Array.to_list @@ Array.map (fun (_, x) -> Array.to_list x) x in
  bench "Place.subregion" 10000L (fun (c, a) -> List.map (Place.subregion c) a)
    [ France, aux france_subregion
    ; Belgium, aux belgium_subregion
    ]
end
