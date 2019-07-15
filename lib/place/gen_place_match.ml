open Place_types
open Place_data

let key = Place_types.key

let gen_not_found oc = Printf.fprintf oc "| _ -> raise Not_found\n"

let gen_country oc =
  Printf.fprintf oc "let country s : country = match key s with\n" ;
  Array.iter
    begin fun (k, names) ->
      Array.to_list names
      |> List.map key
      |> List.sort_uniq compare
      |> List.iter (fun n -> Printf.fprintf oc "|%d" n) ;
      Printf.fprintf oc "->%s\n" (show_country k)
    end
    countries ;
  gen_not_found oc

let gen_region_variable_name country = "region_" ^ (show_country country)
let gen_subregion_variable_name country = "subregion_" ^ (show_country country)

let gen_region oc =
  let data =
    [|Algeria,algeria_region
     ;France,france_region
     ;South_Africa,south_africa_region
     ;United_States,united_states_region
     ;Canada,canada_region
     ;Belgium,belgium_region
     ;Australia,australia_region
     ;Austria,austria_region
     ;Germany,germany_region
     ;Spain,spain_region
    |]
  in
  Array.iter
    begin fun (country, regions) ->
      Printf.fprintf oc
        "let [@warning \"-41\"] [@warning \"-42\"] %s s : region = match key s with\n"
        (gen_region_variable_name country) ;
      Array.iter
        begin fun (k, names) ->
          Array.to_list names
          |> List.map key
          |> List.sort_uniq compare
          |> List.iter (fun n -> Printf.fprintf oc "|%d" n) ;
          Printf.fprintf oc "->%s\n" (show_region k)
        end
        regions ;
      gen_not_found oc ;
    end
    data ;
  Printf.fprintf oc "let region = function\n" ;
  Array.iter
    begin fun (k, _) ->
      Printf.fprintf oc "|%s->%s\n" (show_country k) (gen_region_variable_name k)
    end
    data ;
  gen_not_found oc

let gen_subregion oc =
  let data =
    [|France,france_subregion
     ;Belgium,belgium_subregion
    |]
  in
  Array.iter
    begin fun (country, subregions) ->
      Printf.fprintf oc
        "let [@warning \"-41\"] [@warning \"-42\"] %s s : subregion = match key s with\n"
        (gen_subregion_variable_name country) ;
      Array.iter
        begin fun (k, names) ->
          Array.to_list names
          |> List.map key
          |> List.sort_uniq compare
          |> List.iter (fun n -> Printf.fprintf oc "|%d" n) ;
          Printf.fprintf oc "->%s\n" (show_subregion k)
        end
        subregions ;
      gen_not_found oc ;
    end
    data ;
  Printf.fprintf oc "let subregion = function\n" ;
  Array.iter
    begin fun (k, _) ->
      Printf.fprintf oc "|%s->%s\n" (show_country k) (gen_subregion_variable_name k)
    end
    data ;
  gen_not_found oc

let () =
  let oc = open_out "place_match.ml" in
  Printf.fprintf oc "open Place_types\n" ;
  gen_country oc ;
  gen_region oc ;
  gen_subregion oc ;
  close_out oc
