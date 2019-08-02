open Place_types
open Place_data

let key = Place_types.key

let gen_find oc =
Printf.fprintf oc
{|let find a x =
  let rec loop min max =
    if min > max then raise Not_found ;
    let i = (min + max) / 2 in
    let (m, r) = Array.unsafe_get a i in
    if m < x then loop (i + 1) max
    else if m > x then loop min (i - 1)
    else r
  in
  loop 0 (Array.length a - 1)
|}

let mk_array oc array show =
  let ht = Hashtbl.create 2048 in
  Array.iter
    begin fun (k, names) ->
      Array.to_list names
      |> List.map key
      |> List.sort_uniq compare
      |> List.iter (fun n -> assert (not @@ Hashtbl.mem ht n) ; Hashtbl.add ht n k)
    end
    array ;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) ht []
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.iter (fun (k, v) -> Printf.fprintf oc "%d,%s;" k @@ show v)


let gen_array oc t name array show =
  Printf.fprintf oc
    "let [@warning \"-42\"] %s x : %s = find ([|%a|] : (int * %s) array) (key x)\n"
    name t (fun oc array -> mk_array oc array show) array t

let gen_country oc =
  gen_array oc "country" "country" countries show_country

let gen_region oc =
  Array.iter
    (fun (country, regions) ->
       gen_array oc "region" (gen_region_variable_name country) regions show_region)
    regions ;
  Printf.fprintf oc "let region = function\n" ;
  Array.iter
    begin fun (k, _) ->
      Printf.fprintf oc "|%s->%s\n" (show_country k) (gen_region_variable_name k)
    end
    regions ;
  gen_not_found oc

let gen_subregion oc =
  Array.iter
    (fun (country, subregions) ->
       gen_array oc "subregion" (gen_subregion_variable_name country) subregions show_subregion)
    subregions ;
  Printf.fprintf oc "let subregion = function\n" ;
  Array.iter
    begin fun (k, _) ->
      Printf.fprintf oc "|%s->%s\n" (show_country k) (gen_subregion_variable_name k)
    end
    subregions ;
  gen_not_found oc

let () =
  let oc = open_out "place_match.ml" in
  Printf.fprintf oc "open Place_types\n" ;
  gen_find oc ;
  gen_country oc ;
  gen_region oc ;
  gen_subregion oc ;
  close_out oc
