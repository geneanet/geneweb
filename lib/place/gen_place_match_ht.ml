open Place_types
open Place_data

let key = Place_types.key

let make_ht list =
  let ht = Hashtbl.create (Array.fold_left (fun acc (_, a) -> acc + Array.length a) 0 list) in
  Array.iter
    (fun (v, list) ->
       Array.map key list
       |> Array.to_list
       |> List.sort_uniq compare
       |> List.iter (fun k -> assert (not @@ Hashtbl.mem ht k) ; Hashtbl.add ht k v)
    )
    list ;
  ht

let print_ht oc t ht show =
  Printf.fprintf oc "let ht : (int, %s) Hashtbl.t = Hashtbl.create %d in\n" t (Hashtbl.length ht) ;
  Hashtbl.iter (fun k v -> Printf.fprintf oc "Hashtbl.add ht %d %s ;\n" k (show v)) ht ;
  Printf.fprintf oc "ht\n"

(* let lazy_ht oc t name ht =
 *   Printf.fprintf oc
 *     "let %s = let ht : (int, %s) Hashtbl.t Lazy.t = lazy (Marshal.from_string {%s|%a|%s}) in fun x -> Hashtbl.find (Lazy.force ht) x\n"
 *     name t name (fun oc x -> Marshal.to_channel oc x []) ht name *)

let lazy_ht oc t name ht show =
  Printf.fprintf oc
    "let [@warning \"-42\"]  %s = let ht : (int, %s) Hashtbl.t Lazy.t = lazy begin\n%aend\n in fun x -> Hashtbl.find (Lazy.force ht) (key x)\n"
    name t (fun oc ht -> print_ht oc t ht show) ht

let algeria_region_ht = make_ht algeria_region
let australia_region_ht = make_ht australia_region
let austria_region_ht = make_ht austria_region
let belgium_region_ht = make_ht belgium_region
let canada_region_ht = make_ht canada_region
let france_region_ht = make_ht france_region
let germany_region_ht = make_ht germany_region
let south_africa_region_ht = make_ht south_africa_region
let spain_region_ht = make_ht spain_region
let united_states_region_ht = make_ht united_states_region

let belgium_subregion_ht = make_ht belgium_subregion
let france_subregion_ht = make_ht france_subregion

let gen_country oc =
  lazy_ht oc "country" "country" (make_ht countries) show_country

let gen_region oc =
  Array.iter
    (fun (country, regions) ->
       lazy_ht oc "region" (gen_region_variable_name country) (make_ht regions) show_region)
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
       lazy_ht oc "subregion" (gen_subregion_variable_name country) (make_ht subregions) show_subregion)
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
  gen_country oc ;
  gen_region oc ;
  gen_subregion oc ;
  close_out oc
