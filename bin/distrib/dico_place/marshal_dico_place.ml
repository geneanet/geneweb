open Geneweb

module StrSet = Set.Make (String)

let rest str =
  let i = String.index str ',' in
  String.sub str (i + 1) (String.length str - i - 1)

let write_dico_place_set lang =
  let fname_csv = "dico_place_" ^ lang ^ ".csv" in
  let (towns, area_codes, countys, regions, countrys) =
    let ic = Secure.open_in (Util.search_in_lang_path fname_csv) in
    let string_set_town = ref StrSet.empty in
    let string_set_area_code = ref StrSet.empty in
    let string_set_county = ref StrSet.empty in
    let string_set_region = ref StrSet.empty in
    let string_set_country = ref StrSet.empty in
    begin try while true do
          let line = input_line ic in
          match String.split_on_char ',' line with
          | [ town ; area_code ; county ; region ; country ] ->
            let place = line in
            if town <> "" then string_set_town := StrSet.add place !string_set_town;
            let place = rest place in
            if area_code <> "" then string_set_area_code := StrSet.add place !string_set_area_code;
            let place = rest place in
            if county <> "" then string_set_county := StrSet.add place !string_set_county;
            let place = rest place in
            if region <> "" then string_set_region := StrSet.add place !string_set_region;
            let place = rest place in
            if country <> "" then string_set_country := StrSet.add place !string_set_country;
          | _ -> ()
        done
      with End_of_file -> close_in ic
    end ;
    (List.sort Gutil.alphabetic (StrSet.elements !string_set_town),
     List.sort Gutil.alphabetic (StrSet.elements !string_set_area_code),
     List.sort Gutil.alphabetic (StrSet.elements !string_set_county),
     List.sort Gutil.alphabetic (StrSet.elements !string_set_region),
     List.sort Gutil.alphabetic (StrSet.elements !string_set_country))
  in
  let generate name data =
    let fname_set = "dico_place_" ^ name ^ "_" ^ lang ^ ".list" in
    let ext_flags =
      [ Open_wronly ; Open_append ; Open_creat ; Open_binary ; Open_nonblock ]
    in
    let oc = Secure.open_out_gen ext_flags 0o644 fname_set in
    output_value oc (data : (string list)) ;
    close_out oc
  in
  generate "town" towns ;
  generate "area_code" area_codes ;
  generate "county" countys ;
  generate "region" regions ;
  generate "country" countrys

let _ =
  let lang = ref ["de"; "en"; "es"; "fi"; "fr"; "it"; "nl"; "no"; "pt"; "sv"] in
  let speclist =
    [ ( "-add"
      , Arg.String (fun x -> lang := x :: !lang)
      , "<langs> comma-separated list of languages added to default ones: "
        ^ String.concat "," !lang ^ ".")
    ; ( "-lang"
      , Arg.String (fun x -> lang := String.split_on_char ',' x)
      , "<langs> comma-separated list of languages to process.")
    ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) in
  Arg.parse speclist ignore usage;
  List.iter (write_dico_place_set) !lang
