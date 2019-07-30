(* $Id: advSearchOk.ml,v 5.14 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

let get_number var key env = p_getint env (var ^ "_" ^ key)

let reconstitute_date_dmy conf var =
  match get_number var "yyyy" conf.env with
    Some y ->
      begin match get_number var "mm" conf.env with
        Some m ->
          begin match get_number var "dd" conf.env with
            Some d ->
              if d >= 1 && d <= 31 && m >= 1 && m <= 12 then
                Some {day = d; month = m; year = y; prec = Sure; delta = 0}
              else None
          | None ->
              if m >= 1 && m <= 12 then
                Some {day = 0; month = m; year = y; prec = Sure; delta = 0}
              else None
          end
      | None -> Some {day = 0; month = 0; year = y; prec = Sure; delta = 0}
      end
  | None -> None

let reconstitute_date conf var =
  match reconstitute_date_dmy conf var with
    Some d -> Some (Dgreg (d, Dgregorian))
  | None -> None

let name_eq x y = Name.abbrev (Name.lower x) = Name.abbrev (Name.lower y)

let rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i = ' ' then skip_spaces x (i + 1)
  else i

let rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i

let string_incl x y =
  let rec loop j_ini =
    if j_ini = String.length y then false
    else
      let rec loop1 i j =
        if i = String.length x then
          if j = String.length y then true
          else
            String.unsafe_get y j = ' ' || String.unsafe_get y (j - 1) = ' '
        else if
          j < String.length y && String.unsafe_get x i = String.unsafe_get y j
        then
          loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
      in
      loop1 0 j_ini
  in
  loop 0

let name_incl x y =
  let x = Name.abbrev (Name.lower x) in
  let y = Name.abbrev (Name.lower y) in string_incl x y

(* Get the field name of an event criteria depending of the search type. *)
let get_event_field_name gets event_criteria event_name search_type =
  if search_type <> "OR" then event_name ^ "_" ^ event_criteria
  else if "on" = gets ("event_" ^ event_name) then event_criteria
  else ""

let advanced_search conf base max_answers =
  let hs = Hashtbl.create 73 in
  let hd = Hashtbl.create 73 in
  let gets x =
    try Hashtbl.find hs x with
      Not_found ->
        let v =
          match p_getenv conf.env x with
            Some v -> v
          | None -> ""
        in
        Hashtbl.add hs x v; v
  in
  (* Search type can be AND or OR. *)
  let search_type = gets "search_type" in
  (* Return empty_field_value if the field is empty. Apply function cmp to the field value. Also check the authorization. *)
  let apply_to_field_value p x cmp empty_default_value =
    let y = gets x in
    if y = "" then empty_default_value
    else if authorized_age conf base p then cmp y
    else false
  in
  (* Check if the date matches with the person event. *)
  let match_date p x df empty_default_value =
    let (d1, d2) =
      try Hashtbl.find hd x with
        Not_found ->
          let v =
            reconstitute_date conf (x ^ "1"), reconstitute_date conf (x ^ "2")
          in
          Hashtbl.add hd x v; v
    in
    authorized_age conf base p
    && match d1, d2 with
      | Some (Dgreg (d1, _)), Some (Dgreg (d2, _)) ->
        begin match df () with
          | Some (Dgreg (d, _)) ->
            Date.compare_dmy d d1 >= 0 && Date.compare_dmy d d2 <= 0
          | _ -> false
        end
      | Some (Dgreg (d1, _)), _ ->
        begin match df () with
          | Some (Dgreg (d, _)) -> Date.compare_dmy d d1 >= 0
          | _ -> false
        end
      | _, Some (Dgreg (d2, _)) ->
        begin match df () with
          | Some (Dgreg (d, _)) -> Date.compare_dmy d d2 <= 0
          | _ -> false
        end
      | _ -> empty_default_value
  in
  let match_sex p empty_default_value =
    apply_to_field_value p "sex"
      (function
         "M" -> get_sex p = Male
       | "F" -> get_sex p = Female
       | _ -> true)
      empty_default_value
  in
  let bapt_date_field_name =
    get_event_field_name gets "date" "bapt" search_type
  in
  let birth_date_field_name =
    get_event_field_name gets "date" "birth" search_type
  in
  let death_date_field_name =
    get_event_field_name gets "date" "death" search_type
  in
  let burial_date_field_name =
    get_event_field_name gets "date" "burial" search_type
  in
  let marriage_date_field_name =
    get_event_field_name gets "date" "marriage" search_type
  in
  let bapt_place_field_name =
    get_event_field_name gets "place" "bapt" search_type
  in
  let birth_place_field_name =
    get_event_field_name gets "place" "birth" search_type
  in
  let death_place_field_name =
    get_event_field_name gets "place" "death" search_type
  in
  let burial_place_field_name =
    get_event_field_name gets "place" "burial" search_type
  in
  let marriage_place_field_name =
    get_event_field_name gets "place" "marriage" search_type
  in
  let match_baptism_date p empty_default_value =
    match_date p bapt_date_field_name
      (fun () -> Adef.od_of_cdate (get_baptism p)) empty_default_value
  in
  let match_birth_date p empty_default_value =
    match_date p birth_date_field_name
      (fun () -> Adef.od_of_cdate (get_birth p)) empty_default_value
  in
  let match_death_date p empty_default_value =
    match_date p death_date_field_name
      (fun () ->
         match get_death p with
           Death (_, cd) -> Some (Adef.date_of_cdate cd)
         | _ -> None)
      empty_default_value
  in
  let match_burial_date p empty_default_value =
    match_date p burial_date_field_name
      (fun () ->
         match get_burial p with
           Buried cod -> Adef.od_of_cdate cod
         | Cremated cod -> Adef.od_of_cdate cod
         | _ -> None)
      empty_default_value
  in
  let match_baptism_place p empty_default_value =
    apply_to_field_value p bapt_place_field_name
      (fun x -> name_incl x (sou base (get_baptism_place p)))
      empty_default_value
  in
  let match_birth_place p empty_default_value =
    apply_to_field_value p birth_place_field_name
      (fun x -> name_incl x (sou base (get_birth_place p)))
      empty_default_value
  in
  let match_death_place p empty_default_value =
    apply_to_field_value p death_place_field_name
      (fun x -> name_incl x (sou base (get_death_place p)))
      empty_default_value
  in
  let match_burial_place p empty_default_value =
    apply_to_field_value p burial_place_field_name
      (fun x -> name_incl x (sou base (get_burial_place p)))
      empty_default_value
  in
  let match_occupation p empty_default_value =
    apply_to_field_value p "occu"
      (fun x -> name_incl x (sou base (get_occupation p))) empty_default_value
  in
  let match_first_name p empty_default_value =
    apply_to_field_value p "first_name"
      (fun x -> name_eq x (p_first_name base p)) empty_default_value
  in
  let match_surname p empty_default_value =
    apply_to_field_value p "surname" (fun x -> name_eq x (p_surname base p))
      empty_default_value
  in
  let match_married p empty_default_value =
    apply_to_field_value p "married"
      (function
         "Y" -> get_family p <> [| |]
       | "N" -> get_family p = [| |]
       | _ -> true)
      empty_default_value
  in
  let match_marriage p x y empty_default_value =
    let (d1, d2) =
      try Hashtbl.find hd x with
        Not_found ->
          let v =
            reconstitute_date conf (x ^ "1"), reconstitute_date conf (x ^ "2")
          in
          Hashtbl.add hd x v; v
    in
    let y = gets y in
    let test_date_place df =
      Array.exists
        (fun ifam ->
           let fam = foi base ifam in
           let father = poi base (get_father fam) in
           let mother = poi base (get_mother fam) in
           if authorized_age conf base father &&
              authorized_age conf base mother
           then
             if y = "" then df (Adef.od_of_cdate (get_marriage fam))
             else
               name_incl y (sou base (get_marriage_place fam)) &&
               df (Adef.od_of_cdate (get_marriage fam))
           else false)
        (get_family p)
    in
    match d1, d2 with
      Some d1, Some d2 ->
        test_date_place
          (function
             Some (Dgreg (_, _) as d) ->
               if Date.compare_date d d1 < 0 then false
               else if Date.compare_date d2 d < 0 then false
               else true
           | _ -> false)
    | Some d1, _ ->
        test_date_place
          (function
             Some (Dgreg (_, _) as d) when authorized_age conf base p ->
               if Date.compare_date d d1 < 0 then false else true
           | _ -> false)
    | _, Some d2 ->
        test_date_place
          (function
             Some (Dgreg (_, _) as d) when authorized_age conf base p ->
               if Date.compare_date d d2 > 0 then false else true
           | _ -> false)
    | _ ->
        if y = "" then empty_default_value
        else
          Array.exists
            (fun ifam ->
               let fam = foi base ifam in
               let father = poi base (get_father fam) in
               let mother = poi base (get_mother fam) in
               if authorized_age conf base father &&
                  authorized_age conf base mother
               then
                 name_incl y (sou base (get_marriage_place fam))
               else false)
            (get_family p)
  in
  (* Check the civil status. The test is the same for an AND or a OR search request. *)
  let match_civil_status p =
    match_sex p true && match_first_name p true && match_surname p true &&
    match_married p true && match_occupation p true
  in
  let match_person ((list, len) as acc) p search_type =
    if search_type <> "OR"
    then if match_civil_status p
         && match_baptism_date p true
         && match_baptism_place p true
         && match_birth_date p true
         && match_birth_place p true
         && match_burial_date p true
         && match_burial_place p true
         && match_death_date p true
         && match_death_place p true
         && match_marriage p marriage_date_field_name marriage_place_field_name true
      then (p :: list, len +1)
      else acc
    else if
      match_civil_status p
      && (gets "place" = "" && gets "date2_yyyy" = ""
          && gets "date1_yyyy" = ""
          || (match_baptism_date p false || match_baptism_place p false)
          && match_baptism_date p true && match_baptism_place p true
          || (match_birth_date p false || match_birth_place p false)
          && match_birth_date p true && match_birth_place p true
          || (match_burial_date p false || match_burial_place p false)
          && match_burial_date p true && match_burial_place p true
          || (match_death_date p false || match_death_place p false)
          && match_death_date p true && match_death_place p true
          || match_marriage p marriage_date_field_name marriage_place_field_name false
         )
    then (p :: list, len + 1)
    else acc
  in
  let list, len =
  if gets "first_name" <> "" || gets "surname" <> "" then
    let (slist, _) =
      if gets "first_name" <> "" then
        Some.persons_of_fsname conf base base_strings_of_first_name
          (spi_find (persons_of_first_name base)) get_first_name
          (gets "first_name")
      else
        Some.persons_of_fsname conf base base_strings_of_surname
          (spi_find (persons_of_surname base)) get_surname (gets "surname")
    in
    let slist = List.fold_right (fun (_, _, l) sl -> l @ sl) slist [] in
    let rec loop ((_, len) as acc) = function
      | [] -> acc
      | _ when len > max_answers -> acc
      | ip :: l ->
        loop (match_person acc (pget conf base ip) search_type) l
    in
    loop ([], 0) slist
  else
    Gwdb.Collection.fold_until
      (fun (_, len) -> len <= max_answers)
      (fun acc i -> match_person acc (pget conf base i) search_type)
      ([], 0)
      (Gwdb.ipers base)
  in
  List.rev list, len

let print_result conf base max_answers (list, len) =
  let list =
    if len > max_answers then Util.reduce_list max_answers list else list
  in
  if len = 0 then Wserver.printf "%s\n" (capitale (transl conf "no match"))
  else
    let () = Perso.build_sosa_ht conf base in
    Wserver.printf "<ul>\n";
    List.iter
      (fun p ->
         Wserver.printf "<li>" ;
         Perso.print_sosa conf base p true;
         Wserver.printf "\n%s%s<em>"
           (referenced_person_text conf base p)
           (DateDisplay.short_dates_text conf base p) ;
         specify_homonymous conf base p false;
         Wserver.printf "</em>")
      list;
    if len > max_answers then Wserver.printf "<li>...</li>";
    Wserver.printf "</ul>\n"

let searching_fields conf =
  let test_string x =
    match p_getenv conf.env x with
      Some v -> if v <> "" then true else false
    | None -> false
  in
  let test_date x =
    reconstitute_date conf (x ^ "1") <> None
    || reconstitute_date conf (x ^ "2") <> None
  in
  let gets x =
    match p_getenv conf.env x with
      Some v -> v
    | None -> ""
  in
  let getd x =
    reconstitute_date conf (x ^ "1"), reconstitute_date conf (x ^ "2")
  in
  let sex =
    match gets "sex" with
      "M" -> 0
    | "F" -> 1
    | _ -> 2
  in
  (* Fonction pour tester un simple champ texte (e.g: first_name). *)
  let string_field x search =
    if test_string x then search ^ " " ^ gets x else search
  in
  (* Returns the place and date request. (e.g.: ...in Paris between 1800 and 1900) *)
  let get_place_date_request place_prefix_field_name date_prefix_field_name
      search =
    let search =
      match getd date_prefix_field_name with
        Some d1, Some d2 ->
          Printf.sprintf "%s %s %s %s %s" search
            (transl conf "between (date)") (DateDisplay.string_of_date conf d1)
            (transl conf "and") (DateDisplay.string_of_date conf d2)
      | Some d1, _ ->
          Printf.sprintf "%s %s %s" search (transl conf "after (date)")
            (DateDisplay.string_of_date conf d1)
      | _, Some d2 ->
          Printf.sprintf "%s %s %s" search (transl conf "before (date)")
            (DateDisplay.string_of_date conf d2)
      | _ -> search
    in
    if test_string place_prefix_field_name then
      search ^ " " ^ transl conf "in (place)" ^ " " ^
      gets place_prefix_field_name
    else search
  in
  (* Returns the event request. (e.g.: born in...) *)
  let get_event_field_request place_prefix_field_name date_prefix_field_name
      event_name search search_type =
    (* Separator character depends on search type operator, a comma for AND search, a slash for OR search. *)
    let sep =
      if search <> "" then if search_type <> "OR" then ", " else " / " else ""
    in
    let search =
      if test_string place_prefix_field_name ||
         test_date date_prefix_field_name
      then
        search ^ sep ^ transl_nth conf event_name sex
      else search
    in
    (* The place and date have to be shown after each event only for the AND request. *)
    if search_type <> "OR" then
      get_place_date_request place_prefix_field_name date_prefix_field_name
        search
    else search
  in
  (* Search type can be AND or OR. *)
  let search_type = gets "search_type" in
  let bapt_date_field_name =
    get_event_field_name gets "date" "bapt" search_type
  in
  let birth_date_field_name =
    get_event_field_name gets "date" "birth" search_type
  in
  let death_date_field_name =
    get_event_field_name gets "date" "death" search_type
  in
  let burial_date_field_name =
    get_event_field_name gets "date" "burial" search_type
  in
  let marriage_date_field_name =
    get_event_field_name gets "date" "marriage" search_type
  in
  let bapt_place_field_name =
    get_event_field_name gets "place" "bapt" search_type
  in
  let birth_place_field_name =
    get_event_field_name gets "place" "birth" search_type
  in
  let death_place_field_name =
    get_event_field_name gets "place" "death" search_type
  in
  let burial_place_field_name =
    get_event_field_name gets "place" "burial" search_type
  in
  let marriage_place_field_name =
    get_event_field_name gets "place" "marriage" search_type
  in
  let search = "" in
  let search = string_field "first_name" search in
  let search = string_field "surname" search in
  let event_search = "" in
  let event_search =
    get_event_field_request birth_place_field_name birth_date_field_name
      "born" event_search search_type
  in
  let event_search =
    get_event_field_request bapt_place_field_name bapt_date_field_name
      "baptized" event_search search_type
  in
  let event_search =
    get_event_field_request marriage_place_field_name marriage_date_field_name
      "married" event_search search_type
  in
  let event_search =
    get_event_field_request death_place_field_name death_date_field_name
      "died" event_search search_type
  in
  let event_search =
    get_event_field_request burial_place_field_name burial_date_field_name
      "buried" event_search search_type
  in
  let search =
    if search = "" then event_search
    else if event_search = "" then search
    else search ^ ", " ^ event_search
  in
  (* Adding the place and date at the end for the OR request. *)
  let search =
    if search_type = "OR" &&
       (gets "place" != "" || gets "date2_yyyy" != "" ||
        gets "date1_yyyy" != "")
    then
      get_place_date_request "place" "date" search
    else search
  in
  let search =
    if not (test_string marriage_place_field_name || test_date "marriage")
    then
      let sep = if search <> "" then ", " else "" in
      if gets "married" = "Y" then
        search ^ sep ^ transl conf "having a family"
      else if gets "married" = "N" then
        search ^ sep ^ transl conf "having no family"
      else search
    else search
  in
  let sep = if search <> "" then "," else "" in
  string_field "occu" (search ^ sep)

let print conf base =
  let title _ =
    Wserver.printf "%s" (capitale (transl_nth conf "advanced request" 0))
  in
  let max_answers =
    match p_getint conf.env "max" with
      Some n -> n
    | None -> 100
  in
  Hutil.header conf title;
  Wserver.printf "<p>\n";
  Wserver.printf "%s %s." (capitale (transl conf "searching all"))
    (searching_fields conf);
  Wserver.printf "</p>\n";
  let list = advanced_search conf base max_answers in
  print_result conf base max_answers list; Hutil.trailer conf
