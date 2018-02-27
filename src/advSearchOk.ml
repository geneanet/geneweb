(* camlp5r ./pa_html.cmo *)
(* $Id: advSearchOk.ml,v 5.14 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Util;

value get_number var key env = p_getint env (var ^ "_" ^ key);

value reconstitute_date_dmy conf var =
  match get_number var "yyyy" conf.env with
  [ Some y ->
      match get_number var "mm" conf.env with
      [ Some m ->
          match get_number var "dd" conf.env with
          [ Some d ->
              if d >= 1 && d <= 31 && m >= 1 && m <= 12 then
                Some {day = d; month = m; year = y; prec = Sure; delta = 0}
              else None
          | None ->
              if m >= 1 && m <= 12 then
                Some {day = 0; month = m; year = y; prec = Sure; delta = 0}
              else None ]
      | None -> Some {day = 0; month = 0; year = y; prec = Sure; delta = 0} ]
  | None -> None ]
;

value reconstitute_date conf var =
  match reconstitute_date_dmy conf var with
  [ Some d -> Some (Dgreg d Dgregorian)
  | None -> None ]
;

value name_eq x y = Name.abbrev (Name.lower x) = Name.abbrev (Name.lower y);

value rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i = ' ' then skip_spaces x (i + 1)
  else i
;

value rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i
;

value string_incl x y =
  loop 0 where rec loop j_ini =
    if j_ini = String.length y then False
    else
      let rec loop1 i j =
        if i = String.length x then
          if j = String.length y then True
          else (String.unsafe_get y j = ' ' || String.unsafe_get y (j-1) = ' ')
        else if
          j < String.length y &&
          String.unsafe_get x i = String.unsafe_get y j then
          loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
      in
      loop1 0 j_ini
;

value name_incl x y =
  let x = Name.abbrev (Name.lower x) in
  let y = Name.abbrev (Name.lower y) in
  string_incl x y
;

(* Get the field name of an event criteria depending of the search type. *)
value get_event_field_name gets event_criteria event_name search_type =
  if (search_type <> "OR") then
    event_name ^ "_" ^ event_criteria
  else
    if ("on" = gets ("event_" ^ event_name)) then
      event_criteria
    else
      ""
;

value advanced_search conf base max_answers =
  let hs = Hashtbl.create 73 in
  let hd = Hashtbl.create 73 in
  let gets x =
    try Hashtbl.find hs x with
    [ Not_found ->
        let v =
          match p_getenv conf.env x with
          [ Some v -> v
          | None -> "" ]
        in
        do { Hashtbl.add hs x v; v } ]
  in
  (* Search type can be AND or OR. *)
  let search_type = gets "search_type" in
  (* Return empty_field_value if the field is empty. Apply function cmp to the field value. Also check the authorization. *)
  let apply_to_field_value p x cmp empty_default_value =
    let y = gets x in
    if y = "" then empty_default_value else if fast_auth_age conf base p then cmp y else False
  in
  (* Check if the date matches with the person event. *)
  let match_date p x df empty_default_value =
    let (d1, d2) =
      try Hashtbl.find hd x with
      [ Not_found ->
          let v =
            (reconstitute_date conf (x ^ "1"),
             reconstitute_date conf (x ^ "2"))
          in
          do { Hashtbl.add hd x v; v } ]
    in
    match (d1, d2) with
    [ (Some d1, Some d2) ->
        match df () with
        [ Some (Dgreg _ _ as d) when fast_auth_age conf base p ->
            if CheckItem.strictly_before d d1 then False
            else if CheckItem.strictly_before d2 d then False
            else True
        | _ -> False ]
    | (Some d1, _) ->
        match df () with
        [ Some (Dgreg _ _ as d) when fast_auth_age conf base p ->
            if CheckItem.strictly_before d d1 then False else True
        | _ -> False ]
    | (_, Some d2) ->
        match df () with
        [ Some (Dgreg _ _ as d) when fast_auth_age conf base p ->
            if CheckItem.strictly_after d d2 then False else True
        | _ -> False ]
    | _ -> empty_default_value ]
  in
  let match_sex p empty_default_value = apply_to_field_value p
    "sex"
    (fun
     [ "M" -> get_sex p = Male
     | "F" -> get_sex p = Female
     | _ -> True ])
    empty_default_value
  in

  let bapt_date_field_name = get_event_field_name gets "date" "bapt" search_type in
  let birth_date_field_name = get_event_field_name gets "date" "birth" search_type in
  let death_date_field_name = get_event_field_name gets "date" "death" search_type in
  let burial_date_field_name = get_event_field_name gets "date" "burial" search_type in
  let marriage_date_field_name = get_event_field_name gets "date" "marriage" search_type in

  let bapt_place_field_name = get_event_field_name gets "place" "bapt" search_type in
  let birth_place_field_name = get_event_field_name gets "place" "birth" search_type in
  let death_place_field_name = get_event_field_name gets "place" "death" search_type in
  let burial_place_field_name = get_event_field_name gets "place" "burial" search_type in
  let marriage_place_field_name = get_event_field_name gets "place" "marriage" search_type in

  let match_baptism_date p empty_default_value = match_date p bapt_date_field_name (fun () -> Adef.od_of_codate (get_baptism p)) empty_default_value in
  let match_birth_date p empty_default_value = match_date p birth_date_field_name (fun () -> Adef.od_of_codate (get_birth p)) empty_default_value in
  let match_death_date p empty_default_value = match_date p death_date_field_name (fun () ->
    match get_death p with
    [ Death _ cd -> Some (Adef.date_of_cdate cd)
    | _ -> None ]) empty_default_value
  in
  let match_burial_date p empty_default_value = match_date p burial_date_field_name
    (fun () ->
       match get_burial p with
       [ Buried cod -> Adef.od_of_codate cod
       | Cremated cod -> Adef.od_of_codate cod
       | _ -> None ]) empty_default_value
  in

  let match_baptism_place p empty_default_value = apply_to_field_value p bapt_place_field_name (fun x -> name_incl x (sou base (get_baptism_place p))) empty_default_value in
  let match_birth_place p empty_default_value = apply_to_field_value p birth_place_field_name (fun x -> name_incl x (sou base (get_birth_place p))) empty_default_value in
  let match_death_place p empty_default_value = apply_to_field_value p death_place_field_name (fun x -> name_incl x (sou base (get_death_place p))) empty_default_value in
  let match_burial_place p empty_default_value = apply_to_field_value p burial_place_field_name (fun x -> name_incl x (sou base (get_burial_place p))) empty_default_value in
  let match_occupation p empty_default_value = apply_to_field_value p "occu" (fun x -> name_incl x (sou base (get_occupation p))) empty_default_value in
  let match_first_name p empty_default_value = apply_to_field_value p "first_name" (fun x -> name_eq x (p_first_name base p)) empty_default_value in
  let match_surname p empty_default_value = apply_to_field_value p "surname" (fun x -> name_eq x (p_surname base p)) empty_default_value in
  let match_married p empty_default_value = apply_to_field_value p
    "married"
    (fun
     [ "Y" -> get_family p <> [| |]
     | "N" -> get_family p = [| |]
     | _ -> True ])
    empty_default_value
  in
  let match_marriage p x y empty_default_value =
    let (d1, d2) =
      try Hashtbl.find hd x with
      [ Not_found ->
          let v =
            (reconstitute_date conf (x ^ "1"),
             reconstitute_date conf (x ^ "2"))
          in
          do { Hashtbl.add hd x v; v } ]
    in
    let y = gets y in
    let test_date_place df =
      List.exists
        (fun ifam ->
           let fam = foi base ifam in
           let father = poi base (get_father fam) in
           let mother = poi base (get_mother fam) in
           if fast_auth_age conf base father && fast_auth_age conf base mother then
             if y = "" then df (Adef.od_of_codate (get_marriage fam))
             else
               name_incl y (sou base (get_marriage_place fam)) &&
                 df (Adef.od_of_codate (get_marriage fam))
           else False)
        (Array.to_list (get_family p))
    in
    match (d1, d2) with
    [ (Some d1, Some d2) ->
        test_date_place
          (fun
          [ Some (Dgreg _ _ as d) ->
              if CheckItem.strictly_before d d1 then False
              else if CheckItem.strictly_before d2 d then False
              else True
          | _ -> False ])
    | (Some d1, _) ->
        test_date_place
          (fun
          [ Some (Dgreg _ _ as d) when fast_auth_age conf base p ->
              if CheckItem.strictly_before d d1 then False else True
          | _ -> False ])
    | (_, Some d2) ->
        test_date_place
          (fun
          [ Some (Dgreg _ _ as d) when fast_auth_age conf base p ->
              if CheckItem.strictly_after d d2 then False else True
          | _ -> False ])
    | _ ->
        if y = "" then empty_default_value
        else
          List.exists
            (fun ifam ->
              let fam = foi base ifam in
              let father = poi base (get_father fam) in
              let mother = poi base (get_mother fam) in
              if fast_auth_age conf base father && fast_auth_age conf base mother then
                name_incl y (sou base (get_marriage_place fam))
              else False )
            (Array.to_list (get_family p)) ]
  in
  let list = ref [] in
  let len = ref 0 in
  (* Check the civil status. The test is the same for an AND or a OR search request. *)
  let match_civil_status p =
    match_sex p True &&
    match_first_name p True &&
    match_surname p True &&
    match_married p True &&
    match_occupation p True
  in
  let match_person p search_type =
    if (search_type <> "OR") then
        (* AND search. *)
        if (match_civil_status p &&
           match_baptism_date p True &&
           match_baptism_place p True &&
           match_birth_date p True &&
           match_birth_place p True &&
           match_burial_date p True &&
           match_burial_place p True &&
           match_death_date p True &&
           match_death_place p True &&
           match_marriage p marriage_date_field_name marriage_place_field_name True)
        then do {
          list.val := [p :: list.val]; incr len;
        }
        else ()
    else
    (* OR search. *)
    if (match_civil_status p && (
        (* For the case civil status is correct but the event fields are empty. *)
        (gets "place" = "" && gets "date2_yyyy" = "" && gets "date1_yyyy" = "") ||
        (* For each block (baptism, birth, burial, death and marriage), the first part of the condition checks if one of the filled fields matches the person's ones.
           The second part checks the filled fields for the case one of the fields is false (if a field is empty, it is considered valid).
           e.g.: The place is right but the date is wrong, the condition has to return false.
           e.g.: The place is not filled and the date is right, the condition has to return true.
         *)
        (((match_baptism_date p False || match_baptism_place p False) && (match_baptism_date p True && match_baptism_place p True)) ||
        ((match_birth_date p False || match_birth_place p False) && (match_birth_date p True && match_birth_place p True)) ||
        ((match_burial_date p False || match_burial_place p False) && (match_burial_date p True && match_burial_place p True)) ||
        ((match_death_date p False || match_death_place p False) && (match_death_date p True && match_death_place p True)) ||
        match_marriage p marriage_date_field_name marriage_place_field_name False
        )))
    then do {
      list.val := [p :: list.val]; incr len;
    }
    else ()
  in
  do {
    if gets "first_name" <> "" || gets "surname" <> "" then
      let (slist, _) =
        if gets "first_name" <> "" then
          Some.persons_of_fsname conf base base_strings_of_first_name
            (spi_find (persons_of_first_name base)) get_first_name
            (gets "first_name")
        else
          Some.persons_of_fsname conf base base_strings_of_surname
            (spi_find (persons_of_surname base)) get_surname
            (gets "surname")
      in
      let slist = List.fold_right (fun (_, _, l) sl -> l @ sl) slist [] in
      loop slist where rec loop =
        fun
        [ [] -> ()
        | [ip :: l] ->
            if len.val > max_answers then ()
            else do {
              match_person (pget conf base ip) search_type;
              loop l
            } ]
    else
      for i = 0 to nb_of_persons base - 1 do {
        if len.val > max_answers then ()
        else
          match_person (pget conf base (Adef.iper_of_int i)) search_type;
      };
    (List.rev list.val, len.val)
  }
;

value print_result conf base max_answers (list, len) =
  let list =
    if len > max_answers then
      Util.reduce_list max_answers list
    else list
  in
  if len = 0 then
    Wserver.wprint "%s\n" (capitale (transl conf "no match"))
  else
    (* Construction de la table des sosa de la base *)
    let () = Perso.build_sosa_ht conf base in
    tag "ul" begin
      List.iter
        (fun p ->
           do {
             html_li conf;
             Perso.print_sosa conf base p True;
             Wserver.wprint "\n%s" (referenced_person_text conf base p);
             Wserver.wprint "%s" (Date.short_dates_text conf base p);
             stag "em" begin
               specify_homonymous conf base p False;
             end;
           })
        list;
      if len > max_answers then do { html_li conf; Wserver.wprint "...\n"; }
      else ();
    end
;

value searching_fields conf base =
  let test_string x =
    match p_getenv conf.env x with
    [ Some v -> if v <> "" then True else False
    | None -> False ]
  in
  let test_date x =
    match
      (reconstitute_date conf (x ^ "1"), reconstitute_date conf (x ^ "2"))
    with
    [ (Some d1, Some d2) -> True
    | (Some d1, _) -> True
    | (_, Some d2) -> True
    | _ -> False ]
  in
  let gets x =
    match p_getenv conf.env x with
    [ Some v -> v
    | None -> "" ]
  in
  let getd x =
    (reconstitute_date conf (x ^ "1"), reconstitute_date conf (x ^ "2"))
  in
  let sex =
    match gets "sex" with
    [ "M" -> 0
    | "F" -> 1
    | _ -> 2 ]
  in
  (* Fonction pour tester un simple champ texte (e.g: first_name). *)
  let string_field x search =
    if test_string x then search ^ " " ^ gets x
    else search
  in

  (* Returns the place and date request. (e.g.: ...in Paris between 1800 and 1900) *)
  let get_place_date_request place_prefix_field_name date_prefix_field_name search =
    let search =
      match getd date_prefix_field_name with
      [ (Some d1, Some d2) ->
          Printf.sprintf "%s %s %s %s %s"
            search (transl conf "between (date)")
            (Date.string_of_date conf d1)
            (transl conf "and")
            (Date.string_of_date conf d2)
      | (Some d1, _) ->
          Printf.sprintf "%s %s %s"
            search (transl conf "after (date)") (Date.string_of_date conf d1)
      | (_, Some d2) ->
          Printf.sprintf "%s %s %s"
            search (transl conf "before (date)") (Date.string_of_date conf d2)
      | _ -> search ]
      in
      if test_string place_prefix_field_name then
        search ^ " " ^ transl conf "in (place)" ^ " " ^ gets place_prefix_field_name
      else search
  in

  (* Returns the event request. (e.g.: born in...) *)
  let get_event_field_request place_prefix_field_name date_prefix_field_name event_name search search_type =
    (* Separator character depends on search type operator, a comma for AND search, a slash for OR search. *)
    let sep = if search <> "" then if (search_type <> "OR") then ", " else " / " else "" in
    let search =
      if test_string place_prefix_field_name || test_date date_prefix_field_name then
        search ^ sep ^ (transl_nth conf event_name sex)
      else search
    in
    let search =
      (* The place and date have to be shown after each event only for the AND request. *)
      if (search_type <> "OR") then
        get_place_date_request place_prefix_field_name date_prefix_field_name search
      else
        search
    in
    search
  in

  (* Search type can be AND or OR. *)
  let search_type = gets "search_type" in

  let bapt_date_field_name = get_event_field_name gets "date" "bapt" search_type in
  let birth_date_field_name = get_event_field_name gets "date" "birth" search_type in
  let death_date_field_name = get_event_field_name gets "date" "death" search_type in
  let burial_date_field_name = get_event_field_name gets "date" "burial" search_type in
  let marriage_date_field_name = get_event_field_name gets "date" "marriage" search_type in
  let bapt_place_field_name = get_event_field_name gets "place" "bapt" search_type in
  let birth_place_field_name = get_event_field_name gets "place" "birth" search_type in
  let death_place_field_name = get_event_field_name gets "place" "death" search_type in
  let burial_place_field_name = get_event_field_name gets "place" "burial" search_type in
  let marriage_place_field_name = get_event_field_name gets "place" "marriage" search_type in

  let search = "" in
  let search = string_field "first_name" search in
  let search = string_field "surname" search in

  let event_search = "" in
  let event_search = get_event_field_request birth_place_field_name birth_date_field_name "born" event_search search_type in
  let event_search = get_event_field_request bapt_place_field_name bapt_date_field_name "baptized" event_search search_type in
  let event_search = get_event_field_request marriage_place_field_name marriage_date_field_name "married" event_search search_type in
  let event_search = get_event_field_request death_place_field_name death_date_field_name "died" event_search search_type in
  let event_search = get_event_field_request burial_place_field_name burial_date_field_name "buried" event_search search_type in

  let search =
    if (search = "") then
      event_search
    else
      if (event_search = "") then
        search
      else
        search ^ ", " ^ event_search
  in
  (* Adding the place and date at the end for the OR request. *)
  let search =
    if (search_type = "OR" && (gets "place" != "" || gets "date2_yyyy" != "" || gets "date1_yyyy" != "")) then
      get_place_date_request "place" "date" search
    else search in

  let search =
    if not (test_string marriage_place_field_name || test_date "marriage") then
      let sep = if search <> "" then ", " else "" in
      if gets "married" = "Y" then
        search ^ sep ^ transl conf "having a family"
      else if gets "married" = "N" then
        search ^ sep ^ transl conf "having no family"
      else search
    else search
  in
  let search =
    let sep = if search <> "" then "," else "" in
    string_field "occu" (search ^ sep)
  in
  search
;

value print conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "advanced request"))
  in
  let max_answers =
    match p_getint conf.env "max" with
    [ Some n -> n
    | None -> 100 ]
  in
  do {
    header conf title;
    tag "p" begin
      Wserver.wprint "%s: %s."
        (capitale (transl conf "searching all")) (searching_fields conf base);
    end;
    let list = advanced_search conf base max_answers in
    print_result conf base max_answers list;
    trailer conf;
  }
;
