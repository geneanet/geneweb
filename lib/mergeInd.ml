(* $Id: mergeInd.ml,v 5.55 2008-01-21 14:02:36 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

let print_differences conf base branches p1 p2 =
  let gen_string_field chk1 chk2 title name proj =
    let x1 = proj p1 in
    let x2 = proj p2 in
    if x1 <> "" && x1 <> "?" && x2 <> "" && x2 <> "?" && x1 <> x2 then
      begin
        Wserver.printf "<h4>%s</h4>\n" (capitale title);
        Wserver.printf "<div class=\"custom-control custom-radio ml-3\">\n";
        Wserver.printf "  <input class=\"custom-control-input\" type=\"radio\" id=\"%s1\" name=\"%s\" value=\"1\"%s>\n" name name chk1;
        Wserver.printf "  <label class=\"custom-control-label\" for=\"%s1\">%s</label>\n" name x1;
        Wserver.printf "</div>\n";
        Wserver.printf "<div class=\"custom-control custom-radio ml-3 mb-2\">\n";
        Wserver.printf "  <input class=\"custom-control-input\" type=\"radio\" id=\"%s2\" name=\"%s\" value=\"2\"%s>\n" name name chk2;
        Wserver.printf "  <label class=\"custom-control-label\" for=\"%s2\">%s</label>\n" name x2;
        Wserver.printf "</div>\n";
      end
  in
  let string_field = gen_string_field " checked" "" in
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Wserver.printf "<p>\n";
  Util.hidden_env conf;
  Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"MRG_IND_OK\"%s>\n"
    conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"i1\" value=\"%d\"%s>\n"
    (Adef.int_of_iper (get_key_index p1)) conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"i2\" value=\"%d\"%s>\n"
    (Adef.int_of_iper (get_key_index p2)) conf.xhs;
  begin let rec loop =
    function
      [ip1, ip2] ->
        Wserver.printf
          "<input type=\"hidden\" name=\"ini1\" value=\"%d\"%s>\n"
          (Adef.int_of_iper ip1) conf.xhs;
        Wserver.printf
          "<input type=\"hidden\" name=\"ini2\" value=\"%d\"%s>\n"
          (Adef.int_of_iper ip2) conf.xhs
    | _ :: branches -> loop branches
    | _ -> ()
  in
    loop branches
  end;
  begin match p_getenv conf.env "m", p_getint conf.env "ip" with
    Some "MRG_DUP_IND_Y_N", Some ip ->
      Wserver.printf "<input type=\"hidden\" name=\"ip\" value=\"%d\"%s>\n" ip
        conf.xhs;
      List.iter
        (fun excl_name ->
           match p_getenv conf.env excl_name with
             Some "" | None -> ()
           | Some s ->
               Wserver.printf
                 "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n"
                 excl_name s conf.xhs)
        ["iexcl"; "fexcl"]
  | _ -> ()
  end;
  Wserver.printf "</p>\n";
  html_p conf;
  string_field (transl_nth conf "first name/first names" 0) "first_name"
    (fun p -> p_first_name base p);
  string_field (transl_nth conf "surname/surnames" 0) "surname"
    (fun p -> p_surname base p);
  begin let select_smallest_num =
    p_first_name base p1 = p_first_name base p2
  in
    gen_string_field
      (if get_occ p1 < get_occ p2 || not select_smallest_num then " checked"
       else "")
      (if get_occ p1 > get_occ p2 && select_smallest_num then " checked"
       else "")
      (transl conf "number") "number"
      (fun p -> string_of_int (get_occ p))
  end;
  string_field (transl_nth conf "image/images" 0) "image"
    (fun p ->
       let v = image_and_size conf base p (limited_image_size 75 100) in
       match v with
         Some (false, link, _) ->
           "<img src=\"" ^ link ^
           "\" style=\"max-width:75px; max-height:100px\" />"
       | _ -> sou base (get_image p));
  string_field (transl conf "public name") "public_name"
    (fun p -> sou base (get_public_name p));
  string_field (transl_nth conf "occupation/occupations" 0) "occupation"
    (fun p -> sou base (get_occupation p));
  string_field (transl conf "sex") "sex"
    (fun p ->
       match get_sex p with
         Male -> "M"
       | Female -> "F"
       | Neuter -> "");
  (*
      string_field False (transl conf "access") "access"
        (fun p ->
           match p.access with
           [ IfTitles -> transl conf "if titles"
           | Private -> "private"
           | Public -> "public" ]);
  *)
  string_field (transl conf "birth") "birth"
    (fun p ->
       match Adef.od_of_cdate (get_birth p) with
         None -> ""
       | Some d -> Date.string_of_ondate conf d);
  string_field (transl conf "birth" ^ " / " ^ transl conf "place")
    "birth_place" (fun p -> sou base (get_birth_place p));
  string_field (transl conf "baptism") "baptism"
    (fun p ->
       match Adef.od_of_cdate (get_baptism p) with
         None -> ""
       | Some d -> Date.string_of_ondate conf d);
  string_field (transl conf "baptism" ^ " / " ^ transl conf "place")
    "baptism_place" (fun p -> sou base (get_baptism_place p));
  string_field (transl conf "death") "death"
    (fun p ->
       let is = 2 in
       match get_death p with
         NotDead -> transl_nth conf "alive" is
       | Death (dr, cd) ->
           let s =
             match dr with
               Killed -> transl_nth conf "killed (in action)" is
             | Murdered -> transl_nth conf "murdered" is
             | Executed -> transl_nth conf "executed (legally killed)" is
             | Disappeared -> transl_nth conf "disappeared" is
             | Unspecified -> transl_nth conf "died" is
           in
           s ^ " " ^ Date.string_of_ondate conf (Adef.date_of_cdate cd)
       | DeadYoung -> transl_nth conf "died young" is
       | DeadDontKnowWhen -> transl_nth conf "died" is
       | DontKnowIfDead | OfCourseDead -> "");
  string_field (transl conf "death" ^ " / " ^ transl conf "place")
    "death_place" (fun p -> sou base (get_death_place p));
  string_field (transl conf "burial") "burial"
    (fun p ->
       let is = 2 in
       match get_burial p with
         UnknownBurial -> ""
       | Buried cod ->
           transl_nth conf "buried" is ^
           (match Adef.od_of_cdate cod with
              None -> ""
            | Some d -> " " ^ Date.string_of_ondate conf d)
       | Cremated cod ->
           transl_nth conf "cremated" is ^
           (match Adef.od_of_cdate cod with
              None -> ""
            | Some d -> " " ^ Date.string_of_ondate conf d));
  string_field (transl conf "burial" ^ " / " ^ transl conf "place")
    "burial_place" (fun p -> sou base (get_burial_place p));
  html_p conf;
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-primary btn-lg\">\n";
  Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</form>\n"

let compatible_cdates cd1 cd2 =
  cd1 = cd2
  || cd2 = Adef.cdate_None
  || cd1 = Adef.cdate_None

let compatible_death_reasons dr1 dr2 = dr1 = dr2 || dr2 = Unspecified

let compatible_deaths d1 d2 =
  if d1 = d2 then true
  else
    match d1, d2 with
      Death (dr1, cd1), Death (dr2, cd2) ->
        compatible_death_reasons dr1 dr2 && compatible_cdates cd1 cd2
    | Death (_, _), NotDead -> false
    | Death (_, _), _ -> true
    | _, DontKnowIfDead -> true
    | DontKnowIfDead, _ -> true
    | _ -> false

let compatible_burials b1 b2 =
  if b1 = b2 then true
  else
    match b1, b2 with
      _, UnknownBurial -> true
    | UnknownBurial, _ -> true
    | Buried cd1, Buried cd2 -> compatible_cdates cd1 cd2
    | Cremated cd1, Cremated cd2 -> compatible_cdates cd1 cd2
    | _ -> false

let compatible_strings s1 s2 =
  eq_istr s1 s2 || is_empty_string s2 || is_empty_string s1

let compatible_divorces d1 d2 = d1 = d2

let compatible_relation_kinds rk1 rk2 = rk1 = rk2

let compatible_titles t1 t2 =
  Futil.eq_lists (Futil.eq_titles eq_istr) t1 t2 || t2 = []

let compatible_pevents pevt1 pevt2 = pevt1 = [] && pevt2 = []

let compatible_fevents fevt1 fevt2 = fevt1 = [] && fevt2 = []

let compatible_strings_lists sl1 sl2 =
  sl2 = [] || Futil.eq_lists eq_istr sl1 sl2

let compatible_notes base s1 s2 =
  compatible_strings s1 s2 || sou base s1 = sou base s2

let compatible_ind base p1 p2 =
  eq_istr (get_first_name p1) (get_first_name p2) &&
  eq_istr (get_surname p1) (get_surname p2) &&
  compatible_strings (get_image p1) (get_image p2) &&
  compatible_strings (get_public_name p1) (get_public_name p2) &&
  compatible_strings_lists (get_qualifiers p1) (get_qualifiers p2) &&
  compatible_strings_lists (get_aliases p1) (get_aliases p2) &&
  compatible_strings_lists (get_first_names_aliases p1)
    (get_first_names_aliases p2) &&
  compatible_strings_lists (get_surnames_aliases p1)
    (get_surnames_aliases p2) &&
  compatible_titles (get_titles p1) (get_titles p2) &&
  compatible_pevents (get_pevents p1) (get_pevents p2) &&
  get_rparents p2 = [] && get_related p2 = [] &&
  compatible_strings (get_occupation p1) (get_occupation p2) &&
  compatible_cdates (get_birth p1) (get_birth p2) &&
  compatible_strings (get_birth_place p1) (get_birth_place p2) &&
  compatible_cdates (get_baptism p1) (get_baptism p2) &&
  compatible_strings (get_baptism_place p1) (get_baptism_place p2) &&
  compatible_deaths (get_death p1) (get_death p2) &&
  compatible_strings (get_death_place p1) (get_death_place p2) &&
  compatible_burials (get_burial p1) (get_burial p2) &&
  compatible_strings (get_burial_place p1) (get_burial_place p2) &&
  compatible_notes base (get_notes p1) (get_notes p2)

let compatible_fam fam1 fam2 =
  compatible_cdates (get_marriage fam1) (get_marriage fam2) &&
  compatible_strings (get_marriage_place fam1) (get_marriage_place fam2) &&
  Array.length (get_witnesses fam2) = 0 &&
  compatible_fevents (get_fevents fam1) (get_fevents fam2) &&
  compatible_relation_kinds (get_relation fam1) (get_relation fam2) &&
  compatible_divorces (get_divorce fam1) (get_divorce fam2) &&
  compatible_strings (get_fsources fam1) (get_fsources fam2)

let propose_merge_ind conf base branches p1 p2 =
  let title _ =
    let s = transl_nth conf "person/persons" 1 in
    Wserver.printf "%s" (capitale (transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  if branches <> [] then
    begin
      Wserver.printf "%s%s\n" (capitale (transl conf "you must first merge"))
        (transl conf ":");
      begin
        Wserver.printf "<ul>\n";
        html_li conf;
        begin
          Wserver.printf "<a href=\"%s%s\">" (commd conf)
            (acces conf base p1);
          Merge.print_someone base p1;
          Wserver.printf "</a>"
        end;
        Wserver.printf "\n%s\n" (transl_nth conf "and" 0);
        begin
          Wserver.printf "<a href=\"%s%s\">" (commd conf)
            (acces conf base p2);
          Merge.print_someone base p2;
          Wserver.printf "</a>"
        end;
        Wserver.printf "\n";
        Wserver.printf "</ul>\n"
      end;
      html_p conf
    end;
  print_differences conf base branches p1 p2;
  if branches <> [] then
    begin
      html_p conf;
      Wserver.printf "<hr%s>\n" conf.xhs;
      html_p conf;
      Wserver.printf "%s%s\n" (capitale (transl_nth conf "branch/branches" 1))
        (transl conf ":");
      html_p conf;
      begin
        Wserver.printf "<table>\n";
        List.iter
          (fun (ip1, ip2) ->
             let p1 = poi base ip1 in
             let p2 = poi base ip2 in
             Wserver.printf "<tr align=\"%s\">\n" conf.left;
             Wserver.printf "<td>\n";
             Wserver.printf "\n%s" (referenced_person_text conf base p1);
             Wserver.printf "%s" (Date.short_dates_text conf base p1);
             Wserver.printf "</td>\n";
             Wserver.printf "<td>\n";
             Wserver.printf "\n%s" (referenced_person_text conf base p2);
             Wserver.printf "%s" (Date.short_dates_text conf base p2);
             Wserver.printf "</td>\n";
             Wserver.printf "</tr>\n")
          ((get_key_index p1, get_key_index p2) :: branches);
        Wserver.printf "</table>\n"
      end
    end;
  Hutil.trailer conf

let reparent_ind base warning ip1 ip2 =
  let a1 = poi base ip1 in
  let a2 = poi base ip2 in
  match get_parents a1, get_parents a2 with
    None, Some ifam ->
      let des = gen_descend_of_descend (foi base ifam) in
      let rec replace i =
        if des.children.(i) = ip2 then des.children.(i) <- ip1
        else replace (i + 1)
      in
      replace 0;
      let a1 = {parents = Some ifam; consang = Adef.fix (-1)} in
      patch_ascend base ip1 a1; patch_descend base ifam des
  | Some ifam, None ->
      let fam = foi base ifam in
      let children = get_children fam in
      begin match CheckItem.sort_children base children with
        Some (b, a) ->
          let des = gen_descend_of_descend fam in
          patch_descend base ifam des;
          warning (ChangedOrderOfChildren (ifam, fam, b, a))
      | None -> ()
      end
  | _ -> ()

let effective_merge_ind conf base warning p1 p2 =
  let u2 = poi base (get_key_index p2) in
  if Array.length (get_family u2) <> 0 then
    begin
      for i = 0 to Array.length (get_family u2) - 1 do
        let ifam = (get_family u2).(i) in
        let cpl = foi base ifam in
        let cpl =
          if get_key_index p2 = get_father cpl then
            Gutil.couple false (get_key_index p1) (get_mother cpl)
          else if get_key_index p2 = get_mother cpl then
            Gutil.couple false (get_father cpl) (get_key_index p1)
          else assert false
        in
        patch_couple base ifam cpl
      done;
      let u1 = {family = Array.append (get_family p1) (get_family u2)} in
      patch_union base (get_key_index p1) u1;
      let u2 = {family = [| |]} in patch_union base (get_key_index p2) u2
    end;
  let p1 =
    {(gen_person_of_person p1) with sex =
      if get_sex p2 <> Neuter then get_sex p2 else get_sex p1;
     birth =
       if get_birth p1 = Adef.cdate_None then get_birth p2 else get_birth p1;
     birth_place =
       if is_empty_string (get_birth_place p1) then get_birth_place p2
       else get_birth_place p1;
     birth_src =
       if is_empty_string (get_birth_src p1) then get_birth_src p2
       else get_birth_src p1;
     baptism =
       if get_baptism p1 = Adef.cdate_None then get_baptism p2
       else get_baptism p1;
     baptism_place =
       if is_empty_string (get_baptism_place p1) then get_baptism_place p2
       else get_baptism_place p1;
     baptism_src =
       if is_empty_string (get_baptism_src p1) then get_baptism_src p2
       else get_baptism_src p1;
     death =
       if get_death p1 = DontKnowIfDead then get_death p2 else get_death p1;
     death_place =
       if is_empty_string (get_death_place p1) then get_death_place p2
       else get_death_place p1;
     death_src =
       if is_empty_string (get_death_src p1) then get_death_src p2
       else get_death_src p1;
     burial =
       if get_burial p1 = UnknownBurial then get_burial p2 else get_burial p1;
     burial_place =
       if is_empty_string (get_burial_place p1) then get_burial_place p2
       else get_burial_place p1;
     burial_src =
       if is_empty_string (get_burial_src p1) then get_burial_src p2
       else get_burial_src p1;
     occupation =
       if is_empty_string (get_occupation p1) then get_occupation p2
       else get_occupation p1;
     notes =
       if is_empty_string (get_notes p1) then get_notes p2 else get_notes p1}
  in
  patch_person base p1.key_index p1;
  reparent_ind base warning p1.key_index (get_key_index p2);
  delete_key base (sou base (get_first_name p2)) (sou base (get_surname p2))
    (get_occ p2);
  let p2 = UpdateIndOk.effective_del base warning p2 in
  patch_person base p2.key_index p2;
  let s =
    let sl =
      [p1.notes; p1.occupation; p1.birth_note; p1.birth_src; p1.baptism_note;
       p1.baptism_src; p1.death_note; p1.death_src; p1.burial_note;
       p1.burial_src; p1.psources]
    in
    let sl =
      let rec loop l accu =
        match l with
          [] -> accu
        | evt :: l -> loop l (evt.epers_note :: evt.epers_src :: accu)
      in
      loop p1.pevents sl
    in
    String.concat " " (List.map (sou base) sl)
  in
  Notes.update_notes_links_db conf (NotesLinks.PgInd p1.key_index) s

let is_ancestor base p1 p2 =
  let ip1 = get_key_index p1 in
  let ip2 = get_key_index p2 in
  let visited = Array.make (nb_of_persons base) false in
  let rec loop ip =
    if visited.(Adef.int_of_iper ip) then false
    else if ip = ip1 then true
    else
      begin
        visited.(Adef.int_of_iper ip) <- true;
        match get_parents (poi base ip) with
          Some ifam ->
            let cpl = foi base ifam in
            loop (get_father cpl) || loop (get_mother cpl)
        | None -> false
      end
  in
  loop ip2

exception Error_loop of person
exception Same_person
exception Different_sexes

let error_loop conf base p =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<strong>%s%s %s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p);
  Wserver.printf "\n%s\n" (transl conf "would be his/her own ancestor");
  Wserver.printf "\n";
  Hutil.trailer conf

let check_ind base p1 p2 =
  if get_key_index p1 = get_key_index p2 then raise Same_person
  else if get_sex p1 <> get_sex p2 && get_sex p1 <> Neuter
          && get_sex p2 <> Neuter
  then raise Different_sexes
  else if is_ancestor base p1 p2 then raise (Error_loop p2)
  (* begin error_loop conf base p2; false, false end *)
  else if is_ancestor base p2 p1 then raise (Error_loop p1)
  (* begin error_loop conf base p1; false, false end *)
  else compatible_ind base p1 p2

let merge_ind conf base warning branches p1 p2 changes_done propose_merge_ind =
  if check_ind base p1 p2 then
    begin effective_merge_ind conf base warning p1 p2; true, true end
  else
    begin propose_merge_ind conf base branches p1 p2; false, changes_done end

let propose_merge_fam conf base branches fam1 fam2 p1 p2 =
  let title _ =
    let s = transl_nth conf "family/families" 1 in
    Wserver.printf "%s" (capitale (transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  Wserver.printf "%s%s\n"
    (capitale (transl conf "you must first merge the 2 families"))
    (transl conf ":");
  Wserver.printf "<ul>\n";
  html_li conf;
  Wserver.printf "<a href=\"%s%s\">" (commd conf) (acces conf base p1);
  Merge.print_someone base p1;
  Wserver.printf "</a>";
  Wserver.printf "\n%s\n" (transl conf "with");
  Wserver.printf "<a href=\"%s%s\">" (commd conf) (acces conf base p2);
  Merge.print_someone base p2;
  Wserver.printf "</a>";
  Wserver.printf "\n";
  Wserver.printf "</ul>\n";
  html_p conf;
  MergeFam.print_differences conf base branches fam1 fam2;
  Hutil.trailer conf

let effective_merge_fam base ifam1 fam1 ifam2 fam2 =
  let des1 = fam1 in
  let des2 = fam2 in
  let fam1 =
    {(gen_family_of_family fam1) with marriage =
      if get_marriage fam1 = Adef.cdate_None then get_marriage fam2
      else get_marriage fam1;
     marriage_place =
       if is_empty_string (get_marriage_place fam1) then
         get_marriage_place fam2
       else get_marriage_place fam1;
     marriage_src =
       if is_empty_string (get_marriage_src fam1) then get_marriage_src fam2
       else get_marriage_src fam1;
     fsources =
       if is_empty_string (get_fsources fam1) then get_fsources fam2
       else get_fsources fam1}
  in
  let des1 =
    let children = Array.append (get_children des1) (get_children des2) in
    let _ = (CheckItem.sort_children base children : _ option) in
    {children = children}
  in
  UpdateFamOk.effective_del base ifam2 fam2;
  for i = 0 to Array.length (get_children des2) - 1 do
    let ip = (get_children des2).(i) in
    let a = {parents = Some ifam1; consang = Adef.fix (-1)} in
    patch_ascend base ip a
  done;
  patch_family base ifam1 fam1;
  patch_descend base ifam1 des1

let merge_fam conf base branches ifam1 ifam2 fam1 fam2 ip1 ip2 changes_done =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  if compatible_fam fam1 fam2 then
    begin
      effective_merge_fam base ifam1 fam1 ifam2 fam2 ;
      true, true
    end
  else
    begin
      propose_merge_fam conf base branches (ifam1, fam1) (ifam2, fam2) p1 p2;
      false, changes_done
    end

let not_found_or_incorrect conf =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s %s %s %s %s\n" (capitale (transl conf "not found"))
    (transl conf "or") (transl conf "several answers") (transl conf "or")
    (transl conf "incorrect request");
  Hutil.trailer conf

let same_person conf =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s\n" (capitale (transl conf "it is the same person!"));
  Hutil.trailer conf

let different_sexes conf =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s.\n" (capitale (transl conf "incompatible sexes"));
  Hutil.trailer conf

let rec try_merge conf base warning branches ip1 ip2 changes_done propose_merge_ind =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  let ok_so_far = true in
  let (ok_so_far, changes_done) =
    match get_parents p1, get_parents p2 with
      Some ifam1, Some ifam2 when ifam1 <> ifam2 ->
        let branches = (ip1, ip2) :: branches in
        let fam1 = foi base ifam1 in
        let fam2 = foi base ifam2 in
        let f1 = get_father fam1 in
        let f2 = get_father fam2 in
        let m1 = get_mother fam1 in
        let m2 = get_mother fam2 in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if f1 = f2 then true, changes_done
            else try_merge conf base ignore branches f1 f2 changes_done propose_merge_ind
          else false, changes_done
        in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if m1 = m2 then true, changes_done
            else try_merge conf base ignore branches m1 m2 changes_done propose_merge_ind
          else false, changes_done
        in
        let (ok_so_far, changes_done) =
          if ok_so_far
          then merge_fam conf base branches ifam1 ifam2 fam1 fam2 f1 m1 changes_done
          else false, changes_done
        in
        ok_so_far, changes_done
    | _ -> ok_so_far, changes_done
  in
  if ok_so_far
  then merge_ind conf base warning branches p1 p2 changes_done propose_merge_ind
  else false, changes_done

let print_merged conf base wl p =
  let title _ = Wserver.printf "%s" (capitale (transl conf "merge done")) in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>\n";
  Wserver.printf "%s\n" (referenced_person_text conf base p);
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n";
  begin match p_getenv conf.env "m", p_getint conf.env "ip" with
    Some "MRG_DUP_IND_Y_N", Some ip ->
      let s1 =
        match p_getenv conf.env "iexcl" with
          Some "" | None -> ""
        | Some s -> "&iexcl=" ^ s
      in
      let s2 =
        match p_getenv conf.env "fexcl" with
          Some "" | None -> ""
        | Some s -> "&fexcl=" ^ s
      in
      Wserver.printf "<p>\n";
      Wserver.printf "<a href=%sm=MRG_DUP&ip=%d%s%s>" (commd conf) ip s1 s2;
      Wserver.printf "%s" (capitale (transl conf "continue merging"));
      Wserver.printf "</a>";
      begin
        let p =  poi base (Adef.iper_of_int ip) in
        let s = person_text conf base p in
        Wserver.printf "\n(%s)\n"
          (Util.transl_a_of_b conf
             (transl conf "possible duplications")
             (reference conf base p s) s)
      end ;
      Wserver.printf "</p>\n"
  | _ -> ()
  end;
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let merge conf base p1 p2 propose_merge_ind =
  let rev_wl = ref [] in
  let warning w = rev_wl := w :: !rev_wl in
  let (ok, changes_done) =
    try_merge conf base warning [] (get_key_index p1) (get_key_index p2)
      false propose_merge_ind
  in
  if changes_done then Util.commit_patches conf base;
  if ok then begin
    let changed =
      let p1 = Util.string_gen_person base (gen_person_of_person p1) in
      let p2 = Util.string_gen_person base (gen_person_of_person p2) in
      U_Merge_person (p2, p1, p1)
    in
    History.record conf base changed "fp" end ;
  Update.delete_topological_sort conf base ;
  (ok, List.rev !rev_wl)

let print conf base =
  let p1 =
    match p_getint conf.env "i" with
      Some i1 -> Some (poi base (Adef.iper_of_int i1))
    | None -> None
  in
  let p2 =
    match p_getint conf.env "i2" with
      Some i2 -> Some (poi base (Adef.iper_of_int i2))
    | None ->
        match p_getenv conf.env "select", p_getenv conf.env "n" with
          (Some "input" | None), Some n ->
            let ipl = Gutil.person_ht_find_all base n in
            begin match ipl with
              [ip2] -> Some (poi base ip2)
            | _ -> None
            end
        | Some x, (Some "" | None) ->
            Some (poi base (Adef.iper_of_int (int_of_string x)))
        | _ -> None
  in
  match p1, p2 with
  | Some p1, Some p2 ->
    begin
      try
        let (ok, warnings) = merge conf base p1 p2 propose_merge_ind in
        if ok then print_merged conf base warnings p1
      with Error_loop p -> error_loop conf base p
         | Different_sexes -> different_sexes conf
         | Same_person -> same_person conf
    end
  | _ -> not_found_or_incorrect conf

(* Undocumented feature... Kill someone's ancestors *)

let rec kill_ancestors conf base included_self p nb_ind nb_fam =
  begin match get_parents p with
    Some ifam ->
      let cpl = foi base ifam in
      kill_ancestors conf base true (poi base (get_father cpl)) nb_ind nb_fam;
      kill_ancestors conf base true (poi base (get_mother cpl)) nb_ind nb_fam;
      UpdateFamOk.effective_del base ifam (foi base ifam);
      incr nb_fam
  | None -> ()
  end;
  if included_self then
    let ip = get_key_index p in
    let warning _ = () in
    let p = UpdateIndOk.effective_del base warning p in
    patch_person base ip p; incr nb_ind

let print_killed conf base p nb_ind nb_fam =
  let title _ = Wserver.printf "Ancestors killed" in
  Hutil.header conf title;
  Wserver.printf "%s's ancestors killed.<br>\n"
    (referenced_person_title_text conf base p);
  Wserver.printf "%d persons and %d families deleted<p>\n" nb_ind nb_fam;
  Hutil.trailer conf

let print_kill_ancestors conf base =
  match p_getenv conf.base_env "can_kill_ancestors" with
    Some "yes" ->
      begin match find_person_in_env conf base "" with
        Some p ->
          let nb_ind = ref 0 in
          let nb_fam = ref 0 in
          kill_ancestors conf base false p nb_ind nb_fam;
          Util.commit_patches conf base;
          let changed =
            U_Kill_ancestors
              (Util.string_gen_person base (gen_person_of_person p))
          in
          History.record conf base changed "ka";
          print_killed conf base p !nb_ind !nb_fam
      | None -> Hutil.incorrect_request conf
      end
  | _ -> Hutil.incorrect_request conf
