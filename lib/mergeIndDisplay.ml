(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util
open MergeInd

let print_differences conf base branches p1 p2 =
  let gen_string_field chk1 chk2 title name proj =
    let x1 = proj p1 in
    let x2 = proj p2 in
    if x1 <> "" && x1 <> "?" && x2 <> "" && x2 <> "?" && x1 <> x2 then
      begin
        Wserver.printf "<h4>%s</h4>\n" (Utf8.capitalize title);
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
  Wserver.printf "<input type=\"hidden\" name=\"i1\" value=\"%s\"%s>\n"
    (string_of_iper (get_iper p1)) conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"i2\" value=\"%s\"%s>\n"
    (string_of_iper (get_iper p2)) conf.xhs;
  begin let rec loop =
    function
      [ip1, ip2] ->
        Wserver.printf
          "<input type=\"hidden\" name=\"ini1\" value=\"%s\"%s>\n"
          (string_of_iper ip1) conf.xhs;
        Wserver.printf
          "<input type=\"hidden\" name=\"ini2\" value=\"%s\"%s>\n"
          (string_of_iper ip2) conf.xhs
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
  Wserver.printf "</p><p>";
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
       | Some d -> DateDisplay.string_of_ondate conf d);
  string_field (transl conf "birth" ^ " / " ^ transl conf "place")
    "birth_place" (fun p -> sou base (get_birth_place p));
  string_field (transl conf "baptism") "baptism"
    (fun p ->
       match Adef.od_of_cdate (get_baptism p) with
         None -> ""
       | Some d -> DateDisplay.string_of_ondate conf d);
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
           s ^ " " ^ DateDisplay.string_of_ondate conf (Adef.date_of_cdate cd)
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
            | Some d -> " " ^ DateDisplay.string_of_ondate conf d)
       | Cremated cod ->
           transl_nth conf "cremated" is ^
           (match Adef.od_of_cdate cod with
              None -> ""
            | Some d -> " " ^ DateDisplay.string_of_ondate conf d));
  string_field (transl conf "burial" ^ " / " ^ transl conf "place")
    "burial_place" (fun p -> sou base (get_burial_place p));
  Wserver.printf
    {|</p><p><button type="submit" class="btn btn-primary btn-lg">%s</button></form>|}
    (Utf8.capitalize (transl_nth conf "validate/delete" 0))


let propose_merge_ind conf base branches p1 p2 =
  let title _ =
    let s = transl_nth conf "person/persons" 1 in
    Wserver.printf "%s" (Utf8.capitalize (transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  if branches <> [] then begin
    Wserver.printf
      "%s%s<ul><li><a href=\"%s%s\">"
      (Utf8.capitalize (transl conf "you must first merge"))
      (transl conf ":")
      (commd conf)
      (acces conf base p1);
    MergeDisplay.print_someone base p1;
    Wserver.printf "</a> %s <a href=\"%s%s\">"
      (transl_nth conf "and" 0)
      (commd conf)
      (acces conf base p2);
    MergeDisplay.print_someone base p2;
    Wserver.printf "</a></li></ul><p></p>\n"
  end;
  print_differences conf base branches p1 p2;
  if branches <> [] then
    begin
      Wserver.printf
        "<p><hr></p><p>%s%s</p><table>"
        (Utf8.capitalize (transl_nth conf "branch/branches" 1)) (transl conf ":") ;
      List.iter
        (fun (ip1, ip2) ->
           let p1 = poi base ip1 in
           let p2 = poi base ip2 in
           Wserver.printf
             "<tr align=\"%s\"><td>%s%s</td><td>%s%s</td></tr>"
             conf.left
             (referenced_person_text conf base p1)
             (DateDisplay.short_dates_text conf base p1)
             (referenced_person_text conf base p2)
             (DateDisplay.short_dates_text conf base p2))
        ((get_iper p1, get_iper p2) :: branches);
      Wserver.printf "</table>\n"
    end ;
  Hutil.trailer conf

let error_loop conf base p =
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<strong>%s%s %s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p);
  Wserver.printf "\n%s\n" (transl conf "would be his/her own ancestor");
  Wserver.printf "\n";
  Hutil.trailer conf

let propose_merge_fam conf base branches fam1 fam2 p1 p2 =
  let title _ =
    let s = transl_nth conf "family/families" 1 in
    Wserver.printf "%s" (Utf8.capitalize (transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  Wserver.printf "%s%s\n"
    (Utf8.capitalize (transl conf "you must first merge the 2 families"))
    (transl conf ":");
  Wserver.printf "<ul><li><a href=\"%s%s\">"
    (commd conf) (acces conf base p1);
  MergeDisplay.print_someone base p1;
  Wserver.printf "</a> %s <a href=\"%s%s\">"
    (transl conf "with") (commd conf) (acces conf base p2);
  MergeDisplay.print_someone base p2;
  Wserver.printf "</a></li></ul><p>";
  MergeFamDisplay.print_differences conf base branches fam1 fam2;
  Hutil.trailer conf

let not_found_or_incorrect conf =
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s %s %s %s %s\n" (Utf8.capitalize (transl conf "not found"))
    (transl conf "or") (transl conf "several answers") (transl conf "or")
    (transl conf "incorrect request");
  Hutil.trailer conf

let same_person conf =
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s\n" (Utf8.capitalize (transl conf "it is the same person!"));
  Hutil.trailer conf

let different_sexes conf =
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s.\n" (Utf8.capitalize (transl conf "incompatible sexes"));
  Hutil.trailer conf

let print_merged conf base wl p =
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "merge done")) in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>\n";
  Wserver.printf "%s\n" (referenced_person_text conf base p);
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n";
  begin match p_getenv conf.env "m", p_getenv conf.env "ip" with
    Some "MRG_DUP_IND_Y_N", Some ip ->
      let ip = iper_of_string ip in
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
      Wserver.printf "<a href=%sm=MRG_DUP&ip=%s%s%s>" (commd conf) (string_of_iper ip) s1 s2;
      Wserver.printf "%s" (Utf8.capitalize (transl conf "continue merging"));
      Wserver.printf "</a>";
      begin
        let p =  poi base ip in
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

let print conf base =
  let p1 =
    match p_getenv conf.env "i" with
      Some i1 -> Some (poi base (iper_of_string i1))
    | None -> None
  in
  let p2 =
    match p_getenv conf.env "i2" with
      Some i2 -> Some (poi base (iper_of_string i2))
    | None ->
      match p_getenv conf.env "select", p_getenv conf.env "n" with
        (Some "input" | None), Some n ->
        let ipl = Gutil.person_ht_find_all base n in
        begin match ipl with
            [ip2] -> Some (poi base ip2)
          | _ -> None
        end
      | Some x, (Some "" | None) ->
        Some (poi base (iper_of_string x))
      | _ -> None
  in
  match p1, p2 with
  | Some p1, Some p2 ->
    begin
      try
        let (ok, warnings) = merge conf base p1 p2 propose_merge_ind propose_merge_fam in
        if ok then print_merged conf base warnings p1
      with Error_loop p -> error_loop conf base p
         | Different_sexes -> different_sexes conf
         | Same_person -> same_person conf
    end
  | _ -> not_found_or_incorrect conf

(* Undocumented feature... Kill someone's ancestors *)

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
