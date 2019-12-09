(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

type date_event =
    DeBirth
  | DeDeath of death_reason

let print_anniversary_day conf base dead_people liste =
  Wserver.printf "<ul>\n";
  List.iter
    (fun (p, a, date_event, txt_of) ->
       let is = index_of_sex (get_sex p) in
       Wserver.printf "<li>\n";
       Wserver.printf "%s\n" (txt_of conf base p);
       if not dead_people then Wserver.printf " <em>%d</em>\n" a
       else
         begin let txt =
           match date_event with
             DeBirth -> transl_nth conf "born" is
           | DeDeath Unspecified -> transl_nth conf "died" is
           | DeDeath Killed -> transl_nth conf "killed (in action)" is
           | DeDeath Murdered -> transl_nth conf "murdered" is
           | DeDeath Executed ->
               transl_nth conf "executed (legally killed)" is
           | DeDeath Disappeared -> transl_nth conf "disappeared" is
         in
           Wserver.printf ", <em>%s %s %d</em>\n" txt
             (transl conf "in (year)") a
         end;
       Wserver.printf "</li>\n")
    liste;
  Wserver.printf "</ul>\n"

let gen_print conf base mois f_scan dead_people =
  let tab = Array.make 31 [] in
  let title _ =
    let lab =
      if dead_people then transl conf "anniversaries"
      else transl conf "birthdays"
    in
    Wserver.printf "%s %s" (Utf8.capitalize lab)
      (Util.translate_eval (transl_nth conf "(month)" (mois - 1)))
  in
  begin try
    while true do
      let (p, txt_of) = f_scan () in
      if not dead_people then
        match Adef.od_of_cdate (get_birth p), get_death p with
          Some (Dgreg (d, _)), (NotDead | DontKnowIfDead) ->
            if d.prec = Sure && d.day <> 0 && d.month <> 0 &&
               d.month = mois && d.delta = 0
            then
              if authorized_age conf base p then
                let j = d.day in
                tab.(pred j) <- (p, d.year, DeBirth, txt_of) :: tab.(pred j)
        | _ -> ()
      else
        match get_death p with
          NotDead | DontKnowIfDead -> ()
        | _ ->
            begin match Adef.od_of_cdate (get_birth p) with
              Some (Dgreg (dt, _)) ->
                if dt.prec = Sure && dt.day <> 0 && dt.month <> 0 &&
                   dt.month = mois && dt.delta = 0
                then
                  if authorized_age conf base p then
                    let j = dt.day in
                    tab.(pred j) <-
                      (p, dt.year, DeBirth, txt_of) :: tab.(pred j)
            | _ -> ()
            end;
            match get_death p with
              Death (dr, d) ->
                begin match Adef.date_of_cdate d with
                  Dgreg (dt, _) ->
                    if dt.prec = Sure && dt.day <> 0 && dt.month <> 0 &&
                       dt.month = mois && dt.delta = 0
                    then
                      if authorized_age conf base p then
                        let j = dt.day in
                        let a = dt.year in
                        tab.(pred j) <-
                          (p, a, DeDeath dr, txt_of) :: tab.(pred j)
                | _ -> ()
                end
            | _ -> ()
    done
  with Not_found -> ()
  end;
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  if Array.for_all ((=) []) tab then
    begin
      Wserver.printf "<p>\n";
      Wserver.printf "%s.\n" (Utf8.capitalize (transl conf "no anniversary"));
      Wserver.printf "</p>\n"
    end;
  Wserver.printf "<ul>\n";
  for j = 1 to 31 do
    if tab.(pred j) <> [] then
      begin
        Wserver.printf "<li>\n";
        Wserver.printf "%d\n" j;
        begin let liste =
          List.sort (fun (_, a1, _, _) (_, a2, _, _) -> compare a1 a2)
            tab.(pred j)
        in
          print_anniversary_day conf base dead_people liste
        end;
        Wserver.printf "</li>\n"
      end
  done;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let print_anniversary_list conf base dead_people dt liste =
  let a_ref = dt.year in
  Wserver.printf "<ul>\n";
  List.iter
    (fun (p, a, date_event, txt_of) ->
       Wserver.printf "<li>";
       if dead_people then
         begin
           Wserver.printf "<em>";
           begin match date_event with
             DeBirth -> Wserver.printf "%s" (transl conf "birth")
           | DeDeath _ -> Wserver.printf "%s" (transl conf "death")
           end;
           Wserver.printf "</em>\n";
           Wserver.printf "-&gt; ";
           Wserver.printf "%s" (txt_of conf base p);
           Wserver.printf "\n<em>%s %d" (transl conf "in (year)") a;
           Wserver.printf " (";
           Wserver.printf (ftransl conf "%d years ago") (conf.today.year - a);
           Wserver.printf ")</em>"
         end
       else
         begin
           Wserver.printf "%s" (txt_of conf base p);
           match get_death p with
             NotDead ->
               Wserver.printf " <em>";
               begin match a_ref - a with
                 0 -> Wserver.printf "%s" (transl conf "birth")
               | 1 -> Wserver.printf "%s" (transl conf "one year old")
               | n -> Wserver.printf "%d %s" n (transl conf "years old")
               end;
               Wserver.printf "</em>"
           | _ -> ()
         end;
       Wserver.printf "</li>\n")
    liste;
  Wserver.printf "</ul>\n"

let f_scan conf base =
  let next = Gwdb.Collection.iterator (Gwdb.ipers base) in
  fun () -> match next () with
    | Some i -> (pget conf base i, referenced_person_title_text)
    | None -> raise Not_found

let print_birth conf base mois =
  gen_print conf base mois (f_scan conf base) false
let print_dead conf base mois =
  gen_print conf base mois (f_scan conf base) true

let print_birth_day conf base day_name fphrase wd dt list =
  match list with
    [] ->
      Wserver.printf "<p>\n";
      Wserver.printf "%s %s.\n" (Utf8.capitalize (transl conf "no birthday"))
        day_name;
      Wserver.printf "</p>\n"
  | _ ->
      Wserver.printf "<p>\n";
      begin let txt =
        transl_decline conf "on (weekday day month year)"
          (transl_nth conf "(week day)" wd ^ " " ^ DateDisplay.code_dmy conf dt)
      in
        Wserver.printf fphrase
          (Utf8.capitalize day_name ^ ",\n" ^ std_color conf ("<b>" ^ txt ^ "</b>"))
          (transl conf "the birthday")
      end;
      Wserver.printf "...\n";
      Wserver.printf "</p>\n";
      print_anniversary_list conf base false dt list

let propose_months conf mode =
  begin_centered conf;
  Wserver.printf "<span>";
  Wserver.printf "%s"
    (Utf8.capitalize (transl conf "select a month to see all the anniversaries"));
  Wserver.printf "</span>";
  Wserver.printf "<table border=\"%d\">\n" conf.border;
  Wserver.printf "<tr>\n";
  Wserver.printf "<td>\n";
  Wserver.printf "<form class=\"form-inline\" method=\"get\" action=\"%s\">\n"
    conf.command;
  Wserver.printf "<p>\n";
  Util.hidden_env conf;
  mode ();
  Wserver.printf
    "<select class=\"form-control form-control-lg\" name=\"v\">\n";
  for i = 1 to 12 do
    begin
      Wserver.printf "<option value=\"%d\"%s>" i
        (if i = conf.today.month then " selected=\"selected\"" else "");
      Wserver.printf "%s"
        (Utf8.capitalize (Util.translate_eval (transl_nth conf "(month)" (i - 1))));
      Wserver.printf "</option>\n"
    end
  done;
  Wserver.printf "</select>\n";
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
  Wserver.printf "%s" (Utf8.capitalize (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</p>\n";
  Wserver.printf "</form>\n";
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "</table>\n";
  end_centered conf

let day_after d =
  let (day, r) =
    if d.day >= Date.nb_days_in_month d.month d.year then 1, 1
    else succ d.day, 0
  in
  let (month, r) = if d.month + r > 12 then 1, 1 else d.month + r, 0 in
  let year = d.year + r in
  {day = day; month = month; year = year; prec = Sure; delta = 0}

let print_anniv conf base day_name fphrase wd dt list =
  match list with
    [] ->
      Wserver.printf "<p>\n";
      Wserver.printf "%s %s.\n" (Utf8.capitalize (transl conf "no anniversary"))
        day_name;
      Wserver.printf "</p>\n"
  | _ ->
      Wserver.printf "<p>\n";
      begin let txt =
        transl_decline conf "on (weekday day month year)"
          (transl_nth conf "(week day)" wd ^ " " ^ DateDisplay.code_dmy conf dt)
      in
        Wserver.printf fphrase
          (Utf8.capitalize day_name ^ ",\n" ^ std_color conf ("<b>" ^ txt ^ "</b>"))
          (transl conf "the anniversary")
      end;
      Wserver.printf "...\n";
      Wserver.printf "</p>\n";
      print_anniversary_list conf base true dt list

let print_marriage conf base month =
  let title _ =
    let lab = transl conf "anniversaries of marriage" in
    Wserver.printf "%s %s" (Utf8.capitalize lab)
      (transl_decline conf "in (month year)"
         (transl_nth conf "(month)" (month - 1)))
  in
  let tab = Array.make 31 [] in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Gwdb.Collection.iter (fun ifam ->
    let fam = foi base ifam in
      match Adef.od_of_cdate (get_marriage fam) with
      | Some (Dgreg ({day = d; month = m; year = y; prec = Sure}, _))
        when d <> 0 && m <> 0 ->
        let father = pget conf base (get_father fam) in
        let mother = pget conf base (get_mother fam) in
        if m = month && authorized_age conf base father
           && not (is_hidden father) && authorized_age conf base mother
           && not (is_hidden mother)
        then
          tab.(pred d) <- (fam, y) :: tab.(pred d)
      | _ -> ()
    ) (Gwdb.ifams base) ;
  Wserver.printf "<ul>";
  for i = 1 to 31 do
    match tab.(i-1) with
      [] -> ()
    | l ->
        let l = List.sort (fun (_, y1) (_, y2) -> compare y1 y2) l in
        Wserver.printf "\n";
        Wserver.printf "<li>" ;
        Wserver.printf "%d\n<ul>" i;
        List.iter
          (fun (fam, year) ->
             Wserver.printf "<li>" ;
             Wserver.printf "%s"
               (referenced_person_title_text conf base
                  (pget conf base (get_father fam)));
             Wserver.printf "\n%s\n" (transl_nth conf "and" 0);
             Wserver.printf "%s"
               (referenced_person_title_text conf base
                  (pget conf base (get_mother fam)));
             Wserver.printf ", <em>%s %d</em>\n" (transl conf "in (year)")
               year)
          l;
        Wserver.printf "</ul>\n"
  done;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let print_anniversaries_of_marriage conf base list =
  Wserver.printf "<ul>\n";
  List.iter
    (fun (fam, year) ->
       Wserver.printf "<li>";
       Wserver.printf "%s\n"
         (referenced_person_title_text conf base
            (pget conf base (get_father fam)));
       Wserver.printf "%s\n" (transl_nth conf "and" 0);
       Wserver.printf "%s"
         (referenced_person_title_text conf base
            (pget conf base (get_mother fam)));
       Wserver.printf ", <em>%s %d\n(" (transl conf "in (year)") year;
       Wserver.printf (ftransl conf "%d years ago") (conf.today.year - year);
       Wserver.printf "</em>)";
       Wserver.printf "</li>\n")
    list;
  Wserver.printf "</ul>\n"

let print_marriage_day conf base day_name fphrase wd dt list =
  match list with
    [] ->
      Wserver.printf "<p>\n";
      Wserver.printf "%s %s.\n" (Utf8.capitalize (transl conf "no anniversary"))
        day_name;
      Wserver.printf "</p>\n"
  | _ ->
      Wserver.printf "<p>\n";
      Wserver.printf fphrase
        (Utf8.capitalize day_name ^ ",\n" ^
         std_color conf
           ("<b>" ^
            transl_decline conf "on (weekday day month year)"
              (transl_nth conf "(week day)" wd ^ " " ^
               DateDisplay.code_dmy conf dt) ^
            "</b>"))
        (transl conf "the anniversary of marriage");
      Wserver.printf "...\n";
      Wserver.printf "</p>\n";
      print_anniversaries_of_marriage conf base list

let match_dates conf base p d1 d2 =
  if d1.day = d2.day && d1.month = d2.month then authorized_age conf base p
  else if
    d1.day = 29 && d1.month = 2 && d2.day = 1 && d2.month = 3 &&
    not (Date.leap_year d2.year)
  then
    authorized_age conf base p
  else false

let gen_print_menu_birth conf base f_scan mode =
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "birthdays")) in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  begin match Util.find_person_in_env conf base "" with
    Some p ->
      Perso.interp_notempl_with_menu title "perso_header" conf base p;
      Wserver.printf "<h2>\n";
      title false;
      Wserver.printf "</h2>\n"
  | None -> Hutil.header conf title
  end;
  Hutil.print_link_to_welcome conf true;
  begin try
    while true do
      let (p, txt_of) = f_scan () in
      match Adef.od_of_cdate (get_birth p), get_death p with
        Some (Dgreg (d, _)), (NotDead | DontKnowIfDead) ->
          if d.prec = Sure && d.day <> 0 && d.month <> 0 then
            if match_dates conf base p d conf.today then
              list_tod := (p, d.year, DeBirth, txt_of) :: !list_tod
            else if match_dates conf base p d tom then
              list_tom := (p, d.year, DeBirth, txt_of) :: !list_tom
            else if match_dates conf base p d aft then
              list_aft := (p, d.year, DeBirth, txt_of) :: !list_aft
      | _ -> ()
    done
  with Not_found -> ()
  end;
  List.iter
    (fun xx ->
       xx :=
         List.sort (fun (_, a1, _, _) (_, a2, _, _) -> compare a1 a2) !xx)
    [list_tod; list_tom; list_aft];
  print_birth_day conf base (transl conf "today")
    (ftransl conf "%s, it is %s of") conf.today_wd conf.today !list_tod;
  print_birth_day conf base (transl conf "tomorrow")
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 1) mod 7) tom
    !list_tom;
  print_birth_day conf base (transl conf "the day after tomorrow")
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 2) mod 7) aft
    !list_aft;
  Wserver.printf "\n";
  propose_months conf mode;
  Wserver.printf "\n";
  Hutil.trailer conf

let print_menu_birth conf base =
  let f_scan =
    let next = Gwdb.Collection.iterator (Gwdb.ipers base) in
    fun () -> match next () with
      | Some i -> (pget conf base i, referenced_person_title_text)
      | None -> raise Not_found
  in
  let mode () =
    Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"AN\"%s>\n"
      conf.xhs
  in
  gen_print_menu_birth conf base f_scan mode

let gen_print_menu_dead conf base f_scan mode =
  let title _ =
    Wserver.printf "%s"
      (Utf8.capitalize (transl conf "anniversaries of dead people"))
  in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  begin try
    while true do
      let (p, txt_of) = f_scan () in
      match get_death p with
        NotDead | DontKnowIfDead -> ()
      | _ ->
          begin match Adef.od_of_cdate (get_birth p) with
            Some (Dgreg (d, _)) ->
              if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                if match_dates conf base p d conf.today then
                  list_tod := (p, d.year, DeBirth, txt_of) :: !list_tod
                else if match_dates conf base p d tom then
                  list_tom := (p, d.year, DeBirth, txt_of) :: !list_tom
                else if match_dates conf base p d aft then
                  list_aft := (p, d.year, DeBirth, txt_of) :: !list_aft
          | _ -> ()
          end;
          match get_death p with
            Death (dr, d) ->
              begin match Adef.date_of_cdate d with
                Dgreg (d, _) ->
                  if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                    if match_dates conf base p d conf.today then
                      list_tod := (p, d.year, DeDeath dr, txt_of) :: !list_tod
                    else if match_dates conf base p d tom then
                      list_tom := (p, d.year, DeDeath dr, txt_of) :: !list_tom
                    else if match_dates conf base p d aft then
                      list_aft := (p, d.year, DeDeath dr, txt_of) :: !list_aft
              | _ -> ()
              end
          | _ -> ()
    done
  with Not_found -> ()
  end;
  List.iter
    (fun xx ->
       xx :=
         List.sort (fun (_, a1, _, _) (_, a2, _, _) -> compare a1 a2) !xx)
    [list_tod; list_tom; list_aft];
  print_anniv conf base (transl conf "today") (ftransl conf "%s, it is %s of")
    conf.today_wd conf.today !list_tod;
  print_anniv conf base (transl conf "tomorrow")
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 1) mod 7) tom
    !list_tom;
  print_anniv conf base (transl conf "the day after tomorrow")
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 2) mod 7) aft
    !list_aft;
  Wserver.printf "\n";
  propose_months conf mode;
  Wserver.printf "\n";
  Hutil.trailer conf

let print_menu_dead conf base =
  let f_scan =
    let next = Gwdb.Collection.iterator (Gwdb.ipers base) in
    fun () -> match next () with
      | Some i -> (pget conf base i, referenced_person_title_text)
      | None -> raise Not_found
  in
  let mode () =
    Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"AD\"%s>\n"
      conf.xhs
  in
  gen_print_menu_dead conf base f_scan mode

let match_mar_dates conf base cpl d1 d2 =
  if d1.day = d2.day && d1.month = d2.month then
    authorized_age conf base (pget conf base (get_father cpl)) &&
    authorized_age conf base (pget conf base (get_mother cpl))
  else if
    d1.day = 29 && d1.month = 2 && d2.day = 1 && d2.month = 3 &&
    not (Date.leap_year d2.year)
  then
    authorized_age conf base (pget conf base (get_father cpl)) &&
    authorized_age conf base (pget conf base (get_mother cpl))
  else false

let print_menu_marriage conf base =
  let title _ =
    Wserver.printf "%s" (Utf8.capitalize (transl conf "anniversaries of marriage"))
  in
  let tom = day_after conf.today in
  let aft = day_after tom in
  let list_tod = ref [] in
  let list_tom = ref [] in
  let list_aft = ref [] in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Gwdb.Collection.iter (fun ifam ->
    let fam = foi base ifam in
      match Adef.od_of_cdate (get_marriage fam), get_divorce fam with
      | Some (Dgreg (d, _)), NotDivorced
        when d.day <> 0 && d.month <> 0 && d.prec = Sure ->
          let update_list cpl =
            if match_mar_dates conf base cpl d conf.today then
              list_tod := (cpl, d.year) :: !list_tod
            else if match_mar_dates conf base cpl d tom then
              list_tom := (cpl, d.year) :: !list_tom
            else if match_mar_dates conf base cpl d aft then
              list_aft := (cpl, d.year) :: !list_aft
          in
          if conf.use_restrict then
            let father = pget conf base (get_father fam) in
            let mother = pget conf base (get_mother fam) in
            (if not (is_hidden father) && not (is_hidden mother) then
               update_list fam)
          else update_list fam
      | _ -> ()
    ) (Gwdb.ifams base) ;
  List.iter
    (fun xx -> xx := List.sort (fun (_, y1) (_, y2) -> compare y1 y2) !xx)
    [list_tod; list_tom; list_aft];
  print_marriage_day conf base (transl conf "today")
    (ftransl conf "%s, it is %s of") conf.today_wd conf.today !list_tod;
  print_marriage_day conf base (transl conf "tomorrow")
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 1) mod 7) tom
    !list_tom;
  print_marriage_day conf base (transl conf "the day after tomorrow")
    (ftransl conf "%s, it will be %s of") ((conf.today_wd + 2) mod 7) aft
    !list_aft;
  Wserver.printf "\n";
  let mode () =
    Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"AM\"%s>\n"
      conf.xhs
  in
  propose_months conf mode; Wserver.printf "\n"; Hutil.trailer conf

(* template *)
type 'a env =
    Vother of 'a
  | Vnone

let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let print_anniversaries conf =
  if p_getenv conf.env "old" = Some "on" then ()
  else
    Hutil.interp conf "annivmenu"
      {Templ.eval_var = (fun _ -> raise Not_found);
       Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
       Templ.eval_predefined_apply = (fun _ -> raise Not_found);
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach = fun _ -> raise Not_found}
      [] ()
