(* camlp5r ./def_syn.cmo ./pa_html.cmo *)
(* $Id: birthDeath.ml,v 5.40 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;
open Hutil;
open Util;

value get_k conf =
  match p_getint conf.env "k" with
  [ Some x -> x
  | _ ->
      try int_of_string (List.assoc "latest_event" conf.base_env) with
      [ Not_found | Failure _ -> 20 ] ]
;

value select conf base get_date find_oldest =
  let module Q =
    Pqueue.Make
      (struct
         type t = (Gwdb.person * Def.dmy * Def.calendar);
         value leq (_, x, _) (_, y, _) =
           if find_oldest then Date.before_date x y else Date.before_date y x
         ;
       end)
  in
  let n = min (max 0 (get_k conf)) (nb_of_persons base) in
  let ref_date =
    match p_getint conf.env "by" with
    [ Some by ->
        let bm =
          match p_getint conf.env "bm" with
          [ Some x -> x
          | None -> -1 ]
        in
        let bd =
          match p_getint conf.env "bd" with
          [ Some x -> x
          | None -> -1 ]
        in
        Some {day = bd; month = bm; year = by; prec = Sure; delta = 0}
    | None -> None ]
  in
  let rec loop q len i =
    if i = nb_of_persons base then
      let rec loop list q =
        if Q.is_empty q then (list, len)
        else let (e, q) = Q.take q in loop [e :: list] q
      in
      loop [] q
    else
      let p = pget conf base (Adef.iper_of_int i) in
      match get_date p with
      [ Some (Dgreg d cal) ->
          let aft =
            match ref_date with
            [ Some ref_date -> Date.before_date d ref_date
            | None -> False ]
          in
          if aft then loop q len (i + 1)
          else
            let e = (p, d, cal) in
            if len < n then loop (Q.add e q) (len + 1) (i + 1)
            else loop (snd (Q.take (Q.add e q))) len (i + 1)
      | _ -> loop q len (i + 1) ]
  in
  loop Q.empty 0 0
;

value select_family conf base get_date find_oldest =
  let module QF =
    Pqueue.Make
      (struct
         type t = (Adef.ifam * Gwdb.family * Def.dmy * Def.calendar);
         value leq (_, _, x, _) (_, _, y, _) =
           if find_oldest then Date.before_date x y else Date.before_date y x;
       end)
  in
  let n = min (max 0 (get_k conf)) (nb_of_families base) in
  let ref_date =
    match p_getint conf.env "by" with
    [ Some by ->
        let bm =
          match p_getint conf.env "bm" with
          [ Some x -> x
          | None -> -1 ]
        in
        let bd =
          match p_getint conf.env "bd" with
          [ Some x -> x
          | None -> -1 ]
        in
        Some {day = bd; month = bm; year = by; prec = Sure; delta = 0}
    | None -> None ]
  in
  let rec loop q len i =
    if i = nb_of_families base then
      let rec loop list q =
        if QF.is_empty q then (list, len)
        else let (e, q) = QF.take q in loop [e :: list] q
      in
      loop [] q
    else
      let fam = foi base (Adef.ifam_of_int i) in
      if is_deleted_family fam then loop q len (i + 1)
      else
        match get_date (Adef.ifam_of_int i) fam with
        [ Some (Dgreg d cal) ->
            let aft =
              match ref_date with
              [ Some ref_date -> Date.before_date d ref_date
              | None -> False ]
            in
            if aft then loop q len (i + 1)
            else
              let e = (Adef.ifam_of_int i, fam, d, cal) in
              if len < n then loop (QF.add e q) (len + 1) (i + 1)
              else loop (snd (QF.take (QF.add e q))) len (i + 1)
        | _ -> loop q len (i + 1) ]
  in
  loop QF.empty 0 0
;

value print_birth conf base =
  let (list, len) =
    select conf base (fun p -> Adef.od_of_codate (get_birth p)) False
  in
  let title _ =
    Wserver.wprint (fcapitale (ftransl conf "the latest %d births")) len
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<ul>\n";
    let _ =
      List.fold_left
        (fun (last_month_txt, was_future) (p, d, cal) ->
           let month_txt =
             let d = {(d) with day = 0} in
             capitale (Date.string_of_date conf (Dgreg d cal))
           in
           let future = CheckItem.strictly_after_dmy d conf.today in
           do {
             if not future && was_future then do {
               Wserver.wprint "</li>\n</ul>\n</li>\n</ul>\n<p>\n<ul>\n";
               Wserver.wprint "<li>%s\n" month_txt;
               Wserver.wprint "<ul>\n";
             }
             else if month_txt <> last_month_txt then do {
               if last_month_txt = "" then ()
               else Wserver.wprint "</ul>\n</li>\n";
               Wserver.wprint "<li>%s\n" month_txt;
               Wserver.wprint "<ul>\n";
             }
             else ();
             stagn "li" begin
               stag "b" begin
                 Wserver.wprint "%s" (referenced_person_text conf base p);
               end;
               Wserver.wprint ",\n";
               if future then
                 Wserver.wprint "<em>%s</em>.\n"
                   (Date.string_of_date conf (Dgreg d cal))
               else
                 Wserver.wprint "%s <em>%s</em>.\n"
                   (transl_nth conf "born" (index_of_sex (get_sex p)))
                   (Date.string_of_ondate conf (Dgreg d cal));
             end;
             (month_txt, future)
           })
        ("", False) list
    in
    Wserver.wprint "</ul>\n</li>\n</ul>\n";
    trailer conf;
  }
;

value death_date p =
  match get_death p with
  [ Death _ cd -> Some (Adef.date_of_cdate cd)
  | _ -> None ]
;

value print_death conf base =
  let (list, len) = select conf base death_date False in
  let title _ =
    Wserver.wprint (fcapitale (ftransl conf "the latest %t deaths"))
      (fun _ -> string_of_int len)
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    if list <> [] then do {
      Wserver.wprint "<ul>\n";
      let (_, ages_sum, ages_nb) =
        List.fold_left
          (fun (last_month_txt, ages_sum, ages_nb) (p, d, cal) ->
             let month_txt =
               let d = {(d) with day = 0} in
               capitale (Date.string_of_date conf (Dgreg d cal))
             in
             do {
               if month_txt <> last_month_txt then do {
                 if last_month_txt = "" then ()
                 else Wserver.wprint "</ul>\n</li>\n";
                 Wserver.wprint "<li>%s\n" month_txt;
                 Wserver.wprint "<ul>\n";
               }
               else ();
               let (age, ages_sum, ages_nb) =
                 let sure d = d.prec = Sure in
                 match Adef.od_of_codate (get_birth p) with
                 [ Some (Dgreg d1 _) ->
                     if sure d1 && sure d && d1 <> d then
                       let a = CheckItem.time_elapsed d1 d in
                       let ages_sum =
                         match get_sex p with
                         [ Male -> (fst ages_sum + a.year, snd ages_sum)
                         | Female -> (fst ages_sum, snd ages_sum  + a.year)
                         | Neuter -> ages_sum ]
                       in
                       let ages_nb =
                         match get_sex p with
                         [ Male -> (fst ages_nb + 1, snd ages_nb)
                         | Female -> (fst ages_nb, snd ages_nb + 1)
                         | Neuter -> ages_nb ]
                       in
                       (Some a, ages_sum, ages_nb)
                     else
                       (None, ages_sum, ages_nb)
                 | _ -> (None, ages_sum, ages_nb) ]
               in
               stagn "li" begin
                 Wserver.wprint "<b>";
                 Wserver.wprint "%s" (referenced_person_text conf base p);
                 Wserver.wprint "</b>";
                 Wserver.wprint ", %s <em>%s</em>"
                   (transl_nth conf "died" (index_of_sex (get_sex p)))
                   (Date.string_of_ondate conf (Dgreg d cal));
                 match age with
                 [ Some a ->
                     Wserver.wprint " <em>(%s)</em>"
                       (Date.string_of_age conf a)
                 | None -> () ];
               end;
               (month_txt, ages_sum, ages_nb)
             })
          ("", (0, 0), (0, 0)) list
      in
      Wserver.wprint "</ul>\n</li>\n</ul>\n";
      if fst ages_nb >= 3 then
        Wserver.wprint "%s (%s) : %s<br%s>\n"
          (capitale (transl conf "average age at death"))
          (transl_nth conf "M/F" 0)
          (Date.string_of_age conf
             {day = 0; month = 0; year = fst ages_sum / fst ages_nb;
              delta = 0; prec = Sure})
          conf.xhs
      else ();
      if snd ages_nb >= 3 then
        Wserver.wprint "%s (%s) : %s<br%s>\n"
          (capitale (transl conf "average age at death"))
          (transl_nth conf "M/F" 1)
          (Date.string_of_age conf
             {day = 0; month = 0; year = snd ages_sum / snd ages_nb;
              delta = 0; prec = Sure})
          conf.xhs
      else ();
      xtag "br";
      tag "div" "align=\"center\"" begin
        xtag "hr" "width=\"50%%\"";
      end;
      xtag "br";
      let by =
        match p_getenv conf.env "by" with
        [ Some s -> s
        | None -> string_of_int conf.today.year ]
      in
      let bm =
        match p_getenv conf.env "bm" with
        [ Some s -> s
        | None -> string_of_int conf.today.month ]
      in
      let bd =
        match p_getenv conf.env "bd" with
        [ Some s -> s
        | None -> string_of_int conf.today.day ]
      in
      tag "form" "method=\"get\" action=\"%s\"" conf.command begin
        tag "p" begin
          Util.hidden_env conf;
          xtag "input" "type=\"hidden\" name=\"m\" value=\"LD\"";
          let ds =
            Printf.sprintf
              "<input name=\"k\" value=\"%d\" size=\"4\" maxlength=\"4\"%s>"
              len conf.xhs
          in
          Wserver.wprint (fcapitale (ftransl conf "the latest %t deaths"))
            (fun _ -> ds);
          Wserver.wprint "\n... (%s...\n" (transl conf "before");
          xtag "input"
            "name=\"by\" value=\"%s\" size=\"4\" maxlength=\"4\"" by;
          xtag "input"
            "name=\"bm\" value=\"%s\" size=\"2\" maxlength=\"2\"" bm;
          xtag "input"
            "name=\"bd\" value=\"%s\" size=\"2\" maxlength=\"2\"" bd;
          Wserver.wprint ")\n";
          xtag "input" "type=\"submit\" value=\"Ok\"";
        end;
      end;
    }
    else ();
    trailer conf;
  }
;

value print_oldest_alive conf base =
  let limit =
    match p_getint conf.env "lim" with
    [ Some x -> x
    | _ -> 0 ]
  in
  let get_oldest_alive p =
    match get_death p with
    [ NotDead -> Adef.od_of_codate (get_birth p)
    | DontKnowIfDead when limit > 0 ->
        match Adef.od_of_codate (get_birth p) with
        [ Some (Dgreg d _) as x when conf.today.year - d.year <= limit -> x
        | _ -> None ]
    | _ -> None ]
  in
  let (list, len) = select conf base get_oldest_alive True in
  let title _ =
    Wserver.wprint
      (fcapitale (ftransl conf "the %d oldest perhaps still alive")) len
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    tag "ul" begin
      List.iter
        (fun (p, d, cal) ->
           tag "li" begin
             Wserver.wprint "<b>%s</b>,\n"
               (referenced_person_text conf base p);
             Wserver.wprint "%s <em>%s</em>"
               (transl_nth conf "born" (index_of_sex (get_sex p)))
               (Date.string_of_ondate conf (Dgreg d cal));
             if get_death p = NotDead && d.prec = Sure then do {
               let a = CheckItem.time_elapsed d conf.today in
               Wserver.wprint " <em>(%s)</em>" (Date.string_of_age conf a);
             }
             else ();
             Wserver.wprint ".";
           end)
        list;
    end;
    trailer conf;
  }
;

value print_longest_lived conf base =
  let get_longest p =
    if Util.fast_auth_age conf base p then
      match (Adef.od_of_codate (get_birth p), get_death p) with
      [ (Some (Dgreg bd _), Death _ cd) ->
          match Adef.date_of_cdate cd with
          [ Dgreg dd _ ->
              Some (Dgreg (CheckItem.time_elapsed bd dd) Dgregorian)
          | _ -> None ]
      | _ -> None ]
    else None
  in
  let (list, len) = select conf base get_longest False in
  let title _ =
    Wserver.wprint (fcapitale (ftransl conf "the %d who lived the longest"))
      len
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<ul>\n";
    List.iter
      (fun (p, d, cal) ->
         tag "li" begin
           Wserver.wprint "<strong>\n";
           Wserver.wprint "%s" (referenced_person_text conf base p);
           Wserver.wprint "</strong>%s" (Date.short_dates_text conf base p);
           Wserver.wprint "\n(%d %s)" d.year (transl conf "years old");
           Wserver.wprint ".";
         end)
      list;
    Wserver.wprint "</ul>\n\n";
    trailer conf;
  }
;

value print_marr_or_eng conf base title list len =
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<ul>\n";
    let _ =
      List.fold_left
        (fun (last_month_txt, was_future) (ifam, fam, d, cal) ->
           let month_txt =
             let d = {(d) with day = 0} in
             capitale (Date.string_of_date conf (Dgreg d cal))
           in
           let cpl = foi base ifam in
           let future = CheckItem.strictly_after_dmy d conf.today in
           do {
             if not future && was_future then do {
               Wserver.wprint "</ul>\n</li>\n</ul>\n<ul>\n";
               Wserver.wprint "<li>%s\n" month_txt;
               Wserver.wprint "<ul>\n";
             }
             else if month_txt <> last_month_txt then do {
               if last_month_txt = "" then () else
               Wserver.wprint "</ul>\n</li>\n";
               Wserver.wprint "<li>%s\n" month_txt;
               Wserver.wprint "<ul>\n";
             }
             else ();
             stagn "li" begin
               stagn "b" begin
                 Wserver.wprint "%s"
                   (referenced_person_text conf base
                      (pget conf base (get_father cpl)));
               end;
               Wserver.wprint "%s\n" (transl_nth conf "and" 0);
               stag "b" begin
                 Wserver.wprint "%s"
                   (referenced_person_text conf base
                      (pget conf base (get_mother cpl)));
               end;
               Wserver.wprint ",\n";
               if future then
                 Wserver.wprint "<em>%s</em>."
                   (Date.string_of_date conf (Dgreg d cal))
               else
                 Wserver.wprint "%s <em>%s</em>."
                   (match get_relation fam with
                    [ NotMarried | NoSexesCheckNotMarried ->
                        transl_nth conf "relation/relations" 0
                    | Married | NoSexesCheckMarried -> transl conf "married"
                    | Engaged -> transl conf "engaged"
                    | NoMention -> "" ])
                   (Date.string_of_ondate conf (Dgreg d cal));
             end;
             (month_txt, future)
           })
        ("", False) list
    in
    Wserver.wprint "</ul>\n</li>\n</ul>\n";
    trailer conf;
  }
;

value print_marriage conf base =
  let (list, len) =
    select_family conf base
      (fun ifam fam ->
         if get_relation fam = Married then
           Adef.od_of_codate (get_marriage fam)
         else None)
      False
  in
  let title _ =
    Wserver.wprint (fcapitale (ftransl conf "the latest %d marriages")) len
  in
  print_marr_or_eng conf base title list len
;

value print_oldest_engagements conf base =
  let (list, len) =
    select_family conf base
      (fun ifam fam ->
         if get_relation fam = Engaged then
           let husb = pget conf base (get_father fam) in
           let wife = pget conf base (get_mother fam) in
           match (get_death husb, get_death wife) with
           [ (NotDead | DontKnowIfDead, NotDead | DontKnowIfDead) ->
               Adef.od_of_codate (get_marriage fam)
           | _ -> None ]
         else None)
      True
  in
  let title _ =
    Wserver.wprint
     (fcapitale
       (ftransl conf "the %d oldest couples perhaps still alive and engaged"))
     len
  in
  print_marr_or_eng conf base title list len
;

value old_print_statistics conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "statistics")) in
  let n =
    try int_of_string (List.assoc "latest_event" conf.base_env) with
    [ Not_found | Failure _ -> 20 ]
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    tag "ul" begin
      if conf.wizard || conf.friend then do {
        stagn "li" begin
          Wserver.wprint "<a href=\"%sm=LB;k=%d\">" (commd conf) n;
          Wserver.wprint (ftransl conf "the latest %d births") n;
          Wserver.wprint "</a>";
        end;
        stagn "li" begin
          Wserver.wprint "<a href=\"%sm=LD;k=%d\">" (commd conf) n;
          Wserver.wprint (ftransl conf "the latest %t deaths")
            (fun _ -> string_of_int n);
          Wserver.wprint "</a>";
        end;
        stagn "li" begin
          Wserver.wprint "<a href=\"%sm=LM;k=%d\">" (commd conf) n;
          Wserver.wprint (ftransl conf "the latest %d marriages") n;
          Wserver.wprint "</a>";
        end;
        stagn "li" begin
          Wserver.wprint "<a href=\"%sm=OE;k=%d\">" (commd conf) n;
          Wserver.wprint
            (ftransl conf
               "the %d oldest couples perhaps still alive and engaged") n;
          Wserver.wprint "</a>";
        end;
        stagn "li" begin
          Wserver.wprint "<a href=\"%sm=OA;k=%d;lim=0\">" (commd conf) n;
          Wserver.wprint (ftransl conf "the %d oldest perhaps still alive") n;
          Wserver.wprint "</a>";
        end
      }
      else ();
      stagn "li" begin
        Wserver.wprint "<a href=\"%sm=LL;k=%d\">" (commd conf) n;
        Wserver.wprint (ftransl conf "the %d who lived the longest") n;
        Wserver.wprint "</a>";
      end;
    end;
    trailer conf;
  }
;

(* *)

type env 'a =
  [ Vother of 'a
  | Vnone ]
;

value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;

value print_statistics conf base =
  if p_getenv conf.env "old" = Some "on" then old_print_statistics conf base
  else
  Hutil.interp conf base "stats"
    {Templ.eval_var _ = raise Not_found;
     Templ.eval_transl _ = Templ.eval_transl conf;
     Templ.eval_predefined_apply _ = raise Not_found;
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach _ = raise Not_found}
    [] ()
;

value print_population_pyramid conf base = do {
  let interval =
    match p_getint conf.env "int" with
    [ Some i -> max 1 i
    | None -> 5 ]
  in
  let limit =
    match p_getint conf.env "lim" with
    [ Some x -> x
    | _ -> 0 ]
  in
  let at_date =
    match p_getint conf.env "y" with
    [ Some i -> {year = i; month = 31; day = 12; prec = Sure; delta = 0}
    | None -> conf.today ]
  in
  let at_year = at_date.year in
  let nb_intervals = 150 / interval in
  let men = Array.create (nb_intervals + 1) 0 in
  let wom = Array.create (nb_intervals + 1) 0 in
  for i = 0 to nb_of_persons base - 1 do {
    let p = pget conf base (Adef.iper_of_int i) in
    let sex = get_sex p in
    let dea = get_death p in
    if sex <> Neuter then do {
      match Adef.od_of_codate (get_birth p) with
      [ Some (Dgreg dmy _) ->
          if not (Date.before_date dmy at_date) then
            let a = CheckItem.time_elapsed dmy at_date in
            let j = min nb_intervals (a.year / interval) in
            let ok =
              if dea = NotDead || dea = DontKnowIfDead && a.year < limit then
                True
              else
                match dea with
                [ Death _ cd ->
                    match Adef.date_of_cdate cd with
                    [ Dgreg d _ -> Date.before_date d at_date
                    | _ -> False ]
                | _ -> False ]
            in
            if ok then
              if sex = Male then men.(j) := men.(j) + 1
              else wom.(j) := wom.(j) + 1
            else ()
          else ()
      | Some (Dtext _) | None -> () ];
    }
    else ()
  };
  let string_of_nb n =
    Num.to_string_sep (transl conf "(thousand separator)")
      (Num.of_int n)
  in
  let title _ =
    Wserver.wprint "%s (%d)" (capitale (transl conf "population pyramid"))
      at_year
  in
  let print_image doit sex iname =
    stagn "td" begin
      if doit then
        xtag "img" "src=\"%s/%s\" alt=\"%s\" title=\"%s\""
          (Util.image_prefix conf) iname (transl_nth conf "M/F" sex) (transl_nth conf "M/F" sex)
      else Wserver.wprint "&nbsp;";
    end
  in
  Wserver.wrap_string.val := Util.xml_pretty_print;
  Hutil.header conf title;
  print_link_to_welcome conf True;
  let max_hum =
    let max_men = Array.fold_left max 0 men in
    let max_wom = Array.fold_left max 0 wom in
    max 1 (max max_men max_wom)
  in
  let max_size = 70 in
  let band_size n = (2 * max_size * n + max_hum) / (2 * max_hum) in
  let first_interv =
    loop nb_intervals where rec loop i =
      if i <= 0 then 0
      else if men.(i) > 0 || wom.(i) > 0 then i
      else loop (i - 1)
  in
  tag "div" begin
    let c = " cellspacing=\"0\" cellpadding=\"0\"" in
    tag "table id=\"table_pop_pyr\""
      "border=\"%d\"%s style=\"margin: auto\"" conf.border c
    begin
      for i = first_interv downto 0 do {
        let nb_men = men.(i) in
        let nb_wom = wom.(i) in
        tag "tr" begin
          stagn "td" "class=\"pyramid_year\"" begin
            Wserver.wprint "%d" (at_year - i * interval);
          end;
          stagn "td" begin Wserver.wprint "&nbsp;"; end;
          print_image (i = 0) 0 "male.png";
          stagn "td" begin Wserver.wprint "&nbsp;"; end;
          tag "td" "align=\"right\"" begin
            tag "table" "%s" c begin
              tag "tr" begin
                stagn "td" "class=\"pyramid_nb\""
                begin
                  if nb_men <> 0 then Wserver.wprint "%d" nb_men else ();
                  Wserver.wprint "&nbsp;";
                end;
                stagn "td" begin
                  if nb_men = 0 then ()
                  else
                    let n = max 1 (band_size nb_men) in
                    (* On multiplie par 3 parce que c'est *)
                    (* la largeur de l'image : 3 x 14     *)
                    Wserver.wprint
                      ("<img src=\"images/pyr_male.png\" width=%d height=%d />")
                      (n * 3) 14;
                end;
              end;
            end;
          end;
          stagn "td" "align=\"center\"" begin
            if i = nb_intervals then Wserver.wprint "&nbsp;"
            else Wserver.wprint "%d" ((i + 1) * interval);
          end;
          tag "td" "align=\"left\"" begin
            tag "table" "%s" c begin
              tag "tr" begin
                stagn "td" begin
                  if nb_wom = 0 then ()
                  else
                    let n = max 1 (band_size nb_wom) in
                    (* On multiplie par 3 parce que c'est *)
                    (* la largeur de l'image : 3 x 14     *)
                    Wserver.wprint
                      ("<img src=\"images/pyr_female.png\" width=%d height=%d />")
                      (n * 3) 14;
                end;
                stagn "td" "class=\"pyramid_nb\""
                begin
                  Wserver.wprint "&nbsp;";
                  if nb_wom <> 0 then Wserver.wprint "%d" nb_wom else ();
                end;
              end;
            end;
          end;
          stagn "td" begin Wserver.wprint "&nbsp;"; end;
          print_image (i = 0) 1 "female.png";
          stagn "td" begin Wserver.wprint "&nbsp;"; end;
          stagn "td" "class=\"pyramid_year\"" begin
            Wserver.wprint "%d" (at_year - i * interval);
          end;
        end;
      };
    end;
  end;
  let sum_men = Array.fold_left \+ 0 men in
  let sum_wom = Array.fold_left \+ 0 wom in
  tag "p" begin
    Wserver.wprint "%s %s"
      (capitale (transl conf "number of living persons:"))
      (string_of_nb (sum_men + sum_wom));
  end;
  tag "p" begin
    tag "form" "method=\"get\" action=\"%s\"" (commd conf) begin
      hidden_env conf;
      xtag "input" "type=\"hidden\" name=\"m\" value=\"POP_PYR\"";
      xtag "input" "type=\"hidden\" name=\"int\" value=\"%d\"" interval;
      xtag "input" "type=\"hidden\" name=\"lim\" value=\"%d\"" limit;
      Wserver.wprint "%s\n" (transl_nth conf "year/month/day" 0);
      xtag "input" "name=\"y\" value=\"%d\" size=\"5\"" at_year;
    end;
  end;
  Hutil.trailer conf;
};
