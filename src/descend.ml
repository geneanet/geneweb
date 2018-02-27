(* camlp5r ./pa_html.cmo *)
(* $Id: descend.ml,v 5.27 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Dag2html;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

value limit_by_tree conf =
  match p_getint conf.base_env "max_desc_tree" with
  [ Some x -> max 1 x
  | None -> 4 ]
;

value text_to conf =
  fun
  [ 0 ->
      transl_decline conf "specify"
        (transl_nth conf "generation/generations" 0)
  | 1 -> transl conf "to the children"
  | 2 -> transl conf "to the grandchildren"
  | 3 -> transl conf "to the great-grandchildren"
  | i ->
      Printf.sprintf (ftransl conf "to the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value text_level conf =
  fun
  [ 0 ->
      transl_decline conf "specify"
        (transl_nth conf "generation/generations" 0)
  | 1 -> transl conf "the children"
  | 2 -> transl conf "the grandchildren"
  | 3 -> transl conf "the great-grandchildren"
  | i ->
      Printf.sprintf (ftransl conf "the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value descendants_title conf base p h =
  let txt_fun = if h then gen_person_text_no_html else gen_person_text in
  let s =
    translate_eval
      (transl_a_of_gr_eq_gen_lev conf
         (transl conf "descendants") (txt_fun raw_access conf base p))
  in
  Wserver.wprint "%s" (capitale s)
;

value display_descendants_level conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let (levt, _) = Perso.make_desc_level_table conf base max_level ancestor in
  let mark = Array.make (Array.length levt) False in
  let rec get_level level u list =
    List.fold_left
      (fun list ifam ->
         let des = foi base ifam in
         let enfants = get_children des in
         List.fold_left
           (fun list ix ->
              let x = pget conf base ix in
              if mark.(Adef.int_of_iper ix) then list
              else
                let _ = mark.(Adef.int_of_iper ix) := True in
                if levt.(Adef.int_of_iper ix) > max_level then list
                else if level = max_level then
                  if p_first_name base x = "x" ||
                     levt.(Adef.int_of_iper ix) != level
                  then list
                  else [x :: list]
                else if level < max_level then
                  get_level (succ level) (pget conf base ix) list
                else list)
           list (Array.to_list enfants))
      list (Array.to_list (get_family u))
  in
  let len = ref 0 in
  let list = get_level 1 (pget conf base (get_key_index ancestor)) [] in
  let list =
    List.sort
      (fun p1 p2 ->
         let c = alphabetic (p_surname base p2) (p_surname base p1) in
         if c = 0 then
           let c =
             alphabetic (p_first_name base p2) (p_first_name base p1)
           in
           if c = 0 then compare (get_occ p2) (get_occ p1) else c
         else c)
      list
  in
  let list =
    List.fold_left
      (fun pl p ->
         match pl with
         [ [(p1, n) :: pl] when get_key_index p = get_key_index p1 ->
             [(p1, succ n) :: pl]
         | _ -> do { incr len; [(p, 1) :: pl] } ])
      [] list
  in
  do {
    header conf (descendants_title conf base ancestor);
    Wserver.wprint "%s" (capitale (text_level conf max_level));
    if len.val > 1 then
      Wserver.wprint " (%d %s)" len.val
        (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1))
    else ();
    Wserver.wprint ".\n";
    html_p conf;
    print_alphab_list conf
      (fun (p, _) ->
         if is_hidden p then "?" else
         String.sub (p_surname base p) (initial (p_surname base p)) 1)
      (fun (p, c) ->
         do {
           Wserver.wprint "\n%s" (referenced_person_title_text conf base p);
           Wserver.wprint "%s" (Date.short_dates_text conf base p);
           if not (is_hidden p) && c > 1 then
             Wserver.wprint " <em>(%d)</em>" c
           else ();
           Wserver.wprint "\n"
         })
      list;
    trailer conf
  }
;

(* With number *)

value mark_descendants conf base marks max_lev ip =
  loop 0 ip (pget conf base ip) where rec loop lev ip u =
    if lev <= max_lev then do {
      marks.(Adef.int_of_iper ip) := True;
      Array.iter
        (fun ifam ->
           let el = get_children (foi base ifam) in
           Array.iter (fun e -> loop (succ lev) e (pget conf base e)) el)
        (get_family u)
    }
    else ()
;

value label_descendants conf base marks paths max_lev =
  loop [] 0 where rec loop path lev p =
    if lev < max_lev then
      let u = p in
      let _ =
        List.fold_left
          (fun cnt ifam ->
             let fam = foi base ifam in
             let c = spouse (get_key_index p) fam in
             let el = get_children fam in
             List.fold_left
               (fun cnt e ->
                  do {
                    if get_sex p = Male || not marks.(Adef.int_of_iper c)
                    then do {
                      let path = [Char.chr (Char.code 'A' + cnt) :: path] in
                      paths.(Adef.int_of_iper e) := path;
                      loop path (succ lev) (pget conf base e)
                    }
                    else ();
                    succ cnt
                  })
               cnt (Array.to_list el))
          0 (Array.to_list (get_family u))
      in
      ()
    else ()
;

value close_lev = 2;

value close_to_end conf base marks max_lev lev p =
  if lev + close_lev >= max_lev then True
  else
    let rec short dlev p =
      let u = p in
      List.for_all
        (fun ifam ->
           let fam = foi base ifam in
           let c = spouse (get_key_index p) fam in
           let el = get_children fam in
           if get_sex p = Male || not marks.(Adef.int_of_iper c) then
             if dlev = close_lev then Array.length el = 0
             else
               List.for_all (fun e -> short (succ dlev) (pget conf base e))
                 (Array.to_list el)
           else True)
        (Array.to_list (get_family u))
    in
    short 1 p
;

value labelled conf base marks max_lev lev ip =
  let a = pget conf base ip in
  let u = a in
  Array.length (get_family u) <> 0 &&
  (match get_parents a with
   [ Some ifam ->
       let fam = foi base ifam in
       List.exists
         (fun ifam ->
            let el = get_children fam in
            List.exists
              (fun ie ->
                 let e = pget conf base ie in
                 let u = e in
                 Array.length (get_family u) <> 0 &&
                 not (close_to_end conf base marks max_lev lev e))
              (Array.to_list el))
         (Array.to_list (get_family (pget conf base (get_father fam))))
   | _ -> False ])
;

value label_of_path paths p =
  loop paths.(Adef.int_of_iper (get_key_index p)) where rec loop =
    fun
    [ [] -> ""
    | [c :: cl] -> loop cl ^ String.make 1 c ]
;

value print_child conf base p1 p2 e = do {
  stag "strong" begin
    if get_sex p1 = Male && eq_istr (get_surname e) (get_surname p1) ||
       get_sex p2 = Male && eq_istr (get_surname e) (get_surname p2)
    then
      Wserver.wprint "%s"
        (referenced_person_text_without_surname conf base e)
    else Wserver.wprint "\n%s" (referenced_person_text conf base e);
  end;
  Wserver.wprint "%s" (Date.short_dates_text conf base e)
};

value print_repeat_child conf base p1 p2 e =
  stag "em" begin
    if get_sex p1 = Male && eq_istr (get_surname e) (get_surname p1) ||
       get_sex p2 = Male && eq_istr (get_surname e) (get_surname p2)
    then
      Wserver.wprint "%s" (person_text_without_surname conf base e)
    else Wserver.wprint "%s" (person_text conf base e);
  end
;

value display_spouse conf base marks paths fam p c =
  do {
    Wserver.wprint "\n&amp;";
    Wserver.wprint "%s" (Date.short_marriage_date_text conf base fam p c);
    Wserver.wprint " ";
    stag "strong" begin
      Wserver.wprint "\n%s" (referenced_person_text conf base c);
    end;
    if marks.(Adef.int_of_iper (get_key_index c)) then
      Wserver.wprint " (<tt><b>%s</b></tt>)" (label_of_path paths c)
    else Wserver.wprint "%s" (Date.short_dates_text conf base c)
  }
;

value total = ref 0;

value print_family_locally conf base marks paths max_lev lev p1 c1 e =
  loop lev e where rec loop lev p =
    if lev < max_lev then
      let _ =
        List.fold_left
          (fun (cnt, first, need_br) ifam ->
             let fam = foi base ifam in
             let c = spouse (get_key_index p) fam in
             let el = get_children fam in
             let c = pget conf base c in
             do {
               if need_br then html_br conf else ();
               if not first then print_repeat_child conf base p1 c1 p else ();
               display_spouse conf base marks paths fam p c;
               Wserver.wprint "\n";
               let print_children =
                 get_sex p = Male ||
                 not marks.(Adef.int_of_iper (get_key_index c))
               in
               if print_children then
                 Wserver.wprint "<ol start=\"%d\">\n" (succ cnt)
               else ();
               let cnt =
                 List.fold_left
                   (fun cnt ie ->
                      let e = pget conf base ie in
                      do {
                        if print_children then do {
                          Wserver.wprint "<li type=\"A\"> ";
                          print_child conf base p c e;
                          Wserver.wprint "\n";
                          incr total;
                          if succ lev = max_lev then
                            list_iter_first
                              (fun first ifam ->
                                 let fam = foi base ifam in
                                 let c1 = spouse ie fam in
                                 let el = get_children fam in
                                 let c1 = pget conf base c1 in
                                 do {
                                   if not first then do {
                                     html_br conf;
                                     print_repeat_child conf base p c e
                                   }
                                   else ();
                                   display_spouse conf base marks paths fam e
                                     c1;
                                   if Array.length el <> 0 then
                                     Wserver.wprint "....."
                                   else ();
                                   Wserver.wprint "\n"
                                 })
                              (Array.to_list (get_family (pget conf base ie)))
                          else loop (succ lev) e
                        }
                        else ();
                        succ cnt
                      })
                   cnt (Array.to_list el)
               in
               if print_children then Wserver.wprint "</ol>\n" else ();
               (cnt, False, not print_children)
             })
          (0, True, False) (Array.to_list (get_family p))
      in
      ()
    else ()
;

value last_label = ref "";

value print_family conf base marks paths max_lev lev p =
  do {
    if lev <> 0 then do {
      Wserver.wprint "<tt><b>%s</b></tt>." (label_of_path paths p);
      html_br conf
    }
    else ();
    let lab = label_of_path paths p in
    if lab < last_label.val then failwith "print_family"
    else last_label.val := lab;
    let _ =
      List.fold_left
        (fun cnt ifam ->
           let fam = foi base ifam in
           let c = spouse (get_key_index p) fam in
           let el = get_children fam in
           let c = pget conf base c in
           do {
             stag "strong" begin
               Wserver.wprint "\n%s" (referenced_person_text conf base p);
             end;
             display_spouse conf base marks paths fam p c;
             Wserver.wprint "<ol start=\"%d\">\n" (succ cnt);
             let cnt =
               List.fold_left
                 (fun cnt ie ->
                    let e = pget conf base ie in
                    do {
                      if get_sex p = Male ||
                         not marks.(Adef.int_of_iper (get_key_index c))
                      then do {
                        Wserver.wprint "<li type=\"A\">";
                        print_child conf base p c e;
                        incr total;
                        Wserver.wprint "\n";
                        if labelled conf base marks max_lev lev ie then
                          Wserver.wprint " => <tt><b>%s</b></tt>\n"
                            (label_of_path paths e)
                        else if succ lev = max_lev then
                          Array.iter
                            (fun ifam ->
                               let fam = foi base ifam in
                               let c = spouse ie fam in
                               let el = get_children fam in
                               let c = pget conf base c in
                               do {
                                 display_spouse conf base marks paths fam e
                                   c;
                                 if Array.length el <> 0 then
                                   Wserver.wprint "....."
                                 else ();
                                 Wserver.wprint "\n"
                               })
                            (get_family (pget conf base ie))
                        else
                          print_family_locally conf base marks paths max_lev
                            (succ lev) p c e
                      }
                      else ();
                      succ cnt
                    })
                 cnt (Array.to_list el)
             in
             Wserver.wprint "</ol>\n";
             cnt
           })
        0 (Array.to_list (get_family p))
    in
    ()
  }
;

value print_families conf base marks paths max_lev =
  loop 0 where rec loop lev p =
    if lev < max_lev then do {
      print_family conf base marks paths max_lev lev p;
      Array.iter
        (fun ifam ->
           let fam = foi base ifam in
           let c = spouse (get_key_index p) fam in
           let el = get_children fam in
           let c = pget conf base c in
           if get_sex p = Male ||
              not marks.(Adef.int_of_iper (get_key_index c))
           then
             Array.iter
               (fun ie ->
                  let e = pget conf base ie in
                  if labelled conf base marks max_lev lev ie then
                    loop (succ lev) e
                  else ())
               el
           else ())
        (get_family p)
    }
    else ()
;

value display_descendants_with_numbers conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title h =
    if h then descendants_title conf base ancestor h
    else
      wprint_geneweb_link conf
        ("m=D;i=" ^
           string_of_int (Adef.int_of_iper (get_key_index ancestor)) ^
           ";v=" ^ string_of_int max_level ^ ";t=G")
        (capitale
           (transl_a_of_gr_eq_gen_lev conf
              (transl conf "descendants") (person_text conf base ancestor)))
  in
  let marks = Array.create (nb_of_persons base) False in
  let paths = Array.create (nb_of_persons base) [] in
  do {
    header conf title;
    total.val := 0;
    Wserver.wprint "%s" (Date.short_dates_text conf base ancestor);
    let p = ancestor in
    if authorized_age conf base p then
      match (Adef.od_of_codate (get_birth p), get_death p) with
      [ (Some _, _) | (_, Death _ _) -> html_br conf
      | _ -> () ]
    else ();
    Wserver.wprint "%s." (capitale (text_to conf max_level));
    html_p conf;
    mark_descendants conf base marks max_level (get_key_index ancestor);
    label_descendants conf base marks paths max_level ancestor;
    print_families conf base marks paths max_level ancestor;
    if total.val > 1 then do {
      html_p conf;
      Wserver.wprint "%s: %d %s" (capitale (transl conf "total")) total.val
        (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
      if max_level > 1 then
        Wserver.wprint " (%s)" (transl conf "spouses not included")
      else ();
      Wserver.wprint ".\n"
    }
    else ();
    trailer conf
  }
;

value print_ref conf base paths p =
  if paths.(Adef.int_of_iper (get_key_index p)) <> [] then
    Wserver.wprint " => <tt><b>%s</b></tt>" (label_of_path paths p)
  else
    Array.iter
      (fun ifam ->
         let c = spouse (get_key_index p) (foi base ifam) in
         if paths.(Adef.int_of_iper c) <> [] then
           let c = pget conf base c in
           Wserver.wprint " => %s %s <tt><b>%s</b></tt>" (p_first_name base c)
             (p_surname base c) (label_of_path paths c)
         else ())
      (get_family p)
;

value print_elem conf base paths precision (n, pll) =
  do {
    html_li conf;
    match List.rev pll with
    [ [[p]] ->
        do {
          Wserver.wprint "<strong>%s %s %s</strong>" (surname_end base n)
            (reference conf base p (person_text_without_surname conf base p))
            (surname_begin base n);
          Wserver.wprint "%s" (Date.short_dates_text conf base p);
          print_ref conf base paths p;
          Wserver.wprint "\n"
        }
    | pll ->
        do {
          Wserver.wprint "<strong>%s%s</strong>\n" (surname_end base n)
            (surname_begin base n);
          tag "ul" begin
            List.iter
              (fun pl ->
                 let several =
                   match pl with
                   [ [_] -> False
                   | _ -> True ]
                 in
                 List.iter
                   (fun p ->
                      do {
                        html_li conf;
                        stag "strong" begin
                          wprint_geneweb_link conf (acces conf base p)
                            (p_first_name base p);
                        end;
                        if several && precision then do {
                          stag "em" begin
                            specify_homonymous conf base p True;
                          end;
                        }
                        else ();
                        Wserver.wprint "%s"
                          (Date.short_dates_text conf base p);
                        print_ref conf base paths p;
                        Wserver.wprint "\n"
                      })
                   pl)
              pll;
          end
        } ]
  }
;

value sort_and_display conf base paths precision list =
  let list = List.map (pget conf base) list in
  let list =
    List.sort
      (fun p1 p2 ->
         let c = alphabetic (p_surname base p2) (p_surname base p1) in
         if c = 0 then
           alphabetic (p_first_name base p2) (p_first_name base p1)
         else c)
      list
  in
  let list =
    List.fold_left
      (fun npll p ->
         match npll with
         [ [(n, pl) :: npll] when n = p_surname base p ->
             [(n, [p :: pl]) :: npll]
         | _ -> [(p_surname base p, [p]) :: npll] ])
      [] list
  in
  let list =
    List.map
      (fun (n, pl) ->
         let pll =
           List.fold_left
             (fun pll p ->
                match pll with
                [ [([p1 :: _] as pl) :: pll]
                  when eq_istr (get_first_name p1) (get_first_name p) ->
                    [[p :: pl] :: pll]
                | _ -> [[p] :: pll] ])
             [] pl
         in
         (n, pll))
      list
  in
  if list <> [] then
    tag "ul" begin List.iter (print_elem conf base paths precision) list; end
  else ()
;

value display_descendant_index conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title h =
    let txt = capitale (transl conf "index of the descendants") in
    if not h then
      wprint_geneweb_link conf
        ("m=D;i=" ^
           string_of_int (Adef.int_of_iper (get_key_index ancestor)) ^
           ";v=" ^ string_of_int max_level ^ ";t=C")
        txt
    else Wserver.wprint "%s" txt
  in
  do {
    header conf title;
    let marks = Array.create (nb_of_persons base) False in
    let paths = Array.create (nb_of_persons base) [] in
    mark_descendants conf base marks max_level (get_key_index ancestor);
    label_descendants conf base marks paths max_level ancestor;
    let list = ref [] in
    for i = 0 to nb_of_persons base - 1 do {
      if paths.(i) <> [] then
        let p = pget conf base (Adef.iper_of_int i) in
        if p_first_name base p <> "?" && p_surname base p <> "?" &&
           p_first_name base p <> "x"
        && (not (is_hide_names conf p) || fast_auth_age conf base p) then
          list.val := [get_key_index p :: list.val]
        else ()
      else ()
    };
    sort_and_display conf base paths True list.val;
    trailer conf
  }
;

value display_spouse_index conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title _ =
    Wserver.wprint "%s"
      (capitale (transl conf "index of the spouses (non descendants)"))
  in
  do {
    header conf title;
    let marks = Array.create (nb_of_persons base) False in
    let paths = Array.create (nb_of_persons base) [] in
    mark_descendants conf base marks max_level (get_key_index ancestor);
    label_descendants conf base marks paths max_level ancestor;
    let list = ref [] in
    for i = 0 to nb_of_persons base - 1 do {
      if paths.(i) <> [] then
        let p = pget conf base (Adef.iper_of_int i) in
        let u = p in
        if p_first_name base p <> "?" && p_surname base p <> "?" &&
           p_first_name base p <> "x" then
          Array.iter
            (fun ifam ->
               let c = spouse (get_key_index p) (foi base ifam) in
               if paths.(Adef.int_of_iper c) = [] then
                 let c = pget conf base c in
                 if p_first_name base c <> "?" && p_surname base c <> "?" &&
                    p_first_name base p <> "x" &&
                    (not (is_hide_names conf c) || fast_auth_age conf base c) &&
                    not (List.mem (get_key_index c) list.val)
                 then
                   list.val := [get_key_index c :: list.val]
                 else ()
               else ())
            (get_family u)
        else ()
      else ()
    };
    sort_and_display conf base paths False list.val;
    trailer conf
  }
;


(* *********************************************************************** *)
(*  [Fonc] print_desc_table_header : config -> base -> int                 *)
(** [Description] : Affiche en fonction des options qui sont sélectionnées
                    le header du tableau de descendance.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - Le nombre de colonnes à afficher (nombre d'options sélectionnées).
    [Rem] : Non exporté en clair hors de ce module.                        *)
(* *********************************************************************** *)
value print_desc_table_header conf base = do {
  let nb_col = ref 2 in
  tag "tr" "class=\"descends_table_header\"" begin
    tag "th" begin
      Wserver.wprint "%s" (capitale (transl conf "n° d'Aboville"));
    end;
    tag "th" begin
      Wserver.wprint "%s" (capitale (transl_nth conf "person/persons" 0));
    end;
    if p_getenv conf.env "birth" = Some "on" then
      tag "th" begin
        incr nb_col;
        Wserver.wprint "%s" (capitale (transl conf "date of birth"));
      end
    else ();
    if p_getenv conf.env "birth_place" = Some "on" then
      tag "th" begin
        incr nb_col;
        Wserver.wprint "%s" (capitale (transl conf "where born"));
      end
    else ();
    if p_getenv conf.env "marr" = Some "on" then
      tag "th" begin
        incr nb_col;
        Wserver.wprint "%s" (capitale (transl_nth conf "spouse/spouses" 1));
      end
    else ();
    if p_getenv conf.env "marr_date" = Some "on" then
      tag "th" begin
        Wserver.wprint "%s" (capitale (transl conf "date of marriage"));
        incr nb_col;
      end
    else ();
    if p_getenv conf.env "marr_place" = Some "on" then
      tag "th" begin
        incr nb_col;
        Wserver.wprint "%s" (capitale (transl conf "where married"));
      end
    else ();
    if p_getenv conf.env "child" = Some "on" then
      tag "th" begin
        incr nb_col;
        Wserver.wprint "%s" (capitale (transl conf "nb children"));
      end
    else ();
    if p_getenv conf.env "death" = Some "on" then
      tag "th" begin
        Wserver.wprint "%s" (capitale (transl conf "date of death"));
        incr nb_col;
      end
    else ();
    if p_getenv conf.env "death_place" = Some "on" then
      tag "th" begin
        incr nb_col;
        Wserver.wprint "%s" (capitale (transl conf "where dead"));
      end
    else ();
    if p_getenv conf.env "death_age" = Some "on" then
      tag "th" begin
        incr nb_col;
        Wserver.wprint "%s" (capitale (transl conf "age at death"));
      end
    else ();
    if p_getenv conf.env "occu" = Some "on" then
      tag "th" begin
        incr nb_col;
        Wserver.wprint "%s"
          (capitale (transl_nth conf "occupation/occupations" 1));
      end
    else ();
  end;
  nb_col.val
};


(* *********************************************************************** *)
(*  [Fonc] print_person_table : config -> base -> person -> string -> unit *)
(** [Description] : Affiche en fonction des options qui sont sélectionnées
                    les informations d'une personne (correspond à une ligne
                    du tableau).
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
      - lab  : numéro d'Aboville de p
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                        *)
(* *********************************************************************** *)
value print_person_table conf base p lab = do {
  let p_auth = Util.authorized_age conf base p in
  let nb_families = Array.length (get_family p) in
  (* On calcul le nombre de rowspan pour avoir un affichage joli. *)
  let rowspan =
    if nb_families > 1 &&
       (p_getenv conf.env "marr" = Some "on" ||
        p_getenv conf.env "marr_date" = Some "on" ||
        p_getenv conf.env "marr_place" = Some "on")
    then "rowspan=\"" ^ string_of_int nb_families ^ "\""
    else ""
  in
  (* On met partout un &nbsp; dans le cas où ce que l'on souhaite *)
  (* afficher est vide, comme ça, on ne casse pas le rowspan.     *)
  tag "tr" begin
    tag "td" "%s" rowspan begin
      Wserver.wprint "%s" lab;
    end;
    tag "td" "%s" rowspan begin
      Util.print_image_sex conf p 11;
      Wserver.wprint " %s &nbsp;" (referenced_person_title_text conf base p);
    end;
    if p_getenv conf.env "birth" = Some "on" then
      tag "td" "%s" rowspan begin
        if get_birth p <> Adef.codate_None then
          let d =
            match Adef.od_of_codate (get_birth p) with
            [ Some d when p_auth -> Date.string_slash_of_date conf d
            | _ -> "&nbsp;" ]
          in
          Wserver.wprint "%s" d
        else
          let d =
            match Adef.od_of_codate (get_baptism p) with
            [ Some d when p_auth -> Date.string_slash_of_date conf d
            | _ -> "&nbsp;" ]
          in
          Wserver.wprint "%s" d;
      end
    else ();
    if p_getenv conf.env "birth_place" = Some "on" then
      tag "td" "%s" rowspan begin
        if get_birth p <> Adef.codate_None then
          Wserver.wprint "%s &nbsp;"
            (if p_auth then
              Util.string_of_place conf (sou base (get_birth_place p))
             else "")
        else
          Wserver.wprint "%s &nbsp;"
            (if p_auth then
              Util.string_of_place conf (sou base (get_baptism_place p))
             else "");
      end
    else ();
    (* On affiche que la première famille (get_family u).(0). *)
    (* Les autres familles seront affichées après qu'on ait   *)
    (* fini de remplir le <tr>.                               *)
    if p_getenv conf.env "marr" = Some "on" then
      tag "td" "%s"
        (if nb_families > 1 then "style=\"border-bottom:none\"" else "")
        begin
        let u = p in
        if nb_families > 0 then
          do {
            let cpl = foi base (get_family u).(0) in
            let spouse =
              pget conf base (Gutil.spouse (get_key_index p) cpl)
            in
            Util.print_image_sex conf spouse 11;
            Wserver.wprint
              " %s &nbsp;" (referenced_person_text conf base spouse);
          }
        else Wserver.wprint "&nbsp;";
      end
    else ();
    (* On affiche que la première famille (get_family u).(0). *)
    (* Les autres familles seront affichées après qu'on ait   *)
    (* fini de remplir le <tr>.                               *)
    if p_getenv conf.env "marr_date" = Some "on" then
      tag "td" "%s"
        (if nb_families > 1 then "style=\"border-bottom:none\"" else "")
        begin
        let u = p in
        if nb_families > 0 then
          let cpl = foi base (get_family u).(0) in
          let spouse =
            pget conf base (Gutil.spouse (get_key_index p) cpl)
          in
          let mdate =
            if authorized_age conf base p &&
               authorized_age conf base spouse then
              let fam = foi base (get_family u).(0) in
              match Adef.od_of_codate (get_marriage fam) with
              [ Some d -> Date.string_slash_of_date conf d
              | _ -> "&nbsp;" ]
            else "&nbsp;"
          in
          Wserver.wprint "%s" mdate
        else Wserver.wprint "&nbsp;";
      end
    else ();
    (* On affiche que la première famille (get_family u).(0). *)
    (* Les autres familles seront affichées après qu'on ait   *)
    (* fini de remplir le <tr>.                               *)
    if p_getenv conf.env "marr_place" = Some "on" then
      tag "td" "%s"
        (if nb_families > 1 then "style=\"border-bottom:none\"" else "")
        begin
        let u = p in
        if nb_families > 0 then
          let cpl = foi base (get_family u).(0) in
          let spouse =
            pget conf base (Gutil.spouse (get_key_index p) cpl)
          in
          let mplace =
            if authorized_age conf base p && authorized_age conf base spouse
            then
              Util.string_of_place conf (sou base (get_marriage_place cpl))
            else ""
          in
          Wserver.wprint "%s &nbsp;" mplace
        else Wserver.wprint "&nbsp;";
      end
    else ();
    (* On affiche que la première famille (get_family u).(0). *)
    (* Les autres familles seront affichées après qu'on ait   *)
    (* fini de remplir le <tr>.                               *)
    if p_getenv conf.env "child" = Some "on" then
      if p_getenv conf.env "marr" = Some "on" ||
         p_getenv conf.env "marr_place" = Some "on" ||
         p_getenv conf.env "marr_date" = Some "on"
      then
        tag "td" "align=\"center\" %s"
          (if nb_families > 1 then "style=\"border-bottom:none\"" else "")
          begin
          let u = p in
          if nb_families > 0 then
            let fam = foi base (get_family u).(0) in
            Wserver.wprint "%d &nbsp;" (Array.length (get_children fam))
          else Wserver.wprint "&nbsp;";
          end
      else
        let n =
          List.fold_left
            (fun n ifam -> n + Array.length (get_children (foi base ifam)))
            0 (Array.to_list (get_family p))
        in
        tag "td" begin
          Wserver.wprint "%d &nbsp;" n;
        end
    else ();
    if p_getenv conf.env "death" = Some "on" then
      tag "td" "%s" rowspan begin
        let d =
          if not p_auth then "&nbsp;"
          else
            match get_death p with
            [ Death _ d ->
                let d = Adef.date_of_cdate d in
                Date.string_slash_of_date conf d
            | _ ->
                match get_burial p with
                [ Cremated cod | Buried cod ->
                    match Adef.od_of_codate cod with
                    [ Some d -> Date.string_slash_of_date conf d
                    | _ -> "&nbsp;" ]
                | _ -> "&nbsp;" ] ]
        in
        Wserver.wprint "%s" d;
      end
    else ();
    if p_getenv conf.env "death_place" = Some "on" then
      tag "td" "%s" rowspan begin
        let d =
          if not p_auth then ""
          else
            match get_death p with
            [ Death _ d ->
                Util.string_of_place conf (sou base (get_death_place p))
            | _ ->
                match get_burial p with
                [ Cremated cod | Buried cod ->
                    match Adef.od_of_codate cod with
                    [ Some d ->
                        Util.string_of_place conf (sou base (get_burial_place p))
                    | _ -> "" ]
                | _ -> "" ] ]
        in
        Wserver.wprint "%s &nbsp;" d;
      end
    else ();
    if p_getenv conf.env "death_age" = Some "on" then
      tag "td" "%s" rowspan begin
        let d =
          if not p_auth then ""
          else
            match Date.get_birth_death_date p with
            [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
               Some (Dgreg ({prec = Sure | About | Maybe} as d2) _), approx)
              when d1 <> d2 ->
                let a = CheckItem.time_elapsed d1 d2 in
                let s =
                  if not approx && d1.prec = Sure && d2.prec = Sure then ""
                  else transl_decline conf "possibly (date)" "" ^ " "
                in
                s ^ Date.string_of_age conf a
            | _ -> "" ]
        in
        Wserver.wprint "%s &nbsp;" d;
      end
    else ();
    if p_getenv conf.env "occu" = Some "on" then
      tag "td" "%s" rowspan begin
        Wserver.wprint
          "%s &nbsp;" (if p_auth then sou base (get_occupation p) else "");
      end
    else ();
  end;
  (* Maintenant qu'on a fini d'afficher le <tr> complet, si il y a  *)
  (* plusieurs familles, il faut alors afficher chacune d'elle dans *)
  (* un <tr> afin d'avoir une mise en page utilisant des rowspan.   *)
  if nb_families > 1 then
    if p_getenv conf.env "marr" = Some "on" ||
       p_getenv conf.env "marr_date" = Some "on" ||
       p_getenv conf.env "marr_place" = Some "on"
    then
      let u = p in
      for i = 1 to nb_families - 1 do {
        let cpl = foi base (get_family u).(i) in
        let spouse =
          pget conf base (Gutil.spouse (get_key_index p) cpl)
        in
        let fam = foi base (get_family u).(i) in
        tag "tr" begin
          if p_getenv conf.env "marr" = Some "on" then
            tag "td" "style=\"border-top:none; %s\""
              (if (nb_families - 1) <> i then "border-bottom:none;" else "")
              begin
                Util.print_image_sex conf spouse 11;
                Wserver.wprint
                  " %s &nbsp;" (referenced_person_text conf base spouse);
            end
          else ();
          if p_getenv conf.env "marr_date" = Some "on" then
            tag "td" "style=\"border-top:none; %s\""
              (if (nb_families - 1) <> i then "border-bottom:none;" else "")
              begin
                let mdate =
                  if authorized_age conf base p &&
                     authorized_age conf base spouse then
                    let fam = foi base (get_family u).(i) in
                    match Adef.od_of_codate (get_marriage fam) with
                    [ Some d -> Date.string_slash_of_date conf d
                    | _ -> "&nbsp;" ]
                  else "&nbsp;"
                in
                Wserver.wprint "%s" mdate;
            end
          else ();
          if p_getenv conf.env "marr_place" = Some "on" then
            tag "td" "style=\"border-top:none; %s\""
              (if (nb_families - 1) <> i then "border-bottom:none;" else "")
              begin
                let mplace =
                  if authorized_age conf base p &&
                     authorized_age conf base spouse then
                    Util.string_of_place conf (sou base (get_marriage_place cpl))
                  else ""
                in
                Wserver.wprint "%s &nbsp;" mplace;
            end
          else ();
          if p_getenv conf.env "child" = Some "on" then
            tag "td" "align=\"center\" style=\"border-top:none; %s\""
              (if (nb_families - 1) <> i then "border-bottom:none;" else "")
              begin
                Wserver.wprint "%d &nbsp;" (Array.length (get_children fam));
            end
          else ();
        end
      }
    else ()
  else ()
};


(* ********************************************************************** *)
(*  [Fonc] build_desc : config -> base -> person list -> person list      *)
(** [Description] : Construit la liste des descendants de la liste des
                    personnes (passée en paramètre). Correspond à un
                    parcours en largeur.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - l    : person list
    [Retour] : person list
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
value build_desc conf base l =
  let rec loop l accu =
    match l with
    [ [] ->
        (* Comme on a fait un fold_left pour avoir le bon ordre *)
        (* des enfants, on renverse l'accumulateur pour l'avoir *)
        (* lui aussi dans le bon ordre.                         *)
        List.rev accu
    | [(p, lab) :: l] ->
        let cnt = ref 0 in
        let nx_accu =
          (* On fait des fold_left pour garder l'ordre des enfants. *)
          (* lab correspond au numéro d'Aboville de p.              *)
          List.fold_left
            (fun accu ifam ->
              let fam = foi base ifam in
              List.fold_left
                (fun accu ip ->
                  let _ = incr cnt in
                  [(pget conf base ip, lab ^ string_of_int cnt.val ^ ".") :: accu])
                accu (Array.to_list (get_children fam)))
            accu (Array.to_list (get_family p))
        in
        loop l nx_accu ]
  in
  loop l []
;


(* ********************************************************************** *)
(*  [Fonc] display_descendant_with_table :
      config -> base -> int -> person -> unit                             *)
(** [Description] : Affiche sous la forme d'un tableau la descendance
                    d'une personne.
    [Args] :
      - conf    : configuration de la base
      - base    : base de donnée
      - max_lev : le nombre de générations à afficher
      - p       : person
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
value display_descendant_with_table conf base max_lev p =
  let max_lev = min (Perso.limit_desc conf) max_lev in
  let nb_pers = ref 0 in
  (* Affiche la liste des personnes, génération par génération.    *)
  (* On affiche toutes les personnes de l, et quand la liste est   *)
  (* vide, on construit la list des descendants de l (mais comme l *)
  (* est vide, on les calcul par rapport à refl)                   *)
  let rec loop lev nb_col first refl l =
    match l with
    [ [] ->
        if lev < max_lev then
          let nl = build_desc conf base refl in
          loop (lev + 1) nb_col True nl nl
        else ()
    | [(p, lab) :: q] ->
        do {
          if first && lev > 0 && p_getenv conf.env "gen" = Some "on" then
            tag "tr" begin
              tag "th" "align=\"left\" colspan=\"%d\"" nb_col begin
                Wserver.wprint "%s %d"
                  (capitale (transl_nth conf "generation/generations" 0)) lev;
              end;
            end
          else ();
          print_person_table conf base p lab;
          incr nb_pers;
          loop lev nb_col False refl q
        } ]
  in
  do {
    header conf (descendants_title conf base p);
    tag "p" begin
      Wserver.wprint "%s." (capitale (text_to conf max_lev));
    end;
    tag "table" "class=descends_table" begin
      (* On affiche l'entête et on en profite pour récupèrer *)
      (* le nombre de colonnes à afficher pour les colspans. *)
      let nb_col = print_desc_table_header conf base in
      loop 0 nb_col True [(p, "")] [(p, "")];
    end;
    tag "p" begin
      Wserver.wprint "%s: %d %s"
        (capitale (transl conf "total"))
        nb_pers.val
        (transl_nth conf "person/persons" 1);
    end;
    trailer conf
  }
;

value make_tree_hts conf base gv p =
  let bd =
    match Util.p_getint conf.env "bd" with
    [ Some x -> x
    | None -> 0 ]
  in
  let td_prop =
    match Util.p_getenv conf.env "td" with
    [ Some x -> " " ^ x
    | _ ->
        match Util.p_getenv conf.env "color" with
        [ None | Some "" -> ""
        | Some x -> " class=\"" ^ x ^ "\"" ] ]
  in
  let rec nb_column n v u =
    if v = 0 then n + (max 1 (Array.length (get_family u)))
    else if Array.length (get_family u) = 0 then n + 1
    else
      List.fold_left (fun n ifam -> fam_nb_column n v (foi base ifam)) n
        (Array.to_list (get_family u))
  and fam_nb_column n v des =
    if Array.length (get_children des) = 0 then n + 1
    else
      List.fold_left
        (fun n iper -> nb_column n (v - 1) (pget conf base iper)) n
        (Array.to_list (get_children des))
  in
  let vertical_bar_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    let td =
      match po with
      [ Some (p, _) ->
          (* Récupère les options d'affichage. *)
          let options = Util.display_options conf in
          let ncol = nb_column 0 (v - 1) p in
          let vbar_txt =
            Printf.sprintf "%sm=D;t=T;v=%d;%s;%s" (commd conf) gv options
              (acces conf base p)
          in
          (2 * ncol - 1, CenterA, TDbar (Some vbar_txt))
      | None -> (1, LeftA, TDnothing) ]
    in
    [td :: tdl]
  in
  let children_vertical_bars v gen =
    let tdl = List.fold_left (vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let spouses_vertical_bar_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    match po with
    [ Some (p, _) when Array.length (get_family p) > 0 ->
        fst
          (List.fold_left
             (fun (tdl, first) ifam ->
                let tdl =
                  if first then tdl
                  else [(1, LeftA, TDnothing) :: tdl]
                in
                let des = foi base ifam in
                let td =
                  if Array.length (get_children des) = 0 then
                    (1, LeftA, TDnothing)
                  else
                    let ncol = fam_nb_column 0 (v - 1) des in
                    (2 * ncol - 1, CenterA, TDbar None)
                in
                ([td :: tdl], False))
             (tdl, True) (Array.to_list (get_family p)))
    | _ -> [(1, LeftA, TDnothing) :: tdl] ]
  in
  let spouses_vertical_bar v gen =
    let tdl = List.fold_left (spouses_vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let horizontal_bar_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    match po with
    [ Some (p, _) when Array.length (get_family p) > 0 ->
        fst
          (List.fold_left
             (fun (tdl, first) ifam ->
                let tdl =
                  if first then tdl
                  else [(1, LeftA, TDnothing) :: tdl]
                in
                let des = foi base ifam in
                let tdl =
                  if Array.length (get_children des) = 0 then
                    [(1, LeftA, TDnothing) :: tdl]
                  else if Array.length (get_children des) = 1 then
                    let u = pget conf base (get_children des).(0) in
                    let ncol = nb_column 0 (v - 1) u in
                    [(2 * ncol - 1, CenterA, TDbar None) :: tdl]
                  else
                    let rec loop tdl i =
                      if i = Array.length (get_children des) then tdl
                      else
                        let iper = (get_children des).(i) in
                        let u = pget conf base iper in
                        let tdl =
                          if i > 0 then
                            let align = CenterA in
                            [(1, align, TDhr align) :: tdl]
                          else tdl
                        in
                        let ncol = nb_column 0 (v - 1) u in
                        let align =
                          if i = 0 then RightA
                          else if
                            i = Array.length (get_children des) - 1
                          then
                            LeftA
                          else CenterA
                        in
                        let td = (2 * ncol - 1, align, TDhr align) in
                        loop [td :: tdl] (i + 1)
                    in
                    loop tdl 0
                in
                (tdl, False))
             (tdl, True) (Array.to_list (get_family p)))
    | _ -> [(1, LeftA, TDnothing) :: tdl] ]
  in
  let horizontal_bars v gen =
    let tdl = List.fold_left (horizontal_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let person_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    let td =
      match po with
      [ Some (p, auth) ->
          let ncol =
            if v > 1 then nb_column 0 (v - 1) p
            else Array.length (get_family p)
          in
          let txt = person_title_text conf base p in
          let txt = reference conf base p txt in
          let txt =
            if auth then txt ^ Date.short_dates_text conf base p else txt
          in
          let txt = txt ^ Dag.image_txt conf base p in
          let txt =
            if bd > 0 || td_prop <> "" then
              Printf.sprintf
                "<table style=\"border: %dpx solid\"><tr><td align=\"center\"%s>%s</td>\
                 </tr></table>"
                bd td_prop txt
            else txt
          in
          (2 * ncol - 1, CenterA, TDitem txt)
      | None -> (1, LeftA, TDnothing) ]
    in
    [td :: tdl]
  in
  let spouses_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    match po with
    [ Some (p, auth) when Array.length (get_family p) > 0 ->
        let rec loop tdl i =
          if i = Array.length (get_family p) then tdl
          else
            let ifam = (get_family p).(i) in
            let tdl =
              if i > 0 then [(1, LeftA, TDtext "...") :: tdl] else tdl
            in
            let td =
              let fam = foi base ifam in
              let ncol =
                if v > 1 then fam_nb_column 0 (v - 1) fam
                else 1
              in
              let s =
                let sp = pget conf base (spouse (get_key_index p) fam) in
                let txt = person_title_text conf base sp in
                let txt = reference conf base sp txt in
                let txt =
                  if auth then txt ^ Date.short_dates_text conf base sp
                  else txt
                in
                "&amp;" ^
                  (if auth then
                     Date.short_marriage_date_text conf base fam p sp
                   else "") ^
                  "&nbsp;" ^ txt ^ Dag.image_txt conf base sp
              in
              let s =
                if bd > 0 || td_prop <> "" then
                  Printf.sprintf
                    "<table style=\"border: %dpx solid\"><tr>\
                     <td align=\"center\"%s>%s</td></tr></table>" bd td_prop s
                else s
              in
              (2 * ncol - 1, CenterA, TDitem s)
            in
            loop [td :: tdl] (i + 1)
        in
        loop tdl 0
    | _ -> [(1, LeftA, TDnothing) :: tdl] ]
  in
  let next_gen gen =
    List.fold_right
      (fun po gen ->
         match po with
         [ Some (p, auth) ->
             if Array.length (get_family p) = 0 then [None :: gen]
             else
               List.fold_right
                 (fun ifam gen ->
                    let des = foi base ifam in
                    if Array.length (get_children des) = 0 then [None :: gen]
                    else
                      let age_auth =
                        List.for_all
                          (fun ip ->
                             authorized_age conf base (pget conf base ip))
                          (Array.to_list (get_children des))
                      in
                      List.fold_right
                        (fun iper gen ->
                           let g = (pget conf base iper, age_auth) in
                           [Some g :: gen])
                        (Array.to_list (get_children des)) gen)
                 (Array.to_list (get_family p)) gen
         | None -> [None :: gen] ])
      gen []
  in
  let hts =
    let tdal =
      loop [] [] [Some (p, True)] (gv + 1)
      where rec loop tdal prev_gen gen v =
        let tdal =
          if prev_gen <> [] then
            [children_vertical_bars v gen; horizontal_bars v prev_gen;
             spouses_vertical_bar (v + 1) prev_gen :: tdal]
          else tdal
        in
        let tdal =
          let tdl = List.fold_left (person_txt v) [] gen in
          [Array.of_list (List.rev tdl) :: tdal]
        in
        let tdal =
          let tdl = List.fold_left (spouses_txt v) [] gen in
          [Array.of_list (List.rev tdl) :: tdal]
        in
        if v > 1 then
          loop tdal gen (next_gen gen) (v - 1)
        else tdal
    in
    Array.of_list (List.rev tdal)
  in
  hts
;

value print_tree conf base v p =
  let gv = min (limit_by_tree conf) v in
  let page_title =
    translate_eval
      (transl_a_of_gr_eq_gen_lev conf
         (transl conf "descendants") (person_text_no_html conf base p))
  in
  let hts = make_tree_hts conf base gv p in
  Dag.print_slices_menu_or_dag_page conf base page_title hts ""
;

value print_aboville conf base max_level p =
  let max_level = min (Perso.limit_desc conf) max_level in
  let num_aboville = p_getenv conf.env "num" = Some "on" in
  do {
    Hutil.header conf (descendants_title conf base p);
    print_link_to_welcome conf True;
    Wserver.wprint "%s.<br><p>" (capitale (text_to conf max_level));
    let rec loop_ind lev lab p =
      do {
        if num_aboville then Wserver.wprint "<tt>%s</tt>\n" lab
        else Wserver.wprint "%s\n" lab;
        Wserver.wprint "%s%s\n" (referenced_person_title_text conf base p)
          (Date.short_dates_text conf base p);
        let u = p in
        if lev < max_level then
          for i = 0 to Array.length (get_family u) - 1 do {
            let cpl = foi base (get_family u).(i) in
            let spouse =
              pget conf base (Gutil.spouse (get_key_index p) cpl)
            in
            let mdate =
              if authorized_age conf base p &&
                 authorized_age conf base spouse then
                let fam = foi base (get_family u).(i) in
                match Adef.od_of_codate (get_marriage fam) with
                [ Some (Dgreg d _) ->
                    let date = Date.prec_year_text conf d in
                    "<font size=\"-2\"><em>" ^ date ^ "</em></font>"
                | _ -> "" ]
              else ""
            in
            Wserver.wprint "&amp;%s %s%s\n" mdate
              (referenced_person_title_text conf base spouse)
              (Date.short_dates_text conf base spouse)
          }
        else ();
        Wserver.wprint "<br>\n";
        if lev < max_level then
          let rec loop_fam cnt_chil i =
            if i = Array.length (get_family u) then ()
            else
              let des = foi base (get_family u).(i) in
              let rec loop_chil cnt_chil j =
                if j = Array.length (get_children des) then
                  loop_fam cnt_chil (i + 1)
                else do {
                  loop_ind (lev + 1)
                    (if num_aboville then lab ^ string_of_int cnt_chil ^ "."
                     else lab ^ "<span class=\"descends_aboville_pipe\">&nbsp;</span>")
                    (pget conf base (get_children des).(j));
                  loop_chil (cnt_chil + 1) (j + 1)
                }
              in
              loop_chil cnt_chil 0
          in
          loop_fam 1 0
        else ()
      }
    in
    loop_ind 0 "" p;
    Hutil.trailer conf
  }
;

value desmenu_print = Perso.interp_templ "desmenu";

value print conf base p =
  let templ =
    match p_getenv conf.env "t" with
    [ Some ("F" | "L" | "M") -> "deslist"
    | Some _ -> ""
    | _ -> "desmenu" ]
  in
  if templ <> "" then Perso.interp_templ templ conf base p
  else
    match (p_getenv conf.env "t", p_getint conf.env "v") with
    [ (Some "A", Some v) -> print_aboville conf base v p
    | (Some "S", Some v) -> display_descendants_level conf base v p
    | (Some "H", Some v) -> display_descendant_with_table conf base v p
    | (Some "N", Some v) -> display_descendants_with_numbers conf base v p
    | (Some "G", Some v) -> display_descendant_index conf base v p
    | (Some "C", Some v) -> display_spouse_index conf base v p
    | (Some "T", Some v) -> print_tree conf base v p
    | _ -> desmenu_print conf base p ]
;
