(* camlp5r ./pa_html.cmo *)
(* $Id: some.ml,v 5.44 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

value not_found conf txt x =
  let title _ = Wserver.wprint "%s: \"%s\"" (capitale txt) x in
  do {
    rheader conf title; print_link_to_welcome conf False; trailer conf;
  }
;

value first_name_not_found conf =
  not_found conf (transl conf "first name not found")
;

value surname_not_found conf =
  not_found conf (transl conf "surname not found")
;


(* **********************************************************************)
(*  [Fonc] print_branch_to_alphabetic : conf -> string -> int -> unit   *)
(** [Description] : A partir de l'affichage par branches, permet
                    d'afficher les liens pour un affichage par ordre
                    alphabétique.
    [Args] :
      - conf      : configuration de la base
      - base      : base
      - x         : 'nom/prénom/sosa...' recherché
      - nb_branch : nombre de branches dans le résultat de la recherche
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
value print_branch_to_alphabetic conf x nb_branch = do {
  tag "table" "class=\"display_search\"" begin
    stag "tr" begin
      stag "td" begin
        stag "b" begin
          Wserver.wprint "%s"
            (capitale (transl_nth conf "display by/branch/alphabetic order" 0));
        end;
      end;
      stag "td" begin
        xtag "img" "src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\""
          (Util.image_prefix conf) "picto_branch.png";
      end;
      stag "td" begin
        Wserver.wprint "%s (%d)"
          (transl_nth conf "display by/branch/alphabetic order" 1) nb_branch;
      end;
      stag "td" begin
        xtag "img" "src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\""
          (Util.image_prefix conf) "picto_alphabetic_order.png";
      end;
      (* Ne pas oublier l'attribut nofollow pour les robots *)
      stag "td" begin
        if p_getenv conf.env "t" = Some "A" then
          stag "a" "href=\"%sm=N;o=i;v=%s\" rel=\"nofollow\""
            (commd conf) (code_varenv x ^ ";t=A")
            begin
              Wserver.wprint "%s"
                (transl_nth conf "display by/branch/alphabetic order" 2);
            end
        else
          stag "a" "href=\"%sm=N;o=i;v=%s\" rel=\"nofollow\""
            (commd conf) (code_varenv x ^ ";t=N")
            begin
              Wserver.wprint "%s"
                (transl_nth conf "display by/branch/alphabetic order" 2);
            end;
      end;
    end;
  end;
  xtag "br"
};


(* **********************************************************************)
(*  [Fonc] print_alphabetic_to_branch : conf -> string -> int -> unit   *)
(** [Description] : A partir de l'affichage alphabétique, permet
                    d'afficher les liens pour un affichage par branches.
    [Args] :
      - conf      : configuration de la base
      - base      : base
      - x         : 'nom/prénom/sosa...' recherché
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
value print_alphabetic_to_branch conf x = do {
  stag "table" "class=\"display_search\"" begin
    stag "tr" begin
      stag "td" begin
        stag "b" begin
          Wserver.wprint "%s"
            (capitale (transl_nth conf "display by/branch/alphabetic order" 0));
        end;
      end;
      stag "td" begin
        xtag "img" "src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\""
          (Util.image_prefix conf) "picto_branch.png";
      end;
      (* Ne pas oublier l'attribut nofollow pour les robots *)
      stag "td" begin
        if p_getenv conf.env "t" = Some "A" then
          stag "a" "href=\"%sm=N;v=%s\" rel=\"nofollow\""
            (commd conf) (code_varenv x ^ ";t=A")
            begin
              Wserver.wprint "%s"
                (transl_nth conf "display by/branch/alphabetic order" 1);
            end
        else
          stag "a" "href=\"%sm=NG;sn=%s\" rel=\"nofollow\""
            (commd conf) (code_varenv x)
            begin
              Wserver.wprint "%s"
                (transl_nth conf "display by/branch/alphabetic order" 1);
            end;
      end;
      stag "td" begin
        xtag "img" "src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\""
          (Util.image_prefix conf) "picto_alphabetic_order.png";
      end;
      stag "td" begin
        Wserver.wprint "%s"
          (transl_nth conf "display by/branch/alphabetic order" 2);
      end;
    end;
  end;
  xtag "br"
};

value persons_of_fsname conf base base_strings_of_fsname find proj x =
  (* list of strings index corresponding to the crushed lower first name
     or surname "x" *)
  let istrl = base_strings_of_fsname base x in
  (* selecting the persons who have this first name or surname *)
  let l =
    let x = Name.crush_lower x in
    List.fold_right
      (fun istr l ->
         let str = nominative (sou base istr) in
         if Name.crush_lower str = x ||
            List.mem x (List.map Name.crush_lower (surnames_pieces str))
         then
           let iperl = find istr in
           (* maybe they are not the good ones because of changes in the
              database; checking... *)
           let iperl =
             List.fold_left
               (fun iperl iper ->
                  if eq_istr (proj (pget conf base iper)) istr then
                    [iper :: iperl]
                  else iperl)
               [] iperl
           in
           if iperl = [] then l else [(str, istr, iperl) :: l]
         else l)
      istrl []
  in
  let (l, name_inj) =
    let (l1, name_inj) =
      let x = Name.lower x in
      (List.fold_right
         (fun (str, istr, iperl) l ->
            if x = Name.lower str then [(str, istr, iperl) :: l] else l)
         l [],
       Name.lower)
    in
    let (l1, name_inj) =
      if l1 = [] then
        let x = Name.strip_lower x in
        (List.fold_right
           (fun (str, istr, iperl) l ->
              if x = Name.strip_lower str then [(str, istr, iperl) :: l]
              else l)
           l [],
         Name.strip_lower)
      else (l1, name_inj)
    in
    if l1 = [] then (l, Name.crush_lower) else (l1, name_inj)
  in
  (l, name_inj)
;

value print_elem conf base is_surname (p, xl) =
  list_iter_first
    (fun first x ->
       do {
         if not first then Wserver.wprint "</li>\n<li>\n  " else ();
         Perso.print_sosa conf base x True;
         Wserver.wprint "<a href=\"%s%s\">" (commd conf) (acces conf base x);
         if is_surname then
           Wserver.wprint "%s%s" (surname_end base p) (surname_begin base p)
         else Wserver.wprint "%s" (if p = "" then "?" else p);
         Wserver.wprint "</a>";
         Wserver.wprint "%s" (Date.short_dates_text conf base x);
         stag "em" begin
           specify_homonymous conf base x True;
         end;
       })
    xl
;

value first_char s =
  (* Si la personne n'a pas de prénom/nom, on renvoie '?' *)
  if s = "" then "?"
  else
    if Mutil.utf_8_db.val then
      let len = Name.nbc s.[0] in
      if len < String.length s then String.sub s 0 len
      else s
    else String.sub s (initial s) 1
;

value name_unaccent s =
  copy 0 0 where rec copy i len =
    if i = String.length s then Buff.get len
    else
      let (t, j) = Name.unaccent_utf_8 False s i in
      copy j (Buff.mstore len t)
;

value first_name_print_list conf base x1 xl liste = do {
  let liste =
    let l =
      List.sort
        (fun x1 x2 ->
           match alphabetic (p_surname base x1) (p_surname base x2) with
           [ 0 ->
               match
                 (Adef.od_of_codate (get_birth x1),
                  Adef.od_of_codate (get_birth x2))
               with
               [ (Some d1, Some d2) ->
                   if CheckItem.strictly_after d1 d2 then -1 else 1
               | (Some d1, _) -> 1
               | _ -> -1 ]
           | n -> -n  ])
        liste
    in
    List.fold_left
      (fun l x ->
         let px = p_surname base x in
         match l with
         [ [(p, l1) :: l] when alphabetic px p = 0 -> [(p, [x :: l1]) :: l]
         | _ -> [(px, [x]) :: l] ])
      [] l
  in
  let title h =
    if h || p_getenv conf.env "t" = Some "A" then Wserver.wprint "%s" x1
    else
      Mutil.list_iter_first
        (fun first x ->
           Wserver.wprint "%s<a href=\"%sm=P;v=%s;t=A\">%s</a>"
             (if first then "" else ", ") (commd conf) (code_varenv x) x)
        (StrSet.elements xl)
  in
  header conf title;
  print_link_to_welcome conf True;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  let list =
    List.map
      (fun (sn, ipl) ->
         let txt = Util.surname_end base sn ^ Util.surname_begin base sn in
         let ord = name_unaccent txt in
         (ord, txt, ipl))
      liste
  in
  let list = List.sort compare list in
  print_alphab_list conf (fun (ord, txt, _) -> first_char ord)
    (fun (_, txt, ipl) -> print_elem conf base True (txt, ipl)) list;
  trailer conf;
};

value select_first_name conf base n list =
  let title _ =
    Wserver.wprint "%s \"%s\" : %s"
      (capitale (transl_nth conf "first name/first names" 0)) n
      (transl conf "specify")
  in
  do {
    header conf title;
    Wserver.wprint "<ul>";
    List.iter
      (fun (sstr, (strl, _)) ->
         do {
           Wserver.wprint "\n";
           html_li conf;
           Wserver.wprint "<a href=\"%sm=P;v=%s\">" (commd conf)
             (code_varenv sstr);
           list_iter_first
             (fun first str ->
                Wserver.wprint "%s%s" (if first then "" else ", ") str)
             (StrSet.elements strl);
           Wserver.wprint "</a>\n";
         })
      list;
    Wserver.wprint "</ul>\n";
    trailer conf;
  }
;

value rec merge_insert ((sstr, (strl, iperl)) as x) =
  fun
  [ [((sstr1, (strl1, iperl1)) as y) :: l] ->
      if sstr < sstr1 then [x; y :: l]
      else if sstr > sstr1 then [y :: merge_insert x l]
      else [(sstr, (StrSet.union strl strl1, iperl @ iperl1)) :: l]
  | [] -> [x] ]
;

value persons_of_absolute_first_name conf base x =
  let istrl = base_strings_of_first_name base x in
  List.fold_right
    (fun istr l ->
       let str = sou base istr in
       if str = x then
         let iperl = spi_find (persons_of_first_name base) istr in
         let iperl =
           List.fold_left
             (fun iperl iper ->
                if eq_istr (get_first_name (pget conf base iper)) istr then
                  [iper :: iperl]
                else iperl)
             [] iperl
         in
         if iperl = [] then l else [(str, istr, iperl) :: l]
       else l)
    istrl []
;

value first_name_print conf base x =
  let (list, _) =
    if Mutil.utf_8_db.val && p_getenv conf.env "t" = Some "A" then
      (persons_of_absolute_first_name conf base x, fun [])
    else if x = "" then ([], fun [])
    else
      persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, istr, iperl) ->
         (Name.lower str, (StrSet.add str StrSet.empty, iperl)))
      list
  in
  let list = List.fold_right merge_insert list [] in
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  match list with
  [ [] -> first_name_not_found conf x
  | [(_, (strl, iperl))] ->
      let iperl = list_uniq (List.sort compare iperl) in
      let pl = List.map (pget conf base) iperl in
      let pl =
        List.fold_right
          (fun p pl ->
            if not (is_hide_names conf p) || (fast_auth_age conf base p )
            then [p :: pl]
            else pl)
          pl []
      in
      first_name_print_list conf base x strl pl
  | _ -> select_first_name conf base x list ]
;

value has_children_with_that_name conf base des name =
  let compare_name n1 n2 =
    if p_getenv conf.env "t" = Some "A" then n1 = n2
    else Name.lower n1 = Name.lower n2
  in
  List.exists
    (fun ip -> compare_name (p_surname base (pget conf base ip)) name)
    (Array.to_list (get_children des))
;

(* List selection bullets *)

value bullet_sel_txt = "<tt>o</tt>";
value bullet_unsel_txt = "<tt>+</tt>";
value bullet_nosel_txt = "<tt>o</tt>";
value print_selection_bullet conf =
  fun
  [ Some (txt, sel) -> do {
      let req =
        List.fold_left
          (fun req (k, v) ->
             if not sel && k = "u" && v = txt then req
             else
               let s = k ^ "=" ^ v in
               if req = "" then s else req ^ ";" ^ s)
          "" conf.env
      in
      if conf.cancel_links then ()
      else
        Wserver.wprint "<a id=\"i%s\" href=\"%s%s%s%s\" rel=\"nofollow\">" txt (commd conf) req
          (if sel then ";u=" ^ txt else "")
          (if sel || List.mem_assoc "u" conf.env then "#i" ^ txt else "");
      Wserver.wprint "%s" (if sel then bullet_sel_txt else bullet_unsel_txt);
      if conf.cancel_links then () else Wserver.wprint "</a>";
      Wserver.wprint "\n";
    }
  | None -> Wserver.wprint "%s\n" bullet_nosel_txt ]
;

value unselected_bullets conf =
  List.fold_left
    (fun sl (k, v) ->
       try
         if k = "u" then [int_of_string v :: sl]
         else sl
       with
       [ Failure _ -> sl ])
    [] conf.env
;

value alphabetic1 n1 n2 =
  if utf_8_db.val then Gutil.alphabetic_utf_8 n1 n2
  else Gutil.alphabetic n1 n2
;

type branch_head 'a = { bh_ancestor : 'a; bh_well_named_ancestors : list 'a };

value print_branch conf base psn name =
  let unsel_list = unselected_bullets conf in
  loop where rec loop is_first_level p = do {
    let u = pget conf base (get_key_index p) in
    let family_list =
      List.map
        (fun ifam ->
           let fam = foi base ifam in
           let c = spouse (get_key_index p) fam in
           let c = pget conf base c in
           let down = has_children_with_that_name conf base fam name in
           let down =
             if get_sex p = Female && p_surname base c = name then False
             else down
           in
           let i = Adef.int_of_ifam ifam in
           let sel = not (List.mem i unsel_list) in
           (fam, c, if down then Some (string_of_int i, sel) else None))
        (Array.to_list (get_family u))
    in
    let first_select =
      match family_list with
      [ [(_, _, select) :: _] -> select
      | _ -> None ]
    in
    print_selection_bullet conf first_select;
    Perso.print_sosa conf base p True;
    stag "strong" begin
      Wserver.wprint "%s"
        (Util.reference conf base p
           (if (is_hide_names conf p) && not (fast_auth_age conf base p) then "x"
            else if not psn && p_surname base p = name then
              person_text_without_surname conf base p
            else person_text conf base p));
    end;
    Wserver.wprint "%s" (Date.short_dates_text conf base p);
    Wserver.wprint "\n";
    if Array.length (get_family u) = 0 then ()
    else
      let _ =
        List.fold_left
          (fun first (fam, c, select) ->
             do {
               if not first then do {
                 if is_first_level then Wserver.wprint "<br%s>\n" conf.xhs
                 else Wserver.wprint "</dd>\n<dd>\n";
                 print_selection_bullet conf select;
                 Perso.print_sosa conf base p False;
                 stag "em" begin
                   Wserver.wprint "%s"
                     (if (is_hide_names conf p) && not (fast_auth_age conf base p)
                     then "x"
                     else if not psn && p_surname base p = name then
                       person_text_without_surname conf base p
                     else person_text conf base p);
                 end;
                 Wserver.wprint "%s" (Date.short_dates_text conf base p);
                 Wserver.wprint "\n";
               }
               else ();
               Wserver.wprint "  &amp;";
               Wserver.wprint "%s\n"
                 (Date.short_marriage_date_text conf base fam p c);
               Perso.print_sosa conf base c True;
               stag "strong" begin
                 Wserver.wprint "%s"
                   (reference conf base c
                      (if (is_hide_names conf c) && not (fast_auth_age conf base c)
		                   then "x"
                       else person_text conf base c));
               end;
               Wserver.wprint "%s" (Date.short_dates_text conf base c);
               Wserver.wprint "\n";
               let children = get_children fam in
               match select with
               [ Some (_, True) ->
                   tag "dl" begin
                     List.iter
                       (fun e ->
                          tag "dd" begin loop False (pget conf base e); end)
                       (Array.to_list children);
                   end
               | Some (_, False) -> ()
               | None ->
                   if Array.length children <> 0 then
                     stagn "dl" begin
                       stag "dd" begin Wserver.wprint "..."; end;
                     end
                   else () ];
               False
             })
          True family_list
      in
      ();
  }
;

value print_one_branch conf base bh psn lev =
  let p = bh.bh_ancestor in
  match bh.bh_well_named_ancestors with
  [ [] ->
      let x = sou base (get_surname p) in
      print_branch conf base psn x lev p
  | pl -> do {
      if is_hidden p then Wserver.wprint "&lt;&lt;"
      else
        let href = Util.acces conf base p in
        wprint_geneweb_link conf href "&lt;&lt;";
      Wserver.wprint "\n";
      List.iter
        (fun p ->
           let x = sou base (get_surname p) in
           tag "dl" begin
             tag "dd" begin
               print_branch conf base psn x False p;
             end;
           end)
        bh.bh_well_named_ancestors;
    } ]
;


value print_one_surname_by_branch conf base x xl (bhl, str) = do {
  let ancestors =
    match p_getenv conf.env "order" with
    [ Some "d" ->
        let born_before p1 p2 =
          match (Adef.od_of_codate (get_birth p1),
                 Adef.od_of_codate (get_birth p2)) with
          [ (Some d1, Some d2) ->
              if CheckItem.strictly_after d2 d1 then -1 else 1
          | (_, None) -> -1
          | (None, _) -> 1 ]
        in
        List.sort (fun p1 p2 -> born_before p1.bh_ancestor p2.bh_ancestor) bhl
    | _ ->
        List.sort
          (fun p1 p2 ->
             alphabetic1 (p_first_name base p1.bh_ancestor)
               (p_first_name base p2.bh_ancestor))
          bhl ]
  in
  let len = List.length ancestors in
  let psn =
    match p_getenv conf.env "alwsurn" with
    [ Some x -> x = "yes"
    | None ->
        try List.assoc "always_surname" conf.base_env = "yes" with
        [ Not_found -> False ] ]
  in
  let title h =
    if h || p_getenv conf.env "t" = Some "A" then Wserver.wprint "%s" x
    else
      Mutil.list_iter_first
        (fun first x ->
           Wserver.wprint "%s<a href=\"%sm=N;v=%s;t=A\">%s</a>"
             (if first then "" else ", ") (commd conf) (code_varenv x) x)
        (StrSet.elements xl)
  in
  let br = p_getint conf.env "br" in
  Wserver.wrap_string.val := Util.xml_pretty_print;
  header conf title;
  print_link_to_welcome conf True;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  (* Menu afficher par branche/ordre alphabetique *)
  if br = None then print_branch_to_alphabetic conf x len
  else ();
  tag "div" "style=\"white-space:nowrap\"" begin
    if len > 1 && br = None then
      tag "dl" begin
        let _ =
          List.fold_left
            (fun n bh -> do {
               stagn "dt" begin
                 if conf.cancel_links then Wserver.wprint "%d." n
                 else
                   stag "a" "href=\"%sm=N;v=%s;br=%d\" rel=\"nofollow\"" (commd conf)
                       (Util.code_varenv str) n begin
                     Wserver.wprint "%d." n;
                   end;
               end;
               tag "dd" begin
                 print_one_branch conf base bh psn False;
               end;
               n + 1
             })
            1 ancestors
        in
        ();
      end
    else
      let _ =
        List.fold_left
          (fun n bh -> do {
             if br = None || br = Some n then
               print_one_branch conf base bh psn True
             else ();
             n + 1
           })
          1 ancestors
      in
      ();
  end;
  trailer conf;
};

value print_several_possible_surnames x conf base (bhl, homonymes) = do {
  let fx = x in
  let x =
    match homonymes with
    [ [x :: _] -> x
    | _ -> x ]
  in
  let title h =
    Wserver.wprint "%s \"%s\" : %s"
      (capitale (transl_nth conf "surname/surnames" 0)) fx
      (transl conf "specify")
  in
  header conf title;
  print_link_to_welcome conf True;
  let list =
    List.map
      (fun sn ->
         let txt = Util.surname_end base sn ^ Util.surname_begin base sn in
         let ord = name_unaccent txt in
         (ord, txt, sn))
      homonymes
  in
  let list = List.sort compare list in
  let access txt sn =
    geneweb_link conf ("m=N;v=" ^ code_varenv sn ^ ";t=N") txt
  in
  Util.wprint_in_columns conf
    (fun (ord, _, _) -> ord)
    (fun (_, txt, sn) -> Wserver.wprint "%s" (access txt sn)) list;
  tag "p" begin
    Wserver.wprint "<em style=\"font-size:80%%\">\n";
    Wserver.wprint "%s " (capitale (transl conf "click"));
    Wserver.wprint "<a href=\"%sm=N;o=i;v=%s\">%s</a>\n" (commd conf)
      (if List.length homonymes = 1 then code_varenv x ^ ";t=A"
       else code_varenv fx)
      (transl conf "here");
    Wserver.wprint "%s"
      (transl conf "for the first names by alphabetic order");
    Wserver.wprint ".</em>\n";
  end;
  trailer conf;
};

value print_family_alphabetic x conf base liste =
  let homonymes =
    let list =
      List.fold_left
        (fun list p ->
           if List.exists (eq_istr (get_surname p)) list then list
           else [get_surname p :: list])
        [] liste
    in
    let set =
      List.fold_left (fun set istr -> StrSet.add (sou base istr) set)
        StrSet.empty list
    in
    List.sort compare (StrSet.elements set)
  in
  let liste =
    let l =
      List.sort
        (fun x1 x2 ->
           match
             alphabetic1 (p_first_name base x2) (p_first_name base x1)
           with
           [ 0 -> compare (get_occ x1) (get_occ x2)
           | n -> n ])
        liste
    in
    List.fold_left
      (fun l x ->
         let px = p_first_name base x in
         match l with
         [ [(p, l1) :: l] when alphabetic1 px p = 0 -> [(p, [x :: l1]) :: l]
         | _ -> [(px, [x]) :: l] ])
      [] l
  in
  match liste with
  [ [] -> surname_not_found conf x
  | _ ->
      let title h =
        let access x =
          if h || List.length homonymes = 1 then x
          else geneweb_link conf ("m=N;o=i;v=" ^ code_varenv x ^ ";t=A") x
        in
        list_iter_first
          (fun first x ->
             Wserver.wprint "%s%s" (if first then "" else ", ") (access x))
          homonymes
      in
      do {
        header conf title;
        print_link_to_welcome conf True;
        (* Si on est dans un calcul de parenté, on affiche *)
        (* l'aide sur la sélection d'un individu.          *)
        Util.print_tips_relationship conf;
        (* Menu afficher par branche/ordre alphabetique *)
        print_alphabetic_to_branch conf x;
        print_alphab_list conf (fun (p, _) -> first_char p)
          (print_elem conf base False) liste;
        trailer conf;
      } ]
;

value insert_at_position_in_family children ip ipl =
  loop (Array.to_list children) ipl where rec loop child_list ipl =
    match (child_list, ipl) with
    [ ([ip1 :: ipl1], [ip2 :: ipl2]) ->
        if ip1 = ip2 then
          if ip = ip1 then ipl else [ip2 :: loop ipl1 ipl2]
        else
          if ip = ip1 then [ip1 :: ipl] else loop ipl1 ipl
    | ([_ :: _], []) -> [ip]
    | ([], _) -> assert False ]
;

value select_ancestors conf base name_inj ipl =
  let str_inj s = name_inj (sou base s) in
  List.fold_left
    (fun bhl ip ->
       let p = pget conf base ip in
       match get_parents p with
       [ Some ifam ->
           let fam = foi base ifam in
           let ifath = get_father fam in
           let imoth = get_mother fam in
           let fath = pget conf base ifath in
           let moth = pget conf base imoth in
           let s = str_inj (get_surname p) in
           if str_inj (get_surname fath) <> s &&
              str_inj (get_surname moth) <> s
           then
             loop bhl where rec loop =
               fun
               [ [bh :: bhl] ->
                   if bh.bh_ancestor = ifath || bh.bh_ancestor = imoth then
                     let bh =
                       {(bh) with
                        bh_well_named_ancestors =
                          insert_at_position_in_family (get_children fam)
                            ip bh.bh_well_named_ancestors}
                     in
                     [bh :: bhl]
                   else
                     [bh :: loop bhl]
               | [] ->
                   [{bh_ancestor = ifath; bh_well_named_ancestors = [ip]}] ]
           else bhl
       | _ ->
           let bh =
             {bh_ancestor = ip; bh_well_named_ancestors = []}
           in
           [bh :: bhl] ])
    [] ipl
;

value persons_of_absolute_surname conf base x =
  let istrl = base_strings_of_surname base x in
  List.fold_right
    (fun istr l ->
       let str = sou base istr in
       if str = x then
         let iperl = spi_find (persons_of_surname base) istr in
         let iperl =
           List.fold_left
             (fun iperl iper ->
                if eq_istr (get_surname (pget conf base iper)) istr then
                  [iper :: iperl]
                else iperl)
             [] iperl
         in
         if iperl = [] then l else [(str, istr, iperl) :: l]
       else l)
    istrl []
;

module PerSet = Set.Make (struct type t = iper; value compare = compare; end);

value surname_print conf base not_found_fun x =
  let (list, name_inj) =
    if Mutil.utf_8_db.val && p_getenv conf.env "t" = Some "A" then
      (persons_of_absolute_surname conf base x, fun x -> x)
    else if x = "" then ([], fun [])
    else
      persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname x
  in
  let list =
    List.map
      (fun (str, istr, iperl) ->
         (Name.lower str, (StrSet.add str StrSet.empty, iperl)))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let (iperl, strl) =
    List.fold_right
      (fun (str, (istr, iperl1)) (iperl, strl) ->
         let len = List.length iperl1 in
         let strl =
           try
             let len1 = List.assoc str strl in
             [(str, len + len1) :: List.remove_assoc str strl]
           with
           [ Not_found -> [(str, len) :: strl] ]
         in
         (List.fold_right PerSet.add iperl1 iperl, strl))
      list (PerSet.empty, [])
  in
  let iperl = PerSet.elements iperl in
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  match p_getenv conf.env "o" with
  [ Some "i" ->
      let pl =
        List.fold_right (fun ip ipl -> [pget conf base ip :: ipl]) iperl []
      in
      let pl =
        List.fold_right
          (fun p pl ->
	          if not (is_hide_names conf p) || (fast_auth_age conf base p)
	          then [p :: pl]
	          else pl)
          pl []
      in
      print_family_alphabetic x conf base pl
  | _ ->
      let bhl = select_ancestors conf base name_inj iperl in
      let bhl =
        List.map
          (fun bh ->
            {bh_ancestor = pget conf base bh.bh_ancestor;
             bh_well_named_ancestors =
             List.map (pget conf base) bh.bh_well_named_ancestors})
          bhl
      in
      match (bhl, list) with
      [ ([], _) -> not_found_fun conf x
      | (_, [(s, (strl, iperl))]) ->
          print_one_surname_by_branch conf base x strl (bhl, s)
      | _ ->
          let strl = List.map (fun (s, (strl, _)) -> s) list in
          print_several_possible_surnames x conf base (bhl, strl) ]]
;


(**/**)
(* TODO: refactoring avec les fonctions ci-dessus !!! *)


value search_surname conf base x =
  let (list, name_inj) =
    if Mutil.utf_8_db.val && p_getenv conf.env "t" = Some "A" then
      (persons_of_absolute_surname conf base x, fun x -> x)
    else if x = "" then ([], fun [])
    else
      persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname x
  in
  let list =
    List.map
      (fun (str, istr, iperl) ->
         (Name.lower str, (StrSet.add str StrSet.empty, iperl)))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let (iperl, strl) =
    List.fold_right
      (fun (str, (istr, iperl1)) (iperl, strl) ->
         let len = List.length iperl1 in
         let strl =
           try
             let len1 = List.assoc str strl in
             [(str, len + len1) :: List.remove_assoc str strl]
           with
           [ Not_found -> [(str, len) :: strl] ]
         in
         (List.fold_right PerSet.add iperl1 iperl, strl))
      list (PerSet.empty, [])
  in
  let iperl = PerSet.elements iperl in
  let bhl = select_ancestors conf base name_inj iperl in
  let bhl =
    List.map
      (fun bh ->
        {bh_ancestor = pget conf base bh.bh_ancestor;
         bh_well_named_ancestors =
         List.map (pget conf base) bh.bh_well_named_ancestors})
      bhl
  in
  match (bhl, list) with
  [ ([], _) -> []
  | (_, [(s, (strl, iperl))]) -> iperl
  | _ -> [] ]
;

value search_surname_print conf base not_found_fun x =
  let (list, name_inj) =
    if Mutil.utf_8_db.val && p_getenv conf.env "t" = Some "A" then
      (persons_of_absolute_surname conf base x, fun x -> x)
    else if x = "" then ([], fun [])
    else
      persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname x
  in
  let list =
    List.map
      (fun (str, istr, iperl) ->
         (Name.lower str, (StrSet.add str StrSet.empty, iperl)))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let (iperl, strl) =
    List.fold_right
      (fun (str, (istr, iperl1)) (iperl, strl) ->
         let len = List.length iperl1 in
         let strl =
           try
             let len1 = List.assoc str strl in
             [(str, len + len1) :: List.remove_assoc str strl]
           with
           [ Not_found -> [(str, len) :: strl] ]
         in
         (List.fold_right PerSet.add iperl1 iperl, strl))
      list (PerSet.empty, [])
  in
  let iperl = PerSet.elements iperl in
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  match p_getenv conf.env "o" with
  [ Some "i" ->
      let pl =
        List.fold_right (fun ip ipl -> [pget conf base ip :: ipl]) iperl []
      in
      let pl =
        List.fold_right
          (fun p pl ->
	          if not (is_hide_names conf p) || (fast_auth_age conf base p)
	          then [p :: pl]
	          else pl)
          pl []
      in
      print_family_alphabetic x conf base pl
  | _ ->
      let bhl = select_ancestors conf base name_inj iperl in
      let bhl =
        List.map
          (fun bh ->
            {bh_ancestor = pget conf base bh.bh_ancestor;
             bh_well_named_ancestors =
             List.map (pget conf base) bh.bh_well_named_ancestors})
          bhl
      in
      match (bhl, list) with
      [ ([], _) -> not_found_fun conf x
      | (_, [(s, (strl, iperl))]) ->
          print_one_surname_by_branch conf base x strl (bhl, s)
      | _ ->
          let strl = List.map (fun (s, (strl, _)) -> s) list in
          print_several_possible_surnames x conf base (bhl, strl) ]]
;

value search_first_name conf base x =
  let (list, _) =
    if Mutil.utf_8_db.val && p_getenv conf.env "t" = Some "A" then
      (persons_of_absolute_first_name conf base x, fun [])
    else if x = "" then ([], fun [])
    else
      persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, istr, iperl) ->
         (Name.lower str, (StrSet.add str StrSet.empty, iperl)))
      list
  in
  List.fold_right merge_insert list []
;

value search_first_name_print conf base x =
  let (list, _) =
    if Mutil.utf_8_db.val && p_getenv conf.env "t" = Some "A" then
      (persons_of_absolute_first_name conf base x, fun [])
    else if x = "" then ([], fun [])
    else
      persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, istr, iperl) ->
         (Name.lower str, (StrSet.add str StrSet.empty, iperl)))
      list
  in
  let list = List.fold_right merge_insert list [] in
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  match list with
  [ [] -> first_name_not_found conf x
  | [(_, (strl, iperl))] ->
      let iperl = list_uniq (List.sort compare iperl) in
      let pl = List.map (pget conf base) iperl in
      let pl =
        List.fold_right
          (fun p pl ->
            if not (is_hide_names conf p) || (fast_auth_age conf base p )
            then [p :: pl]
            else pl)
          pl []
      in
      first_name_print_list conf base x strl pl
  | _ -> select_first_name conf base x list ]
;
