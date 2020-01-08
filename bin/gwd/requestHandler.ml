open Geneweb
open Config
open Def
open Gwdb
open Util
open Jingoo
open Jg_types

(*** Utils ***)

let person_is_std_key conf base p k =
  let k = Name.strip_lower k in
  if k = Name.strip_lower (p_first_name base p ^ " " ^ p_surname base p) then
    true
  else if
    List.exists (fun n -> Name.strip n = k)
      (person_misc_names base p (nobtit conf base))
  then
    true
  else false

let select_std_eq conf base pl k =
  List.fold_right
    (fun p pl -> if person_is_std_key conf base p k then p :: pl else pl) pl
    []

let name_with_roman_number str =
  let rec loop found len i =
    if i = String.length str then if found then Some (Buff.get len) else None
    else
      match str.[i] with
      |  '0'..'9' as c ->
        let (n, i) =
          let rec loop n i =
            if i = String.length str then n, i
            else
              match str.[i] with
              '0'..'9' as c ->
                loop (10 * n + Char.code c - Char.code '0') (i + 1)
              | _ -> n, i
          in
          loop (Char.code c - Char.code '0') (i + 1)
        in
        loop true (Buff.mstore len (Mutil.roman_of_arabian n)) i
      | c -> loop found (Buff.store len c) (i + 1)
  in
  loop false 0 0

let compact_list base xl =
  let pl = Gutil.sort_person_list base xl in
  List.fold_right
    (fun p pl ->
       match pl with
         p1 :: _ when get_iper p = get_iper p1 -> pl
       | _ -> p :: pl)
    pl []

let cut_words str =
  let rec loop beg i =
    if i < String.length str then
      match str.[i] with
        ' ' ->
        if beg = i then loop (succ beg) (succ i)
        else String.sub str beg (i - beg) :: loop (succ i) (succ i)
      | _ -> loop beg (succ i)
    else if beg = i then []
    else [String.sub str beg (i - beg)]
  in
  loop 0 0

let try_find_with_one_first_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index_opt n1 ' ' with
    Some i ->
    let fn = String.sub n1 0 i in
    let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
    let (list, _) =
      Some.persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname sn
    in
    List.fold_left
      (fun pl (_, _, ipl) ->
         List.fold_left
           (fun pl ip ->
              let p = pget conf base ip in
              let fn1 =
                Name.abbrev (Name.lower (sou base (get_first_name p)))
              in
              if List.mem fn (cut_words fn1) then p :: pl else pl)
           pl ipl)
      [] list
  | None -> []

let find_all conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match sosa_ref, sosa_nb with
    Some p, Some n ->
    if n <> Sosa.zero then
      match Util.branch_of_sosa conf base n p with
        Some (p :: _) -> [p], true
      | _ -> [], false
    else [], false
  | _ ->
    match Gutil.person_of_string_key base an with
      Some ip ->
      let pl =
        let p = pget conf base ip in if is_hidden p then [] else [p]
      in
      let pl =
        if not conf.wizard && not conf.friend then
          List.fold_right
            (fun p pl ->
               if not (is_hide_names conf p) ||
                  Util.authorized_age conf base p
               then
                 p :: pl
               else pl)
            pl []
        else pl
      in
      pl, false
    | None ->
      let ipl = Gutil.person_not_a_key_find_all base an in
      let (an, ipl) =
        if ipl = [] then
          match name_with_roman_number an with
            Some an1 ->
            let ipl = Gutil.person_ht_find_all base an1 in
            if ipl = [] then an, [] else an1, ipl
          | None -> an, ipl
        else an, ipl
      in
      let pl =
        List.fold_left
          (fun l ip ->
             let p = pget conf base ip in
             if is_hidden p then l else p :: l)
          [] ipl
      in
      let spl = select_std_eq conf base pl an in
      let pl =
        if spl = [] then
          if pl = [] then try_find_with_one_first_name conf base an
          else pl
        else spl
      in
      let pl =
        if not conf.wizard && not conf.friend then
          List.fold_right
            (fun p pl ->
               if not (is_hide_names conf p) ||
                  Util.authorized_age conf base p
               then
                 p :: pl
               else pl)
            pl []
        else pl
      in
      compact_list base pl, false

let relation_print conf base p =
  let p1 =
    match p_getenv conf.senv "ei" with
    | Some i ->
      conf.senv <- [];
      (* if i >= 0 && i < nb_of_persons base then *)
        Some (pget conf base (iper_of_string i))
      (* else None *)
    | None ->
      match find_person_in_env conf base "1" with
        Some p1 -> conf.senv <- []; Some p1
      | None -> None
  in
  RelationDisplay.print conf base p p1

let specify conf base n pl =
  let title _ = Wserver.printf "%s : %s" n (transl conf "specify") in
  let n = Name.crush_lower n in
  let ptll =
    List.map
      (fun p ->
         let tl = ref [] in
         let add_tl t =
           tl :=
             let rec add_rec =
               function
                 t1 :: tl1 ->
                 if eq_istr t1.t_ident t.t_ident &&
                    eq_istr t1.t_place t.t_place
                 then
                   t1 :: tl1
                 else t1 :: add_rec tl1
               | [] -> [t]
             in
             add_rec !tl
         in
         let compare_and_add t pn =
           let pn = sou base pn in
           if Name.crush_lower pn = n then add_tl t
           else
             match get_qualifiers p with
               nn :: _ ->
               let nn = sou base nn in
               if Name.crush_lower (pn ^ " " ^ nn) = n then add_tl t
             | _ -> ()
         in
         List.iter
           (fun t ->
              match t.t_name, get_public_name p with
                Tname s, _ -> compare_and_add t s
              | _, pn when sou base pn <> "" -> compare_and_add t pn
              | _ -> ())
           (nobtit conf base p);
         p, !tl)
      pl
  in
  Hutil.header conf title;
  conf.cancel_links <- false;
  Hutil.print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  Wserver.printf "<ul>\n";
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  List.iter
    (fun (p, tl) ->
       Wserver.printf "<li>\n";
       Perso.print_sosa conf base p true;
       begin match tl with
           [] ->
           Wserver.printf "\n%s" (referenced_person_title_text conf base p)
         | t :: _ ->
           Wserver.printf "<a href=\"%s%s\">\n" (commd conf)
             (acces conf base p);
           Wserver.printf "%s" (titled_person_text conf base p t);
           Wserver.printf "</a>\n";
           List.iter
             (fun t -> Wserver.printf "%s" (one_title_text base t)) tl
       end;
       Wserver.printf "%s" (DateDisplay.short_dates_text conf base p);
       if authorized_age conf base p then
         begin match get_first_names_aliases p with
             [] -> ()
           | fnal ->
             Wserver.printf "\n<em>(";
             Mutil.list_iter_first
               (fun first fna ->
                  if not first then Wserver.printf ", ";
                  Wserver.printf "%s" (sou base fna))
               fnal;
             Wserver.printf ")</em>"
         end;
       begin let spouses =
               Array.fold_right
                 (fun ifam spouses ->
                    let cpl = foi base ifam in
                    let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
                    if p_surname base spouse <> "?" then spouse :: spouses
                    else spouses)
                 (get_family p) []
         in
         match spouses with
           [] -> ()
         | h :: hl ->
           let s =
             List.fold_left
               (fun s h -> s ^ ",\n" ^ person_title_text conf base h)
               (person_title_text conf base h) hl
           in
           Wserver.printf ", <em>&amp; %s</em>\n" s
       end;
       Wserver.printf "</li>\n")
    ptll;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

type handler_base = (handler -> config -> base -> unit)

and handler_nobase = (handler -> config -> unit)

and handler =
  { unknown : config -> string -> unit
  ; very_unknown : handler_base
  ; incorrect_request : handler_base
  ; a : handler_base
  ; add_fam : handler_base
  ; add_fam_ok : handler_base
  ; add_ind : handler_base
  ; add_ind_ok : handler_base
  ; add_par : handler_base
  ; add_par_ok : handler_base
  ; anm : handler_base
  ; an : handler_base
  ; ad : handler_base
  ; am : handler_base
  ; as_ok : handler_base
  ; b : handler_base
  ; c : handler_base
  ; cal : handler_base
  ; chg_chn : handler_base
  ; chg_chn_ok : handler_base
  ; chg_evt_ind_ord : handler_base
  ; chg_evt_ind_ord_ok : handler_base
  ; chg_evt_fam_ord : handler_base
  ; chg_evt_fam_ord_ok : handler_base
  ; chg_fam_ord : handler_base
  ; chg_fam_ord_ok : handler_base
  ; conn_wiz : handler_base
  ; d : handler_base
  ; dag : handler_base
  ; del_fam : handler_base
  ; del_fam_ok : handler_base
  ; del_image : handler_base
  ; del_image_ok : handler_base
  ; del_ind : handler_base
  ; del_ind_ok : handler_base
  ; f : handler_base
  ; forum : handler_base
  ; forum_add : handler_base
  ; forum_add_ok : handler_base
  ; forum_del : handler_base
  ; forum_p_p : handler_base
  ; forum_search : handler_base
  ; forum_val : handler_base
  ; forum_view : handler_base
  ; h : handler_base
  ; hist : handler_base
  ; hist_clean : handler_base
  ; hist_clean_ok : handler_base
  ; hist_diff : handler_base
  ; hist_search : handler_base
  ; imh : handler_base
  ; inv_fam : handler_base
  ; inv_fam_ok : handler_base
  ; itree : handler_base
  ; kill_anc : handler_base
  ; lb : handler_base
  ; ld : handler_base
  ; linked : handler_base
  ; list_ind : handler_base
  ; ll : handler_base
  ; lm : handler_base
  ; misc_notes : handler_base
  ; misc_notes_search : handler_base
  ; mod_data : handler_base
  ; mod_data_ok : handler_base
  ; mod_fam : handler_base
  ; mod_fam_ok : handler_base
  ; mod_ind : handler_base
  ; mod_ind_ok : handler_base
  ; mod_notes : handler_base
  ; mod_notes_ok : handler_base
  ; mod_wiznotes : handler_base
  ; mod_wiznotes_ok : handler_base
  ; mrg : handler_base
  ; mrg_dup : handler_base
  ; mrg_dup_ind_y_n : handler_base
  ; mrg_dup_fam_y_n : handler_base
  ; mrg_fam : handler_base
  ; mrg_fam_ok : handler_base
  ; mrg_mod_fam_ok : handler_base
  ; mrg_ind : handler_base
  ; mrg_ind_ok : handler_base
  ; mrg_mod_ind_ok : handler_base
  ; n : handler_base
  ; ng : handler_base
  ; notes : handler_base
  ; oa : handler_base
  ; oe : handler_base
  ; p : handler_base
  ; pop_pyr : handler_base
  ; ps : handler_base
  ; r : handler_base
  ; request : handler_base
  ; rl : handler_base
  ; rlm : handler_base
  ; s : handler_base
  ; snd_image : handler_base
  ; snd_image_ok : handler_base
  ; src : handler_base
  ; stat : handler_base
  ; change_wiz_vis : handler_base
  ; tt : handler_base
  ; u : handler_base
  ; view_wiznotes : handler_base
  ; warnings : handler_base
  ; wiznotes : handler_base
  ; wiznotes_search : handler_base
  ; _no_mode : handler_base
#ifdef API
  ; api_all_persons : handler_base
  ; api_all_families : handler_base
  ; api_base_warnings : handler_base
  ; api_close_persons : handler_base
  ; api_cpl_rel : handler_base
  ; api_graph_asc : handler_base
  ; api_graph_asc_lia : handler_base
  ; api_graph_desc : handler_base
  ; api_graph_rel : handler_base
  ; api_first_available_person : handler_base
  ; api_find_sosa : handler_base
  ; api_info_base : handler_base
  ; api_info_ind : handler_base
  ; api_image : handler_base
  ; api_image_ext : handler_base
  ; api_image_all : handler_base
  ; api_image_person : handler_base
  ; api_image_update : handler_base
  ; api_last_modified_persons : handler_base
  ; api_last_visited_persons : handler_base
  ; api_list_persons : handler_base
  ; api_loop_base : handler_base
  ; api_max_ancestors : handler_base
  ; api_nb_ancestors : handler_base
  ; api_notification_birthday : handler_base
  ; api_ref_person_from_id : handler_base
  ; api_remove_image_ext : handler_base
  ; api_remove_image_ext_all : handler_base
  ; api_search : handler_base
  ; api_graph_tree_v2 : handler_base
  ; api_person_tree : handler_base
  ; api_fiche_person : handler_base
  ; api_auto_complete : handler_base
  ; api_get_config : handler_base
  ; api_person_search_list : handler_base
  ; api_get_person_search_info : handler_base
  ; api_add_child : handler_base
  ; api_add_child_ok : handler_base
  ; api_add_family : handler_base
  ; api_add_family_ok : handler_base
  ; api_add_first_fam_ok : handler_base
  ; api_add_parents : handler_base
  ; api_add_parents_ok : handler_base
  ; api_add_person_ok : handler_base
  ; api_add_person_start_ok : handler_base
  ; api_add_sibling : handler_base
  ; api_add_sibling_ok : handler_base
  ; api_edit_family_request : handler_base
  ; api_edit_family : handler_base
  ; api_edit_family_ok : handler_base
  ; api_edit_person : handler_base
  ; api_edit_person_ok : handler_base
  ; api_del_family_ok : handler_base
  ; api_del_person_ok : handler_base
  ; api_link_tree : handler_base
  ; api_stats : handler_base
  ; api_add_first_fam : handler_nobase
  ; api_select_events : handler_base
#endif
  ; fallback : string -> handler_base
  }

(*** Handlers ***)

let dummyHandler =
  let dummy_base = fun _ _ _ -> assert false in
#ifdef API
  let dummy_nobase = fun _ _ -> assert false in
#endif
  { unknown = begin fun _ _ -> assert false end
  ; very_unknown = dummy_base
  ; incorrect_request = dummy_base
  ; a = dummy_base
  ; add_fam = dummy_base
  ; add_fam_ok = dummy_base
  ; add_ind = dummy_base
  ; add_ind_ok = dummy_base
  ; add_par = dummy_base
  ; add_par_ok = dummy_base
  ; anm = dummy_base
  ; an = dummy_base
  ; ad = dummy_base
  ; am = dummy_base
  ; as_ok = dummy_base
  ; b = dummy_base
  ; c = dummy_base
  ; cal = dummy_base
  ; chg_chn = dummy_base
  ; chg_chn_ok = dummy_base
  ; chg_evt_ind_ord = dummy_base
  ; chg_evt_ind_ord_ok = dummy_base
  ; chg_evt_fam_ord = dummy_base
  ; chg_evt_fam_ord_ok = dummy_base
  ; chg_fam_ord = dummy_base
  ; chg_fam_ord_ok = dummy_base
  ; conn_wiz = dummy_base
  ; d = dummy_base
  ; dag = dummy_base
  ; del_fam = dummy_base
  ; del_fam_ok = dummy_base
  ; del_image = dummy_base
  ; del_image_ok = dummy_base
  ; del_ind = dummy_base
  ; del_ind_ok = dummy_base
  ; f = dummy_base
  ; forum = dummy_base
  ; forum_add = dummy_base
  ; forum_add_ok = dummy_base
  ; forum_del = dummy_base
  ; forum_p_p = dummy_base
  ; forum_search = dummy_base
  ; forum_val = dummy_base
  ; forum_view = dummy_base
  ; h = dummy_base
  ; hist = dummy_base
  ; hist_clean = dummy_base
  ; hist_clean_ok = dummy_base
  ; hist_diff = dummy_base
  ; hist_search = dummy_base
  ; imh = dummy_base
  ; inv_fam = dummy_base
  ; inv_fam_ok = dummy_base
  ; itree = dummy_base
  ; kill_anc = dummy_base
  ; lb = dummy_base
  ; ld = dummy_base
  ; linked = dummy_base
  ; list_ind = dummy_base
  ; ll = dummy_base
  ; lm = dummy_base
  ; misc_notes = dummy_base
  ; misc_notes_search = dummy_base
  ; mod_data = dummy_base
  ; mod_data_ok = dummy_base
  ; mod_fam = dummy_base
  ; mod_fam_ok = dummy_base
  ; mod_ind = dummy_base
  ; mod_ind_ok = dummy_base
  ; mod_notes = dummy_base
  ; mod_notes_ok = dummy_base
  ; mod_wiznotes = dummy_base
  ; mod_wiznotes_ok = dummy_base
  ; mrg = dummy_base
  ; mrg_dup = dummy_base
  ; mrg_dup_ind_y_n = dummy_base
  ; mrg_dup_fam_y_n = dummy_base
  ; mrg_fam = dummy_base
  ; mrg_fam_ok = dummy_base
  ; mrg_mod_fam_ok = dummy_base
  ; mrg_ind = dummy_base
  ; mrg_ind_ok = dummy_base
  ; mrg_mod_ind_ok = dummy_base
  ; n = dummy_base
  ; ng = dummy_base
  ; notes = dummy_base
  ; oa = dummy_base
  ; oe = dummy_base
  ; p = dummy_base
  ; pop_pyr = dummy_base
  ; ps = dummy_base
  ; r = dummy_base
  ; request = dummy_base
  ; rl = dummy_base
  ; rlm = dummy_base
  ; s = dummy_base
  ; snd_image = dummy_base
  ; snd_image_ok = dummy_base
  ; src = dummy_base
  ; stat = dummy_base
  ; change_wiz_vis = dummy_base
  ; tt = dummy_base
  ; u = dummy_base
  ; view_wiznotes = dummy_base
  ; warnings = dummy_base
  ; wiznotes = dummy_base
  ; wiznotes_search = dummy_base
  ; _no_mode = dummy_base
#ifdef API
  ; api_all_persons = dummy_base
  ; api_all_families = dummy_base
  ; api_base_warnings = dummy_base
  ; api_close_persons = dummy_base
  ; api_cpl_rel = dummy_base
  ; api_graph_asc = dummy_base
  ; api_graph_asc_lia = dummy_base
  ; api_graph_desc = dummy_base
  ; api_graph_rel = dummy_base
  ; api_first_available_person = dummy_base
  ; api_find_sosa = dummy_base
  ; api_info_base = dummy_base
  ; api_info_ind = dummy_base
  ; api_image = dummy_base
  ; api_image_ext = dummy_base
  ; api_image_all = dummy_base
  ; api_image_person = dummy_base
  ; api_image_update = dummy_base
  ; api_last_modified_persons = dummy_base
  ; api_last_visited_persons = dummy_base
  ; api_list_persons = dummy_base
  ; api_loop_base = dummy_base
  ; api_max_ancestors = dummy_base
  ; api_nb_ancestors = dummy_base
  ; api_notification_birthday = dummy_base
  ; api_ref_person_from_id = dummy_base
  ; api_remove_image_ext = dummy_base
  ; api_remove_image_ext_all = dummy_base
  ; api_search = dummy_base
  ; api_graph_tree_v2 = dummy_base
  ; api_person_tree = dummy_base
  ; api_fiche_person = dummy_base
  ; api_auto_complete = dummy_base
  ; api_get_config = dummy_base
  ; api_person_search_list = dummy_base
  ; api_get_person_search_info = dummy_base
  ; api_add_child = dummy_base
  ; api_add_child_ok = dummy_base
  ; api_add_family = dummy_base
  ; api_add_family_ok = dummy_base
  ; api_add_first_fam_ok = dummy_base
  ; api_add_parents = dummy_base
  ; api_add_parents_ok = dummy_base
  ; api_add_person_ok = dummy_base
  ; api_add_person_start_ok = dummy_base
  ; api_add_sibling = dummy_base
  ; api_add_sibling_ok = dummy_base
  ; api_edit_family_request = dummy_base
  ; api_edit_family = dummy_base
  ; api_edit_family_ok = dummy_base
  ; api_edit_person = dummy_base
  ; api_edit_person_ok = dummy_base
  ; api_del_family_ok = dummy_base
  ; api_del_person_ok = dummy_base
  ; api_link_tree = dummy_base
  ; api_stats = dummy_base
  ; api_add_first_fam = dummy_nobase
  ; api_select_events = dummy_base
#endif
  ; fallback = dummy_base
  }

let restricted_wizard fn self conf base =
  if conf.wizard then fn self conf base
  else self.incorrect_request self conf base

let person_selected self conf base p =
  match p_getenv conf.senv "em" with
    Some "R" -> relation_print conf base p
  | Some _ -> self.incorrect_request self conf base
  | None -> record_visited conf (get_iper p); Perso.print conf base p

let person_selected_with_redirect self conf base p =
  match p_getenv conf.senv "em" with
    Some "R" -> relation_print conf base p
  | Some _ -> self.incorrect_request self conf base
  | None ->
    Wserver.http_redirect_temporarily (commd conf ^ Util.acces conf base p)

let updmenu_print = Perso.interp_templ "updmenu"

let defaultHandler : handler =
  let if_enabled_forum fn self conf base =
    if p_getenv conf.base_env "disable_forum" <> Some "yes" then fn conf base
    else self.incorrect_request self conf base
  in
  { _no_mode = begin fun self conf base ->
        match find_person_in_env conf base "" with
        | Some p -> person_selected self conf base p
        | _ -> self.very_unknown self conf base
      end

  ; incorrect_request = begin fun _self conf _base ->
      Hutil.incorrect_request conf
    end

  ; unknown = begin fun conf n ->
      let title _ =
        Wserver.printf "%s: \"%s\"" (Utf8.capitalize (transl conf "not found")) n
      in
      Wserver.http Wserver.Not_Found;
      Hutil.rheader conf title;
      Hutil.print_link_to_welcome conf false;
      Hutil.trailer conf
    end

  ; very_unknown = begin fun self conf base ->
      match p_getenv conf.env "n", p_getenv conf.env "p" with
      | Some sname, Some fname ->
        let title _ =
          Wserver.printf "%s: \"%s %s\"" (Utf8.capitalize (transl conf "not found"))
            fname sname
        in
        Wserver.http Wserver.Not_Found;
        Hutil.rheader conf title;
        Hutil.print_link_to_welcome conf false;
        Hutil.trailer conf
      | _ -> self.incorrect_request self conf base
    end

  ; a = begin fun self conf base ->
      match find_person_in_env conf base "" with
      | Some p -> Perso.print_ascend conf base p
      | _ -> self.very_unknown self conf base
    end

  ; add_fam = begin fun self conf base ->
      if conf.wizard then UpdateFam.print_add conf base
      else self.incorrect_request self conf base
    end

  ; add_fam_ok = begin fun self conf base ->
      if conf.wizard then UpdateFamOk.print_add conf base
      else self.incorrect_request self conf base
    end

  ; add_ind = begin fun self conf base ->
      if conf.wizard then UpdateInd.print_add conf base
      else self.incorrect_request self conf base
    end

  ; add_ind_ok = begin fun self conf base ->
      if conf.wizard then UpdateIndOk.print_add conf base
      else self.incorrect_request self conf base
    end

  ; add_par = begin fun self conf base ->
      if conf.wizard then UpdateFam.print_add_parents conf base
      else self.incorrect_request self conf base
    end

  ; add_par_ok = begin fun self conf base ->
      if conf.wizard then UpdateFamOk.print_add_parents conf base
      else self.incorrect_request self conf base
    end

  ; anm = begin fun _self conf _base ->
      BirthdayDisplay.print_anniversaries conf
    end

  ; an = begin fun _self conf base ->
      match p_getenv conf.env "v" with
      | Some x -> BirthdayDisplay.print_birth conf base (int_of_string x)
      | _ -> BirthdayDisplay.print_menu_birth conf base
    end

  ; ad = begin fun _self conf base ->
      match p_getenv conf.env "v" with
      | Some x -> BirthdayDisplay.print_dead conf base (int_of_string x)
      | _ -> BirthdayDisplay.print_menu_dead conf base
    end

  ; am = begin fun _self conf base ->
      match p_getenv conf.env "v" with
      | Some x -> BirthdayDisplay.print_marriage conf base (int_of_string x)
      | _ -> BirthdayDisplay.print_menu_marriage conf base
    end

  ; as_ok = begin fun _self conf base ->
      AdvSearchOkDisplay.print conf base
    end

  ; b = begin fun _self conf base ->
      if conf.wizard || conf.friend then BirthDeathDisplay.print_birth conf base
    end

  ; c = begin fun self conf base ->
      match find_person_in_env conf base "" with
      | Some p -> CousinsDisplay.print conf base p
      | _ -> self.very_unknown self conf base
    end

  ; cal = begin fun _self conf _base ->
      Hutil.print_calendar conf
    end

  ; chg_chn = begin fun self conf base ->
      if conf.wizard then ChangeChildrenDisplay.print conf base
      else self.incorrect_request self conf base
    end

  ; chg_chn_ok = begin fun self conf base ->
      if conf.wizard then ChangeChildrenDisplay.print_ok conf base
      else self.incorrect_request self conf base
    end

  ; chg_evt_ind_ord = begin fun self conf base ->
      if conf.wizard then UpdateInd.print_change_event_order conf base
      else self.incorrect_request self conf base
    end

  ; chg_evt_ind_ord_ok = begin fun self conf base ->
      if conf.wizard then UpdateIndOk.print_change_event_order conf base
      else self.incorrect_request self conf base
    end

  ; chg_evt_fam_ord = begin fun self conf base ->
      if conf.wizard then UpdateFam.print_change_event_order conf base
      else self.incorrect_request self conf base
    end

  ; chg_evt_fam_ord_ok = begin fun self conf base ->
      if conf.wizard then UpdateFamOk.print_change_event_order conf base
      else self.incorrect_request self conf base
    end

  ; chg_fam_ord = begin fun self conf base ->
      if conf.wizard then UpdateFam.print_change_order conf base
      else self.incorrect_request self conf base
    end

  ; chg_fam_ord_ok = begin fun self conf base ->
      if conf.wizard then UpdateFamOk.print_change_order_ok conf base
      else self.incorrect_request self conf base
    end

  ; conn_wiz = begin fun self conf base ->
      if conf.wizard then WiznotesDisplay.connected_wizards conf base
      else self.incorrect_request self conf base
    end

  ; d = begin fun self conf base ->
      match find_person_in_env conf base "" with
      | Some p -> DescendDisplay.print conf base p
      | _ -> self.very_unknown self conf base
    end

  ; dag = begin fun _self conf base ->
      DagDisplay.print conf base
    end

  ; del_fam = begin fun self conf base ->
      if conf.wizard then UpdateFam.print_del conf base
      else self.incorrect_request self conf base
    end

  ; del_fam_ok = begin fun self conf base ->
      if conf.wizard then UpdateFamOk.print_del conf base
      else self.incorrect_request self conf base
    end

  ; del_image = begin fun self conf base ->
      if conf.wizard && conf.can_send_image then SendImage.print_del conf base
      else self.incorrect_request self conf base
    end

  ; del_image_ok = begin fun self conf base ->
      if conf.wizard && conf.can_send_image then SendImage.print_del_ok conf base
      else self.incorrect_request self conf base
    end

  ; del_ind = begin fun self conf base ->
      if conf.wizard then UpdateInd.print_del conf base
      else self.incorrect_request self conf base
    end

  ; del_ind_ok = begin fun self conf base ->
      if conf.wizard then UpdateIndOk.print_del conf base
      else self.incorrect_request self conf base
    end

  ; f = begin fun self conf base ->
      match find_person_in_env conf base "" with
      | Some p -> Perso.interp_templ "family" conf base p
      | _ -> self.very_unknown self conf base
    end

  ; forum = if_enabled_forum ForumDisplay.print

  ; forum_add = if_enabled_forum ForumDisplay.print_add

  ; forum_add_ok = if_enabled_forum ForumDisplay.print_add_ok

  ; forum_del = if_enabled_forum ForumDisplay.print_del

  ; forum_p_p = if_enabled_forum ForumDisplay.print_access_switch

  ; forum_search = if_enabled_forum ForumDisplay.print_search

  ; forum_val = if_enabled_forum ForumDisplay.print_valid

  ; forum_view = if_enabled_forum ForumDisplay.print

  ; h = begin fun self conf base ->
      match Util.p_getenv conf.env "v" with
      | Some "advanced" ->
        JgInterp.render ~conf ~file:"h_advanced" ~models:(Gwxjg.Data.default_env conf base)
      | Some f -> SrcfileDisplay.print conf base f
      | None -> self.incorrect_request self conf base
    end

  ; hist = begin fun _self conf base ->
      History.print conf base
    end

  ; hist_clean = begin fun self conf base ->
      if conf.wizard then HistoryDiffDisplay.print_clean conf
      else self.incorrect_request self conf base
    end

  ; hist_clean_ok = begin fun self conf base ->
      if conf.wizard then HistoryDiffDisplay.print_clean_ok conf
      else self.incorrect_request self conf base
    end

  ; hist_diff = begin fun _self conf base ->
      HistoryDiffDisplay.print conf base
    end

  ; hist_search = begin fun _self conf base ->
      History.print_search conf base
    end

  ; imh = begin fun _self conf _base ->
      ImageDisplay.print_html conf
    end

  ; inv_fam = begin fun self conf base ->
      if conf.wizard then UpdateFam.print_inv conf base
      else self.incorrect_request self conf base
    end

  ; inv_fam_ok = begin fun self conf base ->
      if conf.wizard then UpdateFamOk.print_inv conf base
      else self.incorrect_request self conf base
    end

  ; itree = begin fun self conf base ->
      match find_person_in_env conf base "" with
      | Some p ->
        let root = Gwxjg.Data.get_n_mk_person conf base (get_iper p) in
        JgInterp.render ~conf ~file:"itree"
          ~models:(("root", root) :: Gwxjg.Data.default_env conf base)
      | _ -> self.very_unknown self conf base
    end

  ; kill_anc = begin fun self conf base ->
      if conf.wizard then MergeIndDisplay.print_kill_ancestors conf base
      else self.incorrect_request self conf base
    end

  ; lb = begin fun self conf base ->
      if conf.wizard || conf.friend then BirthDeathDisplay.print_birth conf base
      else self.incorrect_request self conf base
    end

  ; ld = begin fun self conf base ->
      if conf.wizard || conf.friend then BirthDeathDisplay.print_death conf base
      else self.incorrect_request self conf base
    end

  ; linked = begin fun self conf base ->
      match find_person_in_env conf base "" with
      | Some p -> Perso.print_what_links conf base p
      | _ -> self.very_unknown self conf base
    end

  ; list_ind = begin fun self conf base ->
      let open List_ind in
      let num = (Opt.default 1 @@ Util.p_getint conf.env "pg") - 1 in
      let size = Opt.default 2000 @@ Util.p_getint conf.env "sz" in
      if not (is_cache_iper_inorder_uptodate conf base)
      then build_cache_iper_inorder conf base ;
      let page_count, letters, ipers, num =
        read_cache_iper_inorder conf num size
      in
      let persons =
        Array.map begin fun i ->
          Gwxjg.Data.unsafe_mk_person conf base @@ Gwdb.poi base i
        end ipers
      in
      let anchorAtIndex =
        let fst_idx = size * num in
        let list = List.map (fun (c, i) -> (i - fst_idx, c)) letters  in
        Jg_types.func_arg1_no_kw @@ function
        | Tint i ->
          begin match List.assoc_opt i list with
            |  Some s -> Tstr s
            | None -> Tnull
          end
        | x -> Jg_types.func_failure [x]
      in
      let letters =
        List.map begin fun (c, i) ->
          Tset [ Tstr c ; Tint (i / size + 1) ]
        end letters
      in
      let models = ("letters", Tlist letters)
                   :: ("anchorAtIndex", anchorAtIndex)
                   :: ("persons", Tarray persons)
                   :: ("page_num", Tint (num + 1))
                   :: ("page_count", Tint page_count)
                   :: Gwxjg.Data.default_env conf base
      in
      JgInterp.render ~conf ~file:"list_ind" ~models
    end

  ; ll = begin fun _self conf base ->
      BirthDeathDisplay.print_longest_lived conf base
    end

  ; lm = begin fun self conf base ->
      if conf.wizard || conf.friend then BirthDeathDisplay.print_marriage conf base
      else self.incorrect_request self conf base
    end

  ; misc_notes = begin fun _self conf base ->
      NotesDisplay.print_misc_notes conf base
    end

  ; misc_notes_search = begin fun _self conf base ->
      NotesDisplay.print_misc_notes_search conf base
    end

  ; mod_data = begin fun self conf base ->
      if conf.wizard then UpdateDataDisplay.print_mod conf base
      else self.incorrect_request self conf base
    end

  ; mod_data_ok = begin fun self conf base ->
      if conf.wizard then UpdateDataDisplay.print_mod_ok conf base
      else self.incorrect_request self conf base
    end

  ; mod_fam = restricted_wizard begin fun self conf base ->
      match Util.p_getenv conf.env "i" with
      | Some i ->
        let ifam = ifam_of_string i in
        let sfam = UpdateFam.string_family_of conf base ifam in
        let digest = Update.digest_family sfam in
        let models =
          ("digest", Tstr digest)
          :: ( "family"
             , Gwxjg.Data.get_n_mk_family conf base ifam @@ Gwdb.foi base ifam)
          :: Gwxjg.Data.default_env conf base
        in
        JgInterp.render ~conf ~file:"updfam" ~models
      | _ -> self.incorrect_request self conf base
    end

  ; mod_fam_ok = begin fun self conf base ->
      if conf.wizard then UpdateFamOk.print_mod conf base
      else self.incorrect_request self conf base
    end

  ; mod_ind = begin fun self conf base ->
      if conf.wizard then UpdateInd.print_mod conf base
      else self.incorrect_request self conf base
    end

  ; mod_ind_ok = begin fun self conf base ->
      if conf.wizard then UpdateIndOk.print_mod conf base
      else self.incorrect_request self conf base
    end

  ; mod_notes = begin fun self conf base ->
      if conf.wizard then NotesDisplay.print_mod conf base
      else self.incorrect_request self conf base
    end

  ; mod_notes_ok = begin fun self conf base ->
      if conf.wizard then NotesDisplay.print_mod_ok conf base
      else self.incorrect_request self conf base
    end

  ; mod_wiznotes = begin fun self conf base ->
      if conf.authorized_wizards_notes then WiznotesDisplay.print_mod conf base
      else self.incorrect_request self conf base
    end

  ; mod_wiznotes_ok = begin fun self conf base ->
      if conf.authorized_wizards_notes then WiznotesDisplay.print_mod_ok conf base
      else self.incorrect_request self conf base
    end

  ; mrg = begin fun self conf base ->
      if conf.wizard then match find_person_in_env conf base "" with
        | Some p -> MergeDisplay.print conf base p
        | _ -> self.very_unknown self conf base
      else self.incorrect_request self conf base
    end

  ; mrg_dup = begin fun self conf base ->
      if conf.wizard then MergeDupDisplay.main_page conf base
      else self.incorrect_request self conf base
    end

  ; mrg_dup_ind_y_n = begin fun self conf base ->
      if conf.wizard then MergeDupDisplay.answ_ind_y_n conf base
      else self.incorrect_request self conf base
    end

  ; mrg_dup_fam_y_n = begin fun self conf base ->
      if conf.wizard then MergeDupDisplay.answ_fam_y_n conf base
      else self.incorrect_request self conf base
    end

  ; mrg_fam = begin fun self conf base ->
      if conf.wizard then MergeFamDisplay.print conf base
      else self.incorrect_request self conf base
    end

  ; mrg_fam_ok = begin fun self conf base ->
      if conf.wizard then MergeFamOk.print_merge conf base
      else self.incorrect_request self conf base
    end

  ; mrg_mod_fam_ok = begin fun self conf base ->
      if conf.wizard then MergeFamOk.print_mod_merge conf base
      else self.incorrect_request self conf base
    end

  ; mrg_ind = begin fun self conf base ->
      if conf.wizard then MergeIndDisplay.print conf base
      else self.incorrect_request self conf base
    end

  ; mrg_ind_ok = begin fun self conf base ->
      if conf.wizard then MergeIndOkDisplay.print_merge conf base
      else self.incorrect_request self conf base
    end

  ; mrg_mod_ind_ok = begin fun self conf base ->
      if conf.wizard then MergeIndOkDisplay.print_mod_merge conf base
      else self.incorrect_request self conf base
    end

  ; n = begin fun _self conf base ->
      match p_getenv conf.env "v" with
      | Some v -> Some.surname_print conf base Some.surname_not_found v
      | _ -> AllnDisplay.print_surnames conf base
    end

  ; ng = begin fun self conf base ->
      (* Rétro-compatibilité <= 6.06 *)
      let env =
        match p_getenv conf.env "n" with
          Some n ->
          begin match p_getenv conf.env "t" with
              Some "P" -> ("fn", n) :: conf.env
            | Some "N" -> ("sn", n) :: conf.env
            | _ -> ("v", n) :: conf.env
          end
        | None -> conf.env
      in
      let conf = {conf with env = env} in
      (* Nouveau mode de recherche. *)
      begin match p_getenv conf.env "select" with
          Some "input" | None ->
          (* Récupère le contenu non vide de la recherche. *)
          let real_input label =
            match p_getenv conf.env label with
              Some s -> if s = "" then None else Some s
            | None -> None
          in
          (* Recherche par clé, sosa, alias ... *)
          let search n =
            let (pl, sosa_acc) = find_all conf base n in
            match pl with
            | [] ->
              conf.cancel_links <- false ;
              Some.surname_print conf base self.unknown n
            | [p] ->
              if sosa_acc || Gutil.person_of_string_key base n <> None ||
                 person_is_std_key conf base p n
              then
                person_selected_with_redirect self conf base p
              else specify conf base n pl
            | pl -> specify conf base n pl
          in
          begin match real_input "v" with
              Some n -> search n
            | None ->
              match real_input "fn", real_input "sn" with
                Some fn, Some sn -> search (fn ^ " " ^ sn)
              | Some fn, None ->
                conf.cancel_links <- false ;
                Some.first_name_print conf base fn
              | None, Some sn ->
                conf.cancel_links <- false ;
                Some.surname_print conf base self.unknown sn
              | None, None -> self.incorrect_request self conf base
          end
        | Some i ->
          relation_print conf base
            (pget conf base (iper_of_string i))
      end
    end

  ; notes = begin fun _self conf base ->
      NotesDisplay.print conf base
    end

  ; oa = begin fun self conf base ->
      if conf.wizard || conf.friend then BirthDeathDisplay.print_oldest_alive conf base
      else self.incorrect_request self conf base
    end

  ; oe = begin fun self conf base ->
      if conf.wizard || conf.friend then BirthDeathDisplay.print_oldest_engagements conf base
      else self.incorrect_request self conf base
    end

  ; p = begin fun _self conf base ->
      match p_getenv conf.env "v" with
      | Some v -> Some.first_name_print conf base v
      | None -> AllnDisplay.print_first_names conf base
    end

  ; pop_pyr = begin fun self conf base ->
      if conf.wizard || conf.friend then BirthDeathDisplay.print_population_pyramid conf base
      else self.incorrect_request self conf base
    end

  ; ps = begin fun _self conf base ->
      PlaceDisplay.print_all_places_surnames conf base
    end

  ; r = begin fun self conf base ->
      match find_person_in_env conf base "" with
      | Some p -> relation_print conf base p
      | _ -> self.very_unknown self conf base
    end

  ; request = begin fun self conf base ->
      if conf.wizard then begin
        Hutil.header conf (fun _ -> ()) ;
        Wserver.printf "<pre>\n" ;
        List.iter (Wserver.printf "%s\n") conf.Config.request ;
        Wserver.printf "</pre>\n" ;
        Hutil.trailer conf
      end
      else self.incorrect_request self conf base
    end

  ; rl = begin fun _self conf base ->
      RelationLink.print conf base
    end

  ; rlm = begin fun _self conf base ->
      RelationDisplay.print_multi conf base
    end

  ; s = begin fun self conf base ->
      SearchName.print conf base specify self.unknown
    end

  ; snd_image = begin fun self conf base ->
      if conf.wizard && conf.can_send_image then SendImage.print conf base
      else self.incorrect_request self conf base
    end

  ; snd_image_ok = begin fun self conf base ->
      if conf.wizard && conf.can_send_image then SendImage.print_send_ok conf base
      else self.incorrect_request self conf base
    end

  ; src = begin fun _self conf base ->
      match p_getenv conf.env "v" with
      | Some f -> SrcfileDisplay.print_source conf base f
      | _ -> Hutil.incorrect_request conf
    end

  ; stat = begin fun _self conf _base ->
      BirthDeathDisplay.print_statistics conf
    end

  ; change_wiz_vis = begin fun self conf base ->
      if conf.wizard then WiznotesDisplay.change_wizard_visibility conf base
      else self.incorrect_request self conf base
    end

  ; tt = begin fun _self conf base ->
      TitleDisplay.print conf base
    end

  ; u = begin fun self conf base ->
      if conf.wizard then match find_person_in_env conf base "" with
        | Some p -> updmenu_print conf base p
        | _ -> self.very_unknown self conf base
      else self.incorrect_request self conf base
    end

  ; view_wiznotes = begin fun self conf base ->
      if conf.wizard && conf.authorized_wizards_notes then WiznotesDisplay.print_view conf base
      else self.incorrect_request self conf base
    end

  ; warnings = begin fun _self conf base ->
      let ht = Hashtbl.create 1024 in
      Check.check_base base ignore (fun x -> Hashtbl.replace ht x ()) ignore ;
      let warnings = Hashtbl.fold begin fun w () acc ->
          Gwxjg.Data.mk_warning conf base w :: acc
        end ht [] in
      let models = ("warnings", Tlist warnings)
                   :: Gwxjg.Data.default_env conf base
      in
      JgInterp.render ~conf ~file:"warnings" ~models
    end

  ; wiznotes = begin fun self conf base ->
      if conf.authorized_wizards_notes then WiznotesDisplay.print conf base
      else self.incorrect_request self conf base
    end

  ; wiznotes_search = begin fun self conf base ->
      if conf.authorized_wizards_notes then WiznotesDisplay.print_search conf base
      else self.incorrect_request self conf base
    end

#ifdef API

  ; api_all_persons = begin fun _self conf base ->
      Api.print_all_persons conf base
    end

  ; api_all_families = begin fun _self conf base ->
      Api.print_all_families conf base
    end

  ; api_base_warnings = begin fun self conf base ->
      if conf.wizard
      then Api.print_base_warnings conf base
      else self.incorrect_request self conf base
    end

  ; api_close_persons = begin fun _self conf base ->
      Api_graph.print_close_person_relations conf base
    end

  ; api_cpl_rel = begin fun _self conf base ->
      Api_graph.print_cpl_relation conf base
    end

  ; api_graph_asc = begin fun _self conf base ->
      Api_graph.print_graph_asc conf base
    end

  ; api_graph_asc_lia = begin fun _self conf base ->
      Api_graph.print_graph_asc_lia conf base
    end

  ; api_graph_desc = begin fun _self conf base ->
      Api_graph.print_graph_desc conf base
    end

  ; api_graph_rel = begin fun _self conf base ->
      Api_graph.print_graph_rel conf base
    end

  ; api_first_available_person = begin fun _self conf base ->
      Api.print_first_available_person conf base
    end

  ; api_find_sosa = begin fun _self conf base ->
      Api.print_find_sosa conf base
    end

  ; api_info_base = begin fun _self conf base ->
      Api.print_info_base conf base
    end

  ; api_info_ind = begin fun _self conf base ->
      Api.print_info_ind conf base
    end

  ; api_image = begin fun _self conf base ->
      Api.print_img conf base
    end

  ; api_image_ext = begin fun _self conf base ->
      Api.print_img_ext conf base
    end

  ; api_image_all = begin fun _self conf base ->
      Api.print_img_all conf base
    end

  ; api_image_person = begin fun _self conf base ->
      Api.print_img_person conf base
    end

  ; api_image_update = begin fun _self conf base ->
      if conf.wizard then Api.print_updt_image conf base
    end

  ; api_last_modified_persons = begin fun _self conf base ->
      Api.print_last_modified_persons conf base
    end

  ; api_last_visited_persons = begin fun _self conf base ->
      Api.print_last_visited_persons conf base
    end

  ; api_list_persons = begin fun _self conf base ->
      Api.print_list_ref_person conf base
    end

  ; api_loop_base = begin fun _self conf base ->
      Api.print_loop conf base
    end

  ; api_max_ancestors = begin fun _self conf base ->
      if conf.wizard then Api.print_max_ancestors conf base
    end

  ; api_nb_ancestors = begin fun _self conf base ->
      Api_saisie_read.print_nb_ancestors conf base
    end

  ; api_notification_birthday = begin fun _self conf base ->
      Api.print_notification_birthday conf base
    end

  ; api_ref_person_from_id = begin fun _self conf base ->
      Api.print_ref_person_from_ip conf base
    end

  ; api_remove_image_ext = begin fun _self conf base ->
      if conf.wizard then Api.print_remove_image_ext base
    end

  ; api_remove_image_ext_all = begin fun _self conf base ->
      if conf.wizard then Api.print_remove_image_ext_all base
    end

  ; api_search = begin fun _self conf base ->
      Api_search.print_search conf base
    end

  ; api_graph_tree_v2 = begin fun _self conf base ->
      Api_saisie_read.print_graph_tree_v2 conf base
    end

  ; api_person_tree = begin fun _self conf base ->
      Api_saisie_read.print_person_tree conf base
    end

  ; api_fiche_person = begin fun _self conf base ->
      Api_saisie_read.print_fiche_person conf base
    end

  ; api_auto_complete = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_auto_complete conf base
      else self.incorrect_request self conf base
    end

  ; api_get_config = begin fun _self conf base ->
      if conf.wizard then Api_saisie_write.print_config conf base
    end

  ; api_person_search_list = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_person_search_list conf base
      else self.incorrect_request self conf base
    end

  ; api_get_person_search_info = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_person_search_info conf base
      else self.incorrect_request self conf base
    end

  ; api_add_child = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_child conf base
      else self.incorrect_request self conf base
    end

  ; api_add_child_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_child_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_add_family = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_family conf base
      else self.incorrect_request self conf base
    end

  ; api_add_family_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_family_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_add_first_fam_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_first_fam_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_add_parents = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_parents conf base
      else self.incorrect_request self conf base
    end

  ; api_add_parents_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_parents_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_add_person_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_ind_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_add_person_start_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_ind_start_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_add_sibling = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_sibling conf base
      else self.incorrect_request self conf base
    end

  ; api_add_sibling_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_add_sibling_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_edit_family_request = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_mod_family_request conf base
      else self.incorrect_request self conf base
    end

  ; api_edit_family = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_mod_family conf base
      else self.incorrect_request self conf base
    end

  ; api_edit_family_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_mod_family_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_edit_person = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_mod_ind conf base
      else self.incorrect_request self conf base
    end

  ; api_edit_person_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_mod_ind_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_del_family_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_del_fam_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_del_person_ok = begin fun self conf base ->
      if conf.wizard then Api_saisie_write.print_del_ind_ok conf base
      else self.incorrect_request self conf base
    end

  ; api_link_tree = begin fun _self conf base ->
      Api_link.print_link_tree conf base
    end

  ; api_stats = begin fun _self conf base ->
      Api_stats.print_stats conf base
    end

  ; api_select_events = begin fun _self conf base ->
      Api_graph.print_select_events conf base
    end

  ; api_add_first_fam = begin fun _self conf ->
      Api_saisie_write.print_add_first_fam conf
    end

#endif

  ; fallback = begin fun _mode self conf base ->
      self.incorrect_request self conf base
    end

  }
