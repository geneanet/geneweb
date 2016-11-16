(* camlp5r ../src/def_syn.cmo ../src/pa_lock.cmo ../src/pa_html.cmo *)
(* $Id: request.ml,v 5.61 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Util;


(* Make the "special" environement; "em=mode;ei=n" *)

value set_senv conf vm vi =
  do {
    conf.senv := [("em", vm); ("ei", vi)];
    match p_getenv conf.env "image" with
    [ Some "on" -> conf.senv := conf.senv @ [("image", "on")]
    | _ -> () ];
    match p_getenv conf.env "long" with
    [ Some "on" -> conf.senv := conf.senv @ [("long", "on")]
    | _ -> () ];
    match p_getenv conf.env "spouse" with
    [ Some "on" -> conf.senv := conf.senv @ [("spouse", "on")]
    | _ -> () ];
    match p_getenv conf.env "et" with
    [ Some x -> conf.senv := conf.senv @ [("et", x)]
    | _ -> () ];
    match p_getenv conf.env "cgl" with
    [ Some "on" -> conf.senv := conf.senv @ [("cgl", "on")]
    | _ -> () ];
    match p_getenv conf.env "bd" with
    [ None | Some ("0" | "") -> ()
    | Some x -> conf.senv := conf.senv @ [("bd", x)] ];
    match p_getenv conf.env "color" with
    [ Some x -> conf.senv := conf.senv @ [("color", code_varenv x)]
    | _ -> () ];
  }
;

value make_senv conf base =
  let get x = Util.p_getenv conf.env x in
  match (get "em", get "ei", get "ep", get "en", get "eoc") with
  [ (Some vm, Some vi, _, _, _) -> set_senv conf vm vi
  | (Some vm, None, Some vp, Some vn, voco) ->
      let voc =
        match voco with
        [ Some voc -> try int_of_string voc with [ Failure _ -> 0 ]
        | None -> 0 ]
      in
      let ip =
        match person_of_key base vp vn voc with
        [ Some ip -> ip
        | None -> do { incorrect_request conf; raise Exit } ]
      in
      let vi = string_of_int (Adef.int_of_iper ip) in set_senv conf vm vi
  | _ -> () ]
;

value family_m conf base =
  (* On passe en mode API, i.e. que les exceptions API sont levées. *)
  let () = Api_conf.set_mode_api () in
  match p_getenv conf.env "m" with
  (*[ Some "API_ADD_FAMILY" -> Api_update_family.print_add conf base
  | Some "API_ADD_PERSON" -> Api_update_person.print_add conf base*)
  [ Some "API_ALL_PERSONS" -> Api.print_all_persons conf base
  | Some "API_ALL_FAMILIES" -> Api.print_all_families conf base
  | Some "API_ANNIVERSARY" -> Api.print_birthday conf base
  | Some "API_BASE_WARNINGS" when (conf.wizard || conf.friend) ->
      (* Pour les flex, on autorise en mode friend. *)
      Api.print_base_warnings conf base
  | Some "API_CLOSE_PERSONS" -> Api_graph.print_close_person_relations conf base
  | Some "API_CPL_REL" -> Api_graph.print_cpl_relation conf base
(*
  | Some "API_DELETE_FAMILY" -> Api_update_family.print_del conf base
  | Some "API_DELETE_PERSON" -> Api_update_person.print_del conf base
*)
  | Some "API_GRAPH_ASC" -> Api_graph.print_graph_asc conf base
  | Some "API_GRAPH_ASC_LIA" -> Api_graph.print_graph_asc_lia conf base
  | Some "API_GRAPH_DESC" -> Api_graph.print_graph_desc conf base
  | Some "API_GRAPH_REL" -> Api_graph.print_graph_rel conf base
  | Some "API_FIRST_AVAILABLE_PERSON" -> Api.print_first_available_person conf base
  | Some "API_FIND_SOSA" -> Api.print_find_sosa conf base
  | Some "API_INFO_BASE" -> Api.print_info_base conf base
  | Some "API_INFO_IND" -> Api.print_info_ind conf base
  | Some "API_IMAGE" -> Api.print_img conf base
  | Some "API_IMAGE_EXT" -> Api.print_img_ext conf base
  | Some "API_IMAGE_ALL" -> Api.print_img_all conf base
  | Some "API_IMAGE_PERSON" -> Api.print_img_person conf base
  | Some "API_IMAGE_UPDATE" when conf.wizard -> Api.print_updt_image conf base
  | Some "API_LAST_MODIFIED_PERSONS" -> Api.print_last_modified_persons conf base
  | Some "API_LAST_VISITED_PERSONS" -> Api.print_last_visited_persons conf base
  | Some "API_LIST_PERSONS" -> Api.print_list_ref_person conf base
  | Some "API_LOOP_BASE" -> Api.print_loop conf base
  | Some "API_MAX_ANCESTORS" when conf.wizard -> Api.print_max_ancestors conf base
  | Some "API_NB_ANCESTORS" -> Api_saisie_read.print_nb_ancestors conf base
  | Some "API_NOTIFICATION_BIRTHDAY" -> Api.print_notification_birthday conf base
  | Some "API_PRINT_INDEX" -> Api.print_all_full_person conf base
  | Some "API_PRINT_EXPORT" -> Api.print_export conf base
  | Some "API_PRINT_EXPORT_SEARCH" -> Api.print_export_search conf base
  | Some "API_PRINT_SYNCHRO" -> Api.print_synchro_patch_mobile conf base
  | Some "API_REF_PERSON_FROM_ID" -> Api.print_ref_person_from_ip conf base
  | Some "API_REMOVE_IMAGE_EXT" when conf.wizard -> Api.print_remove_image_ext conf base
  | Some "API_REMOVE_IMAGE_EXT_ALL" when conf.wizard -> Api.print_remove_image_ext_all conf base
  | Some "API_SEARCH" -> Api_search.print_search conf base
(*
  | Some "API_UPDATE_PERSON" -> Api_update_person.print_mod conf base
  | Some "API_UPDATE_FAMILY" -> Api_update_family.print_mod conf base
*)
  | Some "API_GRAPH_TREE_V2" -> Api_saisie_read.print_graph_tree_v2 conf base
  | Some "API_PERSON_TREE" -> Api_saisie_read.print_person_tree conf base
  | Some "API_FICHE_PERSON" -> Api_saisie_read.print_fiche_person conf base
  | Some "API_CONF_BASE" -> Api_saisie_read.print_conf_base conf base

  | Some "API_AUTO_COMPLETE" when conf.wizard -> Api_saisie_write.print_auto_complete conf base
  | Some "API_GET_CONFIG" when conf.wizard -> Api_saisie_write.print_config conf base
  | Some "API_PERSON_SEARCH_LIST" when conf.wizard -> Api_saisie_write.print_person_search_list conf base
  | Some "API_GET_PERSON_SEARCH_INFO" when conf.wizard -> Api_saisie_write.print_person_search_info conf base

  | Some "API_ADD_CHILD" when conf.wizard -> Api_saisie_write.print_add_child conf base
  | Some "API_ADD_CHILD_OK" when conf.wizard -> Api_saisie_write.print_add_child_ok conf base
  | Some "API_ADD_FAMILY" when conf.wizard -> Api_saisie_write.print_add_family conf base
  | Some "API_ADD_FAMILY_OK" when conf.wizard -> Api_saisie_write.print_add_family_ok conf base
  | Some "API_ADD_FIRST_FAM_OK" when conf.wizard -> Api_saisie_write.print_add_first_fam_ok conf base
  | Some "API_ADD_PARENTS" when conf.wizard -> Api_saisie_write.print_add_parents conf base
  | Some "API_ADD_PARENTS_OK" when conf.wizard -> Api_saisie_write.print_add_parents_ok conf base
  | Some "API_ADD_PERSON_OK" when conf.wizard -> Api_saisie_write.print_add_ind_ok conf base
  | Some "API_ADD_PERSON_START_OK" when conf.wizard -> Api_saisie_write.print_add_ind_start_ok conf base
  | Some "API_ADD_SIBLING" when conf.wizard -> Api_saisie_write.print_add_sibling conf base
  | Some "API_ADD_SIBLING_OK" when conf.wizard -> Api_saisie_write.print_add_sibling_ok conf base
  | Some "API_EDIT_FAMILY_REQUEST" when conf.wizard -> Api_saisie_write.print_mod_family_request conf base
  | Some "API_EDIT_FAMILY" when conf.wizard -> Api_saisie_write.print_mod_family conf base
  | Some "API_EDIT_FAMILY_OK" when conf.wizard -> Api_saisie_write.print_mod_family_ok conf base
  | Some "API_EDIT_PERSON" when conf.wizard -> Api_saisie_write.print_mod_ind conf base
  | Some "API_EDIT_PERSON_OK" when conf.wizard -> Api_saisie_write.print_mod_ind_ok conf base
  | Some "API_DEL_FAMILY_OK" when conf.wizard -> Api_saisie_write.print_del_fam_ok conf base
  | Some "API_DEL_PERSON_OK" when conf.wizard -> Api_saisie_write.print_del_ind_ok conf base

  | Some "API_LINK_TREE" -> Api_link.print_link_tree conf base

  | Some "API_STATS" -> Api_stats.print_stats conf base

  | Some mode -> ()
  | None -> () ]
;

value family_m_nobase conf =
  (* On passe en mode API, i.e. que les exceptions API sont levées. *)
  let () = Api_conf.set_mode_api () in
  match p_getenv conf.env "m" with
  [ Some "API_ADD_FIRST_FAM" -> Api_saisie_write.print_add_first_fam conf
  | Some mode -> ()
  | None -> () ]
;

value extract_henv conf base =
  do {
    match find_sosa_ref conf base with
    [ Some p ->
        let x =
          let first_name = p_first_name base p in
          let surname = p_surname base p in
          if Util.accessible_by_key conf base p first_name surname then
            [("pz", code_varenv (Name.lower first_name));
             ("nz", code_varenv (Name.lower surname));
             ("ocz", string_of_int (get_occ p))]
          else [("iz", string_of_int (Adef.int_of_iper (get_key_index p)))]
        in
        conf.henv := conf.henv @ x
    | None -> () ];
    match p_getenv conf.env "dsrc" with
    [ Some "" | None -> ()
    | Some s -> conf.henv := conf.henv @ [("dsrc", code_varenv s)] ];
    match p_getenv conf.env "templ" with
    [ None -> ()
    | Some s -> conf.henv := conf.henv @ [("templ", code_varenv s)] ];
    match p_getenv conf.env "escache" with
    [ Some _ ->
        let v = escache_value base in
        conf.henv := conf.henv @ [("escache", v)]
    | None -> () ];
    match p_getenv conf.env "alwsurn" with
    [ Some x -> conf.henv := conf.henv @ [("alwsurn", x)]
    | None -> () ];
    match p_getenv conf.env "pure_xhtml" with
    [ Some x -> conf.henv := conf.henv @ [("pure_xhtml", x)]
    | None -> () ];
    match p_getenv conf.env "size" with
    [ Some x -> conf.henv := conf.henv @ [("size", x)]
    | None -> () ];
    match p_getenv conf.env "manitou" with
    [ Some "off" -> conf.henv := conf.henv @ [("manitou", "off")]
    | Some _ | None -> () ];
  }
;

value set_owner conf =
  IFDEF UNIX THEN
    let s = Unix.stat (Util.base_path [] (conf.bname ^ ".gwb")) in
    try do { Unix.setgid s.Unix.st_gid; Unix.setuid s.Unix.st_uid; } with
    [ Unix.Unix_error _ _ _ -> () ]
  ELSE () END
;

value treat_request conf base log = do {
  match
    (p_getenv conf.base_env "moved",
     p_getenv conf.env "opt",
     p_getenv conf.env "m")
  with
  [ (Some s, _, _) -> ()
  | (_, Some "no_index", _) -> ()
  | (_, _, Some "IM") -> ()
  | _ ->
      do {
        set_owner conf;
(*        extract_henv conf base; *)
        make_senv conf base;
        family_m conf base
      } ];
   Wserver.wflush ();
};

value treat_request_on_possibly_locked_base conf bfile log =
  match try Left (Gwdb.open_base bfile) with e -> Right e with
  [ Left base ->
      do {
        if Mutil.utf_8_db.val then ()
        else do {
          Hashtbl.clear conf.lexicon;
          let fname = Filename.concat "lang" "lexicon.txt" in
          Mutil.input_lexicon conf.lang conf.lexicon
            (fun () -> Secure.open_in (Util.search_in_lang_path fname));
          conf.charset :=
            try Hashtbl.find conf.lexicon " !charset" with
            [ Not_found -> "iso-8859-1" ];
        };
        try treat_request conf base log with exc ->
          do { close_base base; raise exc };
        close_base base;
      }
  | Right e ->
      let transl conf w =
        try Hashtbl.find conf.lexicon w with [ Not_found -> "[" ^ w ^ "]" ]
      in
      let title _ =
        Wserver.wprint "%s" (Util.capitale (transl conf "error"))
      in
      do {
        Hutil.rheader conf title;
        Wserver.wprint "<ul>";
        Util.html_li conf;
        Wserver.wprint "%s"
          (Util.capitale (transl conf "cannot access base"));
        Wserver.wprint " \"%s\".</ul>\n" conf.bname;
        match e with
        [ Sys_error _ -> ()
        | _ ->
            Wserver.wprint
              "<em><font size=\"-1\">Internal message: %s</font></em>\n"
              (Printexc.to_string e) ];
        Hutil.trailer conf;
      } ]
;

value this_request_updates_database conf =
  match p_getenv conf.env "m" with
  [ Some x when conf.wizard ->
      match x with
      [ "API_BASE_WARNINGS" | "API_IMAGE_UPDATE" |
        "API_REMOVE_IMAGE_EXT" | "API_REMOVE_IMAGE_EXT_ALL" |
        "API_DEL_PERSON_OK" | "API_EDIT_PERSON_OK" | "API_ADD_CHILD_OK" |
        "API_ADD_PERSON_OK" | "API_ADD_PARENTS_OK" | "API_ADD_FAMILY_OK" |
        "API_ADD_FIRST_FAM_OK" |
        "API_EDIT_FAMILY_OK" | "API_DEL_FAMILY_OK" | "API_ADD_SIBLING_OK" |
        "API_ADD_PERSON_START_OK" | "API_PRINT_SYNCHRO" -> True
      | _ -> False ]
  | Some "API_PRINT_SYNCHRO" -> True
      (* Dans la synchro, on bloque la base parce que les friend *)
      (* peuvent aussi mettre à jour le fichier synchro.         *)
  | _ -> False ]
;

value treat_request_on_base conf log =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  if this_request_updates_database conf then
    lock Mutil.lock_file bfile with
    [ Accept -> treat_request_on_possibly_locked_base conf bfile log
    | Refuse -> Update.error_locked conf ]
  else treat_request_on_possibly_locked_base conf bfile log
;

value treat_request_on_nobase conf log = do {
  if Mutil.utf_8_db.val then ()
  else do {
    Hashtbl.clear conf.lexicon;
    let fname = Filename.concat "lang" "lexicon.txt" in
    Mutil.input_lexicon conf.lang conf.lexicon
      (fun () -> Secure.open_in (Util.search_in_lang_path fname));
    conf.charset :=
      try Hashtbl.find conf.lexicon " !charset" with
      [ Not_found -> "iso-8859-1" ];
  };
  try
    do {
      family_m_nobase conf;
      Wserver.wflush ();
    }
  with exc -> do { raise exc }
};
