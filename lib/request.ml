(* Copyright (c) 1998-2007 INRIA *)

module type MakeIn = sig
  val handler : RequestHandler.handler
end

module type MakeOut = sig
  val treat_request_on_base : Config.config -> unit
  val treat_request_on_nobase : Config.config -> unit
end

module Make (H : MakeIn) : MakeOut = struct

open Config
open Def
open Gwdb
open Util

(* Make the "special" environement; "em=mode&ei=n" *)

let set_senv conf vm vi =
  conf.senv <- ["em", vm; "ei", vi];
  begin match p_getenv conf.env "image" with
    Some "off" -> conf.senv <- conf.senv @ ["image", "off"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "long" with
    Some "on" -> conf.senv <- conf.senv @ ["long", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "spouse" with
    Some "on" -> conf.senv <- conf.senv @ ["spouse", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "et" with
    Some x -> conf.senv <- conf.senv @ ["et", x]
  | _ -> ()
  end;
  begin match p_getenv conf.env "cgl" with
    Some "on" -> conf.senv <- conf.senv @ ["cgl", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "bd" with
    None | Some ("0" | "") -> ()
  | Some x -> conf.senv <- conf.senv @ ["bd", x]
  end;
  match p_getenv conf.env "color" with
    Some x -> conf.senv <- conf.senv @ ["color", code_varenv x]
  | _ -> ()

let make_senv conf base =
  let get x = Util.p_getenv conf.env x in
  match get "em", get "ei", get "ep", get "en", get "eoc" with
    Some vm, Some vi, _, _, _ -> set_senv conf vm vi
  | Some vm, None, Some vp, Some vn, voco ->
      let voc =
        match voco with
          Some voc -> (try int_of_string voc with Failure _ -> 0)
        | None -> 0
      in
      let ip =
        match person_of_key base vp vn voc with
          Some ip -> ip
        | None -> Hutil.incorrect_request conf; raise Exit
      in
      let vi = string_of_iper ip in set_senv conf vm vi
  | _ -> ()

[@@@ocaml.warning "-45"]
let family_m conf base =
  let open RequestHandler in
  let handler = H.handler in
  let p =
    match p_getenv conf.env "m" with
    | None -> handler._no_mode
    | Some s -> match s with
      | "A" -> handler.a
      | "ADD_FAM" -> handler.add_fam
      | "ADD_FAM_OK" -> handler.add_fam_ok
      | "ADD_IND" -> handler.add_ind
      | "ADD_IND_OK" -> handler.add_ind_ok
      | "ADD_PAR" -> handler.add_par
      | "ADD_PAR_OK" -> handler.add_par_ok
      | "ANM" -> handler.anm
      | "AN" -> handler.an
      | "AD" -> handler.ad
      | "AM" -> handler.am
      | "AS_OK" -> handler.as_ok
      | "B" -> handler.b
      | "C" -> handler.c
      | "CAL" -> handler.cal
      | "CHG_CHN" -> handler.chg_chn
      | "CHG_CHN_OK" -> handler.chg_chn_ok
      | "CHG_EVT_IND_ORD" -> handler.chg_evt_ind_ord
      | "CHG_EVT_IND_ORD_OK" -> handler.chg_evt_ind_ord_ok
      | "CHG_EVT_FAM_ORD" -> handler.chg_evt_fam_ord
      | "CHG_EVT_FAM_ORD_OK" -> handler.chg_evt_fam_ord_ok
      | "CHG_FAM_ORD" -> handler.chg_fam_ord
      | "CHG_FAM_ORD_OK" -> handler.chg_fam_ord_ok
      | "CONN_WIZ" -> handler.conn_wiz
      | "D" -> handler.d
      | "DAG" -> handler.dag
      | "DEL_FAM" -> handler.del_fam
      | "DEL_FAM_OK" -> handler.del_fam_ok
      | "DEL_IMAGE" -> handler.del_image
      | "DEL_IMAGE_OK" -> handler.del_image_ok
      | "DEL_IND" -> handler.del_ind
      | "DEL_IND_OK" -> handler.del_ind_ok
      | "F" -> handler.f
      | "FORUM" -> handler.forum
      | "FORUM_ADD" -> handler.forum_add
      | "FORUM_ADD_OK" -> handler.forum_add_ok
      | "FORUM_DEL" -> handler.forum_del
      | "FORUM_P_P" -> handler.forum_p_p
      | "FORUM_SEARCH" -> handler.forum_search
      | "FORUM_VAL" -> handler.forum_val
      | "FORUM_VIEW" -> handler.forum_view
      | "H" -> handler.h
      | "HIST" -> handler.hist
      | "HIST_CLEAN" -> handler.hist_clean
      | "HIST_CLEAN_OK" -> handler.hist_clean_ok
      | "HIST_DIFF" -> handler.hist_diff
      | "HIST_SEARCH" -> handler.hist_search
      | "IMH" -> handler.imh
      | "INV_FAM" -> handler.inv_fam
      | "INV_FAM_OK" -> handler.inv_fam_ok
      | "KILL_ANC" -> handler.kill_anc
      | "LB" -> handler.lb
      | "LD" -> handler.ld
      | "LINKED" -> handler.linked
      | "LL" -> handler.ll
      | "LM" -> handler.lm
      | "MISC_NOTES" -> handler.misc_notes
      | "MISC_NOTES_SEARCH" -> handler.misc_notes_search
      | "MOD_DATA" -> handler.mod_data
      | "MOD_DATA_OK" -> handler.mod_data_ok
      | "MOD_FAM" -> handler.mod_fam
      | "MOD_FAM_OK" -> handler.mod_fam_ok
      | "MOD_IND" -> handler.mod_ind
      | "MOD_IND_OK" -> handler.mod_ind_ok
      | "MOD_NOTES" -> handler.mod_notes
      | "MOD_NOTES_OK" -> handler.mod_notes_ok
      | "MOD_WIZNOTES" -> handler.mod_wiznotes
      | "MOD_WIZNOTES_OK" -> handler.mod_wiznotes_ok
      | "MRG" -> handler.mrg
      | "MRG_DUP" -> handler.mrg_dup
      | "MRG_DUP_IND_Y_N" -> handler.mrg_dup_ind_y_n
      | "MRG_DUP_FAM_Y_N" -> handler.mrg_dup_fam_y_n
      | "MRG_FAM" -> handler.mrg_fam
      | "MRG_FAM_OK" -> handler.mrg_fam_ok
      | "MRG_MOD_FAM_OK" -> handler.mrg_mod_fam_ok
      | "MRG_IND" -> handler.mrg_ind
      | "MRG_IND_OK" -> handler.mrg_ind_ok
      | "MRG_MOD_IND_OK" -> handler.mrg_mod_ind_ok
      | "N" -> handler.n
      | "NG" -> handler.ng
      | "NOTES" -> handler.notes
      | "OA" -> handler.oa
      | "OE" -> handler.oe
      | "P" -> handler.p
      | "POP_PYR" -> handler.pop_pyr
      | "PS" -> handler.ps
      | "R" -> handler.r
      | "REQUEST" -> handler.request
      | "RL" -> handler.rl
      | "RLM" -> handler.rlm
      | "S" -> handler.s
      | "SND_IMAGE" -> handler.snd_image
      | "SND_IMAGE_OK" -> handler.snd_image_ok
      | "SRC" -> handler.src
      | "STAT" -> handler.stat
      | "CHANGE_WIZ_VIS" -> handler.change_wiz_vis
      | "TT" -> handler.tt
      | "U" -> handler.u
      | "VIEW_WIZNOTES" -> handler.view_wiznotes
      | "WIZNOTES" -> handler.wiznotes
      | "WIZNOTES_SEARCH" -> handler.wiznotes_search
#ifdef API
      | mode
        when
          try
            String.get mode 0 = 'A' && String.get mode 1 = 'P' &&
            String.get mode 2 = 'I' && String.get mode 3 = '_'
          with _ -> false ->
        (* On passe en mode API, i.e. que les exceptions API sont levées. *)
        let () = Api_conf.set_mode_api () in
        begin match mode with
          | "API_ALL_PERSONS" -> handler.api_all_persons
          | "API_ALL_FAMILIES" -> handler.api_all_families
          | "API_BASE_WARNINGS" -> handler.api_base_warnings
          | "API_CLOSE_PERSONS" -> handler.api_close_persons
          | "API_CPL_REL" -> handler.api_cpl_rel
          | "API_GRAPH_ASC" -> handler.api_graph_asc
          | "API_GRAPH_ASC_LIA" -> handler.api_graph_asc_lia
          | "API_GRAPH_DESC" -> handler.api_graph_desc
          | "API_GRAPH_REL" -> handler.api_graph_rel
          | "API_FIRST_AVAILABLE_PERSON" -> handler.api_first_available_person
          | "API_FIND_SOSA" -> handler.api_find_sosa
          | "API_INFO_BASE" -> handler.api_info_base
          | "API_INFO_IND" -> handler.api_info_ind
          | "API_IMAGE_ALL" -> handler.api_image_all
          | "API_IMAGE_PERSON" -> handler.api_image_person
          | "API_IMAGE_UPDATE" -> handler.api_image_update
          | "API_LAST_MODIFIED_PERSONS" -> handler.api_last_modified_persons
          | "API_LAST_VISITED_PERSONS" -> handler.api_last_visited_persons
          | "API_LIST_PERSONS" -> handler.api_list_persons
          | "API_LOOP_BASE" -> handler.api_loop_base
          | "API_MAX_ANCESTORS" -> handler.api_max_ancestors
          | "API_NB_ANCESTORS" -> handler.api_nb_ancestors
          | "API_NOTIFICATION_BIRTHDAY" -> handler.api_notification_birthday
          | "API_REF_PERSON_FROM_ID" -> handler.api_ref_person_from_id
          | "API_SEARCH" -> handler.api_search
          | "API_GRAPH_TREE_V2" -> handler.api_graph_tree_v2
          | "API_PERSON_TREE" -> handler.api_person_tree
          | "API_FICHE_PERSON" -> handler.api_fiche_person
          | "API_AUTO_COMPLETE" -> handler.api_auto_complete
          | "API_GET_CONFIG" -> handler.api_get_config
          | "API_PERSON_SEARCH_LIST" -> handler.api_person_search_list
          | "API_GET_PERSON_SEARCH_INFO" -> handler.api_get_person_search_info
          | "API_ADD_CHILD" -> handler.api_add_child
          | "API_ADD_CHILD_OK" -> handler.api_add_child_ok
          | "API_ADD_FAMILY" -> handler.api_add_family
          | "API_ADD_FAMILY_OK" -> handler.api_add_family_ok
          | "API_ADD_FIRST_FAM_OK" -> handler.api_add_first_fam_ok
          | "API_ADD_PARENTS" -> handler.api_add_parents
          | "API_ADD_PARENTS_OK" -> handler.api_add_parents_ok
          | "API_ADD_PERSON_OK" -> handler.api_add_person_ok
          | "API_ADD_PERSON_START_OK" -> handler.api_add_person_start_ok
          | "API_ADD_SIBLING" -> handler.api_add_sibling
          | "API_ADD_SIBLING_OK" -> handler.api_add_sibling_ok
          | "API_EDIT_FAMILY_REQUEST" -> handler.api_edit_family_request
          | "API_EDIT_FAMILY" -> handler.api_edit_family
          | "API_EDIT_FAMILY_OK" -> handler.api_edit_family_ok
          | "API_EDIT_PERSON" -> handler.api_edit_person
          | "API_EDIT_PERSON_OK" -> handler.api_edit_person_ok
          | "API_DEL_FAMILY_OK" -> handler.api_del_family_ok
          | "API_DEL_PERSON_OK" -> handler.api_del_person_ok
          | "API_LINK_TREE" -> handler.api_link_tree
          | "API_STATS" -> handler.api_stats
          | "API_SELECT_EVENTS" -> handler.api_select_events
          | unknown -> handler.fallback unknown
        end
#endif
      | unknown -> handler.fallback unknown
  in
  p handler conf base

let family_m_nobase conf =
#ifdef API
  let open RequestHandler in
  let handler = H.handler in
  (* On passe en mode API, i.e. que les exceptions API sont levées. *)
  let () = Api_conf.set_mode_api () in
  match p_getenv conf.env "m" with
    Some "API_ADD_FIRST_FAM" -> handler.api_add_first_fam handler conf
  | Some _ | None -> ()
#else
  Hutil.incorrect_request conf
#endif

[@@@ocaml.warning "+45"]

let special_vars =
  ["alwsurn"; "cgl"; "dsrc"; "em"; "ei"; "ep"; "en"; "eoc"; "escache"; "et";
   "iz"; "log_cnl"; "log_pwd"; "log_uid"; "long"; "manitou"; "nz"; "ocz";
   "pz"; "pure_xhtml"; "size"; "spouse"; "templ"; "p_mod"; "wide"]

let only_special_env = List.for_all (fun (x, _) -> List.mem x special_vars)

let extract_henv conf base =
  begin match find_sosa_ref conf base with
    Some p ->
      let x =
        let first_name = p_first_name base p in
        let surname = p_surname base p in
        if Util.accessible_by_key conf base p first_name surname then
          ["pz", code_varenv (Name.lower first_name);
           "nz", code_varenv (Name.lower surname);
           "ocz", string_of_int (get_occ p)]
        else ["iz", string_of_iper (get_iper p)]
      in
      conf.henv <- conf.henv @ x
  | None -> ()
  end;
  begin match p_getenv conf.env "dsrc" with
    Some "" | None -> ()
  | Some s -> conf.henv <- conf.henv @ ["dsrc", code_varenv s]
  end;
  begin match p_getenv conf.env "templ" with
    None -> ()
  | Some s -> conf.henv <- conf.henv @ ["templ", code_varenv s]
  end;
  begin match p_getenv conf.env "escache" with
    Some _ ->
      let v = escache_value base in conf.henv <- conf.henv @ ["escache", v]
  | None -> ()
  end;
  begin match p_getenv conf.env "alwsurn" with
    Some x -> conf.henv <- conf.henv @ ["alwsurn", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "pure_xhtml" with
    Some x -> conf.henv <- conf.henv @ ["pure_xhtml", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "size" with
    Some x -> conf.henv <- conf.henv @ ["size", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "manitou" with
    Some "off" -> conf.henv <- conf.henv @ ["manitou", "off"]
  | Some _ | None -> ()
  end;
  begin match p_getenv conf.env "p_mod" with
    Some x -> conf.henv <- conf.henv @ ["p_mod", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "wide" with
      Some x -> conf.henv <- conf.henv @ [("wide", x)]
    | None -> ()
  end

let set_owner conf =
  if Sys.unix then
    try
      let s = Unix.stat (Util.base_path [] (conf.bname ^ ".gwb")) in
      Unix.setgid s.Unix.st_gid ;
      Unix.setuid s.Unix.st_uid
    with
      Unix.Unix_error (_, _, _) -> ()

let log_count r =
  let thousand oc x = output_string oc @@ Mutil.string_of_int_sep ","  x in
  match r with
    Some (welcome_cnt, request_cnt, start_date) ->
      Log.with_log
        (fun oc ->
           Printf.fprintf oc "  #accesses %a (#welcome %a) since %s\n"
             thousand (welcome_cnt + request_cnt) thousand welcome_cnt
             start_date)
  | None -> ()

let print_moved conf s =
  match Util.open_etc_file "moved" with
    Some ic ->
      let env = ["bname", conf.bname] in
      let conf = {conf with is_printed_by_template = false} in
      Util.html conf; Templ.copy_from_templ conf env ic
  | None ->
      let title _ = Wserver.printf "%s -&gt; %s" conf.bname s in
      Hutil.header_no_page_title conf title;
      Wserver.printf "The database %s has moved to:\n<dl><dt><dd>\n"
        conf.bname;
      Wserver.printf "<a href=\"%s\">" s;
      Wserver.printf "%s" s;
      Wserver.printf "</a>";
      Wserver.printf "\n</dd></dt></dl>\n";
      Hutil.trailer conf

let print_no_index conf base =
  let title _ =
    Wserver.printf "%s" (Utf8.capitalize (transl conf "link to use"))
  in
  let link = url_no_index conf base in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>" ;
  Wserver.printf "<a href=\"http://%s\">\n" link;
  Wserver.printf "%s" link;
  Wserver.printf "</a>\n";
  Wserver.printf "</ul>\n";
  Hutil.print_link_to_welcome conf false;
  Hutil.trailer conf

let treat_request conf base =
  begin match
    p_getenv conf.base_env "moved", p_getenv conf.env "opt",
    p_getenv conf.env "m"
  with
    Some s, _, _ -> print_moved conf s
  | _, Some "no_index", _ -> print_no_index conf base
  | _, _, Some "IM" -> ImageDisplay.print conf base
  | _, _, Some "DOC" ->
      begin match p_getenv conf.env "s" with
        Some f ->
          if Filename.check_suffix f ".txt" then
            let f = Filename.chop_suffix f ".txt" in
            SrcfileDisplay.print_source conf base f
          else ImageDisplay.print conf base
      | None -> Hutil.incorrect_request conf
      end
  | _ ->
      set_owner conf;
      extract_henv conf base;
      make_senv conf base;
      let conf =
        match Util.default_sosa_ref conf base with
          Some p -> {conf with default_sosa_ref = get_iper p, Some p}
        | None -> conf
      in
      if only_special_env conf.env then
        begin
          begin match p_getenv conf.base_env "counter" with
            Some "no" -> ()
          | _ -> let r = SrcfileDisplay.incr_welcome_counter conf in log_count r
          end;
          SrcfileDisplay.print_start conf base
        end
      else
        begin
          begin match p_getenv conf.base_env "counter" with
            Some "no" -> ()
          | _ -> let r = SrcfileDisplay.incr_request_counter conf in log_count r
          end;
          match p_getenv conf.env "ptempl" with
            Some tname when p_getenv conf.base_env "ptempl" = Some "yes" ->
              begin match find_person_in_env conf base "" with
                Some p -> Perso.interp_templ tname conf base p
              | None -> family_m conf base
              end
          | _ -> family_m conf base
        end
  end;
  Wserver.wflush ()

let treat_request_on_possibly_locked_base conf bfile =
  match try Left (Gwdb.open_base bfile) with e -> Right e with
    Left base ->
      (try treat_request conf base with exc -> close_base base; raise exc);
      close_base base
  | Right e ->
      let transl conf w =
        try Hashtbl.find conf.lexicon w with Not_found -> "[" ^ w ^ "]"
      in
      let title _ =
        Wserver.printf "%s" (Utf8.capitalize (transl conf "error"))
      in
      Hutil.rheader conf title;
      Wserver.printf "<ul>";
      Wserver.printf "<li>" ;
      Wserver.printf "%s" (Utf8.capitalize (transl conf "cannot access base"));
      Wserver.printf " \"%s\".</ul>\n" conf.bname;
      begin match e with
        Sys_error _ -> ()
      | _ ->
          Wserver.printf
            "<em><font size=\"-1\">Internal message: %s</font></em>\n"
            (Printexc.to_string e)
      end;
      Hutil.trailer conf

let this_request_updates_database conf =
  match p_getenv conf.env "m" with
    Some ("FORUM_ADD_OK" | "FORUM_DEL" | "FORUM_VAL") -> true
  | Some "API_PRINT_SYNCHRO" -> true
  | Some x when conf.wizard ->
      begin match x with
        "ADD_FAM_OK" | "ADD_IND_OK" | "CHANGE_WIZ_VIS" | "CHG_CHN_OK" |
        "CHG_EVT_IND_ORD_OK" | "CHG_EVT_FAM_ORD_OK" | "CHG_FAM_ORD_OK" |
        "DEL_FAM_OK" | "DEL_IMAGE_OK" | "DEL_IND_OK" | "INV_FAM_OK" |
        "KILL_ANC" | "MOD_FAM_OK" | "MOD_IND_OK" | "MOD_NOTES_OK" |
        "MOD_WIZNOTES_OK" | "MRG_DUP_IND_Y_N" | "MRG_DUP_FAM_Y_N" |
        "MRG_IND" | "MRG_MOD_FAM_OK" | "MRG_MOD_IND_OK" | "MOD_DATA_OK" |
        "SND_IMAGE_OK" ->
          true
      | "API_BASE_WARNINGS" | "API_IMAGE_UPDATE" | "API_REMOVE_IMAGE_EXT" |
        "API_REMOVE_IMAGE_EXT_ALL" | "API_DEL_PERSON_OK" |
        "API_EDIT_PERSON_OK" | "API_ADD_CHILD_OK" | "API_ADD_PERSON_OK" |
        "API_ADD_PARENTS_OK" | "API_ADD_FAMILY_OK" | "API_ADD_FIRST_FAM_OK" |
        "API_EDIT_FAMILY_OK" | "API_DEL_FAMILY_OK" | "API_ADD_SIBLING_OK" |
        "API_ADD_PERSON_START_OK" | "API_PRINT_SYNCHRO" ->
          true
      | _ -> false
      end
  | _ -> false

let treat_request_on_base conf =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  if this_request_updates_database conf then
    Lock.control (Mutil.lock_file bfile) false
      ~onerror:(fun () -> Update.error_locked conf)
      (fun () -> treat_request_on_possibly_locked_base conf bfile)
  else treat_request_on_possibly_locked_base conf bfile

let treat_request_on_nobase conf =
  try family_m_nobase conf; Wserver.wflush () with exc -> raise exc


end
