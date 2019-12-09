(* $Id: wiznotes.ml,v 5.54 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util

let dir conf base =
  Filename.concat (Util.base_path [] (conf.bname ^ ".gwb"))
    (Gwdb.base_wiznotes_dir base)

let wzfile wddir wz = Filename.concat wddir (wz ^ ".txt")

let read_auth_file fname =
  let data = read_gen_auth_file fname in
  List.map
    (fun au ->
       let wizname =
         try
           let k = String.index au.au_info ':' in String.sub au.au_info 0 k
         with Not_found -> au.au_user
       in
       let (wizname, wizorder, islash) =
         try
           let i = String.index wizname '/' in
           let w1 = String.sub wizname 0 i in
           let l = String.length wizname in
           let w2 = String.sub wizname (i + 1) (l - i - 1) in
           w1 ^ w2, w2 ^ w1, i
         with Not_found -> wizname, wizname, 0
       in
       au.au_user, (wizname, (wizorder, islash)))
    data

let read_wizard_notes fname =
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
    Some ic ->
      let (date, len) =
        try
          let line = input_line ic in
          if line = "WIZNOTES" then
            let line = input_line ic in float_of_string line, 0
          else
            let s = Unix.stat fname in
            s.Unix.st_mtime, Buff.store (Buff.mstore 0 line) '\n'
        with End_of_file | Failure _ -> 0., 0
      in
      let rec loop len =
        match try Some (input_char ic) with End_of_file -> None with
          Some c -> loop (Buff.store len c)
        | None -> close_in ic; len
      in
      let len = loop len in Buff.get len, date
  | None -> "", 0.

let write_wizard_notes fname nn =
  if nn = "" then Mutil.rm fname
  else
    match try Some (Secure.open_out fname) with Sys_error _ -> None with
      Some oc ->
        Printf.fprintf oc "WIZNOTES\n%.0f\n" (Unix.time ());
        output_string oc nn;
        output_string oc "\n";
        close_out oc
    | None -> ()

let wiznote_date wfile =
  match try Some (Secure.open_in wfile) with Sys_error _ -> None with
    Some ic ->
      let date =
        try
          let line = input_line ic in
          if line = "WIZNOTES" then float_of_string (input_line ic)
          else raise Exit
        with End_of_file | Failure _ | Exit ->
          let s = Unix.stat wfile in s.Unix.st_mtime
      in
      close_in ic; wfile, date
  | None -> "", 0.

let print_wizards_by_alphabetic_order conf list =
  let wprint_elem (wz, (wname, (_, islash)), wfile, stm) =
    let tm = Unix.localtime stm in
    let with_link =
      conf.wizard && conf.user = wz || wfile <> "" || conf.manitou
    in
    if with_link then
      Wserver.printf "<a href=\"%sm=WIZNOTES&f=%s%t\">" (commd conf)
        (Util.code_varenv wz)
        (fun _ ->
           Printf.sprintf "&d=%d-%02d-%02d,%02d:%02d:%02d"
             (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
             tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec);
    if islash > 0 then
      let s1 =
        let islash = if wname.[islash-1] = ' ' then islash - 1 else islash in
        String.sub wname 0 islash
      in
      let s2 = String.sub wname islash (String.length wname - islash) in
      Wserver.printf "%s (%s)" s2 s1
    else Wserver.printf "%s" wname;
    if with_link then Wserver.printf "</a>"
  in
  let order (_, (_, (ord, _)), _, _) = ord in
  wprint_in_columns conf order wprint_elem list

let print_wizards_by_date conf list =
  let sep_period_list =
    [(fun tm -> tm.Unix.tm_mon),
     (fun tm ->
        let dmy =
          {year = tm.Unix.tm_year + 1900; month = tm.Unix.tm_mon + 1; day = 0;
           prec = Sure; delta = 0}
        in
        Wserver.printf "%s"
          (Utf8.capitalize (DateDisplay.string_of_ondate conf (Dgreg (dmy, Dgregorian)))));
     (fun tm -> tm.Unix.tm_year),
     (fun tm -> Wserver.printf "%d" (tm.Unix.tm_year + 1900))]
  in
  let list =
    List.sort (fun (_, _, _, mtm1) (_, _, _, mtm2) -> compare mtm2 mtm1) list
  in
  Wserver.printf "<dl>\n<dt>";
  let _ =
    List.fold_left
      (fun (spl, prev) (wz, (wname, _), wfile, stm) ->
         let tm = Unix.localtime stm in
         let (new_item, spl) =
           match prev with
             Some prev_tm ->
               let (sep_period, _) =
                 match spl with
                   sp :: _ -> sp
                 | [] -> assert false
               in
               if sep_period tm <> sep_period prev_tm then
                 begin
                   Wserver.printf "</dd>\n<dt>";
                   let spl =
                     match spl with
                       _ :: (next_sp, _) :: _ ->
                         if next_sp tm <> next_sp prev_tm then List.tl spl
                         else spl
                     | _ -> spl
                   in
                   true, spl
                 end
               else false, spl
           | None -> true, spl
         in
         if new_item then
           if stm = 0.0 then Wserver.printf "....."
           else
             begin match spl with
               (_, disp_sep_period) :: _ -> disp_sep_period tm
             | [] -> ()
             end;
         if new_item then Wserver.printf "</dt>\n<dd>\n";
         let wname = if wname = "" then wz else wname in
         Wserver.printf "%s%t" (if prev = None || new_item then "" else ",\n")
           (fun _ ->
              if conf.wizard && conf.user = wz || wfile <> "" then
                Printf.sprintf "<a href=\"%sm=WIZNOTES&f=%s%t\">%s</a>"
                  (commd conf) (Util.code_varenv wz)
                  (fun _ ->
                     Printf.sprintf "&d=%d-%02d-%02d,%02d:%02d:%02d"
                       (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
                       tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
                       tm.Unix.tm_sec)
                  wname
              else wname);
         spl, Some tm)
      (sep_period_list, None) list
  in
  (); Wserver.printf "</dd></dl>\n"

let print_old_wizards conf list =
  if list = [] then ()
  else
    begin
      Wserver.printf "<dl>\n";
      begin
        Wserver.printf "<dd style=\"list-style-type:circle\">\n";
        Wserver.printf "%s..." (transl_nth conf "and" 0);
        begin
          Wserver.printf "<dl>\n";
          begin
            Wserver.printf "<dd>\n";
            Mutil.list_iter_first
              (fun first wz ->
                 if not first then Wserver.printf ",\n";
                 Wserver.printf "<a href=\"%sm=WIZNOTES&f=%s\">" (commd conf)
                   (Util.code_varenv wz);
                 for i = 0 to String.length wz - 1 do
                   if wz.[i] = ' ' then Wserver.printf "&nbsp;"
                   else Wserver.printf "%c" wz.[i]
                 done;
                 Wserver.printf "</a>")
              list;
            Wserver.printf "\n";
            Wserver.printf "</dd>\n"
          end;
          Wserver.printf "</dl>\n"
        end;
        Wserver.printf "</dd>\n"
      end;
      Wserver.printf "</dl>\n"
    end

let wizard_list_from_dir conf base =
  match try Some (Sys.readdir (dir conf base)) with Sys_error _ -> None with
    Some arr ->
      List.fold_left
        (fun list fname ->
           if Filename.check_suffix fname ".txt" then
             let n = Filename.chop_extension fname in n :: list
           else list)
        [] (Array.to_list arr)
  | None -> []

let print_search_form conf from_wiz =
  Wserver.printf "<table>\n";
  Wserver.printf "<tr>\n";
  Wserver.printf "<td align=\"%s\">\n" conf.right;
  Wserver.printf "<form method=\"get\" action=\"%s\">\n" conf.command;
  Wserver.printf "<p>\n";
  hidden_env conf;
  Wserver.printf
    "<input type=\"hidden\" name=\"m\" value=\"WIZNOTES_SEARCH\"%s>\n"
    conf.xhs;
  Wserver.printf
    "<input name=\"s\" size=\"30\" maxlength=\"40\" value=\"%s\"%s>\n"
    (match p_getenv conf.env "s" with
       Some s -> Util.escape_html s
     | None -> "")
    conf.xhs;
  if from_wiz <> "" then
    Wserver.printf "<input type=\"hidden\" name=\"z\" value=\"%s\"%s>\n"
      from_wiz conf.xhs;
  Wserver.printf "<br%s>\n" conf.xhs;
  Wserver.printf "<label>\n";
  Wserver.printf "<input type=\"checkbox\" name=\"c\" value=\"on\"%s%s>\n"
    (match p_getenv conf.env "c" with
       Some "on" -> " checked=\"checked\""
     | Some _ | None -> "")
    conf.xhs;
  Wserver.printf "%s\n" (transl_nth conf "search/case sensitive" 1);
  Wserver.printf "</label>\n";
  Wserver.printf "<input type=\"submit\" value=\"%s\"%s>\n"
    (Utf8.capitalize (transl_nth conf "search/case sensitive" 0)) conf.xhs;
  Wserver.printf "</p>\n";
  Wserver.printf "</form>\n";
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "</table>\n"

let print_main conf base auth_file =
  let wiztxt =
    Util.translate_eval
      (transl_nth conf "wizard/wizards/friend/friends/exterior" 1)
  in
  let title _ =
    Wserver.printf "%s - %s" (Utf8.capitalize wiztxt)
      (Util.translate_eval (transl_nth conf "note/notes" 1))
  in
  let by_alphab_order = p_getenv conf.env "o" <> Some "H" in
  let wizdata =
    let list = read_auth_file auth_file in
    if by_alphab_order then
      List.sort
        (fun (_, (_, (o1, _))) (_, (_, (o2, _))) ->
           Gutil.alphabetic_order o1 o2)
        list
    else list
  in
  let wddir = dir conf base in
  Hutil.header_no_page_title conf title;
  (* mouais... *)
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<h1>";
  title false;
  Wserver.printf "</h1>\n";
  let list =
    List.map
      (fun (wz, wname) ->
         let (wfile, wnote) = wiznote_date (wzfile wddir wz) in
         wz, wname, wfile, wnote)
      wizdata
  in
  let old_list =
    let list = wizard_list_from_dir conf base in
    List.filter (fun n -> not (List.mem_assoc n wizdata)) list
  in
  if by_alphab_order then
    begin
      begin
        Wserver.printf "<p>\n";
        Wserver.printf "%d %s<br%s>\n" (List.length wizdata) wiztxt conf.xhs;
        Wserver.printf "<em style=\"font-size:80%%\">\n";
        Wserver.printf "%s " (Utf8.capitalize (transl conf "click"));
        Wserver.printf "<a href=\"%sm=WIZNOTES&o=H\">%s</a>\n" (commd conf)
          (transl conf "here");
        Wserver.printf "%s"
          (transl conf
             "for the list ordered by the date of the last modification");
        Wserver.printf ".</em>\n";
        Wserver.printf "</p>\n"
      end;
      print_wizards_by_alphabetic_order conf list
    end
  else
    begin
      begin
        Wserver.printf "<p>\n";
        Wserver.printf "%d %s\n" (List.length wizdata) wiztxt;
        Wserver.printf "</p>\n"
      end;
      print_wizards_by_date conf list
    end;
  if by_alphab_order then
    begin print_old_wizards conf old_list; print_search_form conf "" end;
  Hutil.trailer conf

let wizard_page_title wizname _ = Wserver.printf "%s" wizname

let print_whole_wiznote conf base auth_file wz wfile (s, date) ho =
  let wizname =
    let wizdata = read_auth_file auth_file in
    try fst (List.assoc wz wizdata) with Not_found -> wz
  in
  let edit_opt =
    let can_edit = conf.wizard && conf.user = wz || conf.manitou in
    Some (can_edit, "WIZNOTES", code_varenv wz)
  in
  let title = wizard_page_title wizname in
  Hutil.header_no_page_title conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<h1>";
  title false;
  Wserver.printf "</h1>\n";
  begin match Util.open_etc_file "summary" with
    Some ic -> Templ.copy_from_templ conf [] ic
  | None -> ()
  end;
  Wserver.printf "<table border=\"0\" width=\"100%%\">\n";
  Wserver.printf "<tr>\n";
  Wserver.printf "<td>\n";
  begin let s = Util.safe_html @@ string_with_macros conf [] s in
    let s =
      let wi =
        {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
         Wiki.wi_file_path = Notes.file_path conf base;
         Wiki.wi_person_exists = person_exists conf base;
         Wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      Wiki.html_with_summary_of_tlsw conf wi edit_opt s
    in
    let s =
      match ho with
        Some (case_sens, h) -> html_highlight case_sens h s
      | None -> s
    in
    Wserver.printf "%s\n" (Util.safe_html @@ if conf.pure_xhtml then Util.check_xhtml s else s)
  end;
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "</table>\n";
  if Sys.file_exists wfile then
    begin let tm = Unix.localtime date in
      let dmy =
        {day = tm.Unix.tm_mday; month = tm.Unix.tm_mon + 1;
         year = 1900 + tm.Unix.tm_year; prec = Sure; delta = 0}
      in
      Wserver.printf "<p>\n";
      Wserver.printf "<tt>(%s %02d:%02d)</tt>\n"
        (DateDisplay.string_of_ondate conf (Dgreg (dmy, Dgregorian))) tm.Unix.tm_hour
        tm.Unix.tm_min;
      Wserver.printf "</p>\n"
    end;
  begin match p_getenv conf.env "m" with
    Some "WIZNOTES_SEARCH" -> print_search_form conf wz
  | Some _ | None -> ()
  end;
  Hutil.trailer conf

let print_part_wiznote conf base wz s cnt0 =
  let title = wz in
  Hutil.header_no_page_title conf (fun _ -> Wserver.printf "%s" title);
  let s = Util.safe_html @@ string_with_macros conf [] s in
  let lines = Wiki.extract_sub_part s cnt0 in
  let lines = if cnt0 = 0 then title :: "<br /><br />" :: lines else lines in
  let file_path = Notes.file_path conf base in
  let can_edit = conf.wizard && conf.user = wz || conf.manitou in
  let wi =
    {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
     Wiki.wi_file_path = file_path;
     Wiki.wi_person_exists = person_exists conf base;
     Wiki.wi_always_show_link = conf.wizard || conf.friend}
  in
  Wiki.print_sub_part conf wi can_edit "WIZNOTES" (code_varenv wz) cnt0 lines;
  Hutil.trailer conf

let wizard_auth_file_name conf =
  match
    p_getenv conf.base_env "wizard_descr_file",
    p_getenv conf.base_env "wizard_passwd_file"
  with
    (Some "" | None), (Some "" | None) -> ""
  | Some auth_file, _ -> auth_file
  | _, Some auth_file -> auth_file

let print conf base =
  let auth_file = wizard_auth_file_name conf in
  if auth_file = "" then Hutil.incorrect_request conf
  else
    let f =
      (* backward compatibility *)
      match p_getenv conf.env "f" with
        None -> p_getenv conf.env "v"
      | x -> x
    in
    match f with
      Some wz ->
        let wz = Filename.basename wz in
        let wfile = wzfile (dir conf base) wz in
        let (s, date) = read_wizard_notes wfile in
        begin match p_getint conf.env "v" with
          Some cnt0 -> print_part_wiznote conf base wz s cnt0
        | None ->
            print_whole_wiznote conf base auth_file wz wfile (s, date) None
        end
    | None -> print_main conf base auth_file

let print_mod conf base =
  let auth_file =
    match
      p_getenv conf.base_env "wizard_descr_file",
      p_getenv conf.base_env "wizard_passwd_file"
    with
      (Some "" | None), (Some "" | None) -> ""
    | Some auth_file, _ -> auth_file
    | _, Some auth_file -> auth_file
  in
  if auth_file = "" then Hutil.incorrect_request conf
  else
    match p_getenv conf.env "f" with
      Some wz ->
        let wz = Filename.basename wz in
        let can_edit = conf.wizard && conf.user = wz || conf.manitou in
        if can_edit then
          let title = wizard_page_title wz in
          let wfile = wzfile (dir conf base) wz in
          let (s, _) = read_wizard_notes wfile in
          Wiki.print_mod_view_page conf true "WIZNOTES" wz title [] s
        else Hutil.incorrect_request conf
    | None -> Hutil.incorrect_request conf

let print_view conf base =
  let auth_file =
    match
      p_getenv conf.base_env "wizard_descr_file",
      p_getenv conf.base_env "wizard_passwd_file"
    with
      (Some "" | None), (Some "" | None) -> ""
    | Some auth_file, _ -> auth_file
    | _, Some auth_file -> auth_file
  in
  if auth_file = "" then Hutil.incorrect_request conf
  else
    match p_getenv conf.env "f" with
      Some wz ->
        let wz = Filename.basename wz in
        let title = wizard_page_title wz in
        let wfile = wzfile (dir conf base) wz in
        let (s, _) = read_wizard_notes wfile in
        Wiki.print_mod_view_page conf false "WIZNOTES" wz title [] s
    | None -> Hutil.incorrect_request conf

let commit_wiznotes conf base wz s =
  let wddir = dir conf base in
  let fname = wzfile wddir wz in
  (try Unix.mkdir wddir 0o755 with Unix.Unix_error (_, _, _) -> ());
  write_wizard_notes fname s;
  let pg = NotesLinks.PgWizard wz in Notes.update_notes_links_db conf pg s

let print_mod_ok conf base =
  let auth_file =
    match
      p_getenv conf.base_env "wizard_descr_file",
      p_getenv conf.base_env "wizard_passwd_file"
    with
      (Some "" | None), (Some "" | None) -> ""
    | Some auth_file, _ -> auth_file
    | _, Some auth_file -> auth_file
  in
  if auth_file = "" then Hutil.incorrect_request conf
  else
    let fname =
      function
        Some f -> f
      | None -> "nobody"
    in
    let edit_mode wz =
      if conf.wizard && conf.user = wz || conf.manitou then Some "WIZNOTES"
      else None
    in
    let mode = "NOTES" in
    let read_string wz =
      [], fst (read_wizard_notes (wzfile (dir conf base) wz))
    in
    let commit = commit_wiznotes conf base in
    let string_filter s = Util.safe_html @@ string_with_macros conf [] s in
    let file_path = Notes.file_path conf base in
    let wi =
      {Wiki.wi_mode = mode; Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_file_path = file_path;
       Wiki.wi_person_exists = person_exists conf base;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    Wiki.print_mod_ok conf wi edit_mode fname read_string commit string_filter
      false

let wizard_denying wddir =
  let fname = Filename.concat wddir "connected.deny" in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
    Some ic ->
      let rec loop list =
        match try Some (input_line ic) with End_of_file -> None with
          Some wname -> loop (wname :: list)
        | None -> close_in ic; List.rev list
      in
      loop []
  | None -> []

let print_connected_wizard conf first wddir wz tm_user =
  let (wfile, stm) = wiznote_date (wzfile wddir wz) in
  let tm = Unix.localtime stm in
  if wfile <> "" then
    begin
      Wserver.printf "<a href=\"%sm=WIZNOTES&f=%s%t\">" (commd conf)
        (Util.code_varenv wz)
        (fun _ ->
           Printf.sprintf "&d=%d-%02d-%02d,%02d:%02d:%02d"
             (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
             tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec);
      Wserver.printf "%s" wz;
      Wserver.printf "</a>"
    end
  else Wserver.printf "%s" wz;
  Wserver.printf " ";
  Wserver.printf
    "<a href=\"%sm=HIST&k=20&wiz=%s\" style=\"text-decoration:none\">"
    (commd conf) (Util.code_varenv wz);
  Wserver.printf "(*)";
  Wserver.printf "</a>";
  let d = conf.ctime -. tm_user in
  if d = 0.0 then ()
  else
    begin
      Wserver.printf " - %.0f s" d;
      if first then
        begin
          Wserver.printf " ";
          begin
            Wserver.printf "<span style=\"font-size:80%%\">";
            Wserver.printf "(%s)" (transl conf "since the last click");
            Wserver.printf "</span>"
          end
        end
    end

let do_connected_wizards conf base (_, _, _, wl) =
  let title _ =
    Wserver.printf "%s"
      (Utf8.capitalize (transl_nth conf "wizard/wizards/friend/friends/exterior" 1))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  let wddir = dir conf base in
  let denying = wizard_denying wddir in
  let wl =
    if not (List.mem_assoc conf.user wl) then (conf.user, conf.ctime) :: wl
    else wl
  in
  let wl = List.sort (fun (_, tm1) (_, tm2) -> compare tm1 tm2) wl in
  let is_visible = not (List.mem conf.user denying) in
  Wserver.printf "<ul>\n";
  begin let (not_everybody, _) =
    List.fold_left
      (fun (not_everybody, first) (wz, tm_user) ->
         if wz <> conf.user && List.mem wz denying && not conf.manitou then
           true, first
         else
           begin
             begin
               Wserver.printf "<li style=\"list-style-type:%s\">\n"
                 (if wz = conf.user && not is_visible ||
                     conf.manitou && List.mem wz denying
                  then
                    "circle"
                  else "disc");
               print_connected_wizard conf first wddir wz tm_user;
               if wz = conf.user then
                 begin
                   Wserver.printf " :\n%s;"
                     (transl_nth conf "you are visible/you are not visible"
                        (if is_visible then 0 else 1));
                   Wserver.printf " %s %s%s%s %s" (transl conf "click")
                     (Printf.sprintf "<a href=\"%sm=CHANGE_WIZ_VIS&v=%d\">"
                        (commd conf) (if is_visible then 0 else 1))
                     (transl conf "here") "</a>" (transl conf "to change");
                   Wserver.printf "."
                 end;
               Wserver.printf "\n";
               Wserver.printf "</li>\n"
             end;
             not_everybody, false
           end)
      (false, true) wl
  in
    if not_everybody then
      begin
        Wserver.printf "<li>\n";
        Wserver.printf "...";
        Wserver.printf "</li>\n"
      end
  end;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let connected_wizards conf base =
  match conf.n_connect with
    Some x -> do_connected_wizards conf base x
  | None -> Hutil.incorrect_request conf

let do_change_wizard_visibility conf base x set_vis =
  let wddir = dir conf base in
  let denying = wizard_denying wddir in
  let is_visible = not (List.mem conf.user denying) in
  if not set_vis && not is_visible || set_vis && is_visible then ()
  else
    begin let tmp_file = Filename.concat wddir "1connected.deny" in
      let oc = Secure.open_out tmp_file in
      let found =
        List.fold_left
          (fun found wz ->
             if wz = conf.user && set_vis then true
             else begin Printf.fprintf oc "%s\n" wz; found end)
          false denying
      in
      if not found && not set_vis then Printf.fprintf oc "%s\n" conf.user;
      close_out oc;
      let file = Filename.concat wddir "connected.deny" in
      Mutil.remove_file file; Sys.rename tmp_file file
    end;
  do_connected_wizards conf base x

let change_wizard_visibility conf base =
  match conf.n_connect, p_getint conf.env "v" with
    Some x, Some vis -> do_change_wizard_visibility conf base x (vis <> 0)
  | _ -> Hutil.incorrect_request conf

(* searching *)

let search_text conf base s =
  let s = if s = "" then " " else s in
  let case_sens = p_getenv conf.env "c" = Some "on" in
  let list =
    let list = wizard_list_from_dir conf base in
    let list = List.sort compare list in
    match p_getenv conf.env "z" with
      Some "" | None -> list
    | Some wz ->
        let rec loop =
          function
            wz1 :: list -> if wz = wz1 then list else loop list
          | [] -> []
        in
        loop list
  in
  let wizo =
    let rec loop =
      function
        [] -> None
      | wz :: list ->
          let wz = Filename.basename wz in
          let wfile = wzfile (dir conf base) wz in
          let (nt, dt) = read_wizard_notes wfile in
          if in_text case_sens s nt then Some (wz, wfile, nt, dt)
          else loop list
    in
    loop list
  in
  match wizo with
    Some (wz, wf, nt, dt) ->
      let auth_file = wizard_auth_file_name conf in
      print_whole_wiznote conf base auth_file wz wf (nt, dt)
        (Some (case_sens, s))
  | None -> print conf base

let print_search conf base =
  match try Some (List.assoc "s" conf.env) with Not_found -> None with
    Some s -> search_text conf base (Wserver.gen_decode false s)
  | None -> print conf base
