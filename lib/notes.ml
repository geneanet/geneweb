(* $Id: notes.ml,v 5.33 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

module StrSet = Mutil.StrSet

let file_path conf base fname =
  Util.base_path []
    (List.fold_left Filename.concat (conf.bname ^ ".gwb")
       [base_notes_dir base; fname ^ ".txt"])

let path_of_fnotes fnotes =
  match NotesLinks.check_file_name fnotes with
    Some (dl, f) -> List.fold_right Filename.concat dl f
  | None -> ""

let read_notes base fnotes =
  let fnotes = path_of_fnotes fnotes in
  let s = base_notes_read base fnotes in Wiki.split_title_and_text s

let print_search_form conf from_note =
  Wserver.printf "<table>\n";
  Wserver.printf "<tr>\n";
  Wserver.printf "<td align=\"%s\">\n" conf.right;
  Wserver.printf "<form method=\"get\" action=\"%s\">\n" conf.command;
  Wserver.printf "<p>\n";
  hidden_env conf;
  Wserver.printf
    "<input type=\"hidden\" name=\"m\" value=\"MISC_NOTES_SEARCH\"%s>\n"
    conf.xhs;
  Wserver.printf
    "<input name=\"s\" size=\"30\" maxlength=\"40\" value=\"%s\"%s>\n"
    (match p_getenv conf.env "s" with
       Some s -> Util.escape_html s
     | None -> "")
    conf.xhs;
  begin match from_note with
    Some n ->
      Wserver.printf "<input type=\"hidden\" name=\"z\" value=\"%s\"%s>\n" n
        conf.xhs
  | None -> ()
  end;
  Wserver.printf "<br%s>\n" conf.xhs;
  Wserver.printf "<label>\n";
  Wserver.printf "<input type=\"checkbox\" name=\"c\" value=\"on\"%s%s>\n"
    (match p_getenv conf.env "c" with
       Some "on" -> " checked=\"checked\""
     | Some _ | None -> "")
    conf.xhs;
  Wserver.printf "%s\n" (transl_nth conf "search/case sensitive" 1);
  Wserver.printf "</label>\n";
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
  Wserver.printf "%s" (capitale (transl_nth conf "search/case sensitive" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</p>\n";
  Wserver.printf "</form>\n";
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "</table>\n"

let print_whole_notes conf base fnotes title s ho =
  let title = Util.safe_html title in
  Hutil.header_no_page_title conf
    (fun _ -> Wserver.printf "%s" (if title = "" then fnotes else title));
  let what_links_page () =
    if fnotes <> "" then
      begin
        Wserver.printf "<a href=\"%sm=NOTES&f=%s&ref=on\" class=\"mx-2\">"
          (commd conf) fnotes;
        Wserver.printf "(%s)" (transl conf "linked pages");
        Wserver.printf "</a>\n"
      end
  in
  Hutil.gen_print_link_to_welcome what_links_page conf true;
  if title <> "" then
    begin let title =
            match ho with
            | Some (case_sens, h) -> html_highlight case_sens h title
            | None -> title
      in
      Wserver.printf "<h1>%s</h1>\n" title
    end;
  begin match Util.open_etc_file "summary" with
    | Some ic -> Templ.copy_from_templ conf [] ic
    | None -> ()
  end;
  let file_path = file_path conf base in
  let s = string_with_macros conf [] s in
  let edit_opt = Some (conf.wizard, "NOTES", fnotes) in
  let s =
    let wi =
      {Wiki.wi_mode = "NOTES"; Wiki.wi_file_path = file_path;
       Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_person_exists = person_exists conf base;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    Wiki.html_with_summary_of_tlsw conf wi edit_opt s
  in
  let s = Util.safe_html s in
  let s =
    match ho with
    | Some (case_sens, h) -> html_highlight case_sens h s
    | None -> s
  in
  Wserver.printf "%s\n" s;
  begin match ho with
    Some _ -> print_search_form conf (Some fnotes)
  | None -> ()
  end;
  Hutil.trailer conf

let print_notes_part conf base fnotes title s cnt0 =
  let title = Util.safe_html title in
  Hutil.header_no_page_title conf
    (fun _ -> Wserver.printf "%s" (if title = "" then fnotes else title));
  Hutil.print_link_to_welcome conf true;
  begin match Util.open_etc_file "summary" with
    Some ic -> Templ.copy_from_templ conf [] ic
  | None -> ()
  end;
  if cnt0 = 0 && title <> "" then
    begin
      Wserver.printf "<br%s>\n" conf.xhs;
      Wserver.printf "<br%s>\n" conf.xhs;
      Wserver.printf "<h1>%s</h1>\n" title
    end;
  let s = string_with_macros conf [] s in
  let lines = Wiki.extract_sub_part s cnt0 in
  let mode = "NOTES" in
  let wi =
    {Wiki.wi_mode = mode; Wiki.wi_cancel_links = conf.cancel_links;
     Wiki.wi_file_path = file_path conf base;
     Wiki.wi_person_exists = person_exists conf base;
     Wiki.wi_always_show_link = conf.wizard || conf.friend}
  in
  Wiki.print_sub_part conf wi conf.wizard mode fnotes cnt0 lines; Hutil.trailer conf

let merge_possible_aliases conf db =
  let aliases = Wiki.notes_aliases conf in
  let db =
    List.map
      (fun (pg, (sl, il)) ->
         let pg =
           match pg with
             NotesLinks.PgMisc f ->
               NotesLinks.PgMisc (Wiki.map_notes aliases f)
           | x -> x
         in
         let sl = List.map (Wiki.map_notes aliases) sl in pg, (sl, il))
      db
  in
  let db = List.sort (fun (pg1, _) (pg2, _) -> compare pg1 pg2) db in
  List.fold_left
    (fun list (pg, (sl, il)) ->
       let (sl, _il1, list) =
         let (list1, list2) =
           match list with
             (pg1, _ as x) :: l -> if pg = pg1 then [x], l else [], list
           | [] -> [], list
         in
         match list1 with
           [_, (sl1, il1)] ->
             let sl =
               List.fold_left
                 (fun sl s -> if List.mem s sl then sl else s :: sl) sl sl1
             in
             let il =
               List.fold_left
                 (fun il i -> if List.mem i il then il else i :: il) il il1
             in
             sl, il, list2
         | _ -> sl, il, list
       in
       (pg, (sl, il)) :: list)
    [] db

let notes_links_db conf base eliminate_unlinked =
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  let fname = Filename.concat bdir "notes_links" in
  let db = NotesLinks.read_db_from_file fname in
  let db = merge_possible_aliases conf db in
  let db2 =
    List.fold_left
      (fun db2 (pg, (sl, _il)) ->
         let record_it =
           match pg with
             NotesLinks.PgInd ip ->
               authorized_age conf base (pget conf base ip)
           | NotesLinks.PgFam ifam ->
               authorized_age conf base (pget conf base (get_father @@ foi base ifam))
           | NotesLinks.PgNotes | NotesLinks.PgMisc _ |
             NotesLinks.PgWizard _ ->
               true
         in
         if record_it then
           List.fold_left
             (fun db2 s ->
                try
                  let list = List.assoc s db2 in
                  (s, pg :: list) :: List.remove_assoc s db2
                with Not_found -> (s, [pg]) :: db2)
             db2 sl
         else db2)
      [] db
  in
  (* some kind of basic gc... *)
  let misc = Hashtbl.create 1 in
  let set =
    List.fold_left
      (fun set (pg, (sl, _il)) ->
         match pg with
           NotesLinks.PgInd _ | NotesLinks.PgFam _ | NotesLinks.PgNotes |
           NotesLinks.PgWizard _ ->
             List.fold_left (fun set s -> StrSet.add s set) set sl
         | NotesLinks.PgMisc s -> Hashtbl.add misc s sl; set)
      StrSet.empty db
  in
  let mark = Hashtbl.create 1 in
  begin let rec loop =
    function
      s :: sl ->
        if Hashtbl.mem mark s then loop sl
        else
          begin
            Hashtbl.add mark s ();
            let sl1 = try Hashtbl.find misc s with Not_found -> [] in
            loop (List.rev_append sl1 sl)
          end
    | [] -> ()
  in
    loop (StrSet.elements set)
  end;
  let is_referenced s = Hashtbl.mem mark s in
  let db2 =
    if eliminate_unlinked then
      List.fold_right
        (fun (s, list) db2 ->
           if is_referenced s then (s, list) :: db2 else db2)
        db2 []
    else db2
  in
  List.sort
    (fun (s1, _) (s2, _) -> Gutil.alphabetic_order (Name.lower s1) (Name.lower s2))
    db2

let print_linked_list conf base pgl =
  Wserver.printf "<ul>\n";
  List.iter
    (fun pg ->
       Wserver.printf "<li>";
       begin match pg with
         NotesLinks.PgInd ip ->
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf "<a  href=\"%s&i=%s&\">"
                 (commd conf) (Gwdb.string_of_iper ip);
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           begin
             let p = pget conf base ip in
             Wserver.printf "<span class=\"mx-2\">";
             Wserver.printf "%s%s"
               (Util.referenced_person_title_text conf base p)
               (DateDisplay.short_dates_text conf base p);
             Wserver.printf "</span>"
           end;
           Wserver.printf "</tt>\n"
       | NotesLinks.PgFam ifam ->
           let fam = foi base ifam in
           let fath = pget conf base (get_father fam) in
           let moth = pget conf base (get_mother fam) in
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf
                 "<a class=\"mx-2\" href=\"%sm=MOD_FAM&i=%s&ip=%s&\">"
                 (commd conf) (Gwdb.string_of_ifam ifam)
                 (Gwdb.string_of_iper (Gwdb.get_iper fath));
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           Wserver.printf "<span class=\"mx-2\">";
           Wserver.printf "%s%s &amp; %s %s"
             (Util.referenced_person_title_text conf base fath)
             (DateDisplay.short_dates_text conf base fath)
             (Util.referenced_person_title_text conf base moth)
             (DateDisplay.short_dates_text conf base moth);
           Wserver.printf "</span>";
           Wserver.printf "</tt>\n"
       | NotesLinks.PgNotes ->
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf "<a class=\"mx-2\" href=\"%sm=MOD_NOTES&\">"
                 (commd conf);
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           Wserver.printf "<a class=\"mx-2\" href=\"%sm=NOTES\">"
             (commd conf);
           Wserver.printf "%s" (transl_nth conf "note/notes" 1);
           Wserver.printf "</a>\n";
           Wserver.printf "</tt>\n"
       | NotesLinks.PgMisc fnotes ->
           let (nenv, _) = read_notes base fnotes in
           let title = try List.assoc "TITLE" nenv with Not_found -> "" in
           let title = Util.safe_html title in
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf
                 "<a class=\"mx-2\" href=\"%sm=MOD_NOTES&f=%s&\">"
                 (commd conf) fnotes;
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           Wserver.printf "<a class=\"mx-2\" href=\"%sm=NOTES&f=%s&\">"
             (commd conf) fnotes;
           Wserver.printf "%s" fnotes;
           Wserver.printf "</a>";
           if title <> "" then Wserver.printf "(%s)" title;
           Wserver.printf "</tt>\n"
       | NotesLinks.PgWizard wizname ->
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf
                 "<a class=\"mx-2\" href=\"%sm=MOD_WIZNOTES&f=%s&\">"
                 (commd conf) (code_varenv wizname);
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           Wserver.printf "<a class=\"mx-2\" href=\"%sm=WIZNOTES&v=%s\">"
             (commd conf) (code_varenv wizname);
           Wserver.printf "%s" wizname;
           Wserver.printf "</a>";
           Wserver.printf "<i>";
           Wserver.printf "(%s)"
             (transl_nth conf "wizard/wizards/friend/friends/exterior" 0);
           Wserver.printf "</i>";
           Wserver.printf "</tt>\n"
       end;
       Wserver.printf "</li>\n")
    pgl;
  Wserver.printf "</ul>\n"

let print_what_links conf base fnotes =
  let title h =
    Wserver.printf "%s " (capitale (transl conf "linked pages"));
    if h then Wserver.printf "[%s]" fnotes
    else
      begin
        Wserver.printf "<tt>";
        Wserver.printf "[";
        begin
          Wserver.printf "<a href=\"%sm=NOTES&f=%s\">" (commd conf) fnotes;
          Wserver.printf "%s" fnotes;
          Wserver.printf "</a>"
        end;
        Wserver.printf "]";
        Wserver.printf "</tt>"
      end
  in
  let db = notes_links_db conf base false in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  begin match (try Some (List.assoc fnotes db) with Not_found -> None) with
    Some pgl -> print_linked_list conf base pgl
  | None -> ()
  end;
  Hutil.trailer conf

let print conf base =
  let fnotes =
    match p_getenv conf.env "f" with
      Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  match p_getenv conf.env "ref" with
    Some "on" -> print_what_links conf base fnotes
  | _ ->
      let (nenv, s) = read_notes base fnotes in
      let title = try List.assoc "TITLE" nenv with Not_found -> "" in
      let title = Util.safe_html title in
      match p_getint conf.env "v" with
        Some cnt0 -> print_notes_part conf base fnotes title s cnt0
      | None -> print_whole_notes conf base fnotes title s None

let print_mod conf base =
  let fnotes =
    match p_getenv conf.env "f" with
      Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let title _ =
    Wserver.printf "%s - %s%s" (capitale (transl conf "base notes"))
      conf.bname (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  let (env, s) = read_notes base fnotes in
  Wiki.print_mod_view_page conf true "NOTES" fnotes title env s

let update_notes_links_db conf fnotes s =
  let slen = String.length s in
  let (list_nt, list_ind) =
    let rec loop list_nt list_ind pos i =
      if i = slen then list_nt, list_ind
      else if i + 1 < slen && s.[i] = '%' then
        loop list_nt list_ind pos (i + 2)
      else
        match NotesLinks.misc_notes_link s i with
          NotesLinks.WLpage (j, _, lfname, _, _) ->
            let list_nt =
              if List.mem lfname list_nt then list_nt else lfname :: list_nt
            in
            loop list_nt list_ind pos j
        | NotesLinks.WLperson (j, key, _, txt) ->
            let list_ind =
              let link = {NotesLinks.lnTxt = txt; NotesLinks.lnPos = pos} in
              (key, link) :: list_ind
            in
            loop list_nt list_ind (pos + 1) j
        | NotesLinks.WLwizard (j, _, _) -> loop list_nt list_ind pos j
        | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1)
    in
    loop [] [] 1 0
  in
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  NotesLinks.update_db bdir fnotes (list_nt, list_ind)

let commit_notes conf base fnotes s =
  let pg =
    if fnotes = "" then NotesLinks.PgNotes else NotesLinks.PgMisc fnotes
  in
  let fname = path_of_fnotes fnotes in
  let fpath =
    List.fold_left Filename.concat (Util.base_path [] (conf.bname ^ ".gwb"))
      [base_notes_dir base; fname]
  in
  Mutil.mkdir_p (Filename.dirname fpath);
  Gwdb.commit_notes base fname s ;
  History.record conf base (Def.U_Notes (p_getint conf.env "v", fnotes)) "mn";
  update_notes_links_db conf pg s

let print_mod_ok conf base =
  let fname =
    function
      Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let edit_mode _ = if conf.wizard then Some "NOTES" else None in
  let mode = "NOTES" in
  let read_string = read_notes base in
  let commit = commit_notes conf base in
  let string_filter = string_with_macros conf [] in
  let file_path = file_path conf base in
  let wi =
    {Wiki.wi_mode = mode; Wiki.wi_cancel_links = conf.cancel_links;
     Wiki.wi_file_path = file_path;
     Wiki.wi_person_exists = person_exists conf base;
     Wiki.wi_always_show_link = conf.wizard || conf.friend}
  in
  Wiki.print_mod_ok conf wi edit_mode fname read_string commit string_filter
    true

let begin_text_without_html_tags lim s =
  let rec loop i size len =
    if i >= String.length s then Buff.get len
    else if size > lim && String.length s > i + 3 then Buff.get len ^ "..."
    else if s.[i] = '<' then
      let i =
        let rec loop i =
          if i = String.length s then i
          else if s.[i] = '>' then i + 1
          else loop (i + 1)
        in
        loop (i + 1)
      in
      loop i size len
    else if s.[i] = '=' then loop (i + 1) size len
    else
      let nbc = Name.nbc s.[i] in
      loop (i + nbc) (size + 1) (Buff.mstore len (String.sub s i nbc))
  in
  loop 0 0 0

let print_misc_notes conf base =
  let d =
    match p_getenv conf.env "d" with
      Some d -> d
    | None -> ""
  in
  let title h =
    Wserver.printf "%s"
      (if d = "" then
         capitale (Util.translate_eval (transl conf "miscellaneous notes"))
       else if h then "- " ^ d ^ " -"
       else "<tt>- " ^ d ^ " -</tt>")
  in
  let db = notes_links_db conf base true in
  let db =
    List.fold_right
      (fun (f, _) list ->
         if String.length f >= String.length d then
           if String.sub f 0 (String.length d) = d then
             let r =
               String.sub f (String.length d)
                 (String.length f - String.length d)
             in
             if d = "" || r <> "" && r.[0] = NotesLinks.char_dir_sep then
               let r =
                 if d = "" then r else String.sub r 1 (String.length r - 1)
               in
               try
                 let i = String.index r NotesLinks.char_dir_sep in
                 let r = String.sub r 0 i in
                 match list with
                   (r', None) :: _ when r = r' -> list
                 | _ -> (r, None) :: list
               with Not_found -> (r, Some f) :: list
             else list
           else list
         else list)
      db []
  in
  Hutil.header_link_welcome conf title;
  if db <> [] then
    begin
      Wserver.printf "<ul>\n";
      if d <> "" then
        begin
          Wserver.printf "<li class=\"parent\">\n";
          begin
            Wserver.printf "<a href=\"%sm=MISC_NOTES%s\">" (commd conf)
              (try
                 let i = String.rindex d NotesLinks.char_dir_sep in
                 let d = String.sub d 0 i in "&d=" ^ d
               with Not_found -> "");
            Wserver.printf "<tt>&lt;--</tt>";
            Wserver.printf "</a>"
          end;
          Wserver.printf "</li>\n"
        end;
      List.iter
        (fun (r, f) ->
           match f with
             Some f ->
               let txt =
                 let (n, s) = read_notes base f in
                 let t = try List.assoc "TITLE" n with Not_found -> "" in
                 if t <> "" then t
                 else if s = "" then ""
                 else "<em>" ^ begin_text_without_html_tags 50 s ^ "</em>"
               in
               let c =
                 let f = file_path conf base (path_of_fnotes f) in
                 if Sys.file_exists f then "" else " style=\"color:red\""
               in
               Wserver.printf "<li class=\"file\">\n";
               Wserver.printf "<tt>[";
               Wserver.printf "<a href=\"%sm=NOTES&f=%s\"%s>" (commd conf) f
                 c;
               Wserver.printf "%s" r;
               Wserver.printf "</a>";
               Wserver.printf "]</tt>%s\n"
                 (if txt = "" then "" else " : " ^ txt);
               Wserver.printf "</li>\n"
           | None ->
               Wserver.printf "<li class=\"folder\">\n";
               Wserver.printf "<tt>";
               Wserver.printf "<a href=\"%sm=MISC_NOTES&d=%s\">" (commd conf)
                 (if d = "" then r
                  else d ^ String.make 1 NotesLinks.char_dir_sep ^ r);
               Wserver.printf "%s " r;
               Wserver.printf "--&gt;";
               Wserver.printf "</a>";
               Wserver.printf "</tt>";
               Wserver.printf "</li>\n")
        db;
      Wserver.printf "</ul>\n"
    end;
  if d = "" then print_search_form conf None;
  Hutil.trailer conf

(* searching *)

let search_text conf base s =
  let s = if s = "" then " " else s in
  let case_sens = p_getenv conf.env "c" = Some "on" in
  let db =
    let db = notes_links_db conf base true in
    let db = "" :: List.map fst db in
    match p_getenv conf.env "z" with
      None -> db
    | Some f ->
        let rec loop =
          function
            fnotes :: list -> if f = fnotes then list else loop list
          | [] -> []
        in
        loop db
  in
  let noteo =
    let rec loop =
      function
        fnotes :: list ->
          let (nenv, nt) = read_notes base fnotes in
          let tit = try List.assoc "TITLE" nenv with Not_found -> "" in
          if in_text case_sens s tit || in_text case_sens s nt then
            Some (fnotes, tit, nt)
          else loop list
      | [] -> None
    in
    loop db
  in
  match noteo with
    Some (fnotes, tit, nt) ->
      print_whole_notes conf base fnotes tit nt (Some (case_sens, s))
  | None -> print_misc_notes conf base

let print_misc_notes_search conf base =
  match try Some (List.assoc "s" conf.env) with Not_found -> None with
    Some s -> search_text conf base (Wserver.gen_decode false s)
  | None -> print_misc_notes conf base
