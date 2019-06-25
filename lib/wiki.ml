(* $Id: wiki.ml,v 5.31 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util

(* TLSW: Text Language Stolen to Wikipedia
   = title level 1 =
   == title level 2 ==
   ...
   ====== title level 6 ======
   * list ul/li item
   * list ul/li item
   ** list ul/li item 2nd level
   ** list ul/li item 2nd level
   ...
   # list ol/li item
   : indentation list dl/dd item
   ; list dl dt item ; dd item
   ''italic''
   '''bold'''
   '''''bold+italic'''''
   [[first_name/surname/oc/text]] link; 'text' displayed
   [[first_name/surname/text]] link (oc = 0); 'text' displayed
   [[first_name/surname]] link (oc = 0); 'first_name surname' displayed
   [[[notes_subfile/text]]] link to a sub-file; 'text' displayed
   [[[notes_subfile]]] link to a sub-file; 'notes_subfile' displayed
   empty line : new paragraph
   lines starting with space : displayed as they are (providing 1/ there
     are at least two 2/ there is empty lines before and after the group
     of lines).
   __TOC__ : summary
   __SHORT_TOC__ : short summary (unnumbered)
   __NOTOC__ : no (automatic) numbered summary *)

module Buff2 = Buff.Make (struct  end)
module Buff = Buff.Make (struct  end)

let first_cnt = 1

let tab lev s = String.make (2 * lev) ' ' ^ s

let section_level s len =
  let rec loop i j k =
    if i > 5 then i
    else if len > k && s.[i] = '=' && s.[j] = '=' then
      loop (i + 1) (j - 1) (k + 2)
    else i
  in
  loop 1 (len - 2) 4

let notes_aliases conf =
  let fname =
    match p_getenv conf.base_env "notes_alias_file" with
      Some f -> Util.base_path [] f
    | None ->
        Filename.concat (Util.base_path [] (conf.bname ^ ".gwb"))
          "notes.alias"
  in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
    Some ic ->
      let rec loop list =
        match try Some (input_line ic) with End_of_file -> None with
          Some s ->
            let list =
              try
                let i = String.index s ' ' in
                (String.sub s 0 i,
                 String.sub s (i + 1) (String.length s - i - 1)) ::
                list
              with Not_found -> list
            in
            loop list
        | None -> close_in ic; list
      in
      loop []
  | None -> []

let map_notes aliases f = try List.assoc f aliases with Not_found -> f

let fname_of_path (dirs, file) = List.fold_right Filename.concat dirs file

let str_start_with str i x =
  let rec loop i j =
    if j = String.length x then true
    else if i = String.length str then false
    else if str.[i] = x.[j] then loop (i + 1) (j + 1)
    else false
  in
  loop i 0

type wiki_info =
  { wi_mode : string;
    wi_file_path : string -> string;
    wi_cancel_links : bool;
    wi_person_exists : string * string * int -> bool;
    wi_always_show_link : bool }

let syntax_links conf wi s =
  let slen = String.length s in
  let rec loop quot_lev pos i len =
    let (len, quot_lev) =
      if i = slen || List.exists (str_start_with s i) ["</li>"; "</p>"] then
        let len =
          match quot_lev with
            1 -> Buff.mstore len "</i>"
          | 2 -> Buff.mstore len "</b>"
          | 3 -> Buff.mstore len "</b></i>"
          | _ -> len
        in
        len, 0
      else len, quot_lev
    in
    if i = slen then Buff.get len
    else if
      s.[i] = '%' && i < slen - 1 &&
      List.mem s.[i+1] ['['; ']'; '{'; '}'; '\'']
    then
      loop quot_lev pos (i + 2) (Buff.store len s.[i+1])
    else if s.[i] = '%' && i < slen - 1 && s.[i+1] = '/' then
      loop quot_lev pos (i + 2) (Buff.mstore len conf.xhs)
    else if s.[i] = '{' then
      let (b, j) =
        let rec loop len j =
          if j = slen then Buff2.get len, j
          else if j < slen - 1 && s.[j] = '%' then
            loop (Buff2.store len s.[j+1]) (j + 2)
          else if s.[j] = '}' then Buff2.get len, j + 1
          else loop (Buff2.store len s.[j]) (j + 1)
        in
        loop 0 (i + 1)
      in
      let s = Printf.sprintf "<span class=\"highlight\">%s</span>" b in
      loop quot_lev pos j (Buff.mstore len s)
    else if
      i <= slen - 5 && s.[i] = '\'' && s.[i+1] = '\'' && s.[i+2] = '\'' &&
      s.[i+3] = '\'' && s.[i+4] = '\'' && (quot_lev = 0 || quot_lev = 3)
    then
      let s = if quot_lev = 0 then "<i><b>" else "</b></i>" in
      loop (3 - quot_lev) pos (i + 5) (Buff.mstore len s)
    else if
      i <= slen - 3 && s.[i] = '\'' && s.[i+1] = '\'' && s.[i+2] = '\'' &&
      (quot_lev = 0 || quot_lev = 2)
    then
      let s = if quot_lev = 0 then "<b>" else "</b>" in
      loop (2 - quot_lev) pos (i + 3) (Buff.mstore len s)
    else if
      i <= slen - 2 && s.[i] = '\'' && s.[i+1] = '\'' &&
      (quot_lev = 0 || quot_lev = 1)
    then
      let s = if quot_lev = 0 then "<i>" else "</i>" in
      loop (1 - quot_lev) pos (i + 2) (Buff.mstore len s)
    else
      match NotesLinks.misc_notes_link s i with
        NotesLinks.WLpage (j, fpath1, fname1, anchor, text) ->
          let (fpath, fname) =
            let aliases = notes_aliases conf in
            let fname = map_notes aliases fname1 in
            match NotesLinks.check_file_name fname with
              Some fpath -> fpath, fname
            | None -> fpath1, fname1
          in
          let c =
            let f = wi.wi_file_path (fname_of_path fpath) in
            if Sys.file_exists f then "" else " style=\"color:red\""
          in
          let t =
            if wi.wi_cancel_links then text
            else
              Printf.sprintf "<a href=\"%sm=%s&f=%s%s\"%s>%s</a>" (commd conf)
                wi.wi_mode fname anchor c text
          in
          loop quot_lev pos j (Buff.mstore len t)
      | NotesLinks.WLperson (j, (fn, sn, oc), name, _) ->
          let t =
            if wi.wi_cancel_links then name
            else if wi.wi_person_exists (fn, sn, oc) then
              Printf.sprintf "<a id=\"p_%d\" href=\"%sp=%s&n=%s%s\">%s</a>" pos
                (commd conf) (code_varenv fn) (code_varenv sn)
                (if oc = 0 then "" else "&oc=" ^ string_of_int oc) name
            else if wi.wi_always_show_link then
              let s = " style=\"color:red\"" in
              Printf.sprintf "<a id=\"p_%d\" href=\"%sp=%s&n=%s%s\"%s>%s</a>" pos
                (commd conf) (code_varenv fn) (code_varenv sn)
                (if oc = 0 then "" else "&oc=" ^ string_of_int oc) s name
            else
              Printf.sprintf "<a href=\"%s\" style=\"color:red\">%s</a>" (commd conf)
                (if conf.hide_names then "x x" else name)
          in
          loop quot_lev (pos + 1) j (Buff.mstore len t)
      | NotesLinks.WLwizard (j, wiz, name) ->
          let t =
            let s = if name <> "" then name else wiz in
            if wi.wi_cancel_links then s
            else
              Printf.sprintf "<a href=\"%sm=WIZNOTES&f=%s\">%s</a>" (commd conf) wiz
                s
          in
          loop quot_lev (pos + 1) j (Buff.mstore len t)
      | NotesLinks.WLnone -> loop quot_lev pos (i + 1) (Buff.store len s.[i])
  in
  loop 0 1 0 0

let toc_list = ["__NOTOC__"; "__TOC__"; "__SHORT_TOC__"]

let lines_list_of_string s =
  let rec loop no_toc lines len i =
    if i = String.length s then
      List.rev (if len = 0 then lines else Buff.get len :: lines), no_toc
    else if s.[i] = '\n' then
      let line = Buff.get len in
      let no_toc = List.mem line toc_list || no_toc in
      loop no_toc (line :: lines) 0 (i + 1)
    else loop no_toc lines (Buff.store len s.[i]) (i + 1)
  in
  loop false [] 0 0

let adjust_ul_level rev_lines old_lev new_lev =
  if old_lev < new_lev then tab (old_lev + 1) "<ul>" :: rev_lines
  else
    let rev_lines = (List.hd rev_lines ^ "</li>") :: List.tl rev_lines in
    let rec loop rev_lines lev =
      if lev = new_lev then rev_lines
      else loop (tab lev "</ul></li>" :: rev_lines) (lev - 1)
    in
    loop rev_lines old_lev

let message_txt conf i = transl_nth conf "visualize/show/hide/summary" i

let sections_nums_of_tlsw_lines lines =
  let (_, _, _, rev_sections_nums) =
    List.fold_left
      (fun (prev_lev, indent_stack, cnt, sections_nums) s ->
         let len = String.length s in
         if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
           let slev = section_level s len in
           let (lev, stack) =
             let rec loop lev stack =
               match stack with
                 (prev_num, prev_slev) :: rest_stack ->
                   if slev < prev_slev then
                     match rest_stack with
                       (_, prev_prev_slev) :: _ ->
                         if slev > prev_prev_slev then
                           let stack = (prev_num, slev) :: rest_stack in
                           loop lev stack
                         else loop (lev - 1) rest_stack
                     | [] ->
                         let stack = (prev_num + 1, slev) :: rest_stack in
                         lev - 1, stack
                   else if slev = prev_slev then
                     let stack = (prev_num + 1, slev) :: rest_stack in
                     lev - 1, stack
                   else let stack = (1, slev) :: stack in lev, stack
               | [] -> let stack = (1, slev) :: stack in lev, stack
             in
             loop prev_lev indent_stack
           in
           let section_num =
             let nums = List.map fst stack in
             String.concat "." (List.rev_map string_of_int nums)
           in
           lev + 1, stack, cnt + 1, (lev, section_num) :: sections_nums
         else prev_lev, indent_stack, cnt, sections_nums)
      (0, [], first_cnt, []) lines
  in
  List.rev rev_sections_nums

let remove_links s =
  let rec loop len i =
    if i = String.length s then Buff.get len
    else
      let (len, i) =
        match NotesLinks.misc_notes_link s i with
          NotesLinks.WLpage (j, _, _, _, text) -> Buff.mstore len text, j
        | NotesLinks.WLperson (j, _, name, text) ->
            let text =
              match text with
                Some text -> if text = "" then name else text
              | None -> name
            in
            Buff.mstore len text, j
        | NotesLinks.WLwizard (j, _, text) -> Buff.mstore len text, j
        | NotesLinks.WLnone -> Buff.store len s.[i], i + 1
      in
      loop len i
  in
  loop 0 0

let summary_of_tlsw_lines conf short lines =
  let sections_nums = sections_nums_of_tlsw_lines lines in
  let (rev_summary, lev, cnt, _) =
    List.fold_left
      (fun (summary, prev_lev, cnt, sections_nums) s ->
         let s = remove_links s in
         let len = String.length s in
         if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
           let slev = section_level s len in
           let (lev, section_num, sections_nums) =
             match sections_nums with
               (lev, sn) :: sns -> lev, sn, sns
             | [] -> 0, "fuck", []
           in
           let summary =
             let s =
               Printf.sprintf "<a href=\"#a_%d\">%s%s</a>" cnt
                 (if short then "" else section_num ^ " - ")
                 (String.trim (String.sub s slev (len - 2 * slev)))
             in
             if short then if summary = [] then [s] else s :: "&" :: summary
             else
               let line = tab (lev + 1) "<li>" ^ s in
               line :: adjust_ul_level summary (prev_lev - 1) lev
           in
           summary, lev + 1, cnt + 1, sections_nums
         else summary, prev_lev, cnt, sections_nums)
      ([], 0, first_cnt, sections_nums) lines
  in
  if cnt <= first_cnt + 2 then [], []
  else
    let rev_summary =
      if short then rev_summary
      else "</ul>" :: adjust_ul_level rev_summary (lev - 1) 0
    in
    let lines =
      "<dl><dd>" :: "<table id=\"summary\" cellpadding=\"10\">" ::
      ("<tr><td align=\"" ^ conf.left ^ "\">") ::
      ("<div style=\"text-align:center\"><b>" ^
       capitale (message_txt conf 3) ^ "</b>") ::
      "</div>" ::
      "<div class=\"summary\" id=\"tocinside\">" ::
      List.rev_append rev_summary
        ["</div>"; "</td></tr></table>"; "</dd></dl>"]
    in
    lines, sections_nums

let string_of_modify_link conf cnt empty =
  function
    Some (can_edit, mode, sfn) ->
      if conf.wizard then
        let mode_pref = if can_edit then "MOD" else "VIEW" in
        Printf.sprintf "%s(<a href=\"%sm=%s_%s&v=%d%s\">%s</a>)%s\n"
          (if empty then "<p>"
           else Printf.sprintf "<div style=\"font-size:80%%;float:%s;margin-%s:3em\">"
               conf.right conf.left)
          (commd conf) mode_pref mode cnt
          (if sfn = "" then "" else "&f=" ^ sfn)
          (if can_edit then transl_decline conf "modify" ""
           else transl conf "view source")
          (if empty then "</p>" else "</div>")
      else ""
  | None -> ""

let rec tlsw_list tag1 tag2 lev list sl =
  let btag2 = "<" ^ tag2 ^ ">" in
  let etag2 = "</" ^ tag2 ^ ">" in
  let list = tab lev ("<" ^ tag1 ^ ">") :: list in
  let list =
    let rec loop list =
      function
        s1 :: (s2 :: _ as sl) ->
          if String.length s2 > 0 && List.mem s2.[0] ['*'; '#'; ':'; ';'] then
            let list = (tab lev btag2 ^ s1) :: list in
            let (list, sl) = do_sub_list s2.[0] lev list sl in
            loop (tab lev etag2 :: list) sl
          else
            let (s1, ss1) = sub_sub_list lev tag2 s1 in
            loop ((tab lev btag2 ^ s1 ^ etag2 ^ ss1) :: list) sl
      | [s1] ->
          let (s1, ss1) = sub_sub_list lev tag2 s1 in
          (tab lev btag2 ^ s1 ^ etag2 ^ ss1) :: list
      | [] -> list
    in
    loop list sl
  in
  tab lev ("</" ^ tag1 ^ ">") :: list
and sub_sub_list lev tag2 s1 =
  if tag2 = "dt" && String.contains s1 ':' then
    let i = String.index s1 ':' in
    let s = String.sub s1 0 i in
    let ss =
      "\n" ^ tab (lev + 1) "<dd>" ^
      String.sub s1 (i + 1) (String.length s1 - i - 1) ^ "</dd>"
    in
    s, ss
  else s1, ""
and do_sub_list prompt lev list sl =
  let (tag1, tag2) =
    match prompt with
      '*' -> "ul", "li"
    | '#' -> "ol", "li"
    | ':' -> "dl", "dd"
    | ';' -> "dl", "dt"
    | _ -> assert false
  in
  let (list2, sl) =
    let rec loop list =
      function
        s :: sl ->
          if String.length s > 0 && s.[0] = prompt then
            let s = String.sub s 1 (String.length s - 1) in
            loop (s :: list) sl
          else list, s :: sl
      | [] -> list, []
    in
    loop [] sl
  in
  let list = tlsw_list tag1 tag2 (lev + 1) list (List.rev list2) in
  match sl with
    s :: _ ->
      if String.length s > 0 && List.mem s.[0] ['*'; '#'; ':'; ';'] then
        do_sub_list s.[0] lev list sl
      else list, sl
  | [] -> list, sl

let rec hotl conf wlo cnt edit_opt sections_nums list =
  function
    "__NOTOC__" :: sl -> hotl conf wlo cnt edit_opt sections_nums list sl
  | "__TOC__" :: sl ->
      let list =
        match wlo with
          Some lines ->
            let (summary, _) = summary_of_tlsw_lines conf false lines in
            List.rev_append summary list
        | None -> list
      in
      hotl conf wlo cnt edit_opt sections_nums list sl
  | "__SHORT_TOC__" :: sl ->
      let list =
        match wlo with
          Some lines ->
            let (summary, _) = summary_of_tlsw_lines conf true lines in
            List.rev_append summary list
        | None -> list
      in
      hotl conf wlo cnt edit_opt sections_nums list sl
  | "" :: sl ->
      let parag =
        let rec loop1 parag =
          function
            "" :: sl -> Some (parag, sl, true)
          | s :: sl ->
              if List.mem s.[0] ['*'; '#'; ':'; ';'; '='] ||
                 List.mem s toc_list
              then
                if parag = [] then None else Some (parag, s :: sl, true)
              else if s.[0] = ' ' && parag = [] then loop2 [s] sl
              else loop1 (s :: parag) sl
          | [] -> Some (parag, [], true)
        and loop2 parag =
          function
            "" :: sl -> Some (parag, sl, false)
          | s :: sl ->
              if s.[0] = ' ' then loop2 (s :: parag) sl
              else loop1 parag (s :: sl)
          | [] -> Some (parag, [], true)
        in
        loop1 [] sl
      in
      let (list, sl) =
        match parag with
          Some ([], _, _) | None -> list, sl
        | Some (parag, sl, false) when List.length parag >= 2 ->
            "</pre>" :: (parag @ "<pre>" :: list), "" :: sl
        | Some (parag, sl, _) -> "</p>" :: (parag @ "<p>" :: list), "" :: sl
      in
      hotl conf wlo cnt edit_opt sections_nums list sl
  | s :: sl ->
      let len = String.length s in
      let tago =
        if len > 0 then
          match s.[0] with
            '*' -> Some ("ul", "li")
          | '#' -> Some ("ol", "li")
          | ':' -> Some ("dl", "dd")
          | ';' -> Some ("dl", "dt")
          | _ -> None
        else None
      in
      begin match tago with
        Some (tag1, tag2) ->
          let (sl, rest) = select_list_lines conf s.[0] [] (s :: sl) in
          let list = tlsw_list tag1 tag2 0 list sl in
          hotl conf wlo cnt edit_opt sections_nums list ("" :: rest)
      | None ->
          if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
            let slev = section_level s len in
            let (section_num, sections_nums) =
              match sections_nums with
                (_, a) :: l -> a ^ " - ", l
              | [] -> "", []
            in
            let s =
              let style = if slev <= 3 then " class=\"subtitle\"" else "" in
              Printf.sprintf "<h%d%s>%s%s</h%d>" slev style section_num
                (String.sub s slev (len - 2 * slev)) slev
            in
            let list =
              if wlo <> None then
                let s = Printf.sprintf "<p><a id=\"a_%d\"></a></p>" cnt in s :: list
              else list
            in
            let list =
              let s = string_of_modify_link conf cnt false edit_opt in
              if s = "" then list else s :: list
            in
            hotl conf wlo (cnt + 1) edit_opt sections_nums list (s :: sl)
          else hotl conf wlo cnt edit_opt sections_nums (s :: list) sl
      end
  | [] -> List.rev list
and select_list_lines conf prompt list =
  function
    s :: sl ->
      let len = String.length s in
      if len > 0 && s.[0] = '=' then List.rev list, s :: sl
      else if len > 0 && s.[0] = prompt then
        let s = String.sub s 1 (len - 1) in
        let (s, sl) =
          let rec loop s1 =
            function
              "" :: s :: sl
              when String.length s > 1 && s.[0] = prompt && s.[1] = prompt ->
                let br = "<br" ^ conf.xhs ^ ">" in
                loop (s1 ^ br ^ br) (s :: sl)
            | s :: sl ->
                if String.length s > 0 && s.[0] = '=' then s1, s :: sl
                else if String.length s > 0 && s.[0] <> prompt then
                  loop (s1 ^ "\n" ^ s) sl
                else s1, s :: sl
            | [] -> s1, []
          in
          loop s sl
        in
        select_list_lines conf prompt (s :: list) sl
      else List.rev list, s :: sl
  | [] -> List.rev list, []

let html_of_tlsw conf s =
  let (lines, _) = lines_list_of_string s in
  let sections_nums =
    match sections_nums_of_tlsw_lines lines with
      [_] -> []
    | l -> l
  in
  hotl conf (Some lines) first_cnt None sections_nums [] ("" :: lines)

let html_with_summary_of_tlsw conf wi edit_opt s =
  let (lines, no_toc) = lines_list_of_string s in
  let (summary, sections_nums) =
    if no_toc then [], [] else summary_of_tlsw_lines conf false lines
  in
  let (rev_lines_before_summary, lines) =
    let rec loop lines_bef =
      function
        s :: sl ->
          if String.length s > 1 && s.[0] = '=' then lines_bef, s :: sl
          else loop (s :: lines_bef) sl
      | [] -> lines_bef, []
    in
    loop [] lines
  in
  let lines_before_summary =
    hotl conf (Some lines) first_cnt None [] []
      (List.rev rev_lines_before_summary)
  in
  let lines_after_summary =
    hotl conf (Some lines) first_cnt edit_opt sections_nums [] lines
  in
  let s =
    syntax_links conf wi
      (String.concat "\n"
         (lines_before_summary @ summary @ lines_after_summary))
  in
  if lines_before_summary <> [] || lines = [] then
    let s2 = string_of_modify_link conf 0 (s = "") edit_opt in
    s2 ^ "<p><br" ^ conf.xhs ^ "></p>\n" ^ s
  else s

let rev_extract_sub_part s v =
  let (lines, _) = lines_list_of_string s in
  let rec loop lines lev cnt =
    function
      s :: sl ->
        let len = String.length s in
        if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
          if v = first_cnt - 1 then lines
          else
            let nlev = section_level s len in
            if cnt = v then loop (s :: lines) nlev (cnt + 1) sl
            else if cnt > v then
              if nlev > lev then loop (s :: lines) lev (cnt + 1) sl else lines
            else loop lines lev (cnt + 1) sl
        else if cnt <= v then loop lines lev cnt sl
        else loop (s :: lines) lev cnt sl
    | [] -> lines
  in
  loop [] 0 first_cnt lines

let extract_sub_part s v = List.rev (rev_extract_sub_part s v)

let print_sub_part_links conf edit_mode sfn cnt0 is_empty =
  Wserver.printf "<p>\n";
  if cnt0 >= first_cnt then
    begin
      Wserver.printf
        "<a href=\"%sm=%s%s&v=%d\">" (commd conf) edit_mode sfn (cnt0 - 1);
      Wserver.printf "&lt;&lt;" ;
      Wserver.printf "</a>\n"
    end;
  Wserver.printf "<a href=\"%sm=%s%s\">" (commd conf) edit_mode sfn;
  Wserver.printf "^^";
  Wserver.printf "</a>\n";
  if not is_empty then
    begin
      Wserver.printf
        "<a href=\"%sm=%s%s&v=%d\">" (commd conf) edit_mode sfn (cnt0 + 1);
      Wserver.printf "&gt;&gt;" ;
      Wserver.printf "</a>\n"
    end;
  Wserver.printf "</p>\n"

let print_sub_part_text conf wi edit_opt cnt0 lines =
  let lines =
    List.map
      (function
         "__TOC__" | "__SHORT_TOC__" ->
           Printf.sprintf "<p>...%s...</p>" (message_txt conf 3)
       | "__NOTOC__" -> ""
       | s -> s)
      lines
  in
  let lines = hotl conf None cnt0 edit_opt [] [] lines in
  let s = String.concat "\n" lines in
  let s = syntax_links conf wi s in
  let s =
    if cnt0 < first_cnt then
      let s2 = string_of_modify_link conf 0 (s = "") edit_opt in s2 ^ s
    else s
  in
  Wserver.printf "%s\n" s

let print_sub_part conf wi can_edit edit_mode sub_fname cnt0 lines =
  let edit_opt = Some (can_edit, edit_mode, sub_fname) in
  let sfn = if sub_fname = "" then "" else "&f=" ^ sub_fname in
  print_sub_part_links conf edit_mode sfn cnt0 (lines = []);
  print_sub_part_text conf wi edit_opt cnt0 lines

let print_mod_view_page conf can_edit mode fname title env s =
  let s =
    List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" env ^ s
  in
  let mode_pref = if can_edit then "MOD_" else "VIEW_" in
  let (has_v, v) =
    match p_getint conf.env "v" with
      Some v -> true, v
    | None -> false, 0
  in
  let sub_part =
    if not has_v then s else String.concat "\n" (extract_sub_part s v)
  in
  let is_empty = sub_part = "" in
  let sfn = if fname = "" then "" else "&f=" ^ code_varenv fname in
  Hutil.header conf title;
  if can_edit then
    begin
      Wserver.printf "<div style=\"font-size:80%%;float:%s;margin-%s:3em\">\n"
        conf.right conf.left;
      Wserver.printf "(";
      begin
        Wserver.printf "<a href=\"%sm=%s%s%s\">" (commd conf) mode
          (if has_v then "&v=" ^ string_of_int v else "") sfn;
        Wserver.printf "%s" (message_txt conf 0);
        Wserver.printf "</a>"
      end;
      Wserver.printf ")\n";
      Wserver.printf "</div>\n"
    end;
  Hutil.print_link_to_welcome conf true;
  if can_edit && has_v then
    print_sub_part_links conf (mode_pref ^ mode) sfn v is_empty;
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Util.hidden_env conf;
  if can_edit then
    Wserver.printf
      "<input type=\"hidden\" name=\"m\" value=\"MOD_%s_OK\"%s>\n" mode
      conf.xhs;
  if has_v then
    Wserver.printf "<input type=\"hidden\" name=\"v\" value=\"%d\"%s>\n" v
      conf.xhs;
  if fname <> "" then
    Wserver.printf "<input type=\"hidden\" name=\"f\" value=\"%s\"%s>\n" fname
      conf.xhs;
  if can_edit then
    begin let digest = Iovalue.digest s in
      Wserver.printf
        "<input type=\"hidden\" name=\"digest\" value=\"%s\"%s>\n" digest
        conf.xhs
    end;
  Wserver.printf "<div class=\"row ml-3\">\n";
  begin match Util.open_etc_file "toolbar" with
    Some ic ->
      Wserver.printf "<div class=\"d-inline col-9 py-1\">\n";
      Templ.copy_from_templ conf ["name", "notes"] ic;
      Wserver.printf "</div>\n";
  | None -> ()
  end;
  Wserver.printf "<textarea name=\"notes\" id=\"notes_comments\"";
  Wserver.printf " class=\"col-9 form-control\" rows=\"25\" cols=\"110\"%s>"
    (if can_edit then "" else " readonly=\"readonly\"");
  Wserver.printf "%s" (Util.escape_html sub_part);
  Wserver.printf "</textarea>";
  if can_edit then
    begin
      begin
        Wserver.printf
          "<button type=\"submit\" class=\"btn btn-outline-primary btn-lg";
        Wserver.printf " col-4 py-3 mt-2 mb-3 mx-auto order-3\">";
        Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
        Wserver.printf "</button>\n"
      end
    end;
  begin match Util.open_etc_file "accent" with
    Some ic ->
      Wserver.printf "<div class=\"col my-1 mr-2 text-monospace\">\n";
      Templ.copy_from_templ conf ["name", "notes"] ic;
      Wserver.printf "</div>\n";
  | None -> ()
  end;
  Wserver.printf "</div>\n";
  Wserver.printf "</form>\n";
  Hutil.trailer conf

let insert_sub_part s v sub_part =
  let (lines, _) = lines_list_of_string s in
  let (lines, sl) =
    let rec loop sub_part_added lines lev cnt =
      function
        s :: sl ->
          let len = String.length s in
          if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
            if v = first_cnt - 1 then
              (if sub_part = "" then [] else [""; sub_part]), s :: sl
            else
              let nlev = section_level s len in
              if cnt = v then
                let lines =
                  if sub_part = "" then lines else "" :: sub_part :: lines
                in
                loop true lines nlev (cnt + 1) sl
              else if cnt > v then
                if nlev > lev then loop sub_part_added lines lev (cnt + 1) sl
                else lines, s :: sl
              else loop sub_part_added (s :: lines) lev (cnt + 1) sl
          else if cnt <= v then loop sub_part_added (s :: lines) lev cnt sl
          else loop sub_part_added lines lev cnt sl
      | [] ->
          let lines =
            if sub_part_added then lines
            else if sub_part = "" then lines
            else "" :: sub_part :: lines
          in
          lines, []
    in
    loop false [] 0 first_cnt lines
  in
  String.concat "\n" (List.rev_append lines sl)

let rec find_env s i =
  match
    try Some (String.index_from s i '=', String.index_from s i '\n') with
      Not_found -> None
  with
    Some (j, k) ->
      if j > i && j < k then
        let is_key =
          let rec loop i =
            if i = j then true
            else
              match s.[i] with
                'A'..'Z' -> loop (i + 1)
              | _ -> false
          in
          loop i
        in
        if is_key then
          let key = String.sub s i (j - i) in
          let v = String.sub s (j + 1) (k - j - 1) in
          let (env, i) = find_env s (k + 1) in (key, v) :: env, i
        else [], i
      else [], i
  | None -> [], i

let split_title_and_text s =
  let (env, i) = find_env s 0 in
  let s = if i = 0 then s else String.sub s i (String.length s - i) in
  if (try List.assoc "TITLE" env with Not_found -> "") = "" then
    let (tit, txt) =
      try
        let i = String.index s '\n' in
        let tit = String.sub s 0 i in
        let txt = String.sub s (i + 1) (String.length s - i - 1) in tit, txt
      with Not_found -> s, ""
    in
    let (tit, txt) =
      if String.length tit > 0 && tit.[0] = '=' || String.contains tit '<' ||
         String.contains tit '['
      then
        "", s
      else tit, txt
    in
    let env = if tit <> "" then ("TITLE", tit) :: env else env in env, txt
  else env, s

let print_ok conf wi edit_mode fname title_is_1st s =
  let title _ =
    Wserver.printf "%s" (Util.capitale (Util.transl conf "notes modified"))
  in
  Hutil.header_no_page_title conf title;
  Wserver.printf "<div style=\"text-align:center\">\n";
  Wserver.printf "--- ";
  title ();
  Wserver.printf " ---\n";
  Wserver.printf "</div>\n";
  Hutil.print_link_to_welcome conf true;
  let get_v = Util.p_getint conf.env "v" in
  let v =
    match get_v with
      Some v -> v
    | None -> 0
  in
  let (title, s) =
    if v = 0 && title_is_1st then
      let (env, s) = split_title_and_text s in
      (try List.assoc "TITLE" env with Not_found -> ""), s
    else "", s
  in
  let (lines, _) = lines_list_of_string s in
  let lines =
    if v = 0 && title <> "" then
      let title = Printf.sprintf "<h1>%s</h1>\n" title in title :: lines
    else lines
  in
  print_sub_part conf wi conf.wizard edit_mode fname v lines;
  Hutil.trailer conf

let print_mod_ok conf wi edit_mode fname read_string commit string_filter
    title_is_1st =
  let fname = fname (Util.p_getenv conf.env "f") in
  match edit_mode fname with
    Some edit_mode ->
    let old_string =
      let (e, s) = read_string fname in
      List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" e ^ s
    in
    let sub_part =
      match Util.p_getenv conf.env "notes" with
        Some v -> Mutil.strip_all_trailing_spaces v
      | None -> failwith "notes unbound"
    in
    let digest =
      match Util.p_getenv conf.env "digest" with
        Some s -> s
      | None -> ""
    in
    if digest <> Iovalue.digest old_string then Update.error_digest conf
    else
      let s =
        match Util.p_getint conf.env "v" with
          Some v -> insert_sub_part old_string v sub_part
        | None -> sub_part
      in
      if s <> old_string then commit fname s;
      let sub_part = string_filter sub_part in
      print_ok conf wi edit_mode fname title_is_1st sub_part
  | None -> Hutil.incorrect_request conf
