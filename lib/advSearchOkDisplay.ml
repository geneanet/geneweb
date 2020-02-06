(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util

let print_result conf base max_answers (list, len) =
  let list =
    if len > max_answers then Util.reduce_list max_answers list else list
  in
  if len = 0 then Wserver.printf "%s\n" (Utf8.capitalize (transl conf "no match"))
  else
    let () = Perso.build_sosa_ht conf base in
    Wserver.printf "<ul>\n";
    List.iter
      (fun p ->
         Wserver.printf "<li>" ;
         Perso.print_sosa conf base p true;
         Wserver.printf "\n%s%s<em>"
           (referenced_person_text conf base p)
           (DateDisplay.short_dates_text conf base p) ;
         specify_homonymous conf base p false;
         Wserver.printf "</em>")
      list;
    if len > max_answers then Wserver.printf "<li>...</li>";
    Wserver.printf "</ul>\n"


let print conf base =
  let title _ =
    Wserver.printf "%s" (Utf8.capitalize (transl_nth conf "advanced request" 0))
  in
  let max_answers =
    match p_getint conf.env "max" with
      Some n -> n
    | None -> 100
  in
  Hutil.header conf title;
  Wserver.printf "<p>\n";
  Wserver.printf "%s %s." (Utf8.capitalize (transl conf "searching all"))
    (AdvSearchOk.searching_fields conf base);
  Wserver.printf "</p>\n";
  let list = AdvSearchOk.advanced_search conf base max_answers in
  print_result conf base max_answers list; Hutil.trailer conf
