(* $Id: merge.ml, v7-exp 2018-09-26 07:34:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let print_someone base p =
  Wserver.printf "%s%s %s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print conf base p =
  let title h =
    Wserver.printf "%s" (capitale (transl_decline conf "merge" ""));
    if h then ()
    else
      begin
        Wserver.printf " ";
        print_someone base p;
        Wserver.printf " %s%s" (transl_decline conf "with" "")
          (transl conf ":");
      end
  in
  let list = Gutil.find_same_name base p in
  let list =
    List.fold_right
      (fun p1 pl ->
         if get_key_index p1 = get_key_index p then pl else p1 :: pl)
      list []
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Wserver.printf "<h2>";
  title false;
  Wserver.printf "</h2>\n";
  Wserver.printf "<form method=\"get\" action=\"%s\" \
class=\"mx-3 mb-3\">\n" conf.command;
  Util.hidden_env conf;
  Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"MRG_IND\">\n";
  Wserver.printf "<input type=\"hidden\" name=\"i\" value=\"%d\">\n"
    (Adef.int_of_iper (get_key_index p));
  Wserver.printf "<span class=\"form-row align-items-center\">\n";
  Wserver.printf "<span class=\"col-auto\">\n";
  Wserver.printf "<span class=\"custom-control custom-radio\">\n";
  Wserver.printf "  <input type=\"radio\" class=\"custom-control-input\" \
name=\"select\" id=\"input\" value=\"input\" checked>\n";
  Wserver.printf "  <label class=\"custom-control-label\" \
for=\"input\">%s</label>\n" (transl conf "any individual in the base");
  Wserver.printf "</span>\n</span>\n";
  Wserver.printf "<span class=\"col-auto\">\n";
  Wserver.printf "<input type=\"text\" class=\"form-control\" \
name=\"n\" placeholder=\"%s.%s %s\" title=\"%s.%s %s\" \
size=\"50\" id=\"inlineinput\" autofocus>\n</span>\n"
    (transl_nth conf "first name/first names" 0)
    (transl conf "number") (transl_nth conf "surname/surnames" 0)
    (transl_nth conf "first name/first names" 0)
    (transl conf "number") (transl_nth conf "surname/surnames" 0);
  Wserver.printf "</span>\n";
  if list <> [] then
    List.iter
      (fun p ->
         Wserver.printf "<div class=\"custom-control custom-radio\">\n";
         Wserver.printf
           "  <input type=\"radio\" class=\"custom-control-input\" \
name=\"select\" id=\"%d\" value=\"%d\">\n" (Adef.int_of_iper (get_key_index p))
(Adef.int_of_iper (get_key_index p));
         Wserver.printf "  <label class=\"custom-control-label\" \
for=\"%d\">" (Adef.int_of_iper (get_key_index p));
         Update.print_person_parents_and_spouse conf base p;
         Wserver.printf "  </label>\n</div>\n";)
    list;
  Wserver.printf "<button type=\"submit\" \
 class=\"btn btn-primary btn-lg mt-2\">";
  Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</form>\n";
  Hutil.trailer conf

let print_possible_continue_merging conf base =
  match p_getint conf.env "ini1", p_getint conf.env "ini2" with
    Some ini1, Some ini2 ->
      let p1 = poi base (Adef.iper_of_int ini1) in
      let p2 = poi base (Adef.iper_of_int ini2) in
      Wserver.printf "\n";
      html_p conf;
      Wserver.printf "<a href=%sm=MRG_IND&i=%d&i2=%d>" (commd conf) ini1 ini2;
      Wserver.printf "%s" (capitale (transl conf "continue merging"));
      Wserver.printf "</a>";
      Wserver.printf "\n";
      print_someone base p1;
      Wserver.printf "\n%s\n" (transl_nth conf "and" 0);
      print_someone base p2;
      Wserver.printf "\n"
  | _ ->
      match p_getint conf.env "ip" with
        Some ip ->
          let s1 =
            match p_getenv conf.env "iexcl" with
              Some "" | None -> ""
            | Some s -> "&iexcl=" ^ s
          in
          let s2 =
            match p_getenv conf.env "fexcl" with
              Some "" | None -> ""
            | Some s -> "&fexcl=" ^ s
          in
          if s1 <> "" || s2 <> "" then
            begin
              Wserver.printf "<p>\n";
              Wserver.printf "<a href=%sm=MRG_DUP&ip=%d%s%s>" (commd conf)
                ip s1 s2;
              Wserver.printf "%s"
                (capitale (transl conf "continue merging"));
              Wserver.printf "</a>" ;
              begin
                let p = poi base (Adef.iper_of_int ip) in
                let s = person_text conf base p in
                Wserver.printf "\n(%s)\n"
                  (transl_a_of_b conf
                     (transl conf "possible duplications")
                     (reference conf base p s) s)
              end;
              Wserver.printf "</p>\n"
            end
      | None -> ()
