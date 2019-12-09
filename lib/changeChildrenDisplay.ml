(* Copyright (c) 1998-2018 INRIA *)

open Config
open Def
open Gwdb
open Util
open ChangeChildren

let print_child_person conf base p =
  let var = "c" ^ string_of_iper (get_iper p) in
  let first_name =
    match p_getenv conf.env (var ^ "_first_name") with
      Some v -> v
    | None -> p_first_name base p
  in
  let surname =
    match p_getenv conf.env (var ^ "_surname") with
      Some v -> v
    | None -> p_surname base p
  in
  let occ =
    match p_getint conf.env (var ^ "_occ") with
      Some i -> i
    | None -> get_occ p
  in
  Wserver.printf "<table class=\"m1-2\">\n";
  Wserver.printf "<tbody>\n";
  Wserver.printf "<tr align=\"%s\">\n" conf.left;
  Wserver.printf "<td>";
  Wserver.printf "<label for=\"%s_fn\" class=\"mx-2 mb-0\">%s</label>"
    var (Utf8.capitalize (transl_nth conf "first name/first names" 0));
  Wserver.printf "</td>\n";
  Wserver.printf "<td colspan=\"3\">\n";
  Wserver.printf "<input name=\"%s_first_name\" class=\"form-control\" \
size=\"23\" maxlength=\"200\" id=\"%s_fn\" value=\"%s\">\n" var var
(Util.escape_html first_name);
  Wserver.printf "</td>\n";
  Wserver.printf "<td align=\"%s\">" conf.right;
  Wserver.printf "<label for=\"%s_occ\" class=\"mx-2 mb-0\">%s</label>"
    var (Utf8.capitalize (transl conf "number"));
  Wserver.printf "</td>\n";
  Wserver.printf "<td>\n";
  Wserver.printf "<input class=\"form-control\" id=\"%s_occ\" name=\"%s_occ\" \
size=\"5\" maxlength=\"8\"%s>\n" var var
(if occ = 0 then "" else " value=\"" ^ string_of_int occ ^ "\"");
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "<tr align=\"%s\">\n" conf.left;
  Wserver.printf "<td>";
  Wserver.printf "<label for=\"%s_sn\" class=\"mx-2 mb-0\">%s</label>"
    var (Utf8.capitalize (transl_nth conf "surname/surnames" 0));
  Wserver.printf "</td>\n";
  Wserver.printf "<td colspan=\"5\">\n";
  Wserver.printf "<input name=\"%s_surname\" class=\"form-control\" \
size=\"40\" maxlength=\"200\" id=\"%s_sn\" value=\"%s\">\n" var var surname;
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "</tbody>\n";
  Wserver.printf "</table>\n"


let print_children conf base ipl =
  Wserver.printf "<ul>\n";
  List.iter
    (fun ip ->
       let p = poi base ip in
       Wserver.printf "<li class=\"mt-3\">\n";
       Wserver.printf "<span class=\"ml-2\">%s"
         (reference conf base p (person_text conf base p));
       Wserver.printf "%s</span>\n" (DateDisplay.short_dates_text conf base p);
       print_child_person conf base p;
       Wserver.printf "</li>\n")
    ipl;
  Wserver.printf "</ul>\n"

let print_change conf base p =
  let title _ =
    let s = transl conf "change children's names" in
    Wserver.printf "%s" (Utf8.capitalize s)
  in
  let children = select_children_of base p in
  let digest = digest_children base children in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Wserver.printf "<h2>";
    title false;
  begin
    let s = person_text conf base p in
    Wserver.printf "%s" (Util.transl_a_of_b conf "" (reference conf base p s) s)
  end ;
  Wserver.printf " %s" (DateDisplay.short_dates_text conf base p);
  Wserver.printf "</h2>\n";
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Util.hidden_env conf;
  Wserver.printf "<input type=\"hidden\" name=\"ip\" value=\"%s\">\n"
    (string_of_iper (get_iper p));
  Wserver.printf "<input type=\"hidden\" name=\"digest\" value=\"%s\">\n"
    digest;
  Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"CHG_CHN_OK\">\n";
  print_children conf base children;
  Wserver.printf "\n";
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-primary btn-lg ml-5 mb-2\">";
  Wserver.printf "%s" (Utf8.capitalize (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</form>\n";
  Wserver.printf "\n";
  Hutil.trailer conf

let print conf base =
  match p_getenv conf.env "ip" with
    Some i ->
      let p = poi base (iper_of_string i) in print_change conf base p
  | _ -> Hutil.incorrect_request conf

let print_children_list conf base u =
  Wserver.printf "<h4>";
  Wserver.printf "%s" (Utf8.capitalize (transl_nth conf "child/children" 1));
  Wserver.printf "</h4>";
  Wserver.printf "\n<p>\n";
  Wserver.printf "<ul>\n";
  Array.iter
    (fun ifam ->
       let des = foi base ifam in
       Array.iter
         (fun ip ->
            let p = poi base ip in
            Wserver.printf "<li>" ;
            Wserver.printf "\n%s"
              (reference conf base p (person_text conf base p));
            Wserver.printf "%s\n" (DateDisplay.short_dates_text conf base p))
         (get_children des))
    (get_family u);
  Wserver.printf "</ul>\n"

let print_change_done conf base p =
  let title _ =
    let s = transl conf "children's names changed" in
    Wserver.printf "%s" (Utf8.capitalize s)
  in
  Hutil.header conf title;
  Wserver.printf "\n%s" (reference conf base p (person_text conf base p));
  Wserver.printf "%s\n" (DateDisplay.short_dates_text conf base p);
  print_children_list conf base p;
  Hutil.trailer conf

let print_conflict conf base ip_var p =
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Update.print_error conf base (AlreadyDefined p);
  let free_n =
    Gutil.find_free_occ base (p_first_name base p) (p_surname base p) 0
  in
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>";
  Wserver.printf "%s%s %d.\n" (Utf8.capitalize (transl conf "first free number"))
    (Util.transl conf ":") free_n;
  Wserver.printf (fcapitale (ftransl conf "click on \"%s\""))
    (transl conf "create");
  Wserver.printf "%s.\n" (transl conf " to try again with this number");
  Wserver.printf "</li>";
  Wserver.printf "<li>";
  Wserver.printf "%s " (Utf8.capitalize (transl conf "or"));
  Wserver.printf (ftransl conf "click on \"%s\"") (transl conf "back");
  Wserver.printf " %s %s." (transl_nth conf "and" 0)
    (transl conf "change it (the number) yourself");
  Wserver.printf "</li>";
  Wserver.printf "</ul>\n";
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       Wserver.printf "<input type=\"hidden\" name=\"%s\" value=\"%s\">\n" x
         (Util.escape_html (decode_varenv v)))
    (conf.henv @ conf.env);
  begin let var = "c" ^ string_of_iper ip_var in
    Wserver.printf "<input type=\"hidden\" name=\"field\" value=\"%s\">\n" var
  end;
  Wserver.printf "<input type=\"hidden\" name=\"free_occ\" value=\"%d\">\n"
    free_n;
  Wserver.printf  "<button type=\"submit\" name=\"create\" \
class=\"btn btn-primary btn-lg\">%s</button>\n" (Utf8.capitalize (transl conf "create"));
  Wserver.printf "<button type=\"submit\" name=\"return\" \
class=\"btn btn-primary btn-lg\">%s</button>\n" (Utf8.capitalize (transl conf "back"));
  Wserver.printf "</form>\n";
  Update.print_same_name conf base p;
  Hutil.trailer conf;
  raise @@ Update.ModErr __LOC__

let error_person conf err =
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s\n" (Utf8.capitalize err);
  Hutil.trailer conf;
  raise @@ Update.ModErr __LOC__


let print_update_child conf base =
  match p_getenv conf.env "m" with
    Some "CHG_CHN_OK" -> print conf base
  | _ -> Hutil.incorrect_request conf

let print_change_ok conf base p =
  let ipl = select_children_of base p in
  let parent_surname = p_surname base p in
  let redisp =
    match p_getenv conf.env "return" with
      Some _ -> true
    | _ -> false
  in
  if redisp then print_update_child conf base
  else begin
    check_digest conf (digest_children base ipl);
    let changed =
      try change_children conf base parent_surname ipl
      with
      | ChangeChildrenConflict (p, p') -> print_conflict conf base (get_iper p) p'
      | FirstNameMissing _ -> error_person conf (transl conf "first name missing")
    in
    Util.commit_patches conf base;
    let changed =
      U_Change_children_name
        (Util.string_gen_person base (gen_person_of_person p), changed)
    in
    History.record conf base changed "cn";
    print_change_done conf base p
  end

let print_ok o_conf base =
  let conf = Update.update_conf o_conf in
  match p_getenv conf.env "ip" with
    Some i ->
      let p = poi base (iper_of_string i) in print_change_ok conf base p
  | _ -> Hutil.incorrect_request conf
