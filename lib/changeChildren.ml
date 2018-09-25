(* $Id: changeChildren.ml,v 5.22 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

let print_child_person conf base p =
  let var = "c" ^ string_of_int (Adef.int_of_iper (get_key_index p)) in
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
  Wserver.printf "<table border=\"1\">\n";
  Wserver.printf "<tr align=\"%s\">\n" conf.left;
  Wserver.printf "<td>\n";
  Wserver.printf "%s" (capitale (transl_nth conf "first name/first names" 0));
  Wserver.printf "</td>\n";
  Wserver.printf "<td colspan=\"3\">\n";
  Wserver.printf
    "<input name=\"%s_first_name\" class=\"form-control\" size=\"23\" maxlength=\"200\" value=\"%s\"%s>\n"
    var (quote_escaped first_name) conf.xhs;
  Wserver.printf "</td>\n";
  Wserver.printf "<td align=\"%s\">\n" conf.right;
  (let s = capitale (transl conf "number") in Wserver.printf "%s" s);
  Wserver.printf "</td>\n";
  Wserver.printf "<td>\n";
  Wserver.printf
    "<input class=\"form-control\" name=\"%s_occ\" size=\"5\" maxlength=\"8\"%s%s>\n"
    var (if occ = 0 then "" else " value=\"" ^ string_of_int occ ^ "\"")
    conf.xhs;
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "<tr align=\"%s\">\n" conf.left;
  Wserver.printf "<td>\n";
  Wserver.printf "%s" (capitale (transl_nth conf "surname/surnames" 0));
  Wserver.printf "</td>\n";
  Wserver.printf "<td colspan=\"5\">\n";
  Wserver.printf
    "<input name=\"%s_surname\" class=\"form-control\" size=\"40\" maxlength=\"200\" value=\"%s\"%s>\n"
    var surname conf.xhs;
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "</table>\n"

let select_children_of base u =
  List.fold_right
    (fun ifam ipl ->
       let des = foi base ifam in
       List.fold_right (fun ip ipl -> ip :: ipl)
         (Array.to_list (get_children des)) ipl)
    (Array.to_list (get_family u)) []

let digest_children base ipl =
  let l =
    List.map
      (fun ip ->
         let p = poi base ip in
         sou base (get_first_name p), sou base (get_surname p), get_occ p)
      ipl
  in
  Iovalue.digest l

let check_digest conf digest =
  match p_getenv conf.env "digest" with
    Some ini_digest -> if digest <> ini_digest then Update.error_digest conf
  | None -> ()

let print_children conf base ipl =
  Wserver.printf "<h4>";
  Wserver.printf "%s" (capitale (transl_nth conf "child/children" 1));
  Wserver.printf "</h4>\n";
  Wserver.printf "<ul>\n";
  List.iter
    (fun ip ->
       let p = poi base ip in
       Wserver.printf "<li>\n";
       Wserver.printf "%s" (reference conf base p (person_text conf base p));
       Wserver.printf "%s\n" (Date.short_dates_text conf base p);
       print_child_person conf base p;
       Wserver.printf "</li>\n")
    ipl;
  Wserver.printf "</ul>\n"

let print_change conf base p =
  let title _ =
    let s = transl conf "change children's names" in
    Wserver.printf "%s" (capitale s)
  in
  let children = select_children_of base p in
  let digest = digest_children base children in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Wserver.printf "<h2>\n";
  title false;
  Wserver.printf "</h2>\n";
  Wserver.printf "<p>\n";
  Wserver.printf "%s" (reference conf base p (person_text conf base p));
  Wserver.printf "%s\n" (Date.short_dates_text conf base p);
  Wserver.printf "</p>\n";
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Wserver.printf "<p>\n";
  Util.hidden_env conf;
  Wserver.printf "<input type=\"hidden\" name=\"ip\" value=\"%d\"%s>\n"
    (Adef.int_of_iper (get_key_index p)) conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"digest\" value=\"%s\"%s>\n"
    digest conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"CHG_CHN_OK\"%s>\n"
    conf.xhs;
  Wserver.printf "</p>\n";
  print_children conf base children;
  Wserver.printf "\n";
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
  Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</form>\n";
  Wserver.printf "\n";
  Hutil.trailer conf

let print conf base =
  match p_getint conf.env "ip" with
    Some i ->
      let p = poi base (Adef.iper_of_int i) in print_change conf base p
  | _ -> Hutil.incorrect_request conf

let print_children_list conf base u =
  Wserver.printf "<h4>";
  Wserver.printf "%s" (capitale (transl_nth conf "child/children" 1));
  Wserver.printf "</h4>";
  Wserver.printf "\n<p>\n";
  Wserver.printf "<ul>\n";
  Array.iter
    (fun ifam ->
       let des = foi base ifam in
       Array.iter
         (fun ip ->
            let p = poi base ip in
            html_li conf;
            Wserver.printf "\n%s"
              (reference conf base p (person_text conf base p));
            Wserver.printf "%s\n" (Date.short_dates_text conf base p))
         (get_children des))
    (get_family u);
  Wserver.printf "</ul>\n"

let print_change_done conf base p =
  let title _ =
    let s = transl conf "children's names changed" in
    Wserver.printf "%s" (capitale s)
  in
  Hutil.header conf title;
  Wserver.printf "\n%s" (reference conf base p (person_text conf base p));
  Wserver.printf "%s\n" (Date.short_dates_text conf base p);
  print_children_list conf base p;
  Hutil.trailer conf

let print_conflict conf base ip_var p =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Update.print_error conf base (AlreadyDefined p);
  let free_n =
    Gutil.find_free_occ base (p_first_name base p) (p_surname base p) 0
  in
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>";
  Wserver.printf "%s%s %d.\n" (capitale (transl conf "first free number"))
    (Util.transl conf ":") free_n;
  Wserver.printf (fcapitale (ftransl conf "click on \"%s\""))
    (transl conf "create");
  Wserver.printf "%s.\n" (transl conf " to try again with this number");
  Wserver.printf "</li>";
  Wserver.printf "<li>";
  Wserver.printf "%s " (capitale (transl conf "or"));
  Wserver.printf (ftransl conf "click on \"%s\"") (transl conf "back");
  Wserver.printf " %s %s." (transl_nth conf "and" 0)
    (transl conf "change it (the number) yourself");
  Wserver.printf "</li>";
  Wserver.printf "</ul>\n";
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       Wserver.printf "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n" x
         (quote_escaped (decode_varenv v)) conf.xhs)
    (conf.henv @ conf.env);
  begin let var = "c" ^ string_of_int (Adef.int_of_iper ip_var) in
    Wserver.printf "<input type=\"hidden\" name=\"field\" value=\"%s\"%s>\n"
      var conf.xhs
  end;
  Wserver.printf "<input type=\"hidden\" name=\"free_occ\" value=\"%d\"%s>\n"
    free_n conf.xhs;
  Wserver.printf
    "<button type=\"submit\" name=\"create\" class=\"btn btn-secondary btn-lg\">\n";
  Wserver.printf "%s" (capitale (transl conf "create"));
  Wserver.printf "</button>\n";
  Wserver.printf
    "<button type=\"submit\" name=\"return\" class=\"btn btn-secondary btn-lg\">\n";
  Wserver.printf "%s" (capitale (transl conf "back"));
  Wserver.printf "</button>\n";
  Wserver.printf "</form>\n";
  Update.print_same_name conf base p;
  Hutil.trailer conf

let check_conflict conf base p key new_occ ipl =
  let name = Name.lower key in
  List.iter
    (fun ip ->
       let p1 = poi base ip in
       if get_key_index p1 <> get_key_index p &&
          Name.lower (p_first_name base p1 ^ " " ^ p_surname base p1) =
            name &&
          get_occ p1 = new_occ
       then
         begin
           print_conflict conf base (get_key_index p) p1;
           raise Update.ModErr
         end)
    ipl

let error_person conf err =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s\n" (capitale err);
  Hutil.trailer conf;
  raise Update.ModErr

let rename_image_file conf base p (nfn, nsn, noc) =
  match auto_image_file conf base p with
    Some old_f ->
      let s = default_image_name_of_key nfn nsn noc in
      let f = Filename.concat (base_path ["images"] conf.bname) s in
      let new_f =
        if Filename.check_suffix old_f ".gif" then f ^ ".gif" else f ^ ".jpg"
      in
      (try Sys.rename old_f new_f with Sys_error _ -> ())
  | _ -> ()

exception FirstNameMissing of iper

let change_child conf base parent_surname changed ip =
  let p = poi base ip in
  let var = "c" ^ string_of_int (Adef.int_of_iper (get_key_index p)) in
  let new_first_name =
    match p_getenv conf.env (var ^ "_first_name") with
      Some x -> only_printable x
    | _ -> p_first_name base p
  in
  let new_surname =
    match p_getenv conf.env (var ^ "_surname") with
      Some x ->
        let x = only_printable x in if x = "" then parent_surname else x
    | _ -> p_surname base p
  in
  let new_occ =
    match p_getint conf.env (var ^ "_occ") with
      Some x -> x
    | _ -> 0
  in
  if new_first_name = "" then raise (FirstNameMissing ip)
  else if
    new_first_name <> p_first_name base p ||
    new_surname <> p_surname base p || new_occ <> get_occ p
  then begin
    let key = new_first_name ^ " " ^ new_surname in
    let ipl = Gutil.person_ht_find_all base key in
    check_conflict conf base p key new_occ ipl;
    rename_image_file conf base p (new_first_name, new_surname, new_occ);
    (* On ajoute les enfants dans le type Change_children_name       *)
    (* pour la future mise à jour de l'historique et du fichier gwf. *)
    let changed =
      ((p_first_name base p, p_surname base p, get_occ p, ip),
       (new_first_name, new_surname, new_occ, ip))
      :: changed
    in
    let p =
      { (gen_person_of_person p) with
        first_name = Gwdb.insert_string base new_first_name
      ; surname = Gwdb.insert_string base new_surname
      ; occ = new_occ}
    in
    patch_person base ip p;
    patch_key base ip new_first_name new_surname new_occ;
    Gutil.person_ht_add base key ip;
    let np_misc_names = gen_person_misc_names base p (fun p -> p.titles) in
    List.iter (fun key -> Gutil.person_ht_add base key p.key_index) np_misc_names ;
    changed
  end
  else changed

let change_children conf base parent_surname =
  List.fold_left (fun changed ip -> change_child conf base parent_surname changed ip) []

let print_update_child conf base =
  match p_getenv conf.env "m" with
    Some "CHG_CHN_OK" -> print conf base
  | _ -> Hutil.incorrect_request conf

let print_change_ok conf base p =
  try
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
        with FirstNameMissing _ -> error_person conf (transl conf "first name missing")
      in
      Util.commit_patches conf base;
      let changed =
        U_Change_children_name
          (Util.string_gen_person base (gen_person_of_person p), changed)
      in
      History.record conf base changed "cn";
      print_change_done conf base p
    end
  with Update.ModErr -> ()

let print_ok o_conf base =
  let conf = Update.update_conf o_conf in
  match p_getint conf.env "ip" with
    Some i ->
      let p = poi base (Adef.iper_of_int i) in print_change_ok conf base p
  | _ -> Hutil.incorrect_request conf
