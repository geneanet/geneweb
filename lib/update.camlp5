(* $Id: update.ml,v 5.48 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)


open Config
open Def
open Gwdb
open Util

(* #ifdef API *)
exception ModErrApi of string
(* #endif *)

exception ModErr
type create_info =
  { ci_birth_date : date option;
    ci_birth_place : string;
    ci_death : death;
    ci_death_date : date option;
    ci_death_place : string;
    ci_occupation : string;
    ci_public : bool }
type create =
    Create of sex * create_info option
  | Link
type key = string * string * int * create * string

let infer_death_from_age a =
  if a > 120 then OfCourseDead (*TODO private_years??? *)
  (* TODO: TO BE REMOVED else if a <= 80 then NotDead *)
  else DontKnowIfDead

let infer_death_from_date conf d =
  infer_death_from_age (CheckItem.time_elapsed d conf.today).year

let infer_death_from_odate conf = function
  | Some (Dgreg (d, _)) -> infer_death_from_date conf d
  | _ -> DontKnowIfDead

let infer_death_bb conf birth bapt =
  match infer_death_from_odate conf birth with
  | DontKnowIfDead -> infer_death_from_odate conf bapt
  | death -> death

let infer_death_from_parents conf base fam =
  let fath = poi base @@ get_father fam in
  let moth = poi base @@ get_mother fam in
  let aux = function
    | (Death (_, d),_) | (_, Death (_, d)) ->
      infer_death_from_odate conf (Adef.od_of_cdate d)
    | _ ->
      match
        ( Adef.od_of_cdate @@ get_birth moth
        , Adef.od_of_cdate @@ get_birth fath )
      with
      | (Some (Dgreg (d, _)), _) | (_, Some (Dgreg (d, _))) ->
        infer_death_from_age @@ (CheckItem.time_elapsed d conf.today).year - 120
      | _ -> DontKnowIfDead
  in
  match get_death fath, get_death moth with
  | (Death (_, d1) as a), (Death (_, d2) as b) ->
    begin match Adef.od_of_cdate d1, Adef.od_of_cdate d2 with
      | Some (Dgreg (d1, _) as d1'), Some (Dgreg (d2, _)as d2') ->
        if Date.compare_date d1' d2' > 0
        then infer_death_from_date conf d2
        else infer_death_from_date conf d1
      | Some (Dgreg (d, _)), _ | _, Some (Dgreg (d, _)) -> infer_death_from_date conf d
      | _ -> aux (a, b)
    end
  | a, b -> aux (a, b)

let rec infer_death conf base p =
  let death =
    infer_death_bb conf
      (Adef.od_of_cdate (get_birth p))
      (Adef.od_of_cdate (get_baptism p))
  in
  if death <> DontKnowIfDead then death
  else
    let death =
      let families = get_family p in
      let len = Array.length families in
      let rec loop_families i =
        if i = len then DontKnowIfDead
        else
          let fam = foi base families.(i) in
          match Adef.od_of_cdate (get_marriage fam) with
          (* TODO this 120 should be related to private_years_marriage *)
          | Some (Dgreg (d, _)) when (CheckItem.time_elapsed d conf.today).year > 120 ->
            OfCourseDead
          | _ ->
            let death =
              let children = get_children fam in
              let len = Array.length children in
              let rec loop_children j =
                if j = len then DontKnowIfDead
                else
                  let death = infer_death conf base (poi base children.(j)) in
                  if death = OfCourseDead then OfCourseDead
                  else loop_children (j + 1)
              in
              loop_children 0
            in
            if death = OfCourseDead then OfCourseDead
            else loop_families (i + 1)
      in
      loop_families 0
    in
    if death <> DontKnowIfDead then death
    else
      match get_parents p with
      | None -> DontKnowIfDead
      | Some ifam -> infer_death_from_parents conf base (foi base ifam)

(*let restrict_to_small_list el =
  let rec begin_list n rl el =
     TODO suppress this limit
    if n > 25 then
      let rec end_list n sl el =
        if n > 25 then List.rev_append rl (None :: sl)
        else
          match el with
            e :: el -> end_list (n + 1) (Some e :: sl) el
          | [] -> List.rev_append rl sl
      in
      end_list 0 [] (List.rev el)
    else
      match el with
        e :: el -> begin_list (n + 1) (Some e :: rl) el
      | [] -> List.rev rl
  in
  begin_list 0 [] el*)

(* ************************************************************************** *)
(*  [Fonc] print_person_parents_and_spouses :
             config -> base -> person -> unit                                 *)
(** [Description] : Print several information to distinguish homonyms. The
      information includes name of the person, name of the parents,
      name of the spouse.
    [Args] :
      - conf : configuration of the base
      - base : base
      - p    : person
    [Retour] : unit
    [Rem] : Not visible.                                                      *)
(* ************************************************************************** *)
let print_person_parents_and_spouse conf base p =
  Wserver.printf "<a href=\"%s%s\">" (commd conf) (acces conf base p);
  Wserver.printf "%s.%d %s" (p_first_name base p) (get_occ p)
    (p_surname base p);
  Wserver.printf "</a>";
  Wserver.printf "%s" (Date.short_dates_text conf base p);
  let cop = Util.child_of_parent conf base p in
  if (String.length cop) > 0 then Wserver.printf ", %s" cop;
  let hbw = Util.husband_wife conf base p true in
  if (String.length hbw) > 0 then Wserver.printf ", %s" hbw;
  Wserver.printf ".\n"

let print_same_name conf base p =
  match Gutil.find_same_name base p with
    [_] -> ()
  | pl ->
      Wserver.printf "<p>\n";
      Wserver.printf "%s%s\n"
        (capitale (transl conf "persons having the same name"))
        (transl conf ":");
      Wserver.printf "<ul>\n";
      List.iter
        (fun p ->
           Wserver.printf "<li>";
           print_person_parents_and_spouse conf base p;
           Wserver.printf "</li>")
        pl;
      Wserver.printf "</ul>\n";
      Wserver.printf "</p>\n"


(* ************************************************************************* *)
(*  [Fonc] is_label_note : string -> bool                                    *)
(** [Description] : Test si le label contient le mot 'note' pour savoir si
      dans les évènement secondaires, il faut traiter la note comme un
      textarea.
    [Args] :
      - lbl : le label
    [Retour] :
      - bool
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let is_label_note lbl =
  let rec loop i =
    if i = String.length lbl then false
    else if lbl.[i] = 'n' then
      let note = "note" in
      if String.length note + i <= String.length lbl then
        let sub_x = String.sub lbl i (String.length note) in
        if sub_x = note then true else loop (i + String.length note)
      else false
    else loop (i + 1)
  in
  loop 0

let print_return conf =
  Wserver.printf "<p>\n";
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       (* Seul un textarea peut contenir des sauts de ligne. *)
       (* On remplace donc l'input par un textarea.          *)
       if x = "notes" || x = "comment" || is_label_note x then
         begin
           Wserver.printf "<textarea style=\"display:none;\" name=\"%s\">\n"
             x;
           Wserver.printf "%s" (Util.escape_html (decode_varenv v));
           Wserver.printf "</textarea>\n"
         end
       else
         Wserver.printf "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n"
           x (Util.escape_html (decode_varenv v)) conf.xhs)
    (conf.henv @ conf.env);
  Wserver.printf "<input type=\"hidden\" name=\"return\" value=\"on\"%s>\n"
    conf.xhs;
  Wserver.printf "<input type=\"submit\" value=\"%s\"%s>\n"
    (capitale (transl conf "back")) conf.xhs;
  Wserver.printf "</form>\n";
  Wserver.printf "</p>\n"

let print_err_unknown conf _base (f, s, o) =
(* #ifdef API *)
  if !(Api_conf.mode_api) then
    begin let err =
      Printf.sprintf "%s%s <strong>%s.%d %s</strong>\n"
        (capitale (transl conf "unknown person")) (transl conf ":") f o s
    in
      raise (ModErrApi err)
    end;
(* #endif *)
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s%s <strong>%s.%d %s</strong>\n"
    (capitale (transl conf "unknown person")) (transl conf ":") f o s;
  print_return conf;
  Hutil.trailer conf;
  raise ModErr

let update_misc_names_of_family base p_sex u =
  match p_sex with
    Male ->
      List.iter
        (fun ifam ->
           let fam = foi base ifam in
           List.iter
             (fun ip ->
                List.iter
                  (fun name ->
                     if not (List.mem ip (Gutil.person_ht_find_all base name)) then
                       Gutil.person_ht_add base name ip)
                  (person_misc_names base (poi base ip) get_titles))
             (get_mother fam :: Array.to_list (get_children fam)))
        (Array.to_list u.family)
  | _ -> ()

let delete_topological_sort_v conf _base =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  let tstab_file = Filename.concat bfile "tstab_visitor" in
  Util.rm tstab_file ;
  let tstab_file = Filename.concat bfile "restrict" in
  Util.rm tstab_file

let delete_topological_sort conf base =
  let _ = delete_topological_sort_v conf base in
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  let tstab_file = Filename.concat bfile "tstab" in
  Util.rm tstab_file

let print_someone _conf base p =
  Wserver.printf "%s%s %s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print_first_name _conf base p =
  Wserver.printf "%s%s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))

let print_someone_strong _conf base p =
  Printf.sprintf "<strong>%s%s %s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print_first_name_strong _conf base p =
  Wserver.printf "<strong>%s%s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))

let print_error conf base =
  function
    AlreadyDefined p ->
      Wserver.printf
        (fcapitale (ftransl conf "name %s already used by %tthis person%t"))
        ("\"" ^ p_first_name base p ^ "." ^ string_of_int (get_occ p) ^ " " ^
         p_surname base p ^ "\"")
        (fun _ ->
           Printf.sprintf "<a href=\"%s%s\">" (commd conf)
             (acces conf base p))
        (fun _ -> "</a>.")
  | OwnAncestor p ->
      Wserver.printf "%s\n%s" (print_someone_strong conf base p)
        (transl conf "would be his/her own ancestor")
  | BadSexOfMarriedPerson _ ->
      Wserver.printf "%s."
        (capitale (transl conf "cannot change sex of a married person"))

let someone_ref_text conf base p =
  "<a href=\"" ^ commd conf ^ acces conf base p ^ "\">\n" ^
  p_first_name base p ^
  (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p)) ^ " " ^
  p_surname base p ^ "</a>"

let print_warning conf base =
  function
    BigAgeBetweenSpouses (fath, moth, a) ->
      Wserver.printf
        (fcapitale
           (ftransl conf
              "the difference of age between %t and %t is quite important"))
        (fun _ -> print_someone_strong conf base fath)
        (fun _ -> print_someone_strong conf base moth);
      Wserver.printf ": %s" (Date.string_of_age conf a)
  | BirthAfterDeath p ->
      Wserver.printf (ftransl conf "%t died before his/her birth")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | ChangedOrderOfChildren (ifam, _, before, after) ->
      let cpl = foi base ifam in
      let fath = poi base (get_father cpl) in
      let moth = poi base (get_mother cpl) in
      Wserver.printf "%s\n"
        (capitale (transl conf "changed order of children"));
      Wserver.printf "-&gt;\n";
      Wserver.printf "%s"
        (someone_ref_text conf base fath ^ "\n" ^ transl_nth conf "and" 0 ^
         " " ^ someone_ref_text conf base moth ^ "\n");
      let print_list arr diff_arr =
        Array.iteri
          (fun i ip ->
             let p = poi base ip in
             Wserver.printf "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             if eq_istr (get_surname p) (get_surname fath) then
               print_first_name conf base p
             else print_someone conf base p;
             Wserver.printf "%s" (Date.short_dates_text conf base p);
             Wserver.printf "\n";
             Wserver.printf "</li>\n")
          arr
      in
      let (bef_d, aft_d) = Difference.f before after in
      Wserver.printf "<table style=\"margin:1em\">\n";
      Wserver.printf "<tr>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "</tr>\n";
      Wserver.printf "</table>\n"
  | ChildrenNotInOrder (ifam, _, elder, x) ->
      let cpl = foi base ifam in
      Wserver.printf
        (fcapitale
           (ftransl conf
              "the following children of %t and %t are not in order"))
        (fun _ -> print_someone_strong conf base (poi base (get_father cpl)))
        (fun _ -> print_someone_strong conf base (poi base (get_mother cpl)));
      Wserver.printf ":\n";
      Wserver.printf "<ul>\n";
      Wserver.printf "<li>";
      print_first_name_strong conf base elder;
      Wserver.printf "%s" (Date.short_dates_text conf base elder);
      Wserver.printf "</li>";
      Wserver.printf "<li>";
      print_first_name_strong conf base x;
      Wserver.printf "%s" (Date.short_dates_text conf base x);
      Wserver.printf "</li>";
      Wserver.printf "</ul>\n"
  | ChangedOrderOfMarriages (p, before, after) ->
      Wserver.printf "%s\n"
        (capitale (transl conf "changed order of marriages"));
      Wserver.printf "-&gt;\n";
      let print_list arr diff_arr =
        Array.iteri
          (fun i ifam ->
             let fam = foi base ifam in
             let sp = Gutil.spouse (get_key_index p) fam in
             let sp = poi base sp in
             Wserver.printf "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             print_first_name conf base p;
             Wserver.printf "  &amp;";
             Wserver.printf "%s\n"
               (Date.short_marriage_date_text conf base fam p sp);
             print_someone conf base sp;
             Wserver.printf "\n";
             Wserver.printf "</li>\n")
          arr
      in
      let (bef_d, aft_d) = Difference.f before after in
      Wserver.printf "<table style=\"margin:1em\">\n";
      Wserver.printf "<tr>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "</tr>\n";
      Wserver.printf "</table>\n"
  | ChangedOrderOfFamilyEvents (_, before, after) ->
      Wserver.printf "%s\n"
        (capitale (transl conf "changed order of family's events"));
      Wserver.printf "-&gt;\n";
      let print_list arr diff_arr =
        Array.iteri
          (fun i evt ->
             let name = Util.string_of_fevent_name conf base evt.efam_name in
             Wserver.printf "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             Wserver.printf "%s\n" name;
             Wserver.printf "</li>\n")
          arr
      in
      let before = Array.of_list before in
      let after = Array.of_list after in
      let (bef_d, aft_d) = Difference.f before after in
      Wserver.printf "<table style=\"margin:1em\">\n";
      Wserver.printf "<tr>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "</tr>\n";
      Wserver.printf "</table>\n"
  | ChangedOrderOfPersonEvents (_, before, after) ->
      Wserver.printf "%s\n"
        (capitale (transl conf "changed order of person's events"));
      Wserver.printf "-&gt;\n";
      let print_list arr diff_arr =
        Array.iteri
          (fun i evt ->
             let name = Util.string_of_pevent_name conf base evt.epers_name in
             Wserver.printf "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             Wserver.printf "%s\n" name;
             Wserver.printf "</li>\n")
          arr
      in
      let before = Array.of_list before in
      let after = Array.of_list after in
      let (bef_d, aft_d) = Difference.f before after in
      Wserver.printf "<table style=\"margin:1em\">\n";
      Wserver.printf "<tr>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "</tr>\n";
      Wserver.printf "</table>\n"
  | CloseChildren (ifam, _, elder, x) ->
      let cpl = foi base ifam in
      Wserver.printf
        (fcapitale
           (ftransl conf
              "the following children of %t and %t are born very close"))
        (fun _ -> print_someone_strong conf base (poi base (get_father cpl)))
        (fun _ -> print_someone_strong conf base (poi base (get_mother cpl)));
      Wserver.printf ":\n";
      Wserver.printf "<ul>\n";
      Wserver.printf "<li>";
      print_first_name_strong conf base elder;
      Wserver.printf "%s" (Date.short_dates_text conf base elder);
      Wserver.printf "</li>";
      Wserver.printf "<li>";
      print_first_name_strong conf base x;
      Wserver.printf "%s" (Date.short_dates_text conf base x);
      Wserver.printf "</li>";
      Wserver.printf "</ul>\n"
  | DeadOld (p, a) ->
      Wserver.printf "%s\n%s\n" (print_someone_strong conf base p)
        (transl_nth conf "died at an advanced age"
           (index_of_sex (get_sex p)));
      Wserver.printf "(%s)" (Date.string_of_age conf a)
  | DeadTooEarlyToBeFather (father, child) ->
      Wserver.printf
        (ftransl conf "\
%t is born more than 2 years after the death of his/her father %t")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base child)
             (Date.short_dates_text conf base child))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base father)
             (Date.short_dates_text conf base father))
  | FEventOrder (p, e1, e2) ->
      Wserver.printf (fcapitale (ftransl conf "%t's %s before his/her %s"))
        (fun _ -> print_someone_strong conf base p)
        (Util.string_of_fevent_name conf base e1.efam_name)
        (Util.string_of_fevent_name conf base e2.efam_name)
  | FWitnessEventAfterDeath (p, e) ->
      Wserver.printf
        (fcapitale (ftransl conf "%t witnessed the %s after his/her death"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
        (Util.string_of_fevent_name conf base e.efam_name)
  | FWitnessEventBeforeBirth (p, e) ->
      Wserver.printf
        (fcapitale (ftransl conf "%t witnessed the %s before his/her birth"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
        (Util.string_of_fevent_name conf base e.efam_name)
  | IncoherentSex (p, _, _) ->
      Wserver.printf
        (fcapitale
           (ftransl conf "%t's sex is not coherent with his/her relations"))
        (fun _ -> print_someone_strong conf base p)
  | IncoherentAncestorDate (anc, p) ->
      Wserver.printf "%s has a younger ancestor %s"
        (print_someone_strong conf base p)
        (print_someone_strong conf base anc)
  | MarriageDateAfterDeath p ->
      Wserver.printf
        (fcapitale
           (ftransl conf "marriage had occurred after the death of %t"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | MarriageDateBeforeBirth p ->
      Wserver.printf
        (fcapitale
           (ftransl conf "marriage had occurred before the birth of %t"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | MotherDeadAfterChildBirth (mother, child) ->
      Wserver.printf
        (ftransl conf "%t is born after the death of his/her mother %t")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base child)
             (Date.short_dates_text conf base child))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base mother)
             (Date.short_dates_text conf base mother))
  | ParentBornAfterChild (p, c) ->
      Wserver.printf "%s\n%s\n%s" (print_someone_strong conf base p)
        (transl conf "is born after his/her child")
        (print_someone_strong conf base c)
  | ParentTooYoung (p, a) ->
      Wserver.printf "%s\n%s\n" (print_someone_strong conf base p)
        (transl conf "is a very young parent");
      Wserver.printf "(%s)" (Date.string_of_age conf a)
  | ParentTooOld (p, a) ->
      Wserver.printf "%s\n%s\n" (print_someone_strong conf base p)
        (transl conf "is a very old parent");
      Wserver.printf "(%s)" (Date.string_of_age conf a)
  | PossibleDuplicateFam (f1, _) ->
    let f = foi base f1 in
    Wserver.printf
      (fcapitale (ftransl conf "%s and %s have several unions"))
      (print_someone_strong conf base @@ poi base @@ get_father f)
      (print_someone_strong conf base @@ poi base @@ get_mother f)
  | PEventOrder (p, e1, e2) ->
      Wserver.printf (fcapitale (ftransl conf "%t's %s before his/her %s"))
        (fun _ -> print_someone_strong conf base p)
        (Util.string_of_pevent_name conf base e1.epers_name)
        (Util.string_of_pevent_name conf base e2.epers_name)
  | PWitnessEventAfterDeath (p, e) ->
      Wserver.printf
        (fcapitale (ftransl conf "%t witnessed the %s after his/her death"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
        (Util.string_of_pevent_name conf base e.epers_name)
  | PWitnessEventBeforeBirth (p, e) ->
      Wserver.printf
        (fcapitale (ftransl conf "%t witnessed the %s before his/her birth"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
        (Util.string_of_pevent_name conf base e.epers_name)
  | TitleDatesError (p, t) ->
      Wserver.printf
        (fcapitale (ftransl conf "%t has incorrect title dates: %t"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
        (fun _ ->
           Printf.sprintf "<strong>%s %s</strong> <em>%s-%s</em>"
             (sou base t.t_ident) (sou base t.t_place)
             (match Adef.od_of_cdate t.t_date_start with
                Some d -> Date.string_of_date conf d
              | _ -> "")
             (match Adef.od_of_cdate t.t_date_end with
                Some d -> Date.string_of_date conf d
              | _ -> ""))
  | UndefinedSex p ->
      Wserver.printf (fcapitale (ftransl conf "undefined sex for %t"))
        (fun _ -> print_someone_strong conf base p)
  | WitnessDateAfterDeath p ->
      Wserver.printf
        (fcapitale (ftransl conf "%t was witness after his/her death"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | WitnessDateBeforeBirth p ->
      Wserver.printf
        (fcapitale (ftransl conf "%t was witness before his/her birth"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | YoungForMarriage (p, a) ->
      Wserver.printf "%s\n" (print_someone_strong conf base p);
      Wserver.printf (ftransl conf "married at age %t")
        (fun _ -> Date.string_of_age conf a)

let print_warnings conf base wl =
  if wl = [] then ()
  else
    begin
      Wserver.printf "%s\n" (capitale (transl conf "warnings"));
      begin
        Wserver.printf "<ul>\n";
        (* On rend la liste unique, parce qu'il se peut qu'un warning soit *)
        (* levé par plusieurs fonctions différents selon le context.       *)
        begin let wl =
          let ht = Hashtbl.create 1 in
          let rec loop wl accu =
            match wl with
              [] -> accu
            | x :: wl ->
                if Hashtbl.mem ht (Hashtbl.hash x) then loop wl accu
                else
                  begin
                    Hashtbl.add ht (Hashtbl.hash x) true;
                    loop wl (x :: accu)
                  end
          in
          loop wl []
        in
          List.iter
            (fun w ->
               html_li conf; print_warning conf base w; Wserver.printf "\n")
            wl
        end;
        Wserver.printf "</ul>\n"
      end
    end


(* ************************************************************************* *)
(*  [Fonc] print_misc : config -> base -> Def.misc -> unit                   *)
(** [Description] : Fonction d'impression des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - fun  : Def.misc (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let print_misc conf _base =
  function
    MissingSources ->
      Wserver.printf "<em>";
      Wserver.printf "%s\n" (capitale (transl conf "missing sources"));
      Wserver.printf "</em>"


(* ************************************************************************* *)
(*  [Fonc] print_miscs : config -> base -> Def.misc list -> unit             *)
(** [Description] : Affiche la liste des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - ml   : Def.misc list (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let print_miscs conf base ml =
  if ml = [] then ()
  else
    begin
      Wserver.printf "%s\n"
        (capitale (transl conf "miscellaneous informations"));
      begin
        Wserver.printf "<ul>\n";
        List.iter
          (fun m -> html_li conf; print_misc conf base m; Wserver.printf "\n")
          ml;
        Wserver.printf "</ul>\n"
      end
    end


(* ************************************************************************* *)
(*  [Fonc] print_miscs :
      config -> base -> (Def.warning list * Def.misc list) -> unit           *)
(** [Description] : Affiche sous la même rubrique, la liste des warnings
                    et la liste des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - wl   : Def.warning list
      - ml   : Def.misc list (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                              *)
(* ************************************************************************* *)
let print_warnings_and_miscs conf base (wl, ml) =
  if wl = [] && ml = [] then ()
  else
    begin
      Wserver.printf "%s\n" (capitale (transl conf "warnings"));
      begin
        Wserver.printf "<ul>\n";
        List.iter
          (fun w ->
             html_li conf; print_warning conf base w; Wserver.printf "\n")
          wl;
        List.iter
          (fun m -> html_li conf; print_misc conf base m; Wserver.printf "\n")
          ml;
        Wserver.printf "</ul>\n"
      end
    end

let error conf base x =
(* #ifdef API *)
  if !(Api_conf.mode_api) then
    begin let err =
      match x with
        AlreadyDefined p ->
          Printf.sprintf
            (fcapitale
               (ftransl conf "name %s already used by %tthis person%t"))
            ("\"" ^ p_first_name base p ^ "." ^ string_of_int (get_occ p) ^
             " " ^ p_surname base p ^ "\"")
            (fun _ ->
               Printf.sprintf "%s %s" (sou base (get_first_name p))
                 (sou base (get_surname p)))
            (fun _ -> ".")
      | OwnAncestor p ->
          Printf.sprintf "%s\n%s" (print_someone_strong conf base p)
            (transl conf "would be his/her own ancestor")
      | BadSexOfMarriedPerson _ ->
          Printf.sprintf "%s."
            (capitale (transl conf "cannot change sex of a married person"))
    in
      raise (ModErrApi err)
    end;
(* #endif *)
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  print_error conf base x;
  Wserver.printf "\n";
  print_return conf;
  Hutil.trailer conf;
  raise ModErr

let error_locked conf =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "<p>\n";
  Wserver.printf
    (fcapitale
       (ftransl conf "the file is temporarily locked: please try again"));
  Wserver.printf ".\n";
  Wserver.printf "</p>\n";
  Wserver.printf "<table>\n";
  Wserver.printf "<tr>\n";
  Wserver.printf "<td>\n";
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       if x = "retry" then ()
       else if x = "notes" || is_label_note x then
         begin
           Wserver.printf "<textarea style=\"display:none;\" name=\"%s\">\n"
             x;
           Wserver.printf "%s" (Util.escape_html (decode_varenv v));
           Wserver.printf "</textarea>\n"
         end
       else
         Wserver.printf "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n"
           x (Util.escape_html (decode_varenv v)) conf.xhs)
    (conf.henv @ conf.env);
  (* just to see in the traces... *)
  Wserver.printf "<input type=\"hidden\" name=\"retry\" value=\"%s\"%s>\n"
    (Util.escape_html conf.user) conf.xhs;
  Wserver.printf "<input type=\"submit\" value=\"%s\"%s>\n"
    (capitale (transl conf "try again")) conf.xhs;
  Wserver.printf "</form>\n";
  Wserver.printf "</td>\n";
  Wserver.printf "<td>\n";
  Wserver.printf "<form method=\"get\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       Wserver.printf "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n" x
         (Util.escape_html (decode_varenv v)) conf.xhs)
    conf.henv;
  begin let ip =
    match p_getenv conf.env "ip" with
      Some ip -> Some ip
    | None -> p_getenv conf.env "i"
  in
    match ip with
      Some n ->
        Wserver.printf "<input type=\"hidden\" name=\"i\" value=\"%s\"%s>\n" n
          conf.xhs
    | None -> ()
  end;
  Wserver.printf "<input type=\"submit\" value=\"%s\"%s>\n"
    (capitale (transl_nth conf "user/password/cancel" 2)) conf.xhs;
  Wserver.printf "</form>\n";
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "</table>\n";
  Hutil.trailer conf

let error_digest conf =
(* #ifdef API *)
  if !(Api_conf.mode_api) then
    begin let err =
      Printf.sprintf
        (fcapitale
           (ftransl conf "\
the base has changed; do \"back\", \"reload\", and refill the form"))
    in
      raise (ModErrApi err)
    end;
(* #endif *)
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<p>\n";
  Wserver.printf
    (fcapitale
       (ftransl conf "\
the base has changed; do \"back\", \"reload\", and refill the form"));
  Wserver.printf ".\n";
  Wserver.printf "</p>\n";
  Hutil.trailer conf;
  raise ModErr

let digest_person p = Iovalue.digest p
let digest_family (fam, cpl, des) = Iovalue.digest (fam, cpl, des)

let get var key env =
  match p_getenv env (var ^ "_" ^ key) with
    Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound")

let get_number var key env = p_getint env (var ^ "_" ^ key)

let bad_date conf d =
(* #ifdef API *)
  if !(Api_conf.mode_api) then
    begin let err =
      Printf.sprintf "%s%s\n" (capitale (transl conf "incorrect date"))
        (transl conf ":") ^
      (match d with
         {day = 0; month = 0; year = a} -> Printf.sprintf "%d" a
       | {day = 0; month = m; year = a} -> Printf.sprintf "%d/%d" m a
       | {day = j; month = m; year = a} -> Printf.sprintf "%d/%d/%d" j m a)
    in
      raise (ModErrApi err)
    end;
(* #endif *)
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s%s\n" (capitale (transl conf "incorrect date"))
    (transl conf ":");
  begin match d with
    {day = 0; month = 0; year = a} -> Wserver.printf "%d" a
  | {day = 0; month = m; year = a} -> Wserver.printf "%d/%d" m a
  | {day = j; month = m; year = a} -> Wserver.printf "%d/%d/%d" j m a
  end;
  Hutil.trailer conf;
  raise ModErr

let int_of_field s =
  try Some (int_of_string (String.trim s)) with Failure _ -> None

let reconstitute_date_dmy2 conf var =
  let m =
    let m = get var "ormonth" conf.env in
    match String.uppercase_ascii m with
      "VD" -> Some 1
    | "BR" -> Some 2
    | "FM" -> Some 3
    | "NI" -> Some 4
    | "PL" -> Some 5
    | "VT" -> Some 6
    | "GE" -> Some 7
    | "FL" -> Some 8
    | "PR" -> Some 9
    | "ME" -> Some 10
    | "TH" -> Some 11
    | "FT" -> Some 12
    | "JC" -> Some 13
    | _ -> int_of_field m
  in
  match get_number var "oryear" conf.env with
    Some y ->
      begin match m with
        Some m ->
          begin match get_number var "orday" conf.env with
            Some d ->
              let dmy2 = {day2 = d; month2 = m; year2 = y; delta2 = 0} in
              if dmy2.day2 >= 1 && dmy2.day2 <= 31 && dmy2.month2 >= 1 &&
                 dmy2.month2 <= 13
              then
                dmy2
              else let d = Date.dmy_of_dmy2 dmy2 in bad_date conf d
          | None ->
              let dmy2 = {day2 = 0; month2 = m; year2 = y; delta2 = 0} in
              if dmy2.month2 >= 1 && dmy2.month2 <= 13 then dmy2
              else let d = Date.dmy_of_dmy2 dmy2 in bad_date conf d
          end
      | None -> {day2 = 0; month2 = 0; year2 = y; delta2 = 0}
      end
  | None -> raise ModErr

let reconstitute_date_dmy conf var =
  let (prec, y) =
    let y = get var "yyyy" conf.env in
    let prec = p_getenv conf.env (var ^ "_prec") in
    let len = String.length y in
    if len > 1 then
      match y.[0], y.[len-1] with
        '?', _ -> Some "maybe", String.sub y 1 (len - 1)
      | '~', _ -> Some "about", String.sub y 1 (len - 1)
      | '/', '/' -> Some "about", String.sub y 1 (len - 2)
      | '<', _ | '/', _ -> Some "before", String.sub y 1 (len - 1)
      | '>', _ -> Some "after", String.sub y 1 (len - 1)
      | _, '/' -> Some "after", String.sub y 0 (len - 1)
      | _ -> prec, y
    else prec, y
  in
  let (force_f_cal, m) =
    let m = get var "mm" conf.env in
    match String.uppercase_ascii m with
      "VD" -> true, Some 1
    | "BR" -> true, Some 2
    | "FM" -> true, Some 3
    | "NI" -> true, Some 4
    | "PL" -> true, Some 5
    | "VT" -> true, Some 6
    | "GE" -> true, Some 7
    | "FL" -> true, Some 8
    | "PR" -> true, Some 9
    | "ME" -> true, Some 10
    | "TH" -> true, Some 11
    | "FT" -> true, Some 12
    | "JC" -> true, Some 13
    | _ -> false, int_of_field m
  in
  let d =
    match int_of_field y with
      Some y ->
        let prec =
          match prec with
            Some "about" -> About
          | Some "maybe" -> Maybe
          | Some "before" -> Before
          | Some "after" -> After
          | Some "oryear" ->
              begin match get_number var "oryear" conf.env with
                Some _ ->
                  let dmy2 = reconstitute_date_dmy2 conf var in OrYear dmy2
              | None -> Sure
              end
          | Some "yearint" ->
              begin match get_number var "oryear" conf.env with
                Some _ ->
                  let dmy2 = reconstitute_date_dmy2 conf var in YearInt dmy2
              | None -> Sure
              end
          | _ -> Sure
        in
        begin match m with
          Some m ->
            begin match get_number var "dd" conf.env with
              Some d ->
                let d =
                  {day = d; month = m; year = y; prec = prec; delta = 0}
                in
                if d.day >= 1 && d.day <= 31 && d.month >= 1 && d.month <= 13
                then
                  Some d
                else bad_date conf d
            | None ->
                let d =
                  {day = 0; month = m; year = y; prec = prec; delta = 0}
                in
                if d.month >= 1 && d.month <= 13 then Some d
                else bad_date conf d
            end
        | None -> Some {day = 0; month = 0; year = y; prec = prec; delta = 0}
        end
    | None -> None
  in
  d, force_f_cal

let check_greg_day conf d =
  if d.day > CheckItem.nb_days_in_month d.month d.year then bad_date conf d

let reconstitute_date conf var =
  match reconstitute_date_dmy conf var with
    Some d, false ->
      let (d, cal) =
        match p_getenv conf.env (var ^ "_cal") with
          Some "G" | None -> check_greg_day conf d; d, Dgregorian
        | Some "J" -> Calendar.gregorian_of_julian d, Djulian
        | Some "F" -> Calendar.gregorian_of_french d, Dfrench
        | Some "H" -> Calendar.gregorian_of_hebrew d, Dhebrew
        | _ -> d, Dgregorian
      in
      Some (Dgreg (d, cal))
  | Some d, true -> Some (Dgreg (Calendar.gregorian_of_french d, Dfrench))
  | None, _ ->
      match p_getenv conf.env (var ^ "_text") with
        Some _ ->
          let txt = only_printable (get var "text" conf.env) in
          if txt = "" then None else Some (Dtext txt)
      | _ -> None

let rec parse_int n =
  parser
    [< ''0'..'9' as i;
       a = parse_int (10 * n + Char.code i - Char.code '0') ?! >] ->
      a
  | [< >] -> n

let parse_r_parent =
  parser
    [< ''f' >] -> 0
  | [< ''m' >] -> 1

let text_of_var conf =
  function
    "pa1" -> transl_nth conf "him/her" 0
  | "pa2" -> transl_nth conf "him/her" 1
  | var ->
      match Stream.of_string var with parser
        [< ''r'; pos = parse_int 0; ''_'; pn = parse_r_parent >] ->
          transl_nth conf "relation/relations" 0 ^ " " ^ string_of_int pos ^
          " - " ^ transl_nth conf "father/mother" pn
      | [< ''w'; ''i'; ''t'; ''n'; pos = parse_int 0 >] ->
          transl_nth conf "witness/witnesses" 0 ^ " " ^ string_of_int pos
      | [< ''c'; ''h'; pos = parse_int 0 >] ->
          Util.translate_eval (transl_nth conf "child/children" 0) ^ " " ^
          string_of_int pos
      | [< >] -> var

let print_create_conflict conf base p var =
(* #ifdef API *)
  if !(Api_conf.mode_api) then
    begin let err =
      Printf.sprintf
        (fcapitale (ftransl conf "name %s already used by %tthis person%t"))
        ("\"" ^ p_first_name base p ^ "." ^ string_of_int (get_occ p) ^ " " ^
         p_surname base p ^ "\" (" ^ text_of_var conf var ^ ")")
        (fun _ ->
           Printf.sprintf "%s %s" (sou base (get_first_name p))
             (sou base (get_surname p)))
        (fun _ -> ".")
    in
      raise (ModErrApi err)
    end;
(* #endif *)
  let text = text_of_var conf var in
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf
    (fcapitale (ftransl conf "name %s already used by %tthis person%t"))
    ("\"" ^ p_first_name base p ^ "." ^ string_of_int (get_occ p) ^ " " ^
     p_surname base p ^ "\" (" ^ text ^ ")")
    (fun _ ->
       Printf.sprintf "<a href=\"%s%s\">" (commd conf) (acces conf base p))
    (fun _ -> "</a>.");
  let free_n =
    Gutil.find_free_occ base (p_first_name base p) (p_surname base p) 0
  in
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       (* Seul un textarea peut contenir des sauts de ligne. *)
       (* On remplace donc l'input par un textarea.          *)
       if x = "notes" || is_label_note x then
         begin
           Wserver.printf "<textarea style=\"display:none;\" name=\"%s\">\n"
             x;
           Wserver.printf "%s" (Util.escape_html (decode_varenv v));
           Wserver.printf "</textarea>\n"
         end
       else
         Wserver.printf "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n"
           x (Util.escape_html (decode_varenv v)) conf.xhs)
    (conf.henv @ conf.env);
  Wserver.printf "<input type=\"hidden\" name=\"field\" value=\"%s\"%s>\n" var
    conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"free_occ\" value=\"%d\"%s>\n"
    free_n conf.xhs;
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>";
  Wserver.printf "%s%s %d. \n" (capitale (transl conf "first free number"))
    (transl conf ":") free_n;
  Wserver.printf (fcapitale (ftransl conf "click on \"%s\""))
    (transl conf "create");
  Wserver.printf "%s." (transl conf " to try again with this number");
  Wserver.printf "</li>";
  Wserver.printf "<li>";
  Wserver.printf "%s " (capitale (transl conf "or"));
  Wserver.printf (ftransl conf "click on \"%s\"") (transl conf "back");
  Wserver.printf " %s %s." (transl_nth conf "and" 0)
    (transl conf "change it (the number) yourself");
  Wserver.printf "</li>";
  Wserver.printf "<li>";
  Wserver.printf "%s " (capitale (transl conf "or"));
  Wserver.printf (ftransl conf "click on \"%s\"") (transl conf "back");
  Wserver.printf " %s %s." (transl_nth conf "and" 0)
    (transl conf "use \"link\" instead of \"create\"");
  Wserver.printf "</li>";
  Wserver.printf "</ul>\n";
  Wserver.printf "<input type=\"submit\" name=\"create\" value=\"%s\"%s>\n"
    (capitale (transl conf "create")) conf.xhs;
  Wserver.printf "<input type=\"submit\" name=\"return\" value=\"%s\"%s>\n"
    (capitale (transl conf "back")) conf.xhs;
  Wserver.printf "</form>\n";
  print_same_name conf base p;
  Hutil.trailer conf;
  raise ModErr

let add_misc_names_for_new_persons base new_persons =
  List.iter
    (fun p ->
       List.iter (fun n -> Gutil.person_ht_add base n p.key_index)
         (gen_person_misc_names base p (fun p -> p.titles)))
    new_persons

let insert_person conf base src new_persons (f, s, o, create, var) =
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
    Create (sex, info) ->
      begin try
        if f = "?" || s = "?" then
          if o <= 0 || o >= nb_of_persons base then raise Not_found
          else
            let ip = Adef.iper_of_int o in
            let p = poi base ip in
            if p_first_name base p = f && p_surname base p = s then ip
            else raise Not_found
        else
          match person_of_key base f s o with
            Some ip -> print_create_conflict conf base (poi base ip) var
          | None -> raise Not_found
      with Not_found ->
        let o = if f = "?" || s = "?" then 0 else o in
        let ip = Adef.iper_of_int (nb_of_persons base) in
        let empty_string = Gwdb.insert_string base "" in
        let (birth, birth_place, baptism, baptism_place) =
          match info with
            Some {ci_birth_date = b; ci_birth_place = bpl} ->
              if String.length bpl >= 2 && String.sub bpl 0 2 = "b/" then
                None, "", b, String.sub bpl 2 (String.length bpl - 2)
              else b, bpl, None, ""
          | None -> None, "", None, ""
        in
        let (death, death_place) =
          match info with
            Some {ci_death_date = Some d; ci_death_place = dpl} ->
              Death (Unspecified, Adef.cdate_of_date d), dpl
          | Some {ci_death_date = None; ci_death_place = dpl}
            when dpl <> "" ->
              DeadDontKnowWhen, dpl
          | Some
              {ci_death = DeadDontKnowWhen | NotDead as dead;
               ci_death_date = None; ci_death_place = dpl} ->
              dead, dpl
          | _ -> infer_death_bb conf birth baptism, ""
        in
        let occupation =
          match info with
            Some {ci_occupation = occupation} -> occupation
          | _ -> ""
        in
        let access =
          match info with
            Some {ci_public = p} -> if p then Public else IfTitles
          | None -> IfTitles
        in
        let p =
          {first_name = Gwdb.insert_string base f;
           surname = Gwdb.insert_string base s; occ = o; image = empty_string;
           first_names_aliases = []; surnames_aliases = [];
           public_name = empty_string; qualifiers = []; aliases = [];
           titles = []; rparents = []; related = [];
           occupation = Gwdb.insert_string base occupation; sex = sex;
           access = access; birth = Adef.cdate_of_od birth;
           birth_place = Gwdb.insert_string base birth_place;
           birth_note = empty_string; birth_src = empty_string;
           baptism = Adef.cdate_of_od baptism;
           baptism_place = Gwdb.insert_string base baptism_place;
           baptism_note = empty_string; baptism_src = empty_string;
           death = death; death_place = Gwdb.insert_string base death_place;
           death_note = empty_string; death_src = empty_string;
           burial = UnknownBurial; burial_place = empty_string;
           burial_note = empty_string; burial_src = empty_string;
           pevents = []; notes = empty_string;
           psources =
             if f = "?" || s = "?" then empty_string
             else Gwdb.insert_string base (only_printable src);
           key_index = ip}
        in
        let a = {parents = None; consang = Adef.fix (-1)} in
        let u = {family = [| |]} in
        patch_person base p.key_index p;
        patch_ascend base p.key_index a;
        patch_union base p.key_index u;
        if f <> "?" && s <> "?" then
          begin
            patch_cache_info conf Util.cache_nb_base_persons
              (fun v -> let v = int_of_string v + 1 in string_of_int v);
            let fn = Util.translate_eval f in
            let sn = Util.translate_eval s in
            patch_key base ip fn sn o;
            Gutil.person_ht_add base (fn ^ " " ^ sn) ip;
            new_persons := p :: !new_persons
          end;
        ip
      end
  | Link ->
      if f = "?" || s = "?" then
        if o < 0 || o >= nb_of_persons base then
          print_err_unknown conf base (f, s, o)
        else
          let ip = Adef.iper_of_int o in
          let p = poi base ip in
          if p_first_name base p = f && p_surname base p = s then ip
          else print_err_unknown conf base (f, s, o)
      else
        match person_of_key base f s o with
          Some ip -> ip
        | None -> print_err_unknown conf base (f, s, o)

let rec update_conf_env field p occ o_env n_env =
  match o_env with
    [] -> n_env
  | ((name, _) as head) :: rest ->
      if name = field ^ "p" then
        update_conf_env field p occ rest ((name, p) :: n_env)
      else if name = field ^ "occ" then
        update_conf_env field p occ rest ((name, occ) :: n_env)
      else if
        name = "link" || name = "create" || name = "free_occ" ||
        name = "field" || name = "link_occ"
      then
        update_conf_env field p occ rest n_env
      else update_conf_env field p occ rest (head :: n_env)

let update_conf_create conf =
  let field =
    match p_getenv conf.env "field" with
      Some f -> f ^ "_"
    | _ -> ""
  in
  let occ =
    match p_getenv conf.env "free_occ" with
      Some occ -> occ
    | _ -> ""
  in
  {conf with env = update_conf_env field "create" occ conf.env []}

let update_conf_link conf =
  let field =
    match p_getenv conf.env "field" with
      Some f -> f ^ "_"
    | _ -> ""
  in
  let occ =
    match p_getenv conf.env "link_occ" with
      Some occ -> occ
    | _ -> ""
  in
  {conf with env = update_conf_env field "link" occ conf.env []}

let update_conf conf =
  match p_getenv conf.env "link" with
    Some _ -> update_conf_link conf
  | None ->
      match p_getenv conf.env "create" with
        Some _ -> update_conf_create conf
      | None -> conf

let rec list_except x =
  function
    y :: l -> if x = y then l else y :: list_except x l
  | [] -> invalid_arg "list_except"

let update_related_pointers base pi ol nl =
  let ol = List.sort compare ol in
  let nl = List.sort compare nl in
  let (added_rel, removed_rel) =
    let rec loop (added_rel, removed_rel) ol nl =
      match ol, nl with
        oip :: orl, nip :: nrl ->
          if oip < nip then loop (added_rel, oip :: removed_rel) orl nl
          else if oip > nip then loop (nip :: added_rel, removed_rel) ol nrl
          else loop (added_rel, removed_rel) orl nrl
      | [], _ -> nl @ added_rel, removed_rel
      | _, [] -> added_rel, ol @ removed_rel
    in
    loop ([], []) ol nl
  in
  List.iter
    (fun ip ->
       let p = gen_person_of_person (poi base ip) in
       patch_person base ip {p with related = pi :: p.related})
    added_rel;
  List.iter
    (fun ip ->
       let p = gen_person_of_person (poi base ip) in
       let related =
         if List.mem pi p.related then list_except pi p.related
         else
           begin
             Printf.eprintf "Warning: related pointer was missing\n";
             flush stderr;
             p.related
           end
       in
       patch_person base ip {p with related = related})
    removed_rel
