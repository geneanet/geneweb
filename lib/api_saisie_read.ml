#ifdef API

module Mread = Api_saisie_read_piqi
module Mext_read = Api_saisie_read_piqi_ext
module MLink = Api_link_tree_piqi
module MLinkext = Api_link_tree_piqi_ext

open Config
open Def
open Gwdb
open Util
open Api_util

(* ********************************************************************* *)
(*  [Fonc] print_error : conf -> code -> unit                            *)
(** [Description] : Retourne une erreur compréhensible par l'appelant.
    [Args] :
      - conf  : configuration de la base
      - code  : code d'erreur
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_error conf code =
    let piqi_error = Mread.default_error() in
        piqi_error.Mread.Error.code <- code;
    let data = Mext_read.gen_error piqi_error in
    Wserver.http Wserver.Bad_Request ;
    print_result conf data

(**/**) (* Conversion de dates *)

(* Copie de date.ml sans les balises HTML => on devrait créer *)
(* un date_api.ml qu'on utiliserait à la place de date.ml     *)

let short_prec_year_text conf d =
  let prec =
    match d.prec with
    | About | OrYear _ | YearInt _ ->
        (* On utilise le dictionnaire pour être sur *)
        (* que ce soit compréhensible de tous.      *)
        (match transl conf "about (short date)" with
         | "ca" -> "ca "
         | s -> s ^ " ")
    | Maybe -> "? "
    | Before -> "< "
    | After -> "> "
    | _ -> ""
  in
  prec ^ string_of_int d.year

let partial_short_dates_text conf birth_date death_date p =
  match (birth_date, death_date) with
  | (Some (Dgreg (b, _)), Some (Dtext _)) -> short_prec_year_text conf b ^ "-"
  | (Some (Dgreg (b, _)), None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
          short_prec_year_text conf b ^ "-"
      | _ -> short_prec_year_text conf b )
  | (None, Some (Dtext _)) ->
      (match get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung -> DateDisplay.death_symbol conf
      | _ -> "" )
  | (None, None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung -> DateDisplay.death_symbol conf
      | _ -> "" )
  | (_, _) -> ""

let short_dates_text conf base p =
  if authorized_age conf base p then
    let (birth_date, death_date, _) = Date.get_birth_death_date p in
    match (birth_date, death_date) with
    | (Some (Dgreg (b, _)), Some (Dgreg (d, _))) ->
      short_prec_year_text conf b ^ "-" ^ short_prec_year_text conf d
    | (Some (Dgreg (b, _)), None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match get_death p with
       | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
         short_prec_year_text conf b ^ "-"
       | _ -> short_prec_year_text conf b )
    | (None, Some (Dgreg (d, _))) ->
      (match get_death p with
       | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
         DateDisplay.death_symbol conf ^ short_prec_year_text conf d
       | _ -> "" )
    | (None, None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match get_death p with
       | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
         DateDisplay.death_symbol conf
       | _ -> "" )
    (* On ne peut pas traiter les dates au format texte, mais on *)
    (* affiche tout de même les dates au format Dgreg.           *)
    | (_, _) -> partial_short_dates_text conf birth_date death_date p
  else ""

let code_french_date conf d m y =
  let s =
    if d = 0 then ""
    else string_of_int d
  in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ DateDisplay.french_month conf (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^ DateDisplay.code_french_year conf y


let encode_dmy conf d m y is_long =
  let date = if d != 0 then string_of_int d else "" in
  let date =
    if m != 0 then
      let date_keyword = if is_long then "(month)" else "(short month)" in
      if date = "" then transl_nth conf date_keyword (m - 1)
      else date ^ " " ^ transl_nth conf date_keyword (m - 1)
    else date
  in
  if date = "" then string_of_int y
  else date ^ " " ^ string_of_int y

let string_of_dmy conf d is_long =
  let sy = encode_dmy conf d.day d.month d.year is_long in
  let sy2 =
    match d.prec with
    | OrYear d2 | YearInt d2 ->
        let d2 = Date.dmy_of_dmy2 d2 in
        encode_dmy conf d2.day d2.month d2.year is_long
    | _ -> ""
  in
  DateDisplay.string_of_prec_dmy conf sy sy2 d

(* ************************************************************************** *)
(*  [Fonc] string_of_dmy_raw : Def.dmy -> string                              *)
(** [Description] : Renvoie la date dans un format texte brut (analysable)
    [Args] :
      - d : date
    [Retour] :
      - date
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let string_of_dmy_raw d =
  let prec =
    match d.prec with
    | About -> "~"
    | Maybe -> "?"
    | Before -> "<"
    | After -> ">"
    | _ -> ""
  in
  let date =
    Printf.sprintf "%d/%d/%d" d.year d.month d.day
  in
  let delta =
    match d.prec with
    | OrYear d2 -> Printf.sprintf "|/%d/%d/%d" d2.year2 d2.month2 d2.day2
    | YearInt d2 -> Printf.sprintf "../%d/%d/%d" d2.year2 d2.month2 d2.day2
    | _ -> ""
  in
  prec ^ "/" ^ date ^ "#" ^ delta

(* ************************************************************************** *)
(*  [Fonc] string_of_date_raw : Def.date -> string                            *)
(** [Description] : Renvoie la date dans un format texte brut (analysable)
    [Args] :
      - conf : configuration de la base
      - d : date
    [Retour] :
      - date
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let string_of_date_raw conf d =
  match d with
  | Dgreg (d, _) -> string_of_dmy_raw d
  | Dtext t -> Util.safe_html (string_with_macros conf [] t)

let gregorian_precision conf d is_long =
  if d.delta = 0 then string_of_dmy conf d is_long
  else
    let d2 =
      Calendar.gregorian_of_sdn d.prec (Calendar.sdn_of_gregorian d + d.delta)
    in
    transl conf "between (date)" ^ " " ^ string_of_dmy conf d is_long ^ " " ^
      transl_nth conf "and" 0 ^ " " ^ string_of_dmy conf d2 is_long

let string_of_french_dmy conf d =
  code_french_date conf d.day d.month d.year

let string_of_hebrew_dmy conf d =
  DateDisplay.code_hebrew_date conf d.day d.month d.year


(* ************************************************************************** *)
(*  [Fonc] string_of_date_and_conv :
             ?bool -> config -> Def.date -> (string * string * cal)           *)
(** [Description] : Renvoie la date, la date traduite et le calendrier au
                    format texte.
    [Args] :
      - is_long : définit si la date doit être au format long
      - conf : configuration de la base
      - d : date
    [Retour] :
      - (date greg * date * calendar option)
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let string_of_date_and_conv conf d =
  match d with
  | Dgreg (d, Dgregorian) ->
      let date = string_of_dmy conf d false in
      let date_long = string_of_dmy conf d true in
      let date_conv = date in
      let date_conv_long = date_long
      in
      (date, date_long, date_conv, date_conv_long, Some `gregorian)
  | Dgreg (d, Djulian) ->
      let date_conv =
        if d.year < 1582 then "" else gregorian_precision conf d false
      in
      let date_conv_long =
        if d.year < 1582 then "" else gregorian_precision conf d true
      in
      let d1 = Calendar.julian_of_gregorian d in
      let year_prec =
        if d1.month > 0 && d1.month < 3 ||
           d1.month = 3 && d1.day > 0 && d1.day < 25 then
          Printf.sprintf " (%d/%d)" (d1.year - 1) (d1.year mod 10)
        else ""
      in
      let date =
        DateDisplay.string_of_dmy conf d1 ^ year_prec ^ " " ^
          transl_nth conf "gregorian/julian/french/hebrew" 1
      in
      (date, date, date_conv, date_conv_long, Some `julian)
  | Dgreg (d, Dfrench) ->
      let d1 = Calendar.french_of_gregorian d in
      let date = string_of_french_dmy conf d1 in
      let date_long = DateDisplay.string_of_on_french_dmy conf d1 in
      let date_conv = gregorian_precision conf d false in
      let date_conv_long = DateDisplay.string_of_dmy conf d
      in
      (date, date_long, date_conv, date_conv_long, Some `french)
  | Dgreg (d, Dhebrew) ->
      let d1 = Calendar.hebrew_of_gregorian d in
      let date = string_of_hebrew_dmy conf d1 in
      let date_long = DateDisplay.string_of_on_hebrew_dmy conf d1 in
      let date_conv = gregorian_precision conf d false in
      let date_conv_long = DateDisplay.string_of_dmy conf d
      in
      (date, date_long, date_conv, date_conv_long, Some `hebrew)
  | Dtext t -> ("(" ^ Util.safe_html (string_with_macros conf [] t) ^ ")", "", "", "", None)

(**/**) (* Affichage nom/prénom *)

let person_firstname_surname_txt base p =
  if not (is_empty_string (get_public_name p)) then
    let fn = sou base (get_public_name p) in
    let sn =
      match get_qualifiers p with
      | s :: _ -> " " ^ sou base s
      | _ -> sou base (get_surname p)
    in
    (fn, sn)
  else
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    let sn =
      match get_qualifiers p with
      | s :: _ -> sn ^ " " ^ sou base s
      | _ -> sn
    in
    (fn, sn)

(**/**) (* Fonctions de transformation person <=> piqi person *)

type graph_more_info =
  | Root
  | Siblings
  | Children
  | Ancestor
  | Spouse

(* ************************************************************************** *)
(*  [Fonc] event_to_piqi_event : string -> event_type                         *)
(** [Description] : Convertit les balises wiki des notes en html.
    [Args] :
      - conf
      - base
      - env
      - wiki_notes       : les notes au format wiki
      - separator_string : caractère de séparations entre les lignes
    [Retour] :
      - html_notes : les notes au format html                                 *)
(* ************************************************************************** *)
let convert_wiki_notes_to_html_notes conf base env wiki_notes separator_string =
    let html_notes = string_with_macros conf env wiki_notes in
    let lines = Wiki.html_of_tlsw conf html_notes in
    let wi =
        {Wiki.wi_mode = "NOTES";
        Wiki.wi_cancel_links = conf.cancel_links;
        Wiki.wi_file_path = Notes.file_path conf base;
        Wiki.wi_person_exists = person_exists conf base;
        Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    let html_notes = Wiki.syntax_links conf wi (String.concat separator_string lines) in
    if conf.pure_xhtml then Util.check_xhtml html_notes else html_notes

(* ************************************************************************** *)
(*  [Fonc] event_to_piqi_event : string -> event_type                         *)
(** [Description] : Retourne à partir d'un évènement (gwdb) un évènement (piqi)
    [Args] :
      - evt_name : nom de l'évènement
    [Retour] :
      - event_type : évènement piqi
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let event_to_piqi_event pevt_name fevt_name =
  match pevt_name with
  | Some (Epers_Name _) -> `epers_custom
  | Some pevt -> Api_util.piqi_pevent_name_of_pevent_name pevt
  | None -> match fevt_name with
    | Some (Efam_Name _) -> `efam_custom
    | Some fevt -> piqi_fevent_name_of_fevent_name fevt
    | None -> failwith "event_to_piqi_event"

(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_tree :
            config -> base -> person -> string -> PersonTree                  *)
(** [Description] : Retourne à partir d'une person (gwdb) une PersonTree (piqi)
    [Args] :
      - conf        : configuration de la base
      - base        : base de donnée
      - p           : person
      - base_prefix : nom de l'arbre (différent de base dans le cas des LIA)
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_tree conf base p more_info gen max_gen base_prefix =
  if is_restricted conf base (get_iper p) then
    {
      Mread.Person_tree.index = Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
      sex = `unknown;
      lastname = "x";
      firstname = "x";
      n = "";
      p = "";
      occ = Int32.of_int 0;
      dates = None;
      image = None;
      sosa = `no_sosa;
      has_more_infos = false;
      baseprefix = "";
    }
  else
    let p_auth = authorized_age conf base p in
    let index = Int32.of_string @@ Gwdb.string_of_iper (get_iper p) in
    let sex =
      match get_sex p with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown
    in
    let sosa =
      if conf.bname <> Link.chop_base_prefix base_prefix then `no_sosa
      else
        let sosa_nb = Perso.get_sosa_person p in
        if Sosa.eq sosa_nb Sosa.zero then `no_sosa
        else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
        else `sosa
    in
    let sn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_surname p))
    in
    let fn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_first_name p))
    in
    let occ = Int32.of_int (get_occ p) in
    let (first_name, surname) =
      if not p_auth && (is_hide_names conf p) then ("x", "x")
      else person_firstname_surname_txt base p
    in
    let dates = short_dates_text conf base p in
    let image =
      if has_image conf base p then
        let img = sou base (get_image p) in
        if img <> "" then img
        else if Api_util.find_image_file conf base p <> None
        then "1" else ""
      else ""
    in
    let has_more_infos =
      match more_info with
      | Root -> false
      | Siblings -> Array.length (get_family p) > 0
      | Children ->
           gen = max_gen - 1 && Array.length (get_family p) > 0
      | Ancestor ->
          let has_parents = get_parents p <> None in
          (gen = max_gen - 1 && has_parents) ||
           (fst (Array.fold_left
                   (fun (children_or_spouses, nb_fam) ifam ->
                     let nb_fam = succ nb_fam in
                     let fam = foi base ifam in
                     let children = get_children fam in
                     (children_or_spouses || (gen > 1 && Array.length children > 1) || nb_fam > 1,
                      nb_fam))
                   (false, 0) (get_family p)))
      | Spouse ->
          (get_parents p <> None) || Array.length (get_family p) > 1
    in
    {
      Mread.Person_tree.index = index;
      sex = sex;
      lastname = surname;
      firstname = first_name;
      n = sn;
      p = fn;
      occ = occ;
      dates = if dates = "" then None else Some dates;
      image = if image = "" then None else Some image;
      sosa = sosa;
      has_more_infos = has_more_infos;
      baseprefix = base_prefix
    }

(* Common functions to build a SimplePerson or a FichePerson. *)
let get_restricted_person () =
  let restricted_person = Mread.default_person () in
  restricted_person.Mread.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  restricted_person.Mread.Person.lastname <- "x";
  restricted_person.Mread.Person.firstname <- "x";
  restricted_person

let fill_sex p =
      match get_sex p with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown

let fill_sosa p =
  let sosa_nb = Perso.get_sosa_person p in
  if Sosa.eq sosa_nb Sosa.zero then `no_sosa
  else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
  else `sosa

let fill_sn conf base p p_auth =
  if (is_hide_names conf p) && not p_auth then ""
  else Name.lower (sou base (get_surname p))

let fill_fn conf base p p_auth =
  if (is_hide_names conf p) && not p_auth then ""
  else Name.lower (sou base (get_first_name p))

let fill_occ p =
  Int32.of_int (get_occ p)

let fill_surname conf p p_auth gen_p =
  if not p_auth && (is_hide_names conf p) then "x" else gen_p.surname

let fill_firstname conf p p_auth gen_p =
  if not p_auth && (is_hide_names conf p) then "x" else gen_p.first_name

let fill_publicname p_auth gen_p =
  let publicname = if not p_auth then "" else gen_p.public_name in
  if publicname = "" then None else Some publicname

let fill_aliases p_auth gen_p =
  if not p_auth then [] else gen_p.aliases

let fill_qualifiers p_auth gen_p =
  if not p_auth then [] else gen_p.qualifiers

let fill_firstname_aliases p_auth gen_p =
  if not p_auth then [] else gen_p.first_names_aliases

let fill_surname_aliases p_auth gen_p =
  if not p_auth then [] else gen_p.surnames_aliases

(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_simple_person :
             config -> base -> person -> string -> SimplePerson               *)
(** [Description] : Retourne à partir d'une person (gwdb) une SimplePerson
                    (piqi).
    [Args] :
      - conf        : configuration de la base
      - base        : base de donnée
      - p           : person
      - base_prefix : nom de l'arbre (différent de base dans le cas des LIA)
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_simple_person conf base p base_prefix =
  if is_restricted conf base (get_iper p) then
    let restricted_person = Mread.default_simple_person() in
    restricted_person.Mread.Simple_person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
    restricted_person.Mread.Simple_person.lastname <- "x";
    restricted_person.Mread.Simple_person.firstname <- "x";
    restricted_person.Mread.Simple_person.visible_for_visitors <- false;
    restricted_person
  else
    let p_auth = authorized_age conf base p in
    let index = Int32.of_string @@ Gwdb.string_of_iper (get_iper p) in
    let visible_for_visitors = is_visible conf base p in
    let sex =
      match get_sex p with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown
    in
    let sosa_nb_num = Perso.get_sosa_person p in
    let sosa =
      if Sosa.eq sosa_nb_num Sosa.zero then `no_sosa
      else if Sosa.eq sosa_nb_num Sosa.one then `sosa_ref
      else `sosa
    in
    let sosa_nb =
        if sosa_nb_num = Sosa.zero
        then None
        else Some (Sosa.to_string sosa_nb_num)
    in
    let sn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_surname p))
    in
    let fn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_first_name p))
    in
    let occ = Int32.of_int (get_occ p) in
    let (birth_short, birth_raw, birth_place, death_short, death_raw, death_place) =
      if p_auth then
        let (birth_date, death_date, _) = Date.get_birth_death_date p in
        let birth =
          match birth_date with
          | Some d -> DateDisplay.string_slash_of_date conf d
          | None -> ""
        in
        let birth_raw =
          match birth_date with
          | Some d -> (string_of_date_raw conf d)
          | None -> ""
        in
        let birth_place =
          let birth_place = sou base (get_birth_place p) in
          if birth_place <> "" then Util.string_of_place conf birth_place
          else
            let baptism_place = sou base (get_baptism_place p) in
            Util.string_of_place conf baptism_place
        in
        let death =
          match death_date with
          | Some d -> DateDisplay.string_slash_of_date conf d
          | None -> ""
        in
        let death_raw =
          match death_date with
          | Some d -> string_of_date_raw conf d
          | None -> ""
        in
        let death_place =
          let death_place = sou base (get_death_place p) in
          if death_place <> "" then Util.string_of_place conf death_place
          else
            let burial_place = sou base (get_burial_place p) in
            Util.string_of_place conf burial_place
        in
        (birth, birth_raw, birth_place, death, death_raw, death_place)
      else ("", "", "", "", "", "")
    in
    let image =
      if has_image conf base p then
        let img = sou base (get_image p) in
        if img <> "" then img
        else if Api_util.find_image_file conf base p <> None
        then "1"
        else ""
      else ""
    in
    let has_parent = get_parents p <> None in
    let has_spouse = Array.length (get_family p) >= 1 in
    let has_child =
    (Array.fold_left
        (fun has_children ifam ->
          let fam = foi base ifam in
          let children = get_children fam in
          (has_children || Array.length children >= 1))
        false (get_family p))
    in
    let gen_p = Util.string_gen_person base (gen_person_of_person p)
    in
    {
      Mread.Simple_person.index = index;
      sex = sex;
      lastname = fill_surname conf p p_auth gen_p;
      firstname = fill_firstname conf p p_auth gen_p;
      n = sn;
      p = fn;
      occ = occ;
      birth_short_date = if birth_short = "" then None else Some birth_short;
      birth_date_raw = if birth_raw = "" then None else Some birth_raw;
      birth_place = if birth_place = "" then None else Some birth_place;
      death_short_date = if death_short = "" then None else Some death_short;
      death_date_raw = if death_raw = "" then None else Some death_raw;
      death_place = if death_place = "" then None else Some death_place;
      image = if image = "" then None else Some image;
      sosa = sosa;
      sosa_nb = sosa_nb;
      visible_for_visitors = visible_for_visitors;
      baseprefix = base_prefix;
      has_parent = has_parent;
      has_spouse = has_spouse;
      has_child = has_child
    }


(* ********************************************************************* *)
(*  [Fonc] fam_to_piqi_family_link : config -> base -> ifam -> Family    *)
(** [Description] : Retourne à partir d'une famille distante une Family
                    (piqi app) dont tous les champs sont complétés.
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      - Family : Retourne une famille dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let fam_to_piqi_family_link conf base (ifath : Gwdb.iper) imoth sp ifam fam fam_link spouse_to_piqi_callback witness_to_piqi_callback child_to_piqi_callback family_link_constructor =
  let base_prefix = fam_link.MLink.Family.baseprefix in
  let spouse = spouse_to_piqi_callback conf base sp base_prefix in
  let p_auth = true in
  let m_auth = true in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let index = Int32.of_string @@ Gwdb.string_of_ifam gen_f.fam_index in
  let (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal, marriage_date_raw) =
    match (m_auth, Adef.od_of_cdate gen_f.marriage) with
    | (true, Some d) ->
      let (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal) = string_of_date_and_conv conf d in
      (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal, string_of_date_raw conf d)
    | _ -> ("", "", "", "", None, "")
  in
  let marriage_date_text = Perso.get_marriage_date_text conf fam p_auth in
  let marriage_place =
    if m_auth then Util.string_of_place conf gen_f.marriage_place else ""
  in
  let marriage_src = if p_auth then gen_f.marriage_src else "" in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
    | MarriageBann -> `marriage_bann
    | MarriageContract -> `marriage_contract
    | MarriageLicense -> `marriage_license
    | Pacs -> `pacs
    | Residence -> `residence
  in
  let (divorce_type, divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal, divorce_date_raw) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, "", "", "", "", None, "")
    | Divorced cod ->
        (match Adef.od_of_cdate cod with
         | Some d when m_auth ->
             let (divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal) =
               string_of_date_and_conv conf d
             in
             (`divorced, divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal, string_of_date_raw conf d)
         | _ -> (`divorced, "", "", "", "", None, ""))
    | Separated -> (`separated, "", "", "", "", None, "")
  in
  let witnesses =
    Mutil.array_to_list_map
      (fun ip -> witness_to_piqi_callback conf base (poi base ip) base_prefix)
      gen_f.witnesses
  in
  let notes =
    if m_auth && not conf.no_note then
      let s = gen_f.comment in
      convert_wiki_notes_to_html_notes conf base [] s "\n"
    else ""
  in
  let fsources =
    if m_auth then
      let s = gen_f.fsources in
      let s =
        let wi =
          {Wiki.wi_mode = "NOTES";
           Wiki.wi_cancel_links = conf.cancel_links;
           Wiki.wi_file_path = Notes.file_path conf base;
           Wiki.wi_person_exists = person_exists conf base;
           Wiki.wi_always_show_link = conf.wizard || conf.friend}
        in
        Wiki.syntax_links conf wi s
      in
      string_with_macros conf [] s
    else ""
  in
  let children =
    List.map
      (fun c_link ->
        let base_prefix = c_link.MLink.Person.baseprefix in
        let (p, _) = Perso_link.make_ep_link base c_link in
        child_to_piqi_callback conf base p base_prefix)
      (Perso_link.get_children_of_parents base_prefix ifam ifath imoth)
  in
  family_link_constructor index spouse marriage_date marriage_date_long marriage_date_raw marriage_date_conv marriage_date_conv_long
    marriage_cal marriage_date_text marriage_place marriage_src marriage_type divorce_type divorce_date divorce_date_long divorce_date_raw divorce_date_conv
    divorce_date_conv_long divorce_cal witnesses notes fsources children

(* ********************************************************************* *)
(*  [Fonc] get_events_piqi
    conf -> base -> person -> string -> bool -> function -> function -> function
                                                                         *)
(** [Description] : Returns an array of events built by an event_constructor.
    [Args] :
      - conf                  : configuraion
      - base                  : database
      - p                     : the person
      - base_prefix           : the name of the base of the person
      - p_auth                : private informations are returned
      - pers_to_piqi_callback : function to call to create the person object (spouse / witnesses)
      - witness_constructor   : function to call to create the witness object
      - event_constructor     : function to call to create the event object
    [Returns] :
      - Array of events
                                                                         *)
(* ********************************************************************* *)
let fill_events conf base p base_prefix p_auth pers_to_piqi_callback witness_constructor event_constructor =
  if p_auth then
    List.map
      (fun (name, date, place, note, src, w, isp) ->
        let (name, type_) =
          match name with
          | Perso.Pevent name -> (Util.string_of_pevent_name conf base name, event_to_piqi_event (Some name) None)
          | Perso.Fevent name -> (Util.string_of_fevent_name conf base name, event_to_piqi_event None (Some name))
        in
        let (date, date_long, date_conv, date_conv_long, date_cal, date_raw) =
          match Adef.od_of_cdate date with
          | Some d ->
            let (date, date_long, date_conv, date_conv_long, date_cal) = string_of_date_and_conv conf d in
            (date, date_long, date_conv, date_conv_long, date_cal, string_of_date_raw conf d)
          | _ -> ("", "", "", "", None, "")
        in
        let place = Util.string_of_place conf (sou base place) in
        let note =
          if not conf.no_note then
            begin
              let env = [('i', fun () -> Util.default_image_name base p)] in
              let s = sou base note in
              convert_wiki_notes_to_html_notes conf base env s "\n"
            end
          else ""
        in
        let src =
          let s = sou base src in
          let env = [('i', fun () -> Util.default_image_name base p)] in
          let s =
            let wi =
              {Wiki.wi_mode = "NOTES";
               Wiki.wi_cancel_links = conf.cancel_links;
               Wiki.wi_file_path = Notes.file_path conf base;
               Wiki.wi_person_exists = person_exists conf base;
               Wiki.wi_always_show_link = conf.wizard || conf.friend}
            in
            Wiki.syntax_links conf wi s
          in
          string_with_macros conf env s
        in
        let spouse =
          Opt.map (fun ip -> pers_to_piqi_callback conf base (poi base ip) base_prefix) isp
        in
        let witnesses =
          Mutil.array_to_list_map
            (fun (ip, wk) ->
               let witness_type =
                 match wk with
                 | Witness -> `witness
                 | Witness_GodParent -> `witness_godparent
               in
               let witness = poi base ip in
               let witness = pers_to_piqi_callback conf base witness base_prefix in
               witness_constructor witness_type witness
               )
            w
        in
          event_constructor name type_ date date_long date_raw date_conv date_conv_long date_cal place note src spouse witnesses
        )
      (Perso.events_list conf base p)
  else []


let fill_events_if_is_main_person conf base p base_prefix p_auth is_main_person pers_to_piqi_callback witness_constructor event_constructor =
  if is_main_person then
    fill_events conf base p base_prefix p_auth pers_to_piqi_callback witness_constructor event_constructor
  else []

(* ********************************************************************* *)
(*  [Fonc] get_related_piqi                                              *)
(** [Description] : Returns an array of related person built by an relation_person_constructor.
    [Args] :
      - conf                  : configuration
      - base                  : database
      - p                     : the person
      - base_prefix           : the name of the base of the person
      - p_auth                : private informations are returned
      - pers_to_piqi_callback : function to call to create the person object (spouse / witnesses)
      - witness_constructor   : function to call to create the witness object
      - event_constructor     : function to call to create the event object
    [Returns] :
      - Array of related person
                                                                         *)
(* ********************************************************************* *)
let get_related_piqi conf base p base_prefix gen_p has_relations pers_to_piqi_callback relation_person_constructor =
  if has_relations then
    let list =
      let list = List.sort_uniq compare gen_p.related in
      List.fold_left
        (fun list ic ->
           let c = pget conf base ic in
           let rec loop list =
             function
             | r :: rl ->
                 (match r.r_fath with
                 | Some ip when ip = get_iper p ->
                     loop ((c, r) :: list) rl
                 | _ ->
                     (match r.r_moth with
                     | Some ip when ip = get_iper p ->
                         loop ((c, r) :: list) rl
                     | _ -> loop list rl))
             | [] -> list
           in loop list (get_rparents c))
        [] list
    in
    let list =
      List.sort
        (fun (c1, _) (c2, _) ->
           let d1 =
             match Adef.od_of_cdate (get_baptism c1) with
             | None -> Adef.od_of_cdate (get_birth c1)
             | x -> x
           in
           let d2 =
             match Adef.od_of_cdate (get_baptism c2) with
             | None -> Adef.od_of_cdate (get_birth c2)
             | x -> x
           in
           match (d1, d2) with
           | (Some d1, Some d2) -> Date.compare_date d1 d2
           | _ -> -1 )
      (List.rev list)
    in
    List.map
      (fun (p, rp) ->
        let p = pers_to_piqi_callback conf base p base_prefix in
        let r_type =
          match rp.r_type with
          | Adoption -> `rchild_adoption
          | Recognition -> `rchild_recognition
          | CandidateParent -> `rchild_candidate_parent
          | GodParent -> `rchild_god_parent
          | FosterParent -> `rchild_foster_parent
        in
        relation_person_constructor r_type p
        )
      list
  else []

(* ********************************************************************* *)
(*  [Fonc] get_family_piqi                                               *)
(** [Description] : Returns a family built by a family_constructor.
    [Args] :
      - conf                      : configuration
      - base                      : database
      - p                         : the person
      - base_prefix               : the name of the base of the person
      - p_auth                    : private informations are returned
      - pers_to_piqi_callback     : function to call to create a person object (spouse)
      - witness_constructor       : function to call to create a witness object
      - child_to_piqi_callback : function to call to create a child object
      - event_constructor         : function to call to create an event object
    [Returns] :
      - Array of related person
                                                                         *)
(* ********************************************************************* *)
let get_family_piqi base conf ifam p base_prefix spouse_to_piqi_callback witnesses_to_piqi_callback child_to_piqi_callback family_constructor =
  let fam = foi base ifam in
  let sp = poi base (Gutil.spouse (get_iper p) fam) in
  let spouse = spouse_to_piqi_callback conf base sp base_prefix in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let p_auth = authorized_age conf base p in
  let m_auth =
    authorized_age conf base (pget conf base ifath) &&
    authorized_age conf base (pget conf base imoth)
  in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let index = Int32.of_string @@ Gwdb.string_of_ifam gen_f.fam_index in
  let (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal, marriage_date_raw) =
    match (m_auth, Adef.od_of_cdate gen_f.marriage) with
    | (true, Some d) ->
      let (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal) = string_of_date_and_conv conf d in
      (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal, string_of_date_raw conf d)
    | _ -> ("", "", "", "", None, "")
  in
  let marriage_date_text = Perso.get_marriage_date_text conf fam p_auth in
  let marriage_place =
    if m_auth then Util.string_of_place conf gen_f.marriage_place else ""
  in
  let marriage_src = if p_auth then gen_f.marriage_src else "" in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
    | MarriageBann -> `marriage_bann
    | MarriageContract -> `marriage_contract
    | MarriageLicense -> `marriage_license
    | Pacs -> `pacs
    | Residence -> `residence
  in
  let (divorce_type, divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal, divorce_date_raw) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, "", "", "", "", None, "")
    | Divorced cod ->
        (match Adef.od_of_cdate cod with
         | Some d when m_auth ->
             let (divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal) =
               string_of_date_and_conv conf d
             in
             (`divorced, divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal, string_of_date_raw conf d)
         | _ -> (`divorced, "", "", "", "", None, ""))
    | Separated -> (`separated, "", "", "", "", None, "")
  in
  let witnesses =
    Mutil.array_to_list_map
      (fun ip -> witnesses_to_piqi_callback conf base (poi base ip) base_prefix)
      gen_f.witnesses
  in
  let notes =
    if m_auth && not conf.no_note then
      let s = gen_f.comment in
      convert_wiki_notes_to_html_notes conf base [] s "\n"
    else ""
  in
  let fsources =
    if m_auth then
      let s = gen_f.fsources in
      let s =
        let wi =
          {Wiki.wi_mode = "NOTES";
           Wiki.wi_cancel_links = conf.cancel_links;
           Wiki.wi_file_path = Notes.file_path conf base;
           Wiki.wi_person_exists = person_exists conf base;
           Wiki.wi_always_show_link = conf.wizard || conf.friend}
        in
        Wiki.syntax_links conf wi s
      in
      string_with_macros conf [] s
    else ""
  in
  let children =
    Mutil.array_to_list_map
      (fun ip -> child_to_piqi_callback conf base (poi base ip) base_prefix)
      (get_children fam)
  in
  (* lien inter arbre *)
  let children_link =
    let family_link =
      Perso_link.get_families_of_parents
        conf.command (get_iper p) (get_iper sp)
    in
    List.fold_right
      (fun fam_link accu ->
         List.fold_right
           (fun c_link accu ->
              let baseprefix = c_link.MLink.Person_link.baseprefix in
              let ip_c = Gwdb.iper_of_string @@ Int32.to_string c_link.MLink.Person_link.ip in
              match Perso_link.get_person_link baseprefix ip_c with
              | Some c_link ->
                  let can_merge =
                    Perso_link.can_merge_child conf.command
                       (get_children fam) c_link
                  in
                  if can_merge then accu
                  else
                    let (p, _) = Perso_link.make_ep_link base c_link in
                    child_to_piqi_callback conf base p baseprefix :: accu
              | None -> accu)
           fam_link.MLink.Family.children accu)
      family_link []
  in
  let children = children @ children_link in
    family_constructor index spouse marriage_date marriage_date_long marriage_date_raw marriage_date_conv marriage_date_conv_long
    marriage_cal marriage_date_text marriage_place marriage_src marriage_type divorce_type divorce_date divorce_date_long divorce_date_raw divorce_date_conv
    divorce_date_conv_long divorce_cal witnesses notes fsources children

(* ********************************************************************* *)
(*  [Fonc] get_families_piqi                                             *)
(** [Description] : Returns a array of family built by a family_constructor.
    [Args] :
      - conf                       : configuration
      - base                       : database
      - p                          : the person
      - base_prefix                : the name of the base of the person
      - spouse_to_piqi_callback    : function to call to create the person object (spouse / witnesses)
      - witnesses_to_piqi_callback : function to call to create the witness object
      - child_to_piqi_callback  : function to call to create the child object
    [Returns] :
      - Array of related person
                                                                         *)
(* ********************************************************************* *)
let get_families_piqi base conf p base_prefix spouse_to_piqi_callback witnesses_to_piqi_callback child_to_piqi_callback family_constructor =
  let families =
    Mutil.array_to_list_map
      (fun ifam ->
         get_family_piqi base conf ifam p base_prefix spouse_to_piqi_callback witnesses_to_piqi_callback child_to_piqi_callback family_constructor
      )
      (get_family p)
  in
  (* lien inter arbre *)
  let families_link =
    let ip = get_iper p in
    let families = Perso_link.get_family_link base_prefix ip in
    List.fold_right
      (fun fam_link accu ->
         let (_, fam, _, _) =
           Perso_link.make_efam_link conf base fam_link
         in
         let (ifath, imoth, ifam) =
           (Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath,
            Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth,
            Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam)
         in
         let cpl =
           let ip = get_iper p in
           if ip <> ifath && ip <> imoth then
             match
               Perso_link.get_person_link_with_base
                 conf.command ip fam_link.MLink.Family.baseprefix
             with
             | Some p ->
                 let ip = Gwdb.iper_of_string @@ Int32.to_string p.MLink.Person.ip in
                 (ifath, imoth, if ip = ifath then imoth else ifath)
             | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
           else (ifath, imoth, if ip = ifath then imoth else ifath)
         in
         let can_merge =
           let fam = Mutil.array_to_list_map (foi base) (get_family p) in
           Perso_link.can_merge_family conf.command (get_iper p) fam fam_link cpl
         in
         if can_merge then accu
         else
           let (ifath, imoth, isp) = cpl in
           match
             (Perso_link.get_person_link fam_link.MLink.Family.baseprefix ifath,
              Perso_link.get_person_link fam_link.MLink.Family.baseprefix imoth,
              Perso_link.get_person_link fam_link.MLink.Family.baseprefix isp)
           with
           | (Some _, Some _, Some sp) ->
               let (sp, _) = Perso_link.make_ep_link base sp in
               fam_to_piqi_family_link conf base ifath imoth sp ifam fam fam_link spouse_to_piqi_callback witnesses_to_piqi_callback child_to_piqi_callback family_constructor :: accu
           | _ -> accu)
      families []
  in
    families @ families_link

(* ********************************************************************* *)
(*  [Fonc] get_rparents_piqi                                             *)
(** [Description] : Returns a related parent built by a relation_person_constructor.
    [Args] :
      - conf                        : configuration
      - base                        : database
      - p                           : the person
      - base_prefix                 : the name of the base of the person
      - gen_p                       : the generation of the person
      - pers_to_piqi_callback       : function to call to create a person object
      - relation_person_constructor : function to call to create the child object
    [Returns] :
      - Array of related parents
                                                                         *)
(* ********************************************************************* *)
let get_rparents_piqi base conf base_prefix gen_p has_relations pers_to_piqi_callback relation_person_constructor =
  if has_relations then
    List.fold_left
      (fun rl rp ->
        let r_type =
          match rp.r_type with
          | Adoption -> `rparent_adoption
          | Recognition -> `rparent_recognition
          | CandidateParent -> `rparent_candidate_parent
          | GodParent -> `rparent_god_parent
          | FosterParent -> `rparent_foster_parent
        in
        let to_relation_person conf base ip =
          let p = poi base ip in
          let p = pers_to_piqi_callback conf base p base_prefix in
          relation_person_constructor r_type p
        in
        let rl =
          match rp.r_fath with
          | Some ip ->
              let p = to_relation_person conf base ip in
              p :: rl
          | None -> rl
        in
        match rp.r_moth with
        | Some ip -> to_relation_person conf base ip :: rl
        | None -> rl)
      [] gen_p.rparents
  else []

(* ********************************************************************* *)
(*  [Fonc] get_events_witnesses                                          *)
(** [Description] : Returns a related parent built by a relation_person_constructor.
    [Args] :
      - conf                      : configuration
      - base                      : database
      - p                         : the person
      - base_prefix               : the name of the base of the person
      - gen_p                     : the generation of the person
      - p_auth                    : private informations are returned
      - has_relations             : indicate if the main person has relations
      - pers_to_piqi_callback     : function to call to create a person object
      - event_witness_constructor : function to call to create a event witness object
    [Returns] :
      - Array of events
                                                                         *)
(* ********************************************************************* *)
let get_events_witnesses conf base p base_prefix gen_p p_auth has_relations pers_to_piqi_callback event_witness_constructor =
  if has_relations then
    begin
      let related = List.sort_uniq compare gen_p.related in
      let events_witnesses =
        let list = ref [] in
        let rec make_list =
          function
          | ic :: icl ->
              let c = pget conf base ic in
              List.iter
                (fun ((name, _, _, _, _, wl, _) as evt) ->
                  let (mem, wk) = Util.array_mem_witn conf base (get_iper p) wl in
                  if mem then
                    (* Attention aux doublons pour les evenements famille. *)
                    match name with
                    | Perso.Fevent _ ->
                        if get_sex c = Male then
                          list := (c, wk, evt) :: !list
                        else ()
                    | _ -> list := (c, wk, evt) :: !list
                  else ())
                (Perso.events_list conf base c);
              make_list icl
          | [] -> ()
        in
        make_list related;
        !list
      in
      (* On tri les témoins dans le même ordre que les évènements. *)
      let events_witnesses =
        CheckItem.sort_events
          (fun (_, _, (name, _, _, _, _, _, _)) ->
             match name with
             | Perso.Pevent n -> CheckItem.Psort n
             | Perso.Fevent n -> CheckItem.Fsort n)
          (fun (_, _, (_, date, _, _, _, _, _)) -> date)
          events_witnesses
      in
      List.map
        (fun (p, wk, (name, date, _, _, _, _, isp)) ->
          let witness_date =
            match Adef.od_of_cdate date with
            | Some (Dgreg (dmy, _)) -> " (" ^ DateDisplay.year_text dmy ^ ")"
            | _ -> ""
          in
          let witnesses_name =
            match name with
            | Perso.Pevent name ->
                if p_auth then Util.string_of_pevent_name conf base name
                else  ""
            | Perso.Fevent name ->
                if p_auth then Util.string_of_fevent_name conf base name
                else  ""
          in
          let event_witness_type =
            capitale wk ^ witness_date ^ ": " ^ witnesses_name
          in
          let husband = pers_to_piqi_callback conf base p base_prefix in
          let wife =
            match isp with
            | Some isp ->
                let sp = poi base isp in
                Some (pers_to_piqi_callback conf base sp base_prefix )
            | None -> None
          in
          event_witness_constructor event_witness_type husband wife
          )
        events_witnesses
    end
  else []

(* ********************************************************************* *)
(*  [Fonc] fam_to_piqi_family : config -> base -> ifam -> Family         *)
(** [Description] : Retourne à partir d'une ifam (gwdb) une Family
                    (piqi app) dont tous les champs sont complétés.
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      - Family : Retourne une famille dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let fam_to_piqi_family conf base p ifam =
  let base_prefix = conf.command in
  let spouse_to_piqi_callback conf base p base_prefix =
      pers_to_piqi_simple_person conf base p base_prefix
  in
  let witnesses_to_piqi_callback conf base p base_prefix =
      pers_to_piqi_simple_person conf base p base_prefix
  in
  let child_to_piqi_callback conf base p base_prefix =
      pers_to_piqi_simple_person conf base p base_prefix
  in
  let family_constructor index spouse marriage_date marriage_date_long marriage_date_raw marriage_date_conv marriage_date_conv_long marriage_cal
       marriage_date_text marriage_place marriage_src marriage_type divorce_type divorce_date divorce_date_long divorce_date_raw divorce_date_conv
       divorce_date_conv_long divorce_cal witnesses notes fsources children =
    {
      Mread.Family.index = index;
      spouse = spouse;
      marriage_date = if marriage_date = "" then None else Some marriage_date;
      marriage_date_long = if marriage_date_long = "" then None else Some marriage_date_long;
      marriage_date_raw = if marriage_date_raw = "" then None else Some marriage_date_raw;
      marriage_date_conv =
        if marriage_date_conv = "" then None else Some marriage_date_conv;
      marriage_date_conv_long =
        if marriage_date_conv_long = "" then None else Some marriage_date_conv_long;
      marriage_date_cal = marriage_cal;
      marriage_date_text = if marriage_date_text = "" then None else Some marriage_date_text;
      marriage_place = if marriage_place = "" then None else Some marriage_place;
      marriage_src = if marriage_src = "" then None else Some marriage_src;
      marriage_type = marriage_type;
      divorce_type = divorce_type;
      divorce_date = if divorce_date = "" then None else Some divorce_date;
      divorce_date_long = if divorce_date_long = "" then None else Some divorce_date_long;
      divorce_date_raw = if divorce_date_raw = "" then None else Some divorce_date_raw;
      divorce_date_conv =
        if divorce_date_conv = "" then None else Some divorce_date_conv;
      divorce_date_conv_long =
        if divorce_date_conv_long = "" then None else Some divorce_date_conv_long;
      divorce_date_cal = divorce_cal;
      witnesses = witnesses;
      notes = if notes = "" then None else Some notes;
      fsources = if fsources = "" then None else Some fsources;
      children = children;
    }
  in
  get_family_piqi base conf ifam p base_prefix spouse_to_piqi_callback witnesses_to_piqi_callback child_to_piqi_callback family_constructor

let fill_image conf base p =
  if has_image conf base p then
        let img = sou base (get_image p) in
        if img <> "" then img
        else if Api_util.find_image_file conf base p <> None then "1"
        else ""
  else ""

let fill_birth_place conf p_auth gen_p =
  if p_auth then Util.string_of_place conf gen_p.birth_place else ""

let fill_baptism_place conf p_auth gen_p =
  if p_auth then Util.string_of_place conf gen_p.baptism_place else ""

let fill_death_place conf p_auth gen_p =
  if p_auth then Util.string_of_place conf gen_p.death_place else ""

let fill_birth_src p_auth gen_p =
  if p_auth then gen_p.birth_src else ""

let fill_burial_src p_auth gen_p =
  if p_auth then gen_p.burial_src else ""

let fill_death_src p_auth gen_p =
  if p_auth then gen_p.death_src else ""

let fill_baptism_src p_auth gen_p =
  if p_auth then gen_p.baptism_src else ""

let fill_burial_place conf p_auth gen_p =
  if p_auth then Util.string_of_place conf gen_p.burial_place else ""

let fill_death conf p_auth gen_p =
  match (p_auth, gen_p.death) with
      | (true, NotDead) -> (`not_dead, "", "", None)
      | (true, Death (_, cd)) ->
          let d = Adef.date_of_cdate cd in
          let (death, _, death_conv, _, death_cal) = string_of_date_and_conv conf d in
          (`dead, death, death_conv, death_cal)
      | (true, DeadYoung) -> (`dead_young, "", "", None)
      | (true, DeadDontKnowWhen) -> (`dead_dont_know_when, "", "", None)
      | (true, DontKnowIfDead) -> (`dont_know_if_dead, "", "", None)
      | (true, OfCourseDead) -> (`of_course_dead, "", "", None)
      | _ -> (`dont_know_if_dead, "", "", None)

let fill_birth conf p_auth gen_p =
  match (p_auth, Adef.od_of_cdate gen_p.birth) with
      | (true, Some d) -> string_of_date_and_conv conf d
      | _ -> ("", "", "", "", None)

let fill_baptism conf p_auth gen_p =
  match (p_auth, Adef.od_of_cdate gen_p.baptism) with
      | (true, Some d) -> string_of_date_and_conv conf d
      | _ -> ("", "", "", "", None)

let fill_burial conf p_auth gen_p =
  match (p_auth, gen_p.burial) with
      | (true, Buried cod) | (true, Cremated cod) ->
          (match Adef.od_of_cdate cod with
          | Some d -> string_of_date_and_conv conf d
          | _ -> ("", "", "", "", None))
      | _ -> ("", "", "", "", None)

let fill_occupation conf base p_auth gen_p =
  if p_auth then
        let s = gen_p.occupation in
        let s =
          let wi =
            {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
             Wiki.wi_file_path = Notes.file_path conf base;
             Wiki.wi_person_exists = person_exists conf base;
             Wiki.wi_always_show_link = conf.wizard || conf.friend}
          in
          Wiki.syntax_links conf wi s
        in
        string_with_macros conf [] s
      else ""

let fill_index conf p p_auth =
  if not p_auth && (is_hide_names conf p)
  then
    Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper
  else
    Int32.of_string @@ Gwdb.string_of_iper (get_iper p)

let fill_sources conf base p p_auth gen_p is_main_person =
  if p_auth && is_main_person then
    let s = gen_p.psources in
    let env = [('i', fun () -> Util.default_image_name base p)] in
    let s =
      let wi =
        {Wiki.wi_mode = "NOTES";
         Wiki.wi_cancel_links = conf.cancel_links;
         Wiki.wi_file_path = Notes.file_path conf base;
         Wiki.wi_person_exists = person_exists conf base;
         Wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      Wiki.syntax_links conf wi s
    in
    string_with_macros conf env s
    else ""

let fill_parents conf base p base_prefix =
  match get_parents p with
  | Some ifam ->
    let cpl = foi base ifam in
    let ifath = get_father cpl in
    let imoth = get_mother cpl in
    let father =
      if ifath = Gwdb.dummy_iper then None
      else
        let father = poi base ifath in
        Some (pers_to_piqi_simple_person conf base father base_prefix)
    in
    let mother =
      if imoth = Gwdb.dummy_iper then None
      else
        let mother = poi base imoth in
        Some (pers_to_piqi_simple_person conf base mother base_prefix)
    in
    (father, mother)
  | None ->
    (* lien inter arbre *)
    let ip = get_iper p in
    match Perso_link.get_parents_link base_prefix ip with
    | Some family ->
        begin
          let ifath = Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.ifath in
          let imoth = Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.imoth in
          let fam_base_prefix = family.MLink.Family.baseprefix in
          match
            (Perso_link.get_person_link fam_base_prefix ifath,
             Perso_link.get_person_link fam_base_prefix imoth,
             Perso_link.get_person_link base_prefix ip)
          with
          | (Some pfath, Some pmoth, Some _) ->
              let (fath, _) = Perso_link.make_ep_link base pfath in
              let (moth, _) = Perso_link.make_ep_link base pmoth in
              let father =
                Some (pers_to_piqi_simple_person conf base fath fam_base_prefix)
              in
              let mother =
                Some (pers_to_piqi_simple_person conf base moth fam_base_prefix)
              in
              (father, mother)
          | _ -> (None, None)
        end
    | None -> (None, None)

let fill_fiche_parents conf base p base_prefix nb_asc nb_asc_max with_parent_families pers_to_piqi_person simple_graph_info no_event =
  if nb_asc_max > nb_asc
  then
    match get_parents p with
    | Some ifam ->
        let cpl = foi base ifam in
        let ifath = get_father cpl in
        let imoth = get_mother cpl in
        let father =
          if ifath = Gwdb.dummy_iper then None
          else
            let father = poi base ifath in
            if with_parent_families then
                Some (pers_to_piqi_person conf base father base_prefix false (nb_asc+1) nb_asc_max 0 2 true simple_graph_info no_event)
            else
                Some (pers_to_piqi_person conf base father base_prefix false (nb_asc+1) nb_asc_max 0 0 false simple_graph_info no_event)
        in
        let mother =
          if imoth = Gwdb.dummy_iper then None
          else
            let mother = poi base imoth in
            if with_parent_families then
              Some (pers_to_piqi_person conf base mother base_prefix false (nb_asc+1) nb_asc_max 0 2 true simple_graph_info no_event)
            else
              Some (pers_to_piqi_person conf base mother base_prefix false (nb_asc+1) nb_asc_max 0 0 false simple_graph_info no_event)
        in
        (father, mother)
    | None ->
        (* lien inter arbre *)
        let ip = get_iper p in
        let base_prefix = conf.command in
        match Perso_link.get_parents_link base_prefix ip with
        | Some family ->
            begin
              let ifath = Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.ifath in
              let imoth = Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.imoth in
              let fam_base_prefix = family.MLink.Family.baseprefix in
              match
                (Perso_link.get_person_link fam_base_prefix ifath,
                 Perso_link.get_person_link fam_base_prefix imoth,
                 Perso_link.get_person_link base_prefix ip)
              with
              | (Some pfath, Some pmoth, Some _) ->
                  let (fath, _) = Perso_link.make_ep_link base pfath in
                  let (moth, _) = Perso_link.make_ep_link base pmoth in
                  let father =
                    if with_parent_families then
                      Some (pers_to_piqi_person conf base fath fam_base_prefix false (nb_asc+1) nb_asc_max 0 1 true simple_graph_info no_event)
                    else
                      Some (pers_to_piqi_person conf base fath fam_base_prefix false (nb_asc+1) nb_asc_max 0 0 false simple_graph_info no_event)
                  in
                  let mother =
                    if with_parent_families then
                      Some (pers_to_piqi_person conf base moth fam_base_prefix false (nb_asc+1) nb_asc_max 0 1 true simple_graph_info no_event)
                    else
                      Some (pers_to_piqi_person conf base moth fam_base_prefix false (nb_asc+1) nb_asc_max 0 0 false simple_graph_info no_event)
                  in
                  (father, mother)
              | _ -> (None, None)
            end
        | None -> (None, None)
      else
        (None, None)

let has_relations conf base p p_auth is_main_person =
  if p_auth && conf.use_restrict && is_main_person  then
    let related =
      List.fold_left
        (fun l ip ->
           let rp = pget conf base ip in
           if is_hidden rp then l else (ip :: l))
      [] (get_related p)
    in
    get_rparents p <> [] || related <> []
  else p_auth && (get_rparents p <> [] || get_related p <> [])

let get_event_constructor name type_ date date_long date_raw date_conv date_conv_long date_cal place note src spouse witnesses =
      {
        Mread.Event.name = name;
        type_ = type_;
        date = if date = "" then None else Some date;
        date_long = if date_long = "" then None else Some date_long;
        date_raw = if date_raw = "" then None else Some date_raw;
        date_conv = if date_conv = "" then None else Some date_conv;
        date_conv_long = if date_conv_long = "" then None else Some date_conv_long;
        date_cal = date_cal;
        place = if place = "" then None else Some place;
        reason = None;
        note = if note = "" then None else Some note;
        src = if src= "" then None else Some src;
        spouse = spouse;
        witnesses = witnesses;
      }

let fiche_event_constructor name type_ date date_long date_raw date_conv date_conv_long date_cal place note src spouse witnesses =
  {
      Mread.Fiche_event.name = name;
      type_ = type_;
      date = if date = "" then None else Some date;
      date_long = if date_long = "" then None else Some date_long;
      date_raw = if date_raw = "" then None else Some date_raw;
      date_conv = if date_conv = "" then None else Some date_conv;
      date_conv_long = if date_conv_long = "" then None else Some date_conv_long;
      date_cal = date_cal;
      place = if place = "" then None else Some place;
      reason = None;
      note = if note = "" then None else Some note;
      src = if src= "" then None else Some src;
      spouse = spouse;
      witnesses = witnesses;
  }

let simple_witness_constructor witness_type witness =
  Mread.Witness_event.({
    witness_type = witness_type;
    witness = witness;
  })

let fiche_witness_constructor witness_type witness =
  Mread.Witness_fiche_event.({
    witness_type = witness_type;
    witness = witness;
  })

let simple_event_witness_constructor event_witness_type husband wife =
      Mread.Event_witness.({
        event_witness_type = event_witness_type;
        husband = husband;
        wife = wife;
      })

let fiche_event_witness_constructor event_witness_type husband wife =
  Mread.Event_fiche_witness.({
    event_witness_type = event_witness_type;
    husband = husband;
    wife = wife;
  })

let fill_notes conf base p p_auth is_main_person gen_p =
  if p_auth && not conf.no_note && is_main_person then
    let env = [('i', fun () -> Util.default_image_name base p)] in
    let s = gen_p.notes in
    convert_wiki_notes_to_html_notes conf base env s "\n"
  else ""

let simple_relation_person_constructor r_type p =
  {
    Mread.Relation_person.r_type = r_type;
    person = p;
  }

let fiche_relation_person_constructor r_type p =
  {
    Mread.Relation_fiche_person.r_type = r_type;
    person = p;
  }

let fill_families conf base p =
  let base_prefix = conf.command in
  let spouse_to_piqi_callback conf base p base_prefix =
      pers_to_piqi_simple_person conf base p base_prefix
  in
  let witnesses_to_piqi_callback conf base p base_prefix =
      pers_to_piqi_simple_person conf base p base_prefix
  in
  let child_to_piqi_callback conf base p base_prefix =
      pers_to_piqi_simple_person conf base p base_prefix
  in
  let family_constructor index spouse marriage_date marriage_date_long marriage_date_raw marriage_date_conv marriage_date_conv_long marriage_cal
       marriage_date_text marriage_place marriage_src marriage_type divorce_type divorce_date divorce_date_long divorce_date_raw divorce_date_conv
       divorce_date_conv_long divorce_cal witnesses notes fsources children =
    {
      Mread.Family.index = index;
      spouse = spouse;
      marriage_date = if marriage_date = "" then None else Some marriage_date;
      marriage_date_long = if marriage_date_long = "" then None else Some marriage_date_long;
      marriage_date_raw = if marriage_date_raw = "" then None else Some marriage_date_raw;
      marriage_date_conv =
        if marriage_date_conv = "" then None else Some marriage_date_conv;
      marriage_date_conv_long =
        if marriage_date_conv_long = "" then None else Some marriage_date_conv_long;
      marriage_date_cal = marriage_cal;
      marriage_date_text = if marriage_date_text = "" then None else Some marriage_date_text;
      marriage_place = if marriage_place = "" then None else Some marriage_place;
      marriage_src = if marriage_src = "" then None else Some marriage_src;
      marriage_type = marriage_type;
      divorce_type = divorce_type;
      divorce_date = if divorce_date = "" then None else Some divorce_date;
      divorce_date_long = if divorce_date_long = "" then None else Some divorce_date_long;
      divorce_date_raw = if divorce_date_raw = "" then None else Some divorce_date_raw;
      divorce_date_conv =
        if divorce_date_conv = "" then None else Some divorce_date_conv;
      divorce_date_conv_long =
        if divorce_date_conv_long = "" then None else Some divorce_date_conv_long;
      divorce_date_cal = divorce_cal;
      witnesses = witnesses;
      notes = if notes = "" then None else Some notes;
      fsources = if fsources = "" then None else Some fsources;
      children = children;
    }
  in
  get_families_piqi base conf p base_prefix spouse_to_piqi_callback witnesses_to_piqi_callback child_to_piqi_callback family_constructor

let fill_fiche_families conf base p base_prefix nb_asc nb_desc nb_desc_max pers_to_piqi_person simple_graph_info no_event =
  let include_families = (nb_desc_max > nb_desc && (nb_asc <= 2)) in
  if include_families
  then
    let spouse_to_piqi_callback conf base p base_prefix =
      pers_to_piqi_person conf base p base_prefix false 0 1 0 0 false simple_graph_info no_event
    in
    let witnesses_to_piqi_callback conf base p base_prefix =
      if not simple_graph_info then
        pers_to_piqi_person conf base p base_prefix false 0 1 0 0 false simple_graph_info no_event
      else
        Mread.default_person()
    in
    let child_to_piqi_callback conf base p base_prefix =
      pers_to_piqi_person conf base p base_prefix false 0 0 (nb_desc+1) nb_desc_max false simple_graph_info no_event
    in
    let family_constructor index spouse marriage_date marriage_date_long marriage_date_raw marriage_date_conv marriage_date_conv_long marriage_cal
    marriage_date_text marriage_place marriage_src marriage_type divorce_type divorce_date divorce_date_long divorce_date_raw divorce_date_conv
    divorce_date_conv_long divorce_cal witnesses notes fsources children =
      {
        Mread.Fiche_family.index = index;
        spouse = spouse;
        marriage_date = if marriage_date = "" then None else Some marriage_date;
        marriage_date_long = if marriage_date_long = "" then None else Some marriage_date_long;
        marriage_date_raw = if marriage_date_raw = "" then None else Some marriage_date_raw;
        marriage_date_conv =
          if marriage_date_conv = "" then None else Some marriage_date_conv;
        marriage_date_conv_long =
          if marriage_date_conv_long = "" then None else Some marriage_date_conv_long;
        marriage_date_cal = marriage_cal;
        marriage_date_text = if marriage_date_text = "" then None else Some marriage_date_text;
        marriage_place = if marriage_place = "" then None else Some marriage_place;
        marriage_src = if marriage_src = "" then None else Some marriage_src;
        marriage_type = marriage_type;
        divorce_type = divorce_type;
        divorce_date = if divorce_date = "" then None else Some divorce_date;
        divorce_date_long = if divorce_date_long = "" then None else Some divorce_date_long;
        divorce_date_raw = if divorce_date_raw = "" then None else Some divorce_date_raw;
        divorce_date_conv =
          if divorce_date_conv = "" then None else Some divorce_date_conv;
        divorce_date_conv_long =
          if divorce_date_conv_long = "" then None else Some divorce_date_conv_long;
        divorce_date_cal = divorce_cal;
        witnesses = if not simple_graph_info then witnesses else [];
        notes = if notes = "" || simple_graph_info then None else Some notes;
        fsources = if fsources = "" || simple_graph_info then None else Some fsources;
        children = children;
      }
    in
      get_families_piqi base conf p base_prefix spouse_to_piqi_callback witnesses_to_piqi_callback child_to_piqi_callback family_constructor
  else []

let has_sources p_auth psources birth_src baptism_src death_src burial_src =
  if not p_auth then false
    else if psources <> "" then true
    else if
      p_auth &&
      (birth_src <> "" || baptism_src <> "" ||
       death_src <> "" || burial_src <> "")
    then true
  else false

let fill_titles conf base p =
  let tmp_conf = {(conf) with cancel_links = true} in
  List.map (Perso.string_of_title tmp_conf base "" p) (Perso.nobility_titles_list conf base p)

let transform_empty_string_to_None string =
  if string = "" then None else Some string

let fill_birth_date_raw conf p_auth gen_p =
  match (p_auth, Adef.od_of_cdate gen_p.birth) with
    | (true, Some d) -> string_of_date_raw conf d
    | _ -> ""

let fill_baptism_date_raw conf p_auth gen_p =
  match (p_auth, Adef.od_of_cdate gen_p.baptism) with
    | (true, Some d) -> string_of_date_raw conf d
    | _ -> ""

let fill_death_date_raw conf p_auth gen_p =
  match (p_auth, gen_p.death) with
      | (true, Death (_, cd)) ->
          let d = Adef.date_of_cdate cd in
          string_of_date_raw conf d
      | _ -> ""

let fill_burial_date_raw_if_is_main_person conf p_auth gen_p is_main_person =
  if is_main_person then
    match (p_auth, gen_p.burial) with
    | (true, Buried cod) | (true, Cremated cod) ->
        (match Adef.od_of_cdate cod with
        | Some d -> string_of_date_raw conf d
        | _ -> "")
    | _ -> ""
  else
    ""

let fill_birth_text conf p p_auth =
  Perso.get_birth_text conf p p_auth

let fill_baptism_text conf p p_auth =
  Perso.get_baptism_text conf p p_auth

let fill_death_text conf p p_auth =
  Perso.get_death_text conf p p_auth

let fill_burial_text conf p p_auth =
  Perso.get_burial_text conf p p_auth

let fill_cremation_text conf p p_auth =
  Perso.get_cremation_text conf p p_auth

let fill_baptism_text_if_main_person_or_parent conf p p_auth is_main_person_or_father_or_mother =
  if (is_main_person_or_father_or_mother) then
    fill_baptism_text conf p p_auth
  else ""

let fill_burial_type p_auth gen_p =
  if p_auth then
  match (gen_p.burial) with
    | Buried _ -> `buried
    | Cremated _ -> `cremated
    | _ -> `dont_know
  else `dont_know

let fill_titles_with_links conf base p =
  let tmp_conf = {(conf) with cancel_links = false} in
  List.map (Perso.string_of_title tmp_conf base "" p) (Perso.nobility_titles_list conf base p)

let has_history_if_is_main_person conf base p p_auth is_main_person =
  if is_main_person then
    Perso.has_history conf base p p_auth
  else false

let has_duplication_if_is_main_person conf base p is_main_person =
  (* Les doublons ne sont pas testés pour les LIA. *)
  if is_main_person then
      Perso.has_possible_duplications conf base p
  else
      false

let fill_linked_page_if_is_main_person conf base p is_main_person =
  if is_main_person then
    (
    Perso.get_linked_page conf base p "BIBLIO",
    Perso.get_linked_page conf base p "BNOTE",
    Perso.get_linked_page conf base p "DEATH",
    Perso.get_linked_page conf base p "HEAD",
    Perso.get_linked_page conf base p "OCCU"
    )
  else
    ("", "", "", "", "")

(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person : config -> base -> person -> string -> Person *)
(** [Description] : Retourne à partir d'une person (gwdb) une Person (piqi)
                    dont tous les champs sont complétés.
    [Args] :
      - conf           : configuration de la base
      - base           : base de donnée
      - p              : person
      - base_prefix    : nom de l'arbre (différent de base dans le cas des LIA)
      - is_main_person : si la personne est principale
      - bypass_duplicate_fields       : ne calcule pas des champs en doublon à un objet FichePerson
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person conf base p base_prefix is_main_person =
  if is_restricted conf base (get_iper p) then
    get_restricted_person ()
  else
    let p_auth = authorized_age conf base p in
    let gen_p = Util.string_gen_person base (gen_person_of_person p) in
    let has_relations = has_relations conf base p p_auth is_main_person in

    let (baptism_date, _, baptism_date_conv, _, baptism_cal) = fill_baptism conf p_auth gen_p in
    let (birth_date, _, birth_date_conv, _, birth_cal) = fill_birth conf p_auth gen_p in
    let (burial_date, _, burial_date_conv, _,burial_cal) = fill_burial conf p_auth gen_p in
    let (death_type, death_date, death_date_conv, death_cal) = fill_death conf p_auth gen_p in

    let (father, mother) = fill_parents conf base p base_prefix in

    let psources = fill_sources conf base p p_auth gen_p is_main_person in
    let birth_src = fill_birth_src p_auth gen_p in
    let baptism_src = fill_baptism_src p_auth gen_p in
    let death_src = fill_death_src p_auth gen_p in
    let burial_src = fill_burial_src p_auth gen_p in
    let has_sources = has_sources p_auth psources birth_src baptism_src death_src burial_src in

    {
      Mread.Person.type_ = `simple;
      index = fill_index conf p p_auth;
      sex = fill_sex p;
      lastname = fill_surname conf p p_auth gen_p;
      firstname = fill_firstname conf p p_auth gen_p;
      n = fill_sn conf base p p_auth;
      p = fill_fn conf base p p_auth;
      occ = fill_occ p;
      public_name = fill_publicname p_auth gen_p;
      aliases = fill_aliases p_auth gen_p;
      qualifiers = fill_qualifiers p_auth gen_p;
      firstname_aliases = fill_firstname_aliases p_auth gen_p;
      surname_aliases = fill_surname_aliases p_auth gen_p;
      image = transform_empty_string_to_None (fill_image conf base p);
      birth_date = transform_empty_string_to_None birth_date;
      birth_date_conv = transform_empty_string_to_None birth_date_conv;
      birth_date_cal = birth_cal;
      birth_place = transform_empty_string_to_None (fill_birth_place conf p_auth gen_p);
      birth_src = transform_empty_string_to_None (fill_birth_src p_auth gen_p);
      baptism_date = transform_empty_string_to_None baptism_date;
      baptism_date_conv = transform_empty_string_to_None baptism_date_conv;
      baptism_date_cal = baptism_cal;
      baptism_place = transform_empty_string_to_None (fill_baptism_place conf p_auth gen_p);
      baptism_src = transform_empty_string_to_None baptism_src;
      death_date = transform_empty_string_to_None death_date;
      death_date_conv = transform_empty_string_to_None death_date_conv;
      death_date_cal = death_cal;
      death_place = transform_empty_string_to_None (fill_death_place conf p_auth gen_p);
      death_src = transform_empty_string_to_None death_src;
      death_type = death_type;
      burial_date = transform_empty_string_to_None burial_date;
      burial_date_conv = transform_empty_string_to_None burial_date_conv;
      burial_date_cal = burial_cal;
      burial_place = transform_empty_string_to_None (fill_burial_place conf p_auth gen_p);
      burial_src = transform_empty_string_to_None burial_src;
      occupation = transform_empty_string_to_None (fill_occupation conf base p_auth gen_p);
      notes = transform_empty_string_to_None (fill_notes conf base p p_auth is_main_person gen_p);
      psources = transform_empty_string_to_None psources;
      has_sources = has_sources;
      titles = fill_titles conf base p;
      related = get_related_piqi conf base p base_prefix gen_p has_relations pers_to_piqi_simple_person simple_relation_person_constructor;
      rparents = get_rparents_piqi base conf base_prefix gen_p has_relations pers_to_piqi_simple_person simple_relation_person_constructor;
      father = father;
      mother = mother;
      families = fill_families conf base p;
      sosa = fill_sosa p;
      events = fill_events conf base p base_prefix p_auth pers_to_piqi_simple_person simple_witness_constructor get_event_constructor;
      events_witnesses = get_events_witnesses conf base p base_prefix gen_p p_auth has_relations pers_to_piqi_simple_person simple_event_witness_constructor;
      baseprefix = base_prefix;
      fiche_person_person = None;
    }

let fill_ref_if_is_main_person conf base is_main_person =
  if is_main_person then
    match Util.find_sosa_ref conf base with
      | Some ref -> ( Some (Int32.of_string @@ Gwdb.string_of_iper (get_iper ref))
                    , Some (pers_to_piqi_person conf base ref conf.command false) )
      | None -> (None, None)
  else
    (None, None)

(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_fiche_person :
    config -> base -> person -> base_prefix -> bool -> int -> int -> int
    -> int -> bool -> bool -> bool -> Person                                  *)
(** [Description] : Retourne à partir d'une person (gwdb) une Person fiche
                    (piqi) dont tous les champs sont complétés.
    [Args] :
      - conf         : configuration de la base
      - base         : base de donnée
      - p            : person
      - base_prefix  : nom de l'arbre (différent de base dans le cas des LIA)
      - with_parents : bool
    [Retour] :
      - Person
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let rec pers_to_piqi_fiche_person conf base p base_prefix is_main_person nb_asc nb_asc_max nb_desc nb_desc_max with_parent_families simple_graph_info no_event =
  (* Generates a fiche person by default. *)
  let piqi_fiche_person = Mread.default_fiche_person() in
  (* If the access is restricted, returns the person with default fields. *)
  if is_restricted conf base (get_iper p) then
    get_restricted_person ()
  else
    begin
      let p_auth = authorized_age conf base p in
      let gen_p = Util.string_gen_person base (gen_person_of_person p) in

      (* Sources only returned for the main person. *)
      let psources = if (is_main_person) then fill_sources conf base p p_auth gen_p is_main_person else "" in
      let birth_src = if (is_main_person) then fill_birth_src p_auth gen_p else "" in
      let baptism_src = if (is_main_person) then fill_baptism_src p_auth gen_p else "" in
      let death_src = if (is_main_person) then fill_death_src p_auth gen_p else "" in
      let burial_src = if (is_main_person) then fill_burial_src p_auth gen_p else "" in
      let has_sources = if (is_main_person) then has_sources p_auth psources birth_src baptism_src death_src burial_src else false in
      let (death_type, death_date, death_date_conv, death_cal) = fill_death conf p_auth gen_p in
      (* Linked links (family book). *)
      let (linked_page_biblio, linked_page_bnote, linked_page_death, linked_page_head, linked_page_occu) = if not simple_graph_info then fill_linked_page_if_is_main_person conf base p is_main_person else ("", "", "", "", "") in
      let pers_to_piqi_fiche_person_only conf base p base_prefix =
        pers_to_piqi_fiche_person conf base p base_prefix false 0 0 0 0 false simple_graph_info no_event
      in
      let sosa_nb = Perso.get_sosa_person p in
      let (fiche_father, fiche_mother) = if is_main_person || not simple_graph_info then fill_fiche_parents conf base p base_prefix nb_asc nb_asc_max with_parent_families pers_to_piqi_fiche_person simple_graph_info no_event else (None, None) in
      let (father, mother) = if with_parent_families then fill_parents conf base p base_prefix else (None, None) in
      let has_relations = if is_main_person then has_relations conf base p p_auth is_main_person else false in
      (* Returns simple person attributes only when nb of desc is 0. *)
      let return_simple_attributes = (nb_desc_max == 0) in
      let (ref_index, ref_person) = fill_ref_if_is_main_person conf base is_main_person in
      let piqi_fiche_person =
        (* Fields shared by all the members of the family. *)
        piqi_fiche_person.Mread.Fiche_person.birth_date_raw <- transform_empty_string_to_None (fill_birth_date_raw conf p_auth gen_p);
        piqi_fiche_person.Mread.Fiche_person.birth_text <- transform_empty_string_to_None (fill_birth_text conf p p_auth);
        piqi_fiche_person.Mread.Fiche_person.burial_date_raw <- transform_empty_string_to_None (fill_burial_date_raw_if_is_main_person conf p_auth gen_p is_main_person);
        piqi_fiche_person.Mread.Fiche_person.burial_text <- transform_empty_string_to_None (fill_burial_text conf p p_auth);
        piqi_fiche_person.Mread.Fiche_person.burial_type <- fill_burial_type p_auth gen_p;
        piqi_fiche_person.Mread.Fiche_person.cremation_text <- transform_empty_string_to_None (fill_cremation_text conf p p_auth);
        piqi_fiche_person.Mread.Fiche_person.death_date_raw <- transform_empty_string_to_None (fill_death_date_raw conf p_auth gen_p);
        piqi_fiche_person.Mread.Fiche_person.death_text <- transform_empty_string_to_None (fill_death_text conf p p_auth);
        piqi_fiche_person.Mread.Fiche_person.titles_links <- if not simple_graph_info then fill_titles_with_links conf base p else [];
        piqi_fiche_person.Mread.Fiche_person.sosa_nb <- if sosa_nb = Sosa.zero then None else Some (Sosa.to_string sosa_nb);
        piqi_fiche_person.Mread.Fiche_person.father <- fiche_father;
        piqi_fiche_person.Mread.Fiche_person.mother <- fiche_mother;
        if is_main_person || not simple_graph_info then
          piqi_fiche_person.Mread.Fiche_person.families <- fill_fiche_families conf base p base_prefix nb_asc nb_desc nb_desc_max pers_to_piqi_fiche_person simple_graph_info no_event;

        (* Fields only filled for the main person. *)
        piqi_fiche_person.Mread.Fiche_person.baptism_date_raw <- if is_main_person then transform_empty_string_to_None (fill_baptism_date_raw conf p_auth gen_p) else None;
        piqi_fiche_person.Mread.Fiche_person.baptism_text <- if is_main_person then transform_empty_string_to_None (fill_baptism_text conf p p_auth) else None;
        piqi_fiche_person.Mread.Fiche_person.has_possible_duplications <- has_duplication_if_is_main_person conf base p is_main_person;
        piqi_fiche_person.Mread.Fiche_person.ref_index <- ref_index;
        piqi_fiche_person.Mread.Fiche_person.ref_person <- ref_person;
        piqi_fiche_person.Mread.Fiche_person.has_history <- has_history_if_is_main_person conf base p p_auth is_main_person;
        piqi_fiche_person.Mread.Fiche_person.linked_page_biblio <- linked_page_biblio;
        piqi_fiche_person.Mread.Fiche_person.linked_page_bnote <- linked_page_bnote;
        piqi_fiche_person.Mread.Fiche_person.linked_page_death <- linked_page_death;
        piqi_fiche_person.Mread.Fiche_person.linked_page_head <- linked_page_head;
        piqi_fiche_person.Mread.Fiche_person.linked_page_occu <- linked_page_occu;
        piqi_fiche_person.Mread.Fiche_person.visible_for_visitors <- is_visible conf base p;
        piqi_fiche_person.Mread.Fiche_person.related <- if is_main_person && not simple_graph_info then get_related_piqi conf base p base_prefix gen_p has_relations pers_to_piqi_fiche_person_only fiche_relation_person_constructor else [];
        piqi_fiche_person.Mread.Fiche_person.rparents <- if is_main_person && not simple_graph_info then get_rparents_piqi base conf base_prefix gen_p has_relations pers_to_piqi_fiche_person_only fiche_relation_person_constructor else [];
        if not no_event then
          piqi_fiche_person.Mread.Fiche_person.events_witnesses <- if is_main_person then get_events_witnesses conf base p base_prefix gen_p p_auth has_relations pers_to_piqi_fiche_person_only fiche_event_witness_constructor else [];
        if not no_event then
          piqi_fiche_person.Mread.Fiche_person.events <- fill_events_if_is_main_person conf base p base_prefix p_auth is_main_person pers_to_piqi_fiche_person_only fiche_witness_constructor fiche_event_constructor;
        piqi_fiche_person
      in
      {
        Mread.Person.type_ = `fiche;
        fiche_person_person = Some piqi_fiche_person;
        n = fill_sn conf base p p_auth;
        p = fill_fn conf base p p_auth;
        occ = fill_occ p;

        aliases = if return_simple_attributes && not simple_graph_info then fill_aliases p_auth gen_p else [];
        baptism_src = transform_empty_string_to_None baptism_src;
        birth_place = transform_empty_string_to_None (fill_birth_place conf p_auth gen_p);
        birth_src = transform_empty_string_to_None birth_src;
        burial_place = transform_empty_string_to_None (fill_burial_place conf p_auth gen_p);
        burial_src = transform_empty_string_to_None burial_src;
        death_date = transform_empty_string_to_None death_date;
        death_date_conv = transform_empty_string_to_None death_date_conv;
        death_date_cal = death_cal;
        death_place = transform_empty_string_to_None (fill_death_place conf p_auth gen_p);
        death_src = transform_empty_string_to_None death_src;
        death_type = death_type;
        index = fill_index conf p p_auth;
        image = transform_empty_string_to_None (fill_image conf base p);
        firstname = fill_firstname conf p p_auth gen_p;
        lastname = fill_surname conf p p_auth gen_p;
        qualifiers = if (not simple_graph_info) || is_main_person then fill_qualifiers p_auth gen_p else [];
        occupation = transform_empty_string_to_None (fill_occupation conf base p_auth gen_p);
        sex = fill_sex p;
        public_name = fill_publicname p_auth gen_p;

        (* Fields only filled for the main person. *)
        baptism_place = if is_main_person then transform_empty_string_to_None (fill_baptism_place conf p_auth gen_p) else None;
        firstname_aliases = if is_main_person && not simple_graph_info then fill_firstname_aliases p_auth gen_p else [];
        has_sources = has_sources;
        notes = if is_main_person && not simple_graph_info then transform_empty_string_to_None (fill_notes conf base p p_auth is_main_person gen_p) else None;
        psources = if is_main_person && not simple_graph_info then transform_empty_string_to_None psources else None;
        sosa = if is_main_person then fill_sosa p else `no_sosa;
        surname_aliases = if is_main_person && not simple_graph_info then fill_surname_aliases p_auth gen_p else [];

        (* These fields should not be set because Fiche Person fields are better. *)
        baptism_date = None;
        baptism_date_conv = None;
        baptism_date_cal = None;
        birth_date = None;
        birth_date_conv = None;
        birth_date_cal = None;
        burial_date = None;
        burial_date_conv = None;
        burial_date_cal = None;
        events = if return_simple_attributes && not no_event then fill_events conf base p base_prefix p_auth pers_to_piqi_simple_person simple_witness_constructor get_event_constructor else [];
        events_witnesses = if return_simple_attributes && not no_event then get_events_witnesses conf base p base_prefix gen_p p_auth has_relations pers_to_piqi_simple_person simple_event_witness_constructor else [];
        families = if return_simple_attributes && not simple_graph_info then fill_families conf base p else [];
        father = if return_simple_attributes then father else None;
        mother = if return_simple_attributes then mother else None;
        titles = if not simple_graph_info then fill_titles conf base p else [];
        related = if return_simple_attributes then get_related_piqi conf base p base_prefix gen_p has_relations pers_to_piqi_simple_person simple_relation_person_constructor else [];
        rparents = if return_simple_attributes then get_rparents_piqi base conf base_prefix gen_p has_relations pers_to_piqi_simple_person simple_relation_person_constructor else [];
        baseprefix = base_prefix;
      }
    end

(* ********************************************************************* *)
(*  [Fonc] print_person_tree : conf -> base -> unit                      *)
(** [Description] : Renvoie un objet personne qui servira à afficher
      toutes les informations sur le panneau latéral de l'arbre de
      navigation.
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_person_tree conf base =
  let params = get_params conf Mext_read.parse_index_person in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Mread.Index_person.index in
  (* Construction de la base avec calcul des sosas           *)
  (* Si iz présent, on prend iz comme souche pour le calcul  *)
  (* Sinon on prend la souche de l'arbre                     *)
  let () =
    match params.Mread.Index_person.indexz with
      | Some n -> Perso.build_sosa_tree_ht conf base (poi base (Gwdb.iper_of_string @@ Int32.to_string n))
      | None -> Perso.build_sosa_ht conf base
    in
  let p = poi base ip in
  (* cache lien inter arbre *)
  let () = Perso_link.init_cache conf base ip 1 1 1 in
  let pers_piqi = pers_to_piqi_person conf base p conf.command true in
  let data = Mext_read.gen_person pers_piqi in
  print_result conf data


type search_index_type =  Sosa | Key | Surname | FirstName | ApproxKey | PartialKey
let search_index conf base fn sn search_order =
  let an = fn ^ " " ^ sn in
  let rec loop = function
    | Sosa :: tl ->
      begin match SearchName.search_by_sosa conf base an with
        | [] ->  loop tl
        | [p] -> Some (get_iper p)
        | _ -> None
      end
    | Key :: tl ->
      let pl = SearchName.search_by_key conf base an in
      begin match pl with
        | [] ->  loop tl
        | [p] -> Some (get_iper p)
        | _ -> None
      end
    | Surname :: tl ->
      begin match Some.search_surname base sn with
        | [_, (_, [ip])], _ -> Some ip
        | [], _ -> loop tl
        | _ -> None
      end
    | FirstName :: tl ->
      begin match Some.search_first_name base fn with
        | [] -> loop tl
        | [(_, (_, [i]))] when sn = "" -> Some i
        | list ->
          if sn = "" then None
          else begin match
              let iperl = Some.ipers @@ fst @@ Some.search_surname base sn in
              List.filter (function (_, (_, [])) -> false | _ -> true) @@
              List.map
                begin fun (s, (i, ips)) ->
                  (s, (i, List.filter (fun i -> List.mem i iperl) ips))
                end
                list
            with
            | [] -> loop tl
            | [ (_, (_, [i])) ] -> Some i
            | _ -> None
          end
      end
    | ApproxKey :: tl ->
      begin match SearchName.search_approx_key conf base an with
        | [] ->  loop tl
        | [p] -> Some (get_iper p)
        | _ -> None
      end
    | PartialKey :: tl ->
      begin match SearchName.search_partial_key conf base an with
        | [] ->  loop tl
        | [p] -> Some (get_iper p)
        | _ -> None
      end
    | _ -> None
  in
  loop search_order

(* ********************************************************************* *)
(*  [Fonc] print_result_fiche_person :                                   *)
(*         conf -> base -> ip -> int -> int -> bool -> bool -> unit      *)
(** [Description] : Renvoie un objet personne contenant les données de
      la fiche en fonction d'un index.
    [Args] :
      - conf              : configuration de la base
      - base              : base de données
      - ip                : l'index de la personne
      - nb_asc_max        : nombre d'ascendants à récupérer au maximum
      - nb_desc_max       : nombre de descendants à récupérer au maximum
      - simple_graph_info : ne récupère que des informations simples pour
       les personnes autres que la principale.
      - no_event          : ne récupère pas les évènements.
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_result_fiche_person conf base ip nb_asc_max nb_desc_max simple_graph_info no_event =
  let () = Perso.build_sosa_ht conf base in
  let p = poi base ip in
  (* cache lien inter arbre *)
  let () = Perso_link.init_cache conf base ip 1 1 1 in
  let pers_piqi = pers_to_piqi_fiche_person conf base p conf.command true 0 nb_asc_max 0 nb_desc_max true simple_graph_info no_event in
  let data = Mext_read.gen_person pers_piqi in
  print_result conf data

(* ********************************************************************* *)
(*  [Fonc] is_private_person : conf -> base -> ip -> bool                 *)
(** [Description] : Indique si une personne est privée ou non.
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ip    : index de la personne
    [Retour] : Bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_private_person conf base ip =
    let p = pget conf base ip in
    is_hidden p || ((is_hide_names conf p) && not(authorized_age conf base p))

(* ********************************************************************* *)
(*  [Fonc] print_from_identifier_person : conf -> base ->                *)
(*   print_result_from_ip -> Identifier_person -> unit                   *)
(** [Description] : Utilise un identifiant de personne pour appeler une
    fonction qui utilise l'ip (index de la personne) récupéré.
    Affiche des erreurs si la personne n'est pas trouvée
    ou si les paramètres sont incorrects.
    [Args] :
      - conf                 : configuration de la base
      - base                 : base de donnée
      - print_result_from_ip : fonction permettant d'afficher les resultats
      - identifier_person    : Objet identifiant une personne
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_from_identifier_person conf base print_result_from_ip identifier_person =
  match identifier_person.Mread.Identifier_person.index with
  | exception _ -> print_error conf `not_found
  | Some index ->
    (* Traite l'index *)
    let ip = Gwdb.iper_of_string @@ Int32.to_string index in
    if identifier_person.Mread.Identifier_person.track_visit = Some true
    then record_visited conf ip;
    print_result_from_ip conf base ip
  | None ->
    match (identifier_person.Mread.Identifier_person.oc) with
    | (Some oc) ->
      begin
        match ( identifier_person.Mread.Identifier_person.p
              , identifier_person.Mread.Identifier_person.n) with
        | (Some fn, Some sn) ->
          (* Retourne une personne en fonction de son npoc *)
          begin
            match Gwdb.person_of_key base fn sn (Int32.to_int oc) with
            | Some ip ->
              if is_private_person conf base ip
              then
                print_error conf `not_found
              else
                (if identifier_person.Mread.Identifier_person.track_visit
                    = Some true
                 then record_visited conf ip;
                 print_result_from_ip conf base ip)
            | None ->
              print_error conf `not_found
          end
        | _ -> print_error conf `bad_request
        end
    | None ->
      (* Fait une recherche par mots-clé *)
      let (fn, sn) =
        match ( identifier_person.Mread.Identifier_person.p
              , identifier_person.Mread.Identifier_person.n) with
        | (Some fn, Some sn) -> (fn, sn)
        | (None, Some sn) -> ("", sn)
        | (Some fn, None) -> (fn, "")
        | _ -> print_error conf `bad_request; ("", "")
      in
      let order =
        if fn = "" then [ Sosa; Key; Surname; ApproxKey; PartialKey ]
        else if sn = "" then [ FirstName ]
        else [ Key; ApproxKey; PartialKey ]
      in match search_index conf base fn sn order with
      | Some ip ->
        if identifier_person.Mread.Identifier_person.track_visit = Some true
        then record_visited conf ip;
        print_result_from_ip conf base ip
      | None -> print_error conf `not_found

(* ********************************************************************* *)
(*  [Fonc] print_fiche_person : conf -> base -> unit                     *)
(** [Description] : Affiche une fiche personne en fonction
    d'un identifiant.
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_fiche_person conf base =
  let fiche_parameters = get_params conf Mext_read.parse_fiche_parameters in
  let identifier_person = fiche_parameters.Mread.Fiche_parameters.identifier_person in
  let print_result_from_ip conf base ip =
      let nb_asc_max =
        match fiche_parameters.Mread.Fiche_parameters.nb_asc_max with
        | Some n -> Int32.to_int n
        | None -> 1 (* Add grand-parents. *)
      in
      let nb_desc_max =
        match fiche_parameters.Mread.Fiche_parameters.nb_desc_max with
        | Some n -> Int32.to_int n
        | None -> 0
      in
      let simple_graph_info =
        match fiche_parameters.Mread.Fiche_parameters.simple_graph_info with
        | Some b -> b
        | None -> false
      in
      let no_event =
        match fiche_parameters.Mread.Fiche_parameters.no_event with
        | Some b -> b
        | None -> false
      in
      print_result_fiche_person conf base ip nb_asc_max nb_desc_max simple_graph_info no_event
  in
  print_from_identifier_person conf base print_result_from_ip identifier_person


(**/**)

(* Graphe d'ascendance v2 *)

let build_graph_asc_v2 conf base p max_gen =
(*
  let () = load_ascends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge factor_from baseprefix_from p_from factor_to baseprefix_to p_to =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let id_from =
      Int64.of_string @@ string_of_int (Hashtbl.hash (baseprefix_from, get_iper p_from, factor_from))
    in
    let id_to =
      Int64.of_string @@ string_of_int (Hashtbl.hash (baseprefix_to, get_iper p_to, factor_to))
    in
    Mread.Edge.({
      from_node = id_from;
      to_node = id_to;
    })
  in
  let create_node p gen more_info base_prefix factor =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let uniq_id = Hashtbl.hash (base_prefix, get_iper p, factor) in
    let id = Int64.of_string @@ string_of_int uniq_id in
    let p = pers_to_piqi_person_tree conf base p more_info gen max_gen base_prefix in
    Mread.Node.({
      id = id;
      person = p;
      ifam = None;
    })
  in
(*
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
*)
  let nodes = ref [] in
  let edges = ref [] in
  let rec loop l =
    match l with
    | [] -> ()
    | (p, gen) :: l ->
        if gen >= max_gen then loop l
        else
          begin
            match get_parents p with
            | Some ifam ->
                let factor =
                  try Hashtbl.find ht (get_iper p) with Not_found -> 1
                in
                let cpl = foi base ifam in
                let fath = poi base (get_father cpl) in
                let moth = poi base (get_mother cpl) in
                let fath_factor =
                  try
                    let i = Hashtbl.find ht (get_iper fath) + 1 in
                    Hashtbl.replace ht (get_iper fath) i;
                    i
                  with Not_found -> Hashtbl.add ht (get_iper fath) 1; 1
                in
                let moth_factor =
                  try
                    let i = Hashtbl.find ht (get_iper moth) + 1 in
                    Hashtbl.replace ht (get_iper moth) i;
                    i
                  with Not_found -> Hashtbl.add ht (get_iper moth) 1; 1
                in
                nodes := create_node fath gen Ancestor conf.command fath_factor :: !nodes;
                nodes := create_node moth gen Ancestor conf.command moth_factor :: !nodes;
                edges := create_edge factor conf.command p fath_factor conf.command fath :: !edges;
                edges := create_edge factor conf.command p moth_factor conf.command moth :: !edges;
                (*create_family ifam families;*)
                loop ((fath, gen + 1) :: (moth, gen + 1) :: l)
            | None ->
                (* lien inter arbre *)
                let ip = get_iper p in
                let () =
                  Perso_link.init_cache conf base ip (max_gen - gen) 0 0
                in
                let () =
                  let ht = Hashtbl.create 42 in
                  let rec loop_parents l =
                    match l with
                    | [] -> ()
                    | (base_prefix, p, gen) :: l ->
                        if gen >= max_gen then loop_parents l
                        else
                          let factor =
                            try Hashtbl.find ht (base_prefix, get_iper p) with Not_found -> 1
                          in
                          let ip = get_iper p in
                          match Perso_link.get_parents_link base_prefix ip with
                          | Some family ->
                              begin
                                let ifath = Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.ifath in
                                let imoth = Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.imoth in
                                let fam_base_prefix = family.MLink.Family.baseprefix in
                                match
                                  (Perso_link.get_person_link fam_base_prefix ifath,
                                   Perso_link.get_person_link fam_base_prefix imoth,
                                   Perso_link.get_person_link base_prefix ip)
                                with
                                | (Some pfath, Some pmoth, Some _) ->
                                    let (fath, _) = Perso_link.make_ep_link base pfath in
                                    let (moth, _) = Perso_link.make_ep_link base pmoth in
                                    let fath_factor =
                                      try
                                        let i = Hashtbl.find ht (fam_base_prefix, get_iper fath) + 1 in
                                        Hashtbl.replace ht (fam_base_prefix, get_iper fath) i;
                                        i
                                      with Not_found -> Hashtbl.add ht (fam_base_prefix, get_iper fath) 1; 1
                                    in
                                    let moth_factor =
                                      try
                                        let i = Hashtbl.find ht (fam_base_prefix, get_iper moth) + 1 in
                                        Hashtbl.replace ht (fam_base_prefix, get_iper moth) i;
                                        i
                                      with Not_found -> Hashtbl.add ht (fam_base_prefix, get_iper moth) 1; 1
                                    in
                                    nodes := create_node fath gen Ancestor pfath.MLink.Person.baseprefix fath_factor :: !nodes;
                                    nodes := create_node moth gen Ancestor pmoth.MLink.Person.baseprefix moth_factor :: !nodes;
                                    edges := create_edge factor base_prefix p fath_factor pfath.MLink.Person.baseprefix fath :: !edges;
                                    edges := create_edge factor base_prefix p moth_factor pmoth.MLink.Person.baseprefix moth :: !edges;
                                    let l =
                                      ((fam_base_prefix, fath, gen + 1) :: (fam_base_prefix, moth, gen + 1) :: l)
                                    in
                                    loop_parents l
                                | _ -> loop_parents l
                              end
                          | None -> loop_parents l
                  in
                  loop_parents [(conf.command, p, gen)]
                in
                loop l
          end
  in
  nodes := create_node p 1 Root conf.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges)


(* Graphe de descendance v2 *)

let build_graph_desc_v2 conf base p max_gen =
(*
  let () = load_descends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge factor_from baseprefix_from p_from factor_to baseprefix_to p_to =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let id_from =
      Int64.of_string @@ string_of_int (Hashtbl.hash (baseprefix_from, get_iper p_from, factor_from))
    in
    let id_to =
      Int64.of_string @@ string_of_int (Hashtbl.hash (baseprefix_to, get_iper p_to, factor_to))
    in
    Mread.Edge.({
      from_node = id_from;
      to_node = id_to;
    })
  in
  let create_node p ifam gen more_info base_prefix factor =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let uniq_id = Hashtbl.hash (base_prefix, get_iper p, factor) in
    let id = Int64.of_string @@ string_of_int uniq_id in
    let p = pers_to_piqi_person_tree conf base p more_info gen max_gen base_prefix in
    let ifam = Int64.of_string @@ Gwdb.string_of_ifam ifam in
    Mread.Node.({
      id = id;
      person = p;
      ifam = Some ifam;
    })
  in
(*
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
*)
  let nodes = ref [] in
  let edges = ref [] in
  let rec loop l =
    match l with
    | [] -> ()
    | (p, gen) :: l ->
        if gen >= max_gen then loop l
        else
          begin
            let factor =
              try Hashtbl.find ht (get_iper p) with Not_found -> 1
            in
            let ifam = get_family p in
            let l =
              Array.fold_left
                (fun accu ifam  ->
                  let fam = foi base ifam in
                  let sp = poi base (Gutil.spouse (get_iper p) fam) in
                  let sp_factor =
                    try
                      let i = Hashtbl.find ht (get_iper sp) + 1 in
                      Hashtbl.replace ht (get_iper sp) i;
                      i
                    with Not_found -> Hashtbl.add ht (get_iper sp) 1; 1
                  in
                  let children =
                    Mutil.array_to_list_map (poi base) (get_children fam)
                  in
                  nodes := create_node sp ifam gen Spouse conf.command sp_factor :: !nodes;
                  edges := create_edge factor conf.command p sp_factor conf.command sp :: !edges;
                  if gen <> max_gen then
                    begin
                      List.iter
                        (fun c ->
                          let c_factor =
                            try
                              let i = Hashtbl.find ht (get_iper c) + 1 in
                              Hashtbl.replace ht (get_iper c) i;
                              i
                            with Not_found -> Hashtbl.add ht (get_iper c) 1; 1
                          in
                          nodes := create_node c ifam gen Children conf.command c_factor :: !nodes;
                          edges := create_edge factor conf.command p c_factor conf.command c :: !edges;
                          edges := create_edge sp_factor conf.command sp c_factor conf.command c :: !edges)
                        children;
                      (*create_family ifam families;*)
                      let child_local =
                        List.fold_left
                          (fun accu c -> (c, gen + 1) :: accu)
                          accu children
                      in

                      (* lien inter arbre *)
                      let () =
                        Perso_link.init_cache conf base (get_iper p) 1 1 (max_gen - gen)
                      in
                      let () =
                        let ht = Hashtbl.create 42 in
                        let rec loop_child fam_link =
                          match fam_link with
                          | [] -> ()
                          | (base_prefix, p, gen) :: l ->
                              if gen >= max_gen then loop_child l
                              else
                                begin
                                  let factor =
                                    try Hashtbl.find ht (base_prefix, get_iper p) with Not_found -> 1
                                  in
                                  let family_link =
                                    Perso_link.get_families_of_parents
                                      base_prefix (get_iper p) (get_iper sp)
                                  in
                                  let children_link =
                                    List.fold_left
                                      (fun accu fam_link ->
                                        let (ifath, imoth, ifam) =
                                          (Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath,
                                           Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth,
                                           Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam)
                                        in
                                        let cpl =
                                          let ip = get_iper p in
                                          if ip <> ifath && ip <> imoth then
                                          match
                                            Perso_link.get_person_link_with_base
                                              conf.command ip fam_link.MLink.Family.baseprefix
                                          with
                                          | Some p ->
                                              let ip = Gwdb.iper_of_string @@ Int32.to_string p.MLink.Person.ip in
                                              (ifath, imoth, if ip = ifath then imoth else ifath)
                                          | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
                                          else (ifath, imoth, if ip = ifath then imoth else ifath)
                                        in
                                        let (_, _, isp) = cpl in
                                        let sp_factor =
                                          try
                                            let i = Hashtbl.find ht (fam_link.MLink.Family.baseprefix, isp) + 1 in
                                            Hashtbl.replace ht (fam_link.MLink.Family.baseprefix, isp) i;
                                            i
                                          with Not_found -> Hashtbl.add ht (fam_link.MLink.Family.baseprefix, isp) 1; 1
                                        in
                                        List.fold_left
                                          (fun accu c_link ->
                                            let baseprefix = c_link.MLink.Person_link.baseprefix in
                                            let ip_c = Gwdb.iper_of_string @@ Int32.to_string c_link.MLink.Person_link.ip in
                                            match Perso_link.get_person_link baseprefix ip_c with
                                            | Some c_link ->
                                                let can_merge =
                                                  Perso_link.can_merge_child base_prefix
                                                    (get_children fam) c_link
                                                in
                                                if can_merge then accu
                                                else
                                                  let (c, _) = Perso_link.make_ep_link base c_link in
                                                  let c_factor =
                                                    try
                                                      let i = Hashtbl.find ht (baseprefix, get_iper c) + 1 in
                                                      Hashtbl.replace ht (baseprefix, get_iper c) i;
                                                      i
                                                    with Not_found -> Hashtbl.add ht (baseprefix, get_iper c) 1; 1
                                                  in
                                                  nodes := create_node c ifam gen Children baseprefix c_factor :: !nodes;
                                                  edges := create_edge factor base_prefix p c_factor baseprefix c :: !edges;
                                                  edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                                                  (baseprefix, c, gen + 1) :: accu
                                            | None -> accu)
                                          accu fam_link.MLink.Family.children)
                                      l family_link
                                  in
                                  loop_child children_link;
                                end
                        in
                        loop_child [(conf.command, p, gen)]
                      in
                      child_local
                    end
                  else accu)
                l ifam
            in

            (* lien inter arbre *)
            let () =
              Perso_link.init_cache conf base (get_iper p) 1 1 (max_gen - gen)
            in
            let () =
              let ht = Hashtbl.create 42 in
              let rec loop_desc l =
                match l with
                | [] -> ()
                | (base_prefix, p, gen) :: l ->
                    if gen >= max_gen then loop_desc l
                    else
                      begin
                        let ip = get_iper p in
                        let families = Perso_link.get_family_link base_prefix ip in
                        let l =
                          List.fold_left
                            (fun accu fam_link ->
                               let (ifath, imoth, _) =
                                 (Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath,
                                  Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth,
                                  Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam)
                               in
                               let cpl =
                                 let ip = get_iper p in
                                 if ip <> ifath && ip <> imoth then
                                   match
                                     Perso_link.get_person_link_with_base
                                       conf.command ip fam_link.MLink.Family.baseprefix
                                   with
                                   | Some p ->
                                       let ip = Gwdb.iper_of_string @@ Int32.to_string p.MLink.Person.ip in
                                       (ifath, imoth, if ip = ifath then imoth else ifath)
                                   | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
                                 else (ifath, imoth, if ip = ifath then imoth else ifath)
                               in
                               let can_merge =
                                 let fam = Mutil.array_to_list_map (foi base) (get_family p) in
                                 Perso_link.can_merge_family conf.command (get_iper p) fam fam_link cpl
                               in
                               if can_merge then accu
                               else
                                 let (_, _, isp) = cpl in
                                 match Perso_link.get_person_link fam_link.MLink.Family.baseprefix isp with
                                 | Some sp ->
                                     let (sp, _) = Perso_link.make_ep_link base sp in
                                     let baseprefix = fam_link.MLink.Family.baseprefix in
                                     let ifam = Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam in
                                     let sp_factor =
                                       try
                                         let i = Hashtbl.find ht (baseprefix, get_iper sp) + 1 in
                                         Hashtbl.replace ht (baseprefix, get_iper sp) i;
                                         i
                                       with Not_found -> Hashtbl.add ht (baseprefix, get_iper sp) 1; 1
                                     in
                                     nodes := create_node sp ifam gen Spouse baseprefix sp_factor :: !nodes;
                                     edges := create_edge factor base_prefix p sp_factor baseprefix sp :: !edges;
                                     if gen <> max_gen then
                                       begin
                                         let family_link =
                                           Perso_link.get_families_of_parents baseprefix ifath imoth
                                         in
                                         List.fold_left
                                           (fun accu fam_link ->
                                              List.fold_left
                                                (fun accu c_link ->
                                                   let baseprefix = c_link.MLink.Person_link.baseprefix in
                                                   let ip_c = Gwdb.iper_of_string @@ Int32.to_string c_link.MLink.Person_link.ip in
                                                   match Perso_link.get_person_link baseprefix ip_c with
                                                   | Some c_link ->
                                                     let (c, _) = Perso_link.make_ep_link base c_link in
                                                     let c_factor =
                                                       try
                                                         let i = Hashtbl.find ht (baseprefix, get_iper c) + 1 in
                                                         Hashtbl.replace ht (baseprefix, get_iper c) i;
                                                         i
                                                       with Not_found -> Hashtbl.add ht (baseprefix, get_iper c) 1; 1
                                                     in
                                                     nodes := create_node c ifam gen Children baseprefix c_factor :: !nodes;
                                                     edges := create_edge factor base_prefix p c_factor baseprefix c :: !edges;
                                                     edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                                                     (baseprefix, c, gen + 1) :: accu
                                                   | None -> accu)
                                                accu fam_link.MLink.Family.children)
                                           accu family_link
                                       end
                                     else accu
                                 | None -> accu)
                            l families
                        in
                        loop_desc l
                      end
              in
              loop_desc [(conf.command, p, gen)]
            in

            loop l
          end
  in
  nodes := create_node p Gwdb.dummy_ifam 1 Root conf.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges)


(* ********************************************************************* *)
(*  [Fonc] print_result_graph_tree_v2 : conf -> base -> todo                    *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
    [Retour] :
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_result_graph_tree_v2 conf base ip =
  let params = get_params conf Mext_read.parse_graph_tree_params in
  (* Construction de la base avec calcul des sosas           *)
  (* Si iz présent, on prend iz comme souche pour le calcul  *)
  (* Sinon on prend la souche de l'arbre                     *)
  let () =
    match params.Mread.Graph_tree_params.indexz with
      | Some n -> Perso.build_sosa_tree_ht conf base (poi base (Gwdb.iper_of_string @@ Int32.to_string n))
      | None -> Perso.build_sosa_ht conf base
    in
  let p = poi base ip in
  let max_asc = 12 in
  let nb_asc =
    match params.Mread.Graph_tree_params.nb_asc with
    | Some n -> min max_asc (max (Int32.to_int n) 1)
    | None -> max_asc
  in
  (* cache lien inter arbre *)
  let () = Perso_link.init_cache conf base ip 1 1 1 in
  let (nodes_asc, edges_asc) = build_graph_asc_v2 conf base p nb_asc in
  (*
  let nodes_asc =
    List.rev_map
      (fun p ->
        let id = Int32.of_string @@ Gwdb.string_of_iper (get_iper p) in
        let p = pers_to_piqi_person_tree conf base p in
        Mread.Node.({
          id = id;
          person = p;
          ifam = None;
        }))
      nodes_asc
  in
  *)
  let max_desc = 12 in
  let nb_desc =
    match params.Mread.Graph_tree_params.nb_desc with
    | Some n -> min max_desc (max (Int32.to_int n) 1)
    | None -> max_desc
  in
  let (nodes_desc, edges_desc) = build_graph_desc_v2 conf base p nb_desc in
  let nodes_siblings =
    match get_parents p with
    | Some ifam ->
        let fam = foi base ifam in
        Array.fold_right
          (fun ic accu ->
            if ic = ip then accu
            else
              let c = poi base ic in
              (* Pour les liens inter arbres, on rend l'id unique avec *)
              (* le prefix de la base et l'index de la personne.       *)
              let uniq_id = Hashtbl.hash (conf.command, ic) in
              let id = Int64.of_string @@ string_of_int uniq_id in
              let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.command in
              let node =
                Mread.Node.({
                  id = id;
                  person = c;
                  ifam = None;
                })
              in
              node :: accu)
          (get_children fam) []
    | None -> []
  in
  let (nodes_siblings_before, nodes_siblings_after) =
    match get_parents p with
    | Some ifam ->
        let fam = foi base ifam in
        let children = Array.to_list (get_children fam) in
        let rec split_at_person before after l =
          match l with
          | [] -> (List.rev before, after)
          | ic :: l ->
              if ic = ip then
                let after =
                  List.map
                    (fun ic ->
                      let c = poi base ic in
                      (* Pour les liens inter arbres, on rend l'id unique avec *)
                      (* le prefix de la base et l'index de la personne.       *)
                      let uniq_id = Hashtbl.hash (conf.command, ic) in
                      let id = Int64.of_string @@ string_of_int uniq_id in
                      let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.command in
                      Mread.Node.({
                        id = id;
                        person = c;
                        ifam = None;
                      }))
                    l
                in
                (List.rev before, after)
              else
                let c = poi base ic in
                (* Pour les liens inter arbres, on rend l'id unique avec *)
                (* le prefix de la base et l'index de la personne.       *)
                let uniq_id = Hashtbl.hash (conf.command, ic) in
                let id = Int64.of_string @@ string_of_int uniq_id in
                let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.command in
                let node =
                  Mread.Node.({
                    id = id;
                    person = c;
                    ifam = None;
                  })
                in
                split_at_person (node :: before) after l
        in
        split_at_person [] [] children
    | None -> ([], [])
  in
  let graph =
    Mread.Graph_tree_new.({
      nodes_asc = nodes_asc;
      edges_asc = edges_asc;
      nodes_desc = nodes_desc;
      edges_desc = edges_desc;
      nodes_siblings = nodes_siblings;
      nodes_siblings_before = nodes_siblings_before;
      nodes_siblings_after = nodes_siblings_after;
    })
  in
  let data = Mext_read.gen_graph_tree_new graph in
  print_result conf data


(**/**) (* Version identique mais avec les infos "complète" d'une personne. *)


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_tree_full :
             config -> base -> person -> PersonTreeFull                       *)
(** [Description] : Retourne à partir d'une person (gwdb) une
                    PersonTreeFull (piqi)
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_tree_full conf base p more_info gen max_gen base_prefix =
  if is_restricted conf base (get_iper p) then
    {
      Mread.Person_tree_full.index = Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
      sex = `unknown;
      lastname = "x";
      firstname = "x";
      n = "";
      p = "";
      occ = Int32.of_int 0;
      image = None;
      sosa = `no_sosa;
      public_name = None;
      aliases = [];
      qualifiers = [];
      firstname_aliases = [];
      surname_aliases = [];
      birth_date = None;
      birth_place = None;
      birth_src = None;
      baptism_date = None;
      baptism_place = None;
      baptism_src = None;
      death_date = None;
      death_place = None;
      death_src = None;
      death_type = `dont_know_if_dead;
      burial_date = None;
      burial_place = None;
      burial_src = None;
      occupation = None;
      psources = None;
      titles = [];
      visible_for_visitors = false;
      has_more_infos = false;
      baseprefix = "";
    }
  else
    let gen_p = Util.string_gen_person base (gen_person_of_person p) in
    let p_auth = authorized_age conf base p in
    let index = Int32.of_string @@ Gwdb.string_of_iper gen_p.key_index in
    let sex =
      match gen_p.sex with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown
    in
    let sosa =
      if conf.bname <> Link.chop_base_prefix base_prefix then `no_sosa
      else
        let sosa_nb = Perso.get_sosa_person p in
        if Sosa.eq sosa_nb Sosa.zero then `no_sosa
        else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
        else `sosa
    in
    let sn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower gen_p.surname
    in
    let fn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower gen_p.first_name
    in
    let occ = Int32.of_int (get_occ p) in
    let (first_name, surname) =
      if not p_auth && (is_hide_names conf p) then ("x", "x")
      else person_firstname_surname_txt base p
    in
    let image =
      if has_image conf base p then
        let img = gen_p.image in
        if img <> "" then img
        else if Api_util.find_image_file conf base p <> None
        then "1"
        else ""
      else ""
    in
    let public_name =
      if gen_p.public_name = "" then None
      else Some gen_p.public_name
    in
    let aliases = gen_p.aliases in
    let qualifiers = gen_p.qualifiers in
    let firstname_aliases = gen_p.first_names_aliases in
    let surname_aliases = gen_p.surnames_aliases in
    let birth =
      match Adef.od_of_cdate gen_p.birth with
      | Some d when p_auth -> Some (string_of_date d)
      | _ -> None
    in
    let birth_place =
      if p_auth then Some gen_p.birth_place
      else None
    in
    let birth_src =
      if p_auth then Some gen_p.birth_src
      else None
    in
    let baptism =
      match Adef.od_of_cdate gen_p.baptism with
      | Some d when p_auth -> Some (string_of_date d)
      | _ -> None
    in
    let baptism_place =
      if p_auth then Some gen_p.baptism_place
      else None
    in
    let baptism_src =
      if p_auth then Some gen_p.baptism_src
      else None
    in
    let (death_type, death) =
      if p_auth then
        match gen_p.death with
        | NotDead -> (`not_dead, None)
        | Death (_, cd) ->
            let d = Adef.date_of_cdate cd in
            (`dead, Some (string_of_date d))
        | DeadYoung -> (`dead_young, None)
        | DeadDontKnowWhen -> (`dead_dont_know_when, None)
        | DontKnowIfDead -> (`dont_know_if_dead, None)
        | OfCourseDead -> (`of_course_dead, None)
      else
        (`not_dead, None)
    in
    let death_place =
      if p_auth then Some gen_p.death_place
      else None
    in
    let death_src =
      if p_auth then Some gen_p.death_src
      else None
    in
    let burial =
      match get_burial p with
      | Buried cod | Cremated cod ->
          (match Adef.od_of_cdate cod with
          | Some d when p_auth -> Some (string_of_date d)
          | _ -> None)
      | _ -> None
    in
    let burial_place =
      if p_auth then Some gen_p.burial_place
      else None
    in
    let burial_src =
      if p_auth then Some gen_p.burial_src
      else None
    in
    let titles =
      List.map
        (fun t ->
          let (title_type, name) =
            match t.t_name with
            | Tmain -> (`title_main, "")
            | Tname name -> (`title_name, name)
            | Tnone -> (`title_none, "")
          in
          let title = t.t_ident in
          let fief = t.t_place in
          let date_begin =
            match Adef.od_of_cdate t.t_date_start with
            | Some d -> Some (string_of_date d)
            | None -> None
          in
          let date_end =
            match Adef.od_of_cdate t.t_date_end with
            | Some d -> Some (string_of_date d)
            | None -> None
          in
          let nth = Some (Int32.of_int t.t_nth) in
          Mread.Title.({
            title_type = title_type;
            name = if name = "" then None else Some name;
            title = if title = "" then None else Some title;
            fief = if fief = "" then None else Some fief;
            date_begin = date_begin;
            date_end = date_end;
            nth = nth;
          }))
        gen_p.titles
    in
    let occupation =
      if p_auth then Some gen_p.occupation
      else None
    in
    let psources =
      if p_auth then Some gen_p.psources
      else None
    in
    let visible = is_visible conf base p in
    let has_more_infos =
      match more_info with
      | Root -> false
      | Siblings -> Array.length (get_family p) > 0
      | Children ->
           gen = max_gen - 1 && Array.length (get_family p) > 0
      | Ancestor ->
          let has_parents = get_parents p <> None in
          (gen = max_gen - 1 && has_parents) ||
           (fst (Array.fold_left
                   (fun (children_or_spouses, nb_fam) ifam ->
                     let nb_fam = succ nb_fam in
                     let fam = foi base ifam in
                     let children = get_children fam in
                     (children_or_spouses ||
                      (gen > 1 && Array.length children > 1) ||
                      nb_fam > 1,
                      nb_fam))
                   (false, 0) (get_family p)))
      | Spouse ->
          let has_parents = get_parents p <> None in
          has_parents || Array.length (get_family p) > 1
    in
    {
      Mread.Person_tree_full.index = index;
      sex = sex;
      lastname = surname;
      firstname = first_name;
      n = sn;
      p = fn;
      occ = occ;
      image = if image = "" then None else Some image;
      sosa = sosa;
      public_name = public_name;
      aliases = aliases;
      qualifiers = qualifiers;
      firstname_aliases = firstname_aliases;
      surname_aliases = surname_aliases;
      birth_date = birth;
      birth_place = birth_place;
      birth_src = birth_src;
      baptism_date = baptism;
      baptism_place = baptism_place;
      baptism_src = baptism_src;
      death_date = death;
      death_place = death_place;
      death_src = death_src;
      death_type = death_type;
      burial_date = burial;
      burial_place = burial_place;
      burial_src = burial_src;
      occupation = occupation;
      psources = psources;
      titles = titles;
      visible_for_visitors = visible;
      has_more_infos = has_more_infos;
      baseprefix = base_prefix
    }


(* ********************************************************************* *)
(*  [Fonc] fam_to_piqi_family_tree :
             config -> base -> ifam -> Full_family                       *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      -
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let fam_to_piqi_family_tree conf base ifam =
  let fam = foi base ifam in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let m_auth =
    authorized_age conf base (pget conf base ifath) &&
    authorized_age conf base (pget conf base imoth)
  in
  let index = Int32.of_string @@ Gwdb.string_of_ifam ifam in
  let fsources =
    if m_auth then Some gen_f.fsources
    else None
  in
  let marriage =
    match Adef.od_of_cdate gen_f.marriage with
    | Some d when m_auth -> Some (string_of_date d)
    | _ -> None
  in
  let marriage_place =
    if m_auth then Some gen_f.marriage_place
    else None
  in
  let marriage_src =
    if m_auth then Some gen_f.marriage_src
    else None
  in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
    | MarriageBann -> `marriage_bann
    | MarriageContract -> `marriage_contract
    | MarriageLicense -> `marriage_license
    | Pacs -> `pacs
    | Residence -> `residence
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
        (match Adef.od_of_cdate cod with
         | Some d when m_auth -> (`divorced, Some (string_of_date d))
         | _ -> (`divorced, None))
    | Separated -> (`separated, None)
  in
  {
    Mread.Family_tree_full.fsources = fsources;
    marriage_date = marriage;
    marriage_place = marriage_place;
    marriage_src = marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    index = index;
  }


(* *********************************************************************** *)
(*  [Fonc] fam_to_piqi_family_tree_link :
             config -> base -> ifam -> Full_family                         *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      -
    [Rem] : Non exporté en clair hors de ce module.                        *)
(* *********************************************************************** *)
let fam_to_piqi_family_tree_link base ifam fam =
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let index = Int32.of_string @@ Gwdb.string_of_ifam ifam in
  let fsources = None in
  let marriage =
    match Adef.od_of_cdate gen_f.marriage with
    | Some d -> Some (string_of_date d)
    | _ -> None
  in
  let marriage_place = Some gen_f.marriage_place in
  let marriage_src = Some gen_f.marriage_src in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
    | MarriageBann -> `marriage_bann
    | MarriageContract -> `marriage_contract
    | MarriageLicense -> `marriage_license
    | Pacs -> `pacs
    | Residence -> `residence
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
        (match Adef.od_of_cdate cod with
         | Some d -> (`divorced, Some (string_of_date d))
         | _ -> (`divorced, None))
    | Separated -> (`separated, None)
  in
  {
    Mread.Family_tree_full.fsources = fsources;
    marriage_date = marriage;
    marriage_place = marriage_place;
    marriage_src = marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    index = index;
  }


(* Graphe d'ascendance (api.proto) *)

let build_graph_asc_full conf base p max_gen =
(*
  let () = load_ascends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge factor_from baseprefix_from p_from factor_to baseprefix_to p_to =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let id_from =
      Int64.of_string @@ string_of_int (Hashtbl.hash (baseprefix_from, get_iper p_from, factor_from))
    in
    let id_to =
      Int64.of_string @@ string_of_int (Hashtbl.hash (baseprefix_to, get_iper p_to, factor_to))
    in
    Mread.Edge.({
      from_node = id_from;
      to_node = id_to;
    })
  in
  let create_node p gen more_info base_prefix factor =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let uniq_id = Hashtbl.hash (base_prefix, get_iper p, factor) in
    let id = Int64.of_int uniq_id in
    let p = pers_to_piqi_person_tree_full conf base p more_info gen max_gen base_prefix in
    Mread.Node_full.({
      id = id;
      person = p;
      ifam = None;
    })
  in
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family_tree conf base ifam) :: !families
  in
  let create_family_link ifam fam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families :=
        (fam_to_piqi_family_tree_link base ifam fam) :: !families
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  let rec loop l =
    match l with
    | [] -> ()
    | (p, gen) :: l ->
        if gen >= max_gen then loop l
        else
          begin
            match get_parents p with
            | Some ifam ->
                let factor =
                  try Hashtbl.find ht (get_iper p) with Not_found -> 1
                in
                let cpl = foi base ifam in
                let fath = poi base (get_father cpl) in
                let moth = poi base (get_mother cpl) in
                let fath_factor =
                  try
                    let i = Hashtbl.find ht (get_iper fath) + 1 in
                    Hashtbl.replace ht (get_iper fath) i;
                    i
                  with Not_found -> Hashtbl.add ht (get_iper fath) 1; 1
                in
                let moth_factor =
                  try
                    let i = Hashtbl.find ht (get_iper moth) + 1 in
                    Hashtbl.replace ht (get_iper moth) i;
                    i
                  with Not_found -> Hashtbl.add ht (get_iper moth) 1; 1
                in
                nodes := create_node fath gen Ancestor conf.command fath_factor :: !nodes;
                nodes := create_node moth gen Ancestor conf.command moth_factor :: !nodes;
                edges := create_edge factor conf.command p fath_factor conf.command fath :: !edges;
                edges := create_edge factor conf.command p moth_factor conf.command moth :: !edges;
                create_family ifam families;
                loop ((fath, gen + 1) :: (moth, gen + 1) :: l)
            | None ->
                (* lien inter arbre *)
                let ip = get_iper p in
                let () =
                  Perso_link.init_cache conf base ip (max_gen - gen) 0 0
                in
                let () =
                  let ht = Hashtbl.create 42 in
                  let rec loop_parents l =
                    match l with
                    | [] -> ()
                    | (base_prefix, p, gen) :: l ->
                        if gen >= max_gen then loop_parents l
                        else
                          let factor =
                            try Hashtbl.find ht (base_prefix, get_iper p) with Not_found -> 1
                          in
                          let ip = get_iper p in
                          match Perso_link.get_parents_link base_prefix ip with
                          | Some family ->
                              begin
                                let ifath = Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.ifath in
                                let imoth = Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.imoth in
                                let fam_base_prefix = family.MLink.Family.baseprefix in
                                let (ifam, fam, _, _) = Perso_link.make_efam_link conf base family in
                                match
                                  (Perso_link.get_person_link fam_base_prefix ifath,
                                   Perso_link.get_person_link fam_base_prefix imoth,
                                   Perso_link.get_person_link base_prefix ip)
                                with
                                | (Some pfath, Some pmoth, Some _) ->
                                    let (fath, _) = Perso_link.make_ep_link base pfath in
                                    let (moth, _) = Perso_link.make_ep_link base pmoth in
                                    let fath_factor =
                                      try
                                        let i = Hashtbl.find ht (fam_base_prefix, get_iper fath) + 1 in
                                        Hashtbl.replace ht (fam_base_prefix, get_iper fath) i;
                                        i
                                      with Not_found -> Hashtbl.add ht (fam_base_prefix, get_iper fath) 1; 1
                                    in
                                    let moth_factor =
                                      try
                                        let i = Hashtbl.find ht (fam_base_prefix, get_iper moth) + 1 in
                                        Hashtbl.replace ht (fam_base_prefix, get_iper moth) i;
                                        i
                                      with Not_found -> Hashtbl.add ht (fam_base_prefix, get_iper moth) 1; 1
                                    in
                                    nodes := create_node fath gen Ancestor pfath.MLink.Person.baseprefix fath_factor :: !nodes;
                                    nodes := create_node moth gen Ancestor pmoth.MLink.Person.baseprefix moth_factor :: !nodes;
                                    edges := create_edge factor base_prefix p fath_factor pfath.MLink.Person.baseprefix fath :: !edges;
                                    edges := create_edge factor base_prefix p moth_factor pmoth.MLink.Person.baseprefix moth :: !edges;
                                    create_family_link ifam fam families;
                                    let l =
                                      ((fam_base_prefix, fath, gen + 1) :: (fam_base_prefix, moth, gen + 1) :: l)
                                    in
                                    loop_parents l
                                | _ -> loop_parents l
                              end
                          | None -> loop_parents l
                  in
                  loop_parents [(conf.command, p, gen)]
                in
                loop l
          end
  in
  nodes := create_node p 1 Root conf.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges, List.rev !families)


(* Graphe de descendance (api.proto) *)

let build_graph_desc_full conf base p max_gen =
(*
  let () = load_descends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge factor_from baseprefix_from p_from factor_to baseprefix_to p_to =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let id_from =
      Int64.of_string @@ string_of_int (Hashtbl.hash (baseprefix_from, get_iper p_from, factor_from))
    in
    let id_to =
      Int64.of_string @@string_of_int (Hashtbl.hash (baseprefix_to, get_iper p_to, factor_to))
    in
    Mread.Edge.({
      from_node = id_from;
      to_node = id_to;
    })
  in
  let create_node p ifam gen more_info base_prefix factor =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let uniq_id = Hashtbl.hash (base_prefix, get_iper p, factor) in
    let id = Int64.of_int uniq_id in
    let p = pers_to_piqi_person_tree_full conf base p more_info gen max_gen base_prefix in
    let ifam = Int64.of_string @@ Gwdb.string_of_ifam ifam in
    Mread.Node_full.({
      id = id;
      person = p;
      ifam = Some ifam;
    })
  in
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family_tree conf base ifam) :: !families
  in
  let create_family_link ifam fam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families :=
        (fam_to_piqi_family_tree_link base ifam fam) :: !families
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  let rec loop l =
    match l with
    | [] -> ()
    | (p, gen) :: l ->
        if gen >= max_gen then loop l
        else
          begin
            let factor =
              try Hashtbl.find ht (get_iper p) with Not_found -> 1
            in
            let ifam = get_family p in
            let l =
              List.fold_left
                (fun accu ifam  ->
                  let fam = foi base ifam in
                  let sp = poi base (Gutil.spouse (get_iper p) fam) in
                  let sp_factor =
                    try
                      let i = Hashtbl.find ht (get_iper sp) + 1 in
                      Hashtbl.replace ht (get_iper sp) i;
                      i
                    with Not_found -> Hashtbl.add ht (get_iper sp) 1; 1
                  in
                  let children = Mutil.array_to_list_map (poi base) (get_children fam) in
                  nodes := create_node sp ifam gen Spouse conf.command sp_factor :: !nodes;
                  edges := create_edge factor conf.command p sp_factor conf.command sp :: !edges;
                  if gen <> max_gen then
                    begin
                      List.iter
                        (fun c ->
                          let c_factor =
                            try
                              let i = Hashtbl.find ht (get_iper c) + 1 in
                              Hashtbl.replace ht (get_iper c) i;
                              i
                            with Not_found -> Hashtbl.add ht (get_iper c) 1; 1
                          in
                          nodes := create_node c ifam gen Children conf.command c_factor :: !nodes;
                          edges := create_edge factor conf.command p c_factor conf.command c :: !edges;
                          edges := create_edge sp_factor conf.command sp c_factor conf.command c :: !edges)
                        children;
                      create_family ifam families;
                      let child_local =
                        List.fold_left
                          (fun accu c -> (c, gen + 1) :: accu)
                          accu children
                      in

                      (* lien inter arbre *)
                      let () =
                        Perso_link.init_cache conf base (get_iper p) 1 1 (max_gen - gen)
                      in
                      let () =
                        let ht = Hashtbl.create 42 in
                        let rec loop_child fam_link =
                          match fam_link with
                          | [] -> ()
                          | (base_prefix, p, gen) :: l ->
                              if gen >= max_gen then loop_child l
                              else
                                begin
                                  let factor =
                                    try Hashtbl.find ht (base_prefix, get_iper p) with Not_found -> 1
                                  in
                                  let family_link =
                                    Perso_link.get_families_of_parents
                                      base_prefix (get_iper p) (get_iper sp)
                                  in
                                  let children_link =
                                    List.fold_left
                                      (fun accu fam_link ->
                                        let (ifath, imoth, ifam) =
                                          (Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath,
                                           Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth,
                                           Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam)
                                        in
                                        let cpl =
                                          let ip = get_iper p in
                                          if ip <> ifath && ip <> imoth then
                                          match
                                            Perso_link.get_person_link_with_base
                                              conf.command ip fam_link.MLink.Family.baseprefix
                                          with
                                          | Some p ->
                                              let ip = Gwdb.iper_of_string @@ Int32.to_string p.MLink.Person.ip in
                                              (ifath, imoth, if ip = ifath then imoth else ifath)
                                          | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
                                          else (ifath, imoth, if ip = ifath then imoth else ifath)
                                        in
                                        let (_, _, isp) = cpl in
                                        let sp_factor =
                                          try
                                            let i = Hashtbl.find ht (fam_link.MLink.Family.baseprefix, isp) + 1 in
                                            Hashtbl.replace ht (fam_link.MLink.Family.baseprefix, isp) i;
                                            i
                                          with Not_found -> Hashtbl.add ht (fam_link.MLink.Family.baseprefix, isp) 1; 1
                                        in
                                        create_family_link ifam fam families;
                                        List.fold_left
                                          (fun accu c_link ->
                                            let baseprefix = c_link.MLink.Person_link.baseprefix in
                                            let ip_c = Gwdb.iper_of_string @@ Int32.to_string c_link.MLink.Person_link.ip in
                                            match Perso_link.get_person_link baseprefix ip_c with
                                            | Some c_link ->
                                                let can_merge =
                                                  Perso_link.can_merge_child base_prefix
                                                    (get_children fam) c_link
                                                in
                                                if can_merge then accu
                                                else
                                                  let (c, _) = Perso_link.make_ep_link base c_link in
                                                  let c_factor =
                                                    try
                                                      let i = Hashtbl.find ht (baseprefix, get_iper c) + 1 in
                                                      Hashtbl.replace ht (baseprefix, get_iper c) i;
                                                      i
                                                    with Not_found -> Hashtbl.add ht (baseprefix, get_iper c) 1; 1
                                                  in
                                                  nodes := create_node c ifam gen Children baseprefix c_factor :: !nodes;
                                                  edges := create_edge factor base_prefix p c_factor baseprefix c :: !edges;
                                                  edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                                                  (baseprefix, c, gen + 1) :: accu
                                            | None -> accu)
                                          accu fam_link.MLink.Family.children)
                                      l family_link
                                  in
                                  loop_child children_link;
                                end
                        in
                        loop_child [(conf.command, p, gen)]
                      in
                      child_local
                    end
                  else accu)
                l (Array.to_list ifam)
            in

            (* lien inter arbre *)
            let () =
              Perso_link.init_cache conf base (get_iper p) 1 1 (max_gen - gen)
            in
            let () =
              let ht = Hashtbl.create 42 in
              let rec loop_desc l =
                match l with
                | [] -> ()
                | (base_prefix, p, gen) :: l ->
                    if gen >= max_gen then loop_desc l
                    else
                      begin
                        let ip = get_iper p in
                        let families = Perso_link.get_family_link base_prefix ip in
                        let l =
                          List.fold_left
                            (fun accu fam_link ->
                               let (ifath, imoth, _) =
                                 (Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath,
                                  Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth,
                                  Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam)
                               in
                               let cpl =
                                 let ip = get_iper p in
                                 if ip <> ifath && ip <> imoth then
                                   match
                                     Perso_link.get_person_link_with_base
                                       conf.command ip fam_link.MLink.Family.baseprefix
                                   with
                                   | Some p ->
                                       let ip = Gwdb.iper_of_string @@ Int32.to_string p.MLink.Person.ip in
                                       (ifath, imoth, if ip = ifath then imoth else ifath)
                                   | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
                                 else (ifath, imoth, if ip = ifath then imoth else ifath)
                               in
                               let can_merge =
                                 let fam = Mutil.array_to_list_map (foi base) (get_family p) in
                                 Perso_link.can_merge_family conf.command (get_iper p) fam fam_link cpl
                               in
                               if can_merge then accu
                               else
                                 let (_, _, isp) = cpl in
                                 match Perso_link.get_person_link fam_link.MLink.Family.baseprefix isp with
                                 | Some sp ->
                                     let (sp, _) = Perso_link.make_ep_link base sp in
                                     let baseprefix = fam_link.MLink.Family.baseprefix in
                                     let ifam = Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam in
                                     let sp_factor =
                                       try
                                         let i = Hashtbl.find ht (baseprefix, get_iper sp) + 1 in
                                         Hashtbl.replace ht (baseprefix, get_iper sp) i;
                                         i
                                       with Not_found -> Hashtbl.add ht (baseprefix, get_iper sp) 1; 1
                                     in
                                     nodes := create_node sp ifam gen Spouse baseprefix sp_factor :: !nodes;
                                     edges := create_edge factor base_prefix p sp_factor baseprefix sp :: !edges;
                                     if gen <> max_gen then
                                       begin
                                         let family_link =
                                           Perso_link.get_families_of_parents baseprefix ifath imoth
                                         in
                                         (* TODO ?
                                         let (_, fam, _, _) =
                                           Perso_link.make_efam_link conf base ip fam_link
                                         in
                                         create_family_link (ifath, imoth) ifam fam families;
                                         *)
                                         List.fold_left
                                           (fun accu fam_link ->
                                              List.fold_left
                                                (fun accu c_link ->
                                                   let baseprefix = c_link.MLink.Person_link.baseprefix in
                                                   let ip_c = Gwdb.iper_of_string @@ Int32.to_string c_link.MLink.Person_link.ip in
                                                   match Perso_link.get_person_link baseprefix ip_c with
                                                   | Some c_link ->
                                                     let (c, _) = Perso_link.make_ep_link base c_link in
                                                     let c_factor =
                                                       try
                                                         let i = Hashtbl.find ht (baseprefix, get_iper c) + 1 in
                                                         Hashtbl.replace ht (baseprefix, get_iper c) i;
                                                         i
                                                       with Not_found -> Hashtbl.add ht (baseprefix, get_iper c) 1; 1
                                                     in
                                                     nodes := create_node c ifam gen Children baseprefix c_factor :: !nodes;
                                                     edges := create_edge factor base_prefix p c_factor baseprefix c :: !edges;
                                                     edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                                                     (baseprefix, c, gen + 1) :: accu
                                                   | None -> accu)
                                                accu fam_link.MLink.Family.children)
                                           accu family_link
                                       end
                                     else accu
                                 | None -> accu)
                            l families
                        in
                        loop_desc l
                      end
              in
              loop_desc [(conf.command, p, gen)]
            in

            loop l
          end
  in
  nodes := create_node p Gwdb.dummy_ifam 1 Root conf.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges, List.rev !families)

(* ************************************************************************ *)
(*  [Fonc] get_nb_ancestors : config -> base -> ip -> int                   *)
(** [Description] : Retourne le nombre d'ascendants d'une personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - ip   : l'index de la personne
    [Retour] : int
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let get_nb_ancestors base ip =
  (* Tableau qui conserve les index des personnes déjà parcourues. *)
  let visited_ips = Gwdb.iper_marker (Gwdb.ipers base) false in
  let rec count_nb_ancestors base not_visited_ips nb_visited_ips =
    match not_visited_ips with
      [] -> nb_visited_ips
      | current_ip::not_visited_ips ->
        if Gwdb.Marker.get visited_ips current_ip then
          (* Passe au noeud suivant si le noeud courant a déjà été visité. *)
          count_nb_ancestors base not_visited_ips nb_visited_ips
        else
          begin
            let not_visited_ips =
              match get_parents (poi base current_ip) with
              | Some ifam ->
                let cpl = foi base ifam in
                (* Ajoute les index des parents au tableau des noeuds à parcourir. *)
                not_visited_ips@[get_father cpl]@[get_mother cpl]
              | None ->
                (* Si pas de parents, le tableau des noeuds à visiter ne change pas. *)
                not_visited_ips
            in
            (* Met à jour le tableau des noeuds parcourus. *)
            Gwdb.Marker.set visited_ips current_ip true;
            (* Passe au noeud suivant en incrémentant le nombre de noeuds. *)
            count_nb_ancestors base not_visited_ips (nb_visited_ips + 1)
          end
  in
  (* Le nombre d'ascendants d'un individu est le nombre de personnes parcourues moins 1 (lui-même). *)
  count_nb_ancestors base [ip] (-1)

(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_nb_ancestors : conf -> base -> int -> NbAncestors     *)
(** [Description] : Retourne à partir d'un nombre un NbAncestors (piqi).
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - nb   : nombre d'ascendants
    [Retour] : NbAncestors
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let nb_to_piqi_nb_ancestors nb =
    let piqi_nb_ancestors = Mread.default_nb_ancestors() in
        piqi_nb_ancestors.Mread.Nb_ancestors.nb <- Int32.of_int nb;
    Mext_read.gen_nb_ancestors piqi_nb_ancestors

(* ********************************************************************* *)
(*  [Fonc] print_result_nb_ancestors : conf -> base -> ip -> unit        *)
(** [Description] : Retourne le nombre d'ascendants d'un individu.
    [Args] :
      - conf : configuration de la base.
      - base : base.
      - ip   : l'index de la personne.
    [Retour] : unit
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_result_nb_ancestors conf base ip =
    let data = nb_to_piqi_nb_ancestors (get_nb_ancestors base ip) in
    print_result conf data

(* ********************************************************************* *)
(*  [Fonc] print_nb_ancestors : conf -> base -> unit                     *)
(** [Description] : Retourne le nombre d'ascendants d'un individu.
    [Args] :
      - conf : configuration de la base.
      - base : base.
      - ip   : l'index de la personne.
    [Retour] : unit (NbAncestors | Error)
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_nb_ancestors conf base =
  print_from_identifier_person conf base print_result_nb_ancestors (get_params conf Mext_read.parse_identifier_person)

(* ********************************************************************* *)
(*  [Fonc] print_graph_tree_v2 : conf -> base -> unit                    *)
(** [Description] : Retourne un graph d'ascendance et de descendance
       d'une personne
    [Args] :
      - conf : configuration de la base.
      - base : base.
    [Retour] : unit (graph | Error)
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_graph_tree_v2 conf base =
  let params = get_params conf Mext_read.parse_graph_tree_params in
  let identifier_person = params.Mread.Graph_tree_params.identifier_person in
  print_from_identifier_person conf base print_result_graph_tree_v2 identifier_person

#endif
