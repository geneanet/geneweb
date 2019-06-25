#ifdef API

module M = Api_piqi
module Mext = Api_piqi_ext

module Mwrite = Api_saisie_write_piqi
module Mext_write = Api_saisie_write_piqi_ext

open Config
open Def
open Gwdb
open Util
open Api_util


(**/**) (* Misc *)

let new_gutil_find_free_occ base f s i =
  let ipl = persons_of_name base (f ^ " " ^ s) in
  let first_name = Name.lower f in
  let surname = Name.lower s in
  let list_occ =
    let rec loop list =
      function
      | ip :: ipl ->
          let p = poi base ip in
          if not (List.mem (get_occ p) list) &&
             first_name = Name.lower (p_first_name base p) &&
             surname = Name.lower (p_surname base p) then
            loop (get_occ p :: list) ipl
          else loop list ipl
      | [] -> list
    in
    loop [] ipl
  in
  let list_occ = List.sort compare list_occ in
  let rec loop cnt1 =
    function
    | cnt2 :: list ->
        if cnt2 <= i || cnt1 = cnt2 then loop (cnt2 + 1) list
        else loop cnt1 list
    | [] -> cnt1
  in
  loop 0 list_occ

let ht_free_occ = Hashtbl.create 33 ;;
let api_find_free_occ base fn sn =
  let key = Name.lower (fn ^ " " ^ sn) in
  try
    begin
      let free_occ = Hashtbl.find ht_free_occ key in
      let free_occ = succ free_occ in
      let base_free_occ = new_gutil_find_free_occ base fn sn free_occ in
      let occ = max free_occ base_free_occ in
      Hashtbl.add ht_free_occ key occ;
      occ
    end
  with Not_found ->
    begin
      (* On regarde dans la base quelle est le occ dispo. *)
      let free_occ = Gutil.find_free_occ base fn sn 0 in
      Hashtbl.add ht_free_occ key free_occ;
      free_occ
    end


(**/**) (* Type de retour de modification. *)


(* Voir également update.mli, erreurs possibles :
     - "UnknownPerson"
     - "AlreadyDefined"
     - "OwnAncestor"
     - "BadSexOfMarriedPerson"
     - "BaseChanged"
     - "BadDateFormat"
     - "CreateConflictOcc"
     - "AlreadyHasParent"
     - "FatherShouldBeMale"
     - "MotherShouldBeFemale"
     - "Disconnected"
     - "error"
   On ajoute également les erreurs suivantes :
     - "PersonKey"
   => On ne renvoie en fait qu'une seule string qui est
      directement traduite du côté GeneWeb.
*)
type update_base_status =
  | UpdateSuccess of CheckItem.base_warning list * CheckItem.base_misc list * (unit -> unit) list
  | UpdateError of string
  | UpdateErrorConflict of Mwrite.Create_conflict.t


(* Exception qui gère les conflits de création de personnes. *)
exception ModErrApiConflict of Mwrite.Create_conflict.t ;;

let error_conflict_person_link base (f, s, o, create, _, force_create) =
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
  | Update.Create (_, _) ->
      if f <> "?" && s <> "?" then
        if force_create then
          (* On a dit qu'on voulait forcer la création, donc on a *)
          (* calculer un occ de libre, si c'est pas le cas, c'est *)
          (* une erreur, et on doit quitter.                      *)
          if Gwdb.person_of_key base f s o <> None
          then failwith "error_conflict_person_link"
          else false
        else
          match Gwdb.persons_of_name base (f ^ " " ^ s) with
         | [] -> false
          | l ->
              (* On test nom et prénom individuellement, si c'est  *)
              (* trop gourmand, alors on pourrait juste dire qu'il *)
              (* y a un conflit.                                   *)
              let f = Name.lower f in
              let s = Name.lower s in
              let rec loop l =
                match l with
                | [] -> false
                | ip :: l ->
                    let p = poi base ip in
                    let fn = Name.lower (sou base (get_first_name p)) in
                    let sn = Name.lower (sou base (get_surname p)) in
                    if fn = f && sn = s then true
                    else loop l
              in
              loop l
        (*
        (match Gwdb.person_of_key base f s o with
         | Some ip -> true
         | None -> false)
        *)
             (*
             let fn = Util.translate_eval f in
             let sn = Util.translate_eval s in
             let key = fn ^ " " ^ sn in
             let ipl = Gutil.person_ht_find_all base key in
             let name = Name.lower (f ^ " " ^ s) in
             let rec loop ipl =
               match ipl with
               | [] -> false
               | ip :: ipl ->
                   let p1 = poi base ip in
                   if Name.lower (p_first_name base p1 ^ " " ^ p_surname base p1) = name &&
                      o = 0
                   then
                     true
                   else
                     loop ipl
             in
             loop ipl)
             *)
      else false
  | _ -> false

let check_person_conflict conf base sp =
  (* Vérification de la personne. *)
  if nb_of_persons base = 0 then () (* FIXME: remove this ? *)
  else
    begin
      let op = poi base (sp.key_index) in
      let ofn = sou base (get_first_name op) in
      let osn = sou base (get_surname op) in
      let oocc = get_occ op in
      if ofn = sp.first_name && osn = sp.surname && oocc = sp.occ then ()
      else
        begin
          let fn = Util.translate_eval sp.first_name in
          let sn = Util.translate_eval sp.surname in
          let key = fn ^ " " ^ sn in
          let ipl = Gutil.person_ht_find_all base key in
          (try UpdateIndOk.check_conflict conf base sp ipl
           with Update.ModErr _ ->
             let conflict =
               let form = Some `person_form1 in
               let lastname = sp.surname in
               let firstname = sp.first_name in
               {
                 Mwrite.Create_conflict.form = form;
                 witness = false;
                 rparents = false;
                 event = false;
                 pos = None;
                 pos_witness = None;
                 lastname = lastname;
                 firstname = firstname;
               }
             in
             raise (ModErrApiConflict conflict))
        end;
      (* Vérification des rparents. *)
      List.iteri begin fun i r ->
        match (r.r_fath, r.r_moth) with
        | (Some (f, s, o, create, var, force_create), None)
        | (None, Some (f, s, o, create, var, force_create)) ->
          if error_conflict_person_link base (f, s, o, create, var, force_create) then
            let form = Some `person_form1 in
            let conflict =
              {
                Mwrite.Create_conflict.form = form;
                witness = false;
                rparents = true;
                event = false;
                pos = Some (Int32.of_int i);
                pos_witness = None;
                lastname = s;
                firstname = f;
              }
            in
            raise (ModErrApiConflict conflict)
        | _ -> failwith __LOC__ (* Dans l'API, ne peut pas arriver *)
      end sp.rparents ;
      (* Vérification des pevents. *)
      List.iteri begin
        fun i evt ->
          Array.iteri begin
            fun j ((f, s, o, create, var, force_create), _) ->
              if error_conflict_person_link base (f, s, o, create, var, force_create) then
                let form = Some `person_form1 in
                let conflict =
                  {
                    Mwrite.Create_conflict.form = form;
                    witness = true;
                    rparents = false;
                    event = true;
                    pos = Some (Int32.of_int i);
                    pos_witness = Some (Int32.of_int j);
                    lastname = s;
                    firstname = f;
                  }
                in
                raise (ModErrApiConflict conflict)
          end evt.epers_witnesses ;
      end sp.pevents ;
    end

let check_family_conflict base sfam scpl sdes =
  (* Vérification des parents. *)
  Array.iteri begin fun i (f, s, o, create, var, force_create) ->
    if error_conflict_person_link base (f, s, o, create, var, force_create) then
      let form =
        if i = 0 then Some `person_form1 else Some `person_form2
      in
      let conflict =
        {
          Mwrite.Create_conflict.form = form;
          witness = false;
          rparents = false;
          event = false;
          pos = None;
          pos_witness = None;
          lastname = s;
          firstname = f;
        }
      in
      raise (ModErrApiConflict conflict)
  end (Adef.parent_array scpl) ;
  (* Vérification des fevents. *)
  List.iteri begin fun i evt ->
    Array.iteri begin fun j ((f, s, o, create, var, force_create), _) ->
      if error_conflict_person_link base (f, s, o, create, var, force_create) then
        let form = Some `family_form in
        let conflict =
          {
            Mwrite.Create_conflict.form = form;
            witness = true;
            rparents = false;
            event = true;
            pos = Some (Int32.of_int i);
            pos_witness = Some (Int32.of_int j);
            lastname = s;
            firstname = f;
          }
        in
        raise (ModErrApiConflict conflict)
    end evt.efam_witnesses ;
  end sfam.fevents ;
  (* Vérification des enfants. *)
  Array.iter begin fun (f, s, o, create, var, force_create) ->
    if error_conflict_person_link base (f, s, o, create, var, force_create) then
      let form = Some `person_form1 in
      let conflict =
        {
          Mwrite.Create_conflict.form = form;
          witness = false;
          rparents = false;
          event = false;
          pos = None;
          pos_witness = None;
          lastname = s;
          firstname = f;
        }
      in
      raise (ModErrApiConflict conflict)
  end sdes.children

(**/**) (* Convertion d'une date. *)


(* ************************************************************************ *)
(*  [Fonc] piqi_date_of_date : def.date -> piqi_date                        *)
(** [Description] : Converti une date en date piqi
    [Args] :
      - date : la date a convertir
    [Retour] :
      - piqi date : date du module Mwrite.
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let piqi_date_of_date date =
  match date with
  | Dgreg (dmy, cal) ->
      let (cal, dmy) =
        match cal with
        | Dgregorian -> (None, dmy)
        | Djulian -> (Some `julian, Calendar.julian_of_gregorian dmy)
        | Dfrench -> (Some `french, Calendar.french_of_gregorian dmy)
        | Dhebrew -> (Some `hebrew, Calendar.hebrew_of_gregorian dmy)
      in
      let (prec, dmy, dmy2) =
        let d = Some (Int32.of_int dmy.day) in
        let m = Some (Int32.of_int dmy.month) in
        let y = Some (Int32.of_int dmy.year) in
        let delta = Some (Int32.of_int dmy.delta) in
        let dmy1 = {Mwrite.Dmy.day = d; month = m; year = y; delta = delta;} in
        let (prec, dmy2) =
          match dmy.prec with
          | Sure -> (`sure, None)
          | About -> (`about, None)
          | Maybe -> (`maybe, None)
          | Before -> (`before, None)
          | After -> (`after, None)
          | OrYear dmy2 ->
              let d = Some (Int32.of_int dmy2.day2) in
              let m = Some (Int32.of_int dmy2.month2) in
              let y = Some (Int32.of_int dmy2.year2) in
              let delta = Some (Int32.of_int dmy2.delta2) in
              let dmy2 =
                {Mwrite.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`oryear, Some dmy2)
          | YearInt dmy2 ->
              let d = Some (Int32.of_int dmy2.day2) in
              let m = Some (Int32.of_int dmy2.month2) in
              let y = Some (Int32.of_int dmy2.year2) in
              let delta = Some (Int32.of_int dmy2.delta2) in
              let dmy2 =
                {Mwrite.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`yearint, Some dmy2)
        in
        (prec, dmy1, dmy2)
      in
      {
        Mwrite.Date.cal = cal;
        prec = Some prec;
        dmy = Some dmy;
        dmy2 = dmy2;
        text = None;
      }
  | Dtext txt ->
      {
        Mwrite.Date.cal = None;
        prec = None;
        dmy = None;
        dmy2 = None;
        text = Some txt;
      }


(* ************************************************************************ *)
(*  [Fonc] date_of_piqi_date : piqi_date -> option def.date                 *)
(** [Description] : Converti date piqi en date
    [Args] :
      - date : date du module Mwrite
    [Retour] :
      - date : date
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let date_of_piqi_date conf date =
  match date.Mwrite.Date.text with
  | Some txt -> Some (Dtext txt)
  | _ ->
      (* Si on a une année, on a une date. *)
      match date.Mwrite.Date.dmy with
      | Some dmy ->
          begin
            match dmy.Mwrite.Dmy.year with
            | Some _ ->
                let cal =
                  match date.Mwrite.Date.cal with
                  | Some `julian -> Djulian
                  | Some `french -> Dfrench
                  | Some `hebrew -> Dhebrew
                  | _ -> Dgregorian
                in
                let get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal prec =
                  let day =
                    match dmy.Mwrite.Dmy.day with
                    | Some day -> Int32.to_int day
                    | None -> 0
                  in
                  let month =
                    match dmy.Mwrite.Dmy.month with
                    | Some month -> Int32.to_int month
                    | None -> 0
                  in
                  let year =
                    match dmy.Mwrite.Dmy.year with
                    | Some year -> Int32.to_int year
                    | None -> 0
                  in
                  let delta =
                    match dmy.Mwrite.Dmy.delta with
                    | Some delta -> Int32.to_int delta
                    | None -> 0
                  in
                  (* Error handling. *)
                  let (day, month, year) =
                    if year = 0 && month <= 0
                    then
                        (0, 0, year)
                    else
                        (day, month, year)
                  in
                  let adef_dmy =
                    {day = day; month = month; year = year; delta = delta; prec = prec}
                  in
                  let day_to_check =
                    adef_dmy.day >= 1 && adef_dmy.day <= 31
                  in
                  let month_to_check =
                    adef_dmy.month >= 1 && adef_dmy.month <= 13
                  in
                  (* Returns date directy if there is no month. *)
                  if adef_dmy.month = 0 then adef_dmy
                  (* If no specified day, checks the month value. *)
                  else if adef_dmy.day = 0 && month_to_check
                  then
                    (* Check the month in the gregorian calendar. *)
                    if cal = Dgregorian
                    then
                      begin
                        (* The day is set to 1 for checking. *)
                        Update.check_greg_day conf {day = 1; month = adef_dmy.month; year = adef_dmy.year; delta = delta; prec = prec};
                        adef_dmy
                      end
                    else
                      adef_dmy
                  (* Day and month are specified here. *)
                  else if day_to_check && month_to_check
                  then
                    (* Check the date in the gregorian calendar. *)
                    if cal = Dgregorian
                    then
                      begin
                        Update.check_greg_day conf {day = adef_dmy.day; month = adef_dmy.month; year = adef_dmy.year; delta = delta; prec = prec};
                        adef_dmy
                      end
                    else
                      adef_dmy
                  else
                    Update.bad_date conf adef_dmy
                in
                let delta2 = 0 in
                let prec =
                  match date.Mwrite.Date.prec with
                  | Some `about -> About
                  | Some `maybe -> Maybe
                  | Some `before -> Before
                  | Some `after -> After
                  | Some `oryear ->
                      (match date.Mwrite.Date.dmy2 with
                      | Some dmy ->
                          begin
                            match dmy.Mwrite.Dmy.year with
                            | Some _ ->
                              let adef_dmy = get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal Sure in
                              OrYear {day2 = adef_dmy.day; month2 = adef_dmy.month; year2 = adef_dmy.year; delta2 = delta2}
                            | None -> Sure
                          end
                      | None -> Sure (*OrYear {day2 = 0; month2 = 0; year2 = 0; delta2 = 0}*) (* erreur*))
                  | Some `yearint ->
                      (match date.Mwrite.Date.dmy2 with
                      | Some dmy ->
                          begin
                            match dmy.Mwrite.Dmy.year with
                            | Some _ ->
                              let adef_dmy = get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal Sure in
                              YearInt {day2 = adef_dmy.day; month2 = adef_dmy.month; year2 = adef_dmy.year; delta2 = delta2}
                            | None -> Sure
                          end
                      | None -> Sure (*YearInt {day2 = 0; month2 = 0; year2 = 0; delta2 = 0}*) (* erreur*))
                  | _ -> Sure
                in
                let dmy =
                  match date.Mwrite.Date.dmy with
                  | Some dmy ->
                      get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal prec
                  | None -> (* erreur*)
                      {day = 0; month = 0; year = 0; prec = Sure; delta = 0}
                in
                let dmy =
                  match cal with
                  | Dgregorian ->
                      let _check_date = Update.check_greg_day conf dmy in
                      dmy
                  | Djulian -> Calendar.gregorian_of_julian dmy
                  | Dfrench -> Calendar.gregorian_of_french dmy
                  | Dhebrew -> Calendar.gregorian_of_hebrew dmy
                in
                Some (Dgreg (dmy, cal))
          | None -> None
          end
      | None -> None


(**/**) (* Convertion d'une personne pour la lecture. *)


(* Copie de util.ml pour supprimer le html *)

let child_of_parent conf base p =
  (* Si le père a un nom de famille différent de la personne *)
  (* alors on l'affiche, sinon on n'affiche que le prénom.   *)
  let print_father fath =
    if not (eq_istr (get_surname p) (get_surname fath)) then
      person_text_no_html conf base fath
    else
      gen_person_text_no_html (p_first_name, (fun _ _ -> "")) conf base fath
  in
  let a = pget conf base (get_iper p) in
  let ifam =
    match get_parents a with
    | Some ifam ->
        let cpl = foi base ifam in
        let fath =
          let fath = pget conf base (get_father cpl) in
          if p_first_name base fath = "?" then None else Some fath
        in
        let moth =
          let moth = pget conf base (get_mother cpl) in
          if p_first_name base moth = "?" then None else Some moth
        in
        Some (fath, moth)
    | None -> None
  in
  match ifam with
  | Some (None, None) | None -> ""
  | Some (fath, moth) ->
      let s =
        match (fath, moth) with
        | (Some fath, None) -> print_father fath
        | (None, Some moth) -> person_text_no_html conf base moth
        | (Some fath, Some moth) ->
            print_father fath ^ " " ^ transl_nth conf "and" 0 ^ " " ^
              person_text_no_html conf base moth
        | _ -> ""
      in
      let is = index_of_sex (get_sex p) in
      translate_eval
        (transl_a_of_gr_eq_gen_lev conf
           (transl_nth conf "son/daughter/child" is) s s)

let husband_wife conf base p =
  let rec loop i =
    if i < Array.length (get_family p) then
      let fam = foi base (get_family p).(i) in
      let conjoint = Gutil.spouse (get_iper p) fam in
      let conjoint = pget conf base conjoint in
      if p_first_name base conjoint <> "?" || p_surname base conjoint <> "?"
      then
        let relation =
          Printf.sprintf (relation_txt conf (get_sex p) fam) (fun () -> "")
        in
        translate_eval
          (relation ^ " " ^ (person_text_no_html conf base conjoint))
      else loop (i + 1)
    else ""
  in
  loop 0


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_simple_person :
             config -> base -> person -> SimplePerson                         *)
(** [Description] : Retourne à partir d'une person (gwdb) une SimplePerson
                    (piqi).
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_simple_person conf base p =
  let index = Int32.of_string @@ Gwdb.string_of_iper (get_iper p) in
  let sex =
    match get_sex p with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let sosa =
    let sosa_nb = Perso.get_single_sosa conf base p in
    if Sosa.eq sosa_nb Sosa.zero then `no_sosa
    else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
    else `sosa
  in
  let (first_name, surname) =
    Api_saisie_read.person_firstname_surname_txt base p
  in
  let (birth_short, birth_place, death_short, death_place) =
    let (birth, death, _) = Date.get_birth_death_date p in
    let birth =
      match birth with
      | Some d -> DateDisplay.string_slash_of_date conf d
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
      match death with
      | Some d -> DateDisplay.string_slash_of_date conf d
      | None -> ""
    in
    let death_place =
      let death_place = sou base (get_death_place p) in
      if death_place <> "" then Util.string_of_place conf death_place
      else
        let burial_place = sou base (get_burial_place p) in
        Util.string_of_place conf burial_place
    in
    (birth, birth_place, death, death_place)
  in
  let image =
    let img = sou base (get_image p) in
    if img <> "" then img
    else ""
      (* On veut pas vraiment vérifier les images fichiers ...
      match Api_util.find_image_file conf base p with
      | Some s -> "1"
      | None -> ""
      *)
  in
  {
    Mwrite.Simple_person.index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    birth_short_date = if birth_short = "" then None else Some birth_short;
    birth_place = if birth_place = "" then None else Some birth_place;
    death_short_date = if death_short = "" then None else Some death_short;
    death_place = if death_place = "" then None else Some death_place;
    image = if image = "" then None else Some image;
    sosa = sosa;
  }


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_search :
             config -> base -> person -> PersonSearchLink                     *)
(** [Description] : Retourne une personne qui sert lors de la recherche pour
                    relier un individu dans la saisie.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
    [Retour] : PersonSearchLink
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_search conf base p =
  let index = Int32.of_string @@ Gwdb.string_of_iper (get_iper p) in
  let sex =
    match get_sex p with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let sosa =
    let sosa_nb = Perso.get_sosa_person p in
    if Sosa.eq sosa_nb Sosa.zero then `no_sosa
    else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
    else `sosa
  in
  let (first_name, surname) =
    Api_saisie_read.person_firstname_surname_txt base p
  in
  let dates = Api_saisie_read.short_dates_text conf base p in
  let image =
    let img = sou base (get_image p) in
    if img <> "" then img
    else ""
      (* On veut pas vraiment vérifier les images fichiers ...
      match Api_util.find_image_file conf base p with
      | Some s -> "1"
      | None -> ""
      *)
  in
  let family =
    let hw = husband_wife conf base p in
    if hw <> "" then hw
    else child_of_parent conf base p
  in
  {
    Mwrite.Person_search.index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    dates = if dates = "" then None else Some dates;
    image = if image = "" then None else Some image;
    sosa = sosa;
    family = family;
  }


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_search_info :
             config -> base -> person -> PersonSearchInfo                     *)
(** [Description] : Retourne une personne qui sert lors de la recherche pour
                    relier un individu dans la saisie (affichage des
                    informations détaillées).
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
    [Retour] : PersonSearchInfo
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_search_info conf base p =
  let index = Int32.of_string @@ Gwdb.string_of_iper (get_iper p) in
  let sex =
    match get_sex p with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let sosa =
    let sosa_nb = Perso.get_single_sosa conf base p in
    if Sosa.eq sosa_nb Sosa.zero then `no_sosa
    else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
    else `sosa
  in
  let surname = sou base (get_surname p) in
  let first_name = sou base (get_first_name p) in
  let publicname = sou base (get_public_name p) in
  let aliases = List.map (sou base) (get_aliases p) in
  let qualifiers = List.map (sou base) (get_qualifiers p) in
  let firstname_aliases = List.map (sou base) (get_first_names_aliases p) in
  let surname_aliases = List.map (sou base) (get_surnames_aliases p) in
  let image =
    let img = sou base (get_image p) in
    if img <> "" then img
    else ""
      (* On veut pas vraiment vérifier les images fichiers ...
      match Api_util.find_image_file conf base p with
      | Some s -> "1"
      | None -> ""
      *)
  in
  let occupation =
    let s = sou base (get_occupation p) in
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
  in
  let events =
    List.map
      (fun (name, date, place, note, src, w, isp) ->
        let name =
          match name with
          | Perso.Pevent name -> Util.string_of_pevent_name conf base name
          | Perso.Fevent name -> Util.string_of_fevent_name conf base name
        in
        let (date, _, date_conv, _, date_cal) =
          match Adef.od_of_cdate date with
          | Some d -> Api_saisie_read.string_of_date_and_conv conf d
          | _ -> ("", "", "", "", None)
        in
        let place = Util.string_of_place conf (sou base place) in
        let note =
          let env = [('i', fun () -> Util.default_image_name base p)] in
          let s = sou base note in
          let s = string_with_macros conf env s in
          let lines = Wiki.html_of_tlsw conf s in
          let wi =
            {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
             Wiki.wi_file_path = Notes.file_path conf base;
             Wiki.wi_person_exists = person_exists conf base;
             Wiki.wi_always_show_link = conf.wizard || conf.friend}
          in
          let s = Wiki.syntax_links conf wi (String.concat "\n" lines) in
          if conf.pure_xhtml then Util.check_xhtml s else s
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
          match isp with
          | Some ip ->
              let sp = poi base ip in
              Some (pers_to_piqi_simple_person conf base sp)
          | None -> None
        in
        let witnesses =
          Mutil.array_to_list_map
            (fun (ip, wk) ->
               let witness_type =
                 match wk with
                 | Witness -> `witness
                 | Witness_GodParent -> `witness_godparent
               in
               let witness = pers_to_piqi_simple_person conf base @@ poi base ip in
               Mwrite.Witness_event.{ witness_type ; witness })
            w
        in
        {
          Mwrite.Event.name = name;
          date = if date = "" then None else Some date;
          date_conv = if date_conv = "" then None else Some date_conv;
          date_cal = date_cal;
          place = if place = "" then None else Some place;
          reason = None;
          note = if note = "" then None else Some note;
          src = if src= "" then None else Some src;
          spouse = spouse;
          witnesses = witnesses;
        })
      (Perso.events_list conf base p)
  in
  let notes =
    let env = [('i', fun () -> Util.default_image_name base p)] in
    let s = sou base (get_notes p) in
    let s = string_with_macros conf env s in
    let lines = Wiki.html_of_tlsw conf s in
    let wi =
      {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_file_path = Notes.file_path conf base;
       Wiki.wi_person_exists = person_exists conf base;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    let s = Wiki.syntax_links conf wi (String.concat "\n" lines) in
    if conf.pure_xhtml then Util.check_xhtml s else s
  in
  let psources =
    let s = sou base (get_psources p) in
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
  let has_sources = psources <> "" in
  let titles = Perso.nobility_titles_list conf base p in
  let titles =
    let tmp_conf = {(conf) with cancel_links = true} in
    List.map (Perso.string_of_title tmp_conf base "" p) titles
  in
  let related =
    let list =
      let list = List.sort_uniq compare (get_related p) in
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
           |(Some d1, Some d2) -> Date.compare_date d1 d2
           | _ -> -1 )
      (List.rev list)
    in
    List.map
      (fun (p, rp) ->
        let p = pers_to_piqi_simple_person conf base p in
        let r_type =
          match rp.r_type with
          | Adoption -> `rchild_adoption
          | Recognition -> `rchild_recognition
          | CandidateParent -> `rchild_candidate_parent
          | GodParent -> `rchild_god_parent
          | FosterParent -> `rchild_foster_parent
        in
        {
          Mwrite.Relation_person.r_type = r_type;
          person = p;
        } )
      list
  in
  let rparents =
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
        let rl =
          match rp.r_fath with
          | Some ip ->
              let p = poi base ip in
              let p = pers_to_piqi_simple_person conf base p in
              let p =
                {
                  Mwrite.Relation_person.r_type = r_type;
                  person = p;
                }
              in
              p :: rl
          | None -> rl
        in
        match rp.r_moth with
        | Some ip ->
          let p = poi base ip in
          let p = pers_to_piqi_simple_person conf base p in
          let p =
            {
              Mwrite.Relation_person.r_type = r_type;
              person = p;
            }
          in
          p :: rl
        | None -> rl)
      [] (get_rparents p)
  in
  let was_witness =
    let list =
      let list = ref [] in
      let related = List.sort_uniq compare (get_related p) in
      let rec make_list =
        function
        | ic :: icl ->
            let c = pget conf base ic in
            if get_sex c = Male then
              Array.iter
                (fun ifam ->
                   let fam = foi base ifam in
                   if Array.mem (get_iper p) (get_witnesses fam)
                   then
                     list := (ifam, fam) :: !list
                   else ())
                (get_family (pget conf base ic))
            else ();
            make_list icl
        | [] -> ()
      in
      make_list related;
      !list
    in
    let list =
      List.sort
        (fun (_, fam1) (_, fam2) ->
           match
             (Adef.od_of_cdate (get_marriage fam1),
              Adef.od_of_cdate (get_marriage fam2))
           with
           | (Some d1, Some d2) -> Date.compare_date d1 d2
           | _ -> 0 )
        list
    in
    List.map
      (fun (_, fam) ->
         let ifath = get_father fam in
         let imoth = get_mother fam in
         let father = poi base ifath in
         let mother = poi base imoth in
         let father_auth = authorized_age conf base father in
         let husband =
           if not father_auth && (is_hide_names conf father) then "x x"
           else p_first_name base father ^ " " ^ p_surname base father
         in
         let mother_auth = authorized_age conf base mother in
         let wife =
           if not mother_auth && (is_hide_names conf mother) then "x x"
           else p_first_name base mother ^ " " ^ p_surname base mother
         in
         (*
         let husband = pers_to_piqi_simple_person conf base father in
         let wife = pers_to_piqi_simple_person conf base mother in
         *)
         Mwrite.Was_witness.({
           husband = husband;
           wife = wife;
         }) )
      list
  in
  {
    Mwrite.Person_search_info.index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    public_name = if publicname = "" then None else Some publicname;
    aliases = aliases;
    qualifiers = qualifiers;
    firstname_aliases = firstname_aliases;
    surname_aliases = surname_aliases;
    image = if image = "" then None else Some image;
    events = events;
    occupation = if occupation = "" then None else Some occupation;
    notes = if notes = "" then None else Some notes;
    psources = if psources = "" then None else Some psources;
    has_sources = has_sources;
    titles = titles;
    related = related;
    rparents = rparents;
    was_witness = was_witness;
    sosa = sosa;
  }


(**/**) (* Convertion d'une personne, d'une famille. *)


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_link :
             config -> base -> person -> PersonSearchLink                     *)
(** [Description] : Retourne une personne qui sert lors de la recherche pour
                    relier un individu dans la saisie.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
    [Retour] : PersonSearchLink
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_link conf base p =
  let create_link = `link in
  let index = Int32.of_string @@ Gwdb.string_of_iper (get_iper p) in
  let sex =
    match get_sex p with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let first_name = sou base (get_first_name p) in
  let surname = sou base (get_surname p) in
  let occ = get_occ p in
  let occ = if occ = 0 then None else Some (Int32.of_int occ) in
  let dates = Api_saisie_read.short_dates_text conf base p in
  let dates =
    if dates = "" then None
    else Some ("(" ^ dates ^ ")")
  in
  {
    Mwrite.Person_link.create_link = create_link;
    index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    occ = occ;
    dates = dates;
  }


(* ************************************************************************* *)
(*  [Fonc] pers_to_piqi_mod_person : config -> base -> person -> piqi person *)
(** [Description] : Converti une personne en personne piqi.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] :
      - piqi person : person du module Mwrite.
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let pers_to_piqi_mod_person conf base p =
  let digest = Update.digest_person (UpdateInd.string_person_of base p) in
  let create_link = `link in
  let index = Int32.of_string @@ Gwdb.string_of_iper (get_iper p) in
  let sex =
    match get_sex p with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let surname = sou base (get_surname p) in
  let first_name = sou base (get_first_name p) in
  let occ = get_occ p in
  let occ =
    (* Cas particulier pour les personnes sans clé, et principalement *)
    (* ? ?. On ne renvoie pas le occ, comme ça si la personne existe  *)
    (* dans la base, on aura le droit à un conflit de nom.            *)
    if Name.lower surname = "" || Name.lower first_name = "" then None
    else
      if occ = 0 then None
      else Some (Int32.of_int occ)
  in
  let publicname = sou base (get_public_name p) in
  let aliases = List.map (sou base) (get_aliases p) in
  let qualifiers = List.map (sou base) (get_qualifiers p) in
  let firstname_aliases = List.map (sou base) (get_first_names_aliases p) in
  let surname_aliases = List.map (sou base) (get_surnames_aliases p) in
  let image = sou base (get_image p) in
  let death_type =
    match get_death p with
    | NotDead -> `not_dead
    | Death _ | DeadDontKnowWhen -> `dead
    | DeadYoung -> `dead_young
    | DontKnowIfDead -> `dont_know_if_dead
    | OfCourseDead -> `of_course_dead
  in
  let occupation = sou base (get_occupation p) in
  let psources = sou base (get_psources p) in
  let notes = sou base (get_notes p) in
  let titles =
    List.map
      (fun t ->
        (* On ne prend pas en compte le type du titre (main/name/none). *)
        let name =
          match t.t_name with
          | Tmain -> ""
          | Tname name -> sou base name
          | Tnone -> ""
        in
        let title = sou base t.t_ident in
        let fief = sou base t.t_place in
        let date_begin =
          match Adef.od_of_cdate t.t_date_start with
          | Some d -> Some (piqi_date_of_date d)
          | None -> None
        in
        let date_end =
          match Adef.od_of_cdate t.t_date_end with
          | Some d -> Some (piqi_date_of_date d)
          | None -> None
        in
        let nth = Some (Int32.of_int t.t_nth) in
        Mwrite.Title.({
          name = if name = "" then None else Some name;
          title = if title = "" then None else Some title;
          fief = if fief = "" then None else Some fief;
          date_begin = date_begin;
          date_end = date_end;
          nth = nth;
        }))
      (get_titles p)
  in
  let pevents =
    List.map
      (fun evt ->
         let (pevent_type, event_perso) =
           match evt.epers_name with
           | Epers_Birth -> (Some `epers_birth, None)
           | Epers_Baptism -> (Some `epers_baptism, None)
           | Epers_Death -> (Some `epers_death, None)
           | Epers_Burial -> (Some `epers_burial, None)
           | Epers_Cremation -> (Some `epers_cremation, None)
           | Epers_Accomplishment -> (Some `epers_accomplishment, None)
           | Epers_Acquisition -> (Some `epers_acquisition, None)
           | Epers_Adhesion -> (Some `epers_adhesion, None)
           | Epers_BaptismLDS -> (Some `epers_baptismlds, None)
           | Epers_BarMitzvah -> (Some `epers_barmitzvah, None)
           | Epers_BatMitzvah -> (Some `epers_batmitzvah, None)
           | Epers_Benediction -> (Some `epers_benediction, None)
           | Epers_ChangeName -> (Some `epers_changename, None)
           | Epers_Circumcision -> (Some `epers_circumcision, None)
           | Epers_Confirmation -> (Some `epers_confirmation, None)
           | Epers_ConfirmationLDS -> (Some `epers_confirmationlds, None)
           | Epers_Decoration -> (Some `epers_decoration, None)
           | Epers_DemobilisationMilitaire -> (Some `epers_demobilisationmilitaire, None)
           | Epers_Diploma -> (Some `epers_diploma, None)
           | Epers_Distinction -> (Some `epers_distinction, None)
           | Epers_Dotation -> (Some `epers_dotation, None)
           | Epers_DotationLDS -> (Some `epers_dotationlds, None)
           | Epers_Education -> (Some `epers_education, None)
           | Epers_Election -> (Some `epers_election, None)
           | Epers_Emigration -> (Some `epers_emigration, None)
           | Epers_Excommunication -> (Some `epers_excommunication, None)
           | Epers_FamilyLinkLDS -> (Some `epers_familylinklds, None)
           | Epers_FirstCommunion -> (Some `epers_firstcommunion, None)
           | Epers_Funeral -> (Some `epers_funeral, None)
           | Epers_Graduate -> (Some `epers_graduate, None)
           | Epers_Hospitalisation -> (Some `epers_hospitalisation, None)
           | Epers_Illness -> (Some `epers_illness, None)
           | Epers_Immigration -> (Some `epers_immigration, None)
           | Epers_ListePassenger -> (Some `epers_listepassenger, None)
           | Epers_MilitaryDistinction -> (Some `epers_militarydistinction, None)
           | Epers_MilitaryPromotion -> (Some `epers_militarypromotion, None)
           | Epers_MilitaryService -> (Some `epers_militaryservice, None)
           | Epers_MobilisationMilitaire -> (Some `epers_mobilisationmilitaire, None)
           | Epers_Naturalisation -> (Some `epers_naturalisation, None)
           | Epers_Occupation -> (Some `epers_occupation, None)
           | Epers_Ordination -> (Some `epers_ordination, None)
           | Epers_Property -> (Some `epers_property, None)
           | Epers_Recensement -> (Some `epers_recensement, None)
           | Epers_Residence-> (Some `epers_residence, None)
           | Epers_Retired -> (Some `epers_retired, None)
           | Epers_ScellentChildLDS -> (Some `epers_scellentchildlds, None)
           | Epers_ScellentParentLDS -> (Some `epers_scellentparentlds, None)
           | Epers_ScellentSpouseLDS -> (Some `epers_scellentspouselds, None)
           | Epers_VenteBien -> (Some `epers_ventebien, None)
           | Epers_Will -> (Some `epers_will, None)
           | Epers_Name n -> (None, Some (sou base n))
         in
         let date =
           match Adef.od_of_cdate evt.epers_date with
           | Some d -> Some (piqi_date_of_date d)
           | _ -> None
         in
         let place = sou base evt.epers_place in
         let reason = None in
         let note = sou base evt.epers_note in
         let src = sou base evt.epers_src in
         let witnesses =
           Mutil.array_to_list_map
             (fun (ip, wk) ->
                let witness_type =
                  match wk with
                  | Witness -> `witness
                  | Witness_GodParent -> `witness_godparent
                in
                let p = poi base ip in
                let person_link = pers_to_piqi_person_link conf base p in
                Mwrite.Witness.{ witness_type ; person = Some person_link })
             evt.epers_witnesses
         in
         {
           Mwrite.Pevent.pevent_type = pevent_type;
           date = date;
           place = if place = "" then None else Some place;
           reason = reason;
           note = if note = "" then None else Some note;
           src = if src = "" then None else Some src;
           witnesses = witnesses;
           event_perso = event_perso;
         })
      (get_pevents p)
  in
  (* Si la personne n'a aucun évènement et/ou est décédée mais *)
  (* sans évènement, on ajoute les évènements nécessaires.     *)
  let pevents =
    if pevents = [] then
      begin
        let birth =
          {
            Mwrite.Pevent.pevent_type = Some `epers_birth;
            date = None;
            place = None;
            reason = None;
            note = None;
            src = None;
            witnesses = [];
            event_perso = None;
          }
        in
        (* Que pour les personnes qui existent. *)
        if get_iper p <> dummy_iper && death_type != `not_dead then
          let death =
            {
              Mwrite.Pevent.pevent_type = Some `epers_death;
              date = None;
              place = None;
              reason = None;
              note = None;
              src = None;
              witnesses = [];
              event_perso = None;
            }
          in
          [birth; death]
        else [birth]
      end
    else
      let (has_birth, has_death) =
        List.fold_left
          (fun (has_birth, has_death) evt ->
            (has_birth || evt.epers_name = Epers_Birth,
             has_death || evt.epers_name = Epers_Death))
          (false, false) (get_pevents p)
      in
      let pevents =
        if has_birth then pevents
        else
          begin
            let birth =
              {
                Mwrite.Pevent.pevent_type = Some `epers_birth;
                date = None;
                place = None;
                reason = None;
                note = None;
                src = None;
                witnesses = [];
                event_perso = None;
              }
            in
            birth :: pevents
          end
      in
      if has_death || death_type = `not_dead  then pevents
      else
        begin
          let death =
            {
              Mwrite.Pevent.pevent_type = Some `epers_death;
              date = None;
              place = None;
              reason = None;
              note = None;
              src = None;
              witnesses = [];
              event_perso = None;
            }
          in
          pevents @ [death]
        end;
  in
  let related = List.map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (get_related p) in
  let rparents =
    List.fold_right
      (fun rp accu ->
        let source = sou base rp.r_sources in
        let accu =
          match rp.r_fath with
          | Some ip ->
              let p = poi base ip in
              let father = pers_to_piqi_person_link conf base p in
              let rpt_type =
                match rp.r_type with
                | Adoption -> `rpt_adoption_father
                | Recognition -> `rpt_recognition_father
                | CandidateParent -> `rpt_candidate_parent_father
                | GodParent -> `rpt_god_parent_father
                | FosterParent -> `rpt_foster_parent_father
              in
              let r =
                Mwrite.Relation_parent.({
                  rpt_type = rpt_type;
                  person = Some father;
                  source = if source = "" then None else Some source;
                })
              in
              r :: accu
          | None -> accu
        in
        match rp.r_moth with
        | Some ip ->
          let p = poi base ip in
          let mother = pers_to_piqi_person_link conf base p in
          let rpt_type =
            match rp.r_type with
            | Adoption -> `rpt_adoption_mother
            | Recognition -> `rpt_recognition_mother
            | CandidateParent -> `rpt_candidate_parent_mother
            | GodParent -> `rpt_god_parent_mother
            | FosterParent -> `rpt_foster_parent_mother
          in
          let r =
            Mwrite.Relation_parent.({
                rpt_type = rpt_type;
                person = Some mother;
                source = if source = "" then None else Some source;
              })
          in
          r :: accu
        | None -> accu)
      (get_rparents p) []
  in
  let access =
    match get_access p with
    | IfTitles -> `access_iftitles
    | Public -> `access_public
    | Private -> `access_private
  in
  let parents =
    match get_parents p with
    | Some ifam -> Some (Int32.of_string (Gwdb.string_of_ifam ifam))
    | None -> None
  in
  let families =
    Mutil.array_to_list_map (fun x -> Int32.of_string @@ Gwdb.string_of_ifam x) (get_family p)
  in
  {
    Mwrite.Person.digest = digest;
    index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    occ = occ;
    public_name = if publicname = "" then None else Some publicname;
    aliases = aliases;
    qualifiers = qualifiers;
    firstname_aliases = firstname_aliases;
    surname_aliases = surname_aliases;
    image = if image = "" then None else Some image;
    death_type = death_type;
    occupation = if occupation = "" then None else Some occupation;
    psources = if psources = "" then None else Some psources;
    notes = if notes = "" then None else Some notes;
    titles = titles;
    pevents = pevents;
    related = related;
    rparents = rparents;
    access = access;
    parents = parents;
    families = families;
    create_link = create_link;
  }

(* ************************************************************************ *)
(*  [Fonc] fam_to_piqi_mod_family :
             config -> base -> ip -> family -> piqi family                  *)
(** [Description] : Converti une personne en personne piqi.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] :
      - piqi person : person du module Mwrite.
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let fam_to_piqi_mod_family conf base ifam fam =
  let digest = "" in
  let index = Int32.of_string (Gwdb.string_of_ifam ifam) in
  let fevents =
    List.map
      (fun evt ->
         let (fevent_type, event_perso) =
           match evt.efam_name with
           | Efam_Marriage -> (Some `efam_marriage, None)
           | Efam_NoMarriage -> (Some `efam_no_marriage, None)
           | Efam_NoMention -> (Some `efam_no_mention, None)
           | Efam_Engage -> (Some `efam_engage, None)
           | Efam_Divorce -> (Some `efam_divorce, None)
           | Efam_Separated -> (Some `efam_separated, None)
           | Efam_Annulation -> (Some `efam_annulation, None)
           | Efam_MarriageBann -> (Some `efam_marriage_bann, None)
           | Efam_MarriageContract -> (Some `efam_marriage_contract, None)
           | Efam_MarriageLicense -> (Some `efam_marriage_license, None)
           | Efam_PACS -> (Some `efam_pacs, None)
           | Efam_Residence -> (Some `efam_residence, None)
           | Efam_Name n -> (None, Some (sou base n))
         in
         let date =
           match Adef.od_of_cdate evt.efam_date with
           | Some d -> Some (piqi_date_of_date d)
           | _ -> None
         in
         let place = sou base evt.efam_place in
         let reason = None in
         let note = sou base evt.efam_note in
         let src = sou base evt.efam_src in
         let witnesses =
           Mutil.array_to_list_map
             (fun (ip, wk) ->
                let witness_type =
                  match wk with
                  | Witness -> `witness
                  | Witness_GodParent -> `witness_godparent
                in
                let p = poi base ip in
                let person_link = pers_to_piqi_person_link conf base p in
                Mwrite.Witness.{ witness_type; person = Some person_link })
             evt.efam_witnesses
         in
         {
           Mwrite.Fevent.fevent_type = fevent_type;
           date = date;
           place = if place = "" then None else Some place;
           reason = reason;
           note = if note = "" then None else Some note;
           src = if src = "" then None else Some src;
           witnesses = witnesses;
           event_perso = event_perso;
         })
      (get_fevents fam)
  in
  let fsources = sou base (get_fsources fam) in
  let origin_file = sou base (get_origin_file fam) in
  let comment = sou base (get_comment fam) in
  let father = poi base (get_father fam) in
  let father = pers_to_piqi_mod_person conf base father in
  let mother = poi base (get_mother fam) in
  let mother = pers_to_piqi_mod_person conf base mother in
  let children =
    Mutil.array_to_list_map
      (fun ip -> pers_to_piqi_person_link conf base @@ poi base ip)
      (get_children fam)
  in
  (* Compatibilité avec GeneWeb. *)
  let old_witnesses =
    Mutil.array_to_list_map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (get_witnesses fam)
  in
  {
    Mwrite.Family.digest = digest;
    index = index;
    fevents = fevents;
    fsources = if fsources = "" then None else Some fsources;
    comment = if comment = "" then None else Some comment;
    origin_file = if origin_file = "" then None else Some origin_file;
    father = father;
    mother = mother;
    children = children;
    old_witnesses = old_witnesses;
  }


(* ************************************************************************** *)
(*  [Fonc] piqi_mod_person_of_person_start :
             config -> base -> Person_start -> Person                         *)
(** [Description] : Converti une personne start pour la première saisie en
                    Person afin de suivre le chemin classique de modification
                    de la base.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - start_    : Person_start
    [Retour] : Person
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let piqi_mod_person_of_person_start conf base start_p =
  let p = Gwdb.empty_person base (Gwdb.dummy_iper) in
  let mod_p = pers_to_piqi_mod_person conf base p in
  (* Les index négatifs ne marchent pas. *)
  mod_p.Mwrite.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  mod_p.Mwrite.Person.lastname <- start_p.M.Person_start.lastname;
  mod_p.Mwrite.Person.firstname <- start_p.M.Person_start.firstname;
  mod_p.Mwrite.Person.sex <- start_p.M.Person_start.sex;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  mod_p.Mwrite.Person.access <- `access_iftitles;
  let birth_date =
    match start_p.M.Person_start.birth_date_year with
    | Some y ->
        let y = Int32.to_int y in
        if y > 0 then
          (match start_p.M.Person_start.birth_date_month with
           | Some m ->
               let m = Int32.to_int m in
               (match start_p.M.Person_start.birth_date_day with
               | Some d ->
                   let d = Int32.to_int d in
                   let dmy =
                     {day = d; month = m; year = y; prec = Sure; delta = 0}
                   in
                   Some (Dgreg (dmy, Dgregorian))
               | None ->
                   let dmy =
                     {day = 0; month = m; year = y; prec = Sure; delta = 0}
                   in
                   Some (Dgreg (dmy, Dgregorian)))
           | None ->
               let dmy =
                 {day = 0; month = 0; year = y; prec = Sure; delta = 0}
               in
               Some (Dgreg (dmy, Dgregorian)))
        else
          None
    | None -> None
  in
  let birth_date =
    match birth_date with
    | Some d -> Some (piqi_date_of_date d)
    | _ -> None
  in
  let birth =
    {
      Mwrite.Pevent.pevent_type = Some `epers_birth;
      date = birth_date;
      place = None;
      reason = None;
      note = None;
      src = None;
      witnesses = [];
      event_perso = None;
    }
  in
  mod_p.Mwrite.Person.pevents <- [birth];
  mod_p


(**/**) (* Famille vide. *)


let piqi_empty_family conf base ifam =
  let father = Gwdb.empty_person base (Gwdb.dummy_iper) in
  let mother = Gwdb.empty_person base (Gwdb.dummy_iper) in
  let father = pers_to_piqi_mod_person conf base father in
  let mother = pers_to_piqi_mod_person conf base mother in
  (* Les index négatifs ne marchent pas ! *)
  father.Mwrite.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  mother.Mwrite.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  father.Mwrite.Person.access <- `access_iftitles;
  mother.Mwrite.Person.access <- `access_iftitles;
  let fevents =
    let evt =
      {
        Mwrite.Fevent.fevent_type = Some `efam_marriage;
        date = None;
        place = None;
        reason = None;
        note = None;
        src = None;
        witnesses = [];
        event_perso = None;
      }
    in
    [evt]
  in
  {
    Mwrite.Family.digest = "";
    index = Int32.of_string (Gwdb.string_of_ifam ifam);
    fevents = fevents;
    fsources = None;
    comment = None;
    origin_file = None;
    father = father;
    mother = mother;
    children = [];
    old_witnesses = [];
  }

(* List of strings in which some characters were removed. *)
let removed_string = ref [] ;;

let reconstitute_somebody base person =
  let create_link = person.Mwrite.Person_link.create_link in
  let (fn, sn, occ, create, var, force_create) = match create_link with
    | `link ->
      let ip = Gwdb.iper_of_string @@ Int32.to_string person.Mwrite.Person_link.index in
      let p = poi base ip in
      let fn = sou base (get_first_name p) in
      let sn = sou base (get_surname p) in
      let occ = get_occ p in
      (fn, sn, occ, Update.Link, "", false)
    | _ ->
      let sex =
        match person.Mwrite.Person_link.sex with
          | `male -> Male
          | `female -> Female
          | `unknown -> Neuter
      in
      let fn = person.Mwrite.Person_link.firstname in
      let sn = person.Mwrite.Person_link.lastname in
      let (occ, force_create) = match create_link with
        | `create_default_occ ->
          (match person.Mwrite.Person_link.occ with
            | Some occ -> (Int32.to_int occ, false)
            | None -> (0, false))
        | `create ->
          let occ = api_find_free_occ base fn sn in
          (* Update the person because if we want to find it, we have to know its occ. *)
          let () =
            if occ = 0 then person.Mwrite.Person_link.occ <- None
            else person.Mwrite.Person_link.occ <- Some (Int32.of_int occ)
          in
          (occ, true)
        | _ -> (0, false) (* Should not happen. *)
      in
      (fn, sn, occ, Update.Create (sex, None), "", force_create)
  in
  let (fn, sn) =
    (* If there are forbidden characters, delete them. *)
    let contain_fn = String.contains fn in
    let contain_sn = String.contains sn in
    if (List.exists contain_fn Name.forbidden_char)
      || (List.exists contain_sn Name.forbidden_char) then
      begin
        removed_string :=
          (Name.purge fn ^ " " ^ Name.purge sn) :: !removed_string;
        (Name.purge fn, Name.purge sn)
      end
    else (fn, sn)
  in
  (fn, sn, occ, create, var, force_create)

#endif
