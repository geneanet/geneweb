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


(**/**) (* Fonctions pour l'auto-completion. *)


(* ************************************************************************ *)
(*  [Fonc] print_auto_complete : config -> base -> AutoCompleteResult       *)
(** [Description] : Renvoie la liste unique d'un champ. Par exemple la liste
                    de nom de famille en fonction de ce qui est tapé.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - result : la liste de la recherche.
                                                                           *)
(* ************************************************************************ *)
let print_auto_complete conf base =
  let params = get_params conf Mext_write.parse_auto_complete in
  let s = params.Mwrite.Auto_complete.input in
  let max_res = Int32.to_int params.Mwrite.Auto_complete.limit in
  let mode = params.Mwrite.Auto_complete.field in
  let place_mode = params.Mwrite.Auto_complete.place_field in
  let list =
    if nb_of_persons base > 100000 then
      Api_saisie_autocomplete.get_list_from_cache conf base s max_res mode
    else
      Api_search.search_auto_complete conf base mode place_mode max_res s
  in
  let result =
    Mwrite.Auto_complete_result.({
      result = list;
    })
  in
  let data = Mext_write.gen_auto_complete_result result in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] print_person_search_list : config -> base -> SearchPerson        *)
(** [Description] : Renvoie la liste des personnes qui ont ce nom ou prénom.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - result : la liste de la recherche.
                                                                           *)
(* ************************************************************************ *)
let print_person_search_list conf base =
  let params = get_params conf Mext_write.parse_person_search_list_params in
  let surname = params.Mwrite.Person_search_list_params.lastname in
  let first_name = params.Mwrite.Person_search_list_params.firstname in
  let max_res = Int32.to_int params.Mwrite.Person_search_list_params.limit in
  let list =
    Api_search.search_person_list base surname first_name
  in
  let list =
    List.sort
      (fun ip1 ip2 ->
        let p1 = poi base ip1 in
        let p2 = poi base ip2 in
        let fn1 = sou base (get_first_name p1) in
        let sn1 = sou base (get_surname p1) in
        let fn2 = sou base (get_first_name p2) in
        let sn2 = sou base (get_surname p2) in
        let cmp_sn = Gutil.alphabetic_order sn1 sn2 in
        if cmp_sn = 0 then
          let cmp_fn = Gutil.alphabetic_order fn1 fn2 in
          if cmp_fn = 0 then
            (match
              (Adef.od_of_cdate (get_birth p1),
               Adef.od_of_cdate (get_birth p2))
             with
             | (Some d1, Some d2) ->
                 if CheckItem.strictly_before d1 d2 then -1
                 else 1
             | (Some _, _) -> -1
             | (_, Some _) -> 1
             | (_, _) -> 0)
          else cmp_fn
        else cmp_sn)
      list
  in
  (* On préfère limiter la liste ici, même si on perd un peu en performance. *)
  let list = Util.reduce_list max_res list in
  let () = Perso.build_sosa_ht conf base in
  let list =
    List.map
      (fun ip ->
        let p = poi base ip in
        Api_update_util.pers_to_piqi_person_search conf base p)
      list
  in
  let result = Mwrite.Person_search_list.({ persons = list; }) in
  let data = Mext_write.gen_person_search_list result in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] print_person_search_info : config -> base -> PersonSearchInfo    *)
(** [Description] : Affiche les informations telles que sur le panneau
                    droit dans l'arbre.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : PersonSearchInfo
                                                                           *)
(* ************************************************************************ *)
let print_person_search_info conf base =
  let params = get_params conf Mext_write.parse_index_person in
  let ip = Adef.iper_of_int (Int32.to_int params.Mwrite.Index_person.index) in
  let p = poi base ip in
  let pers = Api_update_util.pers_to_piqi_person_search_info conf base p in
  let data = Mext_write.gen_person_search_info pers in
  print_result conf data


(**/**) (* Configuration pour la saisie. *)

let piqi_event_of_fevent evt_name =
  match evt_name with
  | Efam_Marriage -> `efam_marriage
  | Efam_NoMarriage -> `efam_no_marriage
  | Efam_NoMention -> `efam_no_mention
  | Efam_Engage -> `efam_engage
  | Efam_Divorce -> `efam_divorce
  | Efam_Separated -> `efam_separated
  | Efam_Annulation -> `efam_annulation
  | Efam_MarriageBann -> `efam_marriage_bann
  | Efam_MarriageContract -> `efam_marriage_contract
  | Efam_MarriageLicense -> `efam_marriage_license
  | Efam_PACS -> `efam_pacs
  | Efam_Residence -> `efam_residence
  | _ -> failwith "print_config"

let piqi_event_of_pevent evt_name =
  match evt_name with
  | Epers_Birth -> `epers_birth
  | Epers_Baptism -> `epers_baptism
  | Epers_Death -> `epers_death
  | Epers_Burial -> `epers_burial
  | Epers_Cremation -> `epers_cremation
  | Epers_Accomplishment -> `epers_accomplishment
  | Epers_Acquisition -> `epers_acquisition
  | Epers_Adhesion -> `epers_adhesion
  | Epers_BaptismLDS -> `epers_baptismlds
  | Epers_BarMitzvah -> `epers_barmitzvah
  | Epers_BatMitzvah -> `epers_batmitzvah
  | Epers_Benediction -> `epers_benediction
  | Epers_ChangeName -> `epers_changename
  | Epers_Circumcision-> `epers_circumcision
  | Epers_Confirmation -> `epers_confirmation
  | Epers_ConfirmationLDS -> `epers_confirmationlds
  | Epers_Decoration -> `epers_decoration
  | Epers_DemobilisationMilitaire -> `epers_demobilisationmilitaire
  | Epers_Diploma -> `epers_diploma
  | Epers_Distinction -> `epers_distinction
  | Epers_Dotation -> `epers_dotation
  | Epers_DotationLDS -> `epers_dotationlds
  | Epers_Education -> `epers_education
  | Epers_Election -> `epers_election
  | Epers_Emigration -> `epers_emigration
  | Epers_Excommunication -> `epers_excommunication
  | Epers_FamilyLinkLDS -> `epers_familylinklds
  | Epers_FirstCommunion -> `epers_firstcommunion
  | Epers_Funeral -> `epers_funeral
  | Epers_Graduate -> `epers_graduate
  | Epers_Hospitalisation -> `epers_hospitalisation
  | Epers_Illness -> `epers_illness
  | Epers_Immigration-> `epers_immigration
  | Epers_ListePassenger -> `epers_listepassenger
  | Epers_MilitaryDistinction -> `epers_militarydistinction
  | Epers_MilitaryPromotion -> `epers_militarypromotion
  | Epers_MilitaryService -> `epers_militaryservice
  | Epers_MobilisationMilitaire -> `epers_mobilisationmilitaire
  | Epers_Naturalisation -> `epers_naturalisation
  | Epers_Occupation -> `epers_occupation
  | Epers_Ordination -> `epers_ordination
  | Epers_Property -> `epers_property
  | Epers_Recensement -> `epers_recensement
  | Epers_Residence -> `epers_residence
  | Epers_Retired -> `epers_retired
  | Epers_ScellentChildLDS -> `epers_scellentchildlds
  | Epers_ScellentParentLDS -> `epers_scellentparentlds
  | Epers_ScellentSpouseLDS -> `epers_scellentspouselds
  | Epers_VenteBien -> `epers_ventebien
  | Epers_Will -> `epers_will
  | _ -> failwith "print_config"


(* ************************************************************************ *)
(*  [Fonc] print_config : config -> base -> Config                          *)
(** [Description] : Renvoi un message contenant la configuration, i.e. la
                    traduction de plusieurs mots clés : pevent, fevent, ...
                    ainsi que le découpage des lieux ...
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - Config
                                                                           *)
(* ************************************************************************ *)
let print_config conf base =
  let transl_cal =
    List.map
      (fun cal ->
        let (pos, sval) =
          match cal with
          | `gregorian ->
              (cal, transl_nth conf "gregorian/julian/french/hebrew" 0)
          | `julian ->
              (cal, transl_nth conf "gregorian/julian/french/hebrew" 1)
          | `french ->
              (cal, transl_nth conf "gregorian/julian/french/hebrew" 2)
          | `hebrew ->
              (cal, transl_nth conf "gregorian/julian/french/hebrew" 3)
        in
        Mwrite.Transl_calendar.({pos = pos; sval = sval;}))
      [ `gregorian; `julian; `french; `hebrew ]
  in
  let transl_cal = Mwrite.Config_transl_calendar.({msg = transl_cal;}) in
  let transl_wit =
    List.map
      (fun wit ->
        let (pos, sval) =
          match wit with
          | `witness ->
              (`witness, transl_nth conf "witness/witnesses" 0)
          | `witness_godparent ->
              (`witness_godparent,
               transl_nth conf "godfather/godmother/godparents" 2)
        in
        Mwrite.Transl_witness_type.({pos = pos; sval = sval;}))
      [ `witness; `witness_godparent ]
  in
  let transl_wit = Mwrite.Config_transl_witness_type.({msg = transl_wit;}) in
  let transl_prec =
    List.map
      (fun prec ->
        let (pos, sval) =
          match prec with
          | `sure -> (prec, transl conf "exact")
          | `about -> (prec, transl conf "about (date)")
          | `maybe -> (prec, transl conf "possibly (date)")
          | `before -> (prec, transl conf "before (date)")
          | `after -> (prec, transl conf "after (date)")
          | `oryear -> (prec, transl conf "or")
          | `yearint -> (prec, transl conf "between (date)")
        in
        Mwrite.Transl_precision.({pos = pos; sval = sval;}))
      [ `sure; `about; `maybe; `before; `after; `oryear; `yearint ]
  in
  let transl_prec = Mwrite.Config_transl_precision.({msg = transl_prec;}) in
  let transl_death =
    List.map
      (fun death ->
        let (pos, sval) =
          match death with
          | `not_dead -> (death, transl conf "alive")
          | `dead -> (death, transl conf "died")
          | `dead_young -> (death, transl conf "died young")
          | `dont_know_if_dead -> (death, "-")
          | `of_course_dead -> (death, transl conf "of course dead")
          | _ -> failwith "transl_death"
        in
        Mwrite.Transl_death_type.({pos = pos; sval = sval;}))
      [ `not_dead; `dead; `dead_young; `dont_know_if_dead; `of_course_dead ]
  in
  let transl_death = Mwrite.Config_transl_death_type.({msg = transl_death;}) in
  let transl_rel =
    List.map
      (fun rel ->
        let (pos, sval) =
          match rel with
          | `rpt_adoption_father ->
              (rel, transl_nth conf "adoptive father/adoptive mother/adoptive parents" 0)
          | `rpt_adoption_mother ->
              (rel, transl_nth conf "adoptive father/adoptive mother/adoptive parents" 1)
          | `rpt_recognition_father ->
              (rel, transl_nth conf "recognizing father/recognizing mother/recognizing parents" 0)
          | `rpt_recognition_mother ->
              (rel, transl_nth conf "recognizing father/recognizing mother/recognizing parents" 1)
          | `rpt_candidate_parent_father ->
              (rel, transl_nth conf "candidate father/candidate mother/candidate parents" 0)
          | `rpt_candidate_parent_mother ->
              (rel, transl_nth conf "candidate father/candidate mother/candidate parents" 1)
          | `rpt_god_parent_father ->
              (rel, transl_nth conf "godfather/godmother/godparents" 0)
          | `rpt_god_parent_mother ->
              (rel, transl_nth conf "godfather/godmother/godparents" 1)
          | `rpt_foster_parent_father ->
              (rel, transl_nth conf "foster father/foster mother/foster parents" 0)
          | `rpt_foster_parent_mother ->
              (rel, transl_nth conf "foster father/foster mother/foster parents" 1)
        in
        Mwrite.Transl_relation_parent_type.({pos = pos; sval = sval;}))
      [ `rpt_adoption_father; `rpt_adoption_mother;
        `rpt_recognition_father; `rpt_recognition_mother;
        `rpt_candidate_parent_father; `rpt_candidate_parent_mother;
        `rpt_god_parent_father; `rpt_god_parent_mother;
        `rpt_foster_parent_father; `rpt_foster_parent_mother ]
  in
  let transl_rel =
    Mwrite.Config_transl_relation_parent_type.({msg = transl_rel;})
  in
  let transl_fevent =
    List.map
      (fun evt ->
        let (pos, sval) =
          (piqi_event_of_fevent evt, Util.string_of_fevent_name conf base evt)
        in
        Mwrite.Transl_fevent_name.({
          pos = pos;
          sval = sval;
        }))
      [ Efam_Marriage; Efam_NoMarriage; Efam_Engage;
        Efam_Divorce ; Efam_Separated;
        Efam_Annulation; Efam_MarriageBann; Efam_MarriageContract;
        Efam_MarriageLicense; Efam_PACS; Efam_Residence ]
  in
  let transl_fevent =
    Mwrite.Config_transl_fevent_name.({msg = transl_fevent;})
  in
  (* On tri les évènements. Les 4 principaux en premier, *)
  (* les autres et les LDS en derniers.                  *)
  let transl_pevent_prim =
    List.map
      (fun evt ->
        let (pos, sval) =
          (piqi_event_of_pevent evt, Util.string_of_pevent_name conf base evt)
        in
        Mwrite.Transl_pevent_name.({
          pos = pos;
          sval = sval;
        }))
      [ Epers_Birth; Epers_Baptism; Epers_Death; Epers_Burial ]
  in
  let transl_pevent_sec =
    List.map
      (fun evt ->
        let (pos, sval) =
          (piqi_event_of_pevent evt, Util.string_of_pevent_name conf base evt)
        in
        Mwrite.Transl_pevent_name.({
          pos = pos;
          sval = sval;
        }))
      [ Epers_Accomplishment; Epers_Acquisition; Epers_Adhesion;
        Epers_BarMitzvah; Epers_BatMitzvah; Epers_Benediction; Epers_Cremation;
        Epers_ChangeName; Epers_Circumcision; Epers_Confirmation;
        Epers_Decoration; Epers_DemobilisationMilitaire; Epers_Diploma;
        Epers_Distinction; Epers_Dotation; Epers_Education; Epers_Election;
        Epers_Emigration; Epers_Excommunication; Epers_FirstCommunion;
        Epers_Funeral; Epers_Graduate; Epers_Hospitalisation;
        Epers_Illness; Epers_Immigration; Epers_ListePassenger;
        Epers_MilitaryDistinction; Epers_MilitaryPromotion; Epers_MilitaryService;
        Epers_MobilisationMilitaire; Epers_Naturalisation; Epers_Occupation;
        Epers_Ordination; Epers_Property; Epers_Recensement; Epers_Residence;
        Epers_Retired; Epers_VenteBien; Epers_Will ]
  in
  let transl_pevent_sec =
    List.sort
      (fun msg1 msg2 ->
        Gutil.alphabetic_order
          msg1.Mwrite.Transl_pevent_name.sval
          msg2.Mwrite.Transl_pevent_name.sval)
      transl_pevent_sec
  in
  let transl_pevent_LDS =
    List.map
      (fun evt ->
        let (pos, sval) =
          (piqi_event_of_pevent evt, Util.string_of_pevent_name conf base evt)
        in
        Mwrite.Transl_pevent_name.({
          pos = pos;
          sval = sval;
        }))
      [ Epers_BaptismLDS; Epers_ConfirmationLDS; Epers_DotationLDS;
        Epers_FamilyLinkLDS; Epers_ScellentChildLDS; Epers_ScellentParentLDS;
        Epers_ScellentSpouseLDS ]
  in
  let transl_pevent_LDS =
    List.sort
      (fun msg1 msg2 ->
        Gutil.alphabetic_order
          msg1.Mwrite.Transl_pevent_name.sval
          msg2.Mwrite.Transl_pevent_name.sval)
      transl_pevent_LDS
  in
  let transl_pevent = transl_pevent_prim @ transl_pevent_sec @ transl_pevent_LDS in
  let transl_pevent =
    Mwrite.Config_transl_pevent_name.({msg = transl_pevent;})
  in
  let transl_access =
    List.map
      (fun access ->
        let (pos, sval) =
          match access with
          | IfTitles -> (`access_iftitles, transl conf "if titles")
          | Public -> (`access_public, transl conf "public")
          | Private -> (`access_private, transl conf "private")
        in
        Mwrite.Transl_access.({
          pos = pos;
          sval = sval;
        }))
      [ IfTitles; Public; Private ]
  in
  let transl_access = Mwrite.Config_transl_access.({msg = transl_access;}) in
  let transl_warning =
    List.map
      (fun warn ->
        let (pos, sval) =
          match warn with
          (* erreur JS *)
          | `empty_index -> (warn, transl conf "index required")
          | `empty_surname -> (warn, transl conf "surname missing")
          | `empty_first_name -> (warn, transl conf "first name missing")
          | `empty_sex -> (warn, transl conf "sex required")
          | `required_field -> (warn, transl conf "field required")
          | `birth_date_after_event -> (warn, transl conf "birth date after event")
          | `death_date_before_event -> (warn, transl conf "death date before event")
        in
        Mwrite.Transl_update_warning_js.({
          pos = pos;
          sval = sval;
        }))
      [ `empty_index; `empty_surname; `empty_first_name;
        `empty_sex; `required_field; `birth_date_after_event;
        `death_date_before_event ]
  in
  let transl_warning = Mwrite.Config_transl_update_warning_js.({msg = transl_warning;}) in
  let transl_short_greg_month =
    List.map
      (fun month ->
        let (pos, sval) =
          match month with
          | `janv -> (month, transl_nth conf "(short month)" 0)
          | `fevr -> (month, transl_nth conf "(short month)" 1)
          | `mars -> (month, transl_nth conf "(short month)" 2)
          | `avr -> (month, transl_nth conf "(short month)" 3)
          | `mai -> (month, transl_nth conf "(short month)" 4)
          | `juin -> (month, transl_nth conf "(short month)" 5)
          | `juil -> (month, transl_nth conf "(short month)" 6)
          | `aout -> (month, transl_nth conf "(short month)" 7)
          | `sept -> (month, transl_nth conf "(short month)" 8)
          | `oct -> (month, transl_nth conf "(short month)" 9)
          | `nov -> (month, transl_nth conf "(short month)" 10)
          | `dec -> (month, transl_nth conf "(short month)" 11)
        in
        Mwrite.Transl_short_greg_month.({
          pos = pos;
          sval = sval;
        }))
      [ `janv; `fevr; `mars; `avr; `mai; `juin;
        `juil; `aout; `sept; `oct; `nov; `dec ]
  in
  let transl_short_greg_month = Mwrite.Config_transl_short_greg_month.({msg = transl_short_greg_month;}) in
  let transl_french_month =
    List.map
      (fun month ->
        let (pos, sval) =
          match month with
          | `vendemiaire -> (month, transl_nth conf "(french revolution month)" 0)
          | `brumaire -> (month, transl_nth conf "(french revolution month)" 1)
          | `frimaire -> (month, transl_nth conf "(french revolution month)" 2)
          | `nivose -> (month, transl_nth conf "(french revolution month)" 3)
          | `pluviose -> (month, transl_nth conf "(french revolution month)" 4)
          | `ventose -> (month, transl_nth conf "(french revolution month)" 5)
          | `germinal -> (month, transl_nth conf "(french revolution month)" 6)
          | `floreal -> (month, transl_nth conf "(french revolution month)" 7)
          | `prairial -> (month, transl_nth conf "(french revolution month)" 8)
          | `messidor -> (month, transl_nth conf "(french revolution month)" 9)
          | `thermidor -> (month, transl_nth conf "(french revolution month)" 10)
          | `fructidor -> (month, transl_nth conf "(french revolution month)" 11)
          | `complementaire -> (month, transl_nth conf "(french revolution month)" 12)
        in
        Mwrite.Transl_french_month.({
          pos = pos;
          sval = sval;
        }))
      [ `vendemiaire; `brumaire; `frimaire; `nivose; `pluviose; `ventose;
        `germinal; `floreal; `prairial; `messidor; `thermidor; `fructidor;
        `complementaire ]
  in
  let transl_french_month = Mwrite.Config_transl_french_month.({msg = transl_french_month;}) in
  let transl_hebrew_month =
    List.map
      (fun month ->
        let (pos, sval) =
          match month with
          | `tichri -> (month, transl_nth conf "(hebrew month)" 0)
          | `marhechvan -> (month, transl_nth conf "(hebrew month)" 1)
          | `kislev -> (month, transl_nth conf "(hebrew month)" 2)
          | `tevet -> (month, transl_nth conf "(hebrew month)" 3)
          | `chevat -> (month, transl_nth conf "(hebrew month)" 4)
          | `adar_1 -> (month, transl_nth conf "(hebrew month)" 5)
          | `adar_2 -> (month, transl_nth conf "(hebrew month)" 6)
          | `nissan -> (month, transl_nth conf "(hebrew month)" 7)
          | `iyar -> (month, transl_nth conf "(hebrew month)" 8)
          | `sivan -> (month, transl_nth conf "(hebrew month)" 9)
          | `tamouz -> (month, transl_nth conf "(hebrew month)" 10)
          | `av -> (month, transl_nth conf "(hebrew month)" 11)
          | `eloul -> (month, transl_nth conf "(hebrew month)" 12)
        in
        Mwrite.Transl_hebrew_month.({
          pos = pos;
          sval = sval;
        }))
      [ `tichri; `marhechvan; `kislev; `tevet; `chevat; `adar_1;
        `adar_2; `nissan; `iyar; `sivan; `tamouz; `av; `eloul ]
  in
  let transl_hebrew_month = Mwrite.Config_transl_hebrew_month.({msg = transl_hebrew_month;}) in
  let (gwf_place_format, gwf_place_format_placeholder) =
    match p_getenv conf.base_env "places_format" with
    | Some s ->
        let placeholder =
          (try
             List.fold_right
               (fun s accu ->
                  match s with
                  | "Subdivision" -> accu
                  | "Town" -> (transl conf "town") :: accu
                  | "Area code" -> (transl conf "area code") :: accu
                  | "County" -> (transl conf "county") :: accu
                  | "Region" -> (transl conf "region") :: accu
                  | "Country" -> (transl conf "country") :: accu
                  | _ -> raise Not_found)
               (String.split_on_char ',' s) []
           with Not_found -> [])
        in
        let placeholder = String.concat ", " placeholder in
        (* On ajoute les lieux-dit. *)
        let placeholder =
          match String.split_on_char ',' s with
          | "Subdivision" :: _ ->
            "[" ^ (transl conf "subdivision") ^ "] - " ^ placeholder
          | _ -> placeholder
        in
        (s, placeholder)
    | None -> ("", "")
  in
  let config =
    Mwrite.Config.({
      transl_cal = transl_cal;
      transl_wit = transl_wit;
      transl_prec = transl_prec;
      transl_death = transl_death;
      transl_rel = transl_rel;
      transl_fevents = transl_fevent;
      transl_pevents = transl_pevent;
      transl_access = transl_access;
      transl_warning = transl_warning;
      transl_short_greg_month = transl_short_greg_month;
      transl_french_month = transl_french_month;
      transl_hebrew_month = transl_hebrew_month;
      gwf_place_format = gwf_place_format;
      gwf_place_format_placeholder = gwf_place_format_placeholder;
    })
  in
  let data = Mext_write.gen_config config in
  print_result conf data


(**/**) (* Fonctions qui calcul "l'inférence" du nom de famille. *)

let all_children_surname_are_the_same base fam =
  let count = ref 0 in
  let fam' =
    Array.map
      (fun i -> let c = get_children @@ foi base i in count := !count + Array.length c ; c)
      fam
  in
  let all_children_surname = Array.make !count "" in
  count := 0 ;
  Array.iter
    (Array.iter (fun i ->
         all_children_surname.(!count) <- sou base @@ get_surname @@ poi base i ;
         incr count) )
    fam' ;
  match all_children_surname with
  | [||] -> (false, "")
  | [|x|] -> (true, x)
  | a ->
    let x_crush = Name.crush_lower a.(0) in
    if Array.for_all (fun n -> Name.crush_lower n = x_crush) a
    then (true, a.(0))
    else (false, "")

(* ************************************************************************ *)
(*  [Fonc] infer_surname : config -> base -> person -> string               *)
(** [Description] : Renvoie le nom de famille qui peut être attribué à une
                    personne. Si aucun nom ne peut être trouvé, on renvoi
                    vide.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - person : la personne à partir de laquelle on calcul le nom
    [Retour] :
      - string : le potentiel nom hérité.
                                                                           *)
(* ************************************************************************ *)
let rec infer_surname conf base p ifam =
  let surname = sou base (get_surname p) in
  if surname = "?" then ""
  else
    if get_sex p = Male then
      (* On prend le nom de la fratrie parce que y'a de *)
      (* grande chance que ce soit le même.             *)
      let fam = get_family p in
      if Array.length fam > 0 then
        begin
          if Array.exists (fun ifam -> [||] <> get_children (foi base ifam)) fam then
            let all_children_surname_are_the_same, name =
              all_children_surname_are_the_same base fam
            in
            if all_children_surname_are_the_same then
              (* On fait une recherche métaphone. *)
              let (primary_surname, secondary_surname) =
                Metaphone.double_metaphone surname
              in
              let (primary_name, secondary_name) = Metaphone.double_metaphone name in
              if primary_surname = primary_name ||
                 secondary_surname = secondary_name
              then surname
              else ""
            else ""
          else
            begin
              match get_parents p with
              | Some ifam ->
                  begin
                    let g_fam = foi base ifam in
                    let g_father = poi base (get_father g_fam) in
                    if Name.crush_lower surname =
                       Name.crush_lower (sou base (get_surname g_father))
                    then
                      surname
                    else
                      (* On fait une recherche métaphone. *)
                      let (primary_father, secondary_father) =
                        Metaphone.double_metaphone surname
                      in
                      let (primary_g_father, secondary_g_father) =
                        Metaphone.double_metaphone
                          (sou base (get_surname g_father))
                      in
                      if primary_father = primary_g_father ||
                         secondary_father = secondary_g_father
                      then surname
                      else ""
                  end
              | None -> surname
            end
        end
      (* On regarde si le nom est pareil sur 2 générations. *)
      else
        begin
          match get_parents p with
          | Some ifam ->
              begin
                let g_fam = foi base ifam in
                let g_father = poi base (get_father g_fam) in
                if Name.crush_lower surname =
                   Name.crush_lower (sou base (get_surname g_father))
                then
                  surname
                else
                  (* On fait une recherche métaphone. *)
                  let (primary_father, secondary_father) =
                    Metaphone.double_metaphone surname
                  in
                  let (primary_g_father, secondary_g_father) =
                    Metaphone.double_metaphone
                      (sou base (get_surname g_father))
                  in
                  if primary_father = primary_g_father ||
                     secondary_father = secondary_g_father
                  then surname
                  else ""
              end
          | None -> surname
        end
    else
      (* Si on a envoyé dans l'objet AddChildRequest l'index de la famille,  *)
      (* et que la personne selectionnée est une femme, on relance le calcul avec *)
      (* le nom du père.                                                           *)
      match ifam with
      | Some ifam ->
          let ifam = Adef.ifam_of_int (Int32.to_int ifam) in
          let fam = foi base ifam in
          let isp = Gutil.spouse (get_key_index p) fam in
          let sp = poi base isp in
          if get_sex sp = Male then infer_surname conf base sp None
          else ""
      | None ->
          (* On prend le nom de la fratrie parce que y'a de *)
          (* grande chance que ce soit le même.             *)
          let fam = get_family p in
          if Array.length fam > 0 then
            begin
              if Array.exists (fun ifam -> [||] <> get_children (foi base ifam)) fam then
                let all_children_surname_are_the_same, name =
                  all_children_surname_are_the_same base fam
                in
                (* On ne fait pas de recherche métaphone *)
                (* car on est dans le cas d'une femme.   *)
                if all_children_surname_are_the_same then name
                else ""
              else
              if Array.length (get_family p) = 1 then
                let fam = get_family p in
                let ifam = fam.(0) in
                let fam = foi base ifam in
                let isp = Gutil.spouse (get_key_index p) fam in
                let sp = poi base isp in
                if sou base (get_surname sp) = "?" then ""
                else sou base (get_surname sp)
              else ""
            end
          else ""

(* FIXME: factorize *)
let piqi_death_type_of_death = function
  | NotDead -> `not_dead
  | DontKnowIfDead -> `dont_know_if_dead
  | OfCourseDead -> `of_course_dead
  | _ -> assert false

let infer_death conf base p =
  piqi_death_type_of_death (Update.infer_death conf base p)

let empty_death_pevent () =
  { Mwrite.Pevent.pevent_type = Some `epers_death;
    date = None;
    place = None;
    reason = None;
    note = None;
    src = None;
    witnesses = [];
    event_perso = None;
  }

(**/**) (* Fonctions qui renvoie le ModificationStatus. *)

let compute_warnings conf base resp =
  let print_someone p =
    sou base (get_first_name p) ^ " " ^ sou base (get_surname p)
  in
  let print_someone_dates p =
    print_someone p ^ " " ^ Date.short_dates_text conf base p
  in
  match resp with
  | Api_update_util.UpdateErrorConflict c -> (false, [], [], Some c, [])
  | Api_update_util.UpdateError s -> (false, [s], [], None, [])
  | Api_update_util.UpdateSuccess (wl, ml, hr) ->
      let warning =
        List.fold_right
          (fun w wl ->
            match w with
            | BigAgeBetweenSpouses (fath, moth, a) ->
                let w =
                  (Printf.sprintf
                     (fcapitale
                        (ftransl conf
                           "the difference of age between %t and %t is quite important"))
                     (fun _ -> print_someone fath)
                     (fun _ -> print_someone moth))
                  ^ ": " ^ (Date.string_of_age conf a)
                in
                w :: wl
            | BirthAfterDeath p ->
                let w =
                Printf.sprintf
                  (ftransl conf "%t died before his/her birth")
                  (fun _ -> print_someone_dates p)
                in
                w :: wl
            | ChangedOrderOfChildren _ -> wl
                (* On ignore les messages de changement d'ordre. *)
                (*
                let cpl = foi base ifam in
                let fath = poi base (get_father cpl) in
                let moth = poi base (get_mother cpl) in
                (capitale (transl conf "changed order of children")) ^ " " ^
                (Gutil.designation base fath ^ "\n" ^ transl_nth conf "and" 0 ^
                     " " ^ Gutil.designation base moth ^ "\n")
                *)
            | ChangedOrderOfMarriages _ -> wl
                (* On ignore les messages de changement d'ordre. *)
                (*
                (capitale (transl conf "changed order of marriages"))
                *)
            | ChangedOrderOfFamilyEvents _ -> wl
                (* On ignore les messages de changement d'ordre. *)
                (*
                (capitale (transl conf "changed order of family's events"))
                *)
            | ChangedOrderOfPersonEvents _ -> wl
                (* On ignore les messages de changement d'ordre. *)
                (*
                (capitale (transl conf "changed order of person's events"))
                *)
            | ChildrenNotInOrder _ -> wl
                (* On ignore les messages de changement d'ordre. *)
                (*
                let cpl = foi base ifam in
                (Printf.sprintf
                   (fcapitale
                      (ftransl conf
                         "the following children of %t and %t are not in order"))
                   (fun _ ->
                     Gutil.designation base (poi base (get_father cpl)))
                   (fun _ ->
                     Gutil.designation base (poi base (get_mother cpl))))
                ^ ": " ^
                Gutil.designation base elder ^ (Date.short_dates_text conf base elder) ^
                Gutil.designation base x ^ (Date.short_dates_text conf base x)
                *)
            | CloseChildren (ifam, _, elder, x) ->
                let cpl = foi base ifam in
                let w =
                (Printf.sprintf
                   (fcapitale
                      (ftransl conf
                         "the following children of %t and %t are born very close"))
                   (fun _ -> print_someone (poi base (get_father cpl)))
                   (fun _ -> print_someone (poi base (get_mother cpl))))
                ^ ": " ^
                print_someone_dates elder ^ " " ^ print_someone_dates x
                in
                w :: wl
            | DeadOld (p, a) ->
                let w =
                print_someone p
                  ^ " " ^
                  (transl_nth
                     conf "died at an advanced age" (index_of_sex (get_sex p)))
                  ^ " " ^
                  (Date.string_of_age conf a)
                in
                w :: wl
            | DeadTooEarlyToBeFather (father, child) ->
                let w =
                Printf.sprintf
                  (ftransl conf "\
          %t is born more than 2 years after the death of his/her father %t")
                  (fun _ -> print_someone_dates child)
                  (fun _ -> print_someone_dates father)
                in
                w :: wl
            | FEventOrder (p, e1, e2) ->
                let w =
                  Printf.sprintf
                    (ftransl conf "%t's %s before his/her %s")
                    (fun _ -> print_someone_dates p)
                    (Util.string_of_fevent_name conf base e1.efam_name)
                    (Util.string_of_fevent_name conf base e2.efam_name)
                in
                w :: wl
            | FWitnessEventAfterDeath (p, e) ->
                let w =
                  Printf.sprintf
                    (ftransl conf "%t witnessed the %s after his/her death")
                    (fun _ -> print_someone_dates p)
                    (Util.string_of_fevent_name conf base e.efam_name)
                in
                w :: wl
            | FWitnessEventBeforeBirth (p, e) ->
                let w =
                  Printf.sprintf
                    (ftransl conf "%t witnessed the %s before his/her birth")
                    (fun _ -> print_someone_dates p)
                    (Util.string_of_fevent_name conf base e.efam_name)
                in
                w :: wl
            | IncoherentSex (p, _, _) ->
                let w =
                Printf.sprintf
                  (fcapitale
                     (ftransl conf "%t's sex is not coherent with his/her relations"))
                  (fun _ -> print_someone p)
                in
                w :: wl
            | IncoherentAncestorDate (anc, p) ->
                let w =
                Printf.sprintf "%s has a younger ancestor %s"
                  (print_someone p)
                  (print_someone anc)
                in
                w :: wl
            | MarriageDateAfterDeath p ->
                let w =
                Printf.sprintf
                  (fcapitale (ftransl conf "marriage had occurred after the death of %t"))
                  (fun _ -> print_someone_dates p)
                in
                w :: wl
            | MarriageDateBeforeBirth p ->
                let w =
                Printf.sprintf
                  (fcapitale (ftransl conf "marriage had occurred before the birth of %t"))
                  (fun _ -> print_someone_dates p)
                in
                w :: wl
            | MotherDeadAfterChildBirth (mother, child) ->
                let w =
                Printf.sprintf
                  (ftransl conf "%t is born after the death of his/her mother %t")
                  (fun _ -> print_someone_dates child)
                  (fun _ -> print_someone_dates mother)
                in
                w :: wl
            | ParentBornAfterChild (p, c) ->
                let w =
                Printf.sprintf "%s\n%s\n%s" (print_someone p)
                  (transl conf "is born after his/her child")
                  (print_someone c)
                in
                w :: wl
            | ParentTooYoung (p, a) ->
                let w =
                Printf.sprintf "%s\n%s\n" (print_someone_dates p)
                  (transl conf "is a very young parent") ^
                Printf.sprintf "(%s)" (Date.string_of_age conf a)
                in
                w :: wl
            | PossibleDuplicateFam (f1, _) ->
              let f = foi base f1 in
              let w =
                Printf.sprintf
                  (fcapitale (ftransl conf "%s and %s have several unions"))
                  (print_someone @@ poi base @@ get_father f)
                  (print_someone @@ poi base @@ get_mother f)
              in
              w :: wl
            | ParentTooOld (p, a) ->
                let w =
                Printf.sprintf "%s\n%s\n" (print_someone p)
                  (transl conf "is a very old parent") ^
                Printf.sprintf "(%s)" (Date.string_of_age conf a);
                in
                w :: wl
            | PEventOrder (p, e1, e2) ->
                let w =
                  Printf.sprintf
                    (ftransl conf "%t's %s before his/her %s")
                    (fun _ -> print_someone_dates p)
                    (Util.string_of_pevent_name conf base e1.epers_name)
                    (Util.string_of_pevent_name conf base e2.epers_name)
                in
                w :: wl
            | PWitnessEventAfterDeath (p, e) ->
                let w =
                  Printf.sprintf
                    (ftransl conf "%t witnessed the %s after his/her death")
                    (fun _ -> print_someone_dates p)
                    (Util.string_of_pevent_name conf base e.epers_name)
                in
                w :: wl
            | PWitnessEventBeforeBirth (p, e) ->
                let w =
                  Printf.sprintf
                    (ftransl conf "%t witnessed the %s before his/her birth")
                    (fun _ -> print_someone_dates p)
                    (Util.string_of_pevent_name conf base e.epers_name)
                in
                w :: wl
            | TitleDatesError (p, t) ->
                let w =
                Printf.sprintf
                  (fcapitale (ftransl conf "%t has incorrect title dates: %t"))
                  (fun _ -> print_someone_dates p)
                  (fun _ ->
                     Printf.sprintf "%s %s %s-%s"
                       (sou base t.t_ident) (sou base t.t_place)
                       (match Adef.od_of_cdate t.t_date_start with
                        | Some d -> Date.string_of_date conf d
                        | _ -> "" )
                       (match Adef.od_of_cdate t.t_date_end with
                        | Some d -> Date.string_of_date conf d
                        | _ -> "" ))
                in
                w :: wl
            | UndefinedSex p ->
                let w =
                Printf.sprintf
                  (fcapitale (ftransl conf "undefined sex for %t"))
                  (fun _ -> print_someone p)
                in
                w :: wl
            | WitnessDateAfterDeath p ->
                let w =
                Printf.sprintf
                  (fcapitale (ftransl conf "%t was witness after his/her death"))
                  (fun _ -> print_someone_dates p)
                in
                w :: wl
            | WitnessDateBeforeBirth p ->
                let w =
                Printf.sprintf
                  (fcapitale (ftransl conf "%t was witness before his/her birth"))
                  (fun _ -> print_someone_dates p)
                in
                w :: wl
            | YoungForMarriage (p, a) ->
                let w =
                print_someone p ^ " " ^
                  (Printf.sprintf
                     (ftransl conf "married at age %t")
                     (fun _ -> Date.string_of_age conf a))
                in
                w :: wl)
          wl []
      in
      let misc =
        List.fold_right
          (fun m ml ->
            match m with
            | MissingSources ->
                let m = (capitale (transl conf "missing sources")) in
                m :: ml)
          ml []
      in
      (true, warning, misc, None, hr)

let compute_modification_status conf base ip ifam resp =
  let (surname, first_name, occ, index_person, surname_str, first_name_str) =
    if ip < 0 then ("", "", None, None, None, None)
    else
      let p = poi base (Adef.iper_of_int ip) in
      let surname = sou base (get_surname p) in
      let first_name = sou base (get_first_name p) in
      let index_person = Some (Int32.of_int ip) in
      let occ = get_occ p in
      let occ = if occ = 0 then None else Some (Int32.of_int occ) in
      let surname_str = Some (sou base (get_surname p)) in
      let first_name_str = Some (sou base (get_first_name p)) in
      if not (Util.accessible_by_key conf base p first_name surname) ||
         (surname = "" && first_name = "")
      then
        ("", "", None, index_person, surname_str, first_name_str)
      else
        (surname, first_name, occ, index_person, surname_str, first_name_str)
  in
  let sn = if surname = "" then None else Some (Name.lower surname) in
  let fn = if first_name = "" then None else Some (Name.lower first_name) in
  let index_family = if ifam < 0 then None else Some (Int32.of_int ifam) in
  let (is_base_updated, warnings, miscs, conflict, history_records) =
    compute_warnings conf base resp
  in
  (* Maintenant que l'on sait si tout s'est bien passé, *)
  (* on peut enfin commiter le fichier patch.           *)
  let () =
    if is_base_updated then
      begin
        List.iter (fun f -> f ()) history_records;
        Util.commit_patches conf base;
      end
    else ()
  in
  let response =
    {
      Mwrite.Modification_status.is_base_updated = is_base_updated;
      base_warnings = warnings;
      base_miscs = miscs;
      index_person = index_person;
      lastname = surname;
      firstname = first_name;
      occ = occ;
      index_family = index_family;
      conflict = conflict;
      lastname_str = surname_str;
      firstname_str = first_name_str;
      n = sn;
      p = fn;
    }
  in
  Mext_write.gen_modification_status response 

(**/**) (* Fonctions d'ajout de la première personne. *)


(* ************************************************************************ *)
(*  [Fonc] print_add_ind_start_ok : config -> base -> ModificationStatus    *)
(** [Description] : Fonction qui ajoute une personne à la base lors de la
                    création d'un arbre et de la première saisie.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_add_ind_start_ok conf base =
  let start_p = get_params conf Mext.parse_person_start in
  let mod_p =
    Api_update_util.piqi_mod_person_of_person_start conf base start_p
  in
  let resp = Api_update_person.print_add conf base mod_p in
  let ref_p =
    match resp with
    | Api_update_util.UpdateError _  | Api_update_util.UpdateErrorConflict _ ->
        M.Reference_person.({
          n = "";
          p = "";
          oc = Int32.of_int 0;
        })
    | Api_update_util.UpdateSuccess _ ->
        Util.commit_patches conf base;
        let ip = Int32.to_int mod_p.Mwrite.Person.index in
        let p = poi base (Adef.iper_of_int ip) in
        let fn = Name.lower (sou base (get_first_name p)) in
        let sn = Name.lower (sou base (get_surname p)) in
        let occ = Int32.of_int (get_occ p) in
        M.Reference_person.({
          n = sn;
          p = fn;
          oc = occ;
        })
  in
  let data = Mext.gen_reference_person ref_p in
  print_result conf data


(**/**) (* Fonctions de modification individu. *)


(* ************************************************************************ *)
(*  [Fonc] print_mod_ind : config -> base -> ModificationStatus             *)
(** [Description] : Fonction qui renvoi les informations d'une personne
                    afin d'afficher le formulaire de modification.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_mod_ind conf base =
  let params = get_params conf Mext_write.parse_index_person in
  let ip = Adef.iper_of_int (Int32.to_int params.Mwrite.Index_person.index) in
  let p = poi base ip in
  let mod_p = Api_update_util.pers_to_piqi_mod_person conf base p in
  let data = Mext_write.gen_person mod_p in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] print_mod_ind_ok : config -> base -> ModificationStatus          *)
(** [Description] : Fonction qui réalise les modifications d'une personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_mod_ind_ok conf base =
  let mod_p = get_params conf Mext_write.parse_person in
  let resp = Api_update_person.print_mod conf base mod_p in
  let ip = Int32.to_int mod_p.Mwrite.Person.index in
  let data = compute_modification_status conf base ip (-1) resp in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] print_add_ind_ok : config -> base -> ModificationStatus              *)
(** [Description] : Fonction qui ajoute une personne à la base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_add_ind_ok conf base =
  let mod_p = get_params conf Mext_write.parse_person in
  let resp = Api_update_person.print_add conf base mod_p in
  let ip = Int32.to_int mod_p.Mwrite.Person.index in
  let data = compute_modification_status conf base ip (-1) resp in
  print_result conf data


(* Fonction qui calcule la personne sur laquelle on va faire la redirection. *)
(*
   Dans l'ordre :
     - sur le parent (père)
     - sur le premier enfant
     - sur le sosa
     - sur l'accueil
*)
let compute_redirect_person conf base ip =
  let p = poi base ip in
  match get_parents p with
  | Some ifam ->
      let fam = foi base ifam in
      let ifath = get_father fam in
      let imoth = get_mother fam in
      let father = poi base ifath in
      if sou base (get_surname father) = "?" &&
         sou base (get_first_name father) = "?"
      then imoth
      else ifath
  | None ->
      if Array.length (get_family p) > 0 then
        (* On renvoi sur le premier conjoint. *)
        let fam = get_family p in
        Gutil.spouse ip (foi base fam.(0))
      else
        match Util.find_sosa_ref conf base with
        | Some pz ->
            let ipz = get_key_index pz in
            (* Si on supprime le sosa ... *)
            if ip = ipz then
              match Util.default_sosa_ref conf base with
              | Some p -> get_key_index p
              | None -> Adef.iper_of_int (-1)
            else ipz
        | None -> Adef.iper_of_int (-1)


(* ************************************************************************ *)
(*  [Fonc] print_del_ind_ok : config -> base -> ModificationStatus          *)
(** [Description] : Fonction qui supprime une personne de la base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_del_ind_ok conf base =
  let params = get_params conf Mext_write.parse_index_person in
  let ip = Int32.to_int params.Mwrite.Index_person.index in
  let ip = Adef.iper_of_int ip in
  let ip_redirect = compute_redirect_person conf base ip in
  (* Si la personne n'a pas d'enfant, on veut alors *)
  (* également le délier de sa/ses famille/s        *)
  let p = poi base ip in
  let has_children =
    List.exists
      (fun ifam ->
        let des = foi base ifam in
        Array.length (get_children des) > 0)
      (Array.to_list (get_family p))
  in
  let resp =
    try
      (* Déliaison de toutes les familles. *)
      let (all_wl, all_ml, all_hr) =
        if has_children then ([], [], [])
        else
          List.fold_left
            (fun (all_wl, all_ml, all_hr) ifam ->
               match Api_update_family.print_del conf base ip ifam with
               | Api_update_util.UpdateSuccess (wl, ml, hr) ->
                   (all_wl @ wl, all_ml @ ml, all_hr @ hr)
               | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
               | Api_update_util.UpdateErrorConflict c ->
                    raise (Api_update_util.ModErrApiConflict c))
            ([], [], []) (Array.to_list (get_family p))
      in
      let (all_wl, all_ml, all_hr) =
        match Api_update_person.print_del conf base ip with
        | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
        | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
        | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
      in
      Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
    with
    | Update.ModErrApi s -> Api_update_util.UpdateError s
    | Api_update_util.ModErrApiConflict c ->
        Api_update_util.UpdateErrorConflict c
  in
  let data =
    compute_modification_status
      conf base (Adef.int_of_iper ip_redirect) 0 resp
  in
  print_result conf data


(**/**) (* Fonctions de modification famille. *)


(* ************************************************************************ *)
(*  [Fonc] print_del_fam_ok : config -> base -> ModificationStatus          *)
(** [Description] : Fonction qui supprime une famille de la base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_del_fam_ok conf base =
  let params = get_params conf Mext_write.parse_index_person_and_family in
  let ip = Int32.to_int params.Mwrite.Index_person_and_family.index_person in
  let ifam =
    Int32.to_int params.Mwrite.Index_person_and_family.index_family
  in
  let ip = Adef.iper_of_int ip in
  let ifam = Adef.ifam_of_int ifam in
  let resp = Api_update_family.print_del conf base ip ifam in
  let data =
    compute_modification_status conf base (Adef.int_of_iper ip) (-1) resp
  in
  print_result conf data

let set_parents_fields conf base p linked created =
  linked.Mwrite.Person.create_link <- `link;
  created.Mwrite.Person.index <- Int32.of_int 0;
  created.Mwrite.Person.access <- `access_iftitles;
  created.Mwrite.Person.create_link <- `create_default_occ;
  created.Mwrite.Person.digest <- "";
  match infer_death conf base p with
  | `of_course_dead ->
    created.Mwrite.Person.death_type <- `of_course_dead ;
    created.Mwrite.Person.pevents <- created.Mwrite.Person.pevents @ [ empty_death_pevent () ]
  | x -> created.Mwrite.Person.death_type <- x

(* ************************************************************************ *)
(*  [Fonc] compute_add_family : config -> base -> person -> Family (piqi)   *)
(** [Description] : Permet la factorisation du code pour ajouter une famille
                    et ajouter un enfant à une nouvelle famille.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : la personne à qui on ajoute la famille
    [Retour] :
      - Family : la famille piqi
                                                                           *)
(* ************************************************************************ *)
let compute_add_family conf base p =
  let adding_to_father = get_sex p = Male in
  let family =
    Api_update_util.piqi_empty_family conf base (Adef.ifam_of_int (-1))
  in
  let p_father =
    if adding_to_father then p else Gwdb.empty_person base (Adef.iper_of_int (-1))
  in
  let p_mother =
    if adding_to_father then Gwdb.empty_person base (Adef.iper_of_int (-1))
    else p
  in
  let father = Api_update_util.pers_to_piqi_mod_person conf base p_father in
  let mother = Api_update_util.pers_to_piqi_mod_person conf base p_mother in
  (* Les index négatifs ne marchent pas ! *)
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  family.Mwrite.Family.index <- Int32.of_int 0;
  if adding_to_father
  then begin
    mother.Mwrite.Person.sex <- `female ;
    set_parents_fields conf base p father mother
  end
  else begin
    father.Mwrite.Person.sex <- `male;
    set_parents_fields conf base p mother father
  end ;
  family.Mwrite.Family.father <- father;
  family.Mwrite.Family.mother <- mother;
  family

(* ************************************************************************ *)
(*  [Fonc] print_add_family : config -> base -> AddFamily                   *)
(** [Description] : Renvoie le conjoint dive où on a calculé le décès pour
                    le conjoint, ainsi qu'une famille vide.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - AddFamily : les informations du template.
                                                                           *)
(* ************************************************************************ *)
let print_add_family conf base =
  let params = get_params conf Mext_write.parse_index_person in
  let ip = Int32.to_int params.Mwrite.Index_person.index in
  let ip = Adef.iper_of_int ip in
  let p = poi base ip in
  let surname = sou base (get_surname p) in
  let first_name = sou base (get_first_name p) in
  let family = compute_add_family conf base p in
  let add_family =
    {
      Mwrite.Add_family.person_lastname = surname;
      person_firstname = first_name;
      family = family;
    }
  in
  let data = Mext_write.gen_add_family add_family in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] compute_add_family_ok : config -> base -> Family -> UpdateStatus *)
(** [Description] : Permet la factorisation du code pour ajouter une famille
                    et ajouter un enfant à une nouvelle famille.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - mod_family : la famille que l'on veut ajouter
    [Retour] :
      - UpdateStatus
                                                                           *)
(* ************************************************************************ *)
let compute_add_family_ok conf base mod_family =
  let mod_father = mod_family.Mwrite.Family.father in
  let mod_mother = mod_family.Mwrite.Family.mother in
  let moth_fn = mod_mother.Mwrite.Person.firstname in
  let moth_sn = mod_mother.Mwrite.Person.lastname in
  mod_mother.Mwrite.Person.firstname <- moth_fn;
  mod_mother.Mwrite.Person.lastname <- moth_sn;
  (*
     On ajoute une famille, il faut effectuer les actions suivantes :
       - modification de la personne sur laquelle on clic (pour les clés) => MOD_IND
       - ajout de la famille => ADD_FAM
       - modification de la personne restante => MOD_IND
  *)
  try
    begin
      match (mod_father.Mwrite.Person.create_link,
             mod_mother.Mwrite.Person.create_link)
      with
      | (`link, (`create | `create_default_occ)) ->
        let (all_wl, all_ml, all_hr) =
          match Api_update_person.print_mod conf base mod_father with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          match Api_update_family.print_add
                  conf base mod_family mod_father mod_mother
          with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        (* Dans le cas d'ajout d'un enfant avec nouveau conjoint, *)
        (* le parent créé vaut ??, donc on ne pourra JAMAIS lui   *)
        (* apporter de modifications.                             *)
        let (all_wl, all_ml, all_hr) =
          if mod_mother.Mwrite.Person.lastname = "" ||
             mod_mother.Mwrite.Person.firstname = ""
          then
            (all_wl, all_ml, all_hr)
          else
            match Api_update_person.print_mod conf base mod_mother with
            | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
            | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
            | Api_update_util.UpdateErrorConflict c ->
              (* On dit que c'est le formulaire de la femme. *)
              c.Mwrite.Create_conflict.form <- Some `person_form2;
              raise (Api_update_util.ModErrApiConflict c)
        in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
      | ((`create | `create_default_occ), `link) ->
          (*
          let occ = Api_update_util.find_free_occ base fath_fn fath_sn in
          if occ = 0 then
            mod_father.Mwrite.Person.occ <- None
          else
            mod_father.Mwrite.Person.occ <- Some (Int32.of_int occ);
          *)
        let (all_wl, all_ml, all_hr) =
          match Api_update_person.print_mod conf base mod_mother with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
            (* On dit que c'est le formulaire de la femme. *)
            c.Mwrite.Create_conflict.form <- Some `person_form2;
            raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          match
            Api_update_family.print_add
              conf base mod_family mod_father mod_mother
          with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        (* Dans le cas d'ajout d'un enfant avec nouveau conjoint, *)
        (* le parent créé vaut ??, donc on ne pourra JAMAIS lui   *)
        (* apporter de modifications.                             *)
        let (all_wl, all_ml, all_hr) =
          if mod_father.Mwrite.Person.lastname = ""
          || mod_father.Mwrite.Person.firstname = ""
          then
            (all_wl, all_ml, all_hr)
          else
            match Api_update_person.print_mod conf base mod_father with
            | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
            | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
            | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
      | (`link, `link) ->
        let (all_wl, all_ml, all_hr) =
          match Api_update_person.print_mod conf base mod_father with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          match Api_update_person.print_mod conf base mod_mother with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
            (* On dit que c'est le formulaire de la femme. *)
            c.Mwrite.Create_conflict.form <- Some `person_form2;
            raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          match Api_update_family.print_add
                  conf base mod_family mod_father mod_mother
          with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
      | ((`create | `create_default_occ), (`create | `create_default_occ)) ->
        let (all_wl, all_ml, all_hr) =
          match Api_update_family.print_add
                  conf base mod_family mod_father mod_mother
          with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        (* Dans le cas d'ajout d'un enfant avec nouveau conjoint, *)
        (* le parent créé vaut ??, donc on ne pourra JAMAIS lui   *)
        (* apporter de modifications.                             *)
        let (all_wl, all_ml, all_hr) =
          if mod_father.Mwrite.Person.lastname = "" ||
             mod_father.Mwrite.Person.firstname = ""
          then
            (all_wl, all_ml, all_hr)
          else
            match Api_update_person.print_mod conf base mod_father with
            | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
            | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
            | Api_update_util.UpdateErrorConflict c ->
              (* On dit que c'est le formulaire de la femme. *)
              c.Mwrite.Create_conflict.form <- Some `person_form2;
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          if mod_mother.Mwrite.Person.lastname = "" ||
             mod_mother.Mwrite.Person.firstname = ""
          then
            (all_wl, all_ml, all_hr)
          else
            match Api_update_person.print_mod conf base mod_mother with
            | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
            | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
            | Api_update_util.UpdateErrorConflict c ->
              (* On dit que c'est le formulaire de la femme. *)
              c.Mwrite.Create_conflict.form <- Some `person_form2;
              raise (Api_update_util.ModErrApiConflict c)
        in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
    end
  with
  | Update.ModErrApi s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c


(* ************************************************************************ *)
(*  [Fonc] print_add_family_ok : config -> base -> ModificationStatus       *)
(** [Description] : Enregistre l'ajout d'une famille.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_add_family_ok conf base =
  let add_family_ok = get_params conf Mext_write.parse_add_family_ok in
  let ip = Int32.to_int add_family_ok.Mwrite.Add_family_ok.index_person in
  let mod_family = add_family_ok.Mwrite.Add_family_ok.family in
  let resp = compute_add_family_ok conf base mod_family in
  let ifam = Int32.to_int mod_family.Mwrite.Family.index in
  let data = compute_modification_status conf base ip ifam resp in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] print_mod_family_request : config -> base -> EditFamily          *)
(** [Description] :
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - EditFamily : les informations du template.
*)
(* ************************************************************************ *)
let print_mod_family_request conf base =
  let params = get_params conf Mext_write.parse_add_child_request in
  let ip = Int32.to_int params.Mwrite.Add_child_request.index in
  let ip = Adef.iper_of_int ip in
  let p = poi base ip in
  let spouses =
    Array.fold_right
      (fun ifam accu ->
         let cpl = foi base ifam in
         let isp = Gutil.spouse ip cpl in
         let sp = poi base isp in
         let index_family = Int32.of_int (Adef.int_of_ifam ifam) in
         let index_person = Int32.of_int (Adef.int_of_iper isp) in
         let sex =
           match get_sex sp with
           | Male -> `male
           | Female -> `female
           | Neuter -> `unknown
         in
         let lastname = sou base (get_surname sp) in
         let firstname = sou base (get_first_name sp) in
         let dates = Opt.of_string @@ Api_saisie_read.short_dates_text conf base sp in
         let image =
           Opt.of_string @@
           let img = sou base (get_image sp) in
           if img <> "" then img
           else if Api_util.find_image_file conf base sp <> None
           then "1"
           else ""
         in
         let sosa =
           let sosa_nb = Perso.get_single_sosa conf base sp in
           if Sosa.eq sosa_nb Sosa.zero then `no_sosa
           else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
           else `sosa
         in
         let family_spouse =
           {
             Mwrite.Family_spouse.index_family;
             index_person;
             sex;
             lastname;
             firstname;
             dates ;
             image ;
             sosa ;
           }
         in
         family_spouse :: accu)
      (get_family p) []
  in
  let first_family =
    match get_family p with
    | [||] -> None
    | families ->
      let ifam = Array.get families 0 in
      let fam = foi base ifam in
      let person_lastname = sou base (get_surname p) in
      let person_firstname = sou base (get_first_name p) in
      let family = Api_update_util.fam_to_piqi_mod_family conf base ifam fam in
      let (p_father, p_mother) =
        if get_sex p = Male then (p, poi base (Gutil.spouse ip fam))
        else (poi base (Gutil.spouse ip fam), p)
      in
      let father = Api_update_util.pers_to_piqi_mod_person conf base p_father in
      let mother = Api_update_util.pers_to_piqi_mod_person conf base p_mother in
      (* Mise à jour des parents dans la famille. *)
      family.Mwrite.Family.father <- father ;
      family.Mwrite.Family.mother <- mother ;
      Some { Mwrite.Edit_family.person_lastname ; person_firstname ; family }
  in
  print_result conf
    (Mext_write.gen_edit_family_request
       { Mwrite.Edit_family_request.spouses ; first_family })


(* ************************************************************************ *)
(*  [Fonc] print_mod_family : config -> base -> EditFamily                  *)
(** [Description] :
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - EditFamily : les informations du template.
                                                                           *)
(* ************************************************************************ *)
let print_mod_family conf base =
  let params = get_params conf Mext_write.parse_index_person_and_family in
  let ip = Int32.to_int params.Mwrite.Index_person_and_family.index_person in
  let ifam = Int32.to_int params.Mwrite.Index_person_and_family.index_family in
  let ip = Adef.iper_of_int ip in
  let ifam = Adef.ifam_of_int ifam in
  let p = poi base ip in
  let fam = foi base ifam in
  let surname = sou base (get_surname p) in
  let first_name = sou base (get_first_name p) in
  let family = Api_update_util.fam_to_piqi_mod_family conf base ifam fam in
  let (p_father, p_mother) =
    if get_sex p = Male then (p, poi base (Gutil.spouse ip fam))
    else (poi base (Gutil.spouse ip fam), p)
  in
  let father = Api_update_util.pers_to_piqi_mod_person conf base p_father in
  let mother = Api_update_util.pers_to_piqi_mod_person conf base p_mother in
  (* Mise à jour des parents dans la famille. *)
  let () =
    family.Mwrite.Family.father <- father;
    family.Mwrite.Family.mother <- mother;
  in
  let edit_family =
    {
      Mwrite.Edit_family.person_lastname = surname;
      person_firstname = first_name;
      family = family;
    }
  in
  let data = Mext_write.gen_edit_family edit_family in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] print_mod_family_ok : config -> base -> ModificationStatus       *)
(** [Description] : Enregistre l'ajout d'une famille.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_mod_family_ok conf base =
  let edit_family_ok = get_params conf Mext_write.parse_edit_family_ok in
  let ip = Int32.to_int edit_family_ok.Mwrite.Edit_family_ok.index_person in
  let mod_family = edit_family_ok.Mwrite.Edit_family_ok.family in
  let mod_father = mod_family.Mwrite.Family.father in
  let mod_mother = mod_family.Mwrite.Family.mother in
  (*
     On modifie une famille, il faut effectuer les actions suivantes :
       - modification du père => MOD_IND
       - modification de la mère => MOD_IND
       - modification de la famille => MOD_FAM
  *)
  let resp =
    try
      begin
        let (all_wl, all_ml, all_hr) =
          if mod_father.Mwrite.Person.lastname = "?" &&
             mod_father.Mwrite.Person.firstname = "?"
          then
          (* TODO
            raise (Update.ModErrApi "PersonKey")
          *)
            ([], [], [])
          else
            match Api_update_person.print_mod conf base mod_father with
            | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
            | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
            | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          if mod_mother.Mwrite.Person.lastname = "?" &&
             mod_mother.Mwrite.Person.firstname = "?"
          then
          (* TODO
            raise (Update.ModErrApi "PersonKey")
          *)
            (all_wl, all_ml, all_hr)
          else
            match Api_update_person.print_mod conf base mod_mother with
            | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
            | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
            | Api_update_util.UpdateErrorConflict c ->
                (* On dit que c'est le formulaire de la femme. *)
                c.Mwrite.Create_conflict.form <- Some `person_form2;
                raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          match Api_update_family.print_mod conf base ip mod_family with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
          | Api_update_util.UpdateError s ->
              raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
        in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
      end
    with
    | Update.ModErrApi s -> Api_update_util.UpdateError s
    | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
  in
  let ifam = Int32.to_int mod_family.Mwrite.Family.index in
  let data = compute_modification_status conf base ip ifam resp in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] print_add_parents : config -> base -> AddParents                 *)
(** [Description] : Renvoie les parents vides où on a calculé le nom pour
                    le père et le décès pour les parents, ainsi qu'une
                    famille vide.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - AddChild : les informations du template.
                                                                           *)
(* ************************************************************************ *)
let print_add_parents conf base =
  let params = get_params conf Mext_write.parse_index_person in
  let ip = Int32.to_int params.Mwrite.Index_person.index in
  let ip = Adef.iper_of_int ip in
  let p = poi base ip in
  let surname = sou base (get_surname p) in
  let first_name = sou base (get_first_name p) in
  let family =
    Api_update_util.piqi_empty_family conf base (Adef.ifam_of_int (-1))
  in
  let father = family.Mwrite.Family.father in
  let mother = family.Mwrite.Family.mother in
  (* On supprime le digest car on créé un enfant *)
  father.Mwrite.Person.digest <- "";
  mother.Mwrite.Person.digest <- "";
  (* Les index négatifs ne marchent pas ! *)
  family.Mwrite.Family.index <- Int32.of_int 0;
  father.Mwrite.Person.index <- Int32.of_int 0;
  mother.Mwrite.Person.index <- Int32.of_int 0;
  (* On met à jour la famille avec l'enfant. *)
  let child = Api_update_util.pers_to_piqi_person_link conf base p in
  family.Mwrite.Family.children <- [child];
  (* On met les parents en mode Create. *)
  father.Mwrite.Person.create_link <- `create_default_occ;
  mother.Mwrite.Person.create_link <- `create_default_occ;
  (* On met à jour les sexes. *)
  father.Mwrite.Person.sex <- `male;
  mother.Mwrite.Person.sex <- `female;
  (* On calcul si les parents sont décédés
     et ajoute les évènements nécessaires. *)
  let () =
    match infer_death conf base p with
    | `of_course_dead as x ->
      father.Mwrite.Person.death_type <- x ;
      mother.Mwrite.Person.death_type <- x ;
      father.Mwrite.Person.pevents <- father.Mwrite.Person.pevents @ [ empty_death_pevent () ];
      mother.Mwrite.Person.pevents <- mother.Mwrite.Person.pevents @ [ empty_death_pevent () ]
    | x ->
      father.Mwrite.Person.death_type <- x ;
      mother.Mwrite.Person.death_type <- x
  in
  (* On calcul le nom du père. *)
  let () =
    if get_sex p = Male then
      let father_surname = infer_surname conf base p None in
      father.Mwrite.Person.lastname <- father_surname;
    else
      father.Mwrite.Person.lastname <- surname;
  in
  let add_parents =
    {
      Mwrite.Add_parents.person_lastname = surname;
      person_firstname = first_name;
      family = family;
    }
  in
  let data = Mext_write.gen_add_parents add_parents in
  print_result conf data

let do_mod_fam_add_child conf base ifam ip mod_c =
    (*
       On modifie une famille, il faut effectuer les actions suivantes :
         - modification de la famille => MOD_FAM
         - modification de l'enfant => MOD_IND
    *)
  let child =
    { Mwrite.Person_link.create_link = mod_c.Mwrite.Person.create_link
    ; index = mod_c.Mwrite.Person.index
    ; sex = mod_c.Mwrite.Person.sex
    ; lastname = mod_c.Mwrite.Person.lastname
    ; firstname = mod_c.Mwrite.Person.firstname
    ; occ = mod_c.Mwrite.Person.occ (* Directement mis à jour dans update_family *)
    ; dates = None
    }
  in
  let fam = foi base (Adef.ifam_of_int ifam) in
  let mod_f =
    Api_update_util.fam_to_piqi_mod_family conf base (Adef.ifam_of_int ifam) fam
  in
  (* On ajoute le nouvel enfant. *)
  mod_f.Mwrite.Family.children <- mod_f.Mwrite.Family.children @ [ child ] ;
  let resp =
    try
      begin
        let (all_wl, all_ml, all_hr) =
          match Api_update_family.print_mod conf base ip mod_f with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
        in
        if (mod_c.Mwrite.Person.firstname = "?" || mod_c.Mwrite.Person.firstname = "") &&
           (mod_c.Mwrite.Person.lastname = "?" || mod_c.Mwrite.Person.lastname = "")
        then
          Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
        else
          (* On met à jour l'enfant et l'index ! *)
          let (all_wl, all_ml, all_hr) =
            let occ = Opt.map_default 0 Int32.to_int child.Mwrite.Person_link.occ in
            match person_of_key base mod_c.Mwrite.Person.firstname mod_c.Mwrite.Person.lastname occ with
            | Some ip_child ->
              mod_c.Mwrite.Person.index <- Int32.of_int (Adef.int_of_iper ip_child);
              mod_c.Mwrite.Person.occ <- child.Mwrite.Person_link.occ;
              (* On calcul le digest maintenant que l'enfant est créé. *)
              let child = poi base ip_child in
              let digest = Update.digest_person (UpdateInd.string_person_of base child) in
              mod_c.Mwrite.Person.digest <- digest;
              if mod_c.Mwrite.Person.death_type = `dont_know_if_dead
              then
                mod_c.Mwrite.Person.death_type <-
                  piqi_death_type_of_death (Update.infer_death_from_parents conf base fam) ;
              (match Api_update_person.print_mod conf base mod_c with
               | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
               | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
               | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c))
            | None -> failwith "ErrorAddChild"
          in
          Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
      end
    with
    | Update.ModErrApi s -> Api_update_util.UpdateError s
    | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
  in
  let data = compute_modification_status conf base ip ifam resp in
  print_result conf data

(* ************************************************************************ *)
(*  [Fonc] print_add_child_ok : config -> base -> ModificationStatus        *)
(** [Description] : Enregistre en base les informations envoyées.
      2 cas de figures :
        - ajout d'un conjoint et d'un enfant
        - ajout d'un enfant
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
(* FIXME: if and else could be factorized? *)
let print_add_child_ok_aux conf base add_child_ok =
  let ip = Int32.to_int add_child_ok.Mwrite.Add_child_ok.index_person in
  let ifam = Int32.to_int add_child_ok.Mwrite.Add_child_ok.index_family in
  let mod_c = add_child_ok.Mwrite.Add_child_ok.child in
  (* Le nouvel enfant à créer. *)
  let fn = mod_c.Mwrite.Person.firstname in
  let sn = mod_c.Mwrite.Person.lastname in
  let occ = mod_c.Mwrite.Person.occ in
  let create_child =
    {
      Mwrite.Person_link.create_link = mod_c.Mwrite.Person.create_link;
      index = mod_c.Mwrite.Person.index;
      sex = mod_c.Mwrite.Person.sex;
      lastname = sn;
      firstname = fn;
      occ = occ; (* Directement mis à jour dans update_family *)
      dates = None;
    }
  in
  if add_child_ok.Mwrite.Add_child_ok.new_family then
    begin
      (*
         On ajoute une famille, il faut effectuer les actions suivantes :
           - ajout d'une famille et de l'enfant => ADD_FAM
           - modification de l'enfant => MOD_IND
      *)
      let new_ifam = ref (-1) in
      let resp =
        try
          let p = poi base (Adef.iper_of_int ip) in
          let mod_f = compute_add_family conf base p in
          (* On ajoute le nouvel enfant. *)
          mod_f.Mwrite.Family.children <-
            mod_f.Mwrite.Family.children @ [create_child];
          (* On ajoute la famille : ADD_FAM *)
          let (all_wl, all_ml, all_hr) =
            match compute_add_family_ok conf base mod_f with
            | Api_update_util.UpdateSuccess (wl, ml, hr) ->
              (* On ajoute une famille donc l'ifam est nouveau *)
              let () = new_ifam := Int32.to_int mod_f.Mwrite.Family.index in
              (wl, ml, hr)
            | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
            | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
          in
          match fn, sn with
          | ("?" | ""), ("" | "?") -> Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
          | _ ->
            (* On met à jour l'enfant et l'index ! *)
            let (all_wl, all_ml, all_hr) =
              let occ = Opt.map_default 0 Int32.to_int create_child.Mwrite.Person_link.occ in
              match person_of_key base fn sn occ with
              | Some ip_child ->
                mod_c.Mwrite.Person.index <- Int32.of_int (Adef.int_of_iper ip_child);
                mod_c.Mwrite.Person.occ <- create_child.Mwrite.Person_link.occ;
                (* On calcul le digest maintenant que l'enfant est créé. *)
                let child = poi base ip_child in
                let digest = Update.digest_person (UpdateInd.string_person_of base child) in
                mod_c.Mwrite.Person.digest <- digest;
                if mod_c.Mwrite.Person.death_type = `dont_know_if_dead
                then
                  Opt.iter
                    (fun i ->
                       mod_c.Mwrite.Person.death_type <-
                         piqi_death_type_of_death (Update.infer_death_from_parents conf base @@ foi base i) )
                    (get_parents child) ;
                (match Api_update_person.print_mod conf base mod_c with
                 | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
                 | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
                 | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c))
              | None -> failwith "ErrorAddChildAndFamily"
            in
            Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
        with
        | Update.ModErrApi s -> Api_update_util.UpdateError s
        | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
      in
      let data = compute_modification_status conf base ip !new_ifam resp in
      print_result conf data
    end
  else
    do_mod_fam_add_child conf base ifam ip mod_c

let print_add_child_ok conf base =
  let add_child_ok = get_params conf Mext_write.parse_add_child_ok in
  print_add_child_ok_aux conf base add_child_ok

(* ************************************************************************ *)
(*  [Fonc] print_add_parents_ok : config -> base -> ModificationStatus      *)
(** [Description] : Enregistre les modifications de l'ajout de parents.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_add_parents_ok conf base =
  let add_parents_ok = get_params conf Mext_write.parse_add_parents_ok in
  let ip = Int32.to_int add_parents_ok.Mwrite.Add_parents_ok.index_person in
  let mod_family = add_parents_ok.Mwrite.Add_parents_ok.family in
  let mod_father = mod_family.Mwrite.Family.father in
  let mod_mother = mod_family.Mwrite.Family.mother in
  let existing_fam =
    if mod_father.Mwrite.Person.create_link = `link
    && mod_mother.Mwrite.Person.create_link = `link
    then
      let ifath = Adef.iper_of_int @@ Int32.to_int mod_father.Mwrite.Person.index in
      let imoth = Adef.iper_of_int @@ Int32.to_int mod_mother.Mwrite.Person.index in
      let families = get_family (poi base ifath) in
      let len = Array.length families in
      try
        (* Should test compatibility of events and set a warning flag if PossibleDuplicateFam *)
        let ifam =
          let rec loop i =
            if i = len then raise Not_found
            else
              let fam = foi base families.(i) in
              if (get_father fam = ifath && get_mother fam = imoth)
              then families.(i)
              else loop (i + 1)
          in
          loop 0
        in
        Some ifam
      with Not_found -> None
    else None
  in
  (* If both parents are linked, and no extra information is provided,
     we update an existing union (if exists) instead of creating a new one
     (aka add a child instead of add parents) *)
  match existing_fam with
  | Some ifam
    when begin match mod_family.Mwrite.Family.fevents with
      (* FIXME: really test events compatibilty instead of only handling default case *)
        [ { Mwrite.Fevent.fevent_type = Some `efam_marriage
          ; date
          ; place = None
          ; reason = None
          ; note = None
          ; src = None
          ; witnesses = []
          ; event_perso = None
          } ] ->
        begin match date with
          | None | Some  { Mwrite.Date.dmy = None ; text = None } -> true
          | _ -> false
        end
      | _ -> false
    end ->
    let add_child_ok =
      { Mwrite.Add_child_ok.index_person = add_parents_ok.Mwrite.Add_parents_ok.index_person
      ; index_family = Int32.of_int @@ Adef.int_of_ifam ifam
      ; new_family = false
      ; child = Api_update_util.pers_to_piqi_mod_person conf base @@ Gwdb.poi base @@ Adef.iper_of_int ip
      }
    in
    print_add_child_ok_aux conf base add_child_ok
  | _ ->
  (*
     On ajoute les parents, il faut effectuer les actions suivantes :
       - ajout de la famille => ADD_FAM
       - modification du père => MOD_IND
       - modification de la mère => MOD_IND
  *)
    let resp =
      try
        begin
          let (all_wl, all_ml, all_hr) =
            match
              Api_update_family.print_add
                conf base mod_family mod_father mod_mother
            with
            | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
            | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
            | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
          in
          (* Mise à jour des index et digest => fait dans Api_update_family.print_add *)
          let (all_wl, all_ml, all_hr) =
            match mod_father.Mwrite.Person.firstname, mod_father.Mwrite.Person.lastname with
            | ("?"|""), ("?"|"") ->
              (* TODO
                 raise (Update.ModErrApi "PersonKey")
              *)
              (all_wl, all_ml, all_hr)
            | _ ->
              match Api_update_person.print_mod conf base mod_father with
              | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
              | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
              | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
          in
          let (all_wl, all_ml, all_hr) =
            match mod_mother.Mwrite.Person.firstname, mod_mother.Mwrite.Person.lastname with
            | ("?"|""), ("?"|"") ->
              (* TODO
                 raise (Update.ModErrApi "PersonKey")
              *)
              (all_wl, all_ml, all_hr)
            | _ ->
              match Api_update_person.print_mod conf base mod_mother with
              | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
              | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
              | Api_update_util.UpdateErrorConflict c ->
                (* On dit que c'est le formulaire de la femme. *)
                c.Mwrite.Create_conflict.form <- Some `person_form2;
                raise (Api_update_util.ModErrApiConflict c)
          in
          let all_wl = match existing_fam with
            | Some ifam ->
              let ifam' = Adef.ifam_of_int @@ Int32.to_int mod_family.Mwrite.Family.index in
              Def.PossibleDuplicateFam (ifam, ifam') :: all_wl
            | _ -> all_wl
          in
          Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
        end
      with
      | Update.ModErrApi s -> Api_update_util.UpdateError s
      | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
    in
    let data = compute_modification_status conf base ip (-1) resp in
    print_result conf data

(* ************************************************************************ *)
(*  [Fonc] print_add_child : config -> base -> AddChild                     *)
(** [Description] : Renvoie un enfant vide pour lequel on calcul son nom
                    et s'il est potentiellement décédé ou pas.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - AddChild : les informations du template.
                                                                           *)
(* ************************************************************************ *)
let print_add_child conf base =
  let params = get_params conf Mext_write.parse_add_child_request in
  let ip = Int32.to_int params.Mwrite.Add_child_request.index in
  let ifam = params.Mwrite.Add_child_request.index_family in
  let ip = Adef.iper_of_int ip in
  let p = poi base ip in
  let family_spouse =
    List.fold_right
      (fun ifam accu ->
         let cpl = foi base ifam in
         let isp = Gutil.spouse ip cpl in
         let sp = poi base isp in
         let index_family = Int32.of_int (Adef.int_of_ifam ifam) in
         let index_person = Int32.of_int (Adef.int_of_iper isp) in
         let sex =
           match get_sex sp with
           | Male -> `male
           | Female -> `female
           | Neuter -> `unknown
         in
         let surname = sou base (get_surname sp) in
         let first_name = sou base (get_first_name sp) in
         let dates = Api_saisie_read.short_dates_text conf base sp in
         let image =
           let img = sou base (get_image sp) in
           if img <> "" then img
           else if Api_util.find_image_file conf base sp <> None
           then "1"
           else ""
         in
         let sosa =
           let sosa_nb = Perso.get_single_sosa conf base sp in
           if Sosa.eq sosa_nb Sosa.zero then `no_sosa
           else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
           else `sosa
         in
         let family_spouse =
           {
             Mwrite.Family_spouse.index_family = index_family;
             index_person = index_person;
             sex = sex;
             lastname = surname;
             firstname = first_name;
             dates = if dates = "" then None else Some dates;
             image = if image = "" then None else Some image;
             sosa = sosa;
           }
         in
         family_spouse :: accu)
      (Array.to_list (get_family p)) []
  in
  let surname = sou base (get_surname p) in
  let first_name = sou base (get_first_name p) in
  let empty_child = Gwdb.empty_person base (Adef.iper_of_int (-1)) in
  let child = Api_update_util.pers_to_piqi_mod_person conf base empty_child in
  (* On supprime le digest car on créé un enfant *)
  child.Mwrite.Person.digest <- "";
  (* Les index négatifs ne marchent pas ! *)
  child.Mwrite.Person.index <- Int32.of_int 0;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  child.Mwrite.Person.access <- `access_iftitles;
  (* On met l'enfant en mode Create. *)
  child.Mwrite.Person.create_link <- `create_default_occ;
  (* On met à jour le sex *)
  let () =
    match params.Mwrite.Add_child_request.sex with
    | Some sex -> child.Mwrite.Person.sex <- sex
    | None -> ()
  in
  (* On calcul si l'enfant est décédé. *)
  let () =
    match infer_death conf base p with
    | `of_course_dead ->
      child.Mwrite.Person.death_type <- `dont_know_if_dead;
      child.Mwrite.Person.pevents <- child.Mwrite.Person.pevents @ [ empty_death_pevent () ];
    | x -> child.Mwrite.Person.death_type <- x
  in
  (* On prend le nom du père *)
  let child_surname = infer_surname conf base p ifam in
  child.Mwrite.Person.lastname <- child_surname;
  let add_child =
    Mwrite.Add_child.({
      person_lastname = surname;
      person_firstname = first_name;
      family_spouse = family_spouse;
      child = child;
    })
  in
  let data = Mext_write.gen_add_child add_child in
  print_result conf data


(*
(* ************************************************************************ *)
(*  [Fonc] print_add_child_ok : config -> base -> ModificationStatus        *)
(** [Description] : Enregistre en base les informations envoyées.
      2 cas de figures :
        - ajout d'un conjoint et d'un enfant
        - ajout d'un enfant
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_add_child_ok conf base =
  let add_child_ok = get_params conf Mext_write.parse_add_child_ok in
  let ip = Int32.to_int add_child_ok.Mwrite.Add_child_ok.index_person in
  let ifam = Int32.to_int add_child_ok.Mwrite.Add_child_ok.index_family in
  let mod_c = add_child_ok.Mwrite.Add_child_ok.child in
  if add_child_ok.Mwrite.Add_child_ok.new_family then
    let resp = Api_update_family.print_add_child_and_family conf base ip ifam mod_c in
    (* TODO ifam *)
    let data = compute_modification_status conf base ip (-1) resp in
    print_result conf data
  else
    let resp = Api_update_family.print_add_child conf base ip ifam mod_c in
    let data = compute_modification_status conf base ip ifam resp in
    print_result conf data
*)


(* ************************************************************************ *)
(*  [Fonc] print_add_sibling : config -> base -> AddSibling                 *)
(** [Description] : Renvoie un frère/sœur vide pour lequel on calcul son
                    nom et s'il est potentiellement décédé ou pas.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - AddSibling : les informations du template.
                                                                           *)
(* ************************************************************************ *)
let print_add_sibling conf base =
  let params = get_params conf Mext_write.parse_add_sibling_request in
  let ip = Int32.to_int params.Mwrite.Add_sibling_request.index in
  let ip = Adef.iper_of_int ip in
  let p = poi base ip in
  let father =
    Opt.map (fun ifam -> poi base @@ get_father @@ foi base ifam) (get_parents p)
  in
  let surname = sou base (get_surname p) in
  let first_name = sou base (get_first_name p) in
  let empty_sibling = Gwdb.empty_person base (Adef.iper_of_int (-1)) in
  let sibling = Api_update_util.pers_to_piqi_mod_person conf base empty_sibling in
  (* On supprime le digest car on créé un enfant *)
  sibling.Mwrite.Person.digest <- "";
  (* Les index négatifs ne marchent pas ! *)
  sibling.Mwrite.Person.index <- Int32.of_int 0;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  sibling.Mwrite.Person.access <- `access_iftitles;
  (* On met le frère/soeur en mode Create. *)
  sibling.Mwrite.Person.create_link <- `create_default_occ;
  (* On met à jour le sex *)
  Opt.iter (fun s -> sibling.Mwrite.Person.sex <- s) params.Mwrite.Add_sibling_request.sex ;
  (* On calcul si l'enfant est décédé. *)
  let () =
    match father with
    | Some father ->
      begin match infer_death conf base father with
        | `of_course_dead ->
          sibling.Mwrite.Person.death_type <- `of_course_dead ;
          sibling.Mwrite.Person.pevents <- sibling.Mwrite.Person.pevents @ [ empty_death_pevent () ] ;
        | x -> sibling.Mwrite.Person.death_type <- x
      end
    | None -> sibling.Mwrite.Person.death_type <- `not_dead
  in
  (* On prend le nom du père *)
  let sibling_surname =
    match father with
    | Some father -> infer_surname conf base father None
    | None -> surname
  in
  sibling.Mwrite.Person.lastname <- sibling_surname;
  let add_sibling =
    Mwrite.Add_sibling.({
      person_lastname = surname;
      person_firstname = first_name;
      sibling = sibling;
    })
  in
  let data = Mext_write.gen_add_sibling add_sibling in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] print_add_sibling_ok : config -> base -> ModificationStatus      *)
(** [Description] : Enregistre en base les informations envoyées.
      2 cas de figures :
        - ajout d'un conjoint et d'un enfant
        - ajout d'un enfant
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_add_sibling_ok conf base =
  let add_sibling_ok = get_params conf Mext_write.parse_add_sibling_ok in
  let ip = Int32.to_int add_sibling_ok.Mwrite.Add_sibling_ok.index_person in
  let mod_c = add_sibling_ok.Mwrite.Add_sibling_ok.sibling in
  let p = poi base (Adef.iper_of_int ip) in
  (* Le nouvel enfant à créer. *)
  let fn = mod_c.Mwrite.Person.firstname in
  let sn = mod_c.Mwrite.Person.lastname in
  let occ = mod_c.Mwrite.Person.occ in
  let create_sibling =
    {
      Mwrite.Person_link.create_link = mod_c.Mwrite.Person.create_link;
      index = mod_c.Mwrite.Person.index;
      sex = mod_c.Mwrite.Person.sex;
      lastname = sn;
      firstname = fn;
      occ = occ; (* Directement mis à jour dans update_family *)
      dates = None;
    }
  in
  (* Si il n'y a pas de parent, on veut créer la famille *)
  match get_parents p with
  | None ->
      begin
        (*
           On ajoute une famille, il faut effectuer les actions suivantes :
             - ajout d'une famille et de l'enfant => ADD_FAM
             - modification de l'enfant => MOD_IND
        *)
        let new_ifam = ref (-1) in
        let resp =
          try
            (* TODO compute add_parents. *)
            let family =
              Api_update_util.piqi_empty_family conf base (Adef.ifam_of_int (-1))
            in
            let father = family.Mwrite.Family.father in
            let mother = family.Mwrite.Family.mother in
            (* On supprime le digest car on créé un enfant *)
            father.Mwrite.Person.digest <- "";
            mother.Mwrite.Person.digest <- "";
            (* Les index négatifs ne marchent pas ! *)
            family.Mwrite.Family.index <- Int32.of_int 0;
            father.Mwrite.Person.index <- Int32.of_int 0;
            mother.Mwrite.Person.index <- Int32.of_int 0;
            (* On met à jour la famille avec l'enfant. *)
            let child = Api_update_util.pers_to_piqi_person_link conf base p in
            family.Mwrite.Family.children <- [child; create_sibling];
            (* On met les parents en mode Create. *)
            father.Mwrite.Person.create_link <- `create_default_occ;
            mother.Mwrite.Person.create_link <- `create_default_occ;
            (* On met à jour les sexes. *)
            father.Mwrite.Person.sex <- `male;
            mother.Mwrite.Person.sex <- `female;
            (* On ajoute la famille : ADD_FAM *)
            let (all_wl, all_ml, all_hr) =
              match
                Api_update_family.print_add conf base family father mother
              with
              | Api_update_util.UpdateSuccess (wl, ml, hr) ->
                  (* On ajoute une famille donc l'ifam est nouveau *)
                  let () = new_ifam := Int32.to_int family.Mwrite.Family.index in
                  (wl, ml, hr)
              | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
              | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
            in
            if (fn = "?" || fn = "") &&
               (sn = "?" || sn = "")
            then
              Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
            else
              (* On met à jour l'enfant et l'index ! *)
              let (all_wl, all_ml, all_hr) =
                let occ =
                  match create_sibling.Mwrite.Person_link.occ with
                  | None -> 0
                  | Some occ -> Int32.to_int occ
                in
                match person_of_key base fn sn occ with
                | Some ip_sibling ->
                    mod_c.Mwrite.Person.index <- Int32.of_int (Adef.int_of_iper ip_sibling);
                    mod_c.Mwrite.Person.occ <- create_sibling.Mwrite.Person_link.occ;
                    (* On calcul le digest maintenant que l'enfant est créé. *)
                    let sibling = poi base ip_sibling in
                    let digest = Update.digest_person (UpdateInd.string_person_of base sibling) in
                    mod_c.Mwrite.Person.digest <- digest;
                    (match Api_update_person.print_mod conf base mod_c with
                    | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
                    | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
                    | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c))
                | None -> failwith "ErrorAddSiblingAndFamily"
              in
              Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
          with
          | Update.ModErrApi s -> Api_update_util.UpdateError s
          | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
        in
        let data = compute_modification_status conf base ip !new_ifam resp in
        print_result conf data
      end
  | Some ifam ->
      (*
         On modifie une famille, il faut effectuer les actions suivantes :
           - modification de la famille => MOD_FAM
           - modification de l'enfant => MOD_IND
      *)
      begin
        let fam = foi base ifam in
        let mod_f =
          Api_update_util.fam_to_piqi_mod_family conf base ifam fam
        in
        (* On ajoute le nouvel enfant. *)
        mod_f.Mwrite.Family.children <-
          mod_f.Mwrite.Family.children @ [create_sibling];
        let resp =
          try
            begin
              let (all_wl, all_ml, all_hr) =
                match Api_update_family.print_mod conf base ip mod_f with
                | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
                | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
                | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
              in
              if (fn = "?" || fn = "") &&
                 (sn = "?" || sn = "")
              then
                Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
              else
                (* On met à jour l'enfant et l'index ! *)
                let (all_wl, all_ml, all_hr) =
                  let occ =
                    match create_sibling.Mwrite.Person_link.occ with
                    | None -> 0
                    | Some occ -> Int32.to_int occ
                  in
                  match person_of_key base fn sn occ with
                  | Some ip_sibling ->
                      mod_c.Mwrite.Person.index <- Int32.of_int (Adef.int_of_iper ip_sibling);
                      mod_c.Mwrite.Person.occ <- create_sibling.Mwrite.Person_link.occ;
                      (* On calcul le digest maintenant que l'enfant est créé. *)
                      let sibling = poi base ip_sibling in
                      let digest = Update.digest_person (UpdateInd.string_person_of base sibling) in
                      mod_c.Mwrite.Person.digest <- digest;
                      (match Api_update_person.print_mod conf base mod_c with
                       | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
                       | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
                       | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c))
                  | None -> failwith "ErrorAddSibling"
                in
                Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
            end
          with
          | Update.ModErrApi s -> Api_update_util.UpdateError s
          | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
        in
        let data = compute_modification_status conf base ip (Adef.int_of_ifam ifam) resp in
        print_result conf data
      end


(**/**) (* Fonctions pour la première saisie. *)


(* ************************************************************************ *)
(*  [Fonc] check_input_person : config -> Person -> unit                    *)
(** [Description] : Cette fonction vérifie que les champs obligatoire sont
                    bien renseignés.
    [Args] :
      - conf  : configuration de la base
      - mod_p : person piqi
    [Retour] : Néant
                                                                           *)
(* ************************************************************************ *)
let check_input_person conf mod_p =
  let designation () =
    let occ =
      match mod_p.Mwrite.Person.occ with
      | Some i -> Int32.to_int i
      | None -> 0
    in
    mod_p.Mwrite.Person.firstname ^ "." ^ string_of_int occ ^
      " " ^ mod_p.Mwrite.Person.lastname
  in
  let () =
    if mod_p.Mwrite.Person.lastname = "" &&
       mod_p.Mwrite.Person.firstname = ""
    then
      List.fold_right
        (fun evt () ->
          (match evt.Mwrite.Pevent.date with
          | Some date ->
              (match date.Mwrite.Date.dmy with
              | Some dmy ->
                  (match
                     (dmy.Mwrite.Dmy.year,
                      dmy.Mwrite.Dmy.month,
                      dmy.Mwrite.Dmy.day)
                   with
                   | (None, None, None) -> ()
                   | _ ->
                       let err =
                         transl conf "unknown person" ^ ": " ^ designation ()
                       in
                       raise (Update.ModErrApi err))
              | None -> ())
          | None -> ()))
        mod_p.Mwrite.Person.pevents ()
    else if mod_p.Mwrite.Person.lastname <> "" &&
            mod_p.Mwrite.Person.firstname <> ""
    then
      if mod_p.Mwrite.Person.sex = `unknown then
        let err =
          Printf.sprintf
            (ftransl conf "undefined sex for %t") (fun _ -> designation ())
        in
        raise (Update.ModErrApi err)
      else ()
    else
      let err =
        if mod_p.Mwrite.Person.lastname = "" then
          transl conf "surname missing" ^ ": " ^ designation ()
        else
          transl conf "first name missing" ^ ": " ^ designation ()
      in
      raise (Update.ModErrApi err)
  in
  ()


(* ************************************************************************ *)
(*  [Fonc] compute_add_first_fam : config ->
                                     (AddFirstFam, ModificationStatus)      *)
(** [Description] : Permet de vérifier qu'à partir de l'objet AddFirstFam,
      la saisie va bien se passer. C'est elle qui calcul les occ des
      homonymes pour ne pas avoir de conflit et vérifie que les champs
      requis sont bien renseignés.
    [Args] :
      - conf : configuration de la base
    [Retour] :
      - AddFirstFam, ModificationStatus :
         L'objet AddFirstFam modifié afain de ne pas avoir de conflit de
         occ et le status de la réponse si l'utilisateur a fait une mauvaise
         saisie.
                                                                           *)
(* ************************************************************************ *)
let compute_add_first_fam conf =
  let add_first_fam = get_params conf Mext_write.parse_add_first_fam in

  (* On ré-initialise un certain nombre de valeurs. *)
  add_first_fam.Mwrite.Add_first_fam.sosa.Mwrite.Person.digest <- "";
  add_first_fam.Mwrite.Add_first_fam.sosa.Mwrite.Person.create_link <- `create_default_occ;
  add_first_fam.Mwrite.Add_first_fam.sosa.Mwrite.Person.index <- Int32.of_int 0;
  add_first_fam.Mwrite.Add_first_fam.sosa.Mwrite.Person.occ <- None;
  add_first_fam.Mwrite.Add_first_fam.sosa.Mwrite.Person.access <- `access_iftitles;

  add_first_fam.Mwrite.Add_first_fam.father.Mwrite.Person.digest <- "";
  add_first_fam.Mwrite.Add_first_fam.father.Mwrite.Person.create_link <- `create_default_occ;
  add_first_fam.Mwrite.Add_first_fam.father.Mwrite.Person.index <- Int32.of_int 0;
  (* On n'autorise pas les parents de meme sexe. *)
  add_first_fam.Mwrite.Add_first_fam.father.Mwrite.Person.sex <- `male;
  add_first_fam.Mwrite.Add_first_fam.father.Mwrite.Person.occ <- None;
  add_first_fam.Mwrite.Add_first_fam.father.Mwrite.Person.access <- `access_iftitles;

  add_first_fam.Mwrite.Add_first_fam.mother.Mwrite.Person.digest <- "";
  add_first_fam.Mwrite.Add_first_fam.mother.Mwrite.Person.create_link <- `create_default_occ;
  add_first_fam.Mwrite.Add_first_fam.mother.Mwrite.Person.index <- Int32.of_int 0;
  (* On n'autorise pas les parents de meme sexe. *)
  add_first_fam.Mwrite.Add_first_fam.mother.Mwrite.Person.sex <- `female;
  add_first_fam.Mwrite.Add_first_fam.mother.Mwrite.Person.occ <- None;
  add_first_fam.Mwrite.Add_first_fam.mother.Mwrite.Person.access <- `access_iftitles;

  add_first_fam.Mwrite.Add_first_fam.spouse.Mwrite.Person.digest <- "";
  add_first_fam.Mwrite.Add_first_fam.spouse.Mwrite.Person.create_link <- `create_default_occ;
  add_first_fam.Mwrite.Add_first_fam.spouse.Mwrite.Person.index <- Int32.of_int 0;
  add_first_fam.Mwrite.Add_first_fam.spouse.Mwrite.Person.occ <- None;
  add_first_fam.Mwrite.Add_first_fam.spouse.Mwrite.Person.access <- `access_iftitles;

  (* On strip aussi les enfants. *)
  add_first_fam.Mwrite.Add_first_fam.children <-
    List.fold_right
      (fun mod_c accu ->
        if mod_c.Mwrite.Person.lastname = "" &&
           mod_c.Mwrite.Person.firstname = ""
        then accu
        else
          begin
            mod_c.Mwrite.Person.digest <- "";
            mod_c.Mwrite.Person.create_link <- `create_default_occ;
            mod_c.Mwrite.Person.index <- Int32.of_int 0;
            mod_c.Mwrite.Person.occ <- None;
            mod_c.Mwrite.Person.access <- `access_iftitles;
            mod_c :: accu
          end)
      add_first_fam.Mwrite.Add_first_fam.children [];

  let mod_p = add_first_fam.Mwrite.Add_first_fam.sosa in
  let mod_father = add_first_fam.Mwrite.Add_first_fam.father in
  let mod_mother = add_first_fam.Mwrite.Add_first_fam.mother in
  let mod_spouse = add_first_fam.Mwrite.Add_first_fam.spouse in
  let mod_children = add_first_fam.Mwrite.Add_first_fam.children in

  (* On vérifie toutes les erreurs de saisie possibles. *)
  (* Attention, il faut envoyer dans le même ordre que pour la saisie, *)
  (* sinon les occurrences seront toutes changées !!!                  *)
  let resp =
    try
      begin
        (* On vérifie qu'il n'y a pas de problème de nom/prénom. *)
        check_input_person conf mod_p;
        check_input_person conf mod_father;
        check_input_person conf mod_mother;
        check_input_person conf mod_spouse;
        List.iter (check_input_person conf) mod_children;

        let (all_wl, all_ml, all_hr) =
          match Api_update_person.print_add_nobase conf mod_father with
          | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          match Api_update_person.print_add_nobase conf mod_mother with
          | Api_update_util.UpdateSuccess (wl, ml, hr) ->
              (all_wl @ wl, all_ml @ ml, all_hr @ hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          match Api_update_person.print_add_nobase conf mod_p with
          | Api_update_util.UpdateSuccess (wl, ml, hr) ->
              (all_wl @ wl, all_ml @ ml, all_hr @ hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          match Api_update_person.print_add_nobase conf mod_spouse with
          | Api_update_util.UpdateSuccess (wl, ml, hr) ->
              (all_wl @ wl, all_ml @ ml, all_hr @ hr)
          | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
          | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr) =
          List.fold_left
            (fun (all_wl, all_ml, all_hr) mod_child ->
              match Api_update_person.print_add_nobase conf mod_child with
              | Api_update_util.UpdateSuccess (wl, ml, hr) ->
                  (all_wl @ wl, all_ml @ ml, all_hr @ hr)
              | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
              | Api_update_util.UpdateErrorConflict c ->
                  raise (Api_update_util.ModErrApiConflict c))
            (all_wl, all_ml, all_hr) mod_children
        in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
      end
    with
    | Update.ModErrApi s -> Api_update_util.UpdateError s
    | Api_update_util.ModErrApiConflict c ->
        Api_update_util.UpdateErrorConflict c
  in
  (add_first_fam, resp)


(* ************************************************************************ *)
(*  [Fonc] print_add_first_fam : config -> ModificationStatus               *)
(** [Description] : Simule l'ajout de la première saisie mais sans
      l'existence d'une base GeneWeb.
    [Args] :
      - conf : configuration de la base
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_add_first_fam conf =
  let (add_first_fam, resp) = compute_add_first_fam conf in
  let mod_p = add_first_fam.Mwrite.Add_first_fam.sosa in

  (* compute_modification_status si pas de base, on ne peut *)
  (* pas avoir ni warnings, ni modification d'historique.   *)
  let (surname, first_name, occ, index_person, surname_str, first_name_str) =
    (mod_p.Mwrite.Person.lastname,
     mod_p.Mwrite.Person.firstname,
     mod_p.Mwrite.Person.occ,
     None,
     Some mod_p.Mwrite.Person.lastname,
     Some mod_p.Mwrite.Person.firstname)
  in
  let sn = if surname = "" then None else Some (Name.lower surname) in
  let fn = if first_name = "" then None else Some (Name.lower first_name) in
  let index_family = None in
  let (is_base_updated, warnings, miscs, conflict, _) =
    match resp with
    | Api_update_util.UpdateErrorConflict c -> (false, [], [], Some c, [])
    | Api_update_util.UpdateError s -> (false, [s], [], None, [])
    | Api_update_util.UpdateSuccess _ -> (true, [], [], None, [])
  in
  let response =
    {
      Mwrite.Modification_status.is_base_updated = is_base_updated;
      base_warnings = warnings;
      base_miscs = miscs;
      index_person = index_person;
      lastname = surname;
      firstname = first_name;
      occ = occ;
      index_family = index_family;
      conflict = conflict;
      lastname_str = surname_str;
      firstname_str = first_name_str;
      n = sn;
      p = fn;
    }
  in
  let data = Mext_write.gen_modification_status response in
  print_result conf data


(* ************************************************************************ *)
(*  [Fonc] print_add_first_fam_ok : config -> base -> ModificationStatus    *)
(** [Description] : Enregistre en base les informations envoyées.
      On reconstitue toutes les liaisons à la main pour pouvoir les
      enregistrer en base, i.e. ascendants de la personne (famille) et
      descendants de la personne (famille), et éventuelle modification des
      personnes ensuite.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
(* ************************************************************************ *)
let print_add_first_fam_ok conf base =
  let (add_first_fam, _) = compute_add_first_fam conf in
  let mod_p = add_first_fam.Mwrite.Add_first_fam.sosa in
  let mod_father = add_first_fam.Mwrite.Add_first_fam.father in
  let mod_mother = add_first_fam.Mwrite.Add_first_fam.mother in
  let mod_spouse = add_first_fam.Mwrite.Add_first_fam.spouse in
  let mod_children = add_first_fam.Mwrite.Add_first_fam.children in

  (* Pour l'instant, on a pas d'ip. *)
  let ip = ref (-1) in
  let ifam = ref (-1) in

  let resp =
    try
      begin
        (* On crée la famille avec les parents. *)
        let fam_asc =
          let family =
            Api_update_util.piqi_empty_family conf base (Adef.ifam_of_int (-1))
          in
          (* On ré-initialise un certain nombre de valeurs, *)
          (* surtout si c'est des personnes vides.          *)
          family.Mwrite.Family.father.Mwrite.Person.digest <- "";
          family.Mwrite.Family.father.Mwrite.Person.create_link <- `create_default_occ;
          family.Mwrite.Family.father.Mwrite.Person.index <- Int32.of_int 0;
          family.Mwrite.Family.father.Mwrite.Person.occ <- None;
          family.Mwrite.Family.father.Mwrite.Person.access <- `access_iftitles;

          family.Mwrite.Family.mother.Mwrite.Person.digest <- "";
          family.Mwrite.Family.mother.Mwrite.Person.create_link <- `create_default_occ;
          family.Mwrite.Family.mother.Mwrite.Person.index <- Int32.of_int 0;
          family.Mwrite.Family.mother.Mwrite.Person.occ <- None;
          family.Mwrite.Family.mother.Mwrite.Person.access <- `access_iftitles;

          (* On remplace les parents. *)
          if mod_father.Mwrite.Person.lastname = "" &&
             mod_father.Mwrite.Person.firstname = ""
          then ()
          else family.Mwrite.Family.father <- mod_father;
          if mod_mother.Mwrite.Person.lastname = "" &&
             mod_mother.Mwrite.Person.firstname = ""
          then ()
          else family.Mwrite.Family.mother <- mod_mother;
          (* Les index négatifs ne marchent pas ! *)
          family.Mwrite.Family.index <- Int32.of_int 0;
          family.Mwrite.Family.father.Mwrite.Person.index <- Int32.of_int 0;
          family.Mwrite.Family.mother.Mwrite.Person.index <- Int32.of_int 0;
          (* On met à jour les sexes. *)
          family.Mwrite.Family.father.Mwrite.Person.sex <- `male;
          family.Mwrite.Family.mother.Mwrite.Person.sex <- `female;
          (* On met à jour la famille avec l'enfant. *)
          let child =
            {
              Mwrite.Person_link.create_link = `create_default_occ;
              index = mod_p.Mwrite.Person.index;
              sex = mod_p.Mwrite.Person.sex;
              lastname = mod_p.Mwrite.Person.lastname;
              firstname = mod_p.Mwrite.Person.firstname;
              occ = mod_p.Mwrite.Person.occ;
              dates = None;
            }
          in
          family.Mwrite.Family.children <- [child];
          family
        in

        (* Ajout de fam_asc. *)
        let (all_wl, all_ml, all_hr) =
          (* La personne n'a pas de parents. *)
          if (fam_asc.Mwrite.Family.father.Mwrite.Person.firstname = "" &&
              fam_asc.Mwrite.Family.father.Mwrite.Person.lastname = "" &&
              fam_asc.Mwrite.Family.mother.Mwrite.Person.firstname = "" &&
              fam_asc.Mwrite.Family.mother.Mwrite.Person.lastname = "")
          then
            let (all_wl, all_ml, all_hr) =
              match Api_update_person.print_add conf base mod_p with
              | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
              | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
              | Api_update_util.UpdateErrorConflict c ->
                  raise (Api_update_util.ModErrApiConflict c)
            in
            (* On met à jour l'index. *)
            let () =
              let (sn, fn) =
                (mod_p.Mwrite.Person.lastname,
                 mod_p.Mwrite.Person.firstname)
              in
              let occ =
                match mod_p.Mwrite.Person.occ with
                | None -> 0
                | Some occ -> Int32.to_int occ
              in
              match person_of_key base fn sn occ with
              | Some ip ->
                  mod_p.Mwrite.Person.index <- Int32.of_int (Adef.int_of_iper ip)
              | None -> failwith "ErrorAddFirstFamNoChildFound"
            in
            (all_wl, all_ml, all_hr)
          else
            (* On ajoute la famille avec les parents. *)
            let (all_wl, all_ml, all_hr) =
              match compute_add_family_ok conf base fam_asc with
              | Api_update_util.UpdateSuccess (wl, ml, hr) -> (wl, ml, hr)
              | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
              | Api_update_util.UpdateErrorConflict c ->
                  raise (Api_update_util.ModErrApiConflict c)
            in
            (* On modifie la personne "principale". *)
            let (all_wl, all_ml, all_hr) =
              match fam_asc.Mwrite.Family.children with
              | [create_child] ->
                  let (sn, fn) =
                    (mod_p.Mwrite.Person.lastname,
                     mod_p.Mwrite.Person.firstname)
                  in
                  let occ =
                    match create_child.Mwrite.Person_link.occ with
                    | None -> 0
                    | Some occ -> Int32.to_int occ
                  in
                  (match person_of_key base fn sn occ with
                  | Some ip_child ->
                      mod_p.Mwrite.Person.index <-
                        Int32.of_int (Adef.int_of_iper ip_child);
                      mod_p.Mwrite.Person.occ <-
                        create_child.Mwrite.Person_link.occ;
                      (* On calcul le digest maintenant que l'enfant est créé. *)
                      let child = poi base ip_child in
                      let digest =
                        Update.digest_person (UpdateInd.string_person_of base child)
                      in
                      mod_p.Mwrite.Person.digest <- digest;
                      (match Api_update_person.print_mod conf base mod_p with
                      | Api_update_util.UpdateSuccess (wl, ml, hr) ->
                          (all_wl @ wl, all_ml @ ml, all_hr @ hr)
                      | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
                      | Api_update_util.UpdateErrorConflict c ->
                          raise (Api_update_util.ModErrApiConflict c))
                  | None -> failwith "ErrorAddFirstFamNoChildFound")
              | _ -> failwith "ErrorAddFirstFamNoChild"
            in
            (all_wl, all_ml, all_hr)
        in

        (* Normalement, on a réussi à mettre à jour l'ip de la personne. *)
        let () = ip := Int32.to_int mod_p.Mwrite.Person.index in
        let () = ifam := Int32.to_int fam_asc.Mwrite.Family.index in

        (* On crée la famille avec les enfants. *)
        let fam_desc =
          let family =
            Api_update_util.piqi_empty_family conf base (Adef.ifam_of_int (-1))
          in
          (* On ré-initialise un certain nombre de valeurs, *)
          (* surtout si c'est des personnes vides.          *)
          family.Mwrite.Family.father.Mwrite.Person.digest <- "";
          family.Mwrite.Family.father.Mwrite.Person.create_link <- `create_default_occ;
          family.Mwrite.Family.father.Mwrite.Person.index <- Int32.of_int 0;
          family.Mwrite.Family.father.Mwrite.Person.occ <- None;
          family.Mwrite.Family.father.Mwrite.Person.access <- `access_iftitles;

          family.Mwrite.Family.mother.Mwrite.Person.digest <- "";
          family.Mwrite.Family.mother.Mwrite.Person.create_link <- `create_default_occ;
          family.Mwrite.Family.mother.Mwrite.Person.index <- Int32.of_int 0;
          family.Mwrite.Family.mother.Mwrite.Person.occ <- None;
          family.Mwrite.Family.mother.Mwrite.Person.access <- `access_iftitles;

          (* On remplace les parents. *)
          if mod_p.Mwrite.Person.sex = `male then
            begin
              family.Mwrite.Family.father <- mod_p;
              if mod_spouse.Mwrite.Person.lastname = "" &&
                 mod_spouse.Mwrite.Person.firstname = ""
              then ()
              else family.Mwrite.Family.mother <- mod_spouse;
            end
          else
            begin
              if mod_spouse.Mwrite.Person.lastname = "" &&
                 mod_spouse.Mwrite.Person.firstname = ""
              then ()
              else family.Mwrite.Family.father <- mod_spouse;
              family.Mwrite.Family.mother <- mod_p;
            end;
          (* Les index négatifs ne marchent pas ! *)
          family.Mwrite.Family.index <- Int32.of_int 0;
          (* On n'autorise pas les parents de meme sexe. *)
          (* On met les parents en mode Create. *)
          if mod_p.Mwrite.Person.sex = `male then
            begin
              family.Mwrite.Family.father.Mwrite.Person.create_link <- `link;
              let p = poi base (Adef.iper_of_int !ip) in
              let digest = Update.digest_person (UpdateInd.string_person_of base p) in
              family.Mwrite.Family.father.Mwrite.Person.digest <- digest;
              family.Mwrite.Family.mother.Mwrite.Person.create_link <- `create_default_occ;
              family.Mwrite.Family.mother.Mwrite.Person.index <- Int32.of_int 0;
            end
          else
            begin
              family.Mwrite.Family.father.Mwrite.Person.create_link <- `create_default_occ;
              family.Mwrite.Family.father.Mwrite.Person.index <- Int32.of_int 0;
              family.Mwrite.Family.mother.Mwrite.Person.create_link <- `link;
              let p = poi base (Adef.iper_of_int !ip) in
              let digest = Update.digest_person (UpdateInd.string_person_of base p) in
              family.Mwrite.Family.mother.Mwrite.Person.digest <- digest;
            end;
          (* On met à jour la famille avec les enfants. *)
          let children =
            List.map
              (fun c ->
                {
                  Mwrite.Person_link.create_link = `create_default_occ;
                  index = c.Mwrite.Person.index;
                  sex = c.Mwrite.Person.sex;
                  lastname = c.Mwrite.Person.lastname;
                  firstname = c.Mwrite.Person.firstname;
                  occ = c.Mwrite.Person.occ;
                  dates = None;
                })
              mod_children
          in
          family.Mwrite.Family.children <- children;
          family
        in

        (* On ajoute la famille avec les enfants. *)
        (* S'il n'y a pas de descendance, on ne fait pas l'ajout. *)
        let (all_wl, all_ml, all_hr) =
          if (mod_spouse.Mwrite.Person.firstname = "" &&
              mod_spouse.Mwrite.Person.lastname = "" &&
              mod_children = [])
          then (all_wl, all_ml, all_hr)
          else
            let (all_wl, all_ml, all_hr) =
              match compute_add_family_ok conf base fam_desc with
              | Api_update_util.UpdateSuccess (wl, ml, hr) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr)
              | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
              | Api_update_util.UpdateErrorConflict c ->
                  raise (Api_update_util.ModErrApiConflict c)
            in
            let (all_wl, all_ml, all_hr) =
              List.fold_left
                (fun (all_wl, all_ml, all_hr) mod_child ->
                  let (sn, fn) =
                    (mod_child.Mwrite.Person.lastname,
                     mod_child.Mwrite.Person.firstname)
                  in
                  let occ =
                    match mod_child.Mwrite.Person.occ with
                    | None -> 0
                    | Some occ -> Int32.to_int occ
                  in
                  (match person_of_key base fn sn occ with
                  | Some ip_child ->
                      mod_child.Mwrite.Person.index <-
                        Int32.of_int (Adef.int_of_iper ip_child);
                      (* On calcul le digest maintenant que l'enfant est créé. *)
                      let child = poi base ip_child in
                      let digest =
                        Update.digest_person (UpdateInd.string_person_of base child)
                      in
                      mod_child.Mwrite.Person.digest <- digest;
                      (match Api_update_person.print_mod conf base mod_child with
                      | Api_update_util.UpdateSuccess (wl, ml, hr) ->
                          (all_wl @ wl, all_ml @ ml, all_hr @ hr)
                      | Api_update_util.UpdateError s -> raise (Update.ModErrApi s)
                      | Api_update_util.UpdateErrorConflict c ->
                          raise (Api_update_util.ModErrApiConflict c))
                  | None -> failwith "ErrorAddFirstFamNoChildFound"))
                (all_wl, all_ml, all_hr) mod_children
            in
            (all_wl, all_ml, all_hr)
        in

        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr)
      end
    with
    | Update.ModErrApi s -> Api_update_util.UpdateError s
    | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
  in
  let data = compute_modification_status conf base !ip !ifam resp in
  print_result conf data

#endif
