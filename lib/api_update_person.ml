#ifdef API

module Mwrite = Api_saisie_write_piqi
module Mext_write = Api_saisie_write_piqi_ext

open Gwdb
open Def
open Util
open Api_update_util

let reconstitute_person conf base mod_p : ('a, string * string * int * Update.create * string, string) gen_person =
  let key_index = Gwdb.iper_of_string @@ Int32.to_string mod_p.Mwrite.Person.index in
  let first_name = no_html_tags (only_printable mod_p.Mwrite.Person.firstname) in
  let surname = no_html_tags (only_printable mod_p.Mwrite.Person.lastname) in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if (List.exists contain_fn Name.forbidden_char) ||
       (List.exists contain_sn Name.forbidden_char) then
      begin
        removed_string :=
          (Name.purge first_name ^ " " ^ Name.purge surname) :: !removed_string;
        (Name.purge first_name, Name.purge surname)
      end
    else (first_name, surname)
  in
  (* Attention, dans le cas où l'on fait modifier personne, *)
  (* pour lui changer son occ, par un occ qui existe déjà,  *)
  (* il faut lui calculer le prochain occ de libre.         *)
  let occ =
    match mod_p.Mwrite.Person.create_link with
    | `create ->
        let fn = mod_p.Mwrite.Person.firstname in
        let sn = mod_p.Mwrite.Person.lastname in
        Api_update_util.api_find_free_occ base fn sn
    | _ ->
        (* Cas par défaut, i.e. modifier personne sans changer le occ. *)
        Opt.map_default 0 Int32.to_int mod_p.Mwrite.Person.occ
  in
  let image = Opt.map_default "" only_printable mod_p.Mwrite.Person.image in
  let first_names_aliases =
    List.map
      (fun s -> no_html_tags (only_printable s))
      mod_p.Mwrite.Person.firstname_aliases
  in
  let surnames_aliases =
    List.map
      (fun s -> no_html_tags (only_printable s))
      mod_p.Mwrite.Person.surname_aliases
  in
  let public_name =
    match mod_p.Mwrite.Person.public_name with
    | Some s -> no_html_tags (only_printable s)
    | None -> ""
  in
  let qualifiers =
    List.map
      (fun s -> no_html_tags (only_printable s ))
      mod_p.Mwrite.Person.qualifiers
  in
  let aliases =
    List.map
      (fun s -> no_html_tags (only_printable s ))
      mod_p.Mwrite.Person.aliases
  in
  let titles =
    List.map
      (fun t ->
        let t_name =
          match t.Mwrite.Title.name with
          | Some s -> if s = "" then Tnone else Tname s
          | None -> Tnone
        in
        let t_ident =
          match t.Mwrite.Title.title with
          | Some s -> s
          | None -> ""
        in
        let t_place =
          match t.Mwrite.Title.fief with
          | Some s -> s
          | None -> ""
        in
        let t_date_start =
          match t.Mwrite.Title.date_begin with
          | Some date -> Api_update_util.date_of_piqi_date conf date
          | None -> None
        in
        let t_date_end =
          match t.Mwrite.Title.date_end with
          | Some date -> Api_update_util.date_of_piqi_date conf date
          | None -> None
        in
        let t_nth =
          match t.Mwrite.Title.nth with
          | Some i -> Int32.to_int i
          | None -> 0
        in
        { t_name = t_name; t_ident = t_ident; t_place = t_place;
          t_date_start = Adef.cdate_of_od t_date_start;
          t_date_end = Adef.cdate_of_od t_date_end;
          t_nth = t_nth } )
      mod_p.Mwrite.Person.titles
  in
  let rparents =
    List.fold_right
      (fun r accu ->
        match r.Mwrite.Relation_parent.person with
        | Some person ->
            let r_type =
              match r.Mwrite.Relation_parent.rpt_type with
              | `rpt_adoption_father | `rpt_adoption_mother -> Adoption
              | `rpt_recognition_father | `rpt_recognition_mother -> Recognition
              | `rpt_candidate_parent_father | `rpt_candidate_parent_mother -> CandidateParent
              | `rpt_god_parent_father | `rpt_god_parent_mother -> GodParent
              | `rpt_foster_parent_father | `rpt_foster_parent_mother -> FosterParent
            in
            let (r_fath, r_moth) =
              match person.Mwrite.Person_link.sex with
                | `female -> (None, Some (reconstitute_somebody base person))
                | _ -> (Some (reconstitute_somebody base person), None)
            in
            let r_sources =
              match r.Mwrite.Relation_parent.source with
              | Some s -> s
              | None -> ""
            in
            let r =
              { r_type = r_type; r_fath = r_fath;
                r_moth = r_moth; r_sources = r_sources }
            in
            r :: accu
        | None -> accu)
      mod_p.Mwrite.Person.rparents []
  in
  let access =
    match mod_p.Mwrite.Person.access with
    | `access_iftitles -> IfTitles
    | `access_public -> Public
    | `access_private -> Private
  in
  let occupation = Opt.map_default "" only_printable mod_p.Mwrite.Person.occupation in
  let sex =
    match mod_p.Mwrite.Person.sex with
    | `male -> Male
    | `female -> Female
    | `unknown -> Neuter
  in
  let death =
    match mod_p.Mwrite.Person.death_type with
    | `not_dead -> NotDead
    | `dead -> DeadDontKnowWhen
    | `dead_young -> DeadYoung
    | `dead_dont_know_when -> DeadDontKnowWhen
    | `dont_know_if_dead -> DontKnowIfDead
    | `of_course_dead -> OfCourseDead
  in
  let psources = Opt.map_default "" only_printable mod_p.Mwrite.Person.psources in
  let notes =
    Opt.map_default ""
      (fun s -> only_printable_or_nl (Mutil.strip_all_trailing_spaces s))
      mod_p.Mwrite.Person.notes
  in
  let pevents =
    List.fold_right
      (fun evt pevents ->
        let name =
          match evt.Mwrite.Pevent.event_perso with
          | Some n -> Epers_Name (no_html_tags (only_printable n))
          | _ ->
              match evt.Mwrite.Pevent.pevent_type with
              | Some `epers_birth -> Epers_Birth
              | Some `epers_baptism -> Epers_Baptism
              | Some `epers_death -> Epers_Death
              | Some `epers_burial -> Epers_Burial
              | Some `epers_cremation -> Epers_Cremation
              | Some `epers_accomplishment -> Epers_Accomplishment
              | Some `epers_acquisition -> Epers_Acquisition
              | Some `epers_adhesion -> Epers_Adhesion
              | Some `epers_baptismlds -> Epers_BaptismLDS
              | Some `epers_barmitzvah -> Epers_BarMitzvah
              | Some `epers_batmitzvah -> Epers_BatMitzvah
              | Some `epers_benediction -> Epers_Benediction
              | Some `epers_changename -> Epers_ChangeName
              | Some `epers_circumcision -> Epers_Circumcision
              | Some `epers_confirmation -> Epers_Confirmation
              | Some `epers_confirmationlds -> Epers_ConfirmationLDS
              | Some `epers_decoration -> Epers_Decoration
              | Some `epers_demobilisationmilitaire -> Epers_DemobilisationMilitaire
              | Some `epers_diploma -> Epers_Diploma
              | Some `epers_distinction -> Epers_Distinction
              | Some `epers_dotation -> Epers_Dotation
              | Some `epers_dotationlds -> Epers_DotationLDS
              | Some `epers_education -> Epers_Education
              | Some `epers_election -> Epers_Election
              | Some `epers_emigration -> Epers_Emigration
              | Some `epers_excommunication -> Epers_Excommunication
              | Some `epers_familylinklds -> Epers_FamilyLinkLDS
              | Some `epers_firstcommunion -> Epers_FirstCommunion
              | Some `epers_funeral -> Epers_Funeral
              | Some `epers_graduate -> Epers_Graduate
              | Some `epers_hospitalisation -> Epers_Hospitalisation
              | Some `epers_illness -> Epers_Illness
              | Some `epers_immigration -> Epers_Immigration
              | Some `epers_listepassenger -> Epers_ListePassenger
              | Some `epers_militarydistinction -> Epers_MilitaryDistinction
              | Some `epers_militarypromotion -> Epers_MilitaryPromotion
              | Some `epers_militaryservice -> Epers_MilitaryService
              | Some `epers_mobilisationmilitaire -> Epers_MobilisationMilitaire
              | Some `epers_naturalisation -> Epers_Naturalisation
              | Some `epers_occupation -> Epers_Occupation
              | Some `epers_ordination -> Epers_Ordination
              | Some `epers_property -> Epers_Property
              | Some `epers_recensement -> Epers_Recensement
              | Some `epers_residence -> Epers_Residence
              | Some `epers_retired -> Epers_Retired
              | Some `epers_scellentchildlds -> Epers_ScellentChildLDS
              | Some `epers_scellentparentlds -> Epers_ScellentParentLDS
              | Some `epers_scellentspouselds -> Epers_ScellentSpouseLDS
              | Some `epers_ventebien -> Epers_VenteBien
              | Some `epers_will -> Epers_Will
              | _ -> Epers_Name ""
        in
        let date =
          match evt.Mwrite.Pevent.date with
          | Some date -> Api_update_util.date_of_piqi_date conf date
          | None -> None
        in
        let place = Opt.map_default "" (fun p -> no_html_tags (only_printable p)) evt.Mwrite.Pevent.place in
        let reason = Opt.map_default "" (fun r -> no_html_tags (only_printable r)) evt.Mwrite.Pevent.reason in
        let note =
          Opt.map_default
            "" (fun n -> only_printable_or_nl (Mutil.strip_all_trailing_spaces n))
            evt.Mwrite.Pevent.note
        in
        let src = Opt.map_default "" only_printable evt.Mwrite.Pevent.src in
        let witnesses =
          List.fold_right
            (fun witness accu ->
              match witness.Mwrite.Witness.person with
              | Some person ->
                  let wk =
                    match witness.Mwrite.Witness.witness_type with
                    | `witness -> Witness
                    | `witness_godparent -> Witness_GodParent
                  in
                  let wit = (reconstitute_somebody base person, wk) in
                  wit :: accu
              | None -> accu)
            evt.Mwrite.Pevent.witnesses []
        in
        (* strip evenement vide *)
        if name = Epers_Death && date = None && place = "" &&
           reason = "" && note = "" && src = "" &&
           witnesses = [] && death = DontKnowIfDead
        then pevents
        else
          let evt =
            { epers_name = name; epers_date = Adef.cdate_of_od date;
              epers_place = place; epers_reason = reason; epers_note = note;
              epers_src = src; epers_witnesses = Array.of_list witnesses }
          in
          evt :: pevents)
      mod_p.Mwrite.Person.pevents []
  in
  (* Mise à jour des évènements principaux. *)
  let (bi, bp, de, bu, pevents) =
    UpdateIndOk.reconstitute_from_pevents pevents false
      (Adef.cdate_None, "", "", "")
      (Adef.cdate_None, "", "", "")
      (death, "", "", "")
      (UnknownBurial, "", "", "")
  in
  let (birth, birth_place, birth_note, birth_src) = bi in
  let (baptism, baptism_place, baptism_note, baptism_src) = bp in
  let (death, death_place, death_note, death_src) = de in
  let (burial, burial_place, burial_note, burial_src) = bu in
  (* Maintenant qu'on a propagé les évènements, on a *)
  (* peut-être besoin de refaire un infer_death.     *)
  (* FIXME: do no use the _bb version *)
  let death =
    match death with
    | DontKnowIfDead ->
      Update.infer_death_bb conf (Adef.od_of_cdate birth) (Adef.od_of_cdate baptism)
    | _ -> death
  in
  let p =
    {first_name = first_name; surname = surname; occ = occ; image = image;
     first_names_aliases = first_names_aliases;
     surnames_aliases = surnames_aliases; public_name = public_name;
     qualifiers = qualifiers; aliases = aliases; titles = titles;
     rparents = rparents; occupation = occupation; related = [];
     sex = sex; access = access; birth = birth;
     birth_place = birth_place; birth_note = birth_note; birth_src = birth_src;
     baptism = baptism; baptism_place = baptism_place;
     baptism_note = baptism_note; baptism_src = baptism_src; death = death;
     death_place = death_place; death_note = death_note;
     death_src = death_src; burial = burial; burial_place = burial_place;
     burial_note = burial_note; burial_src = burial_src; notes = notes;
     pevents = pevents;
     psources = psources; key_index}
  in
  (* On vérifie s'il y a des conflits de personne. *)
  (* Normalement, il ne doit plus y avoir de lever *)
  (* de conflits par les autres modules : update,  *)
  (* updateIndOk et updateFamOk.                   *)
  let _err = Api_update_util.check_person_conflict conf base p in
  (* Maintenant qu'on a fini les conflit, on remet l'objet person *)
  (* tel que pour GeneWeb, c'est à dire qu'on supprime l'option   *)
  (* force_create.                                                *)
  let pevents_gw =
    List.map
      (fun e ->
        let w =
          Array.map
            (fun ((f, s, o, create, var, _), wk) ->
              ((f, s, o, create, var), wk))
            e.epers_witnesses
        in
        {(e) with epers_witnesses = w})
      pevents
  in
  let rparents_gw =
    List.map
      (fun r ->
        let (fath, moth) =
          match (r.r_fath, r.r_moth) with
          | (Some (f, s, o, create, var, _), None) ->
              (Some (f, s, o, create, var), None)
          | (None, Some (f, s, o, create, var, _)) ->
              (None, Some (f, s, o, create, var))
          | _ -> failwith "rparents_gw"
        in
        {(r) with r_fath = fath; r_moth = moth})
      rparents
  in
  { p with rparents = rparents_gw; pevents = pevents_gw ; related = [] }

(**/**)


let print_add conf base mod_p =
  try
    let sp : ('a, string * string * int * Update.create * string, string) gen_person = reconstitute_person conf base mod_p in
    let sp = {(sp) with key_index = Gwdb.dummy_iper} in
    (* On met à jour les occ. *)
    if sp.occ <> 0 then mod_p.Mwrite.Person.occ <- Some (Int32.of_int sp.occ);
    let sp : ('a, string * string * int * Update.create * string, string) gen_person = UpdateIndOk.strip_person sp in
    match UpdateIndOk.check_person conf (sp : ('a, string * string * int * Update.create * string, string) gen_person) with
    | Some err ->
        (* Correspond au cas ou fn/sn = ""/"?" *)
        (* => ne devrait pas se produire       *)
        Api_update_util.UpdateError err
    | None ->
        let (p, a) = UpdateIndOk.effective_add conf base (sp : ('a, string * string * int * Update.create * string, string) gen_person) in
        let u = {family = get_family (poi base p.key_index)} in
        let wl = UpdateIndOk.all_checks_person base p a u in
        let changed = U_Add_person (Util.string_gen_person base p) in
        let hr = [(fun () -> History.record conf base changed "ap")] in
        Api_update_util.UpdateSuccess (wl, [], hr)
  with
  | Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c

let print_del conf base ip =
  let p = poi base ip in
  let old_related = get_related p in
  let op = Util.string_gen_person base (gen_person_of_person p) in
  UpdateIndOk.update_relations_of_related base ip old_related;
  let warning _ = () in
  let p = UpdateIndOk.effective_del base warning p in
  patch_person base ip p;
  Notes.update_notes_links_db conf (NotesLinks.PgInd p.key_index) "";
  let changed = U_Delete_person op in
  let hr = [(fun () -> History.record conf base changed "dp")] in
  Api_update_util.UpdateSuccess ([], [], hr)


let print_mod_aux conf base mod_p callback =
  try
    let p : ('a, string * string * int * Update.create * string, string) gen_person = reconstitute_person conf base mod_p in
    let p = UpdateIndOk.strip_person p in
    let ini_ps = UpdateInd.string_person_of base (poi base p.key_index) in
    let digest = Update.digest_person ini_ps in
    if digest = mod_p.Mwrite.Person.digest then
      match UpdateIndOk.check_person conf p with
      | Some err ->
          (* Correspond au cas ou fn/sn = ""/"?" *)
          (* => ne devrait pas se produire       *)
          Api_update_util.UpdateError err
      | None -> callback p
    else
      (* On lance l'exception par Update.error_digest *)
      let _ = Update.error_digest conf in
      Api_update_util.UpdateError "BaseChanged"
  with
  | Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c


let print_mod ?(fexclude = []) conf base mod_p =
  let ip = Gwdb.iper_of_string @@ Int32.to_string mod_p.Mwrite.Person.index in
  let o_p =
    Util.string_gen_person base (gen_person_of_person (poi base ip))
  in
  let callback p =
    begin
      let p = UpdateIndOk.effective_mod conf base p in
      let op = poi base p.key_index in
      let u = {family = get_family op} in
      patch_person base p.key_index p;
      let s =
        let sl =
          [p.notes; p.occupation; p.birth_note; p.birth_src; p.baptism_note;
           p.baptism_src; p.death_note; p.death_src; p.burial_note;
           p.burial_src; p.psources]
        in
        let sl =
          let rec loop l accu =
            match l with
            | [] -> accu
            | evt :: l -> loop l (evt.epers_note :: evt.epers_src :: accu)
          in
          loop (p.pevents) sl
        in
        String.concat " " (List.map (sou base) sl)
      in
      Notes.update_notes_links_db conf (NotesLinks.PgInd p.key_index) s;
      let wl =
        let a = poi base p.key_index in
        let a = {parents = get_parents a; consang = get_consang a} in
        let family =
          Array.of_list @@
          Array.fold_right begin fun ifam acc ->
            if List.mem ifam fexclude then acc else ifam :: acc
          end u.family []
        in
        let u = { family } in
        UpdateIndOk.all_checks_person base p a u
      in
      let changed = U_Modify_person (o_p, (Util.string_gen_person base p)) in
      let hr =
        [(fun () -> History.record conf base changed "mp");
         (fun () ->
           if not (is_quest_string p.surname) &&
              not (is_quest_string p.first_name) &&
              not (is_old_person conf p)
           then
             Update.delete_topological_sort_v conf base
           else ())]
      in
      Api_update_util.UpdateSuccess (wl, [], hr)
    end
  in
  print_mod_aux conf base mod_p callback


(**/**) (* Fonctions pour la première saisie, i.e. on n'a pas de base ! *)


(* Comme on n'a pas de base, on va garder une hashtbl des occurrences. *)
let ht_occ = Hashtbl.create 7 ;;

let find_free_occ_nobase fn sn =
  let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
  try
    let occ = Hashtbl.find ht_occ key in
    Hashtbl.replace ht_occ key (succ occ);
    occ
  with Not_found ->
    begin
      let occ = 0 in
      Hashtbl.add ht_occ key (succ occ);
      occ
    end


let reconstitute_person_nobase conf mod_p =
  let key_index = Gwdb.iper_of_string @@ Int32.to_string mod_p.Mwrite.Person.index in
  let first_name = no_html_tags (only_printable mod_p.Mwrite.Person.firstname) in
  let surname = no_html_tags (only_printable mod_p.Mwrite.Person.lastname) in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if (List.exists contain_fn Name.forbidden_char) ||
       (List.exists contain_sn Name.forbidden_char) then
      begin
        removed_string :=
          (Name.purge first_name ^ " " ^ Name.purge surname) :: !removed_string;
        (Name.purge first_name, Name.purge surname)
      end
    else (first_name, surname)
  in
  (* Attention, dans le cas où l'on fait modifier personne, *)
  (* pour lui changer son occ, par un occ qui existe déjà,  *)
  (* il faut lui calculer le prochain occ de libre.         *)
  let occ =
    match mod_p.Mwrite.Person.create_link with
    | `create_default_occ ->
        let fn = mod_p.Mwrite.Person.firstname in
        let sn = mod_p.Mwrite.Person.lastname in
        find_free_occ_nobase fn sn
    | `create ->
        begin
          match mod_p.Mwrite.Person.occ with
          | Some occ -> Int32.to_int occ
          | None -> 0
        end
    | `link ->
        (* Impossible ! On ne peut pas lier, il n'y a pas de base. *)
        failwith "ErrorAddPersonNoBase"
  in
  let image =
    match mod_p.Mwrite.Person.image with
    | Some s -> only_printable s
    | None -> ""
  in
  let first_names_aliases =
    List.map
      (fun s -> no_html_tags (only_printable s))
      mod_p.Mwrite.Person.firstname_aliases
  in
  let surnames_aliases =
    List.map
      (fun s -> no_html_tags (only_printable s))
      mod_p.Mwrite.Person.surname_aliases
  in
  let public_name =
    match mod_p.Mwrite.Person.public_name with
    | Some s -> no_html_tags (only_printable s)
    | None -> ""
  in
  let qualifiers =
    List.map
      (fun s -> no_html_tags (only_printable s ))
      mod_p.Mwrite.Person.qualifiers
  in
  let aliases =
    List.map
      (fun s -> no_html_tags (only_printable s ))
      mod_p.Mwrite.Person.aliases
  in
  let titles =
    List.map
      (fun t ->
        let t_name =
          match t.Mwrite.Title.name with
          | Some s -> if s = "" then Tnone else Tname s
          | None -> Tnone
        in
        let t_ident =
          match t.Mwrite.Title.title with
          | Some s -> s
          | None -> ""
        in
        let t_place =
          match t.Mwrite.Title.fief with
          | Some s -> s
          | None -> ""
        in
        let t_date_start =
          match t.Mwrite.Title.date_begin with
          | Some date -> Api_update_util.date_of_piqi_date conf date
          | None -> None
        in
        let t_date_end =
          match t.Mwrite.Title.date_end with
          | Some date -> Api_update_util.date_of_piqi_date conf date
          | None -> None
        in
        let t_nth =
          match t.Mwrite.Title.nth with
          | Some i -> Int32.to_int i
          | None -> 0
        in
        { t_name = t_name; t_ident = t_ident; t_place = t_place;
          t_date_start = Adef.cdate_of_od t_date_start;
          t_date_end = Adef.cdate_of_od t_date_end;
          t_nth = t_nth } )
      mod_p.Mwrite.Person.titles
  in
  let rparents = [] in
  let access =
    match mod_p.Mwrite.Person.access with
    | `access_iftitles -> IfTitles
    | `access_public -> Public
    | `access_private -> Private
  in
  let occupation =
    match mod_p.Mwrite.Person.occupation with
    | Some s -> only_printable s
    | None -> ""
  in
  let sex =
    match mod_p.Mwrite.Person.sex with
    | `male -> Male
    | `female -> Female
    | `unknown -> Neuter
  in
  let death =
    match mod_p.Mwrite.Person.death_type with
    | `not_dead -> NotDead
    | `dead -> DeadDontKnowWhen
    | `dead_young -> DeadYoung
    | `dead_dont_know_when -> DeadDontKnowWhen
    | `dont_know_if_dead -> DontKnowIfDead
    | `of_course_dead -> OfCourseDead
  in
  let psources =
    match mod_p.Mwrite.Person.psources with
    | Some s -> only_printable s
    | None -> ""
  in
  let notes =
    match mod_p.Mwrite.Person.notes with
    | Some s -> only_printable_or_nl (Mutil.strip_all_trailing_spaces s)
    | None -> ""
  in
  let pevents =
    List.fold_right
      (fun evt pevents ->
        let name =
          match evt.Mwrite.Pevent.event_perso with
          | Some n -> Epers_Name (no_html_tags (only_printable n))
          | _ ->
              match evt.Mwrite.Pevent.pevent_type with
              | Some `epers_birth -> Epers_Birth
              | Some `epers_baptism -> Epers_Baptism
              | Some `epers_death -> Epers_Death
              | Some `epers_burial -> Epers_Burial
              | Some `epers_cremation -> Epers_Cremation
              | Some `epers_accomplishment -> Epers_Accomplishment
              | Some `epers_acquisition -> Epers_Acquisition
              | Some `epers_adhesion -> Epers_Adhesion
              | Some `epers_baptismlds -> Epers_BaptismLDS
              | Some `epers_barmitzvah -> Epers_BarMitzvah
              | Some `epers_batmitzvah -> Epers_BatMitzvah
              | Some `epers_benediction -> Epers_Benediction
              | Some `epers_changename -> Epers_ChangeName
              | Some `epers_circumcision -> Epers_Circumcision
              | Some `epers_confirmation -> Epers_Confirmation
              | Some `epers_confirmationlds -> Epers_ConfirmationLDS
              | Some `epers_decoration -> Epers_Decoration
              | Some `epers_demobilisationmilitaire -> Epers_DemobilisationMilitaire
              | Some `epers_diploma -> Epers_Diploma
              | Some `epers_distinction -> Epers_Distinction
              | Some `epers_dotation -> Epers_Dotation
              | Some `epers_dotationlds -> Epers_DotationLDS
              | Some `epers_education -> Epers_Education
              | Some `epers_election -> Epers_Election
              | Some `epers_emigration -> Epers_Emigration
              | Some `epers_excommunication -> Epers_Excommunication
              | Some `epers_familylinklds -> Epers_FamilyLinkLDS
              | Some `epers_firstcommunion -> Epers_FirstCommunion
              | Some `epers_funeral -> Epers_Funeral
              | Some `epers_graduate -> Epers_Graduate
              | Some `epers_hospitalisation -> Epers_Hospitalisation
              | Some `epers_illness -> Epers_Illness
              | Some `epers_immigration -> Epers_Immigration
              | Some `epers_listepassenger -> Epers_ListePassenger
              | Some `epers_militarydistinction -> Epers_MilitaryDistinction
              | Some `epers_militarypromotion -> Epers_MilitaryPromotion
              | Some `epers_militaryservice -> Epers_MilitaryService
              | Some `epers_mobilisationmilitaire -> Epers_MobilisationMilitaire
              | Some `epers_naturalisation -> Epers_Naturalisation
              | Some `epers_occupation -> Epers_Occupation
              | Some `epers_ordination -> Epers_Ordination
              | Some `epers_property -> Epers_Property
              | Some `epers_recensement -> Epers_Recensement
              | Some `epers_residence -> Epers_Residence
              | Some `epers_retired -> Epers_Retired
              | Some `epers_scellentchildlds -> Epers_ScellentChildLDS
              | Some `epers_scellentparentlds -> Epers_ScellentParentLDS
              | Some `epers_scellentspouselds -> Epers_ScellentSpouseLDS
              | Some `epers_ventebien -> Epers_VenteBien
              | Some `epers_will -> Epers_Will
              | _ -> Epers_Name ""
        in
        let date =
          match evt.Mwrite.Pevent.date with
          | Some date -> Api_update_util.date_of_piqi_date conf date
          | None -> None
        in
        let place =
          match evt.Mwrite.Pevent.place with
          | Some place -> no_html_tags (only_printable place)
          | None -> ""
        in
        let reason =
          match evt.Mwrite.Pevent.reason with
          | Some reason -> no_html_tags (only_printable reason)
          | None -> ""
        in
        let note =
          match evt.Mwrite.Pevent.note with
          | Some note ->
              only_printable_or_nl (Mutil.strip_all_trailing_spaces note)
          | None -> ""
        in
        let src =
          match evt.Mwrite.Pevent.src with
          | Some src -> only_printable src
          | None -> ""
        in
        let witnesses = [] in
        (* strip evenement vide *)
        if name = Epers_Death && date = None && place = "" &&
           reason = "" && note = "" && src = "" &&
           witnesses = [] && death = DontKnowIfDead
        then pevents
        else
          let evt =
            { epers_name = name; epers_date = Adef.cdate_of_od date;
              epers_place = place; epers_reason = reason; epers_note = note;
              epers_src = src; epers_witnesses = Array.of_list witnesses }
          in
          evt :: pevents)
      mod_p.Mwrite.Person.pevents []
  in
  (* Mise à jour des évènements principaux. *)
  let (bi, bp, de, bu, pevents) =
    UpdateIndOk.reconstitute_from_pevents pevents false
      (Adef.cdate_None, "", "", "")
      (Adef.cdate_None, "", "", "")
      (death, "", "", "")
      (UnknownBurial, "", "", "")
  in
  let (birth, birth_place, birth_note, birth_src) = bi in
  let (baptism, baptism_place, baptism_note, baptism_src) = bp in
  let (death, death_place, death_note, death_src) = de in
  let (burial, burial_place, burial_note, burial_src) = bu in
  (* Maintenant qu'on a propagé les évènements, on a *)
  (* peut-être besoin de refaire un infer_death.     *)
  {first_name = first_name; surname = surname; occ = occ; image = image;
   first_names_aliases = first_names_aliases;
   surnames_aliases = surnames_aliases; public_name = public_name;
   qualifiers = qualifiers; aliases = aliases; titles = titles;
   rparents = rparents; occupation = occupation; related = [];
   sex = sex; access = access; birth = birth;
   birth_place = birth_place; birth_note = birth_note; birth_src = birth_src;
   baptism = baptism; baptism_place = baptism_place;
   baptism_note = baptism_note; baptism_src = baptism_src; death = death;
   death_place = death_place; death_note = death_note;
   death_src = death_src; burial = burial; burial_place = burial_place;
   burial_note = burial_note; burial_src = burial_src; notes = notes;
   pevents = pevents;
   psources = psources; key_index}
(* On vérifie s'il y a des conflits de personne. *)
(* Normalement, il ne doit plus y avoir de lever *)
(* de conflits par les autres modules : update,  *)
(* updateIndOk et updateFamOk.                   *)
(* Api_update_util.check_person_conflict conf base p *)

let print_add_nobase conf mod_p =
  try
    let sp = reconstitute_person_nobase conf mod_p in
    let sp = {(sp) with key_index = Gwdb.dummy_iper} in
    (* On met à jour les occ. *)
    if sp.occ <> 0 then mod_p.Mwrite.Person.occ <- Some (Int32.of_int sp.occ);
    let _sp = UpdateIndOk.strip_person sp in
    (* On ne vérifie pas ici si le prénom de la personne est vide, mais *)
    (* on le fait plus haut, pour savoir si c'est un oubli ou si l'on   *)
    (* ne connait pas la personne.                                      *)
    (* On n'appelle pas CheckItem car ils ne sont pas révélateurs *)
    Api_update_util.UpdateSuccess ([], [], [])
  with
  | Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c

#endif
