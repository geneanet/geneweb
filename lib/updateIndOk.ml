(* $Id: updateIndOk.ml,v 5.75 2008-01-21 13:28:12 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

(* Liste des string dont on a supprimé un caractère.       *)
(* Utilisé pour le message d'erreur lors de la validation. *)
let removed_string = ref []

let raw_get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get_nth conf key cnt = p_getenv conf.env (key ^ string_of_int cnt)

let getn conf var key =
  match p_getenv conf.env (var ^ "_" ^ key) with
    Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound")

let rec reconstitute_string_list conf var ext cnt =
  match get_nth conf var cnt with
    Some s ->
      let s = no_html_tags (only_printable s) in
      let (sl, ext) = reconstitute_string_list conf var ext (cnt + 1) in
      begin match get_nth conf ("add_" ^ var) cnt with
        Some "on" -> s :: "" :: sl, true
      | _ -> s :: sl, ext
      end
  | _ -> [], ext

let reconstitute_insert_title conf ext cnt tl =
  let var = "ins_title" ^ string_of_int cnt in
  let n =
    match p_getenv conf.env var, p_getint conf.env (var ^ "_n") with
      _, Some n when n > 1 -> n
    | Some "on", _ -> 1
    | _ -> 0
  in
  if n > 0 then
    let tl =
      let rec loop tl n =
        if n > 0 then
          let t1 =
            {t_name = Tnone; t_ident = ""; t_place = "";
             t_date_start = Adef.cdate_None; t_date_end = Adef.cdate_None;
             t_nth = 0}
          in
          loop (t1 :: tl) (n - 1)
        else tl
      in
      loop tl n
    in
    tl, true
  else tl, ext

let rec reconstitute_titles conf ext cnt =
  match
    get_nth conf "t_ident" cnt, get_nth conf "t_place" cnt,
    get_nth conf "t_name" cnt
  with
    Some t_ident, Some t_place, Some t_name ->
      let t_name =
        match get_nth conf "t_main_title" cnt, t_name with
          Some "on", _ -> Tmain
        | _, "" -> Tnone
        | _, _ -> Tname (no_html_tags (only_printable t_name))
      in
      let t_date_start =
        Update.reconstitute_date conf ("t_date_start" ^ string_of_int cnt)
      in
      let t_date_end =
        Update.reconstitute_date conf ("t_date_end" ^ string_of_int cnt)
      in
      let t_nth =
        match get_nth conf "t_nth" cnt with
          Some s -> (try int_of_string s with Failure _ -> 0)
        | _ -> 0
      in
      let t =
        {t_name = t_name; t_ident = no_html_tags (only_printable t_ident);
         t_place = no_html_tags (only_printable t_place);
         t_date_start = Adef.cdate_of_od t_date_start;
         t_date_end = Adef.cdate_of_od t_date_end; t_nth = t_nth}
      in
      let (tl, ext) = reconstitute_titles conf ext (cnt + 1) in
      let (tl, ext) = reconstitute_insert_title conf ext cnt tl in
      t :: tl, ext
  | _ -> [], ext

let reconstitute_somebody conf var =
  let first_name = no_html_tags (only_printable (getn conf var "fn")) in
  let surname = no_html_tags (only_printable (getn conf var "sn")) in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if List.exists contain_fn Name.forbidden_char ||
       List.exists contain_sn Name.forbidden_char
    then
      begin
        removed_string :=
          (Name.purge first_name ^ " " ^ Name.purge surname) ::
          !removed_string;
        Name.purge first_name, Name.purge surname
      end
    else first_name, surname
  in
  let occ = try int_of_string (getn conf var "occ") with Failure _ -> 0 in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
      Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter
  in
  let create =
    match getn conf var "p" with
      "create" -> Update.Create (sex, None)
    | _ -> Update.Link
  in
  first_name, surname, occ, create, var

let reconstitute_insert_pevent conf ext cnt el =
  let var = "ins_event" ^ string_of_int cnt in
  let n =
    match p_getenv conf.env var, p_getint conf.env (var ^ "_n") with
      _, Some n when n > 1 -> n
    | Some "on", _ -> 1
    | _ -> 0
  in
  if n > 0 then
    let el =
      let rec loop el n =
        if n > 0 then
          let e1 =
            {epers_name = Epers_Name ""; epers_date = Adef.cdate_None;
             epers_place = ""; epers_reason = ""; epers_note = "";
             epers_src = ""; epers_witnesses = [| |]}
          in
          loop (e1 :: el) (n - 1)
        else el
      in
      loop el n
    in
    el, true
  else el, ext

let rec reconstitute_pevents conf ext cnt =
  match get_nth conf "e_name" cnt with
    Some epers_name ->
      let epers_name =
        match epers_name with
          "#birt" -> Epers_Birth
        | "#bapt" -> Epers_Baptism
        | "#deat" -> Epers_Death
        | "#buri" -> Epers_Burial
        | "#crem" -> Epers_Cremation
        | "#acco" -> Epers_Accomplishment
        | "#acqu" -> Epers_Acquisition
        | "#adhe" -> Epers_Adhesion
        | "#awar" -> Epers_Decoration
        | "#bapl" -> Epers_BaptismLDS
        | "#barm" -> Epers_BarMitzvah
        | "#basm" -> Epers_BatMitzvah
        | "#bles" -> Epers_Benediction
        | "#cens" -> Epers_Recensement
        | "#chgn" -> Epers_ChangeName
        | "#circ" -> Epers_Circumcision
        | "#conf" -> Epers_Confirmation
        | "#conl" -> Epers_ConfirmationLDS
        | "#degr" -> Epers_Diploma
        | "#demm" -> Epers_DemobilisationMilitaire
        | "#dist" -> Epers_Distinction
        | "#dotl" -> Epers_DotationLDS
        | "#educ" -> Epers_Education
        | "#elec" -> Epers_Election
        | "#emig" -> Epers_Emigration
        | "#endl" -> Epers_Dotation
        | "#exco" -> Epers_Excommunication
        | "#fcom" -> Epers_FirstCommunion
        | "#flkl" -> Epers_FamilyLinkLDS
        | "#fune" -> Epers_Funeral
        | "#grad" -> Epers_Graduate
        | "#hosp" -> Epers_Hospitalisation
        | "#illn" -> Epers_Illness
        | "#immi" -> Epers_Immigration
        | "#lpas" -> Epers_ListePassenger
        | "#mdis" -> Epers_MilitaryDistinction
        | "#mobm" -> Epers_MobilisationMilitaire
        | "#mpro" -> Epers_MilitaryPromotion
        | "#mser" -> Epers_MilitaryService
        | "#natu" -> Epers_Naturalisation
        | "#occu" -> Epers_Occupation
        | "#ordn" -> Epers_Ordination
        | "#prop" -> Epers_Property
        | "#resi" -> Epers_Residence
        | "#reti" -> Epers_Retired
        | "#slgc" -> Epers_ScellentChildLDS
        | "#slgp" -> Epers_ScellentParentLDS
        | "#slgs" -> Epers_ScellentSpouseLDS
        | "#vteb" -> Epers_VenteBien
        | "#will" -> Epers_Will
        | n -> Epers_Name (no_html_tags (only_printable n))
      in
      let epers_date =
        Update.reconstitute_date conf ("e_date" ^ string_of_int cnt)
      in
      let epers_place =
        match get_nth conf "e_place" cnt with
          Some place -> no_html_tags (only_printable place)
        | _ -> ""
      in
      let epers_note =
        match get_nth conf "e_note" cnt with
          Some note ->
            Util.sanitize_html
              (only_printable_or_nl (Mutil.strip_all_trailing_spaces note))
        | _ -> ""
      in
      let epers_src =
        match get_nth conf "e_src" cnt with
          Some src -> only_printable src
        | _ -> ""
      in
      (* Type du témoin par défaut lors de l'insertion de nouveaux témoins. *)
      let wk =
        if epers_name = Epers_Baptism then Witness_GodParent else Witness
      in
      let (witnesses, ext) =
        let rec loop i ext =
          match
            try
              let var = "e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i in
              Some (reconstitute_somebody conf var)
            with Failure _ -> None
          with
            Some c ->
              let (witnesses, ext) = loop (i + 1) ext in
              let var_c =
                "e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i ^ "_kind"
              in
              let c =
                match p_getenv conf.env var_c with
                  Some "godp" -> c, Witness_GodParent
                | _ -> c, Witness
              in
              let var_w =
                "e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i
              in
              begin match p_getenv conf.env var_w with
                Some "on" ->
                  let ins_witn_n =
                    "e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i ^
                    "_n"
                  in
                  begin match p_getint conf.env ins_witn_n with
                    Some n when n > 1 ->
                      let rec loop_witn n witnesses =
                        if n = 0 then c :: witnesses, true
                        else
                          let new_witn =
                            ("", "", 0, Update.Create (Neuter, None), ""), wk
                          in
                          let witnesses = new_witn :: witnesses in
                          loop_witn (n - 1) witnesses
                      in
                      loop_witn n witnesses
                  | _ ->
                      let new_witn =
                        ("", "", 0, Update.Create (Neuter, None), ""), wk
                      in
                      c :: new_witn :: witnesses, true
                  end
              | _ -> c :: witnesses, ext
              end
          | None -> [], ext
        in
        loop 1 ext
      in
      let (witnesses, ext) =
        let evt_ins = "e" ^ string_of_int cnt ^ "_ins_witn0" in
        match p_getenv conf.env evt_ins with
          Some "on" ->
            let ins_witn_n = "e" ^ string_of_int cnt ^ "_ins_witn0_n" in
            begin match p_getint conf.env ins_witn_n with
              Some n when n > 1 ->
                let rec loop_witn n witnesses =
                  if n = 0 then witnesses, true
                  else
                    let new_witn =
                      ("", "", 0, Update.Create (Neuter, None), ""), wk
                    in
                    let witnesses = new_witn :: witnesses in
                    loop_witn (n - 1) witnesses
                in
                loop_witn n witnesses
            | _ ->
                let new_witn =
                  ("", "", 0, Update.Create (Neuter, None), ""), wk
                in
                new_witn :: witnesses, true
            end
        | _ -> witnesses, ext
      in
      let e =
        {epers_name = epers_name; epers_date = Adef.cdate_of_od epers_date;
         epers_place = epers_place; epers_reason = "";
         epers_note = epers_note; epers_src = epers_src;
         epers_witnesses = Array.of_list witnesses}
      in
      let (el, ext) = reconstitute_pevents conf ext (cnt + 1) in
      let (el, ext) = reconstitute_insert_pevent conf ext (cnt + 1) el in
      e :: el, ext
  | _ -> [], ext

let rec reconstitute_sorted_pevents conf cnt =
  match get_nth conf "e_id" cnt, get_nth conf "e_pos" cnt with
    Some id, Some pos ->
      let (id, pos) =
        try int_of_string id, int_of_string pos with Failure _ -> 0, 0
      in
      let el = reconstitute_sorted_pevents conf (cnt + 1) in (id, pos) :: el
  | _ -> []

let reconstitute_add_relation conf ext cnt rl =
  match get_nth conf "add_relation" cnt with
    Some "on" ->
      let r =
        {r_type = GodParent; r_fath = None; r_moth = None; r_sources = ""}
      in
      r :: rl, true
  | _ -> rl, ext

let deleted_relation = ref []

let reconstitute_relation_parent conf var key sex =
  match getn conf var (key ^ "_fn"), getn conf var (key ^ "_sn") with
    "", _ | _, "" | "?", _ | _, "?" as n ->
      let p = only_printable (fst n) ^ only_printable (snd n) in
      if p = "" || p = "??" then ()
      else deleted_relation := p :: !deleted_relation;
      None
  | fn, sn ->
      let fn = only_printable fn in
      let sn = only_printable sn in
      (* S'il y a des caractères interdits, on les supprime *)
      let (fn, sn) =
        let contain_fn = String.contains fn in
        let contain_sn = String.contains sn in
        if List.exists contain_fn Name.forbidden_char ||
           List.exists contain_sn Name.forbidden_char
        then
          begin
            removed_string :=
              (Name.purge fn ^ " " ^ Name.purge sn) :: !removed_string;
            Name.purge fn, Name.purge sn
          end
        else fn, sn
      in
      let occ =
        try int_of_string (getn conf var (key ^ "_occ")) with Failure _ -> 0
      in
      let create =
        match getn conf var (key ^ "_p") with
          "create" -> Update.Create (sex, None)
        | _ -> Update.Link
      in
      Some (fn, sn, occ, create, var ^ "_" ^ key)

let reconstitute_relation conf var =
  try
    let r_fath = reconstitute_relation_parent conf var "fath" Male in
    let r_moth = reconstitute_relation_parent conf var "moth" Female in
    let r_type =
      match getn conf var "type" with
        "Adoption" -> Adoption
      | "Recognition" -> Recognition
      | "CandidateParent" -> CandidateParent
      | "GodParent" -> GodParent
      | "FosterParent" -> FosterParent
      | _ -> GodParent
    in
    Some {r_type = r_type; r_fath = r_fath; r_moth = r_moth; r_sources = ""}
  with Failure _ -> None

let rec reconstitute_relations conf ext cnt =
  match reconstitute_relation conf ("r" ^ string_of_int cnt) with
    Some r ->
      let (rl, ext) = reconstitute_relations conf ext (cnt + 1) in
      let (rl, ext) = reconstitute_add_relation conf ext cnt rl in
      r :: rl, ext
  | _ -> [], ext

let reconstitute_death conf birth baptism death_place burial burial_place =
  let d = Update.reconstitute_date conf "death" in
  let dr =
    match p_getenv conf.env "death_reason" with
      Some "Killed" -> Killed
    | Some "Murdered" -> Murdered
    | Some "Executed" -> Executed
    | Some "Disappeared" -> Disappeared
    | Some "Unspecified" | None -> Unspecified
    | Some x -> failwith ("bad death reason type " ^ x)
  in
  match get conf "death" with
    "Auto" when d = None ->
      if death_place <> "" || burial <> UnknownBurial || burial_place <> "" ||
         dr <> Unspecified
      then
        DeadDontKnowWhen
      else Update.infer_death_bb conf birth baptism
  | "DeadYoung" when d = None -> DeadYoung
  | "DontKnowIfDead" when d = None -> DontKnowIfDead
  | "NotDead" -> NotDead
  | "OfCourseDead" when d = None -> OfCourseDead
  | _ ->
      match d with
        Some d -> Death (dr, Adef.cdate_of_date d)
      | _ -> DeadDontKnowWhen

let reconstitute_burial conf burial_place =
  let d = Update.reconstitute_date conf "burial" in
  match p_getenv conf.env "burial" with
    Some "UnknownBurial" | None ->
      begin match d, burial_place with
        None, "" -> UnknownBurial
      | _ -> Buried (Adef.cdate_of_od d)
      end
  | Some "Buried" -> Buried (Adef.cdate_of_od d)
  | Some "Cremated" -> Cremated (Adef.cdate_of_od d)
  | Some x -> failwith ("bad burial type " ^ x)

let reconstitute_from_pevents pevents ext bi bp de bu =
  (* On tri les évènements pour être sûr. *)
  let pevents =
    CheckItem.sort_events
      ((fun evt -> CheckItem.Psort evt.epers_name),
       (fun evt -> evt.epers_date))
      pevents
  in
  let found_birth = ref false in
  let found_baptism = ref false in
  let found_death = ref false in
  let found_burial = ref false in
  let death_reason_std_fields =
    let (death_std_fields, _, _, _) = de in
    match death_std_fields with
      Death (dr, _) -> dr
    | _ -> Unspecified
  in
  let rec loop pevents bi bp de bu =
    match pevents with
      [] -> bi, bp, de, bu
    | evt :: l ->
        match evt.epers_name with
          Epers_Birth ->
            if !found_birth then loop l bi bp de bu
            else
              let bi =
                evt.epers_date, evt.epers_place, evt.epers_note, evt.epers_src
              in
              let () = found_birth := true in loop l bi bp de bu
        | Epers_Baptism ->
            if !found_baptism then loop l bi bp de bu
            else
              let bp =
                evt.epers_date, evt.epers_place, evt.epers_note, evt.epers_src
              in
              let () = found_baptism := true in loop l bi bp de bu
        | Epers_Death ->
            if !found_death then loop l bi bp de bu
            else
              let death =
                match Adef.od_of_cdate evt.epers_date with
                  Some d ->
                    Death (death_reason_std_fields, Adef.cdate_of_date d)
                | None ->
                    let (death, _, _, _) = de in
                    (* On ajoute DontKnowIfDead dans le cas où tous les *)
                    (* champs sont vides.                               *)
                    match death with
                      DeadYoung | DeadDontKnowWhen | OfCourseDead |
                      DontKnowIfDead as death ->
                        death
                    | _ -> DeadDontKnowWhen
              in
              let de =
                death, evt.epers_place, evt.epers_note, evt.epers_src
              in
              let () = found_death := true in loop l bi bp de bu
        | Epers_Burial ->
            if !found_burial then loop l bi bp de bu
            else
              let bu =
                Buried evt.epers_date, evt.epers_place, evt.epers_note,
                evt.epers_src
              in
              let () = found_burial := true in loop l bi bp de bu
        | Epers_Cremation ->
            if !found_burial then loop l bi bp de bu
            else
              let bu =
                Cremated evt.epers_date, evt.epers_place, evt.epers_note,
                evt.epers_src
              in
              let () = found_burial := true in loop l bi bp de bu
        | _ -> loop l bi bp de bu
  in
  let (bi, bp, de, bu) = loop pevents bi bp de bu in
  (* Hack *)
  let pevents =
    if not !found_death then
      let remove_evt = ref false in
      List.fold_left
        (fun accu evt ->
           if not !remove_evt then
             if evt.epers_name = Epers_Name "" then
               begin remove_evt := true; accu end
             else evt :: accu
           else evt :: accu)
        [] (List.rev pevents)
    else pevents
  in
  let pevents =
    if not !found_burial then
      let remove_evt = ref false in
      List.fold_left
        (fun accu evt ->
           if not !remove_evt then
             if evt.epers_name = Epers_Name "" then
               begin remove_evt := true; accu end
             else evt :: accu
           else evt :: accu)
        [] (List.rev pevents)
    else pevents
  in
  let pevents =
    if not ext then
      let remove_evt = ref false in
      List.fold_left
        (fun accu evt ->
           if not !remove_evt then
             if evt.epers_name = Epers_Name "" then
               begin remove_evt := true; accu end
             else evt :: accu
           else evt :: accu)
        [] (List.rev pevents)
    else pevents
  in
  (* Il faut gérer le cas où l'on supprime délibérément l'évènement. *)
  let bi = if not !found_birth then Adef.cdate_None, "", "", "" else bi in
  let bp = if not !found_baptism then Adef.cdate_None, "", "", "" else bp in
  let de =
    if not !found_death then
      if !found_burial then DeadDontKnowWhen, "", "", ""
      else
        let (death, _, _, _) = de in
        match death with
          NotDead -> NotDead, "", "", ""
        | _ -> DontKnowIfDead, "", "", ""
    else de
  in
  let bu = if not !found_burial then UnknownBurial, "", "", "" else bu in
  bi, bp, de, bu, pevents

let reconstitute_person conf =
  let ext = false in
  let key_index =
    match p_getenv conf.env "i" with
      Some s -> (try int_of_string (String.trim s) with Failure _ -> -1)
    | _ -> -1
  in
  let first_name = no_html_tags (only_printable (get conf "first_name")) in
  let surname = no_html_tags (only_printable (get conf "surname")) in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if List.exists contain_fn Name.forbidden_char ||
       List.exists contain_sn Name.forbidden_char
    then
      begin
        removed_string :=
          (Name.purge first_name ^ " " ^ Name.purge surname) ::
          !removed_string;
        Name.purge first_name, Name.purge surname
      end
    else first_name, surname
  in
  let occ =
    try int_of_string (String.trim (get conf "occ")) with Failure _ -> 0
  in
  let image = only_printable (get conf "image") in
  let (first_names_aliases, ext) =
    reconstitute_string_list conf "first_name_alias" ext 0
  in
  let (surnames_aliases, ext) =
    reconstitute_string_list conf "surname_alias" ext 0
  in
  let public_name = no_html_tags (only_printable (get conf "public_name")) in
  let (qualifiers, ext) = reconstitute_string_list conf "qualifier" ext 0 in
  let (aliases, ext) = reconstitute_string_list conf "alias" ext 0 in
  let (titles, ext) = reconstitute_titles conf ext 1 in
  let (titles, ext) = reconstitute_insert_title conf ext 0 titles in
  let (rparents, ext) = reconstitute_relations conf ext 1 in
  let (rparents, ext) = reconstitute_add_relation conf ext 0 rparents in
  let access =
    match p_getenv conf.env "access" with
      Some "Public" -> Public
    | Some "Private" -> Private
    | _ -> IfTitles
  in
  let occupation = only_printable (get conf "occu") in
  let sex =
    match p_getenv conf.env "sex" with
      Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter
  in
  let birth = Update.reconstitute_date conf "birth" in
  let birth_place = no_html_tags (only_printable (get conf "birth_place")) in
  let birth_note =
    only_printable_or_nl (Mutil.strip_all_trailing_spaces (get conf "birth_note"))
  in
  let birth_src = only_printable (get conf "birth_src") in
  let bapt = Update.reconstitute_date conf "bapt" in
  let bapt_place = no_html_tags (only_printable (get conf "bapt_place")) in
  let bapt_note =
    only_printable_or_nl (Mutil.strip_all_trailing_spaces (get conf "bapt_note"))
  in
  let bapt_src = only_printable (get conf "bapt_src") in
  let burial_place =
    no_html_tags (only_printable (get conf "burial_place"))
  in
  let burial_note =
    only_printable_or_nl (Mutil.strip_all_trailing_spaces (get conf "burial_note"))
  in
  let burial_src = only_printable (get conf "burial_src") in
  let burial = reconstitute_burial conf burial_place in
  let death_place = no_html_tags (only_printable (get conf "death_place")) in
  let death_note =
    only_printable_or_nl (Mutil.strip_all_trailing_spaces (get conf "death_note"))
  in
  let death_src = only_printable (get conf "death_src") in
  let death =
    reconstitute_death conf birth bapt death_place burial burial_place
  in
  let death_place =
    match death with
      Death (_, _) | DeadYoung | DeadDontKnowWhen -> death_place
    | _ -> ""
  in
  let death =
    match death, burial with
      (NotDead | DontKnowIfDead), (Buried _ | Cremated _) -> DeadDontKnowWhen
    | _ -> death
  in
  let (pevents, ext) = reconstitute_pevents conf ext 1 in
  let (pevents, ext) = reconstitute_insert_pevent conf ext 0 pevents in
  let notes =
    if first_name = "?" || surname = "?" then ""
    else
      Util.sanitize_html
        (only_printable_or_nl (Mutil.strip_all_trailing_spaces (get conf "notes")))
  in
  let psources = only_printable (get conf "src") in
  (* Mise à jour des évènements principaux. *)
  let (bi, bp, de, bu, pevents) =
    reconstitute_from_pevents pevents ext
      (Adef.cdate_of_od birth, birth_place, birth_note, birth_src)
      (Adef.cdate_of_od bapt, bapt_place, bapt_note, bapt_src)
      (death, death_place, death_note, death_src)
      (burial, burial_place, burial_note, burial_src)
  in
  let (birth, birth_place, birth_note, birth_src) = bi in
  let (bapt, bapt_place, bapt_note, bapt_src) = bp in
  let (death, death_place, death_note, death_src) = de in
  let (burial, burial_place, burial_note, burial_src) = bu in
  (* Maintenant qu'on a propagé les evèenements, on a *)
  (* peut-être besoin de refaire un infer_death.      *)
  let death =
    match death with
      DontKnowIfDead ->
      (* FIXME: do not use _bb version *)
        Update.infer_death_bb conf (Adef.od_of_cdate birth)
          (Adef.od_of_cdate bapt)
    | _ -> death
  in
  let p =
    {first_name = first_name; surname = surname; occ = occ; image = image;
     first_names_aliases = first_names_aliases;
     surnames_aliases = surnames_aliases; public_name = public_name;
     qualifiers = qualifiers; aliases = aliases; titles = titles;
     rparents = rparents; occupation = occupation; related = []; sex = sex;
     access = access; birth = birth; birth_place = birth_place;
     birth_note = birth_note; birth_src = birth_src; baptism = bapt;
     baptism_place = bapt_place; baptism_note = bapt_note;
     baptism_src = bapt_src; death = death; death_place = death_place;
     death_note = death_note; death_src = death_src; burial = burial;
     burial_place = burial_place; burial_note = burial_note;
     burial_src = burial_src; pevents = pevents; notes = notes;
     psources = psources; key_index = Adef.iper_of_int key_index}
  in
  p, ext

let check_event_witnesses conf witnesses =
  let len = Array.length witnesses in
  let rec loop i =
    if i = len then None
    else begin
      let ((fn, sn, _, _, _), _) = Array.get witnesses i in
      if fn = "" && sn = "" then loop (i + 1)
      else if fn = "" || fn = "?" then
        Some
          (transl_nth conf "witness/witnesses" 0 ^ " : " ^
           transl conf "first name missing")
      else if sn = "" || sn = "?" then
        Some
          (transl_nth conf "witness/witnesses" 0 ^ " : " ^
           transl conf "surname missing")
      else loop (i + 1)
    end
  in
  loop 0

let check_person conf p =
  if p.first_name = "" || p.first_name = "?" then
    Some (transl conf "first name missing")
  else if p.surname = "" || p.surname = "?" then
    Some (transl conf "surname missing")
  else
    let rec loop pevents =
      match pevents with
        [] -> None
      | evt :: l ->
          match check_event_witnesses conf evt.epers_witnesses with
            Some err -> Some err
          | _ -> loop l
    in
    loop p.pevents

let error_person conf err =
#ifdef API
  if !(Api_conf.mode_api) then
    begin let err = Printf.sprintf "%s" (capitale (transl conf "error")) in
      raise (Update.ModErrApi err)
    end;
#endif
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s\n" (capitale err);
  Update.print_return conf;
  Hutil.trailer conf;
  raise Update.ModErr

let strip_pevents p =
  let strip_array_witness pl =
    let pl =
      Array.fold_right
        (fun ((f, _, _, _, _), _ as p) pl -> if f = "" then pl else p :: pl)
        pl []
    in
    Array.of_list pl
  in
  List.fold_right
    (fun e accu ->
       let (has_infos, witnesses) =
         match e.epers_name with
           Epers_Name s -> s <> "", strip_array_witness e.epers_witnesses
         | Epers_Birth | Epers_Baptism ->
             Adef.od_of_cdate e.epers_date <> None || e.epers_place <> "" ||
             e.epers_reason <> "" || e.epers_note <> "" || e.epers_src <> "",
             strip_array_witness e.epers_witnesses
         | _ -> true, strip_array_witness e.epers_witnesses
       in
       if has_infos || Array.length witnesses > 0 then
         {e with epers_witnesses = witnesses} :: accu
       else accu)
    p.pevents []

let strip_list = List.filter (fun s -> s <> "")

let strip_person p =
  {p with first_names_aliases = strip_list p.first_names_aliases;
   surnames_aliases = strip_list p.surnames_aliases;
   qualifiers = strip_list p.qualifiers; aliases = strip_list p.aliases;
   titles = List.filter (fun t -> t.t_ident <> "") p.titles;
   pevents = strip_pevents p;
   rparents =
     List.filter (fun r -> r.r_fath <> None || r.r_moth <> None) p.rparents}

let print_conflict conf base p =
#ifdef API
  if !(Api_conf.mode_api) then
    begin let err =
      Printf.sprintf
        (fcapitale (ftransl conf "name %s already used by %tthis person%t"))
        ("\"" ^ p_first_name base p ^ "." ^ string_of_int (get_occ p) ^ " " ^
         p_surname base p ^ "\"")
        (fun _ ->
           Printf.sprintf "%s %s" (sou base (get_first_name p))
             (sou base (get_surname p)))
        (fun _ -> ".")
    in
      raise (Update.ModErrApi err)
    end;
#endif
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
  Wserver.printf "<input type=\"hidden\" name=\"free_occ\" value=\"%d\"%s>\n"
    free_n conf.xhs;
  Wserver.printf "<input type=\"submit\" name=\"create\" value=\"%s\"%s>\n"
    (capitale (transl conf "create")) conf.xhs;
  Wserver.printf "<input type=\"submit\" name=\"return\" value=\"%s\"%s>\n"
    (capitale (transl conf "back")) conf.xhs;
  Wserver.printf "</form>\n";
  Update.print_same_name conf base p;
  Hutil.trailer conf;
  raise Update.ModErr

let print_cannot_change_sex conf base p =
#ifdef API
  if !(Api_conf.mode_api) then
    begin let err =
      Printf.sprintf "%s."
        (capitale (transl conf "cannot change sex of a married person"))
    in
      raise (Update.ModErrApi err)
    end;
#endif
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Update.print_error conf base (BadSexOfMarriedPerson p);
  Wserver.printf "<ul>\n";
  html_li conf;
  Wserver.printf "\n%s" (referenced_person_text conf base p);
  Wserver.printf "\n";
  Wserver.printf "</ul>\n";
  Update.print_return conf;
  Hutil.trailer conf;
  raise Update.ModErr

let check_conflict conf base sp ipl =
  let name = Name.lower (sp.first_name ^ " " ^ sp.surname) in
  List.iter
    (fun ip ->
       let p1 = poi base ip in
       if get_key_index p1 <> sp.key_index &&
          Name.lower (p_first_name base p1 ^ " " ^ p_surname base p1) =
            name &&
          get_occ p1 = sp.occ
       then
       (* TODO probably better to use Update.print_create_conflict
          but one additionnal parameter (var) I dont understand ?? *)
         print_conflict conf base p1)
    ipl

let check_sex_married conf base sp op =
  if sp.sex <> get_sex op then
    let no_check =
      Array.for_all
        (fun ifam ->
           let r = get_relation (foi base ifam) in
           r = NoSexesCheckNotMarried || r = NoSexesCheckMarried)
        (get_family op)
    in
    if not no_check then print_cannot_change_sex conf base op

let rename_image_file conf base op sp =
  match auto_image_file conf base op with
    Some old_f ->
      let s = default_image_name_of_key sp.first_name sp.surname sp.occ in
      let f = Filename.concat (Util.base_path ["images"] conf.bname) s in
      let new_f =
        if Filename.check_suffix old_f ".gif" then f ^ ".gif" else f ^ ".jpg"
      in
      (try Sys.rename old_f new_f with Sys_error _ -> ())
  | _ -> ()

let rparents_of rparents =
  List.fold_left
    (fun ipl r ->
       match r.r_fath, r.r_moth with
         Some ip1, Some ip2 -> ip1 :: ip2 :: ipl
       | Some ip, _ -> ip :: ipl
       | _, Some ip -> ip :: ipl
       | _ -> ipl)
    [] rparents

let pwitnesses_of pevents =
  List.fold_left
    (fun ipl e ->
       Array.fold_left (fun ipl (ip, _) -> ip :: ipl) ipl
         e.epers_witnesses)
    [] pevents

(* sp.death *)
let effective_mod conf base sp =
  let pi = sp.key_index in
  let op = poi base pi in
  let key = sp.first_name ^ " " ^ sp.surname in
  let ofn = p_first_name base op in
  let osn = p_surname base op in
  let oocc = get_occ op in
  if ofn = sp.first_name && osn = sp.surname && oocc = sp.occ then ()
  else
    begin let ipl = Gutil.person_ht_find_all base key in
      check_conflict conf base sp ipl; rename_image_file conf base op sp
    end;
  let same_fn_sn =
    Name.lower (Mutil.nominative sp.first_name) = Name.lower ofn &&
    Name.lower (Mutil.nominative sp.surname) = Name.lower osn
  in
  if sp.first_name <> "?" && sp.surname <> "?" &&
     (not same_fn_sn || oocc <> sp.occ)
  then
    begin
      delete_key base ofn osn oocc;
      patch_key base pi sp.first_name sp.surname sp.occ;
      if not same_fn_sn then patch_name base key pi
    end;
  (* Si on modifie la personne pour lui ajouter un nom/prénom, alors *)
  (* il faut remettre le compteur du nombre de personne à jour.      *)
  if ofn = "?" && osn = "?" && sp.first_name <> "?" && sp.surname <> "?" then
    patch_cache_info conf Util.cache_nb_base_persons
      (fun v -> let v = int_of_string v + 1 in string_of_int v);
  check_sex_married conf base sp op;
  let created_p = ref [] in
  let np =
    Futil.map_person_ps (Update.insert_person conf base "" created_p)
      (Gwdb.insert_string base) sp
  in
  let op_misc_names = person_misc_names base op get_titles in
  let np = {np with related = get_related op} in
  let np_misc_names = gen_person_misc_names base np (fun p -> p.titles) in
  List.iter
    (fun key ->
       if List.mem key op_misc_names then () else Gutil.person_ht_add base key pi)
    np_misc_names;
  let ol_rparents = rparents_of (get_rparents op) in
  let nl_rparents = rparents_of np.rparents in
  let ol_pevents = pwitnesses_of (get_pevents op) in
  let nl_pevents = pwitnesses_of np.pevents in
  let ol = List.append ol_rparents ol_pevents in
  let nl = List.append nl_rparents nl_pevents in
  let pi = np.key_index in Update.update_related_pointers base pi ol nl; np

let effective_add conf base sp =
  let pi = Adef.iper_of_int (nb_of_persons base) in
  let fn = Util.translate_eval sp.first_name in
  let sn = Util.translate_eval sp.surname in
  let key = fn ^ " " ^ sn in
  let ipl = Gutil.person_ht_find_all base key in
  check_conflict conf base sp ipl;
  patch_key base pi fn sn sp.occ;
  Gutil.person_ht_add base key pi;
  patch_cache_info conf Util.cache_nb_base_persons
    (fun v -> let v = int_of_string v + 1 in string_of_int v);
  let created_p = ref [] in
  let np =
    Futil.map_person_ps (Update.insert_person conf base "" created_p)
      (Gwdb.insert_string base) {sp with key_index = pi}
  in
  patch_person base pi np;
  let na = {parents = None; consang = Adef.fix (-1)} in
  patch_ascend base pi na;
  let nu = {family = [| |]} in
  patch_union base pi nu;
  let np_misc_names = gen_person_misc_names base np (fun p -> p.titles) in
  List.iter (fun key -> Gutil.person_ht_add base key pi) np_misc_names; np, na

let array_except v a =
  let rec loop i =
    if i = Array.length a then a
    else if a.(i) = v then
      Array.append (Array.sub a 0 i)
        (Array.sub a (i + 1) (Array.length a - i - 1))
    else loop (i + 1)
  in
  loop 0

let update_relations_of_related base ip old_related =
  List.iter
    (fun ip1 ->
       let p1 = poi base ip1 in
       let (rparents, rparents_are_different) =
         List.fold_right
           (fun rel (list, rad) ->
              let (rfath, rad) =
                match rel.r_fath with
                  Some ip2 -> if ip2 = ip then None, true else Some ip2, rad
                | None -> None, rad
              in
              let (rmoth, rad) =
                match rel.r_moth with
                  Some ip2 -> if ip2 = ip then None, true else Some ip2, rad
                | None -> None, rad
              in
              if rfath = None && rmoth = None then list, true
              else
                let rel = {rel with r_fath = rfath; r_moth = rmoth} in
                rel :: list, rad)
           (get_rparents p1) ([], false)
       in
       let (pevents, pevents_are_different) =
         List.fold_right
           (fun e (list, rad) ->
              let (witnesses, rad) =
                Array.fold_right
                  (fun (ip2, k) (accu, rad) ->
                     if ip2 = ip then accu, true else (ip2, k) :: accu, rad)
                  e.epers_witnesses ([], rad)
              in
              let e = {e with epers_witnesses = Array.of_list witnesses} in
              e :: list, rad)
           (get_pevents p1) ([], false)
       in
       if rparents_are_different || pevents_are_different then
         begin let p = gen_person_of_person p1 in
           let rparents =
             if rparents_are_different then rparents else p.rparents
           in
           let pevents =
             if pevents_are_different then pevents else p.pevents
           in
           patch_person base ip1
             {p with rparents = rparents; pevents = pevents}
         end;
       let families = get_family p1 in
       for i = 0 to Array.length families - 1 do
         let ifam = families.(i) in
         let fam = foi base ifam in
         let old_witnesses = Array.to_list (get_witnesses fam) in
         let new_witnesses = List.filter ((<>) ip) old_witnesses in
         let (fevents, fevents_are_different) =
           List.fold_right
             (fun e (list, rad) ->
                let (witnesses, rad) =
                  Array.fold_right
                    (fun (ip2, k) (accu, rad) ->
                       if ip2 = ip then accu, true else (ip2, k) :: accu, rad)
                    e.efam_witnesses ([], rad)
                in
                let e = {e with efam_witnesses = Array.of_list witnesses} in
                e :: list, rad)
             (get_fevents fam) ([], false)
         in
         if new_witnesses <> old_witnesses || fevents_are_different then
           let fam = gen_family_of_family fam in
           let witnesses =
             if new_witnesses <> old_witnesses then
               Array.of_list new_witnesses
             else fam.witnesses
           in
           let fevents =
             if fevents_are_different then fevents else fam.fevents
           in
           patch_family base ifam
             {fam with witnesses = witnesses; fevents = fevents}
       done)
    old_related

let effective_del base warning p =
  let none = Gwdb.insert_string base "?" in
  let empty = Gwdb.insert_string base "" in
  let ip = get_key_index p in
  begin match get_parents p with
    Some ifam ->
      let des = foi base ifam in
      let des =
        let children = array_except ip (get_children des) in
        begin match CheckItem.sort_children base children with
          Some (b, a) -> warning (ChangedOrderOfChildren (ifam, des, b, a))
        | None -> ()
        end;
        {children = children}
      in
      patch_descend base ifam des;
      let asc = {parents = None; consang = Adef.fix (-1)} in
      patch_ascend base ip asc
  | None -> ()
  end;
  let old_rparents = rparents_of (get_rparents p) in
  let old_pevents = pwitnesses_of (get_pevents p) in
  let old = List.append old_rparents old_pevents in
  Update.update_related_pointers base ip old [];
  {first_name = none; surname = none; occ = 0; image = empty;
   public_name = empty; qualifiers = []; aliases = []; sex = get_sex p;
   first_names_aliases = []; surnames_aliases = []; titles = [];
   rparents = []; related = []; occupation = empty; access = IfTitles;
   birth = Adef.cdate_None; birth_place = empty; birth_note = empty;
   birth_src = empty; baptism = Adef.cdate_None; baptism_place = empty;
   baptism_note = empty; baptism_src = empty; death = DontKnowIfDead;
   death_place = empty; death_note = empty; death_src = empty;
   burial = UnknownBurial; burial_place = empty; burial_note = empty;
   burial_src = empty; pevents = []; notes = empty; psources = empty;
   key_index = ip}

let print_mod_ok conf base wl pgl p ofn osn oocc =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "person modified"))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then
    begin
      Wserver.printf "<h3 class=\"error\">";
      Wserver.printf (fcapitale (ftransl conf "%s forbidden char"))
        (List.fold_left (fun acc c -> acc ^ "'" ^ Char.escaped c ^ "' ") " "
           Name.forbidden_char);
      Wserver.printf "</h3>\n";
      List.iter (Wserver.printf "<p>%s</p>") !removed_string
    end;
  (* Si on a supprimé des relations, on les mentionne *)
  begin match !deleted_relation with
    [] -> ()
  | _ ->
      Wserver.printf "<p>\n";
      Wserver.printf "%s, %s %s %s :"
        (capitale (transl_nth conf "relation/relations" 0))
        (transl conf "first name missing") (transl conf "or")
        (transl conf "surname missing");
      Wserver.printf "<ul>\n";
      List.iter
        (fun s ->
           Wserver.printf "<li>";
           Wserver.printf "%s" s;
           Wserver.printf "</li>")
        !deleted_relation;
      Wserver.printf "</ul>\n";
      Wserver.printf "</p>\n"
  end;
  Wserver.printf "\n<p>%s</p>"
    (referenced_person_text conf base (poi base p.key_index));
  Wserver.printf "\n";
  Update.print_warnings conf base wl;
  let pi = p.key_index in
  let np = poi base pi in
  let nfn = p_first_name base np in
  let nsn = p_surname base np in
  let nocc = get_occ np in
  if pgl <> [] && (ofn <> nfn || osn <> nsn || oocc <> nocc) then
    begin
      Wserver.printf
        "<div class='alert alert-danger mx-auto mt-1' role='alert'>\n";
      Wserver.printf (ftransl conf "name changed. update linked pages");
      Wserver.printf "</div>\n";
      let soocc = if oocc <> 0 then Printf.sprintf "/%d" oocc else "" in
      let snocc = if nocc <> 0 then Printf.sprintf "/%d" nocc else "" in
      Wserver.printf "<span class=\"unselectable float-left\">%s%s</span>\n\
                      <span class=\"float-left ml-1\">%s/%s%s</span>\n<br>"
        (capitale (transl conf "old name")) (transl conf ":") ofn osn soocc;
      Wserver.printf "<span class=\"unselectable float-left\">%s%s</span>\n\
                      <span class=\"float-left ml-1\">%s/%s%s</span>\n<br>"
        (capitale (transl conf "new name")) (transl conf ":") nfn nsn snocc;
      Wserver.printf "<span>%s%s</span>"
        (capitale (transl conf "linked pages")) (transl conf ":");
      Notes.print_linked_list conf base pgl
    end;
  Hutil.trailer conf

(*
value print_mod_ok conf base wl p =
  if wl = [] then Perso.print conf base p
  else print_mod_ok_aux conf base wl p
;
*)

let relation_sex_is_coherent base warning p =
  List.iter
    (fun r ->
       begin match r.r_fath with
         Some ip ->
           let p = poi base ip in
           if get_sex p <> Male then warning (IncoherentSex (p, 0, 0))
       | None -> ()
       end;
       match r.r_moth with
         Some ip ->
           let p = poi base ip in
           if get_sex p <> Female then warning (IncoherentSex (p, 0, 0))
       | None -> ())
    p.rparents

let all_checks_person base p a u =
  let wl = ref [] in
  let warning w = wl := w :: !wl in
  let _ =
    (let p = person_of_gen_person base (p, a, u) in
     CheckItem.person base warning p :
     _ option)
  in
  relation_sex_is_coherent base warning p;
  begin match a.parents with
    Some ifam ->
      CheckItem.reduce_family base warning ifam (foi base ifam)
  | _ -> ()
  end;
  Array.iter
    (fun ifam ->
       CheckItem.reduce_family base warning ifam (foi base ifam))
    u.family;
  let wl = List.sort_uniq compare !wl in
  List.iter
    (function
       ChangedOrderOfChildren (ifam, _, _, after) ->
         patch_descend base ifam {children = after}
     | ChangedOrderOfPersonEvents (_, _, after) ->
         patch_person base p.key_index {p with pevents = after}
     | _ -> ())
    wl;
  wl

let print_add_ok conf base wl p =
  let title _ = Wserver.printf "%s" (capitale (transl conf "person added")) in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then
    begin
      Wserver.printf "<h2 class=\"error\">%s</h2>\n"
        (capitale (transl conf "forbidden char"));
      List.iter (Wserver.printf "<p>%s</p>") !removed_string
    end;
  (* Si on a supprimé des relations, on les mentionne *)
  List.iter
    (fun s -> Wserver.printf "%s -> %s\n" s (transl conf "forbidden char"))
    !deleted_relation;
  Wserver.printf "\n%s"
    (referenced_person_text conf base (poi base p.key_index));
  Wserver.printf "\n";
  Update.print_warnings conf base wl;
  Hutil.trailer conf

(*
value print_add_ok conf base wl p =
  if wl = [] then Perso.print conf base p
  else print_add_ok_aux conf base wl p
;
*)

let print_del_ok conf =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "person deleted"))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf false;
  Hutil.trailer conf

let print_change_event_order_ok conf base wl p =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "person modified"))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Update.print_warnings conf base wl;
  Wserver.printf "\n%s"
    (referenced_person_text conf base (poi base p.key_index));
  Wserver.printf "\n";
  Hutil.trailer conf


let print_add o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let conf = Update.update_conf o_conf in
  try
    let (sp, ext) = reconstitute_person conf in
    let redisp =
      match p_getenv conf.env "return" with
        Some _ -> true
      | _ -> false
    in
    if ext || redisp then UpdateInd.print_update_ind conf base sp ""
    else
      let sp = strip_person sp in
      match check_person conf sp with
        Some err -> error_person conf err
      | None ->
          let (p, a) = effective_add conf base sp in
          let u = {family = get_family (poi base p.key_index)} in
          let wl = all_checks_person base p a u in
          Util.commit_patches conf base;
          let changed = U_Add_person (Util.string_gen_person base p) in
          History.record conf base changed "ap"; print_add_ok conf base wl p
  with Update.ModErr -> ()

let print_del conf base =
  match p_getint conf.env "i" with
    Some i ->
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      let fn = sou base (get_first_name p) in
      let sn = sou base (get_surname p) in
      let occ = get_occ p in
      let old_related = get_related p in
      let op = Util.string_gen_person base (gen_person_of_person p) in
      update_relations_of_related base ip old_related;
      let warning _ = () in
      let p = effective_del base warning p in
      patch_person base ip p;
      if fn <> "?" && sn <> "?" then
        patch_cache_info conf Util.cache_nb_base_persons
          (fun v -> let v = int_of_string v - 1 in string_of_int v);
      delete_key base fn sn occ;
      Notes.update_notes_links_db conf (NotesLinks.PgInd p.key_index) "";
      Util.commit_patches conf base;
      let changed = U_Delete_person op in
      History.record conf base changed "dp";
      print_del_ok conf
  | _ -> Hutil.incorrect_request conf

let print_mod_aux conf base callback =
  try
    let (p, ext) = reconstitute_person conf in
    let redisp =
      match p_getenv conf.env "return" with
        Some _ -> true
      | _ -> false
    in
    let ini_ps = UpdateInd.string_person_of base (poi base p.key_index) in
    let digest = Update.digest_person ini_ps in
    if digest = raw_get conf "digest" then
      if ext || redisp then UpdateInd.print_update_ind conf base p digest
      else
        let p = strip_person p in
        match check_person conf p with
          Some err -> error_person conf err
        | None -> callback p
    else Update.error_digest conf
  with Update.ModErr -> ()

let print_mod o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let o_p =
    match p_getint o_conf.env "i" with
      Some ip ->
        Util.string_gen_person base
          (gen_person_of_person (poi base (Adef.iper_of_int ip)))
    | None ->
        Util.string_gen_person base
          (gen_person_of_person (poi base (Adef.iper_of_int (-1))))
  in
  let ofn = o_p.first_name in
  let osn = o_p.surname in
  let oocc = o_p.occ in
  let key = Name.lower ofn, Name.lower osn, oocc in
  let conf = Update.update_conf o_conf in
  let pgl =
    let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
    let fname = Filename.concat bdir "notes_links" in
    let db = NotesLinks.read_db_from_file fname in
    let db = Notes.merge_possible_aliases conf db in
    Perso.links_to_ind conf base db key
  in
  let callback sp =
    let p = effective_mod conf base sp in
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
            [] -> accu
          | evt :: l -> loop l (evt.epers_note :: evt.epers_src :: accu)
        in
        loop p.pevents sl
      in
      String.concat " " (List.map (sou base) sl)
    in
    Notes.update_notes_links_db conf (NotesLinks.PgInd p.key_index) s;
    if not (eq_istr (get_surname op) p.surname) ||
       not (Futil.eq_lists eq_istr (get_surnames_aliases op) p.surnames_aliases) ||
       not (Futil.eq_lists (Futil.eq_titles eq_istr) (get_titles op) p.titles)
    then
      Update.update_misc_names_of_family base p.sex u;
    let wl =
      let a = poi base p.key_index in
      let a = {parents = get_parents a; consang = get_consang a} in
      all_checks_person base p a u
    in
    Util.commit_patches conf base;
    let changed = U_Modify_person (o_p, Util.string_gen_person base p) in
    History.record conf base changed "mp";
    if not (is_quest_string p.surname) &&
       not (is_quest_string p.first_name) && not (is_old_person conf p)
    then
      Update.delete_topological_sort_v conf base;
    print_mod_ok conf base wl pgl p ofn osn oocc
  in
  print_mod_aux conf base callback

let print_change_event_order conf base =
  match p_getint conf.env "i" with
    Some ip ->
      begin try
        let p = poi base (Adef.iper_of_int ip) in
        let o_p = Util.string_gen_person base (gen_person_of_person p) in
        let ht = Hashtbl.create 50 in
        let _ =
          List.fold_left (fun id evt -> Hashtbl.add ht id evt; succ id) 1
            (get_pevents p)
        in
        let sorted_pevents =
          List.sort (fun (_, pos1) (_, pos2) -> compare pos1 pos2)
            (reconstitute_sorted_pevents conf 1)
        in
        let pevents =
          List.fold_right
            (fun (id, _) accu ->
               try Hashtbl.find ht id :: accu with
                 Not_found -> failwith "Sorting event")
            sorted_pevents []
        in
        let p = gen_person_of_person p in
        let p = {p with pevents = pevents} in
        patch_person base p.key_index p;
        let wl =
          let a = poi base p.key_index in
          let a = {parents = get_parents a; consang = get_consang a} in
          let u = poi base p.key_index in
          let u = {family = get_family u} in all_checks_person base p a u
        in
        Util.commit_patches conf base;
        let changed = U_Modify_person (o_p, Util.string_gen_person base p) in
        History.record conf base changed "mp";
        print_change_event_order_ok conf base wl p
      with Update.ModErr -> ()
      end
  | _ -> Hutil.incorrect_request conf
