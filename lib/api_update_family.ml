#ifdef API

module Mwrite = Api_saisie_write_piqi
module Mext_write = Api_saisie_write_piqi_ext

open Config
open Gwdb
open Def
open Util
open Api_update_util

let reconstitute_family conf base mod_f =
  (* Valeurs par défaut qui seront écrasées par reconstitute_from_fevents. *)
  let relation = NoMention in
  let marriage = Adef.cdate_None in
  let marriage_place = "" in
  let marriage_note = "" in
  let marriage_src = "" in
  let divorce = NotDivorced in
  (* Attention, si witnesses est vide, on va supprimer des témoins (qui sont
     en double parce que dans GeneWeb, ils sont récupérés une fois dans fevents
     et une fois dans le bloc hidden. Du coup à la validation, on supprime
     les 'deuxième' témoins (voir update.ml) *)
  let witnesses =
    List.map
      (fun ip ->
        let p = poi base (Gwdb.iper_of_string @@ Int32.to_string ip) in
        let fn = sou base (get_first_name p) in
        let sn = sou base (get_surname p) in
        let occ =
          if fn = "?" || sn = "?"
          then int_of_string @@ Gwdb.string_of_iper (get_iper p)
          else get_occ p
        in
        (fn, sn, occ, Update.Link, "", false))
      mod_f.Mwrite.Family.old_witnesses
  in
  let fevents =
    List.map
      (fun evt ->
        let name =
          match evt.Mwrite.Fevent.event_perso with
          | Some n -> Efam_Name (no_html_tags (only_printable n))
          | _ ->
              match evt.Mwrite.Fevent.fevent_type with
              | Some `efam_marriage -> Efam_Marriage
              | Some `efam_no_marriage -> Efam_NoMarriage
              | Some `efam_no_mention -> Efam_NoMention
              | Some `efam_engage -> Efam_Engage
              | Some `efam_divorce -> Efam_Divorce
              | Some `efam_separated -> Efam_Separated
              | Some `efam_annulation -> Efam_Annulation
              | Some `efam_marriage_bann -> Efam_MarriageBann
              | Some `efam_marriage_contract -> Efam_MarriageContract
              | Some `efam_marriage_license -> Efam_MarriageLicense
              | Some `efam_pacs -> Efam_PACS
              | Some `efam_residence -> Efam_Residence
              | _ -> Efam_Name ""
        in
        let date =
          match evt.Mwrite.Fevent.date with
          | Some date -> Api_update_util.date_of_piqi_date conf date
          | None -> None
        in
        let place =
          match evt.Mwrite.Fevent.place with
          | Some place -> no_html_tags (only_printable place)
          | None -> ""
        in
        let reason =
          match evt.Mwrite.Fevent.reason with
          | Some reason -> no_html_tags (only_printable reason)
          | None -> ""
        in
        let note =
          match evt.Mwrite.Fevent.note with
          | Some note ->
              only_printable_or_nl (Mutil.strip_all_trailing_spaces note)
          | None -> ""
        in
        let src =
          match evt.Mwrite.Fevent.src with
          | Some src -> only_printable src
          | None -> ""
        in
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
            evt.Mwrite.Fevent.witnesses []
        in
        { efam_name = name; efam_date = Adef.cdate_of_od date;
          efam_place = place; efam_reason = reason; efam_note = note;
          efam_src = src; efam_witnesses = Array.of_list witnesses })
      mod_f.Mwrite.Family.fevents
  in
  let comment =
    match mod_f.Mwrite.Family.comment with
    | Some comment ->
        only_printable_or_nl (Mutil.strip_all_trailing_spaces comment)
    | None -> ""
  in
  let fsources =
    match mod_f.Mwrite.Family.fsources with
    | Some s -> only_printable s
    | None -> ""
  in
  let origin_file =
    match mod_f.Mwrite.Family.origin_file with
    | Some s -> s
    | None -> ""
  in
  let fam_index = Gwdb.ifam_of_string @@ Int32.to_string mod_f.Mwrite.Family.index in
  let parents =
    let father = mod_f.Mwrite.Family.father in
    let sex =
      match father.Mwrite.Person.sex with
      | `male -> Male
      | `female -> Female
      | `unknown -> Neuter
    in
    let father =
      match father.Mwrite.Person.create_link with
      | `create_default_occ ->
          let fn = father.Mwrite.Person.firstname in
          let sn = father.Mwrite.Person.lastname in
          let occ =
            match father.Mwrite.Person.occ with
            | Some occ -> Int32.to_int occ
            | None -> 0
          in
          (fn, sn, occ, Update.Create (sex, None), "", false)
      | `create ->
          let fn = father.Mwrite.Person.firstname in
          let sn = father.Mwrite.Person.lastname in
          let occ = Api_update_util.api_find_free_occ base fn sn in
          (* On met à jour parce que si on veut le rechercher, *)
          (* il faut qu'on connaisse son occ.                  *)
          let () =
            if occ = 0 then father.Mwrite.Person.occ <- None
            else father.Mwrite.Person.occ <- Some (Int32.of_int occ)
          in
          (fn, sn, occ, Update.Create (sex, None), "", true)
      | `link ->
          let ip = Gwdb.iper_of_string @@ Int32.to_string father.Mwrite.Person.index in
          let p = poi base ip in
          let fn = sou base (get_first_name p) in
          let sn = sou base (get_surname p) in
          let occ =
            if fn = "?" || sn = "?"
            then int_of_string @@ Gwdb.string_of_iper (get_iper p)
            else get_occ p
          in
          (fn, sn, occ, Update.Link, "", false)
    in
    let mother = mod_f.Mwrite.Family.mother in
    let sex =
      match mother.Mwrite.Person.sex with
      | `male -> Male
      | `female -> Female
      | `unknown -> Neuter
    in
    let mother =
      match mother.Mwrite.Person.create_link with
      | `create_default_occ ->
          let fn = mother.Mwrite.Person.firstname in
          let sn = mother.Mwrite.Person.lastname in
          let occ =
            match mother.Mwrite.Person.occ with
            | Some occ -> Int32.to_int occ
            | None -> 0
          in
          (fn, sn, occ, Update.Create (sex, None), "", false)
      | `create ->
          let fn = mother.Mwrite.Person.firstname in
          let sn = mother.Mwrite.Person.lastname in
          let occ = Api_update_util.api_find_free_occ base fn sn in
          (* On met à jour parce que si on veut le rechercher, *)
          (* il faut qu'on connaisse son occ.                  *)
          let () =
            if occ = 0 then mother.Mwrite.Person.occ <- None
            else mother.Mwrite.Person.occ <- Some (Int32.of_int occ)
          in
          (fn, sn, occ, Update.Create (sex, None), "", true)
      | `link ->
          let ip = Gwdb.iper_of_string @@ Int32.to_string mother.Mwrite.Person.index in
          let p = poi base ip in
          let fn = sou base (get_first_name p) in
          let sn = sou base (get_surname p) in
          let occ =
            if fn = "?" || sn = "?"
            then int_of_string @@ Gwdb.string_of_iper (get_iper p)
            else get_occ p
          in
          (fn, sn, occ, Update.Link, "", false)
    in
    [father; mother]
  in
  let children =
    List.map
      (fun child ->
         match child.Mwrite.Person_link.create_link with
         | `create_default_occ ->
             let sex =
               match child.Mwrite.Person_link.sex with
               | `male -> Male
               | `female -> Female
               | `unknown -> Neuter
             in
             let fn = child.Mwrite.Person_link.firstname in
             let sn = child.Mwrite.Person_link.lastname in
             let occ =
               match child.Mwrite.Person_link.occ with
               | Some occ -> Int32.to_int occ
               | None -> 0
             in
             (fn, sn, occ, Update.Create (sex, None), "", false)
         | `create ->
             let sex =
               match child.Mwrite.Person_link.sex with
               | `male -> Male
               | `female -> Female
               | `unknown -> Neuter
             in
             let fn = child.Mwrite.Person_link.firstname in
             let sn = child.Mwrite.Person_link.lastname in
             let occ = Api_update_util.api_find_free_occ base fn sn in
             (* On met à jour parce que si on veut le rechercher, *)
             (* il faut qu'on connaisse son occ.                  *)
             let () =
               if occ = 0 then child.Mwrite.Person_link.occ <- None
               else child.Mwrite.Person_link.occ <- Some (Int32.of_int occ)
             in
             (fn, sn, occ, Update.Create (sex, None), "", true)
         | `link ->
             let ip = Gwdb.iper_of_string @@ Int32.to_string child.Mwrite.Person_link.index in
             let p = poi base ip in
             let fn = sou base (get_first_name p) in
             let sn = sou base (get_surname p) in
             let occ =
               if fn = "?" || sn = "?"
               then int_of_string @@ Gwdb.string_of_iper (get_iper p)
               else get_occ p
             in
             (fn, sn, occ, Update.Link, "", false))
      mod_f.Mwrite.Family.children
  in
  (* Attention, surtout pas les witnesses, parce que si on en créé un, *)
  (* on le créé aussi dans witness et on ne pourra jamais valider.     *)
  let (marr, div, _) =
    UpdateFamOk.reconstitute_from_fevents (p_getenv conf.env "nsck" = Some "on") "" fevents
      (relation, marriage, marriage_place, marriage_note, marriage_src)
      divorce
  in
  let (relation, marriage, marriage_place,
       marriage_note, marriage_src) =
    marr
  in
  (* Si parents de même sex ... *)
  let relation =
    let father = mod_f.Mwrite.Family.father in
    let mother = mod_f.Mwrite.Family.mother in
    match (father.Mwrite.Person.sex, mother.Mwrite.Person.sex) with
    | (`male, `male) | (`female, `female) ->
        (match relation with
         | Married -> NoSexesCheckMarried
         | _ -> NoSexesCheckNotMarried)
    | _ -> relation
  in
  (* => pour l'instant, CheckItem ne vérifie pas le sex des parents. *)
  let divorce = div in
  let fam =
    {marriage = marriage; marriage_place = marriage_place;
     marriage_note = marriage_note; marriage_src = marriage_src;
     fevents = fevents; witnesses = Array.of_list witnesses;
     relation = relation; divorce = divorce; comment = comment;
     origin_file = origin_file; fsources = fsources; fam_index = fam_index}
  and cpl = Futil.parent conf.multi_parents (Array.of_list parents)
  and des = {children = Array.of_list children} in
  (* On vérifie s'il y a des conflits de personne. *)
  (* Normalement, il ne doit plus y avoir de lever *)
  (* de conflits par les autres modules : update,  *)
  (* updateIndOk et updateFamOk.                   *)
  let _err = Api_update_util.check_family_conflict base fam cpl des in
  (* Maintenant qu'on a fini les conflit, on remet l'objet person *)
  (* tel que pour GeneWeb, c'est à dire qu'on supprime l'option   *)
  (* force_create.                                                *)
  let witnesses_gw =
    List.map
      (fun (f, s, o, create, var, _) -> (f, s, o, create, var))
      witnesses
  in
  let fevents_gw =
    List.map
      (fun e ->
        let w =
          Array.map
            (fun ((f, s, o, create, var, _), wk) ->
              ((f, s, o, create, var), wk))
            e.efam_witnesses
        in
        {(e) with efam_witnesses = w})
      fevents
  in
  let parents_gw =
    List.map
      (fun (f, s, o, create, var, _) -> (f, s, o, create, var))
      parents
  in
  let children_gw =
    List.map
      (fun (f, s, o, create, var, _) -> (f, s, o, create, var))
      children
  in
  let fam =
    {(fam) with witnesses = Array.of_list witnesses_gw; fevents = fevents_gw}
  in
  let cpl = Futil.parent conf.multi_parents (Array.of_list parents_gw) in
  let des = {children = Array.of_list children_gw} in
  ((fam : (Update.key, ifam, string) gen_family), cpl, des)


(**/**)


let print_add conf base mod_f mod_fath mod_moth =
  (try
    let (sfam, scpl, sdes) = reconstitute_family conf base mod_f in
      (match UpdateFamOk.check_family conf sfam scpl with
      | (Some err, _) | (_, Some err) ->
          (* Correspond au cas ou fn/sn = ""/"?" *)
          (* => ne devrait pas se produire       *)
          Api_update_util.UpdateError err
      | (None, None) ->
          begin
            let (sfam, sdes) = UpdateFamOk.strip_family sfam sdes in
            let (ifam, fam, cpl, des) =
              UpdateFamOk.effective_add conf base sfam scpl sdes
            in
            let () = UpdateFamOk.patch_parent_with_pevents base cpl in
            let () = UpdateFamOk.patch_children_with_pevents base des in
            (* On met à jour les index ! et le digest ! *)
            let () =
              let fam = family_of_gen_family base (fam, cpl, des) in
              let ifath = get_father fam in
              let imoth = get_mother fam in
              let father = poi base ifath in
              let mother = poi base imoth in
              mod_f.Mwrite.Family.index <- Int32.of_string @@ Gwdb.string_of_ifam ifam;
              mod_fath.Mwrite.Person.index <- Int32.of_string @@ Gwdb.string_of_iper ifath;
              let fath_occ = get_occ father in
              mod_fath.Mwrite.Person.occ <-
                if fath_occ = 0 then None else Some (Int32.of_int fath_occ);
              mod_moth.Mwrite.Person.index <- Int32.of_string @@ Gwdb.string_of_iper imoth;
              let moth_occ = get_occ mother in
              mod_moth.Mwrite.Person.occ <-
                if moth_occ = 0 then None else Some (Int32.of_int moth_occ);
              let digest_father =
                Update.digest_person (UpdateInd.string_person_of base father)
              in
              mod_fath.Mwrite.Person.digest <- digest_father;
              let digest_mother =
                Update.digest_person (UpdateInd.string_person_of base mother)
              in
              mod_moth.Mwrite.Person.digest <- digest_mother;
              mod_f.Mwrite.Family.father <- mod_fath;
              mod_f.Mwrite.Family.mother <- mod_moth;
            in
            (* TODO ?? idem enfant/witness ? *)
            (* optim ? regarder que ceux dont index = 0 *)
            let (wl, ml) =
              UpdateFamOk.all_checks_family
                conf base ifam fam cpl des (scpl, sdes, None)
            in
            (* TODO *)
            let (changed, act) =
              let fam = Util.string_gen_family base fam in
              let (ip, act) =
                match p_getenv conf.env "ip" with
                | Some i ->
                  let i = Gwdb.iper_of_string i in
                    if (Adef.mother cpl) = i then
                      (Adef.mother cpl, "af")
                    else
                      let a = poi base i in
                      (match get_parents a with
                      | Some x when x = ifam -> (i, "aa")
                      | _ -> (Adef.father cpl, "af"))
                | None -> (Adef.father cpl, "af")
              in
              match act with
              | "af" ->
                  let gen_p =
                    Util.string_gen_person
                      base (gen_person_of_person (poi base ip))
                  in
                  (U_Add_family (gen_p, fam), "af")
              | _ ->
                  let gen_p =
                    Util.string_gen_person
                      base (gen_person_of_person (poi base ip))
                  in
                  (U_Add_parent (gen_p, fam), "aa")
            in
            let hr =
              [(fun () -> History.record conf base changed act);
               (fun () -> Update.delete_topological_sort conf base)]
            in
            Api_update_util.UpdateSuccess (wl, ml, hr)
          end)
  with
  | Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c)


let print_del conf base ip ifam =
  let fam = foi base ifam in
      begin
        UpdateFamOk.effective_del base ifam fam;
        let changed =
          let gen_p =
            let p =
              if ip = get_mother fam then poi base (get_mother fam)
              else poi base (get_father fam)
            in
            Util.string_gen_person base (gen_person_of_person p)
          in
          let gen_fam =
            Util.string_gen_family base (gen_family_of_family fam)
          in
          U_Delete_family (gen_p, gen_fam)
        in
        let hr =
          [(fun () -> History.record conf base changed "df");
           (fun () -> Update.delete_topological_sort conf base)]
        in
        Api_update_util.UpdateSuccess ([], [], hr)
      end


let print_mod_aux conf base mod_f callback =
  try
    let (sfam, scpl, sdes) = reconstitute_family conf base mod_f in
      match UpdateFamOk.check_family conf sfam scpl with
      | (Some err, _) | (_, Some err) ->
          (* Correspond au cas ou fn/sn = "" ou "?" *)
          (* => ne devrait pas se produire *)
          Api_update_util.UpdateError err
      | (None, None) ->
          let (sfam, sdes) = UpdateFamOk.strip_family sfam sdes in
          callback sfam scpl sdes
  with
  | Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c


let print_mod conf base ip mod_f =
  let ifam = Gwdb.ifam_of_string @@ Int32.to_string mod_f.Mwrite.Family.index in
  let o_f =
    Util.string_gen_family
      base (gen_family_of_family (foi base ifam))
  in
  let callback sfam scpl sdes =
    begin
      let ofs = UpdateFamOk.family_structure base sfam.fam_index in
      let (ifam, fam, cpl, des) =
        UpdateFamOk.effective_mod conf base sfam scpl sdes
      in
      let () = UpdateFamOk.patch_parent_with_pevents base cpl in
      let () = UpdateFamOk.patch_children_with_pevents base des in
      let s =
        let sl =
          [fam.comment; fam.fsources; fam.marriage_note; fam.marriage_src]
        in
        let rec loop l accu =
          match l with
          | [] -> accu
          | evt :: l -> loop l (evt.efam_note :: evt.efam_src :: accu)
        in
        let sl = loop (fam.fevents) sl in
        String.concat " " (List.map (sou base) sl)
      in
      Notes.update_notes_links_db conf (NotesLinks.PgFam ifam) s;
      let nfs = (Adef.parent_array cpl, des.children) in
      let onfs = Some (ofs, nfs) in
      let (wl, ml) =
        UpdateFamOk.all_checks_family
          conf base ifam fam cpl des (scpl, sdes, onfs)
      in
      let changed =
        let p =
          Util.string_gen_person
            base (gen_person_of_person (poi base ip))
        in
        let n_f = Util.string_gen_family base fam in
        U_Modify_family (p, o_f, n_f)
      in
      let hr =
        [(fun () -> History.record conf base changed "mf");
         (fun () -> Update.delete_topological_sort conf base)]
      in
      Api_update_util.UpdateSuccess (wl, ml, hr)
    end
  in
  print_mod_aux conf base mod_f callback

#endif
