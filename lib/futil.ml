(* $Id: futil.ml,v 5.5 2007-03-05 05:18:23 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Adef
open Def

let map_title_strings f t =
  let t_name =
    match t.t_name with
      Tmain -> Tmain
    | Tname s -> Tname (f s)
    | Tnone -> Tnone
  in
  let t_ident = f t.t_ident in
  let t_place = f t.t_place in
  {t_name = t_name; t_ident = t_ident; t_place = t_place;
   t_date_start = t.t_date_start; t_date_end = t.t_date_end; t_nth = t.t_nth}

let map_pers_event fp fs e =
  let epers_name =
    match e.epers_name with
      Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial |
      Epers_Cremation | Epers_Accomplishment | Epers_Acquisition |
      Epers_Adhesion | Epers_BaptismLDS | Epers_BarMitzvah |
      Epers_BatMitzvah | Epers_Benediction | Epers_ChangeName |
      Epers_Circumcision | Epers_Confirmation | Epers_ConfirmationLDS |
      Epers_Decoration | Epers_DemobilisationMilitaire | Epers_Diploma |
      Epers_Distinction | Epers_Dotation | Epers_DotationLDS |
      Epers_Education | Epers_Election | Epers_Emigration |
      Epers_Excommunication | Epers_FamilyLinkLDS | Epers_FirstCommunion |
      Epers_Funeral | Epers_Graduate | Epers_Hospitalisation | Epers_Illness |
      Epers_Immigration | Epers_ListePassenger | Epers_MilitaryDistinction |
      Epers_MilitaryPromotion | Epers_MilitaryService |
      Epers_MobilisationMilitaire | Epers_Naturalisation | Epers_Occupation |
      Epers_Ordination | Epers_Property | Epers_Recensement |
      Epers_Residence | Epers_Retired | Epers_ScellentChildLDS |
      Epers_ScellentParentLDS | Epers_ScellentSpouseLDS | Epers_VenteBien |
      Epers_Will as evt ->
        evt
    | Epers_Name s -> Epers_Name (fs s)
  in
  let epers_date = e.epers_date in
  let epers_place = fs e.epers_place in
  let epers_reason = fs e.epers_reason in
  let epers_note = fs e.epers_note in
  let epers_src = fs e.epers_src in
  let epers_witnesses = Array.map (fun (p, w) -> fp p, w) e.epers_witnesses in
  {epers_name = epers_name; epers_date = epers_date;
   epers_place = epers_place; epers_reason = epers_reason;
   epers_note = epers_note; epers_src = epers_src;
   epers_witnesses = epers_witnesses}

let map_fam_event fp fs e =
  let efam_name =
    match e.efam_name with
      Efam_Marriage | Efam_NoMarriage | Efam_NoMention | Efam_Engage |
      Efam_Divorce | Efam_Separated | Efam_Annulation | Efam_MarriageBann |
      Efam_MarriageContract | Efam_MarriageLicense | Efam_PACS |
      Efam_Residence as evt ->
        evt
    | Efam_Name s -> Efam_Name (fs s)
  in
  let efam_date = e.efam_date in
  let efam_place = fs e.efam_place in
  let efam_reason = fs e.efam_reason in
  let efam_note = fs e.efam_note in
  let efam_src = fs e.efam_src in
  let efam_witnesses = Array.map (fun (p, w) -> fp p, w) e.efam_witnesses in
  {efam_name = efam_name; efam_date = efam_date; efam_place = efam_place;
   efam_reason = efam_reason; efam_note = efam_note; efam_src = efam_src;
   efam_witnesses = efam_witnesses}

let map_relation_ps fp fs r =
  {r_type = r.r_type;
   r_fath =
     begin match r.r_fath with
       Some x -> Some (fp x)
     | None -> None
     end;
   r_moth =
     begin match r.r_moth with
       Some x -> Some (fp x)
     | None -> None
     end;
   r_sources = fs r.r_sources}

let map_person_ps fp fs p =
  {first_name = fs p.first_name; surname = fs p.surname; occ = p.occ;
   image = fs p.image;
   first_names_aliases = List.map fs p.first_names_aliases;
   surnames_aliases = List.map fs p.surnames_aliases;
   public_name = fs p.public_name; qualifiers = List.map fs p.qualifiers;
   titles = List.map (map_title_strings fs) p.titles;
   rparents = List.map (map_relation_ps fp fs) p.rparents;
   related = p.related; aliases = List.map fs p.aliases;
   occupation = fs p.occupation; sex = p.sex; access = p.access;
   birth = p.birth; birth_place = fs p.birth_place;
   birth_note = fs p.birth_note; birth_src = fs p.birth_src;
   baptism = p.baptism; baptism_place = fs p.baptism_place;
   baptism_note = fs p.baptism_note; baptism_src = fs p.baptism_src;
   death = p.death; death_place = fs p.death_place;
   death_note = fs p.death_note; death_src = fs p.death_src;
   burial = p.burial; burial_place = fs p.burial_place;
   burial_note = fs p.burial_note; burial_src = fs p.burial_src;
   pevents = List.map (map_pers_event fp fs) p.pevents; notes = fs p.notes;
   psources = fs p.psources; key_index = p.key_index}

let map_union_f ff u = {family = Array.map ff u.family}

let map_family_ps fp fs fam =
  {marriage = fam.marriage; marriage_place = fs fam.marriage_place;
   marriage_note = fs fam.marriage_note; marriage_src = fs fam.marriage_src;
   witnesses = Array.map fp fam.witnesses; relation = fam.relation;
   divorce = fam.divorce;
   fevents = List.map (map_fam_event fp fs) fam.fevents;
   comment = fs fam.comment; origin_file = fs fam.origin_file;
   fsources = fs fam.fsources; fam_index = fam.fam_index}

let parent multi parent =
  if not multi then Adef.parent parent else Adef.multi_parent parent

let map_couple_p multi_parents fp cpl =
  parent multi_parents (Array.map fp (parent_array cpl))

let map_descend_p fp des = {children = Array.map fp des.children}

let gen_person_misc_names first_name surname public_name qualifiers aliases
    first_names_aliases surnames_aliases titles husbands
    father_titles_places =
  let first_name = Mutil.nominative first_name in
  let surname = Mutil.nominative surname in
  if first_name = "?" || surname = "?" then []
  else
    let public_names =
      let titles_names =
        let tnl = ref [] in
        List.iter
          (fun t ->
             match t.t_name with
               Tmain | Tnone -> ()
             | Tname x -> tnl := x :: !tnl)
          titles;
        !tnl
      in
      if public_name = "" || titles = [] then titles_names
      else public_name :: titles_names
    in
    let first_names =
      let pn =
        if public_name <> "" && titles = [] then public_name :: public_names
        else public_names
      in
      first_name :: (first_names_aliases @ pn)
    in
    let surnames =
      surname ::
      (Mutil.surnames_pieces surname @ surnames_aliases @ qualifiers)
    in
    let surnames =
      List.fold_left
        (fun list (husband_surname, husband_surnames_aliases) ->
           let husband_surname = Mutil.nominative husband_surname in
           if husband_surname = "?" then husband_surnames_aliases @ list
           else
             husband_surname ::
             (Mutil.surnames_pieces husband_surname @
              husband_surnames_aliases @ list))
        surnames husbands
    in
    let list = public_names in
    let list =
      List.fold_left
        (fun list f ->
           List.fold_left (fun list s -> (f ^ " " ^ s) :: list) list surnames)
        list first_names
    in
    let list =
      let first_names = first_name :: first_names_aliases in
      List.fold_left
        (fun list t ->
           let s = t.t_place in
           if s = "" then list
           else
             let first_names =
               match t.t_name with
                 Tname f -> f :: first_names
               | Tmain | Tnone ->
                   let f = public_name in
                   if f = "" then first_names else f :: first_names
             in
             List.fold_left (fun list f -> (f ^ " " ^ s) :: list) list
               first_names)
        list titles
    in
    let list =
      if father_titles_places = [] then list
      else
        let first_names = first_name :: first_names_aliases in
        List.fold_left
          (fun list s ->
             if s = "" then list
             else
               List.fold_left (fun list f -> (f ^ " " ^ s) :: list) list
                 first_names)
          list father_titles_places
    in
    let list = List.rev_append aliases list in
    let fn = Name.lower (first_name ^ " " ^ surname) in
    List.fold_left
      (fun list s ->
         let s = Name.lower s in
         if s = fn || List.mem s list then list else s :: list)
      [] list

let rec eq_lists eq l1 l2 =
  match l1, l2 with
    x1 :: l1, x2 :: l2 -> eq x1 x2 && eq_lists eq l1 l2
  | [], [] -> true
  | _ -> false

let eq_title_names eq tn1 tn2 =
  match tn1, tn2 with
    Tname i1, Tname i2 -> eq i1 i2
  | Tmain, Tmain | Tnone, Tnone -> true
  | _ -> false

let eq_titles eq t1 t2 =
  eq_title_names eq t1.t_name t2.t_name && eq t1.t_ident t2.t_ident &&
  eq t1.t_place t2.t_place && t1.t_date_start = t2.t_date_start &&
  t1.t_date_end = t2.t_date_end && t1.t_nth = t2.t_nth
