open Def
open Gwdb

type base_error = person error
type base_warning = (iper, person, ifam, family, title, pers_event, fam_event) warning
type base_misc = (person, family, title) misc

(* Constants used for computing the warnings. *)
let max_age_btw_cpl = 50
let max_days_btw_sibl = 10
let max_month_btw_sibl = 7
let lim_date_death = 1900
let max_death_after_lim_date_death = 105
let max_death_before_lim_date_death = 100
let min_parent_age = 11
let max_father_age = 70
let max_mother_age = 55
let lim_date_marriage = 1850
let min_age_marriage = 13
let max_age_marriage = 100
let average_marriage_age = 20
let max_siblings_gap = 50

(* Check if d1 < d2 *)
let strictly_before_dmy d1 d2 =
  match Date.compare_dmy_opt ~strict:true d1 d2 with
  | Some x -> x < 0
  | None -> false

let strictly_before d1 d2 =
  match d1, d2 with
  | Dgreg (d1, _), Dgreg (d2, _) -> strictly_before_dmy d1 d2
  | _ -> false

let strictly_after_dmy d1 d2 =
  match Date.compare_dmy_opt ~strict:true d1 d2 with
  | Some x -> x > 0
  | None -> false

let strictly_after d1 d2 =
  match d1, d2 with
  | Dgreg (d1, _), Dgreg (d2, _) -> strictly_after_dmy d1 d2
  | _ -> false

let strictly_younger age year =
  match age.prec with
  | After -> false
  | _ -> age.year < year

let strictly_older age year =
  match age.prec with
  | Before -> false
  | _ -> age.year > year

let odate = function
  | Some (Dgreg (d, _)) -> Some d
  | _ -> None

let obirth x =
  Adef.od_of_cdate (get_birth x) |> odate

type 'string event_name =
    Psort of 'string gen_pers_event_name
  | Fsort of 'string gen_fam_event_name

(*
   On ignore les événements personnalisés.
   Dans l'ordre de priorité :
     birth, baptism, ..., death, funeral, burial/cremation.
   Pour les évènements familiaux, cet ordre est envisageable :
     engage, PACS, marriage bann, marriage contract, marriage, ...,
     separate, divorce
*)
let compare_event_name name1 name2 =
  match name1, name2 with
    Psort Epers_Birth, _ -> -1
  | _, Psort Epers_Birth -> 1
  | Psort Epers_Baptism
  , ( Psort Epers_Death
    | Psort Epers_Funeral
    | Psort Epers_Burial
    | Psort Epers_Cremation ) ->
    -1
  | ( Psort Epers_Death
    | Psort Epers_Funeral
    | Psort Epers_Burial
    | Psort Epers_Cremation )
  , Psort Epers_Baptism ->
    1
  | Psort Epers_Burial, _ | Psort Epers_Cremation, _ -> 1
  | _, Psort Epers_Burial | _, Psort Epers_Cremation -> -1
  | Psort Epers_Funeral, _ -> 1
  | _, Psort Epers_Funeral -> -1
  | Psort Epers_Death, _ -> 1
  | _, Psort Epers_Death -> -1
  | _ -> 0

let cmp_events get_name get_date e1 e2 =
  match Adef.od_of_cdate (get_date e1) with
  | Some (Dgreg (d1, _)) -> begin
      match Adef.od_of_cdate (get_date e2) with
      | Some (Dgreg (d2, _)) ->
        begin match Date.compare_dmy_opt ~strict:false d1 d2 with
          | Some 0 | None -> compare_event_name (get_name e1) (get_name e2)
          | Some x -> x
        end
      | _ -> compare_event_name (get_name e1) (get_name e2)
    end
  | _ -> compare_event_name (get_name e1) (get_name e2)

let sort_events get_name get_date events =
  List.stable_sort (fun e1 e2 -> cmp_events get_name get_date e1 e2) events

let merge_events get_name get_date l1 l2 =
  List.merge (fun e1 e2 -> cmp_events get_name get_date e1 e2) l1 l2

let changed_pevents_order warning p =
  let a =
    sort_events
      (fun evt -> Psort evt.epers_name) (fun evt -> evt.epers_date)
      (get_pevents p)
  in
  let b = get_pevents p in
  if compare b a <> 0 then warning (ChangedOrderOfPersonEvents (p, b, a))

let changed_fevents_order warning (ifam, fam) =
  let a =
    sort_events (fun evt -> Fsort evt.efam_name) (fun evt -> evt.efam_date)
      (get_fevents fam)
  in
  let b = get_fevents fam in
  if compare b a <> 0 then warning (ChangedOrderOfFamilyEvents (ifam, b, a))

let titles_after_birth warning p t =
  let t_date_start = Adef.od_of_cdate t.t_date_start in
  let t_date_end = Adef.od_of_cdate t.t_date_end in
  begin match t_date_start, t_date_end with
    Some d1, Some d2 ->
      if strictly_after d1 d2 then warning (TitleDatesError (p, t))
  | _ -> ()
  end;
  match Adef.od_of_cdate (get_birth p) with
    Some d1 ->
      begin match t_date_start with
        Some d -> if strictly_after d1 d then warning (TitleDatesError (p, t))
      | None -> ()
      end;
      begin match t_date_end with
        Some d -> if strictly_after d1 d then warning (TitleDatesError (p, t))
      | None -> ()
      end;
      ()
  | _ -> ()

let check_person_age base warning p =
  let aux d1 d2 =
    let a = Date.time_elapsed d1 d2 in
    if d2.year > lim_date_death then begin
      if strictly_older a max_death_after_lim_date_death
      then warning (DeadOld (p, a))
    end else if strictly_older a max_death_before_lim_date_death
    then warning (DeadOld (p, a))
  in
  (* On pourrait faire un calcul sur la descendance ou l'ascendance si  *)
  (* on ne trouve rien ... mais c'est peut être un peu trop gourmand    *)
  (* juste pour un warning ?                                            *)
  match Date.date_of_death (get_death p) with
  | Some (Dgreg (d2, _)) ->
    begin
      match Adef.od_of_cdate (get_birth p) with
      | Some (Dgreg (d, _)) -> aux d d2
      | _ -> match Adef.od_of_cdate (get_baptism p) with
        | Some (Dgreg (d, _)) -> aux d d2
        | _ ->
          let rec loop i =
            if i >= Array.length (get_family p) then ()
            else
              let fam = foi base (get_family p).(i) in
              match Adef.od_of_cdate (get_marriage fam) with
              | Some (Dgreg (d, _)) ->
                aux { d with year = d.year - average_marriage_age } d2
              | _ -> loop (i + 1)
        in
        loop 0
    end
  | _ -> ()

let try_to_fix_relation_sex base warning p_ref =
  let p_index = Some (get_iper p_ref) in
  let fixed = ref 0 in
  let not_fixed = ref 0 in
  let changed_related =
    List.fold_right
      (fun ip changed_related ->
         let p = poi base ip in
         let (rparents, changed, not_changed) =
           List.fold_right
             (fun rel (rparents, changed, not_changed) ->
                let (rel, changed, not_changed) =
                  match p_index = rel.r_fath, p_index = rel.r_moth with
                    true, false ->
                      if get_sex p_ref = Female then
                        match rel.r_moth with
                          Some ip ->
                            let oth_p = poi base ip in
                            if get_sex oth_p = Male then
                              let rel =
                                {rel with r_fath = rel.r_moth;
                                 r_moth = p_index}
                              in
                              rel, changed + 1, not_changed
                            else rel, changed, not_changed + 1
                        | None ->
                            let rel =
                              {rel with r_fath = None; r_moth = p_index}
                            in
                            rel, changed + 1, not_changed
                      else rel, changed, not_changed
                  | false, true ->
                      if get_sex p_ref = Male then
                        match rel.r_fath with
                          Some ip ->
                            let oth_p = poi base ip in
                            if get_sex oth_p = Female then
                              let rel =
                                {rel with r_moth = rel.r_fath;
                                 r_fath = p_index}
                              in
                              rel, changed + 1, not_changed
                            else rel, changed, not_changed + 1
                        | None ->
                            let rel =
                              {rel with r_moth = None; r_fath = p_index}
                            in
                            rel, changed + 1, not_changed
                      else rel, changed, not_changed
                  | false, false -> rel, changed, not_changed
                  | true, true -> rel, changed, not_changed + 1
                in
                rel :: rparents, changed, not_changed)
             (get_rparents p) ([], 0, 0)
         in
         fixed := !fixed + changed;
         not_fixed := !not_fixed + not_changed ;
         if changed > 0 then (ip, p, None, Some rparents) :: changed_related
         else changed_related)
      (get_related p_ref) []
  in
  warning (IncoherentSex (p_ref, !fixed, !not_fixed));
  if !fixed > 0 then Some changed_related else None

let related_sex_is_coherent base warning p_ref =
  let p_index = Some (get_iper p_ref) in
  let merge_sex g1 g2 =
    match g1, g2 with
      Some Male, Some Male -> Some Male
    | Some Female, Some Female -> Some Female
    | Some Neuter, Some g -> Some g
    | Some g, Some Neuter -> Some g
    | _ -> None
  in
  let check_sex sex rparents =
    List.fold_left
      (fun g rel ->
         match p_index = rel.r_fath, p_index = rel.r_moth with
           true, false -> merge_sex g (Some Male)
         | false, true -> merge_sex g (Some Female)
         | false, false -> g
         | true, true -> None)
      sex rparents
  in
  let new_sex =
    List.fold_left
      (fun g ip -> let p = poi base ip in check_sex g (get_rparents p))
      (Some (get_sex p_ref)) (get_related p_ref)
  in
  match new_sex with
    Some g ->
      if get_sex p_ref != g then
        Some [get_iper p_ref, p_ref, Some g, None]
      else None
  | None -> try_to_fix_relation_sex base warning p_ref

let check_difference_age_between_cpl warning fath moth =
  let find_date p =
    match Adef.od_of_cdate (get_birth p) with
    | Some (Dgreg (d, _)) -> Some d
    | _ -> match Adef.od_of_cdate (get_baptism p) with
      | Some (Dgreg (d, _)) -> Some d
      | _ -> None
  in
  match find_date fath with
  | None -> ()
  | Some d1 ->
    match find_date moth with
    | None -> ()
    | Some d2 ->
      let a = Date.time_elapsed d1 d2 in
      if strictly_older a max_age_btw_cpl
      then warning (BigAgeBetweenSpouses (fath, moth, a))

(*
 * Semi sort children by birth dates.
 * If all children have birth dates, no problem.
 * Otherwise, sorting groups of consecutive children who have dates.
 * In not possible cases, try to keep order of children of same sex.
 *   ex: G1, B2 being resp. girl and boy with date(G1) < date(B2)
 *       and G and B begin resp. girls boys without dates
 *     if order is ... B2 B B B G1 ... it becomes ... G1 B2 B B B ...
 *     if order is ... B2 G G G G1 ... it becomes ... G G G G1 B2 ...
 *     if order is ... B2 G B G G1 ... no change (a warning appears).
 *)

let semi_sort base a before comp di =
  let rec loop i =
    if i < 0 || i >= Array.length a then ()
    else
      let p1 = poi base a.(i) in
      let d1 =
        match Adef.od_of_cdate (get_birth p1) with
          Some d1 -> Some d1
        | None -> Adef.od_of_cdate (get_baptism p1)
      in
      match d1 with
        Some d1 ->
          let rec loop_j sex_interm_sib j =
            if j < 0 || j >= Array.length a then loop (i + di)
            else
              let p2 = poi base a.(j) in
              let d2 =
                match Adef.od_of_cdate (get_birth p2) with
                  Some d2 -> Some d2
                | None -> Adef.od_of_cdate (get_baptism p2)
              in
              match d2 with
                Some d2 ->
                  if comp d1 d2 then
                    let j =
                      match sex_interm_sib with
                        Some s ->
                          if s = get_sex p1 then None
                          else if s = get_sex p2 then Some j
                          else None
                      | None -> Some j
                    in
                    match j with
                      Some j ->
                        let k =
                          let rec loop_k k =
                            if k < 0 || k >= Array.length a then k + di
                            else
                              let p3 = poi base a.(k) in
                              let d3 =
                                match Adef.od_of_cdate (get_birth p3) with
                                  Some d3 -> Some d3
                                | None -> Adef.od_of_cdate (get_baptism p3)
                              in
                              match d3 with
                                Some d3 ->
                                  if comp d1 d3 then loop_k (k - di)
                                  else k + di
                              | None -> k + di
                          in
                          loop_k (j - di)
                        in
                        begin match !before with
                          Some _ -> ()
                        | None -> before := Some (Array.copy a)
                        end;
                        let ip = a.(i) in
                        begin let rec loop_up j =
                          if j = k then ()
                          else begin a.(j) <- a.(j-di); loop_up (j - di) end
                        in
                          loop_up i
                        end;
                        a.(k) <- ip;
                        loop (i + di)
                    | None -> loop (i + di)
                  else loop (i + di)
              | None ->
                  match sex_interm_sib with
                    Some s ->
                      if s = get_sex p2 then loop_j sex_interm_sib (j - di)
                      else loop (i + di)
                  | None -> loop_j (Some (get_sex p2)) (j - di)
          in
          loop_j None (i - di)
      | None -> loop (i + di)
  in
  loop

let sort_children base children =
  let before = ref None in
  semi_sort base children before strictly_before 1 1;
  semi_sort base children before strictly_after (~-1) 1;
  semi_sort base children before strictly_before 1 1;
  match !before with
    Some b -> Some (b, children)
  | None -> None

let changed_marriages_order base warning p =
  let b = Array.copy (get_family p) in
  (* Astuce : on construire un tableau identique à la famille dans *)
  (* lequel on remplace toutes les dates inconnues par la dernière *)
  (* date maximale que l'on ait vu.                                *)
  (* Exemple : Ma (mariage sans date), et M3 après M1              *)
  (* ordre initial Ma M5 Mb M3 M1 ... devient Ma M1 M3 M5 Mb       *)
  let (_, a) =
    Array.fold_left
      (fun (max_date, tab) ifam ->
         let fam = foi base ifam in
         let date =
           match Adef.od_of_cdate (get_marriage fam) with
             Some d -> Some d
           | None -> max_date
         in
         let max_date =
           match date, max_date with
             Some d1, Some d2 ->
               if Date.compare_date d1 d2 = 1 then Some d1 else Some d2
           | Some d1, None -> Some d1
           | _ -> max_date
         in
         max_date, Array.append tab [| ifam, date |])
      (None, [| |]) (get_family p)
  in
  Array.stable_sort
    (fun (_f1, d1) (_f2, d2) ->
       match d1, d2 with
         Some d1, Some d2 -> Date.compare_date d1 d2
       | _ -> 0)
    a;
  let a = Array.map (fun (f, _) -> f) a in
  if a <> b then
    begin
      warning (ChangedOrderOfMarriages (p, b, a));
      let rec loop i fam =
        if i = Array.length fam then ()
        else begin fam.(i) <- a.(i); loop (i + 1) fam end
      in
      loop 0 (get_family p)
    end

let close_siblings warning x np ifam des =
  match np with
  | Some (elder, d1) ->
    begin match odate @@ Adef.od_of_cdate (get_birth x) with
      | Some d2 ->
        let d = Date.time_elapsed d1 d2 in
        (* On vérifie les jumeaux ou naissances proches. *)
        if d.year = 0
        && (d.month < max_month_btw_sibl)
        && (d.month <> 0 || d.day >= max_days_btw_sibl)
        then warning (CloseChildren (ifam, des, elder, x))
      | _ -> ()
    end
  | _ -> ()

let born_after_his_elder_sibling warning x b np ifam des =
  match np with
  | None -> ()
  | Some (elder, d1) ->
    match b with
    | Some d2 ->
      if strictly_after_dmy d1 d2 then
        warning (ChildrenNotInOrder (ifam, des, elder, x))
    | None -> match odate @@ Date.date_of_death (get_death x) with
      | Some d2 ->
        if strictly_after_dmy d1 d2 then
          warning (ChildrenNotInOrder (ifam, des, elder, x))
      | None -> ()

let siblings_gap gap child = function
  | None -> gap
  | Some b ->
    match gap with
    | None -> Some ((b, child), (b, child))
    | Some ((min, minp), (max, maxp)) ->
      Some
        ( (if strictly_before_dmy b min then (b, child) else (min, minp))
        , (if strictly_after_dmy b max then (b, child) else (max, maxp)) )

let child_born_after_his_parent warning x parent =
  match Adef.od_of_cdate (get_birth parent) with
  | Some (Dgreg (g1, _)) ->
    begin match Adef.od_of_cdate (get_birth x) with
      | Some (Dgreg (g2, _)) ->
        if strictly_after_dmy g1 g2 then warning (ParentBornAfterChild (parent, x))
        else
          let a = Date.time_elapsed g1 g2 in
          if strictly_younger a min_parent_age
          then warning (ParentTooYoung (parent, a))
          else if (get_sex parent = Female && strictly_older a max_mother_age)
               || (get_sex parent = Male && strictly_older a max_father_age)
          then warning (ParentTooOld (parent, a))
      | _ -> match Date.date_of_death (get_death x) with
        | Some (Dgreg (g2, _)) ->
          if strictly_after_dmy g1 g2 then warning (ParentBornAfterChild (parent, x))
          else
            let a = Date.time_elapsed g1 g2 in
            if strictly_younger a min_parent_age
            then warning (ParentTooYoung (parent, a))
        | _ -> ()
    end
  | _ -> ()

let child_born_before_mother_death warning x mother =
  match Adef.od_of_cdate (get_birth x) with
  | Some (Dgreg (d1, _)) ->
    begin
      match Date.date_of_death @@ get_death mother with
      | Some (Dgreg (d2, _)) ->
        if strictly_after_dmy d1 d2
        then warning (MotherDeadAfterChildBirth (mother, x))
      | _ -> ()
    end
  | _ -> ()

let possible_father warning x father =
  match Adef.od_of_cdate (get_birth x) with
  | Some (Dgreg (d1, _)) when d1.prec <> Before ->
    begin
      match Date.date_of_death (get_death father) with
      | Some (Dgreg (d2, _)) when d2.prec <> After ->
        let a2 =
          match d2 with
          | {prec = YearInt dmy2} -> dmy2.year2
          | {prec = OrYear dmy2} -> dmy2.year2
          | {year = a} -> a
        in
        if d1.year > a2 + 1 then warning (DeadTooEarlyToBeFather (father, x))
      | _ -> ()
    end
  | _ -> ()

let child_has_sex warning child =
  if get_sex child = Neuter then warning (UndefinedSex child)

let check_order_pfevents get_name get_date warning p events =
  let events = sort_events get_name get_date events in
  let rec loop = function
    | e1 :: e2 :: events ->
      if compare_event_name (get_name e1) (get_name e2) = 1
      then warning p e1 e2 ;
      loop (e2 :: events)
    | _ -> ()
  in
  loop events

let check_order_pevents warning p =
  check_order_pfevents
    (fun evt -> Psort evt.epers_name)
    (fun evt -> evt.epers_date)
    (fun p e1 e2 -> warning (PEventOrder (p, e1, e2)))
    (p)
    (get_pevents p)

let check_witness_pevents base warning p =
  List.iter begin fun evt ->
    match Adef.od_of_cdate evt.epers_date with
    | Some (Dgreg (d2, _)) ->
      Array.iter begin fun (iw, _) ->
        let p = poi base iw in
        begin match Adef.od_of_cdate (get_birth p) with
          | Some (Dgreg (d1, _)) ->
            if strictly_before_dmy d2 d1
            then warning (PWitnessEventBeforeBirth (p, evt))
          | _ -> ()
        end;
        match Date.date_of_death @@ get_death p with
        | Some (Dgreg (d3, _)) ->
          if strictly_after_dmy d2 d3
          then warning (PWitnessEventAfterDeath (p, evt))
        | _ -> ()
      end evt.epers_witnesses
    | _ -> ()
  end (get_pevents p)

let check_pevents base warning p =
  check_order_pevents warning p ;
  check_witness_pevents base warning p

let check_children ?(onchange = true) base warning (ifam, fam) fath moth =
  let children =
    if onchange then
      let b = get_children fam in
      match sort_children base b with
      | None -> b
      | Some (b, a) ->
        warning (ChangedOrderOfChildren (ifam, fam, b, a)) ;
        a
    else get_children fam
  in
  let (_, gap) =
    Array.fold_left begin fun (np, gap) child ->
      let child = poi base child in
      let b = obirth child in
      let gap = siblings_gap gap child b in
      check_pevents base warning child;
      born_after_his_elder_sibling warning child b np ifam fam;
      close_siblings warning child np ifam fam;
      child_born_after_his_parent warning child fath;
      child_born_after_his_parent warning child moth;
      child_born_before_mother_death warning child moth;
      possible_father warning child fath;
      child_has_sex warning child;
      let np = match b with
        | Some d -> Some (child, d)
        | _ -> np
      in
      (np, gap)
    end (None, None) children
  in
  match gap with
  | Some ((d1, p1), (d2, p2)) ->
    let e = Date.time_elapsed d1 d2 in
    if e.year > max_siblings_gap then warning (BigAgeBetweenSiblings (p1, p2, e))
   | _ -> ()

let has_family_sources fam =
  not
    (is_empty_string (get_fsources fam) &&
     is_empty_string (get_marriage_src fam))

let has_person_sources p =
  not
    (is_empty_string (get_psources p) &&
     is_empty_string (get_baptism_src p) &&
     is_empty_string (get_birth_src p) && is_empty_string (get_death_src p) &&
     is_empty_string (get_burial_src p))


(* ************************************************************************* *)
(*  [Fonc] check_sources :
      base -> (Def.misc -> unit) -> ifam -> family -> unit                   *)
(** [Description] : Il y a un avertissment 'miscellaneous' si aucune des
                    personnes (conjoint1 ET conjoint2) n'a de sources
                    (indiduelles ou familliales).
    [Args] :
      - base : base
      - misc : fonction qui ajoute un misc à la liste des miscs
      - ifam : ifam
      - fam  : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let check_sources base misc ifam fam =
  if has_family_sources fam then ()
  else
    let cpl = foi base ifam in
    let fath = poi base (get_father cpl) in
    let moth = poi base (get_mother cpl) in
    if has_person_sources fath && has_person_sources moth then ()
    else misc MissingSources

let check_order_fevents base warning fam =
  let p = poi base (get_father fam) in
  check_order_pfevents
    (fun evt -> Fsort evt.efam_name)
    (fun evt -> evt.efam_date)
    (fun p e1 e2 -> warning (FEventOrder (p, e1, e2)))
    (p)
    (get_fevents fam)

let check_witness_fevents base warning fam =
  List.iter begin fun evt ->
    match Adef.od_of_cdate evt.efam_date with
    | Some (Dgreg (d2, _)) ->
      Array.iter begin fun (iw, _) ->
        let p = poi base iw in
        begin match Adef.od_of_cdate (get_birth p) with
          | Some (Dgreg (d1, _)) ->
            if strictly_before_dmy d2 d1
            then warning (FWitnessEventBeforeBirth (p, evt))
          | _ -> ()
        end;
        match Date.date_of_death @@ get_death p with
        | Some (Dgreg (d3, _)) ->
          if strictly_after_dmy d2 d3
          then warning (FWitnessEventAfterDeath (p, evt))
        | _ -> ()
      end evt.efam_witnesses
    | _ -> ()
  end (get_fevents fam)

let check_parent_marriage_age warning fam p =
  let rec loop = function
    | [] -> ()
    | { efam_name = (Efam_Marriage|Efam_PACS) ; efam_date ; _ } :: list ->
        begin match Adef.od_of_cdate efam_date with
          | Some (Dgreg (g2, _) as d2) ->
            begin match Adef.od_of_cdate (get_birth p) with
              | Some (Dgreg (g1, _) as d1) ->
                if strictly_before d2 d1
                then warning (MarriageDateBeforeBirth p)
                else if g2.year > lim_date_marriage
                then
                  let e = Date.time_elapsed g1 g2 in
                  if strictly_younger e min_age_marriage
                  then warning (YoungForMarriage (p, e))
                  else if strictly_older e max_age_marriage
                  then warning (OldForMarriage (p, e))
                  else loop list
                else loop list
              | _ -> loop list
            end
          | _ -> loop list
        end
      | _ :: list -> loop list
  in
  loop (get_fevents fam)

let check_parents warning fam fath moth =
  check_parent_marriage_age warning fam fath ;
  check_parent_marriage_age warning fam moth ;
  check_difference_age_between_cpl warning fath moth

(* main *)

let person ?(onchange = true) base warning p =
  check_pevents base warning p;
  check_person_age base warning p;
  List.iter (titles_after_birth warning p) (get_titles p);
  if onchange then changed_pevents_order warning p ;
  related_sex_is_coherent base warning p

let family ?(onchange = true) base warning ifam fam =
  let fath = poi base @@ get_father fam in
  let moth = poi base @@ get_mother fam in
  check_order_fevents base warning fam ;
  check_witness_fevents base warning fam ;
  check_parents warning fam fath moth ;
  check_children ~onchange base warning (ifam, fam) fath moth ;
  if onchange then begin
    changed_fevents_order warning (ifam, fam);
    let father = poi base (get_father fam) in
    let mother = poi base (get_mother fam) in
    changed_marriages_order base warning father;
    changed_marriages_order base warning mother
  end

let on_person_update base warning p =
  begin match get_parents p with
    | Some i ->
      let fam = foi base i in
      let fath = poi base @@ get_father fam in
      let moth = poi base @@ get_mother fam in
      check_parents warning fam fath moth ;
      child_born_after_his_parent warning p fath ;
      child_born_after_his_parent warning p moth
    | _ -> ()
  end ;
  Array.iter begin fun ifam ->
    let fam = foi base ifam in
    Array.iter begin fun child ->
      let child = poi base child in
      child_born_after_his_parent warning child p ;
      match get_sex p with
      | Male -> possible_father warning child p ;
      | Female -> child_born_before_mother_death warning child p
      | Neuter -> ()
    end (get_children fam)
  end (get_family p)

(* ************************************************************************* *)
(*  [Fonc] check_other_fields :
      base -> (Def.misc -> unit) -> ifam -> family -> unit                   *)
(** [Description] : Vérifie les autres champs de saisie des formulaires
                    individu et famille.
    [Args] :
      - base : base
      - misc : fonction qui ajoute un misc à la liste des miscs
      - ifam : ifam
      - fam  : family
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                              *)
(* ************************************************************************* *)
let check_other_fields base misc ifam fam = check_sources base misc ifam fam
