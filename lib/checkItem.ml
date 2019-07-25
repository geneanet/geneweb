(* $Id: checkItem.ml,v 1.11 2007-09-05 13:16:45 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

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
let average_marriage_age = 20

let strictly_before d1 d2 =
  match d1, d2 with
  | Dgreg (d1, _), Dgreg (d2, _) ->
    begin
      try Date.compare_dmy ~strict:true d1 d2 < 0
      with Date.Not_comparable -> false
    end
  | _ -> false

let strictly_after d1 d2 =
  match d1, d2 with
  | Dgreg (d1, _), Dgreg (d2, _) ->
    begin
      try Date.compare_dmy ~strict:true d1 d2 > 0
      with Date.Not_comparable -> false
    end
  | _ -> false

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

let compare_event_date_prec d1 d2 =
  match d1.prec, d2.prec with
    Before, _ -> -1
  | _, Before -> 1
  | After, _ -> 1
  | _, After -> -1
  | _ -> 0

(* Compare les dates sans prendre en compte les dates textes. *)
let compare_event_date d1 d2 =
  match d1, d2 with
    Dgreg (dmy1, _), Dgreg (dmy2, _) ->
      begin match Stdlib.compare dmy1.year dmy2.year with
        0 ->
          begin match Stdlib.compare dmy1.month dmy2.month with
            0 ->
              (* Si l'une des deux dates n'est pas complète (mois ou jour *)
              (* égal à zéro), alors on ne distingue pas les deux dates.  *)
              if dmy1.day = 0 || dmy2.day = 0 then 0
              else
                begin match Stdlib.compare dmy1.day dmy2.day with
                  0 -> compare_event_date_prec dmy1 dmy2
                | x -> x
                end
          | x ->
              (* Idem ci-dessus. *)
              if dmy1.month = 0 || dmy2.month = 0 then 0 else x
          end
      | x -> x
      end
  | _ -> 0

let cmp_events (get_name, get_date) e1 e2 =
  match Adef.od_of_cdate (get_date e1), Adef.od_of_cdate (get_date e2) with
    Some d1, Some d2 ->
      (* On utilise compare_event_date parce qu'on ne veut *)
      (* pas prendre en compte les dates textes, on veut   *)
      (* que l'évènement soit plus important pour le tri.  *)
      let comp_date = compare_event_date d1 d2 in
      if comp_date = 0 then compare_event_name (get_name e1) (get_name e2)
      else comp_date
  | _ -> compare_event_name (get_name e1) (get_name e2)

let sort_events get_name get_date events =
  List.stable_sort (fun e1 e2 -> cmp_events (get_name, get_date) e1 e2) events

let merge_events get_name get_date l1 l2 =
  List.merge (fun e1 e2 -> cmp_events (get_name, get_date) e1 e2) l1 l2

let sort_pevents warning p =
  let a =
    sort_events
      (fun evt -> Psort evt.epers_name) (fun evt -> evt.epers_date)
      (get_pevents p)
  in
  let b = get_pevents p in
  if compare b a <> 0 then warning (ChangedOrderOfPersonEvents (p, b, a))

let sort_fevents warning (ifam, fam) =
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
  (* On pourrait faire un calcul sur la descendance ou l'ascendance si  *)
  (* on ne trouve rien ... mais c'est peut être un peu trop gourmand    *)
  (* juste pour un warning ?                                            *)
  let first_found_date =
    match
      Adef.od_of_cdate (get_birth p), Adef.od_of_cdate (get_baptism p)
    with
      Some (Dgreg (d, _)), _ -> Some d
    | _, Some (Dgreg (d, _)) -> Some d
    | _ ->
        let rec loop i =
          if i >= Array.length (get_family p) then None
          else
            let fam = foi base (get_family p).(i) in
            match Adef.od_of_cdate (get_marriage fam) with
              Some (Dgreg (d, _)) ->
                let d = {d with year = d.year - average_marriage_age} in
                Some d
            | _ -> loop (i + 1)
        in
        loop 0
  in
  let is_dead =
    match get_death p with
      Death (_, _) | DeadYoung | DeadDontKnowWhen -> true
    | OfCourseDead -> true
    | _ -> false
  in
  if is_dead then
    match first_found_date, Date.date_of_death (get_death p) with
      Some d1, Some (Dgreg (d2, _)) ->
        let a = Date.time_elapsed d1 d2 in
        if d2.year > lim_date_death then
          (if a.year > max_death_after_lim_date_death then
             warning (DeadOld (p, a)))
        else if a.year > max_death_before_lim_date_death then
          warning (DeadOld (p, a))
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
         let _ =
           fixed := !fixed + changed; not_fixed := !not_fixed + not_changed
         in
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

let check_difference_age_between_cpl base warning ifath imoth =
  let fath = poi base ifath in
  let moth = poi base imoth in
  let find_date p =
    match
      Adef.od_of_cdate (get_birth p), Adef.od_of_cdate (get_baptism p)
    with
      Some (Dgreg (d, _)), _ -> Some d
    | _, Some (Dgreg (d, _)) -> Some d
    | _ -> None
  in
  match find_date fath, find_date moth with
    Some d1, Some d2 ->
      let a = Date.time_elapsed d1 d2 in
      if a.year > max_age_btw_cpl then
        warning (BigAgeBetweenSpouses (fath, moth, a))
  | _ -> ()

let year_of d = d.year

let check_normal_marriage_date_for_someone base warning witn fam ip =
  let p = poi base ip in
  match Adef.od_of_cdate (get_marriage fam) with
    Some (Dgreg (g2, _) as d2) ->
      begin match Adef.od_of_cdate (get_birth p) with
        Some (Dgreg (g1, _) as d1) ->
          if strictly_before d2 d1 then
            if witn then warning (WitnessDateBeforeBirth p)
            else warning (MarriageDateBeforeBirth p)
          else if
            not witn && year_of g2 > lim_date_marriage &&
            year_of (Date.time_elapsed g1 g2) < min_age_marriage
          then
            warning (YoungForMarriage (p, Date.time_elapsed g1 g2))
      | _ -> ()
      end;
      begin match get_death p with
        Death (_, d3) ->
          let d3 = Adef.date_of_cdate d3 in
          if strictly_after d2 d3 then
            if witn then warning (WitnessDateAfterDeath p)
            else warning (MarriageDateAfterDeath p)
      | _ -> ()
      end
  | _ -> ()


(* ************************************************************************* *)
(*  [Fonc] check_normal_marriage_date_for_witness :
      base -> (Def.warning -> unit) ->
        (ifam * family) -> unit                                              *)
(** [Description] : Vérifie les dates des témoins par rapport à la date du
                    mariage.
    [Args] :
      - base    : base
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - family  : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let check_normal_marriage_date_for_witness base warning (ifam, fam) =
  let wl = foi base ifam in
  Array.iter
    (fun ip -> check_normal_marriage_date_for_someone base warning true fam ip)
    (get_witnesses wl)


(* ************************************************************************* *)
(*  [Fonc] check_normal_marriage_date_for_parent :
      base -> (Def.warning -> unit) ->
        (ifam * family) -> unit                                              *)
(** [Description] : Vérifie les dates du conjoint1 et du conjoint2 par
                    rapport à la date du mariage.
    [Args] :
      - base    : base
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - family  : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let check_normal_marriage_date_for_parent base warning (ifam, fam) =
  let cpl = foi base ifam in
  check_normal_marriage_date_for_someone base warning false fam
    (get_father cpl);
  check_normal_marriage_date_for_someone base warning false fam
    (get_mother cpl);
  check_difference_age_between_cpl base warning (get_father cpl)
    (get_mother cpl)


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

let sort_children2 base warning ifam des =
  let b = get_children des in
  match sort_children base b with
    None -> b
  | Some (b, a) -> warning (ChangedOrderOfChildren (ifam, des, b, a)); a


(* ********************************************************************** *)
(*  [Fonc] check_marriages_order :
             base -> (Def.warning -> unit) -> person -> unit              *)
(** [Description] : Trie les famillies en fonction des dates de mariages.
    [Args] :
      - base    : base de donnée
      - warning : fonction qui ajoute un warning à la liste des warnings
      - p       : person
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
let check_marriages_order base warning p =
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
  match np, Adef.od_of_cdate (get_birth x) with
    None, _ -> ()
  | Some (elder, d1), Some d2 ->
      begin match d1, d2 with
        Dgreg (d1, _), Dgreg (d2, _) ->
          let d = Date.time_elapsed d1 d2 in
          (* On vérifie les jumeaux ou naissances proches. *)
          if d.year = 0 && d.month = 0 && d.day < max_days_btw_sibl then ()
          else if d.year = 0 && d.month < max_month_btw_sibl then
            warning (CloseChildren (ifam, des, elder, x))
      | _ -> ()
      end
  | _ -> ()

let born_after_his_elder_sibling warning x np ifam des =
  match np, Adef.od_of_cdate (get_birth x), get_death x with
    None, _, _ -> ()
  | Some (elder, d1), Some d2, _ ->
      if strictly_after d1 d2 then
        warning (ChildrenNotInOrder (ifam, des, elder, x))
  | Some (elder, d1), _, Death (_, d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then
        warning (ChildrenNotInOrder (ifam, des, elder, x))
  | _ -> ()

let child_born_after_his_parent base warning x iparent =
  let parent = poi base iparent in
  match
    Adef.od_of_cdate (get_birth parent), Adef.od_of_cdate (get_birth x),
    Date.date_of_death (get_death x)
  with
    Some (Dgreg (g1, _) as d1), Some (Dgreg (g2, _) as d2), _ ->
      if strictly_after d1 d2 then warning (ParentBornAfterChild (parent, x))
      else
        let a = Date.time_elapsed g1 g2 in
        if year_of a < min_parent_age then
          warning (ParentTooYoung (parent, a))
        else if
          get_sex parent = Female && year_of a > max_mother_age ||
          get_sex parent = Male && year_of a > max_father_age
        then
          warning (ParentTooOld (parent, a))
  | Some (Dgreg (g1, _) as d1), _, Some (Dgreg (g2, _) as d2) ->
      if strictly_after d1 d2 then warning (ParentBornAfterChild (parent, x))
      else
        let a = Date.time_elapsed g1 g2 in
        if year_of a < min_parent_age then
          warning (ParentTooYoung (parent, a))
  | _ -> ()

let child_born_before_mother_death base warning x imoth =
  let mother = poi base imoth in
  match Adef.od_of_cdate (get_birth x), get_death mother with
    Some d1, Death (_, d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then
        warning (MotherDeadAfterChildBirth (mother, x))
  | _ -> ()

let possible_father base warning x ifath =
  let father = poi base ifath in
  match Adef.od_of_cdate (get_birth x), Date.date_of_death (get_death father) with
    Some (Dgreg ({prec = Before}, _)), _ |
    _, Some (Dgreg ({prec = After}, _)) ->
      ()
  | Some (Dgreg (d1, _)), Some (Dgreg (d2, _)) ->
      let a2 =
        match d2 with
          {prec = YearInt dmy2} -> dmy2.year2
        | {prec = OrYear dmy2} -> dmy2.year2
        | {year = a} -> a
      in
      if year_of d1 > a2 + 1 then warning (DeadTooEarlyToBeFather (father, x))
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
  List.iter
    (fun evt ->
       match Adef.od_of_cdate evt.epers_date with
         Some (Dgreg (_, _) as d2) ->
           Array.iter
             (fun (iw, _) ->
                let p = poi base iw in
                begin match Adef.od_of_cdate (get_birth p) with
                  Some (Dgreg (_, _) as d1) ->
                    if strictly_before d2 d1 then
                      warning (PWitnessEventBeforeBirth (p, evt))
                | _ -> ()
                end;
                match get_death p with
                  Death (_, d3) ->
                    let d3 = Adef.date_of_cdate d3 in
                    if strictly_after d2 d3 then
                      warning (PWitnessEventAfterDeath (p, evt))
                | _ -> ())
             evt.epers_witnesses
       | _ -> ())
    (get_pevents p)

let check_pevents base warning p =
  check_order_pevents warning p ;
  check_witness_pevents base warning p

(* ************************************************************************* *)
(*  [Fonc] check_children :
      base -> (Def.warning -> unit) ->
        (ifam * family) -> unit                                              *)
(** [Description] : Vérifie toutes les informations des enfants d'une famille.
    [Args] :
      - base    : base
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - family  : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let check_children base warning (ifam, fam) =
  let cpl = fam in
  let des = fam in
  let after = sort_children2 base warning ifam des in
  ignore @@
  Array.fold_left
    (fun np child ->
       let child = poi base child in
       check_pevents base warning child;
       born_after_his_elder_sibling warning child np ifam des;
       close_siblings warning child np ifam des;
       child_born_after_his_parent base warning child
         (get_father cpl);
       child_born_after_his_parent base warning child
         (get_mother cpl);
       child_born_before_mother_death base warning child (get_mother cpl);
       possible_father base warning child (get_father cpl);
       child_has_sex warning child;
       match Adef.od_of_cdate (get_birth child) with
         Some d -> Some (child, d)
       | _ -> np)
    None after

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

let check_order_fevents base warning (_ifam, fam) =
  let p = poi base (get_father fam) in
  check_order_pfevents
    (fun evt -> Fsort evt.efam_name)
    (fun evt -> evt.efam_date)
    (fun p e1 e2 -> warning (FEventOrder (p, e1, e2)))
    (p)
    (get_fevents fam)

let check_witness_fevents base warning (_ifam, fam) =
  List.iter
    (fun evt ->
       match Adef.od_of_cdate evt.efam_date with
         Some (Dgreg (_, _) as d2) ->
           Array.iter
             (fun (iw, _) ->
                let p = poi base iw in
                begin match Adef.od_of_cdate (get_birth p) with
                  Some (Dgreg (_, _) as d1) ->
                    if strictly_before d2 d1 then
                      warning (FWitnessEventBeforeBirth (p, evt))
                | _ -> ()
                end;
                match get_death p with
                  Death (_, d3) ->
                    let d3 = Adef.date_of_cdate d3 in
                    if strictly_after d2 d3 then
                      warning (FWitnessEventAfterDeath (p, evt))
                | _ -> ())
             evt.efam_witnesses
       | _ -> ())
    (get_fevents fam)

let check_marriage_age base warning (_ifam, fam) ip =
  let p = poi base ip in
  let rec loop fevents =
    match fevents with
      [] -> ()
    | e :: l ->
        match e.efam_name with
          Efam_Marriage ->
            begin match Adef.od_of_cdate e.efam_date with
              Some (Dgreg (g2, _)) ->
                begin match Adef.od_of_cdate (get_birth p) with
                  Some (Dgreg (g1, _)) ->
                    if year_of g2 > lim_date_marriage &&
                       year_of (Date.time_elapsed g1 g2) < min_age_marriage
                    then
                      warning (YoungForMarriage (p, Date.time_elapsed g1 g2))
                    else loop l
                | _ -> loop l
                end
            | _ -> loop l
            end
        | _ -> loop l
  in
  loop (get_fevents fam)


let check_reduce_fevents base warning (ifam, fam) =
  let cpl = foi base ifam in
  check_order_fevents base warning (ifam, fam);
  check_marriage_age base warning (ifam, fam) (get_father cpl);
  check_marriage_age base warning (ifam, fam) (get_mother cpl);
  check_difference_age_between_cpl base warning (get_father cpl)
    (get_mother cpl)

let check_fevents base warning (ifam, fam) =
  let cpl = foi base ifam in
  check_order_fevents base warning (ifam, fam);
  check_witness_fevents base warning (ifam, fam);
  check_marriage_age base warning (ifam, fam) (get_father cpl);
  check_marriage_age base warning (ifam, fam) (get_mother cpl);
  check_difference_age_between_cpl base warning (get_father cpl)
    (get_mother cpl)


(* main *)


(* ************************************************************************* *)
(*  [Fonc] person : base -> (Def.warning -> unit) -> person -> unit          *)
(** [Description] : Vérifie les warnings d'une personne à la validation du
                    formulaire individu.
    [Args] :
      - base    : base
      - warning : fonction qui ajoute un warning à la liste des warnings
      - p       : person
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let person base warning p =
  check_pevents base warning p;
  check_person_age base warning p;
  List.iter (titles_after_birth warning p) (get_titles p);
  sort_pevents warning p;
  related_sex_is_coherent base warning p


(* ************************************************************************* *)
(*  [Fonc] family :
      base -> (Def.warning -> unit) -> ifam -> family -> unit                *)
(** [Description] : En cas de modification d'une famille, on vérifie toutes
                    les personnes accessibles après la validation du
                    formulaire (famille).
                    Vérifie s'il y a des erreurs ou des warnings pour le
                    couple, les parents du couple, les témoins et les enfants
                    du couple.
    [Args] :
      - base    : base
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - fam     : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let family base warning ifam fam =
  check_normal_marriage_date_for_parent base warning (ifam, fam);
  check_normal_marriage_date_for_witness base warning (ifam, fam);
  check_fevents base warning (ifam, fam);
  check_children base warning (ifam, fam);
  sort_fevents warning (ifam, fam);
  let father = poi base (get_father fam) in
  let mother = poi base (get_mother fam) in
  check_marriages_order base warning father;
  check_marriages_order base warning mother


(* ************************************************************************* *)
(*  [Fonc] reduce_family :
      base -> (Def.warning -> unit) -> ifam -> family -> unit                *)
(** [Description] : En cas de modification d'une personne, on ne vérifie que
                    les personnes accessibles après la validation du
                    formulaire (individu).
                    Vérifie s'il y a des erreurs ou des warnings pour le
                    couple, les parents du couple et les enfants du couple.
    [Args] :
      - base    : base
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - fam     : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let reduce_family base warning ifam fam =
  check_normal_marriage_date_for_parent base warning (ifam, fam);
  check_reduce_fevents base warning (ifam, fam);
  check_children base warning (ifam, fam)


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
