(* $Id: select.ml,v 5.16 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

let is_censored_person threshold p =
  match Adef.od_of_cdate (get_birth p) with
    Some date ->
      begin match date with
        Dgreg (dmy, _) -> dmy.year >= threshold && get_access p != Public
      | _ -> false
      end
  | None -> false

let is_censored_couple base threshold cpl =
  let fath = poi base (get_father cpl) in
  let moth = poi base (get_mother cpl) in
  is_censored_person threshold fath || is_censored_person threshold moth

let censor_person base per_tab flag threshold p no_check =
  let ps = poi base p in
  if no_check || is_censored_person threshold ps then
    Gwdb.Marker.set per_tab p (Gwdb.Marker.get per_tab p lor flag)

let rec censor_family base per_tab fam_tab flag threshold i no_check =
  let censor_unions p =
    let uni = poi base p in
    Array.iter
      (fun ifam ->
         censor_family base per_tab fam_tab flag threshold ifam true;
         censor_person base per_tab flag threshold p true)
      (get_family uni)
  in
  let censor_descendants f =
    let des = foi base f in
    Array.iter
      (fun iper ->
         if Gwdb.Marker.get per_tab iper <> 0 then () else censor_unions iper)
      (get_children des)
  in
  let all_families_censored p =
    let uni = poi base p in
    Array.fold_left
      (fun check ifam -> Gwdb.Marker.get fam_tab ifam = 0 && check) true
      (get_family uni)
  in
  let censor_spouse iper =
    if all_families_censored iper
    then Gwdb.Marker.set per_tab iper (Gwdb.Marker.get per_tab iper lor flag)
  in
  if Gwdb.Marker.get fam_tab i <> 0 then ()
  else
    let fam = foi base i in
    if no_check || is_censored_couple base threshold fam then
      begin
        Gwdb.Marker.set fam_tab i (Gwdb.Marker.get fam_tab i lor flag);
        censor_spouse (get_father fam);
        censor_spouse (get_mother fam);
        censor_descendants i
      end

let censor_base base per_tab fam_tab flag threshold =
  Gwdb.Collection.iter (fun i ->
      censor_family base per_tab fam_tab flag threshold i false
    ) (Gwdb.ifams base) ;
  Gwdb.Collection.iter (fun i ->
    censor_person base per_tab flag threshold i false
    ) (Gwdb.ipers base)

let restrict_base base per_tab fam_tab flag =
  Gwdb.Collection.iter (fun i ->
      if base_visible_get base (fun _ -> false) i
      then Gwdb.Marker.set per_tab i (Gwdb.Marker.get per_tab i lor flag)
    ) (Gwdb.ipers base) ;
  Gwdb.Collection.iter (fun i ->
      let fam = foi base i in
      let des_visible =
        Array.fold_left
          (fun check iper -> check || Gwdb.Marker.get per_tab iper = 0) false
          (get_children fam)
      in
      let cpl_not_visible =
        Gwdb.Marker.get per_tab (get_father fam) <> 0
        || Gwdb.Marker.get per_tab (get_mother fam) <> 0
      in
      if not des_visible && cpl_not_visible
      then Gwdb.Marker.set fam_tab i (Gwdb.Marker.get fam_tab i lor flag)
    ) (Gwdb.ifams base)

let flag_family base per_tab fam_tab flag ifam =
  let cpl = foi base ifam in
  Gwdb.Marker.set fam_tab ifam (Gwdb.Marker.get fam_tab ifam lor flag);
  let i = (get_father cpl) in
  Gwdb.Marker.set per_tab i (Gwdb.Marker.get per_tab i lor flag) ;
  let i = (get_mother cpl) in
  Gwdb.Marker.set per_tab i (Gwdb.Marker.get per_tab i lor flag)

let select_ancestors base per_tab fam_tab with_siblings flag iper =
  let rec add_ancestors iper =
    if Gwdb.Marker.get per_tab iper land flag <> 0 then ()
    else
      begin
        Gwdb.Marker.set per_tab iper (Gwdb.Marker.get per_tab iper lor flag);
        match get_parents (poi base iper) with
          Some ifam ->
            if Gwdb.Marker.get fam_tab ifam land flag <> 0 then ()
            else
              begin
                Gwdb.Marker.set fam_tab ifam (Gwdb.Marker.get fam_tab ifam lor flag);
                let cpl = foi base ifam in
                add_ancestors (get_father cpl); add_ancestors (get_mother cpl)
              end
        | None -> ()
      end
  in
  add_ancestors iper;
  if with_siblings then
    let add_sibling_spouse_parents ip =
      match get_parents (poi base ip) with
        Some ifam -> flag_family base per_tab fam_tab flag ifam
      | None -> ()
    in
    let add_siblings_marriages ifam =
      let des = foi base ifam in
      Array.iter
        (fun ip ->
           Gwdb.Marker.set per_tab ip (Gwdb.Marker.get per_tab ip lor flag);
           Array.iter
             (fun ifam ->
                let cpl = foi base ifam in
                Gwdb.Marker.set fam_tab ifam
                  (Gwdb.Marker.get fam_tab ifam lor flag );
                List.iter
                  (fun ip ->
                     Gwdb.Marker.set per_tab ip (Gwdb.Marker.get per_tab ip lor flag);
                     add_sibling_spouse_parents ip)
                  [get_father cpl; get_mother cpl])
             (get_family (poi base ip)))
        (get_children des)
    in
    let add_siblings iparent =
      Array.iter
        (fun ifam ->
           flag_family base per_tab fam_tab flag ifam;
           add_siblings_marriages ifam)
        (get_family (poi base iparent))
    in
    let anc_flag = 4 in
    let rec ancestors_loop iper =
      if Gwdb.Marker.get per_tab iper land anc_flag <> 0 then ()
      else
        begin
          Gwdb.Marker.set per_tab iper (Gwdb.Marker.get per_tab iper lor anc_flag);
          match get_parents (poi base iper) with
            Some ifam ->
              if Gwdb.Marker.get fam_tab ifam land anc_flag <> 0 then ()
              else
                begin
                  Gwdb.Marker.set fam_tab ifam
                    (Gwdb.Marker.get fam_tab ifam lor anc_flag);
                  let cpl = foi base ifam in
                  add_siblings (get_father cpl);
                  add_siblings (get_mother cpl);
                  ancestors_loop (get_father cpl);
                  ancestors_loop (get_mother cpl)
                end
          | None -> ()
        end
    in
    let rec remove_anc_flag iper =
      if Gwdb.Marker.get per_tab iper land anc_flag <> 0 then
        begin
          Gwdb.Marker.set per_tab iper
            (Gwdb.Marker.get per_tab iper land lnot anc_flag) ;
          match get_parents (poi base iper) with
          | Some ifam ->
              if Gwdb.Marker.get fam_tab ifam land anc_flag <> 0 then
                begin
                  Gwdb.Marker.set fam_tab ifam (Gwdb.Marker.get fam_tab ifam land lnot anc_flag) ;
                  let cpl = foi base ifam in
                  remove_anc_flag (get_father cpl);
                  remove_anc_flag (get_mother cpl)
                end
          | None -> ()
        end
    in
    ancestors_loop iper; remove_anc_flag iper

let select_descendants base per_tab fam_tab no_spouses_parents flag iper
    maxlev =
  let mark = Marker.make (nb_of_families base) false in
  let select_family ifam cpl =
    Gwdb.Marker.set fam_tab ifam (Gwdb.Marker.get fam_tab ifam lor flag) ;
    let i = get_father cpl in
    Gwdb.Marker.set per_tab i (Gwdb.Marker.get per_tab i lor flag) ;
    let i = get_mother cpl in
    Gwdb.Marker.set per_tab i (Gwdb.Marker.get per_tab i lor flag) ;
  in
  let rec loop lev iper =
    if maxlev >= 0 && lev > maxlev then ()
    else
      Gwdb.Marker.set per_tab iper (Gwdb.Marker.get per_tab iper lor flag) ;
      Array.iter
        (fun ifam ->
           if Gwdb.Marker.get mark ifam then ()
           else
             let fam = foi base ifam in
             Gwdb.Marker.set mark ifam true;
             select_family ifam fam;
             if not no_spouses_parents then
               begin let sp = Gutil.spouse iper fam in
                 match get_parents (poi base sp) with
                   Some ifam -> select_family ifam (foi base ifam)
                 | None -> ()
               end;
             Array.iter (loop (succ lev)) (get_children fam))
        (get_family (poi base iper))
  in
  loop 0 iper

let select_descendants_ancestors base per_tab fam_tab ip =
  let new_mark = let r = ref 0 in fun () -> incr r; !r in
  let tab = Marker.make (nb_of_persons base) (new_mark ()) in
  let anc_mark = new_mark () in
  let anclist =
    let rec loop list ip =
      if Gwdb.Marker.get tab ip = anc_mark then list
      else
        begin
          Gwdb.Marker.set tab ip anc_mark;
          match get_parents (poi base ip) with
            Some ifam ->
              let cpl = foi base ifam in
              let list = loop list (get_father cpl) in
              loop list (get_mother cpl)
          | None -> ip :: list
        end
    in
    loop [] ip
  in
  let des_mark = new_mark () in
  List.iter
    (fun ip ->
       let rec loop ip =
         if Gwdb.Marker.get tab ip = des_mark then ()
         else
           let u = poi base ip in
           Array.iter
             (fun ifam ->
                Gwdb.Marker.set fam_tab ifam
                  (Gwdb.Marker.get fam_tab ifam lor 1) ;
                Array.iter loop (get_children @@ foi base ifam) ;
             ) (get_family u) ;
           Gwdb.Marker.set tab ip des_mark ;
           Gwdb.Marker.set per_tab ip (Gwdb.Marker.get per_tab ip lor 1)
       in
       loop ip)
    anclist

let select_surname base per_tab fam_tab no_spouses_parents surname =
  let surname = Name.strip_lower surname in
  Gwdb.Collection.iter (fun i ->
    let fam = foi base i in
      let fath = poi base (get_father fam) in
      let moth = poi base (get_mother fam) in
      if Name.strip_lower (sou base (get_surname fath)) = surname ||
         Name.strip_lower (sou base (get_surname moth)) = surname
      then
        begin
          Gwdb.Marker.set fam_tab i true ;
          Gwdb.Marker.set per_tab (get_father fam) true ;
          Gwdb.Marker.set per_tab (get_mother fam) true ;
          Array.iter
            (fun ic ->
               let p = poi base ic in
               if not (Gwdb.Marker.get per_tab ic)
               && Name.strip_lower (sou base (get_surname p)) = surname
               then Gwdb.Marker.set per_tab ic true)
            (get_children fam);
          if no_spouses_parents then ()
          else
            List.iter
              (fun x ->
                 match get_parents (poi base x) with
                 | Some ifam when not (Gwdb.Marker.get fam_tab ifam) ->
                     let cpl = foi base ifam in
                     Gwdb.Marker.set fam_tab ifam true ;
                     Gwdb.Marker.set per_tab (get_father cpl) true ;
                     Gwdb.Marker.set per_tab (get_mother cpl) true
                 | _ -> ())
              [get_father fam; get_mother fam]
        end
    ) (Gwdb.ifams base)

let select_ancestors_descendants base anc desc ancdesc no_spouses_parents
    censor with_siblings maxlev
  : (iper -> bool) * (ifam -> bool)
=
  let tm = Unix.localtime (Unix.time ()) in
  let threshold = 1900 + tm.Unix.tm_year - censor in
  match anc, desc, ancdesc with
    None, None, None ->
      if censor <> 0 || censor = -1 then
        let per_tab = Marker.make (nb_of_persons base) 0 in
        let fam_tab = Marker.make (nb_of_families base) 0 in
        let _ =
          if censor = -1 then restrict_base base per_tab fam_tab 1
          else censor_base base per_tab fam_tab 1 threshold
        in
        (fun i -> Gwdb.Marker.get per_tab i = 0),
        (fun i -> Gwdb.Marker.get fam_tab i = 0)
      else (fun _ -> true), (fun _ -> true)
  | None, None, Some iadper ->
      let per_tab = Marker.make (nb_of_persons base) 0 in
      let fam_tab = Marker.make (nb_of_families base) 0 in
      let _ =
        if censor = -1 then restrict_base base per_tab fam_tab 4
        else if censor <> 0 then censor_base base per_tab fam_tab 4 threshold
      in
      select_descendants_ancestors base per_tab fam_tab iadper;
      (fun i -> let fl = Gwdb.Marker.get per_tab i in fl < 4 && fl > 0),
      (fun i -> let fl = Gwdb.Marker.get fam_tab i in fl < 4 && fl > 0)
  | _ ->
      let per_tab = Marker.make (nb_of_persons base) 0 in
      let fam_tab = Marker.make (nb_of_families base) 0 in
      let _ =
        if censor = -1 then restrict_base base per_tab fam_tab 4
        else if censor <> 0 then censor_base base per_tab fam_tab 4 threshold
      in
      match anc, desc with
        Some iaper, None ->
          select_ancestors base per_tab fam_tab with_siblings 1 iaper;
          (fun i -> Gwdb.Marker.get per_tab i = 1),
          (fun i -> Gwdb.Marker.get fam_tab i = 1)
      | None, Some idper ->
          select_descendants base per_tab fam_tab no_spouses_parents 1 idper
            maxlev;
          (fun i -> Gwdb.Marker.get per_tab i = 1),
          (fun i -> Gwdb.Marker.get fam_tab i = 1)
      | Some iaper, Some idper ->
          select_ancestors base per_tab fam_tab false 1 iaper;
          select_descendants base per_tab fam_tab no_spouses_parents 2 idper
            maxlev;
          (fun i -> Gwdb.Marker.get per_tab i = 3),
          (fun i -> Gwdb.Marker.get fam_tab i = 3)
      | _ -> assert false

let select_surnames base surnames no_spouses_parents
  : (iper -> bool) * (ifam -> bool)
 =
  let per_tab = Marker.make (nb_of_persons base) false in
  let fam_tab = Marker.make (nb_of_families base) false in
  List.iter (select_surname base per_tab fam_tab no_spouses_parents) surnames;
  (fun i -> Gwdb.Marker.get per_tab i),
  (fun i -> Gwdb.Marker.get fam_tab i)

let functions base anc desc surnames ancdesc no_spouses_parents censor
    with_siblings maxlev =
  let (per_sel1, fam_sel1) =
    select_ancestors_descendants base anc desc ancdesc no_spouses_parents
      censor with_siblings maxlev
  in
  let (per_sel2, fam_sel2) =
    select_surnames base surnames no_spouses_parents
  in
  match censor, anc, desc, ancdesc, surnames with
    0, None, None, None, _ :: _ -> per_sel2, fam_sel2
  | _, _, _, _, [] -> per_sel1, fam_sel1
  | 0, _, _, _, _ ->
      (fun i -> per_sel1 i || per_sel2 i), (fun i -> fam_sel1 i || fam_sel2 i)
  | _ ->
      (fun i -> per_sel1 i && per_sel2 i), (fun i -> fam_sel1 i && fam_sel2 i)
