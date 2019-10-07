(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

let compatible_cdates cd1 cd2 =
  cd1 = cd2
  || cd2 = Adef.cdate_None
  || cd1 = Adef.cdate_None

let compatible_death_reasons dr1 dr2 = dr1 = dr2 || dr2 = Unspecified

let compatible_deaths d1 d2 =
  if d1 = d2 then true
  else
    match d1, d2 with
      Death (dr1, cd1), Death (dr2, cd2) ->
        compatible_death_reasons dr1 dr2 && compatible_cdates cd1 cd2
    | Death (_, _), NotDead -> false
    | Death (_, _), _ -> true
    | _, DontKnowIfDead -> true
    | DontKnowIfDead, _ -> true
    | _ -> false

let compatible_burials b1 b2 =
  if b1 = b2 then true
  else
    match b1, b2 with
      _, UnknownBurial -> true
    | UnknownBurial, _ -> true
    | Buried cd1, Buried cd2 -> compatible_cdates cd1 cd2
    | Cremated cd1, Cremated cd2 -> compatible_cdates cd1 cd2
    | _ -> false

let compatible_strings s1 s2 =
  eq_istr s1 s2 || is_empty_string s2 || is_empty_string s1

let compatible_divorces d1 d2 = d1 = d2

let compatible_relation_kinds rk1 rk2 = rk1 = rk2

let compatible_titles t1 t2 =
  Futil.eq_lists (Futil.eq_titles eq_istr) t1 t2 || t2 = []

let compatible_pevents pevt1 pevt2 = pevt1 = [] && pevt2 = []

let compatible_fevents fevt1 fevt2 = fevt1 = [] && fevt2 = []

let compatible_strings_lists sl1 sl2 =
  sl2 = [] || Futil.eq_lists eq_istr sl1 sl2

let compatible_notes base s1 s2 =
  compatible_strings s1 s2 || sou base s1 = sou base s2

let compatible_ind base p1 p2 =
  eq_istr (get_first_name p1) (get_first_name p2) &&
  eq_istr (get_surname p1) (get_surname p2) &&
  compatible_strings (get_image p1) (get_image p2) &&
  compatible_strings (get_public_name p1) (get_public_name p2) &&
  compatible_strings_lists (get_qualifiers p1) (get_qualifiers p2) &&
  compatible_strings_lists (get_aliases p1) (get_aliases p2) &&
  compatible_strings_lists (get_first_names_aliases p1)
    (get_first_names_aliases p2) &&
  compatible_strings_lists (get_surnames_aliases p1)
    (get_surnames_aliases p2) &&
  compatible_titles (get_titles p1) (get_titles p2) &&
  compatible_pevents (get_pevents p1) (get_pevents p2) &&
  get_rparents p2 = [] && get_related p2 = [] &&
  compatible_strings (get_occupation p1) (get_occupation p2) &&
  compatible_cdates (get_birth p1) (get_birth p2) &&
  compatible_strings (get_birth_place p1) (get_birth_place p2) &&
  compatible_cdates (get_baptism p1) (get_baptism p2) &&
  compatible_strings (get_baptism_place p1) (get_baptism_place p2) &&
  compatible_deaths (get_death p1) (get_death p2) &&
  compatible_strings (get_death_place p1) (get_death_place p2) &&
  compatible_burials (get_burial p1) (get_burial p2) &&
  compatible_strings (get_burial_place p1) (get_burial_place p2) &&
  compatible_notes base (get_notes p1) (get_notes p2)

let compatible_fam fam1 fam2 =
  compatible_cdates (get_marriage fam1) (get_marriage fam2) &&
  compatible_strings (get_marriage_place fam1) (get_marriage_place fam2) &&
  Array.length (get_witnesses fam2) = 0 &&
  compatible_fevents (get_fevents fam1) (get_fevents fam2) &&
  compatible_relation_kinds (get_relation fam1) (get_relation fam2) &&
  compatible_divorces (get_divorce fam1) (get_divorce fam2) &&
  compatible_strings (get_fsources fam1) (get_fsources fam2)

let reparent_ind base (warning : CheckItem.base_warning -> unit) ip1 ip2 =
  let a1 = poi base ip1 in
  let a2 = poi base ip2 in
  match get_parents a1, get_parents a2 with
  | None, Some ifam ->
      let des = gen_descend_of_family (foi base ifam) in
      let rec replace i =
        if des.children.(i) = ip2 then des.children.(i) <- ip1
        else replace (i + 1)
      in
      replace 0;
      let a1 = {parents = Some ifam; consang = Adef.fix (-1)} in
      patch_ascend base ip1 a1; patch_descend base ifam des
  | Some ifam, None ->
      let fam = foi base ifam in
      let children = get_children fam in
      begin match CheckItem.sort_children base children with
        Some (b, a) ->
          let des = gen_descend_of_family fam in
          patch_descend base ifam des;
          warning (ChangedOrderOfChildren (ifam, fam, b, a))
      | None -> ()
      end
  | _ -> ()

let effective_merge_ind conf base (warning : CheckItem.base_warning -> unit) p1 p2 =
  let u2 = poi base (get_iper p2) in
  if Array.length (get_family u2) <> 0 then
    begin
      for i = 0 to Array.length (get_family u2) - 1 do
        let ifam = (get_family u2).(i) in
        let cpl = foi base ifam in
        let cpl =
          if get_iper p2 = get_father cpl then
            Gutil.couple false (get_iper p1) (get_mother cpl)
          else if get_iper p2 = get_mother cpl then
            Gutil.couple false (get_father cpl) (get_iper p1)
          else assert false
        in
        patch_couple base ifam cpl
      done;
      let u1 = {family = Array.append (get_family p1) (get_family u2)} in
      patch_union base (get_iper p1) u1;
      let u2 = {family = [| |]} in patch_union base (get_iper p2) u2
    end;
  let p1 =
    let get_string fn = if is_empty_string (fn p1) then fn p2 else fn p1 in
    {(gen_person_of_person p1) with
     sex = if get_sex p2 <> Neuter then get_sex p2 else get_sex p1
     ; birth = if get_birth p1 = Adef.cdate_None then get_birth p2 else get_birth p1
     ; birth_place = get_string get_birth_place
     ; birth_src = get_string get_birth_src
     ; baptism =
         if get_baptism p1 = Adef.cdate_None then get_baptism p2
         else get_baptism p1
     ; baptism_place = get_string get_baptism_place
     ; baptism_src = get_string get_baptism_src
     ; death =
         if get_death p1 = DontKnowIfDead then get_death p2 else get_death p1
     ; death_place = get_string get_death_place
     ; death_src = get_string get_death_src
     ; burial =
         if get_burial p1 = UnknownBurial then get_burial p2 else get_burial p1
     ; burial_place = get_string get_burial_place
     ; burial_src = get_string get_burial_src
     ; occupation = get_string get_occupation
     ; notes = get_string get_notes
    }
  in
  patch_person base p1.key_index p1;
  reparent_ind base warning p1.key_index (get_iper p2);
  UpdateIndOk.effective_del base p2 ;
  let s =
    let sl =
      [p1.notes; p1.occupation; p1.birth_note; p1.birth_src; p1.baptism_note;
       p1.baptism_src; p1.death_note; p1.death_src; p1.burial_note;
       p1.burial_src; p1.psources]
    in
    let sl =
      let rec loop l accu =
        match l with
          [] -> accu
        | evt :: l -> loop l (evt.epers_note :: evt.epers_src :: accu)
      in
      loop p1.pevents sl
    in
    String.concat " " (List.map (sou base) sl)
  in
  Notes.update_notes_links_db conf (NotesLinks.PgInd p1.key_index) s

let is_ancestor base p1 p2 =
  let ip1 = get_iper p1 in
  let ip2 = get_iper p2 in
  let visited = Marker.make (nb_of_persons base) false in
  let rec loop ip =
    if Gwdb.Marker.get visited ip then false
    else if ip = ip1 then true
    else
      begin
        Gwdb.Marker.set visited ip true ;
        match get_parents (poi base ip) with
        | Some ifam ->
            let cpl = foi base ifam in
            loop (get_father cpl) || loop (get_mother cpl)
        | None -> false
      end
  in
  loop ip2

exception Error_loop of person
exception Same_person
exception Different_sexes

let check_ind base p1 p2 =
  if get_iper p1 = get_iper p2 then raise Same_person
  else if get_sex p1 <> get_sex p2 && get_sex p1 <> Neuter
          && get_sex p2 <> Neuter
  then raise Different_sexes
  else if is_ancestor base p1 p2 then raise (Error_loop p2)
  (* begin error_loop conf base p2; false, false end *)
  else if is_ancestor base p2 p1 then raise (Error_loop p1)
  (* begin error_loop conf base p1; false, false end *)
  else compatible_ind base p1 p2

let merge_ind conf base warning branches p1 p2 changes_done propose_merge_ind =
  if check_ind base p1 p2 then
    begin effective_merge_ind conf base warning p1 p2; true, true end
  else
    begin propose_merge_ind conf base branches p1 p2; false, changes_done end

let effective_merge_fam base ifam1 fam1 ifam2 fam2 =
  let des1 = fam1 in
  let des2 = fam2 in
  let fam1 =
    {(gen_family_of_family fam1) with marriage =
      if get_marriage fam1 = Adef.cdate_None then get_marriage fam2
      else get_marriage fam1;
     marriage_place =
       if is_empty_string (get_marriage_place fam1) then
         get_marriage_place fam2
       else get_marriage_place fam1;
     marriage_src =
       if is_empty_string (get_marriage_src fam1) then get_marriage_src fam2
       else get_marriage_src fam1;
     fsources =
       if is_empty_string (get_fsources fam1) then get_fsources fam2
       else get_fsources fam1}
  in
  let des1 =
    let children = Array.append (get_children des1) (get_children des2) in
    let _ = (CheckItem.sort_children base children : _ option) in
    {children = children}
  in
  UpdateFamOk.effective_del base ifam2;
  for i = 0 to Array.length (get_children des2) - 1 do
    let ip = (get_children des2).(i) in
    let a = {parents = Some ifam1; consang = Adef.fix (-1)} in
    patch_ascend base ip a
  done;
  patch_family base ifam1 fam1;
  patch_descend base ifam1 des1

let merge_fam conf base branches ifam1 ifam2 fam1 fam2 ip1 ip2 changes_done propose_merge_fam =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  if compatible_fam fam1 fam2 then
    begin
      effective_merge_fam base ifam1 fam1 ifam2 fam2 ;
      true, true
    end
  else
    begin
      propose_merge_fam conf base branches (ifam1, fam1) (ifam2, fam2) p1 p2;
      false, changes_done
    end

let rec try_merge conf base warning branches ip1 ip2 changes_done propose_merge_ind propose_merge_fam =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  let ok_so_far = true in
  let (ok_so_far, changes_done) =
    match get_parents p1, get_parents p2 with
      Some ifam1, Some ifam2 when ifam1 <> ifam2 ->
        let branches = (ip1, ip2) :: branches in
        let fam1 = foi base ifam1 in
        let fam2 = foi base ifam2 in
        let f1 = get_father fam1 in
        let f2 = get_father fam2 in
        let m1 = get_mother fam1 in
        let m2 = get_mother fam2 in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if f1 = f2 then true, changes_done
            else try_merge conf base ignore branches f1 f2 changes_done propose_merge_ind propose_merge_fam
          else false, changes_done
        in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if m1 = m2 then true, changes_done
            else try_merge conf base ignore branches m1 m2 changes_done propose_merge_ind propose_merge_fam
          else false, changes_done
        in
        let (ok_so_far, changes_done) =
          if ok_so_far
          then merge_fam conf base branches ifam1 ifam2 fam1 fam2 f1 m1 changes_done propose_merge_fam
          else false, changes_done
        in
        ok_so_far, changes_done
    | _ -> ok_so_far, changes_done
  in
  if ok_so_far
  then merge_ind conf base warning branches p1 p2 changes_done propose_merge_ind
  else false, changes_done

let merge conf base p1 p2 propose_merge_ind propose_merge_fam =
  let rev_wl = ref [] in
  let warning w = rev_wl := w :: !rev_wl in
  let (ok, changes_done) =
    try_merge conf base warning [] (get_iper p1) (get_iper p2)
      false propose_merge_ind propose_merge_fam
  in
  if changes_done then Util.commit_patches conf base;
  if ok then begin
    let changed =
      let p1 = Util.string_gen_person base (gen_person_of_person p1) in
      let p2 = Util.string_gen_person base (gen_person_of_person p2) in
      U_Merge_person (p2, p1, p1)
    in
    History.record conf base changed "fp" end ;
  Update.delete_topological_sort conf base ;
  (ok, List.rev !rev_wl)

(* Undocumented feature... Kill someone's ancestors *)

let rec kill_ancestors conf base included_self p nb_ind nb_fam =
  begin match get_parents p with
    Some ifam ->
      let cpl = foi base ifam in
      kill_ancestors conf base true (poi base (get_father cpl)) nb_ind nb_fam;
      kill_ancestors conf base true (poi base (get_mother cpl)) nb_ind nb_fam;
      UpdateFamOk.effective_del base ifam ;
      incr nb_fam
  | None -> ()
  end;
  if included_self then begin
    UpdateIndOk.effective_del base p ;
    incr nb_ind
  end
