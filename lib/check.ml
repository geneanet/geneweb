(* $Id: check.ml,v 5.28 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

(* Printing check errors *)

let designation base p =
  let s = Gutil.designation base p in
  if String.get s 0 = '?' || String.get s (String.length s - 1) = '?'
  then s ^ " (i=" ^ string_of_iper (get_iper p) ^ ")"
  else s

let string_of_epers_name base epers_name =
  match epers_name with
    Epers_Birth -> "birth"
  | Epers_Baptism -> "baptism"
  | Epers_Death -> "death"
  | Epers_Burial -> "burial"
  | Epers_Cremation -> "cremation"
  | Epers_Accomplishment -> "accomplishment"
  | Epers_Acquisition -> "acquisition"
  | Epers_Adhesion -> "adhesion"
  | Epers_BaptismLDS -> "baptism (LDS)"
  | Epers_BarMitzvah -> "bar mitzvah"
  | Epers_BatMitzvah -> "bat mitzvah"
  | Epers_Benediction -> "benediction"
  | Epers_ChangeName -> "change name"
  | Epers_Circumcision -> "circumcision"
  | Epers_Confirmation -> "confirmation"
  | Epers_ConfirmationLDS -> "confirmation (LDS)"
  | Epers_Decoration -> "decoration"
  | Epers_DemobilisationMilitaire -> "military demobilisation"
  | Epers_Diploma -> "diploma"
  | Epers_Distinction -> "distinction"
  | Epers_Dotation -> "dotation"
  | Epers_DotationLDS -> "dotation (LDS)"
  | Epers_Education -> "education"
  | Epers_Election -> "election"
  | Epers_Emigration -> "emigration"
  | Epers_Excommunication -> "excommunication"
  | Epers_FamilyLinkLDS -> "family link (LDS)"
  | Epers_FirstCommunion -> "first communion"
  | Epers_Funeral -> "funeral"
  | Epers_Graduate -> "graduation"
  | Epers_Hospitalisation -> "hospitalisation"
  | Epers_Illness -> "illness"
  | Epers_Immigration -> "immigration"
  | Epers_ListePassenger -> "passenger liste"
  | Epers_MilitaryDistinction -> "military distinction"
  | Epers_MilitaryPromotion -> "military promotion"
  | Epers_MilitaryService -> "military service"
  | Epers_MobilisationMilitaire -> "military mobilisation"
  | Epers_Naturalisation -> "naturalisation"
  | Epers_Occupation -> "occupation"
  | Epers_Ordination -> "ordination"
  | Epers_Property -> "property"
  | Epers_Recensement -> "recensement"
  | Epers_Residence -> "residence"
  | Epers_Retired -> "retirement"
  | Epers_ScellentChildLDS -> "scellent child (LDS)"
  | Epers_ScellentParentLDS -> "scellent parent (LDS)"
  | Epers_ScellentSpouseLDS -> "scellent spouse (LDS)"
  | Epers_VenteBien -> "sell"
  | Epers_Will -> "will"
  | Epers_Name n -> sou base n

let string_of_efam_name base efam_name =
  match efam_name with
    Efam_Marriage -> "marriage"
  | Efam_NoMarriage -> "relation"
  | Efam_NoMention -> "relation"
  | Efam_Engage -> "engagement"
  | Efam_Divorce -> "divorce"
  | Efam_Separated -> "separation"
  | Efam_Annulation -> "annulation"
  | Efam_MarriageBann -> "marriage bann"
  | Efam_MarriageContract -> "marriage contract"
  | Efam_MarriageLicense -> "marriage licence"
  | Efam_PACS -> "PACS"
  | Efam_Residence -> "residence"
  | Efam_Name n -> sou base n

let print_base_error oc base =
  function
    AlreadyDefined p ->
      Printf.fprintf oc "%s\nis defined several times\n" (designation base p)
  | OwnAncestor p ->
      Printf.fprintf oc "%s\nis his/her own ancestor\n" (designation base p)
  | BadSexOfMarriedPerson p ->
      Printf.fprintf oc "%s\n  bad sex for a married person\n" (designation base p)

let print_base_warning oc base =
  function
  | BigAgeBetweenSpouses (fath, moth, a) ->
      Printf.fprintf oc
        "The difference of age between %s and %s is quite important: %d\n"
        (designation base fath) (designation base moth) a.year
  | BirthAfterDeath p ->
      Printf.fprintf oc "%s\n  born after his/her death\n" (designation base p)
  | ChangedOrderOfChildren (ifam, _, _, _) ->
      let cpl = foi base ifam in
      Printf.fprintf oc "Changed order of children of %s and %s\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)))
  | ChildrenNotInOrder (ifam, _, elder, x) ->
      let cpl = foi base ifam in
      Printf.fprintf oc
        "The following children of\n  %s\nand\n  %s\nare not in order:\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      Printf.fprintf oc "- %s\n" (designation base elder);
      Printf.fprintf oc "- %s\n" (designation base x)
  | ChangedOrderOfMarriages (p, _, _) ->
      Printf.fprintf oc "Changed order of marriages of %s\n" (designation base p)
  | ChangedOrderOfFamilyEvents (ifam, _, _) ->
      let cpl = foi base ifam in
      Printf.fprintf oc "Changed order of family's events for %s\n"
        (designation base (poi base (get_father cpl)));
      Printf.fprintf oc "Changed order of family's events for %s\n"
        (designation base (poi base (get_mother cpl)))
  | ChangedOrderOfPersonEvents (p, _, _) ->
      Printf.fprintf oc "Changed order of person's events for %s\n"
        (designation base p)
  | CloseChildren (ifam, _, elder, x) ->
      let cpl = foi base ifam in
      Printf.fprintf oc
        "The following children of\n  %s\nand\n  %s\nare born very close:\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      Printf.fprintf oc "- %s\n" (designation base elder);
      Printf.fprintf oc "- %s\n" (designation base x)
  | DeadOld (p, a) ->
      Printf.fprintf oc "%s died at the advanced age of %d years old\n"
        (designation base p) a.year
  | DeadTooEarlyToBeFather (father, child) ->
      Printf.fprintf oc "%s\n" (designation base child);
      Printf.fprintf oc
        "  is born more than 2 years after the death of his/her father\n";
      Printf.fprintf oc "%s\n" (designation base father)
  | FEventOrder (p, e1, e2) ->
      Printf.fprintf oc "%s's %s before his/her %s\n" (designation base p)
        (string_of_efam_name base e1.efam_name)
        (string_of_efam_name base e2.efam_name)
  | FWitnessEventAfterDeath (p, e) ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "witnessed the %s after his/her death\n"
        (string_of_efam_name base e.efam_name)
  | FWitnessEventBeforeBirth (p, e) ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "witnessed the %s before his/her birth\n"
        (string_of_efam_name base e.efam_name)
  | IncoherentSex (p, fixed, not_fixed) ->
      Printf.fprintf oc "%s\n  sex not coherent with relations" (designation base p);
      if fixed > 0 then
        if not_fixed > 0 then
          Printf.fprintf oc " (fixed in %d of the %d cases)" fixed
            (fixed + not_fixed)
        else Printf.fprintf oc " (fixed)";
      Printf.fprintf oc "\n"
  | IncoherentAncestorDate (anc, p) ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "  has a younger ancestor:\n";
      Printf.fprintf oc "%s\n" (designation base anc)
  | MarriageDateAfterDeath p ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "marriage after his/her death\n"
  | MarriageDateBeforeBirth p ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "marriage before his/her birth\n"
  | MotherDeadAfterChildBirth (mother, child) ->
      Printf.fprintf oc "%s\n  is born after the death of his/her mother\n%s\n"
        (designation base child) (designation base mother)
  | ParentBornAfterChild (parent, child) ->
      Printf.fprintf oc "%s born after his/her child %s\n" (designation base parent)
        (designation base child)
  | ParentTooOld (p, a) ->
      Printf.fprintf oc "%s was parent at age of %d\n" (designation base p) a.year
  | ParentTooYoung (p, a) ->
      Printf.fprintf oc "%s was parent at age of %d\n" (designation base p) a.year
  | PossibleDuplicateFam (f1, f2) ->
    Printf.fprintf oc "possible duplicate families: %s and %s\n"
      (string_of_ifam f1) (string_of_ifam f2)
  | PEventOrder (p, e1, e2) ->
      Printf.fprintf oc "%s's %s before his/her %s\n" (designation base p)
        (string_of_epers_name base e1.epers_name)
        (string_of_epers_name base e2.epers_name)
  | PWitnessEventAfterDeath (p, e) ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "witnessed the %s after his/her death\n"
        (string_of_epers_name base e.epers_name)
  | PWitnessEventBeforeBirth (p, e) ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "witnessed the %s before his/her birth\n"
        (string_of_epers_name base e.epers_name)
  | TitleDatesError (p, t) ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "has incorrect title dates as:\n";
      Printf.fprintf oc "  %s %s\n" (sou base t.t_ident) (sou base t.t_place)
  | UndefinedSex _ -> ()
  | WitnessDateAfterDeath p ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "was witness after his/her death\n"
  | WitnessDateBeforeBirth p ->
      Printf.fprintf oc "%s\n" (designation base p);
      Printf.fprintf oc "was witness before his/her birth\n"
  | YoungForMarriage (p, a)
  | OldForMarriage (p, a) ->
      Printf.fprintf oc "%s married at age %d\n" (designation base p) a.year

type check_date = CheckBefore of int | CheckAfter of int | CheckOther of int | CheckInfered of check_date

let min_year_of p =
  let aux = function
    | { prec = After ; year } -> CheckAfter year
    | { prec = Before ; year } -> CheckBefore year
    | { year } -> CheckOther year
  in
  match Adef.od_of_cdate (get_birth p) with
  | Some (Dgreg (d, _)) -> Some (aux d)
  | Some (Dtext _) | None -> None

let dummy_date = CheckInfered (CheckOther max_int)

let rec check_ancestors base warning year year_tab ip ini_p =
  let infer = function
    | CheckBefore i -> CheckInfered (CheckBefore (pred i))
    | CheckAfter i -> CheckInfered (CheckAfter (pred i))
    | CheckOther i -> CheckInfered (CheckOther (pred i))
    | CheckInfered (CheckBefore i) -> CheckInfered (CheckBefore (pred i))
    | CheckInfered (CheckAfter i) -> CheckInfered (CheckAfter (pred i))
    | CheckInfered (CheckOther i) -> CheckInfered (CheckOther (pred i))
    | _ -> assert false
  in
  let own = function CheckInfered _ -> false | _ -> true in
  let test a b p p' =
    match a, b with
    | CheckAfter y
    , (CheckBefore y' | CheckOther y' | CheckInfered (CheckBefore y') | CheckInfered (CheckOther y'))
      when y >= y' ->
      warning (IncoherentAncestorDate (Lazy.force p, p'))
    | _ -> ()
  in
  if Gwdb.Marker.get year_tab ip = dummy_date then
    let p = poi base ip in
    let (new_year, new_ini_p) =
      match min_year_of p with
      | Some y -> y, p
      | None -> infer year, ini_p
    in
    Gwdb.Marker.set year_tab ip new_year ;
    test new_year year (lazy p) ini_p ;
    match get_parents p with
    | Some ifam ->
      let fam = foi base ifam in
      let f ip =
        let year = Gwdb.Marker.get year_tab ip in
        if year = dummy_date
        then check_ancestors base warning new_year year_tab ip new_ini_p
        else if own year then test year new_year (lazy (poi base ip)) new_ini_p
      in
      f @@ get_father fam ;
      f @@ get_mother fam
    | None -> ()

let check_base ?(verbose = false) ?(mem = false) base error warning changed_p =
  if not mem then begin
    Gwdb.load_persons_array base ;
    Gwdb.load_ascends_array base ;
    Gwdb.load_unions_array base ;
    Gwdb.load_couples_array base ;
  end ;
  let persons = Gwdb.ipers base in
  let len = Gwdb.Collection.length persons in

  let year_tab = Gwdb.iper_marker persons dummy_date in
  if verbose then begin
    Printf.eprintf "check persons\n" ;
    ProgrBar.start () ;
    Gwdb.Collection.iteri begin fun i ip ->
      ProgrBar.run i len ;
      let p = poi base ip in
      if Gwdb.Marker.get year_tab ip = dummy_date
      then check_ancestors base warning dummy_date year_tab ip p ;
      match CheckItem.person ~onchange:false base warning p with
      | Some ippl -> List.iter changed_p ippl
      | None -> ()
    end persons ;
    ProgrBar.finish ()
  end else begin
    Gwdb.Collection.iter begin fun ip ->
      let p = poi base ip in
      if Gwdb.Marker.get year_tab ip = dummy_date
      then check_ancestors base warning dummy_date year_tab ip p ;
      match CheckItem.person ~onchange:false base warning p with
      | Some ippl -> List.iter changed_p ippl
      | None -> ()
    end persons ;
  end ;
  if not mem then begin
    Gwdb.clear_unions_array base ;
    Gwdb.load_families_array base ;
    Gwdb.load_descends_array base ;
  end ;
  let families = Gwdb.ifams base in
  let len = Gwdb.Collection.length families in
  if verbose then begin
    Printf.eprintf "check families\n" ;
    ProgrBar.start ();
    Gwdb.Collection.iteri begin fun i ifam ->
      ProgrBar.run i len ;
      CheckItem.family ~onchange:false base warning ifam @@ foi base ifam
    end families ;
    ProgrBar.finish ();
  end else begin
    Gwdb.Collection.iter begin fun ifam ->
      CheckItem.family ~onchange:false base warning ifam @@ foi base ifam
    end families ;
  end ;
  if not mem then begin
    Gwdb.clear_persons_array base ;
    Gwdb.clear_families_array base ;
    Gwdb.clear_descends_array base ;
  end ;
  Consang.check_noloop base error ;
  if not mem then begin
    Gwdb.clear_ascends_array base ;
    Gwdb.clear_couples_array base ;
  end
