(* /!\ This is mostly copy/paste of the Perso module /!\ *)
(* Sync with perso from ed7525bac *)
open Geneweb

open Config
open Def
open Gwdb
open Util

type fam = (ifam * family * (iper * iper * iper) * bool)

type rel = (relation * person option)

type env =
  { all_gp : Perso.generation_person list option
  ; baseprefix : string option
  ; desc_level_table : (int array * int array) Lazy.t option
  ; desc_mark : bool array ref option
  ; f_link : bool option
  ; fam : fam option
  ; fam_link : fam option
  ; p_link : bool option
  ; prev_fam : fam option
  ; sosa : (iper * (Sosa.t * person) option) list ref option
  ; sosa_ref : person option Lazy.t option
  ; src : string option
  }

let conf_w_baseprefix conf env =
  match env.baseprefix with
  | Some baseprefix -> { conf with command = baseprefix }
  | None -> conf

let empty = { all_gp = None
            ; baseprefix = None
            ; desc_level_table = None
            ; desc_mark = None
            ; fam = None
            ; f_link = None
            ; fam_link = None
            ; p_link = None
            ; prev_fam = None
            ; sosa = None
            ; sosa_ref = None
            ; src = None
            }

let env = empty

let get_env x = match x with Some x -> x | None -> raise Not_found

let safe_sou base istr k =
  if istr = dummy_istr then ""
  else k @@ sou base istr

let mk_note conf base env note =
  safe_sou base note @@ fun s ->
  let s = string_with_macros conf env s in
  let lines = Wiki.html_of_tlsw conf s in
  let wi =
    { Wiki.wi_mode = "NOTES"
    ; Wiki.wi_cancel_links = conf.cancel_links
    ; Wiki.wi_file_path = Notes.file_path conf base
    ; Wiki.wi_person_exists = person_exists conf base
    ; Wiki.wi_always_show_link = conf.wizard || conf.friend
    }
  in
  Wiki.syntax_links conf wi (String.concat "\n" lines)

let mk_person_note conf base p note =
  let env = ['i', (fun () -> Util.default_image_name base p)] in
  mk_note conf base env note

let sex_of_index = function
  | 0 -> Male
  | 1 -> Female
  | 2 -> Neuter
  | _ -> raise (Invalid_argument "sex_of_index")

module Person = struct

  let access conf base p =
    Util.acces conf base p

  let birth_date p = Adef.od_of_cdate (get_birth p)

  let birth_place conf base p =
    safe_sou base (get_birth_place p) @@ Util.string_of_place conf

  let birth_note conf base p =
    mk_person_note conf base p (get_birth_note p)

  let baptism_date p = Adef.od_of_cdate (get_baptism p)

  let baptism_place conf base p =
    safe_sou base (get_baptism_place p) @@ Util.string_of_place conf

  let baptism_note conf base p =
    mk_person_note conf base p (get_baptism_note p)

  let burial p =
    get_burial p

  let burial_place conf base p =
    safe_sou base (get_burial_place p) @@ Util.string_of_place conf

  let burial_note conf base p =
    mk_person_note conf base p (get_burial_note p)

  let children base p = ChangeChildren.select_children_of base p

  let consanguinity p =
    let c = get_consang p in
    if c != Adef.fix (-1) && c >= Adef.fix_of_float 0.0001
    then Adef.float_of_fix c
    else 0.

  let cremation_place conf base p =
    safe_sou base (get_burial_place p) @@ Util.string_of_place conf

  let dates conf base p =
    DateDisplay.short_dates_text conf base p

  let death p =
    get_death p

  let death_place conf base p =
    safe_sou base (get_death_place p) @@ Util.string_of_place conf

  let death_note conf base p =
    mk_person_note conf base p (get_death_note p)

  let digest base p =
    Update.digest_person (UpdateInd.string_person_of base p)

  let events conf base p =
    Perso.events_list conf base p

  let first_name base p =
    p_first_name base p

  let first_name_aliases base p =
    List.map (sou base) (get_first_names_aliases p)

  let first_name_key base p =
    code_varenv (Name.lower (p_first_name base p))

  let first_name_key_val base p =
    Name.lower (p_first_name base p)

  let history_file base p =
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    let occ = get_occ p in HistoryDiff.history_file fn sn occ

  let image base p =
    sou base (get_image p)

  let is_accessible_by_key conf base p =
    Util.accessible_by_key
      conf base p (p_first_name base p) (p_surname base p)

  let is_birthday conf p =
    match Adef.od_of_cdate (get_birth p) with
    | Some (Dgreg (d, _)) ->
      if d.prec = Sure && get_death p = NotDead then
        d.day = conf.today.day && d.month = conf.today.month &&
        d.year < conf.today.year ||
        not (Date.leap_year conf.today.year) && d.day = 29 &&
        d.month = 2 && conf.today.day = 1 && conf.today.month = 3
      else false
    | _ -> false

  let linked_page conf base p s =
    let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
    let fname = Filename.concat bdir "notes_links" in
    let db = NotesLinks.read_db_from_file fname in
    let db = Notes.merge_possible_aliases conf db in
    let key =
      let fn = Name.lower (sou base (get_first_name p)) in
      let sn = Name.lower (sou base (get_surname p)) in
      fn, sn, get_occ p
    in
    List.fold_left (Perso.linked_page_text conf base p s key) "" db

  let notes conf base p =
    if not conf.no_note then mk_person_note conf base p (get_notes p)
    else ""

  let occ p =
    get_occ p

  let occupation conf base p =
    mk_note conf base [] (get_occupation p)

  let parents p = get_parents p

  let rparents p = get_rparents p

  let related conf base p =
    List.sort
      (fun (c1, _) (c2, _) ->
         let mk_date c =
           match Adef.od_of_cdate (get_baptism c) with
           | None -> Adef.od_of_cdate (get_birth c)
           | x -> x
         in
         match mk_date c1, mk_date c2 with
         | Some d1, Some d2 -> Date.compare_date d1 d2
         | _ -> -1)
    @@
    List.fold_left (fun list ic ->
        let c = pget conf base ic in
        List.fold_left (fun acc r -> match r.r_fath, r.r_moth with
            | Some ip, _  when ip = get_iper p -> (c, r) :: acc
            | _ , Some ip when ip = get_iper p -> (c, r) :: acc
            | _ -> acc)
          list (get_rparents c) )
      [] (List.sort_uniq compare (get_related p))

  (* Why isnt this already unique? *)
  let relations p =
    List.sort_uniq compare (get_related p)

  let siblings base p =
    match get_parents p with
    | Some ifam ->
      let ip = get_iper p in
      Array.fold_right
        (fun i acc -> if i <> ip then i :: acc else acc)
        (get_children (foi base ifam))
        []
    | None -> []

  let half_siblings base p =
    match get_parents p with
    | Some ifam ->
      let ip = get_iper p in
      let f = foi base ifam in
      let filter = fun (acc : iper list) i ->
        if i = ifam then acc else
          Array.fold_right
            (fun i acc -> if i <> ip then i :: acc else acc)
            (get_children (foi base i)) acc
      in
      let hs =
        let ifath = get_father f in
        if ifath = dummy_iper then []
        else Array.fold_left filter [] (get_family @@ poi base ifath)
      in
      let imoth = get_mother f in
      if imoth = dummy_iper then hs
      else Array.fold_left filter hs (get_family @@ poi base imoth)
    | None -> []

  let public_name base p =
    safe_sou base (get_public_name p) @@ fun x -> x

  let qualifier base p =
    match get_qualifiers p with
    | nn :: _ -> sou base nn
    | _ -> ""

  let qualifiers base p =
    List.map (sou base) (get_qualifiers p)

  let sex p =
    index_of_sex (get_sex p)

  let psources base p =
    safe_sou base (get_psources p) @@ fun x -> x

  let surname base p =
    p_surname base p

  let surname_aliases base p =
    List.map (sou base) (get_surnames_aliases p)

  let surname_key base p =
    code_varenv (Name.lower (p_surname base p))

  let surname_key_val base p =
    Name.lower (p_surname base p)

  let titles p =
    get_titles p

end

module Family = struct

  let children (_, fam, _, _) = get_children fam

  let divorce_date (_, fam, _, auth) =
    match get_divorce fam with
    | Divorced d when auth -> Adef.od_of_cdate d
    | _ -> None

  let father (_, _, (ifath, _, _), _) =
    ifath

  let ifam (ifam, _, _, _) =
    string_of_ifam ifam

  let marriage_date (_, fam, (_, _, _), auth) =
    if auth then Adef.od_of_cdate (get_marriage fam)
    else None

  let marriage_place base (_, fam, _, _) =
    sou base (get_marriage_place fam)

  let marriage_note conf base (_, fam, _, auth) =
    if auth then mk_note conf base [] (get_marriage_note fam)
    else ""

  let marriage_source base (_, fam, _, auth) =
    if auth then sou base (get_marriage_src fam)
    else ""

  let mother (_, _, (_, imoth, _), _) =
    imoth

  let origin_file conf base (_, fam, _, _) =
    if conf.wizard then sou base (get_origin_file fam)
    else ""

  let spouse_iper (_, _, (_, _, ip), _) = ip

  let witnesses (_, fam, _, auth) =
    if auth then get_witnesses fam else [||]

  let events (_, fam, (_, _, isp), auth) =
    if auth then
      List.fold_right
        (fun evt fam_fevents ->
           let name = Perso.Fevent evt.efam_name in
           let date = evt.efam_date in
           let place = evt.efam_place in
           let note = evt.efam_note in
           let src = evt.efam_src in
           let wl = evt.efam_witnesses in
           let x = name, date, place, note, src, wl, Some isp in
           x :: fam_fevents)
        (get_fevents fam) []
    else []

end

module Event = struct

  let date (_, d, _, _, _, _, _) =
    Adef.od_of_cdate d

  let place conf base (_, _, p, _, _, _, _) =
    safe_sou base p @@ Util.string_of_place conf

  let spouse_opt (_, _, _, _, _, _, isp) =
    isp

  let src base (_, _, _, _, s, _, _) =
    safe_sou base s @@ fun x -> x

  let kind (n, _, _, _, _, _, _) =
    match n with
    | Geneweb.Perso.Pevent Epers_Birth -> "EPERS_BIRTH"
    | Pevent Epers_Baptism -> "EPERS_BAPTISM"
    | Pevent Epers_Death -> "EPERS_DEATH"
    | Pevent Epers_Burial -> "EPERS_BURIAL"
    | Pevent Epers_Cremation -> "EPERS_CREMATION"
    | Pevent Epers_Accomplishment -> "EPERS_ACCOMPLISHMENT"
    | Pevent Epers_Acquisition -> "EPERS_ACQUISITION"
    | Pevent Epers_Adhesion -> "EPERS_ADHESION"
    | Pevent Epers_BaptismLDS -> "EPERS_BAPTISMLDS"
    | Pevent Epers_BarMitzvah -> "EPERS_BARMITZVAH"
    | Pevent Epers_BatMitzvah -> "EPERS_BATMITZVAH"
    | Pevent Epers_Benediction -> "EPERS_BENEDICTION"
    | Pevent Epers_ChangeName -> "EPERS_CHANGENAME"
    | Pevent Epers_Circumcision -> "EPERS_CIRCUMCISION"
    | Pevent Epers_Confirmation -> "EPERS_CONFIRMATION"
    | Pevent Epers_ConfirmationLDS -> "EPERS_CONFIRMATIONLDS"
    | Pevent Epers_Decoration -> "EPERS_DECORATION"
    | Pevent Epers_DemobilisationMilitaire -> "EPERS_DEMOBILISATIONMILITAIRE"
    | Pevent Epers_Diploma -> "EPERS_DIPLOMA"
    | Pevent Epers_Distinction -> "EPERS_DISTINCTION"
    | Pevent Epers_Dotation -> "EPERS_DOTATION"
    | Pevent Epers_DotationLDS -> "EPERS_DOTATIONLDS"
    | Pevent Epers_Education -> "EPERS_EDUCATION"
    | Pevent Epers_Election -> "EPERS_ELECTION"
    | Pevent Epers_Emigration -> "EPERS_EMIGRATION"
    | Pevent Epers_Excommunication -> "EPERS_EXCOMMUNICATION"
    | Pevent Epers_FamilyLinkLDS -> "EPERS_FAMILYLINKLDS"
    | Pevent Epers_FirstCommunion -> "EPERS_FIRSTCOMMUNION"
    | Pevent Epers_Funeral -> "EPERS_FUNERAL"
    | Pevent Epers_Graduate -> "EPERS_GRADUATE"
    | Pevent Epers_Hospitalisation -> "EPERS_HOSPITALISATION"
    | Pevent Epers_Illness -> "EPERS_ILLNESS"
    | Pevent Epers_Immigration -> "EPERS_IMMIGRATION"
    | Pevent Epers_ListePassenger -> "EPERS_LISTEPASSENGER"
    | Pevent Epers_MilitaryDistinction -> "EPERS_MILITARYDISTINCTION"
    | Pevent Epers_MilitaryPromotion -> "EPERS_MILITARYPROMOTION"
    | Pevent Epers_MilitaryService -> "EPERS_MILITARYSERVICE"
    | Pevent Epers_MobilisationMilitaire -> "EPERS_MOBILISATIONMILITAIRE"
    | Pevent Epers_Naturalisation -> "EPERS_NATURALISATION"
    | Pevent Epers_Occupation -> "EPERS_OCCUPATION"
    | Pevent Epers_Ordination -> "EPERS_ORDINATION"
    | Pevent Epers_Property -> "EPERS_PROPERTY"
    | Pevent Epers_Recensement -> "EPERS_RECENSEMENT"
    | Pevent Epers_Residence -> "EPERS_RESIDENCE"
    | Pevent Epers_Retired -> "EPERS_RETIRED"
    | Pevent Epers_ScellentChildLDS -> "EPERS_SCELLENTCHILDLDS"
    | Pevent Epers_ScellentParentLDS -> "EPERS_SCELLENTPARENTLDS"
    | Pevent Epers_ScellentSpouseLDS -> "EPERS_SCELLENTSPOUSELDS"
    | Pevent Epers_VenteBien -> "EPERS_VENTEBIEN"
    | Pevent Epers_Will -> "EPERS_WILL"
    | Fevent Efam_Marriage -> "EFAM_MARRIAGE"
    | Fevent Efam_NoMarriage -> "EFAM_NO_MARRIAGE"
    | Fevent Efam_NoMention -> "EFAM_NO_MENTION"
    | Fevent Efam_Engage -> "EFAM_ENGAGE"
    | Fevent Efam_Divorce -> "EFAM_DIVORCE"
    | Fevent Efam_Separated -> "EFAM_SEPARATED"
    | Fevent Efam_Annulation -> "EFAM_ANNULATION"
    | Fevent Efam_MarriageBann -> "EFAM_MARRIAGE_BANN"
    | Fevent Efam_MarriageContract -> "EFAM_MARRIAGE_CONTRACT"
    | Fevent Efam_MarriageLicense -> "EFAM_MARRIAGE_LICENSE"
    | Fevent Efam_PACS -> "EFAM_PACS"
    | Fevent Efam_Residence -> "EFAM_RESIDENCE"
    | Pevent Epers_Name _ -> ""
    | Fevent Efam_Name _ -> ""

  let name conf base (n, _, _, _, _, _, _) =
    match n with
    | Geneweb.Perso.Pevent name -> Util.string_of_pevent_name conf base name
    | Fevent name -> Util.string_of_fevent_name conf base name

  let note conf base (_, _, _, n, _, _, _) =
    if conf.no_note then "" else sou base n

  let witnesses (_, _, _, _, _, w, _) =
    w

end
