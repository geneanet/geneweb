open Geneweb
open Jingoo
open Jg_types

(* FIXME: remove [try ... with Not_found -> ...] wrapping.  *)

let person_ht = Hashtbl.create 32

let mk_opt fn = function None -> Tnull | Some x -> fn x

let rec date_compare_aux date1 date2 =
  let y1 = field date1 "year" in
  let y2 = field date2 "year" in
  match Jg_runtime.jg_compare y1 y2 with
  | Tint 0 -> compare_month date1 date2
  | x -> x
and compare_month date1 date2 = match field date1 "month", field date2 "month" with
  | Tint 0, Tint 0 -> cmp_prec date1 date2
  | Tint 0, Tint _ ->
    if field date1 "prec" = Tstr "after" then Tint 1 else cmp_prec date1 date2
  | Tint _, Tint 0 ->
    if field date2 "prec" = Tstr "after" then Tint (-1) else cmp_prec date1 date2
  | m1, m2 -> match Jg_runtime.jg_compare m1 m2 with
    | Tint 0 -> compare_day date1 date2
    | x -> x
and compare_day date1 date2 = match field date1 "day", field date2 "day" with
  | Tint 0, Tint 0 -> cmp_prec date1 date2
  | Tint 0, Tint _ ->
    if field date1 "prec" = Tstr "after" then Tint 1 else cmp_prec date1 date2
  | Tint _, Tint 0 ->
    if field date2 "prec" = Tstr "after" then Tint (-1) else cmp_prec date1 date2
  | d1, d2 -> match Jg_runtime.jg_compare d1 d2 with
    | Tint 0 -> cmp_prec date1 date2
    | x -> x
and cmp_prec d1 d2 =
  match field d1 "prec", field d2 "prec" with
  | Tstr ("sure"|"about"|"maybe"), Tstr ("sure"|"about"|"maybe") -> Tint 0
  | Tstr "after", Tstr "after" | Tstr "before", Tstr "before" -> Tint 0
  | Tstr "oryear", Tstr "oryear" | Tstr "yearint", Tstr "yearint" ->
    date_compare_aux (field d1 "d2") (field d2 "d2")
  | _, Tstr "after" | Tstr "before", _ -> Tint (-1)
  | Tstr "after", _ | _, Tstr "before" -> Tint 1
  | _ -> Tint 0
and field = Jg_runtime.jg_obj_lookup

let rec mk_family (conf : Config.config) base ((_, fam, _, _) as fcd) =
  let module E = Ezgw.Family in
  let get wrap fn = try wrap (fn fcd) with Not_found -> Tnull in
  let get_str = get box_string in
  let f = E.father fcd in
  let m = E.mother fcd in
  let divorce_date = mk_opt mk_date (E.divorce_date fcd) in
  let father = lazy_get_n_mk_person conf base f in
  let mother = lazy_get_n_mk_person conf base m in
  let spouse =
    let (_, _, (ifath, imoth, ispouse), _) = fcd in
    if ifath = ispouse then father
    else if imoth = ispouse then mother
    else Tnull
  in
  let children = Tarray (Array.map (lazy_get_n_mk_person conf base) (E.children fcd) ) in
  let events = lazy_list (mk_event conf base) (E.events fcd) in
  let marriage_date = mk_opt mk_date (E.marriage_date fcd) in
  let marriage_place = get_str (E.marriage_place base) in
  let marriage_note = get_str (E.marriage_note conf base) in
  let marriage_source = get_str (E.marriage_source base) in
  let relation = mk_fam_relation (Gwdb.get_relation fam) in
  let separation = mk_fam_separation (Gwdb.get_divorce fam) in
  let ifam = get_str E.ifam in
  let witnesses =
    try lazy_array (get_n_mk_person conf base) (E.witnesses fcd)
    with Not_found -> Tnull
  in
  let origin_file = Tlazy (lazy (get_str (E.origin_file conf base))) in
  Tpat begin function
    | "divorce_date" -> divorce_date
    | "children" -> children
    | "father" -> father
    | "events" -> events
    | "ifam" -> ifam
    | "marriage_date" -> marriage_date
    | "marriage_place" -> marriage_place
    | "marriage_note" -> marriage_note
    | "marriage_source" -> marriage_source
    | "mother" -> mother
    | "origin_file" -> origin_file
    | "relation" -> relation
    | "separation" -> separation
    | "spouse" -> spouse
    | "witnesses" -> witnesses
    | _ -> raise Not_found
  end

and mk_fam_relation = function
  | Married -> Tstr "MARRIED"
  | NotMarried -> Tstr "NOT_MARRIED"
  | Engaged -> Tstr "ENGAGED"
  | NoSexesCheckNotMarried -> Tstr "NO_SEXES_CHECK_NOT_MARRIED"
  | NoMention -> Tnull
  | NoSexesCheckMarried -> Tstr "NO_SEXES_CHECK_MARRIED"
  | MarriageBann -> Tstr "MARRIAGE_BANN"
  | MarriageContract -> Tstr "MARRIAGE_CONTRACT"
  | MarriageLicense -> Tstr "MARRIAGE_LICENSE"
  | Pacs -> Tstr "PACS"
  | Residence -> Tstr "RESIDENCE"

and mk_fam_separation = function
  | Divorced _ -> Tstr "DIVORCED"
  | Separated -> Tstr "SEPARATED"
  | NotDivorced -> Tnull

and get_n_mk_family conf base ?(origin = Gwdb.dummy_iper) ifam cpl =
  let ifath = Gwdb.get_father cpl in
  let imoth = Gwdb.get_mother cpl in
  let cpl =
    ifath, imoth, (if ifath = origin then imoth
                   else if imoth = origin then ifath
                   else origin)
  in
  let m_auth =
    Util.authorized_age conf base (Gwdb.poi base ifath)
    && Util.authorized_age conf base (Gwdb.poi base imoth)
  in
  mk_family conf base (ifam, Gwdb.foi base ifam, cpl, m_auth)

and date_compare = func_arg2_no_kw date_compare_aux

and date_eq = func_arg2_no_kw (fun d1 d2 -> Tbool (date_compare_aux d1 d2 = Tint 0))

and dtext_eq =
  func_arg2_no_kw @@ fun d1 d2 ->
  Tbool ((Jg_runtime.jg_obj_lookup d1 "__str__") = (Jg_runtime.jg_obj_lookup d2 "__str__"))

and mk_date = function
  | Def.Dtext s ->
    Tpat begin function
      | "__str__" -> Tstr s
      | "__compare__" -> func_arg2_no_kw (fun _ _ -> Tint 0)
      | "__eq__" -> dtext_eq
      | "__Dtext__" -> Tbool true
      | _ -> raise Not_found
    end
  | Dgreg (d, c) ->
    let year = Tint d.Def.year in
    let month = Tint d.Def.month in
    let day = Tint d.Def.day in
    let prec = to_prec d.Def.prec in
    let d2 = match d.Def.prec with
      | OrYear d2 | YearInt d2 ->
        mk_date (Def.Dgreg ( { Def.day = d2.Def.day2
                             ; month = d2.Def.month2
                             ; year = d2.Def.year2
                             ; prec = Def.Sure ; delta = 0 }
                           , c) )
      | _ -> Tnull
    in
    let calendar = match c with
      | Dgregorian -> Tstr "Dgregorian"
      | Djulian -> Tstr "Djulian"
      | Dfrench -> Tstr "Dfrench"
      | Dhebrew -> Tstr "Dhebrew"
    in
    Tpat begin function
      | "calendar" -> calendar
      | "d2" -> d2
      | "day" -> day
      | "month" -> month
      | "prec" -> prec
      | "year" -> year
      | "__compare__" -> date_compare
      | "__eq__" -> date_eq
      | _ -> raise Not_found
    end

and to_dmy d =
  let int s = match Jg_runtime.jg_obj_lookup d s with Tint i -> i | _ -> 0 in
  { Def.day = int "day" ; month = int "month" ; year = int "year"
  ; prec = of_prec d
  ; delta = 0 }

and to_dmy2 d =
  let int s = match Jg_runtime.jg_obj_lookup d s with Tint i -> i | _ -> 0 in
  { Def.day2 = int "day" ; month2 = int "month" ; year2 = int "year"
  ; delta2 = 0 }

and to_prec = function
  | Def.Sure -> Tstr "sure"
  | About -> Tstr "about"
  | Maybe -> Tstr "maybe"
  | Before -> Tstr "before"
  | After -> Tstr "after"
  | OrYear _ -> Tstr "oryear"
  | YearInt _ -> Tstr "yearint"

and of_prec d = match Jg_runtime.jg_obj_lookup d "prec" with
  | Tstr "sure" -> Def.Sure
  | Tstr "about" -> About
  | Tstr "maybe" -> Maybe
  | Tstr "before" -> Before
  | Tstr "after" -> After
  | Tstr "oryear" -> OrYear (to_dmy2 d)
  | Tstr "yearint" -> YearInt (to_dmy2 d)
  | _ -> assert false

and to_gregorian_aux calendar d =
  let d = to_dmy d in
  match calendar with
  | "Dgregorian" -> d
  | "Djulian" -> Calendar.gregorian_of_julian d
  | "Dfrench" -> Calendar.gregorian_of_french d
  | "Dhebrew" -> Calendar.gregorian_of_hebrew d
  | _ -> assert false

and of_calendar d = match Jg_runtime.jg_obj_lookup d "calendar" with
  | Tstr "Dgregorian" -> Def.Dgregorian
  | Tstr "Djulian" -> Def.Djulian
  | Tstr "Dfrench" -> Def.Dfrench
  | Tstr "Dhebrew" -> Def.Dhebrew
  | _ -> assert false

and module_date conf =
  let now =
    Tvolatile (fun () ->
        let now = Unix.gmtime @@ Unix.time () in
        let day = Tint now.tm_mday in
        let month = Tint (now.tm_mon + 1) in
        let year = Tint (now.tm_year + 1900) in
        Tpat (function
            | "day" -> day
            | "month" -> month
            | "year" -> year
            | "prec" -> Tstr "sure"
            | _ -> raise Not_found)
      )
  in
  let death_symbol = DateDisplay.death_symbol conf in
  let string_of_ondate =
    func_arg1 @@ fun ?(kwargs=[]) d ->
    try
      let link = Opt.map_default false unbox_bool (List.assoc_opt "link" kwargs) in
      Tstr (DateDisplay.string_of_ondate { conf with cancel_links = not link } @@
            Def.Dgreg (to_dmy d, of_calendar d) )
    with e ->
      if Jg_runtime.jg_obj_lookup d "__Dtext__" = Tbool true
      then Jg_runtime.jg_obj_lookup d "__str__"
      else raise e
  in
  let code_french_year =
    func_arg1_no_kw (fun i -> box_string @@ DateDisplay.code_french_year conf (unbox_int i))
  in
  let string_of_age =
    func_arg1_no_kw (fun d -> box_string @@ DateDisplay.string_of_age conf (to_dmy d) )
  in
  let sub =
    func_arg2_no_kw begin fun d1 d2 ->
      mk_dmy @@ Date.time_elapsed (to_dmy d2) (to_dmy d1)
    end
  in
  let calendar =
    func_arg2_no_kw begin fun dst d ->
      (* let src = unbox_string @@ Jg_runtime.jg_obj_lookup d "calendar" in *)
      let convert fn = mk_dmy @@ fn @@ to_dmy (* @@ to_gregorian_aux src *) d in
      match unbox_string @@ dst with
      | "Dgregorian" -> convert (fun x -> x)
      | "Djulian" -> convert Calendar.julian_of_gregorian
      | "Dfrench" -> convert Calendar.french_of_gregorian
      | "Dhebrew" -> convert Calendar.hebrew_of_gregorian
      | s -> failwith @@ "Unknown calendar: " ^ s
    end
  in
  Tpat (function
      | "calendar" -> calendar
      | "compare" -> date_compare
      | "death_symbol" -> Tstr death_symbol
      | "code_french_year" -> code_french_year
      | "eq" -> date_eq
      | "now" -> now
      | "string_of_age" -> string_of_age
      | "string_of_ondate" -> string_of_ondate
      | "sub" -> sub
      | _ -> raise Not_found
    )

and lazy_array : 'a . ('a -> tvalue) -> 'a array -> tvalue = fun fn -> function
  | [||] -> Tarray [||]
  | a -> Tlazy (lazy (box_array @@ Array.map fn a))

and lazy_list : 'a . ('a -> tvalue) -> 'a list -> tvalue = fun fn -> function
  | [] -> Tlist []
  | l -> Tlazy (lazy (box_list @@ List.map fn l))

and lazy_get_n_mk_person conf base i =
  let lp = lazy (get_n_mk_person conf base i) in
  let iper = Tstr (Gwdb.string_of_iper i) in
  Tpat (function "iper" -> iper | s -> unbox_pat (Lazy.force lp) @@ s)

and pget conf base ip =
  let open Geneweb in
  let open Config in
  let open Def in
  let open Gwdb in
  if ip = dummy_iper
  then unsafe_mk_person conf base (Gwdb.empty_person base ip)
  else
    let p = poi base ip in
    if not (Util.authorized_age conf base p)
    then
      if conf.use_restrict
      then unsafe_mk_person conf base (Gwdb.empty_person base ip)
      else if conf.hide_names || get_access p = Private
      then
        let lazy_p = lazy (unbox_pat @@ unsafe_mk_semi_public_person conf base p) in
        Tpat begin function
          | "first_name" | "surname" -> Tstr "x"
          | "first_name_aliases" | "surname_aliases" -> Tlist []
          | x -> (Lazy.force lazy_p) x
        end
      else unsafe_mk_semi_public_person conf base p
    else unsafe_mk_person conf base p

and get_n_mk_person conf base (i : Gwdb.iper) =
  try Hashtbl.find person_ht i
  with Not_found ->
    let p = pget conf base i in
    Hashtbl.add person_ht i p ;
    p

and mk_related conf base acc =
  let mk_rel i t s =
    let iper = Tstr (Gwdb.string_of_iper i) in
    let kind = match t with
      | Def.Adoption -> Tstr "ADOPTION"
      | Def.Recognition -> Tstr "RECOGNITION"
      | Def.CandidateParent -> Tstr "CANDIDATEPARENT"
      | Def.GodParent -> Tstr "GODPARENT"
      | Def.FosterParent -> Tstr "FOSTERPARENT"
    in
    let sources = Tstr (Gwdb.sou base s) in
    let lp = lazy (get_n_mk_person conf base i) in
    Tpat (function
        | "sources" -> sources
        | "kind" -> kind
        | "iper" -> iper
        | s -> unbox_pat (Lazy.force lp) @@ s)
  in
  function
  | { Def.r_fath = None ; r_moth = Some i ; r_type ; r_sources }
  | { r_fath = Some i ; r_moth = None ; r_type ; r_sources } ->
    mk_rel i r_type r_sources :: acc
  | { r_fath = Some i1 ; r_moth = Some i2 ; r_type ; r_sources } ->
    mk_rel i1 r_type r_sources :: mk_rel i2 r_type r_sources :: acc
  | _ -> Tnull :: acc

and mk_witness_kind = function
  | Def.Witness -> Tstr "WITNESS"
  | Def.Witness_GodParent -> Tstr "WITNESS_GODPARENT"
  | Def.Witness_Officer -> Tstr "WITNESS_OFFICER"

and mk_event conf base d =
  let module E = Ezgw.Event in
  let date = match E.date d with Some d -> mk_date d | None -> Tnull in
  let name = Tstr (E.name conf base d) in
  let spouse = match E.spouse_opt d with
    | None -> Tnull
    | Some i -> lazy_get_n_mk_person conf base i
  in
  let kind = Tstr (E.kind d) in
  let witnesses =
    match E.witnesses d with
    | [||] -> Tarray [||]
    | w ->
      let lw = lazy (Array.map (fun (i, _) -> get_n_mk_person conf base i) w) in
      (* We may want to filter on [ip] or [k] before really accessing the person entity *)
      Tarray (Array.mapi (fun i (ip, k) ->
          let kind = mk_witness_kind k in
          let iper = Tstr (Gwdb.string_of_iper ip) in
          Tpat (function
              | "kind" -> kind
              | "iper" -> iper
              | s -> unbox_pat (Lazy.force lw).(i) @@ s) )
          w )
  in
  let place = Tstr (E.place conf base d) in
  let src = Tstr (E.src base d) in
  let note = Tstr (E.note conf base d) in
  Tpat (function "date" -> date
               | "kind" -> kind
               | "name" -> name
               | "note" -> note
               | "place" -> place
               | "spouse" -> spouse
               | "src" -> src
               | "witnesses" -> witnesses
               | _ -> raise Not_found)

and mk_title base t =
  let ident = Tstr (Gwdb.sou base t.Def.t_ident) in
  let name = match t.t_name with
    | Tmain -> Tstr ""
    | Tname s -> Tstr (Gwdb.sou base s)
    | Tnone -> Tnull
  in
  let place = Tstr (Gwdb.sou base t.t_place) in
  let date_start = mk_opt mk_date (Adef.od_of_cdate t.t_date_start) in
  let date_end = mk_opt mk_date (Adef.od_of_cdate t.t_date_start) in
  let nth = Tint t.t_nth in
  Tpat (function
      | "ident" -> ident
      | "name" -> name
      | "place" -> place
      | "date_start" -> date_start
      | "date_end" -> date_end
      | "nth" -> nth
      | _ -> raise Not_found)

and unsafe_mk_semi_public_person conf base (p : Gwdb.person) =
  let iper' = Gwdb.get_iper p in
  let get wrap fn = try wrap (fn p) with Not_found -> Tnull in
  let get_str = get box_string in
  let module E = Ezgw.Person in
  let parents = match E.parents p with
    | Some ifam -> Some (lazy (Gwdb.foi base ifam))
    | None -> None
  in
  let mk_parent fn = match parents with
    | Some f -> Tlazy (lazy (get_n_mk_person conf base (fn @@ Lazy.force f)))
    | None -> Tnull
  in
  let lazy_families = lazy (Array.map (fun ifam -> ifam, Gwdb.foi base ifam) @@ Gwdb.get_family p) in
  let families =
    Tlazy (lazy (Tarray (Array.map (fun (ifam, cpl) -> get_n_mk_family conf base ~origin:iper' ifam cpl) @@
                         Lazy.force lazy_families) ) )
  in
  let spouses =
    Tlazy (lazy (Tarray (Array.map (fun (_, c) ->
        let f = Gwdb.get_father c in
        get_n_mk_person conf base (if f = iper' then Gwdb.get_mother c else f) )
        (Lazy.force lazy_families) ) ) )
  in
  let father = mk_parent Gwdb.get_father in
  let first_name = get_str (E.first_name base) in
  let first_name_aliases =
    box_list @@
    List.map box_string (E.first_name_aliases base p)
  in
  let access = get_str (E.access conf base) in
  let mother = mk_parent Gwdb.get_mother in
  let children = lazy_list (get_n_mk_person conf base) (E.children base p) in
  let iper = Tstr (Gwdb.string_of_iper iper') in
  let related =
    match E.rparents p with
    | [] -> Tlist []
    | r -> box_list @@ List.fold_left (mk_related conf base) [] r
  in
  let siblings_aux fn = lazy_list (get_n_mk_person conf base) (fn base p) in
  let siblings = siblings_aux E.siblings in
  let half_siblings = siblings_aux E.half_siblings in
  let surname = get_str (E.surname base) in
  let surname_aliases = Tlist (List.map box_string (E.surname_aliases base p) ) in
  Tpat
    (function
      | "access" -> access
      | "children" -> children
      | "families" -> families
      | "father" -> father
      | "first_name" -> first_name
      | "first_name_aliases" -> first_name_aliases
      | "half_siblings" -> half_siblings
      | "iper" -> iper
      | "mother" -> mother
      | "related" -> related
      | "siblings" -> siblings
      | "spouses" -> spouses
      | "surname" -> surname
      | "surname_aliases" -> surname_aliases
      | _ -> raise Not_found
    )

and get_sosa_person =
  let loaded = ref false in
  fun conf base p ->
    if not !loaded then (Perso.build_sosa_ht conf base ; loaded := true) ;
    let sosa = Perso.get_sosa_person p in
    if sosa = Sosa.zero then Tnull else Tstr (Sosa.to_string sosa)

and unsafe_mk_person conf base (p : Gwdb.person) =
  let get wrap fn = try wrap (fn p) with Not_found -> Tnull in
  let get_str = get box_string in
  let get_bool = get box_bool in
  let get_int = get box_int in
  let get_float = get box_float in
  let module E = Ezgw.Person in
  let parents = match E.parents p with
    | Some ifam -> Some (lazy (Gwdb.foi base ifam))
    | None -> None
  in
  let mk_parent fn = match parents with
    | Some f -> Tlazy (lazy (get_n_mk_person conf base (fn @@ Lazy.force f)))
    | None -> Tnull
  in
  let parents =
    match parents with
    | Some f ->
      Tlazy begin lazy begin
        let cpl = Lazy.force f in
        let ifam = Gwdb.get_ifam cpl in
        let ifath = Gwdb.get_father cpl in
        let imoth = Gwdb.get_mother cpl in
        let cpl = (ifath, imoth, Gwdb.dummy_iper) in
        let m_auth =
          Util.authorized_age conf base (Gwdb.poi base ifath)
          && Util.authorized_age conf base (Gwdb.poi base imoth)
        in
        mk_family conf base (ifam, Gwdb.foi base ifam, cpl, m_auth)
      end end
    | None -> Tnull
  in
  let iper' = Gwdb.get_iper p in
  let access = get_str (E.access conf base) in
  let baptism_date = mk_opt mk_date (E.baptism_date p) in
  let baptism_place = get_str (E.baptism_place conf base) in
  let birth_date = mk_opt mk_date (E.birth_date p) in
  let birth_place = get_str (E.birth_place conf base) in
  let burial = get mk_burial E.burial in
  let burial_place = get_str (E.burial_place conf base) in
  let children = lazy_list (get_n_mk_person conf base) (E.children base p) in
  let consanguinity = get_float (E.consanguinity) in
  let cremation_place = get_str (E.cremation_place conf base) in
  let dates = get_str (E.dates conf base) in
  let death = get mk_death E.death in
  let death_place = get_str (E.death_place conf base) in
  let digest = Tlazy (lazy (get_str (E.digest base) ) ) in
  let events = lazy_list (mk_event conf base) (E.events conf base p) in
  let lazy_families = lazy (Array.map (fun ifam -> ifam, Gwdb.foi base ifam) @@ Gwdb.get_family p) in
  let families =
    Tlazy (lazy (Tarray (Array.map (fun (ifam, cpl) -> get_n_mk_family conf base ~origin:iper' ifam cpl) @@
                         Lazy.force lazy_families) ) )
  in
  let spouses =
    Tlazy (lazy (Tarray (Array.map (fun (_, c) ->
        let f = Gwdb.get_father c in
        get_n_mk_person conf base (if f = iper' then Gwdb.get_mother c else f) )
        (Lazy.force lazy_families) ) ) )
  in
  let father = mk_parent Gwdb.get_father in
  let first_name = get_str (E.first_name base) in
  let first_name_aliases =
    box_list @@
    List.map box_string (E.first_name_aliases base p)
  in
  let first_name_key = get_str (E.first_name_key base) in
  let first_name_key_val = get_str (E.first_name_key_val base) in
  let iper = Tstr (Gwdb.string_of_iper iper') in
  let is_birthday = get_bool (E.is_birthday conf) in
  let is_visible_for_visitors =
    box_lazy @@
    lazy (Tbool (Util.authorized_age {conf with wizard = false; friend = false} base p))
  in
  let linked_page =
    box_lazy @@
    lazy (let fn = E.linked_page conf base p in Tpat (fun s -> Tstr (fn s) ) )
  in
  let mother = mk_parent Gwdb.get_mother in
  let titles = lazy_list (mk_title base) (E.titles p) in
  let occ = get_int E.occ in
  let occupation = get_str (E.occupation conf base) in
  let public_name = get_str (E.public_name base) in
  let qualifier = get_str (E.qualifier base) in
  let qualifiers =
    Tlist (List.map box_string @@ E.qualifiers base p)
  in
  let related =
    match E.rparents p with
    | [] -> Tlist []
    | r -> box_list @@ List.fold_left (mk_related conf base) [] r
  in
  let relations = lazy_list (get_n_mk_person conf base) (E.relations p) in
  let sex = get_int E.sex in
  let siblings_aux fn = lazy_list (get_n_mk_person conf base) (fn base p) in
  let siblings = siblings_aux E.siblings in
  let half_siblings = siblings_aux E.half_siblings in
  let sources = get_str @@ E.psources base in
  let str__ =
    box_lazy @@
    lazy (get_str (Util.person_text conf base)) (* FIXME *)
  in
  let surname_key_val = get_str (E.surname_key_val base) in
  let surname = get_str (E.surname base) in
  let surname_aliases = Tlist (List.map box_string (E.surname_aliases base p) ) in
  let surname_key = get_str (E.surname_key base) in
  let sosa = box_lazy @@ lazy begin get_sosa_person conf base p end in
  Tpat
    (function
      | "access" -> access
      | "baptism_date" -> baptism_date
      | "baptism_place" -> baptism_place
      | "birth_date" -> birth_date
      | "birth_place" -> birth_place
      | "burial" -> burial
      | "burial_place" -> burial_place
      | "children" -> children
      | "cremation_place" -> cremation_place
      | "consanguinity" -> consanguinity
      | "dates" -> dates
      | "death" -> death
      | "death_place" -> death_place
      | "digest" -> digest
      | "events" -> events
      | "families" -> families
      | "father" -> father
      | "first_name" -> first_name
      | "first_name_aliases" -> first_name_aliases
      | "first_name_key" -> first_name_key
      | "first_name_key_val" -> first_name_key_val
      | "half_siblings" -> half_siblings
      | "iper" -> iper
      | "is_birthday" -> is_birthday
      | "is_visible_for_visitors" -> is_visible_for_visitors
      | "linked_page" -> linked_page
      | "mother" -> mother
      | "occ" -> occ
      | "occupation" -> occupation
      | "parents" -> parents
      | "public_name" -> public_name
      | "qualifier" -> qualifier
      | "qualifiers" -> qualifiers
      | "relations" -> relations
      | "related" -> related
      | "sex" -> sex
      | "siblings" -> siblings
      | "sosa" -> sosa
      | "sources" -> sources
      | "spouses" -> spouses
      | "surname" -> surname
      | "surname_aliases" -> surname_aliases
      | "surname_key" -> surname_key
      | "surname_key_val" -> surname_key_val
      | "titles" -> titles
      | "__str__" -> str__
      | _ -> raise Not_found
    )

and mk_dmy { Def.day ; month ; year ; delta ; prec } =
  let day = Tint day in
  let month = Tint month in
  let year = Tint year in
  let delta = Tint delta in
  let prec = to_prec prec in
  Tpat (function
      | "day" -> day
      | "month" -> month
      | "year" -> year
      | "delta" -> delta
      | "prec" -> prec
      | _ -> raise Not_found
    )

and mk_fevent ?spouse conf base e =
  mk_event conf base
    ( Fevent e.Def.efam_name
    , e.efam_date
    , e.efam_place
    , e.efam_note
    , e.efam_src
    , e.efam_witnesses
    , spouse)

and mk_pevent conf base e =
  mk_event conf base
    ( Pevent e.Def.epers_name
    , e.epers_date
    , e.epers_place
    , e.epers_note
    , e.epers_src
    , e.epers_witnesses
    , None)

and mk_gen_title base t =
  Tpat (function "t_ident" -> Tstr (Gwdb.sou base t.Def.t_ident)
               | "t_place" -> Tstr (Gwdb.sou base t.t_place)
               | "t_date_start" ->
                 begin match Adef.od_of_cdate t.t_date_start with
                   | Some d -> mk_date d
                   | None -> Tnull
                 end
               | "t_date_end" ->
                 begin match Adef.od_of_cdate t.t_date_end with
                   | Some d -> mk_date d
                   | None -> Tnull
                 end
               | _ -> raise Not_found)

and mk_death_reason = function
  | Def.Killed -> Tstr "Killed"
  | Murdered -> Tstr "Murdered"
  | Executed -> Tstr "Executed"
  | Disappeared -> Tstr "Disappeared"
  | Unspecified -> Tstr "Unspecified"

and mk_death =
  let wrap s = Tpat (function "death_reason" -> Tstr s | _ -> raise Not_found) in
  function
  | Def.NotDead -> Tnull
  | Death (r, cd) ->
    let death_reason = mk_death_reason r in
    let date = mk_date (Adef.date_of_cdate cd) in
    Tpat (function "death_reason" -> death_reason
                 | "date" -> date
                 | _ -> raise Not_found)
  | DeadYoung -> wrap "DeadYoung"
  | DeadDontKnowWhen -> wrap "DeadDontKnowWhen"
  | DontKnowIfDead -> wrap "DontKnowIfDead"
  | OfCourseDead -> wrap "OfCourseDead"

and mk_burial = function
  | Def.UnknownBurial -> Tnull
  | Buried d ->
    let type_ = Tstr "Buried" in
    let date = match Adef.od_of_cdate d with
      | Some d -> mk_date d
      | None -> Tnull
    in
    Tpat (function "type" -> type_
                 | "date" -> date
                 | _ -> raise Not_found)
  | Cremated d ->
    let type_ = Tstr "Cremated" in
    let date = match Adef.od_of_cdate d with
      | Some d -> mk_date d
      | None -> Tnull
    in
    Tpat (function "type" -> type_
                 | "date" -> date
                 | _ -> raise Not_found)

(* take optionnal p parameter for spouse things? *)
and mk_warning conf base =
  let get_fam ifam =
    let cpl = Gwdb.foi base ifam in
    let ifath = Gwdb.get_father cpl in
    let imoth = Gwdb.get_mother cpl in
    (* spouse if not used so it should be okay *)
    mk_family conf base (ifam, cpl, (ifath, imoth, imoth), true)
  in
  let array_of_list_map : 'a 'b . ('a -> 'b) -> 'a list -> 'b array =
    fun fn l ->
      if l = [] then [||] else begin
        let a = Array.make (List.length l) (fn @@ List.hd l) in (* FIXME *)
        List.iteri (fun i x -> a.(i) <- fn x) l ;
        a
      end
  in
  function
  | Def.BigAgeBetweenSpouses (f, m, a) ->
    Tset [ Tstr "BigAgeBetweenSpouses"
         ; unsafe_mk_person conf base f
         ; unsafe_mk_person conf base m
         ; mk_date (Dgreg (a, Dgregorian) ) ]
  | BirthAfterDeath p ->
    Tset [ Tstr "BirthAfterDeath" ; unsafe_mk_person conf base p]
  | IncoherentSex (p, i1, i2) ->
    Tset [ Tstr "BirthAfterDeath"
         ; unsafe_mk_person conf base p
         ; Tint i1
         ; Tint i2 ]
  | ChangedOrderOfChildren (ifam, _descend, before, after) ->
    let (bef_d, aft_d) = Difference.f before after in
    Tset [ Tstr "ChangedOrderOfChildren"
         ; get_fam ifam
         ; Tarray (Array.map (get_n_mk_person conf base) before)
         ; Tarray (Array.map (get_n_mk_person conf base) after)
         ; Tarray (Array.map box_bool bef_d)
         ; Tarray (Array.map box_bool aft_d)
         ]
  | ChangedOrderOfMarriages (p, before, after) ->
    let (bef_d, aft_d) = Difference.f before after in
    Tset [ Tstr "ChangedOrderOfMarriages"
         ; unsafe_mk_person conf base p
         ; Tarray (Array.map get_fam before)
         ; Tarray (Array.map get_fam after)
         ; Tarray (Array.map box_bool bef_d)
         ; Tarray (Array.map box_bool aft_d)
         ]
  | ChangedOrderOfFamilyEvents (_ifam, before, after) ->
    let before = array_of_list_map (mk_fevent conf base) before in
    let after = array_of_list_map (mk_fevent conf base) after in
    let (bef_d, aft_d) = Difference.f before after in
    Tset [ Tstr "ChangedOrderOfFamilyEvents"
         ; Tarray before
         ; Tarray after
         ; Tarray (Array.map box_bool bef_d)
         ; Tarray (Array.map box_bool aft_d)
         ]
  | ChangedOrderOfPersonEvents (_p, before, after) ->
    let before = array_of_list_map (mk_pevent conf base) before in
    let after = array_of_list_map (mk_pevent conf base) after in
    let (bef_d, aft_d) = Difference.f before after in
    Tset [ Tstr "ChangedOrderOfPersonEvents"
         ; Tarray before
         ; Tarray after
         ; Tarray (Array.map box_bool bef_d)
         ; Tarray (Array.map box_bool aft_d)
         ]
  | ChildrenNotInOrder (ifam, _descend, elder, x) ->
    Tset [ Tstr "ChildrenNotInOrder"
         ; get_fam ifam
         ; unsafe_mk_person conf base elder
         ; unsafe_mk_person conf base x
         ]
  | CloseChildren (ifam, c1, c2) ->
    Tset [ Tstr "CloseChildren"
         ; get_fam ifam
         ; unsafe_mk_person conf base c1
         ; unsafe_mk_person conf base c2
         ]
  | DistantChildren (ifam, c1, c2) ->
    Tset [ Tstr "DistantChildren"
         ; get_fam ifam
         ; unsafe_mk_person conf base c1
         ; unsafe_mk_person conf base c2
         ]
  | DeadOld (p, a) ->
    Tset [ Tstr "DeadOld"
         ; unsafe_mk_person conf base p
         ; mk_date (Dgreg (a, Dgregorian) ) ]
  | DeadTooEarlyToBeFather (father, child) ->
    Tset [ Tstr "DeadTooEarlyToBeFather"
         ; unsafe_mk_person conf base father
         ; unsafe_mk_person conf base child ]
  | FEventOrder (p, e1, e2) ->
    Tset [ Tstr "FEventOrder"
         ; unsafe_mk_person conf base p
         ; mk_fevent conf base e1
         ; mk_fevent conf base e2 ]
  | FWitnessEventAfterDeath (p, e) ->
    Tset [ Tstr "FWitnessEventAfterDeath"
         ; unsafe_mk_person conf base p
         ; mk_fevent conf base e ]
  | FWitnessEventBeforeBirth (p, e) ->
    Tset [ Tstr "FWitnessEventBeforeBirth"
         ; unsafe_mk_person conf base p
         ; mk_fevent conf base e ]
  | IncoherentAncestorDate (p1, p2) ->
    Tset [ Tstr "IncoherentAncestorDate"
         ; unsafe_mk_person conf base p1
         ; unsafe_mk_person conf base p2 ]
  | MarriageDateAfterDeath p ->
    Tset [ Tstr "MarriageDateAfterDeath"
         ; unsafe_mk_person conf base p ]
  | MarriageDateBeforeBirth p ->
    Tset [ Tstr "MarriageDateBeforeBirth"
         ; unsafe_mk_person conf base p ]
  | MotherDeadAfterChildBirth (p1, p2) ->
    Tset [ Tstr "MotherDeadAfterChildBirth"
         ; unsafe_mk_person conf base p1
         ; unsafe_mk_person conf base p2 ]
  | OldForMarriage (p, a) ->
    Tset [ Tstr "OldForMarriage"
         ; unsafe_mk_person conf base p
         ; mk_date (Dgreg (a, Dgregorian) ) ]
  | ParentBornAfterChild (p1, p2) ->
    Tset [ Tstr "ParentBornAfterChild"
         ; unsafe_mk_person conf base p1
         ; unsafe_mk_person conf base p2 ]
  | ParentTooOld (p, a) ->
    Tset [ Tstr "ParentTooOld"
         ; unsafe_mk_person conf base p
         ; mk_date (Dgreg (a, Dgregorian) ) ]
  | ParentTooYoung (p, a) ->
    Tset [ Tstr "ParentTooYoung"
         ; unsafe_mk_person conf base p
         ; mk_date (Dgreg (a, Dgregorian) ) ]
  | PEventOrder (p, e1, e2) ->
    Tset [ Tstr "PEventOrder"
         ; unsafe_mk_person conf base p
         ; mk_pevent conf base e1
         ; mk_pevent conf base e2 ]
  | PWitnessEventAfterDeath (p, e) ->
    Tset [ Tstr "PWitnessEventAfterDeath"
         ; unsafe_mk_person conf base p
         ; mk_pevent conf base e ]
  | PWitnessEventBeforeBirth (p, e) ->
    Tset [ Tstr "PWitnessEventBeforeBirth"
         ; unsafe_mk_person conf base p
         ; mk_pevent conf base e ]
  | TitleDatesError (p, t) ->
    Tset [ Tstr "PWitnessEventBeforeBirth"
         ; unsafe_mk_person conf base p
         ; mk_gen_title base t ]
  | UndefinedSex p ->
    Tset [ Tstr "UndefinedSex"
         ; unsafe_mk_person conf base p ]
  | WitnessDateAfterDeath p ->
    Tset [ Tstr "WitnessDateAfterDeath"
         ; unsafe_mk_person conf base p ]
  | WitnessDateBeforeBirth p ->
    Tset [ Tstr "WitnessDateBeforeBirth"
         ; unsafe_mk_person conf base p ]
  | YoungForMarriage (p, a) ->
    Tset [ Tstr "YoungForMarriage"
         ; unsafe_mk_person conf base p
         ; mk_date (Dgreg (a, Dgregorian) ) ]
  | PossibleDuplicateFam (ifam1, ifam2) ->
    Tset [ Tstr "PossibleDuplicateFam"
         ; get_fam ifam1
         ; get_fam ifam2
         ]

let module_OPT =
  let map = func_arg2_no_kw begin fun fn -> function
      | Tnull -> Tnull
      | x -> (unbox_fun fn) x
    end
  in
  Tpat begin function
    | "map" -> map
    | _ -> raise Not_found
  end

let module_NAME base =
  let get_particle s = Mutil.get_particle (Gwdb.base_particles base) s in
  let particle =
    func_arg1_no_kw begin function
      | Tstr s -> begin match get_particle s with
          | "" -> Tnull
          | s -> Tstr (String.trim s)
        end
      | _ -> assert false
    end
  in
  let without_particle =
    func_arg1_no_kw begin function
      | Tstr s -> begin match get_particle s with
          | "" -> Tstr s
          | part ->
            let l = String.length part in
            let s = String.sub s l (String.length s - l) in
            Tstr s
        end
      | _ -> assert false
    end
  in
  Tpat begin function
    | "particle" -> particle
    | "without_particle" -> without_particle
    | _ -> raise Not_found
  end

let mk_conf conf =
  let lazy_env e = Tlazy (lazy (Tobj (List.map (fun (k, v) -> (k, Tstr v)) e))) in
  let wizard = Tvolatile (fun () -> Tbool conf.Config.wizard) in
  let friend = Tbool conf.friend in
  let command = Tstr conf.command in
  let env = lazy_env conf.env in
  let senv = lazy_env conf.senv in
  let henv = lazy_env conf.henv in
  let benv = lazy_env conf.base_env in
  let today = mk_dmy conf.today in
  let image_prefix = Tstr conf.image_prefix in
  let user = Tstr conf.user in
  let lang = Tstr conf.lang in
  Tpat begin function
    | "benv" -> benv
    | "command" -> command
    | "env" -> env
    | "friend" -> friend
    | "henv" -> henv
    | "image_prefix" -> image_prefix
    | "lang" -> lang
    | "senv" -> senv
    | "today" -> today
    | "user" -> user
    | "wizard" -> wizard
    | _ -> raise Not_found
  end

let mk_env conf base =
  (* FIXME browsing_with_sosa_ref *)
  let prefix = Tstr (Util.commd conf) in
  let prefix_base = Tstr (Util.prefix_base conf) in
  let sosa_ref =
    box_lazy @@
    lazy (mk_opt (unsafe_mk_person conf base) (Util.find_sosa_ref conf base) )
  in
  Tpat begin function
      | "prefix" -> prefix
      | "prefix_base" -> prefix_base
      | "sosa_ref" -> sosa_ref
      | x -> Tstr (Wserver.decode @@ List.assoc x conf.env)
  end

let decode_varenv =
  func_arg1_no_kw @@ fun str ->
  try Tstr (Wserver.decode @@ unbox_string str)
  with _ -> failwith_type_error_1 "decode_varenv" str

let encode_varenv =
  func_arg1_no_kw @@ fun str ->
  try Tstr (Wserver.encode @@ unbox_string str)
  with _ -> failwith_type_error_1 "encode_varenv" str

let mk_base base =
  Tpat begin function
    | "nb_of_persons" -> Tint (Gwdb.nb_of_persons base)
    | "nb_of_families" -> Tint (Gwdb.nb_of_families base)
    | "name" -> Tstr (Gwdb.bname base)
    | _ -> raise Not_found
  end

let stringify s =
  Printf.sprintf (if String.contains s '\'' then "\"%s\"" else "'%s'") s

let twigify filter =
  func_arg1_no_kw @@ function
  | Tstr s ->
    let s =
      let len = String.length s in
      if len = 0 then ""
      else if String.get s 0 = '{' && String.get s (len - 1) = '}'
      then String.sub s 2 (len - 4)
      else stringify s
    in
    Tstr (Printf.sprintf "{{%s|%s}}" s filter)
  | x -> failwith_type_error_1 filter x

let twig = Tpat (fun s -> twigify s)

let trans (conf : Config.config) =
  let trad ~kwargs s i =
    try
      let s = Hashtbl.find conf.lexicon s in
      let t =
        if Lexicon_parser.need_split s
        then Array.of_list @@ String.split_on_char '/' s
        else [| s |]
      in
      let t =
        Array.map (fun t -> Lexicon_parser.p_trad (Buffer.create 128) [] @@ Lexing.from_string t) t
      in
      let i = if i < 0 || i >= Array.length t then 0 else i in
      let arg s = List.assoc s kwargs in
      let t = Array.unsafe_get t i in
      Tstr begin
        let conv acc = function
          | Lexicon_parser.Str s -> s
          | Arg n -> Jg_runtime.string_of_tvalue (arg n)
          | Elision (s1, s2) ->
            let x = try unbox_string @@ arg "elision" with Not_found -> acc in
            if x <> ""
            && Unidecode.decode
                 (fun _ _ -> false)
                 (fun _ -> function
                    | 'A'|'E'|'I'|'O'|'U'|'a'|'e'|'i'|'o'|'u' -> true
                    | _ -> false)
                 (fun _ -> false)
                 x 0 (String.length x)
            then s2
            else s1
        in
        let rec loop s i =
          if i < 0
          then s
          else loop (conv s (Array.unsafe_get t i) ^ s) (i - 1)
        in
        let len = Array.length t in
        loop (conv "" @@ Array.unsafe_get t @@ len - 1) (len - 2)
        |> Util.translate_eval
      end
    with Not_found -> Tstr (Printf.sprintf "{{%s|trans}}" @@ stringify @@ s)
  in
  Tfun begin fun ?(kwargs=[]) -> function
    | Tint i ->
      let kw = kwargs in
      Tfun begin fun ?(kwargs=[]) s ->
        let kwargs = List.rev_append kwargs kw in
        trad ~kwargs (unbox_string s) i end
    | Tstr s -> trad ~kwargs s 0
    | x -> Jingoo.Jg_types.failwith_type_error_1 "trans" x
  end

let get_person conf base = func_arg1_no_kw @@ function
  | Tint i -> get_n_mk_person conf base (Gwdb.iper_of_string @@ string_of_int i)
  | Tstr i -> get_n_mk_person conf base (Gwdb.iper_of_string i)
  | x -> failwith_type_error_1 "GET_PERSON" x

(* copy/paste from Yojson adapted to Jingoo *)
module Yojson_write = struct

  let rec hex n =
    Char.chr (if n < 10 then n + 48 else n + 87)

  and write_special src start stop ob str =
    Buffer.add_substring ob src !start (stop - !start);
    Buffer.add_string ob str;
    start := stop + 1

  and write_control_char src start stop ob c =
    Buffer.add_substring ob src !start (stop - !start);
    Buffer.add_string ob "\\u00" ;
    Buffer.add_char ob (hex (Char.code c lsr 4)) ;
    Buffer.add_char ob (hex (Char.code c land 0xf));
    start := stop + 1

  and finish_string src start ob =
    Buffer.add_substring ob src !start (String.length src - !start)

  and write_string_body ob s =
    let start = ref 0 in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        '"' -> write_special s start i ob "\\\""
      | '\\' -> write_special s start i ob "\\\\"
      | '\b' -> write_special s start i ob "\\b"
      | '\012' -> write_special s start i ob "\\f"
      | '\n' -> write_special s start i ob "\\n"
      | '\r' -> write_special s start i ob "\\r"
      | '\t' -> write_special s start i ob "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c -> write_control_char s start i ob c
      | _ -> ()
    done;
    finish_string s start ob

  and write_string ob s =
    Buffer.add_char ob '"';
    write_string_body ob s;
    Buffer.add_char ob '"'

  and dec n =
    Char.chr (n + 48)

  and write_digits ob x =
    if x = 0 then ()
    else
      let d = x mod 10 in
      write_digits ob (x / 10) ;
      Buffer.add_char ob (dec (abs d))

  and write_int ob x =
    if x > 0 then write_digits ob x
    else if x < 0 then begin
      Buffer.add_char ob '-' ;
      write_digits ob x
    end
    else
      Buffer.add_char ob '0'

  and write_float ob x =
    Buffer.add_string ob @@ Printf.sprintf "%.17g" x

  and write_null ob () =
    Buffer.add_string ob "null"

  and write_bool ob x =
    Buffer.add_string ob (if x then "true" else "false")

  and write_kv ob (k, v) = write_string ob k ; Buffer.add_char ob ':' ; write_json ob v

  and write_list_aux : 'a . (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a list -> unit =
    fun fn ob x ->
      let rec loop = function
        | [] -> ()
        | [ x ] -> fn ob x
        | hd :: tl -> fn ob hd ; Buffer.add_char ob ',' ; loop tl
      in
      loop x

  and write_assoc ob x =
    Buffer.add_char ob '{' ;
    write_list_aux write_kv ob x ;
    Buffer.add_char ob '}'

  and write_hash ob x =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) x []
    |> write_assoc ob

  and write_list ob x =
    Buffer.add_char ob '[' ;
    write_list_aux write_json ob x ;
    Buffer.add_char ob ']'

  and write_array ob x =
    write_list ob (Array.to_list x)

  and write_json ob = function
    | Tint i -> write_int ob i
    | Tfloat f -> write_float ob f
    | Tnull -> write_null ob ()
    | Tbool b -> write_bool ob b
    | Tstr s -> write_string ob s
    | Tobj assoc -> write_assoc ob assoc
    | Thash hash -> write_hash ob hash
    | Tlist list | Tset list -> write_list ob list
    | Tarray array -> write_array ob array
    | x -> failwith_type_error_1 "write_json" x

end

let json_encode o =
  let open Yojson_write in
  let ob = Buffer.create 64 in
  write_json ob o ;
  Buffer.contents ob

let log = func_arg1_no_kw @@ fun x -> print_endline @@ Jg_runtime.string_of_tvalue x ; Tnull

let alphabetic =
  func_arg2_no_kw @@ fun a b ->
  let str = function
    | Tstr b -> b
    | Tnull -> ""
    | _ -> failwith_type_error_2 "alphabetic" a b
  in
  Tint (Utf8.compare (str a) (str b) )

let module_CAST =
  let string =
    func_arg1_no_kw @@ function
    | Tstr _ as s -> s
    | Tnull -> Tstr ""
    | Tint i -> Tstr (string_of_int i)
    | Tfloat f -> Tstr (string_of_float f)
    | Tbool b -> Tstr (string_of_bool b)
    | x -> failwith_type_error_1 "CAST.string" x
  in
  let int =
    func_arg1_no_kw @@ function
    | Tint _ as i -> i
    | Tnull -> Tint 0
    | Tstr i -> Tint (int_of_string i)
    | Tfloat f -> Tint (int_of_float f)
    | Tbool true -> Tint 1
    | Tbool false -> Tint 0
    | x -> failwith_type_error_1 "CAST.int" x
  in
  let object_ =
    func_arg1_no_kw @@ function
    | Tobj _ as o -> o
    | Tnull -> Tobj []
    | Tlist x ->
      Tobj (List.map (function Tlist [ Tstr k ; v ] | Tset [ Tstr k ; v ] -> (k,  v)
                             | x -> failwith_type_error_1 "CAST.object(list)" x) x)
    | x -> failwith_type_error_1 "CAST.object" x
  in
  Tpat begin function
    | "int" -> int
    | "object" -> object_
    | "string" -> string
    | _ -> raise Not_found
  end

let default_env conf base (* p *) =
  let conf_env = mk_conf conf in
  let module_NAME = module_NAME base in
  ("trans", trans conf)
  :: ("DIGEST", func_arg1_no_kw (function Tstr x -> Tstr (String.sub (Digest.to_hex @@ Digest.string x) 0 6) | _ -> assert false))
  :: ("DATE", module_date conf)
  :: ("OPT", module_OPT)
  :: ("NAME", module_NAME)
  :: ("GET_PERSON", get_person conf base)
  :: ("env", mk_env conf base)
  :: ("decode_varenv", decode_varenv)
  :: ("encode_varenv", encode_varenv)
  :: ("alphabetic", alphabetic)
  :: ("json_encode", func_arg1_no_kw (fun x -> Tstr (json_encode x) ))
  :: ("base", mk_base base)
  :: ("conf", conf_env)
  :: ("flatten", func_arg1_no_kw (function
      | Tlist x ->
        Tlist
          (List.fold_right (function Tlist list -> fun acc -> list @ acc
                                   | x -> fun acc ->  x :: acc) x [])
      | Tarray x ->
        Tlist
          (Array.fold_right (function Tlist list -> fun acc -> list @ acc
                                    | x -> fun acc ->  x :: acc) x [])
      | _ -> print_endline __LOC__ ; Tlist []) )
  :: ("TWIG", twig)
  :: ("LOG", log)
  :: ("CAST", module_CAST)
  :: []

let sandbox (conf : Config.config) base =
  let aux ifam =
    let cpl = Gwdb.foi base ifam in
    get_n_mk_family conf base ifam cpl
  in
  let get_family = func_arg1_no_kw @@ function
    | Tint i -> aux (Gwdb.ifam_of_string @@ string_of_int i)
    | Tstr i -> aux (Gwdb.ifam_of_string i)
    | x -> failwith_type_error_1 "GET_FAMILY" x
  in
  let () = Random.self_init () in
  ("GET_FAMILY", get_family)
  :: ("RANDOM_IPER", Tvolatile (fun () -> Tint (Random.int (Gwdb.nb_of_persons base))))
  :: ("RANDOM_IFAM", Tvolatile (fun () -> Tint (Random.int (Gwdb.nb_of_families base))))
  :: default_env conf base
