#ifdef API

module MLink = Api_link_tree_piqi
module MLinkext = Api_link_tree_piqi_ext


open Config
open Def

(**/**)


(* ************************************************************************ *)
(*  [Fonc] date_of_piqi_date : piqi_date -> option def.date                 *)
(** [Description] : Converti date piqi en date
    [Args] :
      - date : date du module Mwrite
    [Retour] :
      - date : date
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let date_of_piqi_date date =
  match date.MLink.Date.text with
    Some txt -> Some (Dtext txt)
  | _ ->
      (* Si on a une année, on a une date. *)
      match date.MLink.Date.dmy with
        Some dmy ->
          begin match dmy.MLink.Dmy.year with
            Some _ ->
              let cal =
                match date.MLink.Date.cal with
                  Some `julian -> Djulian
                | Some `french -> Dfrench
                | Some `hebrew -> Dhebrew
                | _ -> Dgregorian
              in
              let prec =
                match date.MLink.Date.prec with
                  Some `about -> About
                | Some `maybe -> Maybe
                | Some `before -> Before
                | Some `after -> After
                | Some `oryear ->
                    begin match date.MLink.Date.dmy2 with
                      Some dmy ->
                        begin match dmy.MLink.Dmy.year with
                          Some _ ->
                            let d =
                              match dmy.MLink.Dmy.day with
                                Some day -> Int32.to_int day
                              | None -> 0
                            in
                            let m =
                              match dmy.MLink.Dmy.month with
                                Some month -> Int32.to_int month
                              | None -> 0
                            in
                            let y =
                              match dmy.MLink.Dmy.year with
                                Some year -> Int32.to_int year
                              | None -> 0
                            in
                            (* gestion des erreurs. *)
                            let (d, m, y) =
                              match dmy.MLink.Dmy.year with
                                Some _ -> if m <= 0 then 0, 0, y else d, m, y
                              | None -> 0, 0, 0
                            in
                            let dmy2 =
                              {day2 = d; month2 = m; year2 = y; delta2 = 0}
                            in
                            OrYear dmy2
                        | None -> Sure
                        end
                    | None -> Sure
                    end
                | Some `yearint ->
                    begin match date.MLink.Date.dmy2 with
                      Some dmy ->
                        begin match dmy.MLink.Dmy.year with
                          Some _ ->
                            let d =
                              match dmy.MLink.Dmy.day with
                                Some day -> Int32.to_int day
                              | None -> 0
                            in
                            let m =
                              match dmy.MLink.Dmy.month with
                                Some month -> Int32.to_int month
                              | None -> 0
                            in
                            let y =
                              match dmy.MLink.Dmy.year with
                                Some year -> Int32.to_int year
                              | None -> 0
                            in
                            (* gestion des erreurs. *)
                            let (d, m, y) =
                              match dmy.MLink.Dmy.year with
                                Some _ -> if m <= 0 then 0, 0, y else d, m, y
                              | None -> 0, 0, 0
                            in
                            let dmy2 =
                              {day2 = d; month2 = m; year2 = y; delta2 = 0}
                            in
                            YearInt dmy2
                        | None -> Sure
                        end
                    | None -> Sure
                    end
                | _ -> Sure
              in
              let dmy =
                match date.MLink.Date.dmy with
                  Some dmy ->
                    let day =
                      match dmy.MLink.Dmy.day with
                        Some day -> Int32.to_int day
                      | None -> 0
                    in
                    let month =
                      match dmy.MLink.Dmy.month with
                        Some month -> Int32.to_int month
                      | None -> 0
                    in
                    let year =
                      match dmy.MLink.Dmy.year with
                        Some year -> Int32.to_int year
                      | None -> 0
                    in
                    let delta =
                      match dmy.MLink.Dmy.delta with
                        Some delta -> Int32.to_int delta
                      | None -> 0
                    in
                    (* gestion des erreurs. *)
                    let (day, month, year) =
                      match dmy.MLink.Dmy.year with
                        Some _ ->
                          if month <= 0 then 0, 0, year else day, month, year
                      | None -> 0, 0, 0
                    in
                    {day = day; month = month; year = year; prec = prec;
                     delta = delta}
                | None ->
                    (* erreur*)
                    {day = 0; month = 0; year = 0; prec = Sure; delta = 0}
              in
              let dmy =
                match cal with
                  Dgregorian -> dmy
                | Djulian -> Calendar.gregorian_of_julian dmy
                | Dfrench -> Calendar.gregorian_of_french dmy
                | Dhebrew -> Calendar.gregorian_of_hebrew dmy
              in
              Some (Dgreg (dmy, cal))
          | None -> None
          end
      | None -> None

let make_ep_link base p_link =
  let empty_union = {family = [| |]} in
  let empty_ascend = {parents = None; consang = Adef.fix (-1)} in
  let empty_p = Gwdb.empty_person base (Gwdb.dummy_iper) in
  let empty_string = Gwdb.insert_string base "" in
  let gen_p = Gwdb.gen_person_of_person empty_p in
  let surname = Gwdb.insert_string base p_link.MLink.Person.lastname in
  let first_name = Gwdb.insert_string base p_link.MLink.Person.firstname in
  let occ = Int32.to_int p_link.MLink.Person.oc in
  let key_index = Gwdb.iper_of_string p_link.MLink.Person.ip in
  let image =
    match p_link.MLink.Person.image with
      Some s -> Gwdb.insert_string base s
    | None -> empty_string
  in
  let occupation =
    match p_link.MLink.Person.occupation with
      Some s -> Gwdb.insert_string base s
    | None -> empty_string
  in
  let public_name =
    match p_link.MLink.Person.public_name with
      Some s -> Gwdb.insert_string base s
    | None -> empty_string
  in
  let qualifiers =
    List.map (Gwdb.insert_string base) p_link.MLink.Person.qualifiers
  in
  let aliases =
    List.map (Gwdb.insert_string base) p_link.MLink.Person.aliases
  in
  (* TODO *)
  let titles = [] in
  let sex =
    match p_link.MLink.Person.sex with
      `male -> Male
    | `female -> Female
    | `unknown -> Neuter
  in
  let birth =
    match p_link.MLink.Person.birth_date with
      Some d -> Adef.cdate_of_od (date_of_piqi_date d)
    | None -> Adef.cdate_None
  in
  let birth_place =
    match p_link.MLink.Person.birth_place with
      Some s -> Gwdb.insert_string base s
    | None -> empty_string
  in
  let baptism =
    match p_link.MLink.Person.baptism_date with
      Some d -> Adef.cdate_of_od (date_of_piqi_date d)
    | None -> Adef.cdate_None
  in
  let baptism_place =
    match p_link.MLink.Person.baptism_place with
      Some s -> Gwdb.insert_string base s
    | None -> empty_string
  in
  let death =
    match p_link.MLink.Person.death_type with
      `not_dead -> NotDead
    | `dead ->
        begin match p_link.MLink.Person.death_date with
          Some date ->
            begin match date_of_piqi_date date with
              Some date -> Death (Unspecified, Adef.cdate_of_date date)
            | None -> DeadDontKnowWhen
            end
        | None -> DeadDontKnowWhen
        end
    | `dead_young -> DeadYoung
    | `dead_dont_know_when -> DeadDontKnowWhen
    | `dont_know_if_dead -> DontKnowIfDead
    | `of_course_dead -> OfCourseDead
  in
  let death_place =
    match p_link.MLink.Person.death_place with
      Some s -> Gwdb.insert_string base s
    | None -> empty_string
  in
  let burial =
    match p_link.MLink.Person.baptism_date with
      Some d -> Buried (Adef.cdate_of_od (date_of_piqi_date d))
    | None -> UnknownBurial
  in
  let burial_place =
    match p_link.MLink.Person.burial_place with
      Some s -> Gwdb.insert_string base s
    | None -> empty_string
  in
  let gen_p =
    {gen_p with access = IfTitles; first_name = first_name; surname = surname;
     key_index = key_index; occ = occ; image = image; occupation = occupation;
     public_name = public_name; qualifiers = qualifiers; titles = titles;
     aliases = aliases; sex = sex; birth = birth; birth_place = birth_place;
     baptism = baptism; baptism_place = baptism_place; death = death;
     death_place = death_place; burial = burial; burial_place = burial_place}
  in
  let p = Gwdb.person_of_gen_person base (gen_p, empty_ascend, empty_union) in
  p, true

let make_efam_link conf base fam_link =
  let empty_string = Gwdb.insert_string base "" in
  let children =
    List.map (fun p -> Gwdb.iper_of_string p.MLink.Person_link.ip)
      fam_link.MLink.Family.children
  in
  let des = {children = Array.of_list children} in
  let ifath = Gwdb.iper_of_string fam_link.MLink.Family.ifath in
  let imoth = Gwdb.iper_of_string fam_link.MLink.Family.imoth in
  let cpl = Futil.parent conf.multi_parents [| ifath; imoth |] in
  let marriage =
    match fam_link.MLink.Family.marriage_date with
      Some d -> Adef.cdate_of_od (date_of_piqi_date d)
    | None -> Adef.cdate_None
  in
  let marriage_place =
    match fam_link.MLink.Family.marriage_place with
      Some s -> Gwdb.insert_string base s
    | None -> empty_string
  in
  let relation =
    match fam_link.MLink.Family.marriage_type with
      `married -> Married
    | `not_married -> NotMarried
    | `engaged -> Engaged
    | `no_sexes_check_not_married -> NoSexesCheckNotMarried
    | `no_mention -> NoMention
    | `no_sexes_check_married -> NoSexesCheckMarried
    | `marriage_bann -> MarriageBann
    | `marriage_contract -> MarriageContract
    | `marriage_license -> MarriageLicense
    | `pacs -> Pacs
    | `residence -> Residence
  in
  let divorce =
    match fam_link.MLink.Family.divorce_type with
      `not_divorced -> NotDivorced
    | `divorced ->
        begin match fam_link.MLink.Family.divorce_date with
          Some d -> Divorced (Adef.cdate_of_od (date_of_piqi_date d))
        | None -> Divorced Adef.cdate_None
        end
    | `separated -> Separated
  in
  let index = Gwdb.ifam_of_string fam_link.MLink.Family.ifam in
  let gen_f =
    {marriage = marriage; marriage_place = marriage_place;
     marriage_note = empty_string; marriage_src = empty_string;
     witnesses = [| |]; relation = relation; divorce = divorce; fevents = [];
     comment = empty_string; origin_file = empty_string;
     fsources = empty_string; fam_index = index}
  in
  let fam = Gwdb.family_of_gen_family base (gen_f, cpl, des) in
  index, fam, cpl, true


(**/**)


(* ************************************************************************** *)
(*  [Fonc] get_person_link_with_base :
             string -> iper -> string -> Person.t option                 *)
(** [Description] : Retourne la personne distante que l'on cherche. On
      commence par chercher si elle est dans le cache avec le nom de la base
      actuelle. Si elle n'y est pas, alors on récupère toutes les
      correspondances, puis on vérifie que la personne que l'on souhaite a la
      bonne base distante.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne
      - base_distante : la base sur laquelle on a trouvé la personne
    [Retour] : Person.t option
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_person_link_with_base base_prefix ip base_distante =
  let base_prefix = Link.chop_base_prefix base_prefix in
  let base_distante = Link.chop_base_prefix base_distante in
  try Some (Hashtbl.find Link.ht_person_cache (base_prefix, ip)) with
    Not_found ->
      try
        let rec loop l =
          match l with
            [] -> raise Not_found
          | (base_prefix, ip) :: l ->
              if base_prefix = base_distante then base_prefix, ip else loop l
        in
        let (base_prefix, ip) =
          loop (Hashtbl.find_all Link.ht_corresp (base_prefix, ip))
        in
        Some (Hashtbl.find Link.ht_person_cache (base_prefix, ip))
      with Not_found -> None


(* ************************************************************************** *)
(*  [Fonc] get_person_link : string -> iper -> Person.t option           *)
(** [Description] : Retourne la personne distante que l'on cherche par rapport
      au nom de la base actuelle.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne
    [Retour] : Person.t option
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_person_link base_prefix ip =
  let base_prefix = Link.chop_base_prefix base_prefix in
  try Some (Hashtbl.find Link.ht_person_cache (base_prefix, ip)) with
    Not_found ->
      try
        let (base_prefix, ip) =
          Hashtbl.find Link.ht_corresp (base_prefix, ip)
        in
        Some (Hashtbl.find Link.ht_person_cache (base_prefix, ip))
      with Not_found -> None


(* ************************************************************************** *)
(*  [Fonc] get_persons_link : string -> iper -> Person.t list            *)
(** [Description] : Retourne la liste de toutes les personnes distantes.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne
    [Retour] : Person.t list
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_persons_link base_prefix ip =
  let base_prefix = Link.chop_base_prefix base_prefix in
  let find_corr (base_prefix, ip) =
    let l = Hashtbl.find_all Link.ht_corresp (base_prefix, ip) in
    List.fold_left
      (fun accu (base_prefix, ip) ->
         try
           let p = Hashtbl.find Link.ht_person_cache (base_prefix, ip) in
           p :: accu
         with Not_found -> accu)
      [] l
  in
  try
    let p = Hashtbl.find Link.ht_person_cache (base_prefix, ip) in
    let l = find_corr (base_prefix, ip) in p :: l
  with Not_found -> find_corr (base_prefix, ip)


(* ************************************************************************** *)
(*  [Fonc] get_parents_link : string -> iper -> Family.t option          *)
(** [Description] : Recherche les parents d'une personne. Si on ne la trouve
      pas directement dans le cache, alors on les recherches par rapport à
      toutes les correspondances.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne
    [Retour] : Family.t option
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_parents_link base_prefix ip =
  let base_prefix = Link.chop_base_prefix base_prefix in
  try Some (Hashtbl.find Link.ht_parents_cache (base_prefix, ip)) with
    Not_found ->
      try
        let rec loop l =
          match l with
            [] -> None
          | (base_prefix, ip) :: l ->
              try
                Some (Hashtbl.find Link.ht_parents_cache (base_prefix, ip))
              with Not_found -> loop l
        in
        loop (Hashtbl.find_all Link.ht_corresp (base_prefix, ip))
      with Not_found -> None


(* ************************************************************************** *)
(*  [Fonc] get_father_link : string -> iper -> Person.t option           *)
(** [Description] : Recherche le père d'une famille. Si on ne le trouve
      pas directement dans le cache, alors on le recherche par rapport à
      toutes les correspondances.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne
    [Retour] : Person.t option
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_father_link base_prefix ip =
  match get_parents_link base_prefix ip with
    Some family ->
      let base_prefix = family.MLink.Family.baseprefix in
      let ifath = Gwdb.iper_of_string family.MLink.Family.ifath in
      get_person_link base_prefix ifath
  | None -> None


(* ************************************************************************** *)
(*  [Fonc] get_mother_link : string -> iper -> Person.t option           *)
(** [Description] : Recherche la mère d'une famille. Si on ne la trouve
      pas directement dans le cache, alors on la recherche par rapport à
      toutes les correspondances.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne
    [Retour] : Person.t option
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_mother_link base_prefix ip =
  match get_parents_link base_prefix ip with
    Some family ->
      let base_prefix = family.MLink.Family.baseprefix in
      let imoth = Gwdb.iper_of_string family.MLink.Family.imoth in
      get_person_link base_prefix imoth
  | None -> None


(* ************************************************************************** *)
(*  [Fonc] get_family_correspondance : string -> iper -> Family_link.t list   *)
(** [Description] : Retourne la liste de toutes les correspondances de
                    familles d'une personne.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne
    [Retour] : Family_link.t list
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_family_correspondance base_prefix ip =
  let base_prefix = Link.chop_base_prefix base_prefix in
  try Hashtbl.find Link.ht_families_cache (base_prefix, ip) with
    Not_found ->
      try
        let l = Hashtbl.find_all Link.ht_corresp (base_prefix, ip) in
        List.fold_left
          (fun accu (base_prefix, ip) ->
             try
               Hashtbl.find Link.ht_families_cache (base_prefix, ip) @ accu
             with Not_found -> accu)
          [] l
      with Not_found -> []


(* ************************************************************************** *)
(*  [Fonc] get_family_link : string -> iper -> Family.t list             *)
(** [Description] : Retourne la liste de toutes les familles d'une personne.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne
    [Retour] : Family.t list
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_family_link base_prefix ip =
  let base_prefix = Link.chop_base_prefix base_prefix in
  try
    let l = get_family_correspondance base_prefix ip in
    List.fold_right
      (fun fam accu ->
         let (base_prefix, ifam) =
           fam.MLink.Family_link.baseprefix,
           Gwdb.ifam_of_string fam.MLink.Family_link.ifam
         in
         let base_prefix = Link.chop_base_prefix base_prefix in
         try
           Hashtbl.find Link.ht_family_cache (base_prefix, ifam) :: accu
         with Not_found -> accu)
      l []
  with Not_found -> []


(* ************************************************************************** *)
(*  [Fonc] get_families_of_parents :
             string -> iper -> iper -> Family.t list                     *)
(** [Description] : Retourne la liste de toutes les familles d'un couple.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne
      - isp : l'index de la spouse
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_families_of_parents base_prefix ip isp =
  let lip = get_persons_link base_prefix ip in
  let lisp = get_persons_link base_prefix isp in
  List.fold_left
    (fun accu p ->
       let base_prefix = p.MLink.Person.baseprefix in
       let ip = Gwdb.iper_of_string p.MLink.Person.ip in
       let faml = get_family_link base_prefix ip in
       List.fold_left
         (fun accu fam ->
            let ifath = Gwdb.iper_of_string fam.MLink.Family.ifath in
            let imoth = Gwdb.iper_of_string fam.MLink.Family.imoth in
            List.fold_left
              (fun accu sp ->
                 let isp = Gwdb.iper_of_string sp.MLink.Person.ip in
                 if ip = ifath && isp = imoth || isp = ifath && ip = imoth
                 then
                   fam :: accu
                 else accu)
              accu lisp)
         accu faml)
    [] lip


(* ************************************************************************** *)
(*  [Fonc] get_children_of_fam : string -> ifam -> Person.t list         *)
(** [Description] : Retourne la liste des enfants distants pour une famille
                    donnée.
    [Args] :
      - base_prefix : base locale
      - ifam : l'index de la famille
    [Retour] : Person.t list
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_children_of_fam base_prefix ifam =
  let base_prefix = Link.chop_base_prefix base_prefix in
  try
    let fam = Hashtbl.find Link.ht_family_cache (base_prefix, ifam) in
    List.fold_right
      (fun c accu ->
         let (base_prefix, ip) =
           c.MLink.Person_link.baseprefix,
           Gwdb.iper_of_string c.MLink.Person_link.ip
         in
         match get_person_link base_prefix ip with
           Some p -> p :: accu
         | None -> accu)
      fam.MLink.Family.children []
  with Not_found -> []


(* ************************************************************************** *)
(*  [Fonc] get_children_with_fam :
             string -> ifam -> iper -> iper -> Person.t list             *)
(** [Description] : Retourne la liste des enfants distants pour une couple
                    donné.
    [Args] :
      - base_prefix : base locale
      - ifam : l'index de la famille
      - ifath : index du père
      - imoth : index de la mère
    [Retour] : Person.t list
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_children_of_parents base_prefix ifam ifath imoth =
  let base_prefix = Link.chop_base_prefix base_prefix in
  try
    let fam = Hashtbl.find Link.ht_family_cache (base_prefix, ifam) in
    if ifath = Gwdb.iper_of_string fam.MLink.Family.ifath &&
       imoth = Gwdb.iper_of_string fam.MLink.Family.imoth
    then
      List.fold_right
        (fun c accu ->
           let base_prefix = c.MLink.Person_link.baseprefix in
           let ip = Gwdb.iper_of_string c.MLink.Person_link.ip in
           match get_person_link base_prefix ip with
             Some p -> p :: accu
           | None -> accu)
        fam.MLink.Family.children []
    else []
  with Not_found -> []


(**/**)


(* ************************************************************************** *)
(*  [Fonc] can_merge_family : string -> iper -> family list ->
                                Family.t -> 'a * 'b * iper -> bool       *)
(** [Description] : Fait appel à la fonction qui va calculer le cache avec
                    les appels CURL.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne dont on a les familles
      - fam : la liste des familles en local
      - fam_link : la famille que l'on souhaite merger
      - (_, _, isp) : index de la spouse à merger
    [Retour] : Vrai si on peut merger, faux sinon.
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let can_merge_family base_prefix ip fam fam_link (_, _, isp) =
  let from_baseprefix = Link.chop_base_prefix base_prefix in
  let base_prefix = Link.chop_base_prefix fam_link.MLink.Family.baseprefix in
  let rec loop faml =
    match faml with
      [] -> false
    | fam :: faml ->
        let from_ip = Gutil.spouse ip fam in
        try
          let (to_baseprefix, to_ip) =
            Hashtbl.find Link.ht_corresp (from_baseprefix, from_ip)
          in
          to_baseprefix = base_prefix && to_ip = isp || loop faml
        with Not_found -> loop faml
  in
  loop fam


(* ************************************************************************** *)
(*  [Fonc] can_merge_child : string -> iper Array -> Person.t -> bool    *)
(** [Description] : Vérifie si l'ip donné en paramètre peut être mergé parce
      que cette personne serait déjà présente dans le tableau des enfants.
      Pour cela, on vérifie qu'une correspondance existe.
    [Args] :
      - base_prefix : base locale
      - ip : l'index de la personne que l'on veut merger
      - children : la liste des enfants en local
      - c_link : la personne que l'on souhaite merger
    [Retour] : Vrai si on peut merger, faux sinon.
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let can_merge_child base_prefix children c_link =
  let from_baseprefix = Link.chop_base_prefix base_prefix in
  let ip = Gwdb.iper_of_string c_link.MLink.Person.ip in
  let base_prefix = Link.chop_base_prefix c_link.MLink.Person.baseprefix in
  let rec loop children =
    match children with
      [] -> false
    | from_ip :: children ->
        try
          let (to_baseprefix, to_ip) =
            Hashtbl.find Link.ht_corresp (from_baseprefix, from_ip)
          in
          to_baseprefix = base_prefix && to_ip = ip || loop children
        with Not_found -> loop children
  in
  loop (Array.to_list children)

(**/**)


(* ************************************************************************** *)
(*  [Fonc] init_cache : conf -> base -> iper -> int -> int -> int -> unit     *)
(** [Description] : Fait appel à la fonction qui va calculer le cache avec
                    les appels CURL.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - ip : l'index de la personne sur qui on veut les informations
      - nb_asc : le nombre de génération en asc dont on veut le cache
      - from_gen_desc : la génération à partir de laquelle on veut la desc
      - nb_desc : le nombre de génération en des dont on veut le cache
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let init_cache conf base ip nb_asc from_gen_desc nb_desc =
  (* Option pour activer/desactiver totalement le cache. *)
  let init = Wserver.extract_param "links-tree: " '\n' conf.request in
  if init = "1" then
    Link.init_cache conf base conf.request conf.bname ip nb_asc from_gen_desc
      nb_desc

(* ***************************************************************************** *)
(*  [Fonc] max_interlinks_descendancy_level : config -> base -> ip -> int -> int *)
(** [Description] : Retourne le nombre maximal de niveaux de descendance
                    en prenant en compte les liens inter-arbres.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - ip   : l'index de la personne
      - max_lev : limite du nombre d'ascendants
    [Retour] :
      - int
                                                                                *)
(* **************************************************************************** *)
let max_interlinks_descendancy_level conf base ip max_lev =
  let x = ref 0 in
  (* Charge le cache *)
  let () = init_cache conf base ip 10 1 max_lev in
  (* Itère sur chaque personne de l'arbre *)
  let rec loop level (ip, base_prefix) =
    (* Met à jour x.val avec la valeur la plus haute du niveau trouvé. *)
    x := max !x level;
    (* Sort de la boucle si le nombre de descendants est suffisamment haut. *)
    if !x = max_lev then ()
    else
      let families_link = get_family_link base_prefix ip in
      (* Itère sur chaque famille. *)
      List.iter
        (fun family_link ->
           (* Itère sur chaque enfant de la famille. *)
           List.iter
             (fun child_link ->
                (* Prend le nom de l'arbre de la famille en cours. *)
                let baseprefix = child_link.MLink.Person_link.baseprefix in
                (* Prend l'index de l'enfant. *)
                let ip_child = Gwdb.iper_of_string child_link.MLink.Person_link.ip in
                (* Recherche à nouveau des descendants sur l'enfant en incrémentant le niveau de descendance. *)
                loop (succ level) (ip_child, baseprefix))
             family_link.MLink.Family.children)
        families_link
  in
  loop 0 (ip, conf.command); !x

#endif
