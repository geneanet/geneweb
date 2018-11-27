#ifdef API

module MLink = Api_link_tree_piqi
module MLinkext = Api_link_tree_piqi_ext
module RC = Redis_sync.Client

open Config
open Def
open Gwdb

(* base redis contenant tous les liens. *)
let redis_host_all = ref "127.0.0.1" ;;
let redis_port_all = ref 6379 ;;
(* base redis contenant tous les liens modérés. *)
let redis_host_moderate = ref "127.0.0.1" ;;
let redis_port_moderate = ref 6379 ;;
(* base redis utilisée. *)
let redis_host = ref !redis_host_all ;;
let redis_port = ref !redis_port_all ;;
let api_servers = ref [] ;;


(**/**) (* Redis. *)

let create_redis_connection () =
  let connection_spec = {RC.host = !redis_host; RC.port = !redis_port} in
  RC.IO.run (RC.connect connection_spec)

let redis_p_key base ip =
  let p = poi base ip in
  let sn = Name.lower (sou base (get_surname p)) in
  let fn = Name.lower (sou base (get_first_name p)) in
  let occ = get_occ p in
  sn ^ "|" ^ fn ^ "|" ^ if occ > 0 then string_of_int occ else ""

let filter_bulk l =
  List.fold_right
    (fun reply accu ->
      match reply with
      | `Bulk s ->
          begin
            match s with
            | Some s -> s :: accu
            | None -> accu
          end
      | _ -> accu)
    l []

let filter_string l =
  List.fold_right
    (fun s accu ->
      match s with
      | Some s -> s :: accu
      | None -> accu)
    l []

(* Trouve une key en fonction de l'utilisateur et d'une référence GW *)
let findKeyBySourcenameAndRef redis bname geneweb_key =
  RC.IO.run (RC.zscore redis ("lia.keys." ^ bname) geneweb_key)

let findBridgesBySourcenameAndIdGlinks redis bname fb =
  let l = RC.zrangebyscore redis ("lia.bridges." ^ bname) fb fb in
  filter_bulk (RC.IO.run l)

let findLinksBySourcenameAndBridge redis bname s =
  RC.IO.run (RC.hget redis ("lia.links." ^ bname) s)

let findKeyBySourcenameAndIdGlinks redis bname fb =
  let l = RC.zrangebyscore redis ("lia.keys." ^ bname) fb fb in
  filter_bulk (RC.IO.run l)

(* connection -> string -> string -> string option *)
(* Cherche les données d'un bridge en fonction du sourcename et de l'ID du bridge. *)
let findBridgeDataBySourcenameAndBridgeId redis bname bridge_id =
  RC.IO.run (RC.hget redis ("lia.bridges_data." ^ bname) bridge_id)

let json_list_of_string s =
  Yojson.Basic.Util.filter_string
    (Yojson.Basic.Util.to_list
       (Yojson.Basic.from_string s))

(* conf -> base -> connection -> ip -> bool *)
(* Permet de récupérer les ponts d'une personne (en utilisant son index). *)
(* include_not_validated : booléen indiquant l'inclusion ou non des ponts non validés. *)
let get_bridges conf base redis ip include_not_validated =
  (* on récupère la clé *)
  match
    findKeyBySourcenameAndRef redis conf.bname (redis_p_key base ip)
  with
  | Some f ->
      (* Récupère tous les IDs de ponts. *)
      let bridge_ids =
        findBridgesBySourcenameAndIdGlinks redis conf.bname (RC.FloatBound.Inclusive f)
      in
      (* Filtre sur les ponts validés. *)
      List.fold_left
        (fun accu bridge_id ->
          (* Récupère les données du pont. *)
          match findBridgeDataBySourcenameAndBridgeId redis conf.bname bridge_id
          with
           | Some bridge_data ->
              (* Les ponts ne sont pas filtrés si les ponts non validés sont inclus. *)
              if include_not_validated then
                bridge_id::accu
              else
                (* Parsing du JSON. *)
                let is_validated_bridge = List.hd (Yojson.Basic.Util.filter_string
                  (Yojson.Basic.Util.filter_member "validated" [Yojson.Basic.from_string bridge_data])) in
                (* Ajoute l'ID du pont à la liste seulement s'il est validé. *)
                if ((int_of_string is_validated_bridge) == 1) then
                  bridge_id::accu
                else
                  accu
           | None -> accu)
        [] bridge_ids
  | None -> []

(* conf -> base -> connection -> ip -> bool *)
(* Permet de récupérer les liens d'une personne (en utilisant son index). *)
(* include_not_validated : booléen indiquant l'inclusion ou non des liens non validés. *)
let get_links conf base redis ip include_not_validated =
  (* L'inclusion ou non des liens non validés se fait lors de la récupération des ponts. *)
  (* On pourrait aussi le faire au niveau des liens puisqu'ils contiennent également l'information validated. *)
  match get_bridges conf base redis ip include_not_validated with
  | [] -> []
  | l ->
      (* on récupère les liens associées *)
    List.map (findLinksBySourcenameAndBridge redis conf.bname) l
    |> filter_string

(**/**) (* Convertion d'une date, personne, famille. *)

(* ************************************************************************ *)
(*  [Fonc] piqi_date_of_date : def.date -> piqi_date                        *)
(** [Description] : Converti une date en date piqi
    [Args] :
      - date : la date a convertir
    [Retour] :
      - piqi date : date du module MLink.
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let piqi_date_of_date date =
  match date with
  | Dgreg (dmy, cal) ->
      let (cal, dmy) =
        match cal with
        | Dgregorian -> (None, dmy)
        | Djulian -> (Some `julian, Calendar.julian_of_gregorian dmy)
        | Dfrench -> (Some `french, Calendar.french_of_gregorian dmy)
        | Dhebrew -> (Some `hebrew, Calendar.hebrew_of_gregorian dmy)
      in
      let (prec, dmy, dmy2) =
        let d = Some (Int32.of_int dmy.day) in
        let m = Some (Int32.of_int dmy.month) in
        let y = Some (Int32.of_int dmy.year) in
        let delta = Some (Int32.of_int dmy.delta) in
        let dmy1 = {MLink.Dmy.day = d; month = m; year = y; delta = delta;} in
        let (prec, dmy2) =
          match dmy.prec with
          | Sure -> (`sure, None)
          | About -> (`about, None)
          | Maybe -> (`maybe, None)
          | Before -> (`before, None)
          | After -> (`after, None)
          | OrYear dmy2 ->
              let d = Some (Int32.of_int dmy2.day2) in
              let m = Some (Int32.of_int dmy2.month2) in
              let y = Some (Int32.of_int dmy2.year2) in
              let delta = Some (Int32.of_int dmy2.delta2) in
              let dmy2 =
                {MLink.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`oryear, Some dmy2)
          | YearInt dmy2 ->
              let d = Some (Int32.of_int dmy2.day2) in
              let m = Some (Int32.of_int dmy2.month2) in
              let y = Some (Int32.of_int dmy2.year2) in
              let delta = Some (Int32.of_int dmy2.delta2) in
              let dmy2 =
                {MLink.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`yearint, Some dmy2)
        in
        (prec, dmy1, dmy2)
      in
      {
        MLink.Date.cal = cal;
        prec = Some prec;
        dmy = Some dmy;
        dmy2 = dmy2;
        text = None;
      }
  | Dtext txt ->
      {
        MLink.Date.cal = None;
        prec = None;
        dmy = None;
        dmy2 = None;
        text = Some txt;
      }

let p_to_piqi_full_person conf base ip ip_spouse =
  let baseprefix = conf.command in
  let index = Int32.of_int (Adef.int_of_iper ip) in
  let p = poi base ip in
  let p_auth = Util.authorized_age conf base p in
  let p_hidden = Util.is_hide_names conf p in
  let gen_p = Util.string_gen_person base (gen_person_of_person p) in
  let surname =
    if not p_auth && p_hidden then "x"
    else gen_p.surname
  in
  let first_name =
    if not p_auth && p_hidden then "x"
    else gen_p.first_name
  in
  let sn = Name.lower surname in
  let fn = Name.lower first_name in
  let occ = Int32.of_int (get_occ p) in
  let image = if p_auth && gen_p.image <> "" then Some gen_p.image else None in
  let occupation =
    if p_auth && gen_p.occupation <> "" then Some gen_p.occupation else None
  in
  let publicname =
    if not p_auth && p_hidden then None else Some gen_p.public_name
  in
  let qualifiers = if not p_auth && p_hidden then [] else gen_p.qualifiers in
  (* TODO
  let titles = Perso.nobility_titles_list conf base p in
  let titles =
    let tmp_conf = {(conf) with cancel_links = true} in
    List.map (Perso.string_of_title tmp_conf base "" p) titles
  in *)
  let titles = [] in
  let aliases = if not p_auth && p_hidden then [] else gen_p.aliases in
  let sex =
    match gen_p.sex with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let birth =
    match Adef.od_of_cdate gen_p.birth with
    | Some d when p_auth -> Some (piqi_date_of_date d)
    | _ -> None
  in
  let birth_place =
    if p_auth && gen_p.birth_place <> "" then Some gen_p.birth_place
    else None
  in
  let baptism =
    match Adef.od_of_cdate gen_p.baptism with
    | Some d when p_auth -> Some (piqi_date_of_date d)
    | _ -> None
  in
  let baptism_place =
    if p_auth && gen_p.baptism_place <> "" then Some gen_p.baptism_place
    else None
  in
  let (death_type, death) =
    if p_auth then
      match gen_p.death with
      | NotDead -> (`not_dead, None)
      | Death (_, cd) ->
          let d = Adef.date_of_cdate cd in
          (`dead, Some (piqi_date_of_date d))
      | DeadYoung -> (`dead_young, None)
      | DeadDontKnowWhen -> (`dead_dont_know_when, None)
      | DontKnowIfDead -> (`dont_know_if_dead, None)
      | OfCourseDead -> (`of_course_dead, None)
    else
      (`not_dead, None)
  in
  let death_place =
    if p_auth && gen_p.death_place <> "" then Some gen_p.death_place
    else None
  in
  let burial =
    match gen_p.burial with
    | Buried cod | Cremated cod ->
        (match Adef.od_of_cdate cod with
        | Some d when p_auth -> Some (piqi_date_of_date d)
        | _ -> None)
    | _ -> None
  in
  let burial_place =
    if p_auth && gen_p.burial_place <> "" then Some gen_p.burial_place
    else None
  in
  let families =
    List.fold_right
      (fun ifam accu ->
        let isp = Gutil.spouse ip (foi base ifam) in
        if isp = ip_spouse || ip_spouse = Adef.iper_of_int (-1) then
          let baseprefix = conf.command in
          let index = Int32.of_int (Adef.int_of_ifam ifam) in
          let fl =
            MLink.Family_link.({
              baseprefix = baseprefix;
              ifam = index;
            })
          in
          fl :: accu
        else accu)
      (Array.to_list (get_family p)) []
  in
  {
    MLink.Person.baseprefix = baseprefix;
    ip = index;
    n = sn;
    p = fn;
    oc = occ;
    lastname = surname;
    firstname = first_name;
    image = image;
    occupation = occupation;
    public_name = publicname;
    qualifiers = qualifiers;
    titles = titles;
    aliases = aliases;
    sex = sex;
    birth_date = birth;
    birth_place = birth_place;
    baptism_date = baptism;
    baptism_place = baptism_place;
    death_type = death_type;
    death_date = death;
    death_place = death_place;
    burial_date = burial;
    burial_place = burial_place;
    families = families;
  }

let fam_to_piqi_full_family conf base ip ifam add_children =
  let baseprefix = conf.command in
  let index = Int32.of_int (Adef.int_of_ifam ifam) in
  let fam = foi base ifam in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let m_auth =
    Util.authorized_age conf base (poi base ifath) &&
    Util.authorized_age conf base (poi base imoth)
  in
  let ifath = Int32.of_int (Adef.int_of_iper ifath) in
  let imoth = Int32.of_int (Adef.int_of_iper imoth) in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let marriage =
    match (m_auth, Adef.od_of_cdate gen_f.marriage) with
    | (true, Some d) -> Some (piqi_date_of_date d)
    | _ -> None
  in
  let marriage_place =
    if m_auth && gen_f.marriage_place <> "" then Some gen_f.marriage_place
    else None
  in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
        (match Adef.od_of_cdate cod with
         | Some d when m_auth ->
             let divorce_date = Some (piqi_date_of_date d) in
             (`divorced, divorce_date)
         | _ -> (`divorced, None))
    | Separated -> (`separated, None)
  in
  let children =
    if add_children then
      List.map
        (fun ip ->
          let ip = Int32.of_int (Adef.int_of_iper ip) in
          MLink.Person_link.({
            baseprefix = baseprefix;
            ip = ip;
          }))
        (Array.to_list (get_children fam))
    else
      let pl =
        let ip = Int32.of_int (Adef.int_of_iper ip) in
        MLink.Person_link.({
          baseprefix = baseprefix;
          ip = ip;
        })
      in
      [pl]
  in
  {
    MLink.Family.baseprefix = baseprefix;
    ifam = index;
    ifath = ifath;
    imoth = imoth;
    marriage_type = marriage_type;
    marriage_date = marriage;
    marriage_place = marriage_place;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    children = children;
  }


(**/**)

let get_families_asc conf base ip nb_asc =
  let rec loop_asc parents families =
    match parents with
    | [] -> families
    | (ip, gen) :: parents ->
    if gen = nb_asc then loop_asc parents families
    else
      let p = poi base ip in
      if Util.authorized_age conf base p
      then match get_parents p with
        | Some ifam ->
          let cpl = foi base ifam in
          let ifath = get_father cpl in
          let imoth = get_mother cpl in
          loop_asc ((ifath, gen + 1) :: (imoth, gen + 1) :: parents)
            ((ip, ifam, gen) :: families)
        | None -> loop_asc parents families
      else loop_asc parents families
  in
  loop_asc [(ip, 0)] []

let get_families_desc conf base ip ip_spouse from_gen_desc nb_desc =
  if from_gen_desc <= 0 then []
  else
    let rec loop_asc pl accu =
      match pl with
      | [] -> accu
      | (ip, gen) :: pl ->
          if gen = from_gen_desc then loop_asc pl accu
          else
            let p = poi base ip in
            if Util.authorized_age conf base p
            then match get_parents (poi base ip) with
              | Some ifam ->
                let cpl = foi base ifam in
                let ifath = get_father cpl in
                let imoth = get_mother cpl in
                loop_asc ((ifath, gen + 1) :: (imoth, gen + 1) :: pl)
                    ((ip, gen) :: accu)
              | None -> loop_asc pl accu
            else loop_asc pl accu
    in
    (* Récupère les ascendants jusqu'au nombre de générations from_gen_desc. *)
    (* Utile pour le template affichant les parents des conjoints. *)
    let ipl = loop_asc [(ip, 0)] [] in
    let ipl =
      match ipl with
      | [] -> [(ip, 0)]
      | _ -> ipl
    in
    let rec loop_desc pl accu =
      match pl with
      | [] -> accu (* Retourne accu lorsqu'il n'y a plus rien à parcourir. *)
      | (ip, gen) :: pl ->
        let p = poi base ip in
        if Util.authorized_age conf base p
        then
          let fam = Array.to_list (get_family p) in
          let fam =
            if gen = 0 && ip_spouse <> Adef.iper_of_int (-1) then
              List.filter
                (fun ifam ->
                  let fam = foi base ifam in
                  let isp = Gutil.spouse ip fam in
                  isp = ip_spouse)
                fam
            else fam
          in
          let accu =
            (* Si la génération est inférieure à celle demandée, les données ne sont pas retournées. *)
            if gen <= -nb_desc then accu
            else
              List.fold_left
                (fun accu ifam -> (ip, ifam, gen) :: accu)
                accu fam
          in
          let pl =
            List.fold_left
              (fun pl ifam ->
                let fam = foi base ifam in
                List.fold_left
                  (* Ne récupère pas les descendants si la génération suivante est inférieure à celle demandée. *)
                  (fun pl ic -> if gen - 1 <= -nb_desc then pl else (ic, gen - 1) :: pl)
                  pl (Array.to_list (get_children fam)))
              pl fam
          in
          loop_desc pl accu
        else loop_desc pl accu
    in
    loop_desc ipl []

(**/**)

let get_link_tree_curl conf request basename bname ip s s2 nb_asc from_gen_desc nb_desc =
  let host =
    let rec loop api_servers =
      match api_servers with
      | [] -> ("")
      | (reg, host) :: l ->
          let regexp = Str.regexp reg in
          if Str.string_match regexp bname 0 then host
          else loop l
    in
    loop !api_servers
  in
  let index = Some (Int32.of_int (Adef.int_of_iper ip)) in
  let data =
    MLink.Link_tree_params.({
      basename = basename;
      ip = index;
      ref_person = Some s;
      ref_person2 = Some s2;
      nb_asc = Int32.of_int nb_asc;
      from_gen_desc = Int32.of_int from_gen_desc;
      nb_desc = Int32.of_int nb_desc;
    })
  in
  let data = MLinkext.gen_link_tree_params data `pb in
  let url =
    Printf.sprintf
      "http://%s/%s?m=API_LINK_TREE&input=pb&output=pb&sig=azerty&data=%s"
      host bname (Wserver.encode data)
  in
  let res = ref "" in
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  begin
    let result = Buffer.create 16384
    and errorBuffer = ref "" in
    try
      let connection = Curl.init () in
      let headers = [] in
      let headers =
        let auth = Wserver.extract_param "authorization: " '\r' request in
        if auth <> "" then
          ("Authorization: " ^ auth) :: ("Gw-Connection-Type: auto") ::headers
        else headers
      in
      let headers =
        let include_not_validated =
          Wserver.extract_param "inter-tree-links-include-not-validated: " '\r' request
        in
        if include_not_validated <> "" then
          ("Inter-Tree-Links-Include-Not-Validated: " ^ include_not_validated) :: headers
        else headers
      in
      Curl.set_httpheader connection headers;
      Curl.set_errorbuffer connection errorBuffer;
      Curl.set_writefunction connection
        (fun data ->
           Buffer.add_string result data;
           String.length data);
      Curl.set_followlocation connection true;
      Curl.set_url connection url;
      Curl.set_timeoutms connection 1000;
      Curl.perform connection;
      Curl.cleanup connection;
      res := Buffer.contents result
    with
    | Curl.CurlException _ ->
        Printf.fprintf stderr "Error: %s\n" !errorBuffer
    | Failure s ->
        Printf.fprintf stderr "Caught exception: %s\n" s
  end;
  Curl.global_cleanup ();
  let output_encoding =
    match Api_util.p_getenvbin conf.env "output" with
     | Some "pb" -> `pb
     | Some "json" -> `json
     | Some "xml" -> `xml
     | _ -> exit (-2)
  in
  MLinkext.parse_link_tree !res output_encoding


let print_link_tree conf base =
  let params = Api_util.get_params conf MLinkext.parse_link_tree_params in
  let basename = params.MLink.Link_tree_params.basename in
  let ip = params.MLink.Link_tree_params.ip in
  let ref_person = params.MLink.Link_tree_params.ref_person in
  let ref_person2 = params.MLink.Link_tree_params.ref_person2 in
  let nb_asc = Int32.to_int params.MLink.Link_tree_params.nb_asc in
  let from_gen_desc = Int32.to_int params.MLink.Link_tree_params.from_gen_desc in
  let nb_desc = Int32.to_int params.MLink.Link_tree_params.nb_desc in

  (* Gestion de l'inclusion des not validated. *)
  let include_not_validated =
    let h_include_not_validated = Wserver.extract_param "inter-tree-links-include-not-validated: " '\r' conf.request in
    if h_include_not_validated = "1" then true else false
  in

  let redis = create_redis_connection () in

  let ip_local =
    match ref_person with
    | Some s ->
        begin
          match Link.ip_of_ref_person base s with
          | Some ip -> ip
          | None -> Adef.iper_of_int (-1)
        end
    | None ->
        match ip with
        | Some ip -> Adef.iper_of_int (Int32.to_int ip)
        | None -> Adef.iper_of_int (-1)
  in

  let ip_distant =
    match ip with
    | Some ip -> Adef.iper_of_int (Int32.to_int ip)
    | None -> Adef.iper_of_int (-1)
  in

  let ip_local_spouse =
    match ref_person2 with
    | Some s when s <> "" ->
        begin
          match Link.ip_of_ref_person base s with
          | Some ip -> ip
          | None -> Adef.iper_of_int (-1)
        end
    | _ -> Adef.iper_of_int (-1)
  in

  (* On rend unique tous les résultats. *)
  let _uniq l =
    let ht = Hashtbl.create 101 in
    List.fold_left
      (fun accu l ->
         List.fold_left
           (fun accu x ->
              let h = Hashtbl.hash x in
              if Hashtbl.mem ht h then accu
              else (Hashtbl.add ht h (); x :: accu))
           accu l)
      [] l
  in

  (* La liste de toutes les personnes à renvoyer. *)
  let pl =
    get_families_desc conf base ip_local ip_local_spouse from_gen_desc nb_desc
  in
  (* On dédoublonne la liste. *)
  let pl =
    let ht = Hashtbl.create (List.length pl) in
    List.filter
      (fun (ip, ifam, gen) ->
         not (Hashtbl.mem ht (ip, ifam, gen)) ||
         (Hashtbl.add ht (ip, ifam, gen) (); true))
      pl
  in

  (* Familles ascendantes locales. *)
  let local_asc_fam =
    if conf.bname <> basename &&
       ip_local <> Adef.iper_of_int (-1) &&
       ip_distant <> Adef.iper_of_int (-1)
    then
      let families = get_families_asc conf base ip_local nb_asc in
      List.map
        (fun (ip, ifam, gen) ->
          let add_children = gen < from_gen_desc in
          fam_to_piqi_full_family conf base ip ifam add_children)
        families
    else []
  in

  (* Familles descendantes locales. *)
  let local_desc_fam =
    if conf.bname <> basename &&
       ip_local <> Adef.iper_of_int (-1) &&
       ip_distant <> Adef.iper_of_int (-1)
    then
      List.map
        (fun (ip, ifam, _) -> fam_to_piqi_full_family conf base ip ifam true)
        pl
    else []
  in

  (* Familles locales. *)
  let local_families = local_asc_fam @ local_desc_fam in

  (* Personnes locales issues des familles asc et desc. *)
  let local_persons =
    let ht = Hashtbl.create 101 in
    List.fold_left
      (fun accu fam ->
         let ifath = Adef.iper_of_int (Int32.to_int fam.MLink.Family.ifath) in
         let imoth = Adef.iper_of_int (Int32.to_int fam.MLink.Family.imoth) in
         let accu =
           if Hashtbl.mem ht ifath then accu
           else
             begin
               Hashtbl.add ht ifath ();
               let ip_spouse =
                 if ip_local = ifath then ip_local_spouse
                 else Adef.iper_of_int (-1)
               in
               p_to_piqi_full_person conf base ifath ip_spouse :: accu
             end
         in
         let accu =
           if Hashtbl.mem ht imoth then accu
           else
             begin
               Hashtbl.add ht imoth ();
               let ip_spouse =
                 if ip_local = imoth then ip_local_spouse
                 else Adef.iper_of_int (-1)
               in
               p_to_piqi_full_person conf base imoth ip_spouse :: accu
             end
         in
         List.fold_left
           (fun accu c ->
              let ic = Adef.iper_of_int (Int32.to_int c.MLink.Person_link.ip) in
              if Hashtbl.mem ht ic then accu
              else
                begin
                  Hashtbl.add ht ic ();
                  let ip_spouse =
                    if ip_local = ic then ip_local_spouse
                    else Adef.iper_of_int (-1)
                  in
                  p_to_piqi_full_person conf base ic ip_spouse :: accu
                end)
           accu fam.MLink.Family.children)
      [] local_families
  in

  (* Correspondances locales. *)
  (* On constitue la liste de toutes les personnes, *)
  (* puis on ira chercher les correspondances.      *)
  let all_persons =
    let ht = Hashtbl.create 101 in
    List.fold_left
      (fun accu (_, ifam, _) ->
         let fam = foi base ifam in
         let ifath = get_father fam in
         let imoth = get_mother fam in
         let accu =
           if Hashtbl.mem ht ifath then accu
           else
             begin
               Hashtbl.add ht ifath ();
               p_to_piqi_full_person conf base ifath (Adef.iper_of_int (-1)) :: accu
             end
         in
         let accu =
           if Hashtbl.mem ht imoth then accu
           else
             begin
               Hashtbl.add ht imoth ();
               p_to_piqi_full_person conf base imoth (Adef.iper_of_int (-1)) :: accu
             end
         in
         List.fold_left
           (fun accu ic ->
              if Hashtbl.mem ht ic then accu
              else
                begin
                  Hashtbl.add ht ic ();
                  p_to_piqi_full_person conf base ic (Adef.iper_of_int (-1)) :: accu
                end)
           accu (Array.to_list (get_children fam)))
      [] pl
  in

  let local_connections =
    List.fold_left
      (fun accu p ->
        let ip = Adef.iper_of_int (Int32.to_int p.MLink.Person.ip) in
        let bl = get_bridges conf base redis ip include_not_validated in
        List.fold_left
          (fun accu s ->
             match String.split_on_char ':' s with
             | [_; bname_link; id_link] ->
                 begin
                   match
                     findKeyBySourcenameAndIdGlinks
                       redis bname_link
                       (RC.FloatBound.Inclusive (float_of_string id_link))
                   with
                   | [s] ->
                       let from_ref = redis_p_key base ip in
                       let corresp =
                         MLink.Connection.({
                           from_baseprefix = conf.bname;
                           from_ref = from_ref;
                           to_baseprefix = bname_link;
                           to_ref = s;
                         })
                       in
                       corresp :: accu
                   | _ -> accu
                 end
             | _ -> accu)
          accu bl)
      [] all_persons
  in

  (* Descendance distante. *)
  let distant_desc_fam =
    let pl =
      match pl with
      | [] -> [(ip_local, Adef.ifam_of_int (-1), 0)]
      | _ -> pl
    in
    let ht_request = Hashtbl.create 101 in
    List.fold_left
      (fun (accu_fam, accu_pers, accu_conn) (ip, _, gen) ->
        if Hashtbl.mem ht_request ip then (accu_fam, accu_pers, accu_conn)
        else
          begin
            Hashtbl.add ht_request ip ();
            let links = get_links conf base redis ip include_not_validated in
            List.fold_left
              (fun (accu_fam, accu_pers, accu_conn) s ->
                List.fold_left
                  (fun (accu_fam, accu_pers, accu_conn) x ->
                    match String.split_on_char ':' x with
                    | [_; bname_link; id_link; "spouse-children"; id_link_spouse] ->
                        let pl =
                          findKeyBySourcenameAndIdGlinks redis bname_link
                            (RC.FloatBound.Inclusive (float_of_string id_link))
                        in
                        let pl2 =
                          findKeyBySourcenameAndIdGlinks redis bname_link
                            (RC.FloatBound.Inclusive (float_of_string id_link_spouse))
                        in
                        begin
                          match (pl, pl2) with
                          | ([s], [s2]) ->
                              let fam =
                                get_link_tree_curl conf conf.request conf.bname
                                  bname_link ip s s2 0 1 (nb_desc + gen)
                              in
                              let corr =
                                let from_ref = redis_p_key base ip in
                                MLink.Connection.({
                                  from_baseprefix = conf.bname;
                                  from_ref = from_ref;
                                  to_baseprefix = bname_link;
                                  to_ref = s;
                                })
                              in
                              let (accu_fam, accu_pers, accu_conn) =
                                (fam.MLink.Link_tree.families @ accu_fam,
                                 fam.MLink.Link_tree.persons @ accu_pers,
                                 corr :: fam.MLink.Link_tree.connections @ accu_conn)
                              in
                              (accu_fam, accu_pers, accu_conn)
                          | _ -> (accu_fam, accu_pers, accu_conn)
                        end
                    | _ -> (accu_fam, accu_pers, accu_conn))
                  (accu_fam, accu_pers, accu_conn) (json_list_of_string s))
              (accu_fam, accu_pers, accu_conn) links
           end)
      ([], [], []) pl
  in

  (* Ascendance distante. *)
  let distant_asc_fam =
    if nb_asc > 0 && ip_local <> Adef.iper_of_int (-1) then
      let rec loop parents persons =
        match parents with
        | [] -> persons
        | (ip, gen) :: parents ->
            if gen = nb_asc then loop parents persons
            else
            match get_parents (poi base ip) with
            | Some ifam ->
                let cpl = foi base ifam in
                let ifath = get_father cpl in
                let imoth = get_mother cpl in
                loop ((ifath, gen + 1) :: (imoth, gen + 1) :: parents) persons
            | None -> loop parents ((ip, gen) :: persons)
      in
      let persons = loop [(ip_local, 0)] [] in
      List.fold_left
        (fun (accu_fam, accu_pers, accu_conn) (ip, gen) ->
           let links = get_links conf base redis ip include_not_validated in
           List.fold_left
             (fun (accu_fam, accu_pers, accu_conn) s ->
                List.fold_left
                  (fun (accu_fam, accu_pers, accu_conn) x ->
                     match String.split_on_char ':' x with
                     | [_; bname_link; id_link; "parents"] ->
                         let pl =
                           findKeyBySourcenameAndIdGlinks redis bname_link
                             (RC.FloatBound.Inclusive (float_of_string id_link))
                         in
                         begin
                           match pl with
                           | [s] ->
                               let fam =
                                 get_link_tree_curl conf conf.request conf.bname bname_link
                                   ip s "" (nb_asc - gen) (from_gen_desc - gen)
                                   (if conf.bname <> basename then gen + nb_desc else gen)
                               in
                               let corr =
                                 let from_ref = redis_p_key base ip in
                                 MLink.Connection.({
                                   from_baseprefix = conf.bname;
                                   from_ref = from_ref;
                                   to_baseprefix = bname_link;
                                   to_ref = s;
                                 })
                               in
                               (fam.MLink.Link_tree.families @ accu_fam,
                                fam.MLink.Link_tree.persons @ accu_pers,
                                corr :: fam.MLink.Link_tree.connections @ accu_conn)
                           | _ -> (accu_fam, accu_pers, accu_conn)
                         end
                     | _ -> (accu_fam, accu_pers, accu_conn))
                  (accu_fam, accu_pers, accu_conn) (json_list_of_string s))
             (accu_fam, accu_pers, accu_conn) links)
        ([], [], []) persons
    else ([], [], [])
  in

  let (distant_asc_fam, distant_asc_pers, distant_asc_conn) = distant_asc_fam in
  let (distant_desc_fam, distant_desc_pers, distant_desc_conn) = distant_desc_fam in

  let families = local_families @ distant_asc_fam @ distant_desc_fam in
  let persons = local_persons @ distant_asc_pers @ distant_desc_pers in
  let connections = local_connections @ distant_asc_conn @ distant_desc_conn in

  let data =
    MLink.Link_tree.({
      families = families;
      persons = persons;
      connections = connections;
    })
  in
  let data = MLinkext.gen_link_tree data in
  Api_util.print_result conf data

#endif
