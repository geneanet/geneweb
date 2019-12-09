(* Copyright (c) 1998-2007 INRIA *)

#ifdef API
module MLink = Api_link_tree_piqi
#endif

open Config
open Def
open Gwdb
open TemplAst
open Util

let max_im_wid = 240
let round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0

let has_children base u =
  Array.exists
    (fun ifam ->
       let des = foi base ifam in Array.length (get_children des) > 0)
    (get_family u)

let string_of_marriage_text conf base fam =
  let marriage = Adef.od_of_cdate (get_marriage fam) in
  let marriage_place = sou base (get_marriage_place fam) in
  let s =
    match marriage with
      Some d -> " " ^ DateDisplay.string_of_ondate conf d
    | _ -> ""
  in
  match marriage_place with
    "" -> s
  | _ -> s ^ ", " ^ Util.safe_html (string_with_macros conf [] marriage_place) ^ ","

let string_of_title conf base and_txt p (nth, name, title, places, dates) =
  let href =
    "m=TT&sm=S&t=" ^ code_varenv (sou base title) ^ "&p=" ^
    code_varenv (sou base (List.hd places))
  in
  let (tit, est) = sou base title, sou base (List.hd places) in
  let s = tit ^ " " ^ est in
  let b = Buffer.create 50 in
  Buffer.add_string b (geneweb_link conf href s);
  let rec loop places =
    begin match places with
      [] -> ()
    | [_] -> Printf.bprintf b "\n%s " and_txt
    | _ -> Buffer.add_string b ",\n"
    end;
    match places with
      place :: places ->
        let href =
          "m=TT&sm=S&t=" ^ code_varenv (sou base title) ^ "&p=" ^
          code_varenv (sou base place)
        in
        let est = sou base place in
        Buffer.add_string b (geneweb_link conf href est); loop places
    | _ -> ()
  in
  loop (List.tl places);
  let paren =
    match nth, dates, name with
      n, _, _ when n > 0 -> true
    | _, _, Tname _ -> true
    | _, (Some _, _) :: _, _ -> authorized_age conf base p
    | _ -> false
  in
  if paren then Buffer.add_string b "\n(";
  let first =
    if nth > 0 then
      begin
        Buffer.add_string b
          (if nth >= 100 then string_of_int nth
           else transl_nth conf "nth" nth);
        false
      end
    else true
  in
  let first =
    match name with
      Tname n ->
        if not first then Buffer.add_string b " ,";
        Buffer.add_string b (sou base n);
        false
    | _ -> first
  in
  if authorized_age conf base p && dates <> [None, None] then
    begin let _ =
      List.fold_left
        (fun first (date_start, date_end) ->
           if not first then Buffer.add_string b ",\n";
           begin match date_start with
             Some d -> Buffer.add_string b (DateDisplay.string_of_date conf d)
           | None -> ()
           end;
           begin match date_end with
             Some (Dgreg (d, _)) ->
               if d.month <> 0 then Buffer.add_string b " - "
               else Buffer.add_string b "-"
           | Some (Dtext _) -> Buffer.add_string b " - "
           | _ -> ()
           end;
           begin match date_end with
             Some d -> Buffer.add_string b (DateDisplay.string_of_date conf d)
           | None -> ()
           end;
           false)
        first dates
    in
      ()
    end;
  if paren then Buffer.add_string b ")";
  Buffer.contents b

let name_equiv n1 n2 =
  Futil.eq_title_names eq_istr n1 n2 || n1 = Tmain && n2 = Tnone ||
  n1 = Tnone && n2 = Tmain

let nobility_titles_list conf base p =
  let titles =
    List.fold_right
      (fun t l ->
         let t_date_start = Adef.od_of_cdate t.t_date_start in
         let t_date_end = Adef.od_of_cdate t.t_date_end in
         match l with
           (nth, name, title, place, dates) :: rl
           when
             not conf.is_rtl && nth = t.t_nth && name_equiv name t.t_name &&
             eq_istr title t.t_ident && eq_istr place t.t_place ->
             (nth, name, title, place, (t_date_start, t_date_end) :: dates) ::
             rl
         | _ ->
             (t.t_nth, t.t_name, t.t_ident, t.t_place,
              [t_date_start, t_date_end]) ::
             l)
      (Util.nobtit conf base p) []
  in
  List.fold_right
    (fun (t_nth, t_name, t_ident, t_place, t_dates) l ->
       match l with
         (nth, name, title, places, dates) :: rl
         when
           not conf.is_rtl && nth = t_nth && name_equiv name t_name &&
           eq_istr title t_ident && dates = t_dates ->
           (nth, name, title, t_place :: places, dates) :: rl
       | _ -> (t_nth, t_name, t_ident, [t_place], t_dates) :: l)
    titles []

let print_base_loop conf base p =
  Wserver.printf
    (fcapitale (ftransl conf "loop in database: %s is his/her own ancestor"))
    (Util.update_family_loop conf base p (Gutil.designation base p));
  Wserver.printf ".\n";
  Hutil.trailer conf;
  exit 2

(* Optimisation de find_sosa_aux :                                           *)
(* - ajout d'un cache pour conserver les descendants du sosa que l'on calcul *)
(* - on sauvegarde la dernière génération où l'on a arrêté le calcul pour    *)
(*   ne pas reprendre le calcul depuis la racine                             *)

(* Type pour ne pas créer à chaque fois un tableau tstab et mark *)
type sosa_t =
  { tstab : (iper, int) Gwdb.Marker.t;
    mark : (iper, bool) Gwdb.Marker.t;
    mutable last_zil : (iper * Sosa.t) list;
    sosa_ht : (iper, (Sosa.t * Gwdb.person) option) Hashtbl.t }

let init_sosa_t conf base sosa_ref =
  let tstab =
    try Util.create_topological_sort conf base with
      Consang.TopologicalSortError p ->
        let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "error")) in
        Hutil.rheader conf title; print_base_loop conf base p
  in
  let persons = Gwdb.ipers base in
  let mark = Gwdb.iper_marker persons false in
  let last_zil = [get_iper sosa_ref, Sosa.one] in
  let sosa_ht = Hashtbl.create 5003 in
  let () =
    Hashtbl.add sosa_ht (get_iper sosa_ref) (Some (Sosa.one, sosa_ref))
  in
  {tstab = tstab; mark = mark; last_zil = last_zil; sosa_ht = sosa_ht}

let find_sosa_aux conf base a p t_sosa =
  let cache = ref [] in
  let has_ignore = ref false in
  let ht_add ht k v new_sosa =
    match try Hashtbl.find ht k with Not_found -> v with
      Some (z, _) -> if not (Sosa.gt new_sosa z) then Hashtbl.replace ht k v
    | _ -> ()
  in
  let rec gene_find =
    function
      [] -> Left []
    | (ip, z) :: zil ->
        let _ = cache := (ip, z) :: !cache in
        if ip = get_iper a then Right z
        else if Gwdb.Marker.get t_sosa.mark ip then gene_find zil
        else
          begin
            Gwdb.Marker.set t_sosa.mark ip true;
            if Gwdb.Marker.get t_sosa.tstab (get_iper a)
               <= Gwdb.Marker.get t_sosa.tstab ip
            then
              let _ = has_ignore := true in gene_find zil
            else
              let asc = pget conf base ip in
              match get_parents asc with
                Some ifam ->
                  let cpl = foi base ifam in
                  let z = Sosa.twice z in
                  begin match gene_find zil with
                    Left zil ->
                      Left
                        ((get_father cpl, z) ::
                         (get_mother cpl, Sosa.inc z 1) :: zil)
                  | Right z -> Right z
                  end
              | None -> gene_find zil
          end
  in
  let rec find zil =
    match
      try gene_find zil with
        Invalid_argument msg when msg = "index out of bounds" ->
          Update.delete_topological_sort conf base; Left []
    with
      Left [] ->
        let _ =
          List.iter
            (fun (ip, _) -> Gwdb.Marker.set t_sosa.mark ip false)
            !cache
        in
        None
    | Left zil ->
        let _ =
          if !has_ignore then ()
          else
            begin
              List.iter
                (fun (ip, z) -> ht_add t_sosa.sosa_ht ip (Some (z, p)) z) zil;
              t_sosa.last_zil <- zil
            end
        in
        find zil
    | Right z ->
        let _ =
          List.iter
            (fun (ip, _) -> Gwdb.Marker.set t_sosa.mark ip false)
            !cache
        in
        Some (z, p)
  in
  find t_sosa.last_zil

let find_sosa conf base a sosa_ref_l t_sosa =
  match Lazy.force sosa_ref_l with
    Some p ->
      if get_iper a = get_iper p then Some (Sosa.one, p)
      else
        let u = pget conf base (get_iper a) in
        if has_children base u then
          try Hashtbl.find t_sosa.sosa_ht (get_iper a) with
            Not_found -> find_sosa_aux conf base a p t_sosa
        else None
  | None -> None

(* [Type]: (iper, Sosa.t) Hashtbl.t *)
let sosa_ht = Hashtbl.create 5003

(* ************************************************************************ *)
(*  [Fonc] build_sosa_tree_ht : config -> base -> person -> unit            *)
(** [Description] : Construit à partir d'une personne la base, la
      liste de tous ses ancêtres directs et la stocke dans une hashtbl. La
      clé de la table est l'iper de la personne et on lui associe son numéro
      de sosa. Les sosa multiples ne sont représentés qu'une seule fois par
      leur plus petit numéro sosa.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let build_sosa_tree_ht conf base person =
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let nb_persons = nb_of_persons base in
  let ipers = Gwdb.ipers base in
  let mark = Gwdb.iper_marker ipers false in
  (* Tableau qui va socker au fur et à mesure les ancêtres du person. *)
  (* Attention, on créé un tableau de la longueur de la base + 1 car on *)
  (* commence à l'indice 1 !                                            *)
  let sosa_accu =
    Array.make (nb_persons + 1) (Sosa.zero, dummy_iper)
  in
  let () = Array.set sosa_accu 1 (Sosa.one, get_iper person) in
  let rec loop i len =
    if i > nb_persons then ()
    else
      let (sosa_num, ip) = Array.get sosa_accu i in
      (* Si la personne courante n'a pas de numéro de sosa, alors il n'y *)
      (* a plus d'ancêtres car ils ont été ajoutés par ordre croissant.  *)
      if Sosa.eq sosa_num Sosa.zero then ()
      else
        begin
          Hashtbl.add sosa_ht ip sosa_num;
          let asc = pget conf base ip in
          (* Ajoute les nouveaux ascendants au tableau des ancêtres. *)
          match get_parents asc with
            Some ifam ->
              let cpl = foi base ifam in
              let z = Sosa.twice sosa_num in
              let len =
                if not @@ Gwdb.Marker.get mark (get_father cpl) then
                  begin
                    Array.set sosa_accu (len + 1) (z, get_father cpl);
                    Gwdb.Marker.set mark (get_father cpl) true;
                    len + 1
                  end
                else len
              in
              let len =
                if not @@ Gwdb.Marker.get mark (get_mother cpl) then
                  begin
                    Array.set sosa_accu (len + 1) (Sosa.inc z 1, get_mother cpl);
                    Gwdb.Marker.set mark (get_mother cpl) true ;
                    len + 1
                  end
                else len
              in
              loop (i + 1) len
          | None -> loop (i + 1) len
        end
  in
  loop 1 1

(* ************************************************************************ *)
(*  [Fonc] build_sosa_ht : config -> base -> unit                           *)
(** [Description] : Fait appel à la construction de la
      liste de tous les ancêtres directs de la souche de l'arbre
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let build_sosa_ht conf base =
  match Util.find_sosa_ref conf base with
    Some sosa_ref -> build_sosa_tree_ht conf base sosa_ref
  | None -> ()

(* ******************************************************************** *)
(*  [Fonc] next_sosa : Sosa.t -> Sosa.t               *)
(** [Description] : Recherche le sosa suivant
    [Args] :
      - s    : sosa
    [Retour] :
      - Sosa.t : retourne Sosa.zero s'il n'y a pas de sosa suivant      *)
(* ******************************************************************** *)
let next_sosa s =
  (* La clé de la table est l'iper de la personne et on lui associe son numéro
    de sosa. On inverse pour trier sur les sosa *)
  let sosa_list = Hashtbl.fold (fun k v acc -> (v, k) :: acc) sosa_ht [] in
  let sosa_list = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) sosa_list in
  let rec find_n x lst = match lst with
    | [] -> (Sosa.zero, dummy_iper)
    | (so, _) :: tl ->
        if (Sosa.eq so x) then
          if tl = [] then (Sosa.zero, dummy_iper) else List.hd tl
        else find_n x tl
  in
  let (so, ip) = find_n s sosa_list in
  (so, ip)

let prev_sosa s =
  let sosa_list = Hashtbl.fold (fun k v acc -> (v, k) :: acc) sosa_ht [] in
  let sosa_list = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) sosa_list in
  let sosa_list = List.rev sosa_list in
  let rec find_n x lst = match lst with
    | [] -> (Sosa.zero, dummy_iper)
    | (so, _) :: tl ->
        if (Sosa.eq so x) then
          if tl = [] then (Sosa.zero, dummy_iper) else List.hd tl
        else find_n x tl
  in
  let (so, ip) = find_n s sosa_list in
  (so, ip)



(* ******************************************************************** *)
(*  [Fonc] get_sosa_person : config -> person -> Sosa.t          *)
(** [Description] : Recherche si la personne passée en argument a un
                    numéro de sosa.
    [Args] :
      - p    : personne dont on cherche si elle a un numéro sosa
    [Retour] :
      - Sosa.t : retourne Sosa.zero si la personne n'a pas de numéro de
                sosa, ou retourne son numéro de sosa sinon
    [Rem] : Exporté en clair hors de ce module.                         *)
(* ******************************************************************** *)
let get_sosa_person p =
  try Hashtbl.find sosa_ht (get_iper p) with Not_found -> Sosa.zero

(* ********************************************************************** *)
(*  [Fonc] has_history : config -> string -> bool                         *)
(** [Description] : Indique si l'individu a été modifiée.
    [Args] :
      - conf   : configuration de la base
      - base   : arbre
      - p      : person
      - p_auth : indique si l'utilisateur est authentifié
    [Retour] : Vrai si la personne a été modifiée, Faux sinon.
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let has_history conf base p p_auth =
  let fn = sou base (get_first_name p) in
  let sn = sou base (get_surname p) in
  let occ = get_occ p in
  let person_file = HistoryDiff.history_file fn sn occ in
  p_auth && Sys.file_exists (HistoryDiff.history_path conf person_file)

(* ******************************************************************** *)
(*  [Fonc] get_single_sosa : config -> base -> person -> Sosa.t          *)
(** [Description] : Recherche si la personne passée en argument a un
                    numéro de sosa.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : personne dont on cherche si elle a un numéro sosa
    [Retour] :
      - Sosa.t : retourne Sosa.zero si la personne n'a pas de numéro de
                sosa, ou retourne son numéro de sosa sinon
    [Rem] : Exporté en clair hors de ce module.                         *)
(* ******************************************************************** *)
let get_single_sosa conf base p =
  let sosa_ref = Util.find_sosa_ref conf base in
  match sosa_ref with
    Some p_sosa ->
      let sosa_ref_l = let sosa_ref () = sosa_ref in Lazy.from_fun sosa_ref in
      let t_sosa = init_sosa_t conf base p_sosa in
      begin match find_sosa conf base p sosa_ref_l t_sosa with
        Some (z, _) -> z
      | None -> Sosa.zero
      end
  | None -> Sosa.zero


(* ************************************************************************ *)
(*  [Fonc] print_sosa : config -> base -> person -> bool -> unit            *)
(** [Description] : Affiche le picto sosa ainsi que le lien de calcul de
      relation entre la personne et le sosa 1 (si l'option cancel_link
      n'est pas activée).
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : la personne que l'on veut afficher
      - link : ce booléen permet d'afficher ou non le lien sur le picto
               sosa. Il n'est pas nécessaire de mettre le lien si on a
               déjà affiché cette personne.
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let print_sosa conf base p link =
  let sosa_num = get_sosa_person p in
  if Sosa.gt sosa_num Sosa.zero then
    match Util.find_sosa_ref conf base with
      Some ref ->
        if conf.cancel_links || not link then ()
        else
          begin let sosa_link =
            let i1 = string_of_iper (get_iper p) in
            let i2 = string_of_iper (get_iper ref) in
            let b2 = Sosa.to_string sosa_num in
            "m=RL&i1=" ^ i1 ^ "&i2=" ^ i2 ^ "&b1=1&b2=" ^ b2
          in
            Wserver.printf "<a href=\"%s%s\" style=\"text-decoration:none\">"
              (commd conf) sosa_link
          end;
        let title =
          if is_hide_names conf ref && not (authorized_age conf base ref) then
            ""
          else
            let direct_ancestor =
              Name.strip_c (p_first_name base ref) '"' ^ " " ^
              Name.strip_c (p_surname base ref) '"'
            in
            Printf.sprintf (fcapitale (ftransl conf "direct ancestor of %s"))
              direct_ancestor ^
            Printf.sprintf ", Sosa: %s"
              (Sosa.to_string_sep (transl conf "(thousand separator)") sosa_num)
        in
        Wserver.printf "<img src=\"%s/sosa.png\" alt=\"sosa\" title=\"%s\"/> "
          (image_prefix conf) title;
        if conf.cancel_links || not link then () else Wserver.printf "</a> "
    | None -> ()

(* ************************************************************************ *)
(*  [Fonc] get_death_text : config -> person -> bool -> string      *)
(** [Description] : Retourne une description de la mort de la personne
    [Args] :
      - conf : configuration de la base
      - p    : la personne que l'on veut afficher
      - p_auth : authentifié ou non
    [Retour] :
      - string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let get_death_text conf p p_auth =
  let died =
    if p_auth then
      let is = index_of_sex (get_sex p) in
      match get_death p with
        Death (dr, _) ->
          begin match dr with
            Unspecified -> transl_nth conf "died" is
          | Murdered -> transl_nth conf "murdered" is
          | Killed -> transl_nth conf "killed (in action)" is
          | Executed -> transl_nth conf "executed (legally killed)" is
          | Disappeared -> transl_nth conf "disappeared" is
          end
      | DeadYoung -> transl_nth conf "died young" is
      | DeadDontKnowWhen -> transl_nth conf "died" is
      | _ -> ""
    else ""
  in
  let on_death_date =
    let tmp_conf = {conf with cancel_links = true} in
    match p_auth, get_death p with
      true, Death (_, d) ->
        let d = Adef.date_of_cdate d in
        begin match p_getenv conf.base_env "long_date" with
          Some "yes" ->
            DateDisplay.string_of_ondate tmp_conf d ^ DateDisplay.get_wday tmp_conf d
        | _ -> DateDisplay.string_of_ondate tmp_conf d
        end
    | _ -> ""
  in
  died ^ " " ^ on_death_date

(* ************************************************************************ *)
(*  [Fonc] get_baptism_text : config -> base -> person -> bool -> string    *)
(** [Description] : Retourne le texte sur le baptême de la personne
    [Args] :
      - conf : configuration de la base
      - p    : la personne que l'on veut afficher
      - p_auth : authentifié ou non
    [Retour] :
      - string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let get_baptism_text conf p p_auth =
  let baptized =
    if p_auth then
      let is = index_of_sex (get_sex p) in transl_nth conf "baptized" is
    else ""
  in
  let on_baptism_date =
    let tmp_conf = {conf with cancel_links = true} in
    match p_auth, Adef.od_of_cdate (get_baptism p) with
      true, Some d ->
        begin match p_getenv conf.base_env "long_date" with
          Some "yes" ->
            DateDisplay.string_of_ondate tmp_conf d ^ DateDisplay.get_wday tmp_conf d
        | _ -> DateDisplay.string_of_ondate tmp_conf d
        end
    | _ -> ""
  in
  baptized ^ " " ^ on_baptism_date

(* ************************************************************************ *)
(*  [Fonc] get_birth_text : config -> person -> bool -> string    *)
(** [Description] : Retourne le texte sur la naissance de la personne
    [Args] :
      - conf : configuration de la base
      - p    : la personne que l'on veut afficher
      - p_auth : authentifié ou non
    [Retour] :
      - string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let get_birth_text conf p p_auth =
  let born =
    if p_auth then
      let is = index_of_sex (get_sex p) in transl_nth conf "born" is
    else ""
  in
  let on_birth_date =
    let tmp_conf = {conf with cancel_links = true} in
    match p_auth, Adef.od_of_cdate (get_birth p) with
      true, Some d ->
        begin match p_getenv conf.base_env "long_date" with
          Some "yes" ->
            DateDisplay.string_of_ondate tmp_conf d ^ DateDisplay.get_wday tmp_conf d
        | _ -> DateDisplay.string_of_ondate tmp_conf d
        end
    | _ -> ""
  in
  born ^ " " ^ on_birth_date

(* ************************************************************************ *)
(*  [Fonc] get_marriage_text : config -> fam -> bool -> string      *)
(** [Description] : Retourne le texte sur la date de l'union.
    [Args] :
      - conf   : configuration de la base
      - family : la famille concernée
      - p_auth : authentifié ou non
    [Retour] :
      - string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let get_marriage_date_text conf fam p_auth =
  let tmp_conf = {conf with cancel_links = true} in
  match p_auth, Adef.od_of_cdate (get_marriage fam) with
    true, Some d ->
      begin match p_getenv conf.base_env "long_date" with
        Some "yes" ->
          DateDisplay.string_of_ondate tmp_conf d ^ DateDisplay.get_wday tmp_conf d
      | _ -> DateDisplay.string_of_ondate tmp_conf d
      end
  | _ -> ""

(* ************************************************************************ *)
(*  [Fonc] get_burial_text : config -> person -> bool -> string     *)
(** [Description] : Retourne le texte sur l'incinération de la personne
    [Args] :
      - conf : configuration de la base
      - p    : la personne que l'on veut afficher
      - p_auth : authentifié ou non
    [Retour] :
      - string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let get_burial_text conf p p_auth =
  let buried =
    if p_auth then
      let is = index_of_sex (get_sex p) in transl_nth conf "buried" is
    else ""
  in
  let on_burial_date =
    let tmp_conf = {conf with cancel_links = true} in
    match get_burial p with
      Buried cod ->
        begin match p_auth, Adef.od_of_cdate cod with
          true, Some d ->
            begin match p_getenv conf.base_env "long_date" with
              Some "yes" ->
                DateDisplay.string_of_ondate tmp_conf d ^ DateDisplay.get_wday tmp_conf d
            | _ -> DateDisplay.string_of_ondate tmp_conf d
            end
        | _ -> ""
        end
    | _ -> ""
  in
  buried ^ " " ^ on_burial_date

(* ************************************************************************ *)
(*  [Fonc] get_burial_text : config -> person -> bool -> string     *)
(** [Description] : Retourne le texte sur l'incinération de la personne
    [Args] :
      - conf : configuration de la base
      - p    : la personne que l'on veut afficher
      - p_auth : authentifié ou non
    [Retour] :
      - string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let get_cremation_text conf p p_auth =
  let cremated =
    if p_auth then
      let is = index_of_sex (get_sex p) in transl_nth conf "cremated" is
    else ""
  in
  let on_cremation_date =
    let tmp_conf = {conf with cancel_links = true} in
    match get_burial p with
      Cremated cod ->
        begin match p_auth, Adef.od_of_cdate cod with
          true, Some d ->
            begin match p_getenv conf.base_env "long_date" with
              Some "yes" ->
                DateDisplay.string_of_ondate tmp_conf d ^ DateDisplay.get_wday tmp_conf d
            | _ -> DateDisplay.string_of_ondate tmp_conf d
            end
        | _ -> ""
        end
    | _ -> ""
  in
  cremated ^ " " ^ on_cremation_date

let max_ancestor_level conf base ip max_lev =
  let x = ref 0 in
  let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
#ifdef API
  (* Charge le cache des LIA. *)
  (* On limite à 10 le nombre de générations ascendantes à charger pour les LIA. *)
  let () = Perso_link.init_cache conf base ip 10 0 0 in
#endif
  let rec loop level ip =
    (* Ne traite pas l'index s'il a déjà été traité. *)
    (* Pose surement probleme pour des implexes. *)
    if not @@ Gwdb.Marker.get mark ip then begin
        (* Met à jour le tableau d'index pour indiquer que l'index est traité. *)
        Gwdb.Marker.set mark ip true ;
        x := max !x level;
        if !x <> max_lev then
          match get_parents (pget conf base ip) with
          | Some ifam ->
              let cpl = foi base ifam in
              loop (succ level) (get_father cpl);
              loop (succ level) (get_mother cpl)
          | _ ->
#ifdef API
              (* lia *)
              let rec loop_lia level (ip, base_prefix) =
                x := max !x level;
                if !x = max_lev then ()
                else
                  match Perso_link.get_parents_link base_prefix ip with
                    Some family ->
                      let (ifath, imoth, base_prefix) =
                        Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.ifath,
                        Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.imoth,
                        family.MLink.Family.baseprefix
                      in
                      if Perso_link.get_person_link base_prefix ifath <> None
                      then loop_lia (succ level) (ifath, base_prefix) ;
                      if Perso_link.get_person_link base_prefix imoth <> None
                      then loop_lia (succ level) (imoth, base_prefix)
                  | None -> ()
              in
              loop_lia level (ip, conf.bname)
#else
 ()
#endif
      end
  in
  loop 0 ip; !x

let default_max_cousin_lev = 5

let max_cousin_level conf base p =
  let max_lev =
    try int_of_string (List.assoc "max_cousins_level" conf.base_env) with
      Not_found | Failure _ -> default_max_cousin_lev
  in
  max_ancestor_level conf base (get_iper p) max_lev + 1

let limit_desc conf =
  match p_getint conf.base_env "max_desc_level" with
    Some x -> max 1 x
  | None -> 12

let infinite = 10000

let make_desc_level_table conf base max_level p =
  let line =
    match p_getenv conf.env "t" with
      Some "M" -> Male
    | Some "F" -> Female
    | Some _ | None -> Neuter
  in
  (* the table 'levt' may be not necessary, since I added 'flevt'; kept
     because '%max_desc_level;' is still used... *)
  let levt = Gwdb.iper_marker (Gwdb.ipers base) infinite in
  let flevt = Gwdb.ifam_marker (Gwdb.ifams base) infinite in
  let get = pget conf base in
  let ini_ip = get_iper p in
  let rec fill lev =
    function
      [] -> ()
    | ipl ->
        let new_ipl =
          List.fold_left
            (fun ipl ip ->
               if Gwdb.Marker.get levt ip <= lev then ipl
               else if lev <= max_level then
                 begin
                   Gwdb.Marker.set levt ip lev ;
                   let down =
                     if ip = ini_ip then true
                     else
                       match line with
                       | Male -> get_sex (pget conf base ip) <> Female
                       | Female -> get_sex (pget conf base ip) <> Male
                       | Neuter -> true
                   in
                   if down then
                     Array.fold_left
                       (fun ipl ifam ->
                          if not (Gwdb.Marker.get flevt ifam <= lev)
                          then Gwdb.Marker.set flevt ifam lev ;
                          let ipa = get_children (foi base ifam) in
                          Array.fold_left (fun ipl ip -> ip :: ipl) ipl ipa)
                       ipl (get_family (get ip))
                   else ipl
                 end
               else ipl)
            [] ipl
        in
        fill (succ lev) new_ipl
  in
  fill 0 [ini_ip];
  levt, flevt

let desc_level_max base desc_level_table_l =
  let (levt, _) = Lazy.force desc_level_table_l in
  Gwdb.Collection.fold (fun acc i ->
      let lev = Gwdb.Marker.get levt i in
      if lev != infinite && acc < lev then lev else acc
    ) 0 (Gwdb.ipers base)

let max_descendant_level base desc_level_table_l =
  desc_level_max base desc_level_table_l

(* ancestors by list *)

type generation_person =
    GP_person of Sosa.t * iper * ifam option
  | GP_same of Sosa.t * Sosa.t * iper
  | GP_interv of (Sosa.t * Sosa.t * (Sosa.t * Sosa.t) option) option
  | GP_missing of Sosa.t * iper

let next_generation conf base mark gpl =
  let gpl =
    List.fold_right
      (fun gp gpl ->
         match gp with
           GP_person (n, ip, _) ->
             let n_fath = Sosa.twice n in
             let n_moth = Sosa.inc n_fath 1 in
             let a = pget conf base ip in
             begin match get_parents a with
               Some ifam ->
                 let cpl = foi base ifam in
                 GP_person (n_fath, get_father cpl, Some ifam) ::
                 GP_person (n_moth, get_mother cpl, Some ifam) :: gpl
             | None -> GP_missing (n, ip) :: gpl
             end
         | GP_interv None -> gp :: gpl
         | GP_interv (Some (n1, n2, x)) ->
             let x =
               match x with
                 Some (m1, m2) -> Some (Sosa.twice m1, Sosa.twice m2)
               | None -> None
             in
             let gp = GP_interv (Some (Sosa.twice n1, Sosa.twice n2, x)) in
             gp :: gpl
         | _ -> gpl)
      gpl []
  in
  let gpl =
    List.fold_left
      (fun gpl gp ->
         match gp with
           GP_person (n, ip, _) ->
             let m = Gwdb.Marker.get mark ip in
             if Sosa.eq m Sosa.zero then begin Gwdb.Marker.set mark ip n; gp :: gpl end
             else GP_same (n, m, ip) :: gpl
         | _ -> gp :: gpl)
      [] gpl
  in
  List.rev gpl

let next_generation2 conf base mark gpl =
  let gpl =
    List.map
      (fun gp ->
         match gp with
           GP_same (n, m, _) ->
             GP_interv (Some (n, Sosa.inc n 1, Some (m, Sosa.inc m 1)))
         | _ -> gp)
      gpl
  in
  let gpl = next_generation conf base mark gpl in
  List.fold_right
    (fun gp gpl ->
       match gp, gpl with
         GP_interv (Some (n1, n2, x)), GP_interv (Some (n3, n4, y)) :: gpl1 ->
           if Sosa.eq n2 n3 then
             let z =
               match x, y with
                 Some (m1, m2), Some (m3, m4) ->
                   if Sosa.eq m2 m3 then Some (m1, m4) else None
               | _ -> None
             in
             GP_interv (Some (n1, n4, z)) :: gpl1
           else GP_interv None :: gpl1
       | GP_interv _, GP_interv _ :: gpl -> GP_interv None :: gpl
       | GP_missing (_, _), gpl -> gpl
       | _ -> gp :: gpl)
    gpl []

let sosa_is_present all_gp n1 =
  let rec loop =
    function
      GP_person (n, _, _) :: gpl | GP_same (n, _, _) :: gpl ->
        if Sosa.eq n n1 then true else loop gpl
    | _ :: gpl -> loop gpl
    | [] -> false
  in
  loop all_gp

let get_link all_gp ip =
  let rec loop =
    function
      (GP_person (_, ip0, _) as gp) :: gpl ->
        if ip = ip0 then Some gp else loop gpl
    | _ :: gpl -> loop gpl
    | [] -> None
  in
  loop all_gp

let parent_sosa conf base ip all_gp n parent =
  if sosa_is_present all_gp n then Sosa.to_string n
  else
    match get_parents (pget conf base ip) with
      Some ifam ->
        begin match get_link all_gp (parent (foi base ifam)) with
          Some (GP_person (n, _, _)) -> Sosa.to_string n
        | _ -> ""
        end
    | None -> ""

let will_print =
  function
    GP_person (_, _, _) -> true
  | GP_same (_, _, _) -> true
  | _ -> false

let get_all_generations conf base p =
  let max_level =
    match p_getint conf.env "v" with
      Some v -> v
    | None -> 0
  in
  let mark = Gwdb.iper_marker (Gwdb.ipers base) Sosa.zero in
  let rec get_generations level gpll gpl =
    let gpll = gpl :: gpll in
    if level < max_level then
      let next_gpl = next_generation conf base mark gpl in
      if List.exists will_print next_gpl then
        get_generations (level + 1) gpll next_gpl
      else gpll
    else gpll
  in
  let gpll =
    get_generations 1 [] [GP_person (Sosa.one, get_iper p, None)]
  in
  let gpll = List.rev gpll in List.flatten gpll

(* Ancestors by tree:

  8 ? ? ? ? ? ? ?
   4   5   ?   7
     2       3
         1

1) Build list of levels (t1 = True for parents flag, size 1)
   => [ [8At1 E E] [4Lt1 5Rt1 7At1] [2Lt1 3Rt1] [1Ct1] ]

2) Enrich list of levels (parents flag, sizing)
   => [ [8At1 E E] [4Lt1 5Rf1 7Af1] [2Lt3 3Rt1] [1Ct5] ]

3) Display it
    For each cell:
      Top vertical bar if parents flag (not on top line)
      Person
      Person tree link (vertical bar) ) not on bottom line
      Horizontal line                 )

*)

type pos = Left | Right | Center | Alone
type cell =
    Cell of person * ifam option * pos * bool * int * string
  | Empty

let rec enrich lst1 lst2 =
  match lst1, lst2 with
    _, [] -> []
  | [], lst -> lst
  | Cell (_, _, Right, _, s1, _) :: l1, Cell (p, f, d, u, s2, b) :: l2 ->
      Cell (p, f, d, u, s1 + s2 + 1, b) :: enrich l1 l2
  | Cell (_, _, Left, _, s, _) :: l1, Cell (p, f, d, u, _, b) :: l2 ->
      enrich l1 (Cell (p, f, d, u, s, b) :: l2)
  | Cell (_, _, _, _, s, _) :: l1, Cell (p, f, d, u, _, b) :: l2 ->
      Cell (p, f, d, u, s, b) :: enrich l1 l2
  | Empty :: l1, Cell (p, f, d, _, s, b) :: l2 ->
      Cell (p, f, d, false, s, b) :: enrich l1 l2
  | _ :: l1, Empty :: l2 -> Empty :: enrich l1 l2

let is_empty = List.for_all ((=) Empty)

let rec enrich_tree lst =
  match lst with
    [] -> []
  | head :: tail ->
      if is_empty head then enrich_tree tail
      else
        match tail with
          [] -> [head]
        | thead :: ttail -> head :: enrich_tree (enrich head thead :: ttail)

(* tree_generation_list
    conf: configuration parameters
    base: base name
    gv: number of generations
    p: person *)
let tree_generation_list conf base gv p =
  let next_gen pol =
    List.fold_right
      (fun po list ->
         match po with
           Empty -> Empty :: list
         | Cell (p, _, _, _, _, base_prefix) ->
             match get_parents p with
               Some ifam ->
                 let cpl = foi base ifam in
                 let fath =
                   let p = pget conf base (get_father cpl) in
                   if not @@ is_empty_name p then Some p else None
                 in
                 let moth =
                   let p = pget conf base (get_mother cpl) in
                   if not @@ is_empty_name p then Some p else None
                 in
                 let fo = Some ifam in
                 let base_prefix = conf.bname in
                 begin match fath, moth with
                   Some f, Some m ->
                     Cell (f, fo, Left, true, 1, base_prefix) ::
                     Cell (m, fo, Right, true, 1, base_prefix) :: list
                 | Some f, None ->
                     Cell (f, fo, Alone, true, 1, base_prefix) :: list
                 | None, Some m ->
                     Cell (m, fo, Alone, true, 1, base_prefix) :: list
                 | None, None -> Empty :: list
                 end
             | _ ->
#ifdef API
                 match
                   Perso_link.get_parents_link base_prefix (get_iper p)
                 with
                   Some family ->
                     let (ifath, imoth, ifam) =
                       Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.ifath,
                       Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.imoth,
                       Gwdb.ifam_of_string @@ Int32.to_string family.MLink.Family.ifam
                     in
                     begin match
                       Perso_link.get_person_link
                         family.MLink.Family.baseprefix ifath,
                       Perso_link.get_person_link
                         family.MLink.Family.baseprefix imoth
                     with
                       Some fath, Some moth ->
                         let (fath, _) =
                           Perso_link.make_ep_link base fath
                         in
                         let fath =
                           if not @@ is_empty_name fath then Some fath else None
                         in
                         let (moth, _) =
                           Perso_link.make_ep_link base moth
                         in
                         let moth =
                           if not @@ is_empty_name moth then Some moth else None
                         in
                         let fo = Some ifam in
                         let base_prefix = family.MLink.Family.baseprefix in
                         begin match fath, moth with
                           Some f, Some m ->
                             Cell (f, fo, Left, true, 1, base_prefix) ::
                             Cell (m, fo, Right, true, 1, base_prefix) :: list
                         | Some f, None ->
                             Cell (f, fo, Alone, true, 1, base_prefix) :: list
                         | None, Some m ->
                             Cell (m, fo, Alone, true, 1, base_prefix) :: list
                         | None, None -> Empty :: list
                         end
                     | Some fath, None ->
                         let (fath, _) =
                           Perso_link.make_ep_link base fath
                         in
                         let fath =
                           if not @@ is_empty_name fath then Some fath else None
                         in
                         let fo = Some ifam in
                         let base_prefix = family.MLink.Family.baseprefix in
                         begin match fath with
                           Some f ->
                             Cell (f, fo, Alone, true, 1, base_prefix) :: list
                         | None -> Empty :: list
                         end
                     | None, Some moth ->
                         let (moth, _) =
                           Perso_link.make_ep_link base moth
                         in
                         let moth =
                           if not @@ is_empty_name moth then Some moth else None
                         in
                         let fo = Some ifam in
                         let base_prefix = family.MLink.Family.baseprefix in
                         begin match moth with
                           Some f ->
                             Cell (f, fo, Alone, true, 1, base_prefix) :: list
                         | None -> Empty :: list
                         end
                     | None, None -> Empty :: list
                     end
                 | None -> Empty :: list
#else
               Empty :: list
#endif
)
      pol []
#ifndef API
[@@ocaml.warning "-27"]
#endif
  in
  let gen =
    let rec loop i gen list =
      if i = 0 then gen :: list else loop (i - 1) (next_gen gen) (gen :: list)
    in
    loop (gv - 1) [Cell (p, None, Center, true, 1, conf.bname)] []
  in
  enrich_tree gen

(* Ancestors surnames list *)

let get_date_place conf base auth_for_all_anc p =
  if auth_for_all_anc || authorized_age conf base p then
    let d1 =
      match Adef.od_of_cdate (get_birth p) with
        None -> Adef.od_of_cdate (get_baptism p)
      | x -> x
    in
    let d1 =
      if d1 <> None then d1
      else
        Array.fold_left
          (fun d ifam ->
             if d <> None then d
             else Adef.od_of_cdate (get_marriage (foi base ifam)))
          d1 (get_family p)
    in
    let d2 =
      match get_death p with
        Death (_, cd) -> Some (Adef.date_of_cdate cd)
      | _ ->
          match get_burial p with
            Buried cod -> Adef.od_of_cdate cod
          | Cremated cod -> Adef.od_of_cdate cod
          | _ -> None
    in
    let auth_for_all_anc =
      if auth_for_all_anc then true
      else
        match d2 with
          Some (Dgreg (d, _)) ->
            let a = Date.time_elapsed d conf.today in
            Util.strictly_after_private_years conf a
        | _ -> false
    in
    let pl =
      let pl = "" in
      let pl = if pl <> "" then pl else sou base (get_birth_place p) in
      let pl = if pl <> "" then pl else sou base (get_baptism_place p) in
      let pl = if pl <> "" then pl else sou base (get_death_place p) in
      let pl = if pl <> "" then pl else sou base (get_burial_place p) in
      if pl <> "" then pl
      else
        Array.fold_left
          (fun pl ifam ->
             if pl <> "" then pl
             else sou base (get_marriage_place (foi base ifam)))
          pl (get_family p)
    in
    (d1, d2, pl), auth_for_all_anc
  else (None, None, ""), false

(* duplications proposed for merging *)

type dup =
    DupFam of ifam * ifam
  | DupInd of iper * iper
  | NoDup
type excl_dup = (iper * iper) list * (ifam * ifam) list

let gen_excluded_possible_duplications conf s i_of_string =
  match p_getenv conf.env s with
    Some s ->
      let rec loop ipl i =
        if i >= String.length s then ipl
        else
          let j =
            try String.index_from s i ',' with Not_found -> String.length s
          in
          if j = String.length s then ipl
          else
            let k =
              try String.index_from s (j + 1) ',' with
                Not_found -> String.length s
            in
            let s1 = String.sub s i (j - i) in
            let s2 = String.sub s (j + 1) (k - j - 1) in
            let ipl =
              try (i_of_string s1, i_of_string s2) :: ipl
              with _ -> ipl
            in
            loop ipl (k + 1)
      in
      loop [] 0
  | None -> []

let excluded_possible_duplications conf =
  gen_excluded_possible_duplications conf "iexcl" iper_of_string,
  gen_excluded_possible_duplications conf "fexcl" ifam_of_string

let first_possible_duplication base ip (iexcl, fexcl) =
  let ifaml = Array.to_list (get_family (poi base ip)) in
  let cand_spouse =
    let rec loop_spouse =
      function
        ifam1 :: ifaml1 ->
          let isp1 = Gutil.spouse ip (foi base ifam1) in
          let sp1 = poi base isp1 in
          let fn1 = get_first_name sp1 in
          let sn1 = get_surname sp1 in
          let rec loop_same =
            function
              ifam2 :: ifaml2 ->
                let isp2 = Gutil.spouse ip (foi base ifam2) in
                if isp2 = isp1 then
                  if not (List.mem (ifam1, ifam2) fexcl) then
                    DupFam (ifam1, ifam2)
                  else loop_same ifaml2
                else
                  let sp2 = poi base isp2 in
                  if List.mem (isp1, isp2) iexcl then loop_same ifaml2
                  else if
                    eq_istr (get_first_name sp2) fn1 &&
                    eq_istr (get_surname sp2) sn1
                  then
                    DupInd (isp1, isp2)
                  else loop_same ifaml2
            | [] -> loop_spouse ifaml1
          in
          loop_same ifaml1
      | [] -> NoDup
    in
    loop_spouse ifaml
  in
  if cand_spouse <> NoDup then cand_spouse
  else
    let ipl =
      List.rev @@
      List.fold_left
        (fun acc x ->
           Array.fold_left
             (fun acc x -> x :: acc) acc (get_children (foi base x)))
        [] ifaml
    in
    let rec loop_chil =
      function
        ip1 :: ipl1 ->
          let p1 = poi base ip1 in
          let fn1 = get_first_name p1 in
          let rec loop_same =
            function
              ip2 :: ipl2 ->
                let p2 = poi base ip2 in
                if List.mem (ip1, ip2) iexcl then loop_same ipl2
                else if eq_istr (get_first_name p2) fn1 then DupInd (ip1, ip2)
                else loop_same ipl2
            | [] -> loop_chil ipl1
          in
          loop_same ipl1
      | [] -> NoDup
    in
    loop_chil ipl

let has_possible_duplications conf base p =
  let ip = get_iper p in
  let excl = excluded_possible_duplications conf in
  first_possible_duplication base ip excl <> NoDup

let merge_date_place conf base surn ((d1, d2, pl), auth) p =
  let ((pd1, pd2, ppl), auth) = get_date_place conf base auth p in
  let nd1 =
    if pd1 <> None then pd1
    else if eq_istr (get_surname p) surn then if pd2 <> None then pd2 else d1
    else None
  in
  let nd2 =
    if eq_istr (get_surname p) surn then
      if d2 <> None then d2
      else if d1 <> None then d1
      else if pd1 <> None then pd2
      else pd1
    else if pd2 <> None then pd2
    else if pd1 <> None then pd1
    else d1
  in
  let pl =
    if ppl <> "" then ppl else if eq_istr (get_surname p) surn then pl else ""
  in
  (nd1, nd2, pl), auth

let build_surnames_list conf base v p =
  let ht = Hashtbl.create 701 in
  let mark =
    let n =
      try int_of_string (List.assoc "max_ancestor_implex" conf.base_env)
      with _ -> 5
    in
    Gwdb.iper_marker (Gwdb.ipers base) n
  in
  let auth = conf.wizard || conf.friend in
  let add_surname sosa p surn dp =
    let r =
      try Hashtbl.find ht surn with
        Not_found -> let r = ref ((fst dp, p), []) in Hashtbl.add ht surn r; r
    in
    r := fst !r, sosa :: snd !r
  in
  let rec loop lev sosa p surn dp =
    if Gwdb.Marker.get mark (get_iper p) = 0 then ()
    else if lev = v then
      if is_hide_names conf p && not (authorized_age conf base p) then ()
      else add_surname sosa p surn dp
    else
      begin
        Gwdb.Marker.set mark
          (get_iper p) (Gwdb.Marker.get mark (get_iper p) - 1) ;
        match get_parents p with
          Some ifam ->
            let cpl = foi base ifam in
            let fath = pget conf base (get_father cpl) in
            let moth = pget conf base (get_mother cpl) in
            if not (eq_istr surn (get_surname fath)) &&
               not (eq_istr surn (get_surname moth))
            then
              add_surname sosa p surn dp;
            let sosa = Sosa.twice sosa in
            if not (is_hidden fath) then
              begin let dp1 = merge_date_place conf base surn dp fath in
                loop (lev + 1) sosa fath (get_surname fath) dp1
              end;
            let sosa = Sosa.inc sosa 1 in
            if not (is_hidden moth) then
              let dp2 = merge_date_place conf base surn dp moth in
              loop (lev + 1) sosa moth (get_surname moth) dp2
        | None -> add_surname sosa p surn dp
      end
  in
  loop 1 Sosa.one p (get_surname p) (get_date_place conf base auth p);
  let list = ref [] in
  Hashtbl.iter
    (fun i dp ->
       let surn = sou base i in
       if surn <> "?" then list := (surn, !dp) :: !list)
    ht;
  List.sort
    (fun (s1, _) (s2, _) ->
       match
         Gutil.alphabetic_order (surname_without_particle base s1) (surname_without_particle base s2)
       with
         0 ->
           Gutil.alphabetic_order (surname_particle base s1)
             (surname_particle base s2)
       | x -> x)
    !list


(* ************************************************************************* *)
(*  [Fonc] build_list_eclair :
      config -> base -> int -> person ->
        list
          (string * string * option date * option date * person * list iper) *)
(** [Description] : Construit la liste éclair des ascendants de p jusqu'à la
                    génération v.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - v    : le nombre de génération
      - p    : person
    [Retour] : (surname * place * date begin * date end * person * list iper)
    [Rem] : Exporté en clair hors de ce module.                              *)
(* ************************************************************************* *)
let build_list_eclair conf base v p =
  let ht = Hashtbl.create 701 in
  let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
  (* Fonction d'ajout dans la Hashtbl. A la clé (surname, place) on associe *)
  (* la personne (pour l'interprétation dans le template), la possible date *)
  (* de début, la possible date de fin, la liste des personnes/évènements.  *)
  (* Astuce: le nombre d'élément de la liste correspond au nombre             *)
  (* d'évènements et le nombre d'iper unique correspond au nombre d'individu. *)
  let add_surname p surn pl d =
    if not (is_empty_string pl) then
      let pl = Util.string_of_place conf (sou base pl) in
      let r =
        try Hashtbl.find ht (surn, pl) with
          Not_found ->
            let r = ref (p, None, None, []) in Hashtbl.add ht (surn, pl) r; r
      in
      (* Met la jour le binding : dates et liste des iper. *)
      r :=
        (fun p (pp, db, de, l) ->
           let db =
             match db with
               Some dd ->
                 begin match d with
                   Some d -> if Date.compare_date d dd < 0 then Some d else db
                 | None -> db
                 end
             | None -> d
           in
           let de =
             match de with
               Some dd ->
                 begin match d with
                   Some d -> if Date.compare_date d dd > 0 then Some d else de
                 | None -> de
                 end
             | None -> d
           in
           pp, db, de, get_iper p :: l)
          p !r
  in
  (* Fonction d'ajout de tous les évènements d'une personne (birth, bapt...). *)
  let add_person p surn =
    if Gwdb.Marker.get mark (get_iper p) then ()
    else
      begin
        Gwdb.Marker.set mark (get_iper p) true;
        add_surname p surn (get_birth_place p)
          (Adef.od_of_cdate (get_birth p));
        add_surname p surn (get_baptism_place p)
          (Adef.od_of_cdate (get_baptism p));
        let death =
          match get_death p with
            Death (_, cd) -> Some (Adef.date_of_cdate cd)
          | _ -> None
        in
        add_surname p surn (get_death_place p) death;
        let burial =
          match get_burial p with
            Buried cod | Cremated cod -> Adef.od_of_cdate cod
          | _ -> None
        in
        add_surname p surn (get_burial_place p) burial;
        Array.iter
          (fun ifam ->
             let fam = foi base ifam in
             add_surname p surn (get_marriage_place fam)
               (Adef.od_of_cdate (get_marriage fam)))
          (get_family p)
      end
  in
  (* Parcours les ascendants de p et les ajoute dans la Hashtbl. *)
  let rec loop lev p surn =
    if lev = v then
      if is_hide_names conf p && not (authorized_age conf base p) then ()
      else add_person p surn
    else
      begin
        add_person p surn;
        match get_parents p with
          Some ifam ->
            let cpl = foi base ifam in
            let fath = pget conf base (get_father cpl) in
            let moth = pget conf base (get_mother cpl) in
            if not (is_hidden fath) then
              loop (lev + 1) fath (get_surname fath);
            if not (is_hidden moth) then
              loop (lev + 1) moth (get_surname moth)
        | None -> ()
      end
  in
  (* Construction de la Hashtbl. *)
  loop 1 p (get_surname p);
  (* On parcours la Hashtbl, et on élimine les noms vide (=?) *)
  let list = ref [] in
  Hashtbl.iter
    (fun (istr, place) ht_val ->
       let surn = sou base istr in
       if surn <> "?" then
         let (p, db, de, pl) = (fun x -> x) !ht_val in
         list := (surn, place, db, de, p, pl) :: !list)
    ht;
  (* On trie la liste par nom, puis lieu. *)
  List.sort
    (fun (s1, pl1, _, _, _, _) (s2, pl2, _, _, _, _) ->
       match
         Gutil.alphabetic_order (surname_without_particle base s1) (surname_without_particle base s2)
       with
         0 ->
           begin match
             Gutil.alphabetic_order (surname_particle base s1)
               (surname_particle base s2)
           with
             0 -> Gutil.alphabetic_order pl1 pl2
           | x -> x
           end
       | x -> x)
    !list

let linked_page_text conf base p s key str (pg, (_, il)) =
  match pg with
    NotesLinks.PgMisc pg ->
      let list = List.map snd (List.filter (fun (k, _) -> k = key) il) in
      List.fold_right
        (fun text str ->
           try
             let (nenv, _) = Notes.read_notes base pg in
             let v =
               let v = List.assoc s nenv in
               if v = "" then raise Not_found
               else Util.nth_field v (Util.index_of_sex (get_sex p))
             in
             match text.NotesLinks.lnTxt with
               Some "" -> str
             | _ ->
                 let str1 =
                   let v =
                     let text = text.NotesLinks.lnTxt in
                     match text with
                       Some text ->
                         let rec loop i len =
                           if i = String.length text then Buff.get len
                           else if text.[i] = '*' then
                             loop (i + 1) (Buff.mstore len v)
                           else loop (i + 1) (Buff.store len text.[i])
                         in
                         loop 0 0
                     | None -> v
                   in
                   let (a, b, c) =
                     try
                       let i = String.index v '{' in
                       let j = String.index v '}' in
                       let a = String.sub v 0 i in
                       let b = String.sub v (i + 1) (j - i - 1) in
                       let c =
                         String.sub v (j + 1) (String.length v - j - 1)
                       in
                       a, b, c
                     with Not_found -> "", v, ""
                   in
                   if conf.cancel_links then Printf.sprintf "%s" b
                   else
                     Printf.sprintf
                       "%s<a href=\"%sm=NOTES&f=%s#p_%d\">%s</a>%s" a
                       (commd conf) pg text.NotesLinks.lnPos b c
                 in
                 if str = "" then str1 else str ^ ", " ^ str1
           with Not_found -> str)
        list str
  | _ -> str

let links_to_ind conf base db key =
  let list =
    List.fold_left
      (fun pgl (pg, (_, il)) ->
         let record_it =
           match pg with
             NotesLinks.PgInd ip ->
               authorized_age conf base (pget conf base ip)
           | NotesLinks.PgFam ifam ->
               authorized_age conf base (pget conf base (get_father @@ foi base ifam))
           | NotesLinks.PgNotes | NotesLinks.PgMisc _ |
             NotesLinks.PgWizard _ ->
               true
         in
         if record_it then
           List.fold_left
             (fun pgl (k, _) -> if k = key then pg :: pgl else pgl) pgl il
         else pgl)
      [] db
  in
  List.sort_uniq compare list

(* Interpretation of template file *)

let rec compare_ls sl1 sl2 =
  match sl1, sl2 with
    s1 :: sl1, s2 :: sl2 ->
      (* Je ne sais pas s'il y a des effets de bords, mais on  *)
      (* essaie de convertir s1 s2 en int pour éviter que "10" *)
      (* soit plus petit que "2". J'espère qu'on ne casse pas  *)
      (* les performances à cause du try..with.                *)
      let c =
        try Stdlib.compare (int_of_string s1) (int_of_string s2) with
          Failure _ -> Gutil.alphabetic_order s1 s2
      in
      if c = 0 then compare_ls sl1 sl2 else c
  | _ :: _, [] -> 1
  | [], _ :: _ -> -1
  | [], [] -> 0

module SortedList =
  Set.Make (struct type t = string list let compare = compare_ls end)

module IperSet =
  Set.Make (struct type t = iper let compare = compare end)

(*
   Type pour représenté soit :
     - la liste des branches patronymique
       (surname * date begin * date end * place * person * list sosa * loc)
     - la liste éclair
       (surname * place * date begin * date end * person * list iper * loc)
*)
type ancestor_surname_info =
    Branch of
      (string * date option * date option * string * person * Sosa.t list *
         loc)
  | Eclair of
      (string * string * date option * date option * person * iper list * loc)

type 'a env =
    Vallgp of generation_person list
  | Vanc of generation_person
  | Vanc_surn of ancestor_surname_info
  | Vcell of cell
  | Vcelll of cell list
  | Vcnt of int ref
  | Vdesclevtab of ((iper, int) Marker.t * (ifam, int) Marker.t) lazy_t
  | Vdmark of (iper, bool) Marker.t ref
  | Vslist of SortedList.t ref
  | Vslistlm of string list list
  | Vind of person
  | Vfam of ifam * family * (iper * iper * iper) * bool
  | Vrel of relation * person option
  | Vbool of bool
  | Vint of int
  | Vgpl of generation_person list
  | Vnldb of NotesLinks.notes_links_db
  | Vstring of string
  | Vsosa_ref of person option Lazy.t
  | Vsosa of (iper * (Sosa.t * person) option) list ref
  | Vt_sosa of sosa_t option
  | Vtitle of person * title_item
  | Vevent of person * event_item
  | Vlazyp of string option ref
  | Vlazy of 'a env Lazy.t
  | Vother of 'a
  | Vnone
and title_item =
  int * istr gen_title_name * istr * istr list *
    (date option * date option) list
and event_item =
  event_name * cdate * istr * istr * istr * (iper * witness_kind) array *
    iper option
and event_name =
    Pevent of istr gen_pers_event_name
  | Fevent of istr gen_fam_event_name

let get_env v env =
  try
    match List.assoc v env with
      Vlazy l -> Lazy.force l
    | x -> x
  with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""

let template_file = ref "perso.txt"

let warning_use_has_parents_before_parent (bp, ep) var r =
  if Sys.unix then
    begin
      Printf.eprintf "*** <W> %s" !template_file;
      Printf.eprintf ", chars %d-%d" bp ep;
      Printf.eprintf "\
: since v5.00, must test \"has_parents\" before using \"%s\"\n"
        var;
      flush stderr;
      r
    end
  else r

let obsolete_list = ref []

let obsolete (bp, ep) version var new_var r =
  if List.mem var !obsolete_list then r
  else if Sys.unix then
    begin
      Printf.eprintf "*** <W> %s, chars %d-%d:" !template_file bp ep;
      Printf.eprintf " \"%s\" obsolete since v%s%s\n" var version
        (if new_var = "" then "" else "; rather use \"" ^ new_var ^ "\"");
      flush stderr;
      obsolete_list := var :: !obsolete_list;
      r
    end
  else r


let bool_val x = VVbool x
let str_val x = VVstring x

let gen_string_of_img_sz max_wid max_hei conf base (p, p_auth) =
  if p_auth then
    let v = image_and_size conf base p (limited_image_size max_wid max_hei) in
    match v with
      Some (_, _, Some (width, height)) ->
        Format.sprintf " width=\"%d\" height=\"%d\"" width height
    | Some (_, _, None) -> Format.sprintf " height=\"%d\"" max_hei
    | None -> ""
  else ""
let string_of_image_size = gen_string_of_img_sz max_im_wid max_im_wid
let string_of_image_medium_size = gen_string_of_img_sz 160 120
let string_of_image_small_size = gen_string_of_img_sz 100 75

let get_sosa conf base env r p =
  try List.assoc (get_iper p) !r with
    Not_found ->
      let s =
        match get_env "sosa_ref" env with
          Vsosa_ref v ->
            begin match get_env "t_sosa" env with
              | Vt_sosa (Some t_sosa) -> find_sosa conf base p v t_sosa
              | _ -> None
            end
        | _ -> None
      in
      r := (get_iper p, s) :: !r; s

(* ************************************************************************** *)
(*  [Fonc] get_linked_page : config -> base -> person -> string -> string     *)
(** [Description] : Permet de récupérer un lien de la chronique familiale.
    [Args] :
      - conf : configuration
      - base : base de donnée
      - p    : person
      - s    : nom du lien (eg. "HEAD", "OCCU", "BIBLIO", "BNOTE", "DEATH")
    [Retour] : string : "<a href="xxx">description du lien</a>"
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let get_linked_page conf base p s =
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  let fname = Filename.concat bdir "notes_links" in
  let db = NotesLinks.read_db_from_file fname in
  let db = Notes.merge_possible_aliases conf db in
  let key =
    let fn = Name.lower (sou base (get_first_name p)) in
    let sn = Name.lower (sou base (get_surname p)) in fn, sn, get_occ p
  in
  List.fold_left (linked_page_text conf base p s key) "" db

let events_list conf base p =
  let pevents =
    if authorized_age conf base p then
      List.fold_right
        (fun evt events ->
           let name = Pevent evt.epers_name in
           let date = evt.epers_date in
           let place = evt.epers_place in
           let note = evt.epers_note in
           let src = evt.epers_src in
           let wl = evt.epers_witnesses in
           let x = name, date, place, note, src, wl, None in x :: events)
        (get_pevents p) []
    else []
  in
  let get_name = function
    | (Pevent n, _, _, _, _, _, _) -> CheckItem.Psort n
    | (Fevent n, _, _, _, _, _, _) -> CheckItem.Fsort n
  in
  let get_date (_, date, _, _, _, _, _) = date in
  let fevents =
    (* On conserve l'ordre des familles. *)
    Array.fold_right
      (fun ifam fevents ->
         let fam = foi base ifam in
         let ifath = get_father fam in
         let imoth = get_mother fam in
         let isp = Gutil.spouse (get_iper p) fam in
         let m_auth =
           authorized_age conf base (pget conf base ifath) &&
           authorized_age conf base (pget conf base imoth)
         in
         let fam_fevents =
           if m_auth then
             List.fold_right
               (fun evt fam_fevents ->
                  let name = Fevent evt.efam_name in
                  let date = evt.efam_date in
                  let place = evt.efam_place in
                  let note = evt.efam_note in
                  let src = evt.efam_src in
                  let wl = evt.efam_witnesses in
                  let x = name, date, place, note, src, wl, Some isp in
                  x :: fam_fevents)
               (get_fevents fam) []
           else []
         in
         CheckItem.merge_events get_name get_date fam_fevents fevents)
      (get_family p) []
  in
  CheckItem.merge_events get_name get_date pevents fevents

let make_ep conf base ip =
  let p = pget conf base ip in
  let p_auth = authorized_age conf base p in p, p_auth

let make_efam conf base ip ifam =
  let fam = foi base ifam in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let ispouse = if ip = ifath then imoth else ifath in
  let cpl = ifath, imoth, ispouse in
  let m_auth =
    authorized_age conf base (pget conf base ifath) &&
    authorized_age conf base (pget conf base imoth)
  in
  fam, cpl, m_auth

let mode_local env =
  match get_env "fam_link" env with
  | Vfam _ -> false
  | _ -> true

let get_note_source conf base env auth no_note note_source =
  if auth && not no_note then
    let s = string_with_macros conf env note_source in
    let lines = Wiki.html_of_tlsw conf s in
    let lines =
      (* remove enclosing <p> .. </p> if any *)
      if List.length lines > 2 then
        match lines with
        | "<p>" :: remain ->
          if List.hd (List.rev remain) = "</p>"
          then List.rev (List.tl (List.rev remain))
          else lines
        | _ -> lines
      else lines
    in
    let wi =
      {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_file_path = Notes.file_path conf base;
       Wiki.wi_person_exists = person_exists conf base;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    let s = Wiki.syntax_links conf wi (String.concat "\n" lines) in
    Util.safe_html @@ if conf.pure_xhtml then Util.check_xhtml s else s
  else ""

let rec eval_var conf base env ep loc sl =
  try eval_simple_var conf base env ep sl with
    Not_found -> eval_compound_var conf base env ep loc sl
and eval_simple_var conf base env ep =
  function
    [s] ->
      begin try bool_val (eval_simple_bool_var conf base env s) with
        Not_found -> str_val (eval_simple_str_var conf base env ep s)
      end
  | _ -> raise Not_found
and eval_simple_bool_var conf base env =
  let fam_check_aux fn =
    match get_env "fam" env with
    | Vfam (_, fam, _, _) when mode_local env -> fn fam
    | _ ->
      match get_env "fam_link" env with
      | Vfam (_, fam, _, _) -> fn fam
      | _ -> raise Not_found
  in
  let check_relation test =
    fam_check_aux (fun fam -> test @@ get_relation fam)
  in
  function
  | "are_divorced" ->
    fam_check_aux (fun fam -> match get_divorce fam with Divorced _ -> true | _ -> false)
  | "are_engaged" ->
    check_relation ((=) Engaged)
  | "are_married" ->
    check_relation (function Married | NoSexesCheckMarried -> true | _ -> false)
  | "are_not_married" ->
    check_relation (function NotMarried | NoSexesCheckNotMarried -> true | _ -> false)
  | "are_pacs" ->
    check_relation ((=) Pacs)
  | "are_marriage_banns" ->
    check_relation ((=) MarriageBann)
  | "are_marriage_contract" ->
    check_relation ((=) MarriageContract)
  | "are_marriage_license" ->
    check_relation ((=) MarriageLicense)
  | "are_residence" ->
    check_relation ((=) Residence)
  | "are_separated" ->
    fam_check_aux (fun fam -> get_divorce fam = Separated)
  | "browsing_with_sosa_ref" ->
      begin match get_env "sosa_ref" env with
        Vsosa_ref v -> Lazy.force v <> None
      | _ -> raise Not_found
      end
  | "has_comment" | "has_fnotes" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) when mode_local env ->
          m_auth && not conf.no_note && sou base (get_comment fam) <> ""
      | _ ->
          match get_env "fam_link" env with
            Vfam (_, _, _, _) -> false
          | _ -> raise Not_found
      end
  | "has_fsources" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          m_auth && sou base (get_fsources fam) <> ""
      | _ -> false
      end
  | "has_marriage_note" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          m_auth && not conf.no_note && sou base (get_marriage_note fam) <> ""
      | _ -> raise Not_found
      end
  | "has_marriage_source" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          m_auth && sou base (get_marriage_src fam) <> ""
      | _ -> raise Not_found
      end
  | "has_relation_her" ->
      begin match get_env "rel" env with
        Vrel ({r_moth = Some _}, None) -> true
      | _ -> false
      end
  | "has_relation_him" ->
      begin match get_env "rel" env with
        Vrel ({r_fath = Some _}, None) -> true
      | _ -> false
      end
  | "has_witnesses" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) when mode_local env ->
          m_auth && Array.length (get_witnesses fam) > 0
      | _ ->
          match get_env "fam_link" env with
            Vfam (_, _, _, _) -> false
          | _ -> raise Not_found
      end


  | "is_first" ->
      begin match get_env "first" env with
        Vbool x -> x
      | _ -> raise Not_found
      end
  | "is_last" ->
      begin match get_env "last" env with
        Vbool x -> x
      | _ -> raise Not_found
      end
  | "is_no_mention" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, _) when mode_local env -> get_relation fam = NoMention
      | _ ->
          match get_env "fam_link" env with
            Vfam (_, fam, _, _) -> get_relation fam = NoMention
          | _ -> raise Not_found
      end
  | "is_no_sexes_check" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, _) when mode_local env ->
          get_relation fam = NoSexesCheckNotMarried ||
          get_relation fam = NoSexesCheckMarried
      | _ ->
          match get_env "fam_link" env with
            Vfam (_, fam, _, _) ->
              get_relation fam = NoSexesCheckNotMarried ||
              get_relation fam = NoSexesCheckMarried
          | _ -> raise Not_found
      end
  | "is_self" -> get_env "pos" env = Vstring "self"
  | "is_sibling_after" -> get_env "pos" env = Vstring "next"
  | "is_sibling_before" -> get_env "pos" env = Vstring "prev"
  | "lazy_printed" ->
      begin match get_env "lazy_print" env with
        Vlazyp r -> !r = None
      | _ -> raise Not_found
      end
  | s ->
      let v = extract_var "file_exists_" s in
      if v <> "" then
        let v = code_varenv v in
        let s = SrcfileDisplay.source_file_name conf v in Sys.file_exists s
      else raise Not_found
and eval_simple_str_var conf base env (_, p_auth) =
  function
    "alias" ->
      begin match get_env "alias" env with
        Vstring s -> s
      | _ -> raise Not_found
      end
  | "child_cnt" -> string_of_int_env "child_cnt" env
  | "comment" | "fnotes" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          get_note_source conf base [] m_auth conf.no_note
            (sou base (get_comment fam))
      | _ -> raise Not_found
      end
  | "count" ->
      begin match get_env "count" env with
        Vcnt c -> string_of_int !c
      | _ -> ""
      end
  | "count1" ->
      begin match get_env "count1" env with
        Vcnt c -> string_of_int !c
      | _ -> ""
      end
  | "count2" ->
      begin match get_env "count2" env with
        Vcnt c -> string_of_int !c
      | _ -> ""
      end
  | "divorce_date" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) when mode_local env ->
          begin match get_divorce fam with
            Divorced d ->
              let d = Adef.od_of_cdate d in
              begin match d with
                Some d when m_auth ->
                  begin match p_getenv conf.base_env "long_date" with
                    Some "yes" ->
                      " <em>" ^ DateDisplay.string_of_ondate conf d ^
                      DateDisplay.get_wday conf d ^ "</em>"
                  | _ -> " <em>" ^ DateDisplay.string_of_ondate conf d ^ "</em>"
                  end
              | _ -> ""
              end
          | _ -> raise Not_found
          end
      | _ ->
          match get_env "fam_link" env with
            Vfam (_, fam, _, m_auth) ->
              begin match get_divorce fam with
                Divorced d ->
                  let d = Adef.od_of_cdate d in
                  begin match d with
                    Some d when m_auth ->
                      begin match p_getenv conf.base_env "long_date" with
                        Some "yes" ->
                          " <em>" ^ DateDisplay.string_of_ondate conf d ^
                          DateDisplay.get_wday conf d ^ "</em>"
                      | _ -> " <em>" ^ DateDisplay.string_of_ondate conf d ^ "</em>"
                      end
                  | _ -> ""
                  end
              | _ -> raise Not_found
              end
          | _ -> raise Not_found
      end
  | "slash_divorce_date" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          begin match get_divorce fam with
            Divorced d ->
              let d = Adef.od_of_cdate d in
              begin match d with
                Some d when m_auth -> DateDisplay.string_slash_of_date conf d
              | _ -> ""
              end
          | _ -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | "empty_sorted_list" ->
      begin match get_env "list" env with
        | Vslist l -> l := SortedList.empty; ""
        | _ -> raise Not_found
      end
  | "empty_sorted_listb" ->
    begin match get_env "listb" env with
      | Vslist l -> l := SortedList.empty; ""
      | _ -> raise Not_found
    end
  | "empty_sorted_listc" ->
      begin match get_env "listc" env with
        | Vslist l -> l := SortedList.empty; ""
      | _ -> raise Not_found
      end
  | "family_cnt" -> string_of_int_env "family_cnt" env
  | "first_name_alias" ->
      begin match get_env "first_name_alias" env with
        Vstring s -> s
      | _ -> ""
      end
  | "fsources" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, _) -> sou base (get_fsources fam)
      | _ -> ""
      end
  | "incr_count" ->
      begin match get_env "count" env with
        Vcnt c -> incr c; ""
      | _ -> ""
      end
  | "incr_count1" ->
      begin match get_env "count1" env with
        Vcnt c -> incr c; ""
      | _ -> ""
      end
  | "incr_count2" ->
      begin match get_env "count2" env with
        Vcnt c -> incr c; ""
      | _ -> ""
      end
  | "lazy_force" ->
      begin match get_env "lazy_print" env with
        Vlazyp r ->
          begin match !r with
            Some s -> r := None; s
          | None -> ""
          end
      | _ -> raise Not_found
      end
  | "level" ->
      begin match get_env "level" env with
        Vint i -> string_of_int i
      | _ -> ""
      end
  | "marriage_place" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) when mode_local env ->
          if m_auth then
            Util.string_of_place conf (sou base (get_marriage_place fam))
          else ""
      | _ ->
          match get_env "fam_link" env with
            Vfam (_, fam, _, m_auth) ->
              if m_auth then
                Util.string_of_place conf (sou base (get_marriage_place fam))
              else ""
          | _ -> raise Not_found
      end
  | "marriage_note" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          get_note_source conf base [] m_auth conf.no_note
            (sou base (get_marriage_note fam))
      | _ -> raise Not_found
      end
  | "marriage_source" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          get_note_source conf base [] m_auth false
            (sou base (get_marriage_src fam))
      | _ -> raise Not_found
      end
  | "max_anc_level" ->
      begin match get_env "max_anc_level" env with
        Vint i -> string_of_int i
      | _ -> ""
      end
  | "static_max_anc_level" ->
      begin match get_env "static_max_anc_level" env with
        Vint i -> string_of_int i
      | _ -> ""
      end
  | "sosa_ref_max_anc_level" ->
      begin match get_env "sosa_ref_max_anc_level" env with
      | Vint i -> string_of_int i
      | _ -> ""
    end
  | "max_cous_level" ->
      begin match get_env "max_cous_level" env with
        Vint i -> string_of_int i
      | _ -> ""
      end
  | "max_desc_level" ->
      begin match get_env "max_desc_level" env with
        Vint i -> string_of_int i
      | _ -> ""
      end
  | "static_max_desc_level" ->
    begin match get_env "static_max_desc_level" env with
      | Vint i -> string_of_int i
      | _ -> ""
    end
  | "nobility_title" ->
      begin match get_env "nobility_title" env with
        Vtitle (p, t) ->
          if p_auth then
            string_of_title conf base (transl_nth conf "and" 0) p t
          else ""
      | _ -> raise Not_found
      end
  | "number_of_subitems" ->
      begin match get_env "item" env with
        Vslistlm ((s :: _) :: sll) ->
          let n =
            let rec loop n =
              function
                (s1 :: _) :: sll -> if s = s1 then loop (n + 1) sll else n
              | _ -> n
            in
            loop 1 sll
          in
          string_of_int n
      | _ -> raise Not_found
      end
  | "on_marriage_date" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) when mode_local env ->
          begin match m_auth, Adef.od_of_cdate (get_marriage fam) with
            true, Some s ->
              begin match p_getenv conf.base_env "long_date" with
                Some "yes" ->
                  DateDisplay.string_of_ondate conf s ^ DateDisplay.get_wday conf s
              | _ -> DateDisplay.string_of_ondate conf s
              end
          | _ -> ""
          end
      | _ ->
          match get_env "fam_link" env with
            Vfam (_, fam, _, m_auth) ->
              begin match m_auth, Adef.od_of_cdate (get_marriage fam) with
                true, Some s ->
                  begin match p_getenv conf.base_env "long_date" with
                    Some "yes" ->
                      DateDisplay.string_of_ondate conf s ^ DateDisplay.get_wday conf s
                  | _ -> DateDisplay.string_of_ondate conf s
                  end
              | _ -> ""
              end
          | _ -> raise Not_found
      end
  | "slash_marriage_date" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          begin match m_auth, Adef.od_of_cdate (get_marriage fam) with
            true, Some s -> DateDisplay.string_slash_of_date conf s
          | _ -> ""
          end
      | _ -> raise Not_found
      end
  | "origin_file" ->
      if conf.wizard then
        match get_env "fam" env with
          Vfam (_, fam, _, _) -> sou base (get_origin_file fam)
        | _ -> ""
      else raise Not_found
  | "qualifier" ->
      begin match get_env "qualifier" env with
        Vstring nn -> nn
      | _ -> raise Not_found
      end
  | "related_type" ->
      begin match get_env "rel" env with
        Vrel (r, Some c) ->
          rchild_type_text conf r.r_type (index_of_sex (get_sex c))
      | _ -> raise Not_found
      end
  | "relation_type" ->
      begin match get_env "rel" env with
        Vrel (r, None) ->
          begin match r.r_fath, r.r_moth with
            Some _, None -> relation_type_text conf r.r_type 0
          | None, Some _ -> relation_type_text conf r.r_type 1
          | Some _, Some _ -> relation_type_text conf r.r_type 2
          | _ -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | "reset_count" ->
      begin match get_env "count" env with
        Vcnt c -> c := 0; ""
      | _ -> ""
      end
  | "reset_count1" ->
      begin match get_env "count1" env with
        Vcnt c -> c := 0; ""
      | _ -> ""
      end
  | "reset_count2" ->
      begin match get_env "count2" env with
        Vcnt c -> c := 0; ""
      | _ -> ""
      end
  | "reset_desc_level" ->
      let flevt_save =
        match get_env "desc_level_table_save" env with
          Vdesclevtab levt -> let (_, flevt) = Lazy.force levt in flevt
        | _ -> raise Not_found
      in
      begin match get_env "desc_level_table" env with
        Vdesclevtab levt ->
          let (_, flevt) = Lazy.force levt in
          Gwdb.Collection.iter (fun i ->
              Gwdb.Marker.set flevt i (Gwdb.Marker.get flevt_save i)
            ) (Gwdb.ifams base) ;
          ""
      | _ -> raise Not_found
      end
  | "source_type" ->
      begin match get_env "src_typ" env with
        Vstring s -> s
      | _ -> raise Not_found
      end
  | "surname_alias" ->
      begin match get_env "surname_alias" env with
        Vstring s -> s
      | _ -> raise Not_found
      end
  | s ->
      let rec loop =
        function
          (pfx, f) :: pfx_list ->
            let v = extract_var pfx s in
            if v <> "" then f v else loop pfx_list
        | [] -> raise Not_found
      in
      loop
        ["evar_",
         (fun v ->
            match p_getenv (conf.env @ conf.henv) v with
              Some vv -> Util.escape_html vv
            | None -> "");
         (* warning: "cvar_" deprecated since 5.00; use "bvar." *)
         "cvar_",
         (fun v -> try List.assoc v conf.base_env with Not_found -> "")]
and eval_compound_var conf base env (a, _ as ep) loc =
  function
    "ancestor" :: sl ->
      begin match get_env "ancestor" env with
        Vanc gp -> eval_ancestor_field_var conf base env gp loc sl
      | Vanc_surn info -> eval_anc_by_surnl_field_var conf base env ep info sl
      | _ -> raise Not_found
      end
  | "baptism_witness" :: sl ->
      begin match get_env "baptism_witness" env with
        Vind p ->
          let ep = p, authorized_age conf base p in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | ["base"; "name"] -> VVstring conf.bname
  | ["base"; "nb_persons"] ->
      VVstring
        (Mutil.string_of_int_sep
           (Util.transl conf "(thousand separator)")
           (nb_of_persons base))
  | ["base"; "real_nb_persons"] ->
      VVstring
        (Mutil.string_of_int_sep
           (Util.transl conf "(thousand separator)")
           (Util.real_nb_of_persons conf base))
  | "birth_witness" :: sl ->
      begin match get_env "birth_witness" env with
        Vind p ->
          let ep = p, authorized_age conf base p in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | "burial_witness" :: sl ->
      begin match get_env "burial_witness" env with
        Vind p ->
          let ep = p, authorized_age conf base p in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | "cell" :: sl ->
      begin match get_env "cell" env with
        Vcell cell -> eval_cell_field_var conf base env cell loc sl
      | _ -> raise Not_found
      end
  | "child" :: sl ->
      begin match get_env "child" env with
        Vind p when mode_local env ->
          let auth = authorized_age conf base p in
          let ep = p, auth in eval_person_field_var conf base env ep loc sl
      | _ ->
          match get_env "child_link" env with
            Vind p ->
              let ep = p, true in
              let baseprefix =
                match get_env "baseprefix" env with
                  Vstring b -> b
                | _ -> conf.command
              in
              let conf = {conf with command = baseprefix} in
              eval_person_field_var conf base env ep loc sl
          | _ -> raise Not_found
      end
  | "cremation_witness" :: sl ->
      begin match get_env "cremation_witness" env with
        Vind p ->
          let ep = p, authorized_age conf base p in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | "death_witness" :: sl ->
      begin match get_env "death_witness" env with
        Vind p ->
          let ep = p, authorized_age conf base p in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | "enclosing" :: sl ->
      let rec loop =
        function
          ("#loop", _) :: env -> eval_person_field_var conf base env ep loc sl
        | _ :: env -> loop env
        | [] -> raise Not_found
      in
      loop env
  | "event_witness" :: sl ->
      begin match get_env "event_witness" env with
        Vind p ->
          let ep = p, authorized_age conf base p in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | "event_witness_relation" :: sl ->
      begin match get_env "event_witness_relation" env with
        Vevent (p, e) ->
          eval_event_witness_relation_var conf base env (p, e) loc sl
      | _ -> raise Not_found
      end
  | "event_witness_relation_kind" :: _ ->
      begin match get_env "event_witness_relation_kind" env with
        Vstring wk -> VVstring wk
      | _ -> raise Not_found
      end
  | "event_witness_kind" :: _ ->
      begin match get_env "event_witness_kind" env with
        Vstring s -> VVstring s
      | _ -> raise Not_found
      end
  | "family" :: sl ->
      (* TODO ???
      let mode_local =
        match get_env "fam_link" env with
        [ Vfam ifam _ (_, _, ip) _ -> False
        | _ -> True ]
      in *)
      begin match get_env "fam" env with
        Vfam (i, f, c, m) ->
          eval_family_field_var conf base env (i, f, c, m) loc sl
      | _ ->
          match get_env "fam_link" env with
            Vfam (i, f, c, m) ->
              eval_family_field_var conf base env (i, f, c, m) loc sl
          | _ -> raise Not_found
      end
  | "father" :: sl ->
      begin match get_parents a with
        Some ifam ->
          let cpl = foi base ifam in
          let ep = make_ep conf base (get_father cpl) in
          eval_person_field_var conf base env ep loc sl
      | None ->
#ifdef API
          match Perso_link.get_father_link conf.command (get_iper a) with
            Some fath ->
              let ep = Perso_link.make_ep_link base fath in
              let conf = {conf with command = fath.MLink.Person.baseprefix} in
              let env = ("p_link", Vbool true) :: env in
              eval_person_field_var conf base env ep loc sl
          | None ->
              warning_use_has_parents_before_parent loc "father" (str_val "")
#else
           warning_use_has_parents_before_parent loc "father" (str_val "")
#endif
      end
  | "item" :: sl ->
      begin match get_env "item" env with
        Vslistlm ell -> eval_item_field_var ell sl
      | _ -> raise Not_found
      end
  | "mother" :: sl ->
      begin match get_parents a with
        Some ifam ->
          let cpl = foi base ifam in
          let ep = make_ep conf base (get_mother cpl) in
          eval_person_field_var conf base env ep loc sl
      | None ->
#ifdef API
          match Perso_link.get_mother_link conf.command (get_iper a) with
            Some moth ->
              let ep = Perso_link.make_ep_link base moth in
              let conf = {conf with command = moth.MLink.Person.baseprefix} in
              let env = ("p_link", Vbool true) :: env in
              eval_person_field_var conf base env ep loc sl
          | None ->
              warning_use_has_parents_before_parent loc "mother" (str_val "")
#else
          warning_use_has_parents_before_parent loc "mother" (str_val "")
#endif
      end
  | "next_item" :: sl ->
      begin match get_env "item" env with
        Vslistlm (_ :: ell) -> eval_item_field_var ell sl
      | _ -> raise Not_found
      end
  | "number_of_ancestors" :: sl ->
      begin match get_env "n" env with
        Vint n -> VVstring (eval_num conf (Sosa.of_int (n - 1)) sl)
      | _ -> raise Not_found
      end
  | "number_of_descendants" :: sl ->
      (* FIXME: what is the difference with number_of_descendants_at_level??? *)
      begin match get_env "level" env with
        Vint i ->
          begin match get_env "desc_level_table" env with
            Vdesclevtab t ->
            let m = fst (Lazy.force t) in
            let cnt =
              Gwdb.Collection.fold (fun cnt ip ->
                  if Gwdb.Marker.get m ip <= i then cnt + 1 else cnt
                ) 0 (Gwdb.ipers base)
              in
              VVstring (eval_num conf (Sosa.of_int (cnt - 1)) sl)
          | _ -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | "number_of_descendants_at_level" :: sl ->
      begin match get_env "level" env with
        Vint i ->
          begin match get_env "desc_level_table" env with
            Vdesclevtab t ->
            let m = fst (Lazy.force t) in
            let cnt =
              Gwdb.Collection.fold (fun cnt ip ->
                  if Gwdb.Marker.get m ip <= i then cnt + 1 else cnt
                ) 0 (Gwdb.ipers base)
              in
              VVstring (eval_num conf (Sosa.of_int (cnt - 1)) sl)
          | _ -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | "parent" :: sl ->
      begin match get_env "parent" env with
        Vind p ->
          let ep = p, authorized_age conf base p in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | "prev_item" :: sl ->
      begin match get_env "prev_item" env with
        Vslistlm ell -> eval_item_field_var ell sl
      | _ -> raise Not_found
      end
  | "prev_family" :: sl ->
      begin match get_env "prev_fam" env with
        Vfam (i, f, c, m) ->
          eval_family_field_var conf base env (i, f, c, m) loc sl
      | _ -> raise Not_found
      end
  | "pvar" :: v :: sl ->
      begin match find_person_in_env conf base v with
      | Some p ->
          let ep = make_ep conf base (get_iper p) in
          eval_person_field_var conf base env ep loc sl
      | None -> raise Not_found
      end
  | "qvar" :: v :: sl ->
      (* %qvar.index_v.surname;
         direct access to a person whose index value is v
      *)
      let v0 = iper_of_string v in
      (* if v0 >= 0 && v0 < nb_of_persons base then *)
        let ep = make_ep conf base v0 in
        if is_hidden (fst ep) then raise Not_found
        else eval_person_field_var conf base env ep loc sl
      (* else raise Not_found *)
  | "svar" :: i :: sl ->
      (* http://localhost:2317/HenriT_w?m=DAG&p1=henri&n1=duchmol&s1=243&s2=245
         access to sosa si=n of a person pi ni
         find_base_p will scan down starting from i such that multiple sosa of
         the same person can be listed
      *)
      let rec find_base_p j =
        let s = string_of_int j in
        let po = Util.find_person_in_env conf base s in
        begin match po with
        | Some p -> p
        | None -> if j = 0 then raise Not_found else find_base_p (j-1)
        end
      in
      let p0 = find_base_p (int_of_string i) in
      (* find sosa identified by si= of that person *)
      begin match p_getint conf.env ("s" ^ i) with
      | Some s ->
            let s0 = Sosa.of_int s in
            let ip0 = get_iper p0 in
            begin match Util.branch_of_sosa conf base s0 (pget conf base ip0) with
            | Some (p :: _) ->
                let p_auth = authorized_age conf base p in
                eval_person_field_var conf base env (p, p_auth) loc sl
            | _ -> raise Not_found
            end
      | None -> raise Not_found
      end
  | "sosa_anc" :: s :: sl ->
      (* %sosa_anc.sosa.first_name;
         direct access to a person whose sosa relative to sosa_ref is s
      *)
      begin match get_env "sosa_ref" env with
      | Vsosa_ref v ->
          begin match Lazy.force v with
          | Some p ->
              let ip = get_iper p in
              let s0 = Sosa.of_string s in
              begin match Util.branch_of_sosa conf base s0 (pget conf base ip) with
              | Some (p :: _) ->
                  let p_auth = authorized_age conf base p in
                  eval_person_field_var conf base env (p, p_auth) loc sl
              | _ -> raise Not_found
              end
          | _ -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | "sosa_anc_p" :: s :: sl ->
      (* %sosa_anc_p.sosa.first_name;
         direct access to a person whose sosa relative to current person
      *)
      begin match Util.p_of_sosa conf base (Sosa.of_string s) a with
      | Some np ->
        let np_auth = authorized_age conf base np in
        eval_person_field_var conf base env (np, np_auth) loc sl
      | _ -> raise Not_found
      end
  | "related" :: sl ->
      begin match get_env "rel" env with
        Vrel ({r_type = rt}, Some p) ->
          eval_relation_field_var conf base env
            (index_of_sex (get_sex p), rt, get_iper p, false) loc sl
      | _ -> raise Not_found
      end
  | "relation_her" :: sl ->
      begin match get_env "rel" env with
        Vrel ({r_moth = Some ip; r_type = rt}, None) ->
          eval_relation_field_var conf base env (1, rt, ip, true) loc sl
      | _ -> raise Not_found
      end
  | "relation_him" :: sl ->
      begin match get_env "rel" env with
        Vrel ({r_fath = Some ip; r_type = rt}, None) ->
          eval_relation_field_var conf base env (0, rt, ip, true) loc sl
      | _ -> raise Not_found
      end
  | "self" :: sl -> eval_person_field_var conf base env ep loc sl
  | "sosa_ref" :: sl ->
      begin match get_env "sosa_ref" env with
        Vsosa_ref v ->
          begin match Lazy.force v with
            Some p ->
              let ep = make_ep conf base (get_iper p) in
              eval_person_field_var conf base env ep loc sl
          | None -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | "spouse" :: sl ->
      begin match get_env "fam" env with
        Vfam (_, _, (_, _, ip), _) when mode_local env ->
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ ->
#ifdef API
          match get_env "fam_link" env with
            Vfam (_, _, (_, _, ip), _) ->
              let baseprefix =
                match get_env "baseprefix" env with
                  Vstring baseprefix -> baseprefix
                | _ -> conf.command
              in
              begin match Perso_link.get_person_link baseprefix ip with
                Some spouse ->
                  let ep = Perso_link.make_ep_link base spouse in
                  let conf =
                    {conf with command = spouse.MLink.Person.baseprefix}
                  in
                  let env = ("p_link", Vbool true) :: env in
                  eval_person_field_var conf base env ep loc sl
              | None -> raise Not_found
              end
          | _ -> raise Not_found
#else
        raise Not_found
#endif
      end
  | "witness" :: sl ->
      begin match get_env "witness" env with
        Vind p ->
          let ep = p, authorized_age conf base p in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | "witness_relation" :: sl ->
      begin match get_env "fam" env with
        Vfam (i, f, c, m) ->
          eval_witness_relation_var conf base env (i, f, c, m) loc sl
      | _ -> raise Not_found
      end
  | sl -> eval_person_field_var conf base env ep loc sl
and eval_item_field_var ell =
  function
    [s] ->
      begin try
        match ell with
          el :: _ ->
            let v = int_of_string s in
            let r = try List.nth el (v - 1) with Failure _ -> "" in VVstring r
        | [] -> VVstring ""
      with Failure _ -> raise Not_found
      end
  | _ -> raise Not_found
and eval_relation_field_var conf base env (i, rt, ip, is_relation) loc =
  function
    ["type"] ->
      if is_relation then VVstring (relation_type_text conf rt i)
      else VVstring (rchild_type_text conf rt i)
  | sl ->
      let ep = make_ep conf base ip in
      eval_person_field_var conf base env ep loc sl
and eval_cell_field_var conf base env cell loc =
  function
    ["colspan"] ->
      begin match cell with
        Empty -> VVstring "1"
      | Cell (_, _, _, _, s, _) -> VVstring (string_of_int s)
      end
  | "family" :: sl ->
      begin match cell with
        Cell (p, Some ifam, _, _, _, base_prefix) ->
          if conf.bname = base_prefix then
            let (f, c, a) = make_efam conf base (get_iper p) ifam in
            eval_family_field_var conf base env (ifam, f, c, a) loc sl
          else
#ifdef API
            let conf = {conf with command = base_prefix} in
            let (f, c, a) =
              let rec loop l =
                match l with
                  [] -> failwith "lia"
                | fam_link :: l ->
                    let fam_link_ifam = Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam in
                    if fam_link_ifam = ifam then
                      let (_, fam, _, _) =
                        Perso_link.make_efam_link conf base fam_link
                      in
                      let ifath = Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath in
                      let imoth = Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth in
                      let cpl =
                        let ip = get_iper p in
                        if ip <> ifath && ip <> imoth then
                          match
                            Perso_link.get_person_link_with_base conf.command
                              ip base_prefix
                          with
                            Some p ->
                              let ip = Gwdb.iper_of_string @@ Int32.to_string p.MLink.Person.ip in
                              ifath, imoth,
                              (if ip = ifath then imoth else ifath)
                          | None ->
                              ifath, imoth,
                              (if ip = ifath then imoth else ifath)
                        else
                          ifath, imoth, (if ip = ifath then imoth else ifath)
                      in
                      fam, cpl, true
                    else loop l
              in
              let faml =
                Perso_link.get_family_link conf.command (get_iper p)
              in
              loop faml
            in
            eval_family_field_var conf base env (ifam, f, c, a) loc sl
#else
            VVstring ""
#endif
      | _ -> VVstring ""
      end
  | ["is_center"] ->
      begin match cell with
        Cell (_, _, Center, _, _, _) -> VVbool true
      | _ -> VVbool false
      end
  | ["is_empty"] ->
      begin match cell with
        Empty -> VVbool true
      | _ -> VVbool false
      end
  | ["is_left"] ->
      begin match cell with
        Cell (_, _, Left, _, _, _) -> VVbool true
      | _ -> VVbool false
      end
  | ["is_right"] ->
      begin match cell with
        Cell (_, _, Right, _, _, _) -> VVbool true
      | _ -> VVbool false
      end
  | ["is_top"] ->
      begin match cell with
        Cell (_, _, _, false, _, _) -> VVbool true
      | _ -> VVbool false
      end
  | "person" :: sl ->
      begin match cell with
        Cell (p, _, _, _, _, base_prefix) ->
          if conf.bname = base_prefix then
            let ep = make_ep conf base (get_iper p) in
            eval_person_field_var conf base env ep loc sl
          else
            let conf = {conf with command = base_prefix} in
            let ep = p, true in eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | _ -> raise Not_found
and eval_ancestor_field_var conf base env gp loc =
  function
    "family" :: sl ->
      begin match gp with
        GP_person (_, ip, Some ifam) ->
          let f = foi base ifam in
          let ifath = get_father f in
          let imoth = get_mother f in
          let ispouse = if ip = ifath then imoth else ifath in
          let c = ifath, imoth, ispouse in
          let m_auth =
            authorized_age conf base (pget conf base ifath) &&
            authorized_age conf base (pget conf base imoth)
          in
          eval_family_field_var conf base env (ifam, f, c, m_auth) loc sl
      | _ -> raise Not_found
      end
  | "father" :: sl ->
      begin match gp with
        GP_person (_, ip, _) ->
          begin match
            get_parents (pget conf base ip), get_env "all_gp" env
          with
            Some ifam, Vallgp all_gp ->
              let cpl = foi base ifam in
              begin match get_link all_gp (get_father cpl) with
                Some gp -> eval_ancestor_field_var conf base env gp loc sl
              | None ->
                  let ep = make_ep conf base (get_father cpl) in
                  eval_person_field_var conf base env ep loc sl
              end
          | _, _ -> raise Not_found
          end
      | GP_same (_, _, ip) ->
          begin match get_parents (pget conf base ip) with
            Some ifam ->
              let cpl = foi base ifam in
              let ep = make_ep conf base (get_father cpl) in
              eval_person_field_var conf base env ep loc sl
          | _ -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | ["father_sosa"] ->
      begin match gp, get_env "all_gp" env with
        (GP_person (n, ip, _) | GP_same (n, _, ip)), Vallgp all_gp ->
          let n = Sosa.twice n in
          VVstring (parent_sosa conf base ip all_gp n get_father)
      | _ -> VVstring ""
      end
  | ["interval"] ->
    let to_string x =
      Mutil.string_of_int_sep
        (transl conf "(thousand separator)")
        (int_of_string @@ Sosa.to_string x)
    in
      begin match gp with
        GP_interv (Some (n1, n2, Some (n3, n4))) ->
          let n2 = Sosa.sub n2 Sosa.one in
          let n4 = Sosa.sub n4 Sosa.one in
          VVstring (to_string n1 ^ "-" ^ to_string n2 ^ " = " ^ to_string n3 ^ "-" ^ to_string n4)
      | GP_interv (Some (n1, n2, None)) ->
          let n2 = Sosa.sub n2 Sosa.one in
          VVstring (to_string n1 ^ "-" ^ to_string n2 ^ " = ...")
      | GP_interv None -> VVstring "..."
      | _ -> VVstring ""
      end
  | ["mother_sosa"] ->
      begin match gp, get_env "all_gp" env with
        (GP_person (n, ip, _) | GP_same (n, _, ip)), Vallgp all_gp ->
          let n = Sosa.inc (Sosa.twice n) 1 in
          VVstring (parent_sosa conf base ip all_gp n get_mother)
      | _ -> VVstring ""
      end
  | "same" :: sl ->
      begin match gp with
        GP_same (_, n, _) -> VVstring (eval_num conf n sl)
      | _ -> VVstring ""
      end
  | "anc_sosa" :: sl ->
      begin match gp with
        GP_person (n, _, _) | GP_same (n, _, _) ->
          VVstring (eval_num conf n sl)
      | _ -> VVstring ""
      end
  | "spouse" :: sl ->
      begin match gp with
        GP_person (_, ip, Some ifam) ->
          let ip = Gutil.spouse ip (foi base ifam) in
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | sl ->
      match gp with
        GP_person (_, ip, _) | GP_same (_, _, ip) ->
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
and eval_anc_by_surnl_field_var conf base env ep info =
  match info with
    Branch (_, db, de, place, p, sosa_list, loc) ->
      (function
         "date_begin" :: sl ->
           begin match db with
             Some d -> eval_date_field_var conf d sl
           | None -> VVstring ""
           end
       | "date_end" :: sl ->
           begin match de with
             Some d -> eval_date_field_var conf d sl
           | None -> VVstring ""
           end
       | ["nb_times"] -> VVstring (string_of_int (List.length sosa_list))
       | ["place"] -> VVstring (Util.string_of_place conf place)
       | ["sosa_access"] ->
           let (str, _) =
             List.fold_right
               (fun sosa (str, n) ->
                  let str =
                    str ^ "&s" ^ string_of_int n ^ "=" ^ Sosa.to_string sosa
                  in
                  str, n + 1)
               sosa_list ("", 1)
           in
           let (p, _) = ep in VVstring (acces_n conf base "1" p ^ str)
       | sl ->
           let ep = make_ep conf base (get_iper p) in
           eval_person_field_var conf base env ep loc sl)
  | Eclair (_, place, db, de, p, persl, loc) ->
      function
        "date_begin" :: sl ->
          begin match db with
            Some d -> eval_date_field_var conf d sl
          | None -> VVstring ""
          end
      | "date_end" :: sl ->
          begin match de with
            Some d -> eval_date_field_var conf d sl
          | None -> VVstring ""
          end
      | ["nb_events"] -> VVstring (string_of_int (List.length persl))
      | ["nb_ind"] ->
          let list =
            IperSet.elements (List.fold_right IperSet.add persl IperSet.empty)
          in
          VVstring (string_of_int (List.length list))
      | ["place"] -> VVstring place
      | sl ->
          let ep = make_ep conf base (get_iper p) in
          eval_person_field_var conf base env ep loc sl
and eval_num conf n =
  function
    ["hexa"] -> Printf.sprintf "0x%X" @@ int_of_string (Sosa.to_string n)
  | ["octal"] -> Printf.sprintf "0x%o" @@ int_of_string (Sosa.to_string n)
  | ["lvl"] -> string_of_int @@ Sosa.gen n
  | ["v"] -> Sosa.to_string n
  | [] -> Sosa.to_string_sep (transl conf "(thousand separator)") n
  | _ -> raise Not_found
and eval_person_field_var conf base env (p, p_auth as ep) loc =
  function
    "baptism_date" :: sl ->
      begin match Adef.od_of_cdate (get_baptism p) with
        Some d when p_auth -> eval_date_field_var conf d sl
      | _ -> VVstring ""
      end
  | "birth_date" :: sl ->
      begin match Adef.od_of_cdate (get_birth p) with
        Some d when p_auth -> eval_date_field_var conf d sl
      | _ -> VVstring ""
      end
  | "burial_date" :: sl ->
      begin match get_burial p with
        Buried cod when p_auth ->
          begin match Adef.od_of_cdate cod with
            Some d -> eval_date_field_var conf d sl
          | None -> VVstring ""
          end
      | _ -> VVstring ""
      end
  | "cremated_date" :: sl ->
      begin match get_burial p with
        Cremated cod when p_auth ->
          begin match Adef.od_of_cdate cod with
            Some d -> eval_date_field_var conf d sl
          | None -> VVstring ""
          end
      | _ -> VVstring ""
      end
  | "death_date" :: sl ->
      begin match get_death p with
        Death (_, cd) when p_auth ->
          eval_date_field_var conf (Adef.date_of_cdate cd) sl
      | _ -> VVstring ""
      end
  | "event" :: sl ->
      begin match get_env "event" env with
        Vevent (_, e) -> eval_event_field_var conf base env ep e loc sl
      | _ -> raise Not_found
      end
  | "father" :: sl ->
      begin match get_parents p with
        Some ifam ->
          let cpl = foi base ifam in
          let ep = make_ep conf base (get_father cpl) in
          eval_person_field_var conf base env ep loc sl
      | None ->
#ifdef API
          match Perso_link.get_father_link conf.command (get_iper p) with
            Some fath ->
              let ep = Perso_link.make_ep_link base fath in
              let conf = {conf with command = fath.MLink.Person.baseprefix} in
              let env = ("p_link", Vbool true) :: env in
              eval_person_field_var conf base env ep loc sl
          | None ->
              warning_use_has_parents_before_parent loc "father" (str_val "")
#else
          warning_use_has_parents_before_parent loc "father" (str_val "")
#endif
      end
  | ["has_linked_page"; s] ->
      begin match get_env "nldb" env with
        Vnldb db ->
          let key =
            let fn = Name.lower (sou base (get_first_name p)) in
            let sn = Name.lower (sou base (get_surname p)) in
            fn, sn, get_occ p
          in
          let r =
            List.exists
              (fun (pg, (_, il)) ->
                 match pg with
                   NotesLinks.PgMisc pg ->
                     if List.mem_assoc key il then
                       let (nenv, _) = Notes.read_notes base pg in
                       List.mem_assoc s nenv
                     else false
                 | _ -> false)
              db
          in
          VVbool r
      | _ -> raise Not_found
      end
  | ["has_linked_pages"] ->
      begin match get_env "nldb" env with
        Vnldb db ->
          let r =
            if p_auth then
              let key =
                let fn = Name.lower (sou base (get_first_name p)) in
                let sn = Name.lower (sou base (get_surname p)) in
                fn, sn, get_occ p
              in
              links_to_ind conf base db key <> []
            else false
          in
          VVbool r
      | _ -> raise Not_found
      end
  | ["has_sosa"] ->
      begin match get_env "p_link" env with
        Vbool _ -> VVbool false
      | _ ->
          match get_env "sosa" env with
            Vsosa r -> VVbool (get_sosa conf base env r p <> None)
          | _ -> VVbool false
      end
#ifdef API
  | ["init_cache"; nb_asc; from_gen_desc; nb_desc] ->
      begin try
        let nb_asc = int_of_string nb_asc in
        let from_gen_desc = int_of_string from_gen_desc in
        let nb_desc = int_of_string nb_desc in
        let () =
          Perso_link.init_cache conf base (get_iper p) nb_asc
            from_gen_desc nb_desc
        in
        VVstring ""
      with _ -> raise Not_found
      end
#else
  | "init_cache" :: _ ->
      VVstring ""
#endif
  | ["linked_page"; s] ->
      begin match get_env "nldb" env with
        Vnldb db ->
          let key =
            let fn = Name.lower (sou base (get_first_name p)) in
            let sn = Name.lower (sou base (get_surname p)) in
            fn, sn, get_occ p
          in
          let s = List.fold_left (linked_page_text conf base p s key) "" db in
          VVstring s
      | _ -> raise Not_found
      end
  | "marriage_date" :: sl ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, true) ->
          begin match Adef.od_of_cdate (get_marriage fam) with
            Some d -> eval_date_field_var conf d sl
          | None -> VVstring ""
          end
      | _ -> raise Not_found
      end
  | "mother" :: sl ->
      begin match get_parents p with
        Some ifam ->
          let cpl = foi base ifam in
          let ep = make_ep conf base (get_mother cpl) in
          eval_person_field_var conf base env ep loc sl
      | None ->
#ifdef API
          match Perso_link.get_mother_link conf.command (get_iper p) with
            Some moth ->
              let ep = Perso_link.make_ep_link base moth in
              let conf = {conf with command = moth.MLink.Person.baseprefix} in
              let env = ("p_link", Vbool true) :: env in
              eval_person_field_var conf base env ep loc sl
          | None ->
              warning_use_has_parents_before_parent loc "mother" (str_val "")
#else
          warning_use_has_parents_before_parent loc "mother" (str_val "")
#endif
      end
  | "nobility_title" :: sl ->
      begin match Util.main_title conf base p with
        Some t when p_auth ->
          let id = sou base t.t_ident in
          let pl = sou base t.t_place in
          eval_nobility_title_field_var (id, pl) sl
      | _ -> VVstring ""
      end
  | "self" :: sl -> eval_person_field_var conf base env ep loc sl
  | "sosa" :: sl ->
      begin match get_env "sosa" env with
        Vsosa x ->
          begin match get_sosa conf base env x p with
            Some (n, _) -> VVstring (eval_num conf n sl)
          | None -> VVstring ""
          end
      | _ -> raise Not_found
      end
  | "sosa_next" :: sl ->
      begin match get_env "sosa" env with
      | Vsosa x ->
          begin match get_sosa conf base env x p with
          | Some (n, _) ->
              begin match next_sosa n with
              | (so, ip) ->
                if so = Sosa.zero then VVstring ""
                else
                  let p = poi base ip in
                  let p_auth = authorized_age conf base p in
                  eval_person_field_var conf base env (p, p_auth) loc sl
              end
          | None -> VVstring ""
          end
      | _ -> raise Not_found
      end
  | "sosa_prev" :: sl ->
      begin match get_env "sosa" env with
      | Vsosa x ->
          begin match get_sosa conf base env x p with
          | Some (n, _) ->
              begin match prev_sosa n with
              | (so, ip) ->
                if so = Sosa.zero then VVstring ""
                else
                  let p = poi base ip in
                  let p_auth = authorized_age conf base p in
                  eval_person_field_var conf base env (p, p_auth) loc sl
              end
          | None -> VVstring ""
          end
      | _ -> raise Not_found
      end
  | "spouse" :: sl ->
      begin match get_env "fam" env with
        Vfam (ifam, _, _, _) ->
          let cpl = foi base ifam in
          let ip = Gutil.spouse (get_iper p) cpl in
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | ["var"] -> VVother (eval_person_field_var conf base env ep loc)
  | [s] ->
      begin try bool_val (eval_bool_person_field conf base env ep s) with
        Not_found ->
          begin try str_val (eval_str_person_field conf base env ep s) with
            Not_found -> obsolete_eval conf base env ep loc s
          end
      end
  | [] -> str_val (simple_person_text conf base p p_auth)
  | _ -> raise Not_found
and eval_date_field_var conf d =
  function
    ["prec"] ->
      begin match d with
        Dgreg (dmy, _) -> VVstring (Util.escape_html (DateDisplay.prec_text conf dmy))
      | _ -> VVstring ""
      end
  | ["day"] ->
      begin match d with
        Dgreg (dmy, _) ->
          if dmy.day = 0 then VVstring ""
          else VVstring (string_of_int dmy.day)
      | _ -> VVstring ""
      end
  | ["day2"] ->
      begin match d with
        Dgreg (dmy, _) ->
          begin match dmy.prec with
            OrYear dmy2 | YearInt dmy2 ->
              if dmy2.day2 = 0 then VVstring ""
              else VVstring (string_of_int dmy2.day2)
          | _ -> VVstring ""
          end
      | _ -> VVstring ""
      end
  | ["julian_day"] ->
      begin match d with
        Dgreg (dmy, _) ->
          VVstring (string_of_int (Calendar.sdn_of_julian dmy))
      | _ -> VVstring ""
      end
  | ["month"] ->
      begin match d with
        Dgreg (dmy, _) -> VVstring (DateDisplay.month_text dmy)
      | _ -> VVstring ""
      end
  | ["month2"] ->
      begin match d with
        Dgreg (dmy, _) ->
          begin match dmy.prec with
            OrYear dmy2 | YearInt dmy2 ->
              if dmy2.month2 = 0 then VVstring ""
              else VVstring (string_of_int dmy2.month2)
          | _ -> VVstring ""
          end
      | _ -> VVstring ""
      end
  | ["year"] ->
      begin match d with
        Dgreg (dmy, _) -> VVstring (string_of_int dmy.year)
      | _ -> VVstring ""
      end
  | ["year2"] ->
      begin match d with
        Dgreg (dmy, _) ->
          begin match dmy.prec with
            OrYear dmy2 | YearInt dmy2 -> VVstring (string_of_int dmy2.year2)
          | _ -> VVstring ""
          end
      | _ -> VVstring ""
      end
  | [] -> VVstring (DateDisplay.string_of_date_sep conf "&#010;  " d)
  | _ -> raise Not_found
and _eval_place_field_var conf place =
  function
    [] ->
      (* Compatibility before eval_place_field_var *)
      VVstring place
  | ["other"] ->
      begin match place_of_string conf place with
        Some p -> VVstring p.other
      | None -> VVstring ""
      end
  | ["town"] ->
      begin match place_of_string conf place with
        Some p -> VVstring p.town
      | None -> VVstring ""
      end
  | ["township"] ->
      begin match place_of_string conf place with
        Some p -> VVstring p.township
      | None -> VVstring ""
      end
  | ["canton"] ->
      begin match place_of_string conf place with
        Some p -> VVstring p.canton
      | None -> VVstring ""
      end
  | ["district"] ->
      begin match place_of_string conf place with
        Some p -> VVstring p.district
      | None -> VVstring ""
      end
  | ["county"] ->
      begin match place_of_string conf place with
        Some p -> VVstring p.county
      | None -> VVstring ""
      end
  | ["region"] ->
      begin match place_of_string conf place with
        Some p -> VVstring p.region
      | None -> VVstring ""
      end
  | ["country"] ->
      begin match place_of_string conf place with
        Some p -> VVstring p.country
      | None -> VVstring ""
      end
  | _ -> raise Not_found
and eval_nobility_title_field_var (id, pl) =
  function
    ["ident_key"] -> VVstring (code_varenv id)
  | ["place_key"] -> VVstring (code_varenv pl)
  | [] -> VVstring (if pl = "" then id else id ^ " " ^ pl)
  | _ -> raise Not_found
and eval_bool_event_field base (p, p_auth)
    (_, date, place, note, src, w, isp) =
  function
    "has_date" -> p_auth && date <> Adef.cdate_None
  | "has_place" -> p_auth && sou base place <> ""
  | "has_note" -> p_auth && sou base note <> ""
  | "has_src" -> p_auth && sou base src <> ""
  | "has_witnesses" -> p_auth && Array.length w > 0
  | "has_spouse" -> p_auth && isp <> None
  | "computable_age" ->
      if p_auth then
        match Adef.od_of_cdate (get_birth p) with
          Some (Dgreg (d, _)) ->
            not (d.day = 0 && d.month = 0 && d.prec <> Sure)
        | _ ->
            match Adef.od_of_cdate (get_baptism p) with
              Some (Dgreg (d, _)) ->
                not (d.day = 0 && d.month = 0 && d.prec <> Sure)
            | _ -> false
      else false
  | _ -> raise Not_found
and eval_str_event_field conf base (p, p_auth)
    (name, date, place, note, src, _, _) =
  function
    "age" ->
      if p_auth then
        let (birth_date, approx) =
          match Adef.od_of_cdate (get_birth p) with
            None -> Adef.od_of_cdate (get_baptism p), true
          | x -> x, false
        in
        match birth_date, Adef.od_of_cdate date with
          Some (Dgreg (({prec = Sure | About | Maybe} as d1), _)),
          Some (Dgreg (({prec = Sure | About | Maybe} as d2), _))
          when d1 <> d2 ->
            let a = Date.time_elapsed d1 d2 in
            let s =
              if not approx && d1.prec = Sure && d2.prec = Sure then ""
              else transl_decline conf "possibly (date)" "" ^ " "
            in
            s ^ DateDisplay.string_of_age conf a
        | _ -> ""
      else ""
  | "name" ->
      begin match p_auth, name with
        true, Pevent name -> Util.string_of_pevent_name conf base name
      | true, Fevent name -> Util.string_of_fevent_name conf base name
      | _ -> ""
      end
  | "date" ->
      begin match p_auth, Adef.od_of_cdate date with
        true, Some d -> DateDisplay.string_of_date conf d
      | _ -> ""
      end
  | "on_date" ->
      begin match p_auth, Adef.od_of_cdate date with
        true, Some d ->
          begin match p_getenv conf.base_env "long_date" with
            Some "yes" -> DateDisplay.string_of_ondate conf d ^ DateDisplay.get_wday conf d
          | _ -> DateDisplay.string_of_ondate conf d
          end
      | _ -> ""
      end
  | "place" ->
      if p_auth then Util.string_of_place conf (sou base place) else ""
  | "note" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth conf.no_note (sou base note)
  | "src" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth false (sou base src)
  | _ -> raise Not_found
and eval_event_field_var conf base env (p, p_auth)
    (name, date, place, note, src, w, isp) loc =
  function
    "date" :: sl ->
      begin match p_auth, Adef.od_of_cdate date with
        true, Some d -> eval_date_field_var conf d sl
      | _ -> VVstring ""
      end
  | "spouse" :: sl ->
      begin match isp with
        Some isp ->
          let sp = poi base isp in
          let ep = sp, authorized_age conf base sp in
          eval_person_field_var conf base env ep loc sl
      | None -> VVstring ""
      end
  | [s] ->
      begin try
        bool_val
          (eval_bool_event_field base (p, p_auth)
             (name, date, place, note, src, w, isp) s)
      with Not_found ->
        str_val
          (eval_str_event_field conf base (p, p_auth)
             (name, date, place, note, src, w, isp) s)
      end
  | _ -> raise Not_found
and eval_event_witness_relation_var conf base env (p, e) loc =
  function
    "event" :: sl ->
      let ep = p, authorized_age conf base p in
      eval_event_field_var conf base env ep e loc sl
  | "person" :: sl ->
      let ep = p, authorized_age conf base p in
      eval_person_field_var conf base env ep loc sl
  | _ -> raise Not_found
and eval_bool_person_field conf base env (p, p_auth) =
  function
    "access_by_key" ->
      Util.accessible_by_key conf base p (p_first_name base p)
        (p_surname base p)
  | "birthday" ->
      begin match p_auth, Adef.od_of_cdate (get_birth p) with
        true, Some (Dgreg (d, _)) ->
          if d.prec = Sure && get_death p = NotDead then
            d.day = conf.today.day && d.month = conf.today.month &&
            d.year < conf.today.year ||
            not (Date.leap_year conf.today.year) && d.day = 29 &&
            d.month = 2 && conf.today.day = 1 && conf.today.month = 3
          else false
      | _ -> false
      end
  | "wedding_birthday" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          begin match get_relation fam, get_divorce fam with
            (Married | NoSexesCheckMarried), NotDivorced ->
              begin match m_auth, Adef.od_of_cdate (get_marriage fam) with
                true, Some (Dgreg (d, _)) ->
                  let father = pget conf base (get_father fam) in
                  let mother = pget conf base (get_mother fam) in
                  if d.prec = Sure && authorized_age conf base father &&
                     get_death father = NotDead &&
                     authorized_age conf base mother &&
                     get_death mother = NotDead
                  then
                    d.day = conf.today.day && d.month = conf.today.month &&
                    d.year < conf.today.year ||
                    not (Date.leap_year conf.today.year) && d.day = 29 &&
                    d.month = 2 && conf.today.day = 1 && conf.today.month = 3
                  else false
              | _ -> false
              end
          | _ -> false
          end
      | _ -> false
      end
  | "computable_age" ->
      if p_auth then
        match Adef.od_of_cdate (get_birth p), get_death p with
          Some (Dgreg (d, _)), NotDead ->
            not (d.day = 0 && d.month = 0 && d.prec <> Sure)
        | _ -> false
      else false
  | "computable_death_age" ->
      if p_auth then
        match Date.get_birth_death_date p with
          Some (Dgreg (({prec = Sure | About | Maybe} as d1), _)),
          Some (Dgreg (({prec = Sure | About | Maybe} as d2), _)),
          _
          when d1 <> d2 ->
            let a = Date.time_elapsed d1 d2 in
            a.year > 0 ||
            a.year = 0 && (a.month > 0 || a.month = 0 && a.day > 0)
        | _ -> false
      else false
  | "computable_marriage_age" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          if m_auth then
            match
              Adef.od_of_cdate (get_birth p),
              Adef.od_of_cdate (get_marriage fam)
            with
              Some (Dgreg (({prec = Sure | About | Maybe} as d1), _)),
              Some (Dgreg (({prec = Sure | About | Maybe} as d2), _)) ->
                let a = Date.time_elapsed d1 d2 in
                a.year > 0 ||
                a.year = 0 && (a.month > 0 || a.month = 0 && a.day > 0)
            | _ -> false
          else false
      | _ -> raise Not_found
      end
  | "has_approx_birth_date" ->
      p_auth && fst (Util.get_approx_birth_date_place conf base p) <> None
  | "has_approx_birth_place" ->
      p_auth && snd (Util.get_approx_birth_date_place conf base p) <> ""
  | "has_approx_death_date" ->
      p_auth && fst (Util.get_approx_death_date_place conf base p) <> None
  | "has_approx_death_place" ->
      p_auth && snd (Util.get_approx_death_date_place conf base p) <> ""
  | "has_aliases" ->
      if not p_auth && is_hide_names conf p then false
      else get_aliases p <> []
  | "has_baptism_date" -> p_auth && get_baptism p <> Adef.cdate_None
  | "has_baptism_place" -> p_auth && sou base (get_baptism_place p) <> ""
  | "has_baptism_source" -> p_auth && sou base (get_baptism_src p) <> ""
  | "has_baptism_note" ->
      p_auth && not conf.no_note && sou base (get_baptism_note p) <> ""
  | "has_baptism_witnesses" ->
      let rec loop pevents =
        match pevents with
          [] -> false
        | (name, _, _, _, _, wl, _) :: events ->
            if name = Pevent Epers_Baptism then Array.length wl > 0
            else loop events
      in
      p_auth && loop (events_list conf base p)
  | "has_birth_date" -> p_auth && get_birth p <> Adef.cdate_None
  | "has_birth_place" -> p_auth && sou base (get_birth_place p) <> ""
  | "has_birth_source" -> p_auth && sou base (get_birth_src p) <> ""
  | "has_birth_note" ->
      p_auth && not conf.no_note && sou base (get_birth_note p) <> ""
  | "has_birth_witnesses" ->
      let rec loop pevents =
        match pevents with
          [] -> false
        | (name, _, _, _, _, wl, _) :: events ->
            if name = Pevent Epers_Birth then Array.length wl > 0
            else loop events
      in
      p_auth && loop (events_list conf base p)
  | "has_burial_date" ->
      if p_auth then
        match get_burial p with
          Buried cod -> Adef.od_of_cdate cod <> None
        | _ -> false
      else false
  | "has_burial_place" -> p_auth && sou base (get_burial_place p) <> ""
  | "has_burial_source" -> p_auth && sou base (get_burial_src p) <> ""
  | "has_burial_note" ->
      p_auth && not conf.no_note && sou base (get_burial_note p) <> ""
  | "has_burial_witnesses" ->
      let rec loop pevents =
        match pevents with
          [] -> false
        | (name, _, _, _, _, wl, _) :: events ->
            if name = Pevent Epers_Burial then Array.length wl > 0
            else loop events
      in
      p_auth && loop (events_list conf base p)
  | "has_children" ->
      (* TODO ???
      let mode_local =
        match get_env "fam_link" env with
        [ Vfam ifam _ (_, _, ip) _ -> False
        | _ -> True ]
      in*)
      begin match get_env "fam" env with
        Vfam (_, fam, _, _) ->
          if Array.length (get_children fam) > 0 then true
          else
#ifdef API
            let faml =
              Perso_link.get_family_link conf.command (get_iper p)
            in
            let rec loop faml =
              match faml with
                [] -> false
              | fam_link :: faml ->
                  let (ifath, imoth, _) =
                    Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath,
                    Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth,
                    Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifam
                  in
                  let cpl =
                    let ip = get_iper p in
                    if ip <> ifath && ip <> imoth then
                      match
                        Perso_link.get_person_link_with_base conf.command ip
                          fam_link.MLink.Family.baseprefix
                      with
                        Some p ->
                          let ip = Gwdb.iper_of_string @@ Int32.to_string p.MLink.Person.ip in
                          ifath, imoth, (if ip = ifath then imoth else ifath)
                      | None ->
                          ifath, imoth, (if ip = ifath then imoth else ifath)
                    else ifath, imoth, (if ip = ifath then imoth else ifath)
                  in
                  let can_merge =
                    Perso_link.can_merge_family conf.command (get_iper p)
                      [fam] fam_link cpl
                  in
                  if can_merge then
                    let (ifam, ifath, imoth) =
                      Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam,
                      Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath,
                      Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth
                    in
                    List.length
                      (Perso_link.get_children_of_parents
                         fam_link.MLink.Family.baseprefix ifam ifath imoth) >
                      0
                  else loop faml
            in
            loop faml
#else
            false
#endif
      | _ ->
          let b =
            Array.exists
              (fun ifam ->
                 let des = foi base ifam in
                 Array.length (get_children des) > 0)
              (get_family p)
          in
          if b then b
          else
#ifdef API
            match get_env "fam_link" env with
              Vfam (ifam, _, (ifath, imoth, _), _) ->
                let conf =
                  match get_env "baseprefix" env with
                    Vstring baseprefix -> {conf with command = baseprefix}
                  | _ -> conf
                in
                List.length
                  (Perso_link.get_children_of_parents conf.command ifam ifath
                     imoth) >
                  0
            | _ -> false
#else
            false
#endif
      end
  | "has_consanguinity" ->
      p_auth && get_consang p != Adef.fix (-1) &&
      get_consang p >= Adef.fix_of_float 0.0001
  | "has_cremation_date" ->
      if p_auth then
        match get_burial p with
          Cremated cod -> Adef.od_of_cdate cod <> None
        | _ -> false
      else false
  | "has_cremation_place" -> p_auth && sou base (get_burial_place p) <> ""
  | "has_cremation_witnesses" ->
      let rec loop pevents =
        match pevents with
          [] -> false
        | (name, _, _, _, _, wl, _) :: events ->
            if name = Pevent Epers_Cremation then Array.length wl > 0
            else loop events
      in
      p_auth && loop (events_list conf base p)
  | "has_death_date" ->
      begin match get_death p with
        Death (_, _) -> p_auth
      | _ -> false
      end
  | "has_death_place" -> p_auth && sou base (get_death_place p) <> ""
  | "has_death_source" -> p_auth && sou base (get_death_src p) <> ""
  | "has_death_note" ->
      p_auth && not conf.no_note && sou base (get_death_note p) <> ""
  | "has_death_witnesses" ->
      let rec loop pevents =
        match pevents with
          [] -> false
        | (name, _, _, _, _, wl, _) :: events ->
            if name = Pevent Epers_Death then Array.length wl > 0
            else loop events
      in
      p_auth && loop (events_list conf base p)
  | "has_event" ->
      if p_auth then
        let events = events_list conf base p in
        let nb_fam = Array.length (get_family p) in
        match p_getenv conf.base_env "has_events" with
        | Some "never" -> false
        | Some "always" ->
          if nb_fam > 0 || (List.length events) > 0 then true else false
        | _ ->
            (* Renvoie vrai que si il y a des informations supplémentaires *)
            (* par rapport aux évènements principaux, i.e. témoins (mais   *)
            (* on ne prend pas en compte les notes).                       *)
            let rec loop events nb_birth nb_bapt nb_deat nb_buri nb_marr =
              match events with
                [] -> false
              | (name, _, p, n, s, wl, _) :: events ->
                  let (p, n, s) = sou base p, sou base n, sou base s in
                  match name with
                    Pevent pname ->
                      begin match pname with
                        Epers_Birth | Epers_Baptism | Epers_Death |
                        Epers_Burial | Epers_Cremation ->
                          if Array.length wl > 0 then true
                          else
                            let (nb_birth, nb_bapt, nb_deat, nb_buri) =
                              match pname with
                                Epers_Birth ->
                                  succ nb_birth, nb_bapt, nb_deat, nb_buri
                              | Epers_Baptism ->
                                  nb_birth, succ nb_bapt, nb_deat, nb_buri
                              | Epers_Death ->
                                  nb_birth, nb_bapt, succ nb_deat, nb_buri
                              | Epers_Burial | Epers_Cremation ->
                                  nb_birth, nb_bapt, nb_deat, succ nb_buri
                              | _ -> nb_birth, nb_bapt, nb_deat, nb_buri
                            in
                            if List.exists (fun i -> i > 1)
                                 [nb_birth; nb_bapt; nb_deat; nb_buri]
                            then
                              true
                            else
                              loop events nb_birth nb_bapt nb_deat nb_buri
                                nb_marr
                      | _ -> true
                      end
                  | Fevent fname ->
                      match fname with
                        Efam_Engage | Efam_Marriage | Efam_NoMention |
                        Efam_NoMarriage ->
                          let nb_marr = succ nb_marr in
                          if nb_marr > nb_fam then true
                          else
                            loop events nb_birth nb_bapt nb_deat nb_buri
                              nb_marr
                      | Efam_Divorce | Efam_Separated ->
                          if p <> "" || n <> "" || s <> "" ||
                             Array.length wl > 0
                          then
                            true
                          else
                            loop events nb_birth nb_bapt nb_deat nb_buri
                              nb_marr
                      | _ -> true
            in
            loop events 0 0 0 0 0
      else false
  | "has_families" ->
      Array.length (get_family p) > 0
#ifdef API
      ||
      List.length
        (Perso_link.get_family_correspondance conf.command
           (get_iper p)) >
        0
#endif
  | "has_first_names_aliases" ->
      if not p_auth && is_hide_names conf p then false
      else get_first_names_aliases p <> []
  | "has_history" -> has_history conf base p p_auth
  | "has_image" -> Util.has_image conf base p
  | "has_nephews_or_nieces" -> has_nephews_or_nieces conf base p
  | "has_nobility_titles" -> p_auth && nobtit conf base p <> []
  | "has_notes" | "has_pnotes" ->
      p_auth && not conf.no_note && sou base (get_notes p) <> ""
  | "has_occupation" -> p_auth && sou base (get_occupation p) <> ""
  | "has_parents" ->
      get_parents p <> None
#ifdef API
      ||
      (let conf =
         match get_env "baseprefix" env with
           Vstring baseprefix -> {conf with command = baseprefix}
         | _ -> conf
       in
       Perso_link.get_parents_link conf.command (get_iper p) <> None)
#endif
  | "has_possible_duplications" -> has_possible_duplications conf base p
  | "has_psources" ->
      if is_hide_names conf p && not p_auth then false
      else sou base (get_psources p) <> ""
  | "has_public_name" ->
      if not p_auth && is_hide_names conf p then false
      else sou base (get_public_name p) <> ""
  | "has_qualifiers" ->
      if not p_auth && is_hide_names conf p then false
      else get_qualifiers p <> []
  | "has_relations" ->
      if p_auth && conf.use_restrict then
        let related =
          List.fold_left
            (fun l ip ->
               let rp = pget conf base ip in
               if is_hidden rp then l else ip :: l)
            [] (get_related p)
        in
        get_rparents p <> [] || related <> []
      else p_auth && (get_rparents p <> [] || get_related p <> [])
  | "has_siblings" ->
      begin match get_parents p with
        Some ifam -> Array.length (get_children (foi base ifam)) > 1
      | None ->
#ifdef API
          let conf =
            match get_env "baseprefix" env with
              Vstring baseprefix -> {conf with command = baseprefix}
            | _ -> conf
          in
          match
            Perso_link.get_parents_link conf.command (get_iper p)
          with
            Some family -> List.length family.MLink.Family.children > 1
          | None -> false
#else
          false
#endif
      end
  | "has_sources" ->
      p_auth &&
      (sou base (get_psources p) <> "" || sou base (get_birth_src p) <> "" ||
       sou base (get_baptism_src p) <> "" ||
       sou base (get_death_src p) <> "" ||
       sou base (get_burial_src p) <> "" ||
       Array.exists
         (fun ifam ->
            let fam = foi base ifam in
            let isp = Gutil.spouse (get_iper p) fam in
            let sp = poi base isp in
            (* On sait que p_auth vaut vrai. *)
            let m_auth = authorized_age conf base sp in
            m_auth &&
            (sou base (get_marriage_src fam) <> "" ||
             sou base (get_fsources fam) <> ""))
         (get_family p))
  | "has_surnames_aliases" ->
      if not p_auth && is_hide_names conf p then false
      else get_surnames_aliases p <> []
  | "is_buried" ->
      begin match get_burial p with
        Buried _ -> p_auth
      | _ -> false
      end
  | "is_cremated" ->
      begin match get_burial p with
        Cremated _ -> p_auth
      | _ -> false
      end
  | "is_dead" ->
      begin match get_death p with
        Death (_, _) | DeadYoung | DeadDontKnowWhen -> p_auth
      | _ -> false
      end
  | "is_certainly_dead" ->
      begin match get_death p with
        OfCourseDead -> p_auth
      | _ -> false
      end
  | "is_descendant" ->
      begin match get_env "desc_mark" env with
        Vdmark r -> (Gwdb.Marker.get !r (get_iper p))
      | _ -> raise Not_found
      end
  | "is_female" -> get_sex p = Female
  | "is_invisible" ->
      let conf = {conf with wizard = false; friend = false} in
      not (authorized_age conf base p)
  | "is_male" -> get_sex p = Male
  | "is_private" -> get_access p = Private
  | "is_public" -> get_access p = Public
  | "is_restricted" -> is_hidden p
  | _ -> raise Not_found
and eval_str_person_field conf base env (p, p_auth as ep) =
  function
    "access" -> acces conf base p
  | "age" ->
      begin match p_auth, Adef.od_of_cdate (get_birth p), get_death p with
        true, Some (Dgreg (d, _)), NotDead ->
          let a = Date.time_elapsed d conf.today in
          DateDisplay.string_of_age conf a
      | _ -> ""
      end
  | "alias" ->
      begin match get_aliases p with
        nn :: _ ->
          if not p_auth && is_hide_names conf p then "" else sou base nn
      | _ -> ""
      end
  | "approx_birth_place" ->
      begin match
        p_auth, snd (Util.get_approx_birth_date_place conf base p)
      with
        true, place -> place
      | _ -> ""
      end
  | "approx_death_place" ->
      begin match
        p_auth, snd (Util.get_approx_death_date_place conf base p)
      with
        true, place -> place
      | _ -> ""
      end
  | "auto_image_file_name" ->
      begin match auto_image_file conf base p with
        Some s when p_auth -> s
      | _ -> ""
      end
  | "bname_prefix" -> Util.commd conf
  | "birth_place" ->
      if p_auth then Util.string_of_place conf (sou base (get_birth_place p))
      else ""
  | "birth_note" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth conf.no_note
        (sou base (get_birth_note p))
  | "birth_source" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth false
        (sou base (get_birth_src p))
  | "baptism_place" ->
      if p_auth then
        Util.string_of_place conf (sou base (get_baptism_place p))
      else ""
  | "baptism_note" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth conf.no_note
        (sou base (get_baptism_note p))
  | "baptism_source" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth false
        (sou base (get_baptism_src p))
  | "burial_place" ->
      if p_auth then Util.string_of_place conf (sou base (get_burial_place p))
      else ""
  | "burial_note" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth conf.no_note
        (sou base (get_burial_note p))
  | "burial_source" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth false
        (sou base (get_burial_src p))
  | "child_name" ->
      let force_surname =
        match get_parents p with
          None -> false
        | Some ifam ->
            p_surname base (pget conf base (get_father (foi base ifam))) <>
              p_surname base p
      in
      if not p_auth && is_hide_names conf p then "x x"
      else if force_surname then person_text conf base p
      else person_text_no_surn_no_acc_chk conf base p
  | "consanguinity" ->
      if p_auth then
        string_of_decimal_num conf
          (round_2_dec (Adef.float_of_fix (get_consang p) *. 100.0)) ^
        " %"
      else ""
  | "cremation_place" ->
      if p_auth then Util.string_of_place conf (sou base (get_burial_place p))
      else ""
  | "dates" -> if p_auth then DateDisplay.short_dates_text conf base p else ""
  | "death_age" ->
      if p_auth then
        match Date.get_birth_death_date p with
          Some (Dgreg (({prec = Sure | About | Maybe} as d1), _)),
          Some (Dgreg (({prec = Sure | About | Maybe} as d2), _)), approx
          when d1 <> d2 ->
            let a = Date.time_elapsed d1 d2 in
            let s =
              if not approx && d1.prec = Sure && d2.prec = Sure then ""
              else transl_decline conf "possibly (date)" "" ^ " "
            in
            s ^ DateDisplay.string_of_age conf a
        | _ -> ""
      else ""
  | "death_place" ->
      if p_auth then Util.string_of_place conf (sou base (get_death_place p))
      else ""
  | "death_note" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth conf.no_note
        (sou base (get_death_note p))
  | "death_source" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth false
        (sou base (get_death_src p))
  | "died" -> string_of_died conf p p_auth
  | "fam_access" ->
      (* deprecated since 5.00: rather use "i=%family.index;&ip=%index;" *)
      begin match get_env "fam" env with
        Vfam (ifam, _, _, _) ->
          Printf.sprintf "i=%s&ip=%s" (string_of_ifam ifam)
            (string_of_iper (get_iper p))
      | _ -> raise Not_found
      end
  | "father_age_at_birth" -> string_of_parent_age conf base ep get_father
  | "first_name" ->
      if not p_auth && is_hide_names conf p then "x" else p_first_name base p
  | "first_name_key" ->
      if is_hide_names conf p && not p_auth then ""
      else code_varenv (Name.lower (p_first_name base p))
  | "first_name_key_val" ->
      if is_hide_names conf p && not p_auth then ""
      else Name.lower (p_first_name base p)
  | "first_name_key_strip" ->
      if is_hide_names conf p && not p_auth then ""
      else Name.strip_c (p_first_name base p) '"'
  | "history_file" ->
      if not p_auth then ""
      else
        let fn = sou base (get_first_name p) in
        let sn = sou base (get_surname p) in
        let occ = get_occ p in HistoryDiff.history_file fn sn occ
  | "image" -> if not p_auth then "" else sou base (get_image p)
  | "image_html_url" -> string_of_image_url conf base ep true
  | "image_size" -> string_of_image_size conf base ep
  | "image_medium_size" -> string_of_image_medium_size conf base ep
  | "image_small_size" -> string_of_image_small_size conf base ep
  | "image_url" -> string_of_image_url conf base ep false
  | "ind_access" ->
      (* deprecated since 5.00: rather use "i=%index;" *)
      "i=" ^ string_of_iper (get_iper p)
  | "index" ->
      begin match get_env "p_link" env with
        Vbool _ -> ""
      | _ -> string_of_iper (get_iper p)
      end
  | "mark_descendants" ->
      begin match get_env "desc_mark" env with
        Vdmark r ->
          let tab = Gwdb.iper_marker (Gwdb.ipers base) false in
          let rec mark_descendants len p =
            let i = (get_iper p) in
            if Gwdb.Marker.get tab i then ()
            else
              begin
                Gwdb.Marker.set tab i true;
                let u = p in
                for i = 0 to Array.length (get_family u) - 1 do
                  let des = foi base (get_family u).(i) in
                  for i = 0 to Array.length (get_children des) - 1 do
                    mark_descendants (len + 1)
                      (pget conf base (get_children des).(i))
                  done
                done
              end
          in
          mark_descendants 0 p; r := tab; ""
      | _ -> raise Not_found
      end
  | "marriage_age" ->
      begin match get_env "fam" env with
        Vfam (_, fam, _, m_auth) ->
          if m_auth then
            match
              Adef.od_of_cdate (get_birth p),
              Adef.od_of_cdate (get_marriage fam)
            with
              Some (Dgreg (({prec = Sure | About | Maybe} as d1), _)),
              Some (Dgreg (({prec = Sure | About | Maybe} as d2), _)) ->
                let a = Date.time_elapsed d1 d2 in
                DateDisplay.string_of_age conf a
            | _ -> ""
          else ""
      | _ -> raise Not_found
      end
  | "mother_age_at_birth" -> string_of_parent_age conf base ep get_mother
  | "misc_names" ->
      if p_auth then
        let list = Gwdb.person_misc_names base p (Util.nobtit conf base) in
        let list =
          let first_name = p_first_name base p in
          let surname = p_surname base p in
          if first_name <> "?" && surname <> "?" then
            Name.lower (first_name ^ " " ^ surname) :: list
          else list
        in
        if list <> [] then
          "<ul>\n" ^
          List.fold_left (fun s n -> s ^ "<li>" ^ n ^ "</li>\n") "" list ^
          "</ul>\n"
        else ""
      else ""
  | "nb_children_total" ->
      let n =
        Array.fold_left
          (fun n ifam -> n + Array.length (get_children (foi base ifam))) 0
          (get_family p)
      in
      string_of_int n
  | "nb_children" ->
      (* TODO ???
      let mode_local =
        match get_env "fam_link" env with
        [ Vfam ifam _ (_, _, ip) _ -> False
        | _ -> True ]
      in*)
      begin match get_env "fam" env with
        Vfam (_, fam, _, _) -> string_of_int (Array.length (get_children fam))
      | _ ->
#ifdef API
          match get_env "fam_link" env with
            Vfam (ifam, _, _, _) ->
              let conf =
                match get_env "baseprefix" env with
                  Vstring baseprefix -> {conf with command = baseprefix}
                | _ -> conf
              in
              let children =
                Perso_link.get_children_of_fam conf.command ifam
              in
              string_of_int (List.length children)
          | _ ->
              let n =
                Array.fold_left
                  (fun n ifam ->
                     n + Array.length (get_children (foi base ifam)))
                  0 (get_family p)
              in
              string_of_int n
#else
          let n =
            Array.fold_left
              (fun n ifam ->
                 n + Array.length (get_children (foi base ifam)))
              0 (get_family p)
          in
          string_of_int n
#endif
      end
  | "nb_families" ->
      begin match get_env "p_link" env with
        Vbool _ ->
#ifdef API
          string_of_int
            (List.length
               (Perso_link.get_family_correspondance conf.command
                  (get_iper p)))
#else
      "0"
#endif
      | _ -> string_of_int (Array.length (get_family p))
      end
  | "notes" | "pnotes" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth conf.no_note
        (sou base (get_notes p))
  | "occ" ->
      if is_hide_names conf p && not p_auth then ""
      else string_of_int (get_occ p)
  | "occupation" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth false
        (sou base (get_occupation p))
  | "on_baptism_date" ->
      begin match p_auth, Adef.od_of_cdate (get_baptism p) with
        true, Some d ->
          begin match p_getenv conf.base_env "long_date" with
            Some "yes" -> DateDisplay.string_of_ondate conf d ^ DateDisplay.get_wday conf d
          | _ -> DateDisplay.string_of_ondate conf d
          end
      | _ -> ""
      end
  | "slash_baptism_date" ->
      begin match p_auth, Adef.od_of_cdate (get_baptism p) with
        true, Some d -> DateDisplay.string_slash_of_date conf d
      | _ -> ""
      end
  | "on_birth_date" ->
      begin match p_auth, Adef.od_of_cdate (get_birth p) with
        true, Some d ->
          begin match p_getenv conf.base_env "long_date" with
            Some "yes" -> DateDisplay.string_of_ondate conf d ^ DateDisplay.get_wday conf d
          | _ -> DateDisplay.string_of_ondate conf d
          end
      | _ -> ""
      end
  | "slash_birth_date" ->
      begin match p_auth, Adef.od_of_cdate (get_birth p) with
        true, Some d -> DateDisplay.string_slash_of_date conf d
      | _ -> ""
      end
  | "slash_approx_birth_date" ->
      begin match
        p_auth, fst (Util.get_approx_birth_date_place conf base p)
      with
        true, Some d -> DateDisplay.string_slash_of_date conf d
      | _ -> ""
      end
  | "on_burial_date" ->
      begin match get_burial p with
        Buried cod ->
          begin match p_auth, Adef.od_of_cdate cod with
            true, Some d ->
              begin match p_getenv conf.base_env "long_date" with
                Some "yes" ->
                  DateDisplay.string_of_ondate conf d ^ DateDisplay.get_wday conf d
              | _ -> DateDisplay.string_of_ondate conf d
              end
          | _ -> ""
          end
      | _ -> raise Not_found
      end
  | "psources" ->
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      get_note_source conf base env p_auth false
        (sou base (get_psources p))
  | "slash_burial_date" ->
      begin match get_burial p with
        Buried cod ->
          begin match p_auth, Adef.od_of_cdate cod with
            true, Some d -> DateDisplay.string_slash_of_date conf d
          | _ -> ""
          end
      | _ -> raise Not_found
      end
  | "on_cremation_date" ->
      begin match get_burial p with
        Cremated cod ->
          begin match p_auth, Adef.od_of_cdate cod with
            true, Some d ->
              begin match p_getenv conf.base_env "long_date" with
                Some "yes" ->
                  DateDisplay.string_of_ondate conf d ^ DateDisplay.get_wday conf d
              | _ -> DateDisplay.string_of_ondate conf d
              end
          | _ -> ""
          end
      | _ -> raise Not_found
      end
  | "slash_cremation_date" ->
      begin match get_burial p with
        Cremated cod ->
          begin match p_auth, Adef.od_of_cdate cod with
            true, Some d -> DateDisplay.string_slash_of_date conf d
          | _ -> ""
          end
      | _ -> raise Not_found
      end
  | "on_death_date" ->
      begin match p_auth, get_death p with
        true, Death (_, d) ->
          let d = Adef.date_of_cdate d in
          begin match p_getenv conf.base_env "long_date" with
            Some "yes" -> DateDisplay.string_of_ondate conf d ^ DateDisplay.get_wday conf d
          | _ -> DateDisplay.string_of_ondate conf d
          end
      | _ -> ""
      end
  | "slash_death_date" ->
      begin match p_auth, get_death p with
        true, Death (_, d) ->
          let d = Adef.date_of_cdate d in DateDisplay.string_slash_of_date conf d
      | _ -> ""
      end
  | "slash_approx_death_date" ->
      begin match
        p_auth, fst (Util.get_approx_death_date_place conf base p)
      with
        true, Some d -> DateDisplay.string_slash_of_date conf d
      | _ -> ""
      end
  | "prev_fam_father" ->
      begin match get_env "prev_fam" env with
        Vfam (_, _, (ifath, _, _), _) ->
          string_of_iper ifath
      | _ -> raise Not_found
      end
  | "prev_fam_index" ->
      begin match get_env "prev_fam" env with
        Vfam (ifam, _, _, _) -> string_of_ifam ifam
      | _ -> raise Not_found
      end
  | "prev_fam_mother" ->
      begin match get_env "prev_fam" env with
        Vfam (_, _, (_, imoth, _), _) ->
          string_of_iper imoth
      | _ -> raise Not_found
      end
  | "public_name" ->
      if not p_auth && is_hide_names conf p then ""
      else sou base (get_public_name p)
  | "qualifier" ->
      begin match get_qualifiers p with
        nn :: _ ->
          if not p_auth && is_hide_names conf p then "" else sou base nn
      | _ -> ""
      end
  | "sex" ->
      (* Pour éviter les traductions bizarre, on ne teste pas p_auth. *)
      string_of_int (index_of_sex (get_sex p))
  | "sosa_in_list" ->
      begin match get_env "all_gp" env with
        Vallgp all_gp ->
          begin match get_link all_gp (get_iper p) with
            Some (GP_person (s, _, _)) -> Sosa.to_string s
          | _ -> ""
          end
      | _ -> raise Not_found
      end
  | "sosa_link" ->
      begin match get_env "sosa" env with
        Vsosa x ->
          begin match get_sosa conf base env x p with
            Some (n, q) ->
              Printf.sprintf "m=RL&i1=%s&i2=%s&b1=1&b2=%s"
                (string_of_iper (get_iper p))
                (string_of_iper (get_iper q))
                (Sosa.to_string n)
          | None -> ""
          end
      | _ -> raise Not_found
      end
  | "source" ->
      begin match get_env "src" env with
        Vstring s ->
          let env = ['i', (fun () -> Util.default_image_name base p)] in
          let s =
            let wi =
              {Wiki.wi_mode = "NOTES";
               Wiki.wi_cancel_links = conf.cancel_links;
               Wiki.wi_file_path = Notes.file_path conf base;
               Wiki.wi_person_exists = person_exists conf base;
               Wiki.wi_always_show_link = conf.wizard || conf.friend}
            in
            Wiki.syntax_links conf wi s
          in
          Util.safe_html @@ string_with_macros conf env s
      | _ -> raise Not_found
      end
  | "surname" ->
      if not p_auth && is_hide_names conf p then "x" else p_surname base p
  | "surname_begin" ->
      if not p_auth && is_hide_names conf p then ""
      else surname_particle base (p_surname base p)
  | "surname_end" ->
      if not p_auth && is_hide_names conf p then "x"
      else surname_without_particle base (p_surname base p)
  | "surname_key" ->
      if is_hide_names conf p && not p_auth then ""
      else code_varenv (Name.lower (p_surname base p))
  | "surname_key_val" ->
      if is_hide_names conf p && not p_auth then ""
      else Name.lower (p_surname base p)
  | "surname_key_strip" ->
      if is_hide_names conf p && not p_auth then ""
      else Name.strip_c (p_surname base p) '"'
  | "title" -> person_title conf base p
  | _ -> raise Not_found
and eval_witness_relation_var conf base env
    (_, _, (ip1, ip2, _), m_auth as fcd) loc =
  function
    [] ->
      if not m_auth then VVstring ""
      else
        let s =
          Printf.sprintf (ftransl conf "witness at marriage of %s and %s")
            (referenced_person_title_text conf base (pget conf base ip1))
            (referenced_person_title_text conf base (pget conf base ip2))
        in
        VVstring s
  | sl -> eval_family_field_var conf base env fcd loc sl
and eval_family_field_var conf base env
    (_, fam, (ifath, imoth, _), m_auth as fcd) loc =
  function
    "father" :: sl ->
      begin match get_env "f_link" env with
        Vbool _ -> raise Not_found
      | _ ->
          let ep = make_ep conf base ifath in
          eval_person_field_var conf base env ep loc sl
      end
  | "marriage_date" :: sl ->
      begin match Adef.od_of_cdate (get_marriage fam) with
        Some d when m_auth -> eval_date_field_var conf d sl
      | _ -> VVstring ""
      end
  | "mother" :: sl ->
      begin match get_env "f_link" env with
        Vbool _ -> raise Not_found
      | _ ->
          let ep = make_ep conf base imoth in
          eval_person_field_var conf base env ep loc sl
      end
  | [s] -> str_val (eval_str_family_field env fcd s)
  | _ -> raise Not_found
and eval_str_family_field env (ifam, _, _, _) =
  function
    "desc_level" ->
      begin match get_env "desc_level_table" env with
        Vdesclevtab levt ->
          let (_, flevt) = Lazy.force levt in
          string_of_int (Gwdb.Marker.get flevt ifam)
      | _ -> raise Not_found
      end
  | "index" -> string_of_ifam ifam
  | "set_infinite_desc_level" ->
      begin match get_env "desc_level_table" env with
        Vdesclevtab levt ->
          let (_, flevt) = Lazy.force levt in
          Gwdb.Marker.set flevt ifam infinite; ""
      | _ -> raise Not_found
      end
  | _ -> raise Not_found
and simple_person_text conf base p p_auth =
  if p_auth then
    match main_title conf base p with
      Some t -> titled_person_text conf base p t
    | None -> person_text conf base p
  else if is_hide_names conf p then "x x"
  else person_text conf base p
and string_of_died conf p p_auth =
  if p_auth then
    let is = index_of_sex (get_sex p) in
    match get_death p with
      Death (dr, _) ->
        begin match dr with
          Unspecified -> transl_nth conf "died" is
        | Murdered -> transl_nth conf "murdered" is
        | Killed -> transl_nth conf "killed (in action)" is
        | Executed -> transl_nth conf "executed (legally killed)" is
        | Disappeared -> transl_nth conf "disappeared" is
        end
    | DeadYoung -> transl_nth conf "died young" is
    | DeadDontKnowWhen -> transl_nth conf "died" is
    | _ -> ""
  else ""
and string_of_image_url conf base (p, p_auth) html =
  if p_auth then
    let v =
      image_and_size conf base p (limited_image_size max_im_wid max_im_wid)
    in
    match v with
      Some (true, fname, _) ->
        let s = Unix.stat fname in
        let b = acces conf base p in
        let k = default_image_name base p in
        Format.sprintf "%sm=IM%s&d=%d&%s&k=/%s" (commd conf)
          (if html then "H" else "")
          (int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int))) b
          k
    | Some (false, link, _) -> link
    | None -> ""
  else ""
and string_of_parent_age conf base (p, p_auth) parent =
  match get_parents p with
    Some ifam ->
      let cpl = foi base ifam in
      let pp = pget conf base (parent cpl) in
      if p_auth && authorized_age conf base pp then
        match
          Adef.od_of_cdate (get_birth pp), Adef.od_of_cdate (get_birth p)
        with
          Some (Dgreg (d1, _)), Some (Dgreg (d2, _)) ->
            DateDisplay.string_of_age conf (Date.time_elapsed d1 d2)
        | _ -> ""
      else ""
  | None -> raise Not_found
and string_of_int_env var env =
  match get_env var env with
    Vint x -> string_of_int x
  | _ -> raise Not_found
and obsolete_eval conf base env (p, _) loc =
  function
    "married_to" ->
      let s =
        match get_env "fam" env with
          Vfam (_, fam, _, m_auth) ->
            let format = relation_txt conf (get_sex p) fam in
            Printf.sprintf (fcapitale format)
              (fun _ ->
                 if m_auth then string_of_marriage_text conf base fam else "")
        | _ -> raise Not_found
      in
      obsolete loc "4.08" "married_to" "" (str_val s)
  | _ -> raise Not_found

let eval_transl conf base env upp s c =
  match c with
    "n" | "s" | "w" | "f" | "c" ->
      let n =
        match c with
          "n" ->
            (* replaced by %apply;nth([...],sex) *)
            begin match get_env "p" env with
              Vind p -> 1 - index_of_sex (get_sex p)
            | _ -> 2
            end
        | "s" ->
            begin match get_env "child" env with
              Vind p -> index_of_sex (get_sex p)
            | _ ->
                match get_env "p" env with
                  Vind p -> index_of_sex (get_sex p)
                | _ -> 2
            end
        | "w" ->
            begin match get_env "fam" env with
              Vfam (_, fam, _, _) ->
                if Array.length (get_witnesses fam) <= 1 then 0 else 1
            | _ -> 0
            end
        | "f" ->
            begin match get_env "p" env with
              Vind p -> if Array.length (get_family p) <= 1 then 0 else 1
            | _ -> 0
            end
        | "c" ->
            begin match get_env "fam" env with
              Vfam (_, fam, _, _) ->
                if Array.length (get_children fam) <= 1 then 0 else 1
            | _ ->
                match get_env "p" env with
                  Vind p ->
                    let n =
                      Array.fold_left
                        (fun n ifam ->
                           n + Array.length (get_children (foi base ifam)))
                        0 (get_family p)
                    in
                    if n <= 1 then 0 else 1
                | _ -> 0
            end
        | _ -> assert false
      in
      let r = Util.translate_eval (Util.transl_nth conf s n) in
      if upp then Utf8.capitalize r else r
  | _ -> Templ.eval_transl conf upp s c

let print_foreach conf base print_ast eval_expr =
  let eval_int_expr env ep e =
    let s = eval_expr env ep e in
    try int_of_string s with Failure _ -> raise Not_found
  in
  let rec print_foreach env ini_ep loc s sl ell al =
    let rec loop env (a, _ as ep) efam =
      function
        [s] -> print_simple_foreach env ell al ini_ep ep efam loc s
      | "ancestor" :: sl ->
          let ip_ifamo =
            match get_env "ancestor" env with
              Vanc (GP_person (_, ip, ifamo)) -> Some (ip, ifamo)
            | Vanc (GP_same (_, _, ip)) -> Some (ip, None)
            | _ -> None
          in
          begin match ip_ifamo with
            Some (ip, ifamo) ->
              let ep = make_ep conf base ip in
              let efam =
                match ifamo with
                  Some ifam ->
                    let (f, c, a) = make_efam conf base ip ifam in
                    Vfam (ifam, f, c, a)
                | None -> efam
              in
              loop env ep efam sl
          | _ -> raise Not_found
          end
      | "child" :: sl ->
          begin match get_env "child" env with
            Vind p ->
              let auth = authorized_age conf base p in
              let ep = p, auth in loop env ep efam sl
          | _ ->
              match get_env "child_link" env with
                Vind p ->
                  let env = ("p_link", Vbool true) :: env in
                  let env = ("f_link", Vbool true) :: env in
                  let auth = authorized_age conf base p in
                  let ep = p, auth in loop env ep efam sl
              | _ -> raise Not_found
          end
      | "father" :: sl ->
          begin match get_parents a with
            Some ifam ->
              let cpl = foi base ifam in
              let (_, p_auth as ep) = make_ep conf base (get_father cpl) in
              let ifath = get_father cpl in
              let cpl = ifath, get_mother cpl, ifath in
              let m_auth =
                p_auth && authorized_age conf base (pget conf base ifath)
              in
              let efam = Vfam (ifam, foi base ifam, cpl, m_auth) in
              loop env ep efam sl
          | None ->
#ifdef API
              let conf =
                match get_env "baseprefix" env with
                  Vstring baseprefix -> {conf with command = baseprefix}
                | _ -> conf
              in
              match
                Perso_link.get_parents_link conf.command (get_iper a)
              with
                Some family ->
                  let (ifath, imoth, ifam) =
                    Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.ifath,
                    Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.imoth,
                    Gwdb.ifam_of_string @@ Int32.to_string family.MLink.Family.ifam
                  in
                  begin match
                    Perso_link.get_person_link family.MLink.Family.baseprefix
                      ifath
                  with
                    Some fath ->
                      let ep = Perso_link.make_ep_link base fath in
                      let cpl = ifath, imoth, imoth in
                      let (_, fam, _, _) =
                        Perso_link.make_efam_link conf base family
                      in
                      let efam = Vfam (ifam, fam, cpl, true) in
                      let env = ("p_link", Vbool true) :: env in
                      let env = ("f_link", Vbool true) :: env in
                      let env =
                        ("baseprefix",
                         Vstring fath.MLink.Person.baseprefix) ::
                        env
                      in
                      loop env ep efam sl
                  | None ->
                      warning_use_has_parents_before_parent loc "father" ()
                  end
              | None -> warning_use_has_parents_before_parent loc "father" ()
#else
              warning_use_has_parents_before_parent loc "father" ()
#endif
          end
      | "mother" :: sl ->
          begin match get_parents a with
            Some ifam ->
              let cpl = foi base ifam in
              let (_, p_auth as ep) = make_ep conf base (get_mother cpl) in
              let ifath = get_father cpl in
              let cpl = ifath, get_mother cpl, ifath in
              let m_auth =
                p_auth && authorized_age conf base (pget conf base ifath)
              in
              let efam = Vfam (ifam, foi base ifam, cpl, m_auth) in
              loop env ep efam sl
          | None ->
#ifdef API
              let conf =
                match get_env "baseprefix" env with
                  Vstring baseprefix -> {conf with command = baseprefix}
                | _ -> conf
              in
              match
                Perso_link.get_parents_link conf.command (get_iper a)
              with
                Some family ->
                  let (ifath, imoth, ifam) =
                    Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.ifath,
                    Gwdb.iper_of_string @@ Int32.to_string family.MLink.Family.imoth,
                    Gwdb.ifam_of_string @@ Int32.to_string family.MLink.Family.ifam
                  in
                  begin match
                    Perso_link.get_person_link family.MLink.Family.baseprefix
                      imoth
                  with
                    Some moth ->
                      let ep = Perso_link.make_ep_link base moth in
                      let cpl = ifath, imoth, ifath in
                      let (_, fam, _, _) =
                        Perso_link.make_efam_link conf base family
                      in
                      let efam = Vfam (ifam, fam, cpl, true) in
                      let env = ("p_link", Vbool true) :: env in
                      let env = ("f_link", Vbool true) :: env in
                      let env =
                        ("baseprefix",
                         Vstring moth.MLink.Person.baseprefix) ::
                        env
                      in
                      loop env ep efam sl
                  | None ->
                      warning_use_has_parents_before_parent loc "father" ()
                  end
              | None -> warning_use_has_parents_before_parent loc "mother" ()
#else
              warning_use_has_parents_before_parent loc "mother" ()
#endif
          end
      | "self" :: sl -> loop env ep efam sl
      | "spouse" :: sl ->
          begin match efam with
            Vfam (_, _, (_, _, ip), _) ->
              let ep = make_ep conf base ip in loop env ep efam sl
          | _ ->
#ifdef API
              match get_env "fam_link" env with
                Vfam (_, _, (_, _, ip), _) ->
                  let baseprefix =
                    match get_env "baseprefix" env with
                      Vstring baseprefix -> baseprefix
                    | _ -> conf.command
                  in
                  begin match Perso_link.get_person_link baseprefix ip with
                    Some spouse ->
                      let ep = Perso_link.make_ep_link base spouse in
                      let env = ("p_link", Vbool true) :: env in
                      let env =
                        ("baseprefix",
                         Vstring spouse.MLink.Person.baseprefix) ::
                        env
                      in
                      loop env ep efam sl
                  | None -> raise Not_found
                  end
              | _ -> raise Not_found
#else
              raise Not_found
#endif
          end
      | _ -> raise Not_found
    in
    let efam =
      match get_env "is_link" env with
        Vbool _ -> get_env "fam_link" env
      | _ -> get_env "fam" env
    in
    loop env ini_ep efam (s :: sl)
  and print_simple_foreach env el al ini_ep ep efam loc =
    function
      "alias" -> print_foreach_alias env al ep
    | "ancestor" -> print_foreach_ancestor env al ep
    | "ancestor_level" -> print_foreach_ancestor_level env el al ep
    | "ancestor_level2" -> print_foreach_ancestor_level2 env al ep
    | "ancestor_surname" -> print_foreach_anc_surn env el al loc ep
    | "ancestor_tree_line" -> print_foreach_ancestor_tree env el al ep
    | "baptism_witness" -> print_foreach_baptism_witness env al ep
    | "birth_witness" -> print_foreach_birth_witness env al ep
    | "burial_witness" -> print_foreach_burial_witness env al ep
    | "cell" -> print_foreach_cell env al ep
    | "child" -> print_foreach_child env al ep efam
    | "cousin_level" -> print_foreach_level "max_cous_level" env al ep
    | "cremation_witness" -> print_foreach_cremation_witness env al ep
    | "death_witness" -> print_foreach_death_witness env al ep
    | "descendant_level" -> print_foreach_descendant_level env al ep
    | "event" -> print_foreach_event env al ep
    | "event_witness" -> print_foreach_event_witness env al ep
    | "event_witness_relation" ->
        print_foreach_event_witness_relation env al ep
    | "family" -> print_foreach_family env al ini_ep ep
    | "first_name_alias" -> print_foreach_first_name_alias env al ep
    | "nobility_title" -> print_foreach_nobility_title env al ep
    | "parent" -> print_foreach_parent env al ep
    | "qualifier" -> print_foreach_qualifier env al ep
    | "related" -> print_foreach_related env al ep
    | "relation" -> print_foreach_relation env al ep
    | "sorted_list_item" -> print_foreach_sorted_list_item env al ep
    | "sorted_listb_item" -> print_foreach_sorted_listb_item env al ep
    | "sorted_listc_item" -> print_foreach_sorted_listc_item env al ep
    | "source" -> print_foreach_source env al ep
    | "surname_alias" -> print_foreach_surname_alias env al ep
    | "witness" -> print_foreach_witness env al ep efam
    | "witness_relation" -> print_foreach_witness_relation env al ep
    | _ -> raise Not_found
  and print_foreach_alias env al (p, p_auth as ep) =
    if not p_auth && is_hide_names conf p then ()
    else
      Mutil.list_iter_first
        (fun first a ->
           let env = ("alias", Vstring (sou base a)) :: env in
           let env = ("first", Vbool first) :: env in
           List.iter (print_ast env ep) al)
        (get_aliases p)
  and print_foreach_ancestor env al ep =
    match get_env "gpl" env with
      Vgpl gpl ->
        let rec loop first gpl =
          match gpl with
            [] -> ()
          | gp :: gl ->
              begin match gp with
                GP_missing (_, _) -> ()
              | _ ->
                  let env =
                    ("ancestor", Vanc gp) :: ("first", Vbool first) ::
                    ("last", Vbool (gl = [])) :: env
                  in
                  List.iter (print_ast env ep) al
              end;
              loop false gl
        in
        loop true gpl
    | _ -> ()
  and print_foreach_ancestor_level env el al (p, _ as ep) =
    let max_level =
      match el with
        [[e]] -> eval_int_expr env ep e
      | [] ->
          begin match get_env "max_anc_level" env with
            Vint n -> n
          | _ -> 0
          end
      | _ -> raise Not_found
    in
    let mark = Gwdb.iper_marker (Gwdb.ipers base) Sosa.zero in
    let rec loop gpl i n =
      if i > max_level then ()
      else
        let n =
          List.fold_left
            (fun n gp ->
               match gp with
                 GP_person (_, _, _) -> n + 1
               | _ -> n)
            n gpl
        in
        let env =
          ("gpl", Vgpl gpl) :: ("level", Vint i) :: ("n", Vint n) :: env
        in
        List.iter (print_ast env ep) al;
        let gpl = next_generation conf base mark gpl in loop gpl (succ i) n
    in
    loop [GP_person (Sosa.one, get_iper p, None)] 1 0
  and print_foreach_ancestor_level2 env al (p, _ as ep) =
    let max_lev = "max_anc_level" in
    let max_level =
      match get_env max_lev env with
        Vint n -> n
      | _ -> 0
    in
    let mark = Gwdb.iper_marker (Gwdb.ipers base) Sosa.zero in
    let rec loop gpl i =
      if i > max_level then ()
      else
        let env = ("gpl", Vgpl gpl) :: ("level", Vint i) :: env in
        List.iter (print_ast env ep) al;
        Gwdb.Collection.iter (fun i -> Gwdb.Marker.set mark i Sosa.zero) (Gwdb.ipers base) ;
        let gpl = next_generation2 conf base mark gpl in loop gpl (succ i)
    in
    loop [GP_person (Sosa.one, get_iper p, None)] 1
  and print_foreach_anc_surn env el al loc (p, _ as ep) =
    let max_level =
      match el with
        [[e]] -> eval_int_expr env ep e
      | [] ->
          begin match get_env "max_anc_level" env with
            Vint n -> n
          | _ -> 0
          end
      | _ -> raise Not_found
    in
    (* En fonction du type de sortie demandé, on construit *)
    (* soit la liste des branches soit la liste éclair.    *)
    match p_getenv conf.env "t" with
      Some "E" ->
        let list = build_list_eclair conf base max_level p in
        List.iter
          (fun (a, b, c, d, e, f) ->
             let env =
               ("ancestor", Vanc_surn (Eclair (a, b, c, d, e, f, loc))) :: env
             in
             List.iter (print_ast env ep) al)
          list
    | Some "F" ->
        let list = build_surnames_list conf base max_level p in
        List.iter
          (fun (a, (((b, c, d), e), f)) ->
             let env =
               ("ancestor", Vanc_surn (Branch (a, b, c, d, e, f, loc))) :: env
             in
             List.iter (print_ast env ep) al)
          list
    | _ -> ()
  and print_foreach_ancestor_tree env el al (p, _ as ep) =
    let (p, max_level) =
      match el with
        [[e1]; [e2]] ->
          let ip = iper_of_string @@ eval_expr env ep e1 in
          let max_level = eval_int_expr env ep e2 in
          pget conf base ip, max_level
      | [[e]] -> p, eval_int_expr env ep e
      | [] ->
          begin match get_env "max_anc_level" env with
            Vint n -> p, n
          | _ -> p, 0
          end
      | _ -> raise Not_found
    in
    let gen = tree_generation_list conf base max_level p in
    let rec loop first =
      function
        g :: gl ->
          let env =
            ("celll", Vcelll g) :: ("first", Vbool first) ::
            ("last", Vbool (gl = [])) :: env
          in
          List.iter (print_ast env ep) al; loop false gl
      | [] -> ()
    in
    loop true gen
  and print_foreach_baptism_witness env al (p, _ as ep) =
    let rec loop pevents =
      match pevents with
        [] -> ()
      | (name, _, _, _, _, wl, _) :: events ->
          if name = Pevent Epers_Baptism then
            Array.iteri
              begin fun i (ip, _) ->
                let p = pget conf base ip in
                let env =
                  ("baptism_witness", Vind p)
                  :: ("first", Vbool (i = 0))
                  :: env
                in
                List.iter (print_ast env ep) al
              end
              wl
          else loop events
    in
    loop (events_list conf base p)
  and print_foreach_birth_witness env al (p, _ as ep) =
    let rec loop pevents =
      match pevents with
        [] -> ()
      | (name, _, _, _, _, wl, _) :: events ->
          if name = Pevent Epers_Birth then
            Array.iteri
              begin fun i (ip, _) ->
                let p = pget conf base ip in
                let env =
                  ("birth_witness", Vind p)
                  :: ("first", Vbool (i = 0))
                  :: env
                in
                List.iter (print_ast env ep) al
              end
              wl
          else loop events
    in
    loop (events_list conf base p)
  and print_foreach_burial_witness env al (p, _ as ep) =
    let rec loop pevents =
      match pevents with
        [] -> ()
      | (name, _, _, _, _, wl, _) :: events ->
          if name = Pevent Epers_Burial then
            Array.iteri
              begin fun i (ip, _) ->
                let p = pget conf base ip in
                let env =
                  ("burial_witness", Vind p)
                  :: ("first", Vbool (i = 0))
                  :: env
                in
                List.iter (print_ast env ep) al
              end
              wl
          else loop events
    in
    loop (events_list conf base p)
  and print_foreach_cell env al ep =
    let celll =
      match get_env "celll" env with
        Vcelll celll -> celll
      | _ -> raise Not_found
    in
    Mutil.list_iter_first
      (fun first cell ->
         let env = ("cell", Vcell cell) :: ("first", Vbool first) :: env in
         List.iter (print_ast env ep) al)
      celll
  and print_foreach_child env al ep =
    function
      Vfam (ifam, fam, (ifath, imoth, isp), _) ->
        begin match get_env "f_link" env with
          Vbool _ ->
#ifdef API
            (* On est sur une famille distante. *)
            let env = ("auth", Vbool true) :: env in
            let conf =
              match get_env "baseprefix" env with
                Vstring baseprefix -> {conf with command = baseprefix}
              | _ -> conf
            in
            let children =
              Perso_link.get_children_of_parents conf.command ifam ifath imoth
            in
            List.iter
              (fun c_link ->
                 let (p, _) = Perso_link.make_ep_link base c_link in
                 let baseprefix = c_link.MLink.Person.baseprefix in
                 let env = ("#loop", Vint 0) :: env in
                 let env = ("child_link", Vind p) :: env in
                 let env = ("baseprefix", Vstring baseprefix) :: env in
                 let env = ("p_link", Vbool true) :: env in
                 let ep = p, true in List.iter (print_ast env ep) al)
              children
#else
            ()
#endif
        | _ ->
            let auth =
              Array.for_all
                (fun ip -> authorized_age conf base (pget conf base ip))
                (get_children fam)
            in
            let env = ("auth", Vbool auth) :: env in
            let n =
              let p =
                match get_env "p" env with
                  Vind p -> p
                | _ -> assert false
              in
              let rec loop i =
                if i = Array.length (get_children fam) then -2
                else if (get_children fam).(i) = get_iper p then i
                else loop (i + 1)
              in
              loop 0
            in
            Array.iteri
              (fun i ip ->
                 let p = pget conf base ip in
                 let env = ("#loop", Vint 0) :: env in
                 let env = ("child", Vind p) :: env in
                 let env = ("child_cnt", Vint (i + 1)) :: env in
                 let env =
                   if i = n - 1 && not (is_hidden p) then
                     ("pos", Vstring "prev") :: env
                   else if i = n then ("pos", Vstring "self") :: env
                   else if i = n + 1 && not (is_hidden p) then
                     ("pos", Vstring "next") :: env
                   else env
                 in
                 let ep = p, authorized_age conf base p in
                 List.iter (print_ast env ep) al)
              (get_children fam);
#ifdef API
            (* On ajoute les enfants distants. *)
            let faml =
              Perso_link.get_families_of_parents conf.command
                (get_iper (fst ep)) isp
            in
            List.iter
              (fun fam_link ->
                 let env = ("auth", Vbool true) :: env in
                 List.iter
                   (fun c_link ->
                      let baseprefix = c_link.MLink.Person_link.baseprefix in
                      let ip_c = Gwdb.iper_of_string @@ Int32.to_string c_link.MLink.Person_link.ip in
                      match Perso_link.get_person_link baseprefix ip_c with
                        Some c_link ->
                          let can_merge =
                            Perso_link.can_merge_child conf.command
                              (get_children fam) c_link
                          in
                          if can_merge then ()
                          else
                            let (p, _) =
                              Perso_link.make_ep_link base c_link
                            in
                            let baseprefix = c_link.MLink.Person.baseprefix in
                            let env = ("#loop", Vint 0) :: env in
                            let env = ("child_link", Vind p) :: env in
                            let env =
                              ("baseprefix", Vstring baseprefix) :: env
                            in
                            let env = ("p_link", Vbool true) :: env in
                            let ep = p, true in
                            List.iter (print_ast env ep) al
                      | None -> ())
                   fam_link.MLink.Family.children)
              faml
#endif
        end
    | _ -> ()
#ifndef API
 [@@ocaml.warning "-27"]
#endif
  and print_foreach_cremation_witness env al (p, _ as ep) =
    let rec loop pevents =
      match pevents with
        [] -> ()
      | (name, _, _, _, _, wl, _) :: events ->
          if name = Pevent Epers_Cremation then
            Array.iteri
              begin fun i (ip, _) ->
                let p = pget conf base ip in
                let env =
                  ("cremation_witness", Vind p)
                  :: ("first", Vbool (i = 0))
                  :: env
                in
                List.iter (print_ast env ep) al
              end
              wl
          else loop events
    in
    loop (events_list conf base p)
  and print_foreach_death_witness env al (p, _ as ep) =
    let rec loop pevents =
      match pevents with
        [] -> ()
      | (name, _, _, _, _, wl, _) :: events ->
          if name = Pevent Epers_Death then
            Array.iteri
              begin fun i (ip, _) ->
                let p = pget conf base ip in
                let env =
                  ("death_witness", Vind p)
                  :: ("first", Vbool (i = 0))
                  :: env
                in
                List.iter (print_ast env ep) al
              end
              wl
          else loop events
    in
    loop (events_list conf base p)
  and print_foreach_descendant_level env al ep =
    let max_level =
      match get_env "max_desc_level" env with
        Vint n -> n
      | _ -> 0
    in
    let rec loop i =
      if i > max_level then ()
      else
        let env = ("level", Vint i) :: env in
        List.iter (print_ast env ep) al; loop (succ i)
    in
    loop 0
  and print_foreach_event env al (p, _ as ep) =
    let events = events_list conf base p in
    Mutil.list_iter_first
      (fun first evt ->
         let env = ("event", Vevent (p, evt)) :: env in
         let env = ("first", Vbool first) :: env in
         List.iter (print_ast env ep) al)
      events
  and print_foreach_event_witness env al (_, p_auth as ep) =
    if p_auth then
      match get_env "event" env with
        Vevent (_, (_, _, _, _, _, witnesses, _)) ->
          Array.iteri
            begin fun i (ip, wk) ->
              let p = pget conf base ip in
              let wk = Util.string_of_witness_kind conf (get_sex p) wk in
              let env =
                ("event_witness", Vind p)
                :: ("event_witness_kind", Vstring wk)
                :: ("first", Vbool (i = 0))
                :: env
              in
              List.iter (print_ast env ep) al
            end
            witnesses
      | _ -> ()
  and print_foreach_event_witness_relation env al (p, p_auth as ep) =
    let related = List.sort_uniq compare (get_related p) in
    let events_witnesses =
      let list = ref [] in
      begin let rec make_list =
        function
          ic :: icl ->
            let c = pget conf base ic in
            List.iter
              (fun (name, _, _, _, _, wl, _ as evt) ->
                 let (mem, wk) = Util.array_mem_witn conf base (get_iper p) wl in
                 if mem then
                   match name with
                     Fevent _ ->
                       if get_sex c = Male then list := (c, wk, evt) :: !list
                   | _ -> list := (c, wk, evt) :: !list)
              (events_list conf base c);
            make_list icl
        | [] -> ()
      in
        make_list related
      end;
      !list
    in
    (* On tri les témoins dans le même ordre que les évènements. *)
    let events_witnesses =
      CheckItem.sort_events
        (fun (_, _, (name, _, _, _, _, _, _)) ->
           match name with
           | Pevent n -> CheckItem.Psort n
           | Fevent n -> CheckItem.Fsort n)
        (fun (_, _, (_, date, _, _, _, _, _)) -> date)
        events_witnesses
    in
    List.iter
      (fun (p, wk, evt) ->
         if p_auth then
           let env = ("event_witness_relation", Vevent (p, evt)) :: env in
           let env = ("event_witness_relation_kind", Vstring wk) :: env in
           List.iter (print_ast env ep) al)
      events_witnesses
  and print_foreach_family env al ini_ep (p, _) =
    match get_env "p_link" env with
      Vbool _ ->
#ifdef API
        let conf =
          match get_env "baseprefix" env with
            Vstring baseprefix -> {conf with command = baseprefix}
          | _ -> conf
        in
        let faml =
          Perso_link.get_family_link conf.command (get_iper p)
        in
        let rec loop prev i faml =
          match faml with
            [] -> ()
          | fam_link :: faml ->
              let baseprefix = fam_link.MLink.Family.baseprefix in
              let (_, fam, _, _) =
                Perso_link.make_efam_link conf base fam_link
              in
              let (ifath, imoth, ifam) =
                Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath,
                Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth,
                Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam
              in
              let cpl =
                let ip = get_iper p in
                if ip <> ifath && ip <> imoth then
                  match
                    Perso_link.get_person_link_with_base conf.command ip
                      baseprefix
                  with
                    Some p ->
                      let ip = Gwdb.iper_of_string @@ Int32.to_string p.MLink.Person.ip in
                      ifath, imoth, (if ip = ifath then imoth else ifath)
                  | None ->
                      ifath, imoth, (if ip = ifath then imoth else ifath)
                else ifath, imoth, (if ip = ifath then imoth else ifath)
              in
              let vfam = Vfam (ifam, fam, cpl, true) in
              let env = ("#loop", Vint 0) :: env in
              let env = ("fam_link", vfam) :: env in
              let env = ("f_link", Vbool true) :: env in
              let env = ("is_link", Vbool true) :: env in
              let env = ("baseprefix", Vstring baseprefix) :: env in
              let env = ("family_cnt", Vint (i + 1)) :: env in
              let env =
                match prev with
                  Some vfam -> ("prev_fam", vfam) :: env
                | None -> env
              in
              List.iter (print_ast env ini_ep) al;
              loop (Some vfam) (i + 1) faml
        in
        loop None 0 faml
#else
        ()
#endif
    | _ ->
        if Array.length (get_family p) > 0 then
          begin let rec loop prev i =
            if i = Array.length (get_family p) then ()
            else
              let ifam = (get_family p).(i) in
              let fam = foi base ifam in
              let ifath = get_father fam in
              let imoth = get_mother fam in
              let ispouse = Gutil.spouse (get_iper p) fam in
              let cpl = ifath, imoth, ispouse in
              let m_auth =
                authorized_age conf base (pget conf base ifath) &&
                authorized_age conf base (pget conf base imoth)
              in
              let vfam = Vfam (ifam, fam, cpl, m_auth) in
              let env = ("#loop", Vint 0) :: env in
              let env = ("fam", vfam) :: env in
              let env = ("family_cnt", Vint (i + 1)) :: env in
              let env =
                match prev with
                  Some vfam -> ("prev_fam", vfam) :: env
                | None -> env
              in
              List.iter (print_ast env ini_ep) al; loop (Some vfam) (i + 1)
          in
            loop None 0
          end;
#ifdef API
        (* On ajoute les familles distantes. *)
        let faml =
          Perso_link.get_family_link conf.command (get_iper p)
        in
        let rec loop prev i faml =
          match faml with
            [] -> ()
          | fam_link :: faml ->
              let (_, fam, _, _) =
                Perso_link.make_efam_link conf base fam_link
              in
              let (ifath, imoth, ifam) =
                Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.ifath,
                Gwdb.iper_of_string @@ Int32.to_string fam_link.MLink.Family.imoth,
                Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam
              in
              let cpl =
                let ip = get_iper p in
                if ip <> ifath && ip <> imoth then
                  match
                    Perso_link.get_person_link_with_base conf.command ip
                      fam_link.MLink.Family.baseprefix
                  with
                    Some p ->
                      let ip = Gwdb.iper_of_string @@ Int32.to_string p.MLink.Person.ip in
                      ifath, imoth, (if ip = ifath then imoth else ifath)
                  | None ->
                      ifath, imoth, (if ip = ifath then imoth else ifath)
                else ifath, imoth, (if ip = ifath then imoth else ifath)
              in
              let can_merge =
                let fam =
                  List.map (foi base) (Array.to_list (get_family p))
                in
                Perso_link.can_merge_family conf.command (get_iper p) fam
                  fam_link cpl
              in
              if can_merge then loop None i faml
              else
                let baseprefix = fam_link.MLink.Family.baseprefix in
                let vfam = Vfam (ifam, fam, cpl, true) in
                let env = ("#loop", Vint 0) :: env in
                let env = ("fam_link", vfam) :: env in
                let env = ("f_link", Vbool true) :: env in
                let env = ("is_link", Vbool true) :: env in
                let env = ("baseprefix", Vstring baseprefix) :: env in
                let env = ("family_cnt", Vint (i + 1)) :: env in
                let env =
                  match prev with
                    Some vfam -> ("prev_fam", vfam) :: env
                  | None -> env
                in
                List.iter (print_ast env ini_ep) al;
                loop (Some vfam) (i + 1) faml
        in
        loop None 0 faml
#else
        ()
#endif
  and print_foreach_first_name_alias env al (p, p_auth as ep) =
    if not p_auth && is_hide_names conf p then ()
    else
      Mutil.list_iter_first
        (fun first s ->
           let env = ("first_name_alias", Vstring (sou base s)) :: env in
           let env = ("first", Vbool first) :: env in
           List.iter (print_ast env ep) al)
        (get_first_names_aliases p)
  and print_foreach_level max_lev env al (_, _ as ep) =
    let max_level =
      match get_env max_lev env with
        Vint n -> n
      | _ -> 0
    in
    let rec loop i =
      if i > max_level then ()
      else
        let env = ("level", Vint i) :: env in
        List.iter (print_ast env ep) al; loop (succ i)
    in
    loop 1
  and print_foreach_nobility_title env al (p, p_auth as ep) =
    if p_auth then
      let titles = nobility_titles_list conf base p in
      Mutil.list_iter_first
        (fun first x ->
           let env = ("nobility_title", Vtitle (p, x)) :: env in
           let env = ("first", Vbool first) :: env in
           List.iter (print_ast env ep) al)
        titles
  and print_foreach_parent env al (a, _ as ep) =
    match get_parents a with
      Some ifam ->
        let cpl = foi base ifam in
        Array.iter
          (fun iper ->
             let p = pget conf base iper in
             let env = ("parent", Vind p) :: env in
             List.iter (print_ast env ep) al)
          (get_parent_array cpl)
    | None -> ()
  and print_foreach_qualifier env al (p, p_auth as ep) =
    if not p_auth && is_hide_names conf p then ()
    else
      Mutil.list_iter_first
        (fun first nn ->
           let env = ("qualifier", Vstring (sou base nn)) :: env in
           let env = ("first", Vbool first) :: env in
           List.iter (print_ast env ep) al)
        (get_qualifiers p)
  and print_foreach_relation env al (p, p_auth as ep) =
    if p_auth then
      Mutil.list_iter_first
        (fun first r ->
           let env = ("rel", Vrel (r, None)) :: env in
           let env = ("first", Vbool first) :: env in
           List.iter (print_ast env ep) al)
        (get_rparents p)
  and print_foreach_related env al (p, p_auth as ep) =
    if p_auth then
      let list =
        let list = List.sort_uniq compare (get_related p) in
        List.fold_left
          (fun list ic ->
             let c = pget conf base ic in
             let rec loop list =
               function
                 r :: rl ->
                   begin match r.r_fath with
                     Some ip when ip = get_iper p ->
                       loop ((c, r) :: list) rl
                   | _ ->
                       match r.r_moth with
                         Some ip when ip = get_iper p ->
                           loop ((c, r) :: list) rl
                       | _ -> loop list rl
                   end
               | [] -> list
             in
             loop list (get_rparents c))
          [] list
      in
      let list =
        List.sort
          (fun (c1, _) (c2, _) ->
             let d1 =
               match Adef.od_of_cdate (get_baptism c1) with
                 None -> Adef.od_of_cdate (get_birth c1)
               | x -> x
             in
             let d2 =
               match Adef.od_of_cdate (get_baptism c2) with
                 None -> Adef.od_of_cdate (get_birth c2)
               | x -> x
             in
             match d1, d2 with
               Some d1, Some d2 -> Date.compare_date d1 d2
             | _ -> -1)
          (List.rev list)
      in
      List.iter
        (fun (c, r) ->
           let env = ("rel", Vrel (r, Some c)) :: env in
           List.iter (print_ast env ep) al)
        list
  and print_foreach_sorted_list_item env al ep =
    let list =
      match get_env "list" env with
        Vslist l -> SortedList.elements !l
      | _ -> []
    in
    let rec loop prev_item =
      function
        _ :: sll as gsll ->
          let item = Vslistlm gsll in
          let env = ("item", item) :: ("prev_item", prev_item) :: env in
          List.iter (print_ast env ep) al; loop item sll
      | [] -> ()
    in
    loop (Vslistlm []) list

  and print_foreach_sorted_listb_item env al ep =
    let list =
      match get_env "listb" env with
      | Vslist l -> SortedList.elements !l
      | _ -> []
    in
    let rec loop prev_item =
      function
      | (_ :: sll) as gsll ->
           let item = Vslistlm gsll in
           let env = ("item", item) :: ("prev_item", prev_item) :: env in
           List.iter (print_ast env ep) al;
           loop item sll
      | [] -> ()
    in loop (Vslistlm []) list
  and print_foreach_sorted_listc_item env al ep =
    let list =
      match get_env "listc" env with
      | Vslist l -> SortedList.elements !l
      | _ -> []
    in
    let rec loop prev_item =
      function
      | (_ :: sll) as gsll ->
           let item = Vslistlm gsll in
           let env = ("item", item) :: ("prev_item", prev_item) :: env in
           List.iter (print_ast env ep) al;
           loop item sll
      | [] -> ()
    in loop (Vslistlm []) list

  and print_foreach_source env al (p, p_auth as ep) =
    let rec insert_loop typ src =
      function
        (typ1, src1) :: srcl ->
          if src = src1 then (typ1 ^ ", " ^ typ, src1) :: srcl
          else (typ1, src1) :: insert_loop typ src srcl
      | [] -> [typ, src]
    in
    let insert typ src srcl =
      if src = "" then srcl
      else insert_loop (Util.translate_eval typ) src srcl
    in
    let srcl =
      if p_auth then
        let srcl = [] in
        let srcl =
          insert (transl_nth conf "person/persons" 0)
            (sou base (get_psources p)) srcl
        in
        let srcl =
          insert (transl_nth conf "birth" 0) (sou base (get_birth_src p)) srcl
        in
        let srcl =
          insert (transl_nth conf "baptism" 0) (sou base (get_baptism_src p))
            srcl
        in
        let (srcl, _) =
          Array.fold_left
            (fun (srcl, i) ifam ->
               let fam = foi base ifam in
               let isp = Gutil.spouse (get_iper p) fam in
               let sp = poi base isp in
               (* On sait que p_auth vaut vrai. *)
               let m_auth = authorized_age conf base sp in
               if m_auth then
                 let lab =
                   if Array.length (get_family p) = 1 then ""
                   else " " ^ string_of_int i
                 in
                 let srcl =
                   let src_typ = transl_nth conf "marriage/marriages" 0 in
                   insert (src_typ ^ lab) (sou base (get_marriage_src fam))
                     srcl
                 in
                 let src_typ = transl_nth conf "family/families" 0 in
                 insert (src_typ ^ lab) (sou base (get_fsources fam)) srcl,
                 i + 1
               else srcl, i + 1)
            (srcl, 1) (get_family p)
        in
        let srcl =
          insert (transl_nth conf "death" 0) (sou base (get_death_src p)) srcl
        in
        let buri_crem_lex =
          match get_burial p with
          Cremated _ -> "cremation"
          | _ -> "burial"
        in
        insert (transl_nth conf buri_crem_lex 0) (sou base (get_burial_src p))
          srcl
      else []
    in
    (* Affiche les sources et met à jour les variables "first" et "last". *)
    let rec loop first =
      function
        (src_typ, src) :: srcl ->
          let env =
            ("first", Vbool first) :: ("last", Vbool (srcl = [])) ::
            ("src_typ", Vstring src_typ) :: ("src", Vstring src) :: env
          in
          List.iter (print_ast env ep) al; loop false srcl
      | [] -> ()
    in
    loop true srcl
  and print_foreach_surname_alias env al (p, p_auth as ep) =
    if not p_auth && is_hide_names conf p then ()
    else
      Mutil.list_iter_first
        (fun first s ->
           let env = ("surname_alias", Vstring (sou base s)) :: env in
           let env = ("first", Vbool first) :: env in
           List.iter (print_ast env ep) al)
        (get_surnames_aliases p)
  and print_foreach_witness env al ep =
    function
      Vfam (_, fam, _, true) ->
      Array.iteri
        begin fun i ip ->
          let p = pget conf base ip in
          let env =
            ("witness", Vind p)
            :: ("first", Vbool (i = 0))
            :: env
          in
          List.iter (print_ast env ep) al
        end
        (get_witnesses fam)
    | _ -> ()
  and print_foreach_witness_relation env al (p, _ as ep) =
    let list =
      let list = ref [] in
      let related = List.sort_uniq compare (get_related p) in
      begin let rec make_list =
        function
          ic :: icl ->
            let c = pget conf base ic in
            if get_sex c = Male then
              Array.iter
                (fun ifam ->
                   let fam = foi base ifam in
                   if Array.mem (get_iper p) (get_witnesses fam) then
                     list := (ifam, fam) :: !list)
                (get_family (pget conf base ic));
            make_list icl
        | [] -> ()
      in
        make_list related
      end;
      !list
    in
    let list =
      List.sort
        (fun (_, fam1) (_, fam2) ->
           match
             Adef.od_of_cdate (get_marriage fam1),
             Adef.od_of_cdate (get_marriage fam2)
           with
           | Some d1, Some d2 -> Date.compare_date d1 d2
           | _ -> 0)
        list
    in
    List.iter
      (fun (ifam, fam) ->
         let ifath = get_father fam in
         let imoth = get_mother fam in
         let cpl = ifath, imoth, imoth in
         let m_auth =
           authorized_age conf base (pget conf base ifath) &&
           authorized_age conf base (pget conf base imoth)
         in
         if m_auth then
           let env = ("fam", Vfam (ifam, fam, cpl, true)) :: env in
           List.iter (print_ast env ep) al)
      list
  in
  print_foreach

let eval_predefined_apply conf env f vl =
  let vl =
    List.map
      (function
         VVstring s -> s
       | _ -> raise Not_found)
      vl
  in
  match f, vl with
    "a_of_b", [s1; s2] -> Util.translate_eval (transl_a_of_b conf s1 s2 s2)
  | "a_of_b2", [s1; s2; s3] -> Util.translate_eval (transl_a_of_b conf s1 s2 s3)
  | "a_of_b_gr_eq_lev", [s1; s2] ->
      Util.translate_eval (transl_a_of_gr_eq_gen_lev conf s1 s2 s2)
  | "add_in_sorted_list", sl ->
      begin match get_env "list" env with
        Vslist l -> l := SortedList.add sl !l; ""
      | _ -> raise Not_found
      end
  | ("add_in_sorted_listb", sl) ->
      begin match get_env "listb" env with
      | Vslist l -> l := SortedList.add sl !l; ""
      | _ -> raise Not_found
      end
  | ("add_in_sorted_listc", sl) ->
      begin match get_env "listc" env with
      | Vslist l -> l := SortedList.add sl !l; ""
      | _ -> raise Not_found
      end
  | "hexa", [s] -> Util.hexa_string s
  | "initial", [s] ->
      if String.length s = 0 then ""
      else String.sub s 0 (Utf8.next s 0)
  | "lazy_print", [v] ->
      begin match get_env "lazy_print" env with
        Vlazyp r -> r := Some v; ""
      | _ -> raise Not_found
      end
  | "min", s :: sl ->
      begin try
        let m =
          List.fold_right (fun s -> min (int_of_string s)) sl
            (int_of_string s)
        in
        string_of_int m
      with Failure _ -> raise Not_found
      end
  | "clean_html_tags", [s] ->
      (* On supprime surtout les balises qui peuvent casser la mise en page. *)
      Util.clean_html_tags s
        ["<br */?>"; "</?p>"; "</?div>"; "</?span>"; "</?pre>"]
  | _ -> raise Not_found

let gen_interp_templ menu title templ_fname conf base p =
  template_file := templ_fname ^ ".txt";
  let ep = p, authorized_age conf base p in
  let emal =
    match p_getint conf.env "v" with
      Some i -> i
    | None -> 120
  in
  let env =
    if Util.find_sosa_ref conf base <> None then build_sosa_ht conf base ;
    let sosa_ref = Util.find_sosa_ref conf base in
    let sosa_ref_l = let sosa_ref () = sosa_ref in Lazy.from_fun sosa_ref in
    let t_sosa =
      match sosa_ref with
      | Some p -> Some (init_sosa_t conf base p)
      | _ -> None
    in
    let desc_level_table_l =
      let dlt () = make_desc_level_table conf base emal p in Lazy.from_fun dlt
    in
    let desc_level_table_m =
      let dlt () = make_desc_level_table conf base 120 p in
      Lazy.from_fun dlt
    in
    let desc_level_table_l_save =
      let dlt () = make_desc_level_table conf base emal p in Lazy.from_fun dlt
    in
    let mal () =
      Vint (max_ancestor_level conf base (get_iper p) emal + 1)
    in
    (* Static max ancestor level *)
    let smal () =
      Vint (max_ancestor_level conf base (get_iper p) 120 + 1)
    in
    (* Sosa_ref max ancestor level *)
    let srmal () =
      match Util.find_sosa_ref conf base with
      | Some sosa_ref ->
        Vint (max_ancestor_level conf base (get_iper sosa_ref) 120 + 1)
      | _ -> Vint 0
    in
    let mcl () = Vint (max_cousin_level conf base p) in
    (* Récupère le nombre maximal de niveaux de descendance en prenant en compte les liens inter-arbres (limité à 10 générations car problématique en terme de perf). *)
    let mdl () =
#ifdef API
      Vint
        (max (max_descendant_level base desc_level_table_l)
           (Perso_link.max_interlinks_descendancy_level conf base
              (get_iper p) 10))
#else
      Vint (max_descendant_level base desc_level_table_l)
#endif
    in
    (* Static max descendant level *)
    let smdl () =
      Vint (max_descendant_level base desc_level_table_m)
    in
    let nldb () =
      let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
      let fname = Filename.concat bdir "notes_links" in
      let db = NotesLinks.read_db_from_file fname in
      let db = Notes.merge_possible_aliases conf db in Vnldb db
    in
    let all_gp () = Vallgp (get_all_generations conf base p) in
    [("p", Vind p);
     ("p_auth", Vbool (authorized_age conf base p));
     ("count", Vcnt (ref 0));
     ("count1", Vcnt (ref 0));
     ("count2", Vcnt (ref 0));
     ("list", Vslist (ref SortedList.empty));
     ("listb", Vslist (ref SortedList.empty));
     ("listc", Vslist (ref SortedList.empty));
     ("desc_mark", Vdmark (ref @@ Gwdb.dummy_marker Gwdb.dummy_iper false));
     ("lazy_print", Vlazyp (ref None));
     ("sosa",  Vsosa (ref []));
     ("sosa_ref", Vsosa_ref sosa_ref_l);
     ("t_sosa", Vt_sosa t_sosa);
     ("max_anc_level", Vlazy (Lazy.from_fun mal));
     ("static_max_anc_level", Vlazy (Lazy.from_fun smal));
     ("sosa_ref_max_anc_level", Vlazy (Lazy.from_fun srmal));
     ("max_cous_level", Vlazy (Lazy.from_fun mcl));
     ("max_desc_level", Vlazy (Lazy.from_fun mdl));
     ("static_max_desc_level", Vlazy (Lazy.from_fun smdl));
     ("desc_level_table", Vdesclevtab desc_level_table_l);
     ("desc_level_table_save", Vdesclevtab desc_level_table_l_save);
     ("nldb", Vlazy (Lazy.from_fun nldb));
     ("all_gp", Vlazy (Lazy.from_fun all_gp))]
  in
  if menu then
    let size =
      match Util.open_templ conf templ_fname with
        Some ic ->
          let fd = Unix.descr_of_in_channel ic in
          let stats = Unix.fstat fd in close_in ic; stats.Unix.st_size
      | None -> 0
    in
    if size = 0 then Hutil.header conf title
    else
      Hutil.interp_no_header conf templ_fname
        {Templ.eval_var = eval_var conf base;
         Templ.eval_transl = eval_transl conf base;
         Templ.eval_predefined_apply = eval_predefined_apply conf;
         Templ.get_vother = get_vother; Templ.set_vother = set_vother;
         Templ.print_foreach = print_foreach conf base}
        env ep
  else
    Hutil.interp conf templ_fname
      {Templ.eval_var = eval_var conf base;
       Templ.eval_transl = eval_transl conf base;
       Templ.eval_predefined_apply = eval_predefined_apply conf;
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach = print_foreach conf base}
      env ep

let interp_templ = gen_interp_templ false (fun _ -> ())
let interp_templ_with_menu = gen_interp_templ true
let interp_notempl_with_menu title templ_fname conf base p =
  (* On envoie le header car on n'est pas dans un template (exple: merge). *)
  Hutil.header_without_page_title conf title;
  gen_interp_templ true title templ_fname conf base p

(* Main *)

let print conf base p =
  let passwd =
    if conf.wizard || conf.friend then None
    else
      let src =
        match get_parents p with
          Some ifam -> sou base (get_origin_file (foi base ifam))
        | None -> ""
      in
      try Some (src, List.assoc ("passwd_" ^ src) conf.base_env) with
        Not_found -> None
  in
  match passwd with
    Some (src, passwd)
    when is_that_user_and_password conf.auth_scheme "" passwd = false ->
      Util.unauthorized conf src
  | _ -> interp_templ "perso" conf base p

let limit_by_tree conf =
  match p_getint conf.base_env "max_anc_tree" with
    Some x -> max 1 x
  | None -> 7

let print_ancestors_dag conf base v p =
  let v = min (limit_by_tree conf) v in
  let set =
    let rec loop set lev ip =
      let set = Dag.Pset.add ip set in
      if lev <= 1 then set
      else
        match get_parents (pget conf base ip) with
          Some ifam ->
            let cpl = foi base ifam in
            let set = loop set (lev - 1) (get_mother cpl) in
            loop set (lev - 1) (get_father cpl)
        | None -> set
    in
    loop Dag.Pset.empty v (get_iper p)
  in
  let elem_txt p = DagDisplay.Item (p, "") in
  (* Récupère les options d'affichage. *)
  let options = Util.display_options conf in
  let vbar_txt ip =
    let p = pget conf base ip in
    Printf.sprintf "%sm=A&t=T&v=%d&%s&dag=on&%s" (commd conf) v options
      (acces conf base p)
  in
  let page_title = Utf8.capitalize (Util.transl conf "tree") in
  DagDisplay.make_and_print_dag conf base elem_txt vbar_txt true set [] page_title ""

let print_ascend conf base p =
  match
    p_getenv conf.env "t", p_getenv conf.env "dag", p_getint conf.env "v"
  with
    Some "T", Some "on", Some v -> print_ancestors_dag conf base v p
  | _ ->
      let templ =
        match p_getenv conf.env "t" with
          Some ("E" | "F" | "H" | "L") -> "anclist"
        | Some ("D" | "G" | "M" | "N" | "P" | "X" | "Y" | "Z") -> "ancsosa"
        | Some ("A" | "C" | "T") -> "anctree"
        | _ -> "ancmenu"
      in
      interp_templ templ conf base p

let print_what_links conf base p =
  if authorized_age conf base p then
    let key =
      let fn = Name.lower (sou base (get_first_name p)) in
      let sn = Name.lower (sou base (get_surname p)) in fn, sn, get_occ p
    in
    let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
    let fname = Filename.concat bdir "notes_links" in
    let db = NotesLinks.read_db_from_file fname in
    let db = Notes.merge_possible_aliases conf db in
    let pgl = links_to_ind conf base db key in
    let title h =
      Wserver.printf "%s%s " (Utf8.capitalize (transl conf "linked pages"))
        (Util.transl conf ":");
      if h then Wserver.printf "%s" (simple_person_text conf base p true)
      else
        Wserver.printf "<a href=\"%s%s\">%s</a>" (commd conf)
          (acces conf base p) (simple_person_text conf base p true)
    in
    Hutil.header conf title;
    Hutil.print_link_to_welcome conf true;
    NotesDisplay.print_linked_list conf base pgl;
    Hutil.trailer conf
  else Hutil.incorrect_request conf
