#ifdef API

module M = Api_piqi
module Mext = Api_piqi_ext

open Config
open Def
open Gwdb
open Util
open Api_def
open Api_util

(**/**) (* Services disponibles. *)

(* ******************************************************************** *)
(*  [Fonc] print_info_base : config -> base -> InfosBase                *)
(** [Description] : Retourne les informations d'une base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - string : Mets à jour les champs du type Infos_base.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_info_base conf base =
  let (sosa_p, sosa) =
     match Util.find_sosa_ref conf base with
     | Some p -> (Some p, Some (person_to_reference_person base p))
     | None -> (None, None)
  in
  let last_modified_person =
    let default () = Opt.map (fun p -> Gwdb.string_of_iper (get_iper p)) sosa_p in
    try
      let ic = Secure.open_in_bin (History.file_name conf) in
      let (_, pos, wiz) = (1, in_channel_length ic, "") in
      let vv = (ref (Bytes.create 0), ref 0) in
      let last_modified_person =
        let (line, _) = History.rev_input_line ic pos vv in
        match History.line_fields line with
        | Some (_, user, action, keyo) ->
          if wiz = "" || user = wiz then
            match keyo with
            | Some key ->
              (match action with
               | "mn" -> default ()
               | _ ->
                 (match Gutil.person_ht_find_all base key with
                  | [ip] -> Some (Gwdb.string_of_iper ip)
                  | _ -> default ()))
            | None -> default ()
          else default ()
        | None -> default ()
      in
      close_in ic;
      last_modified_person
    with Sys_error _ | _ -> default ()
  in
  let info_base =
    M.Infos_base.({
      nb_persons = Int64.of_int (Gwdb.nb_of_persons base);
      nb_families = Int64.of_int (Gwdb.nb_of_families base);
      sosa = sosa;
      last_modified_person = Opt.map Int64.of_string last_modified_person;
      real_nb_persons = Some (Int64.of_int (Util.real_nb_of_persons conf base));
    })
  in
  let data = Mext.gen_infos_base info_base in
  print_result conf data



(** [print_loop conf base]
    If there is a loop in the base print a person being its own ancestor.
    Otherwise, print a dummy (empty) person instead. **)
let print_loop conf base =
  let (base_loop, pers) =
    (ref false, ref (poi base (Gwdb.dummy_iper)))
  in
  (* On ne fait pas un Util.create_topological_sort conf base qui est certe *)
  (* plus rapide, mais qui dans de rare cas, n'est pas capable de remonter  *)
  (* la boucle (on ne check pas la base en entier). Avec cette méthode, on  *)
  (* n'a pas de ce problème.                                                *)
  let () =
    load_ascends_array base ;
    load_couples_array base ;
    Consang.check_noloop base
      (function OwnAncestor p -> base_loop := true ; pers := p | _ -> () ) ;
    clear_ascends_array base ;
    clear_couples_array base
  in
  let p =
    if !base_loop then
      (* Comme il y a une boucle, Perso.get_single_sosa ne va pas marcher *)
      (* mais la fonction ne sera pas appelée dans pers_to_piqi_person.   *)
      pers_to_piqi_person conf base !pers !base_loop
        (Perso.get_single_sosa conf base) false
    else
      let ref_pers = empty_reference_person in
      empty_piqi_person conf ref_pers false
  in
  let data = data_person p in
  print_result conf data

(* ******************************************************************** *)
(*  [Fonc] print_info_ind : config -> base -> unit                      *)
(** [Description] : Renvoie à partir d'une référence person (piqi) une
                    person (piqi).
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_info_ind conf base =
  let ref_person = get_params conf Mext.parse_reference_person in
  let filters = get_filters conf in
  let base_loop = has_base_loop conf base in
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  let p =
    match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
    | Some ip ->
        let p = pget conf base ip in
        if apply_filters_p conf filters (Perso.get_single_sosa conf base) p then
          pers_to_piqi_person
            conf base p base_loop (Perso.get_single_sosa conf base) false
        else
          empty_piqi_person conf ref_person base_loop
    | None -> empty_piqi_person conf ref_person base_loop
  in
  let data = data_person p in
  print_result conf data


(* ******************************************************************** *)
(*  [Fonc] print_list_ref_person : config -> base -> unit               *)
(** [Description] : Renvoie à partir d'une liste de référence person
                    (piqi) une liste de persons (piqi).
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_list_ref_person conf base =
  let list_ref_person = get_params conf Mext.parse_list_reference_persons in
  let filters = get_filters conf in
  let pl =
    List.map
      (fun ref_p ->
        let sn = ref_p.M.Reference_person.n in
        let fn = ref_p.M.Reference_person.p in
        let occ = ref_p.M.Reference_person.oc in
        match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
        | Some ip ->
            let p  = pget conf base ip in
            PFull p
        | None -> PLight ref_p )
      list_ref_person.M.List_reference_persons.list_ref_persons
  in
  let data =
    data_list_person_option conf base filters pl
  in
  print_result conf data


(* ******************************************************************** *)
(*  [Fonc] print_ref_person_from_ip : config -> base -> unit            *)
(** [Description] : Renvoie à partir de l'ip une référence personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_ref_person_from_ip conf base =
  let id = get_params conf Mext.parse_index in
  let ip = Gwdb.iper_of_string @@ Int32.to_string id.M.Index.index in
  let ref_p = person_to_reference_person base @@ poi base ip in
  let data = Mext.gen_reference_person ref_p in
  print_result conf data


(**/**) (* API_FIRST_AVAILABLE_PERSON *)

(* ************************************************************************ *)
(*  [Fonc] print_first_available_person : config -> base -> ReferencePerson *)
(** [Description] : Retourne la "première" personne accessible d'un arbre
                    et visible.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : ReferencePerson
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let print_first_available_person conf base =
  let empty_ref = empty_reference_person in
  let continue = ref true in
  let res = ref empty_ref in
  Gwdb.Collection.fold_until (fun () -> !continue) begin fun () p ->
    if is_hide_names conf p || is_empty_or_quest_name p ||
       not (authorized_age conf base p)
    then ()
    else
      begin
        res := person_to_reference_person base p ;
        continue := false
      end
  end () (Gwdb.persons base) ;
  let data = Mext.gen_reference_person !res in
  print_result conf data


(**/**) (* API_SOSA *)

(* ************************************************************************ *)
(*  [Fonc] print_find_sosa : config -> base -> ReferencePerson              *)
(** [Description] : Cette fonction est utilisée pour la première saisie.
       Elle prend une référence_person et si elle a des enfants, alors on
       renvoi le premier enfant, sinon on renvoi la même personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : ReferencePerson
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let print_find_sosa conf base =
  let ref_person = get_params conf Mext.parse_reference_person in
  let n = ref_person.M.Reference_person.n in
  let p = ref_person.M.Reference_person.p in
  let oc = ref_person.M.Reference_person.oc in
  let ref_p =
    match Gwdb.person_of_key base p n (Int32.to_int oc) with
    | Some ip ->
      let arr = get_family (poi base ip) in
      let len = Array.length arr in
      let rec loop i =
        if i < len
        then begin
          let fam = foi base (Array.unsafe_get arr i) in
          match get_children fam with
          | [||] -> loop (i + 1)
          | arr ->
            person_to_reference_person base @@ poi base @@ Array.unsafe_get arr 0
        end else
          (* On reconstruit la ref_person pour être sûr des accents. *)
          person_to_reference_person base @@ poi base ip
      in
      loop 0
    | None -> empty_reference_person
  in
  let data = Mext.gen_reference_person ref_p in
  print_result conf data


(**/**) (* API_LAST_MODIFIED_PERSONS *)

(* ******************************************************************** *)
(*  [Fonc] print_last_modified_persons : config -> base -> persons list *)
(** [Description] : Retourne la liste des dernières personnes modifiées
                    par le magicien. Si aucun magicien n'est donné, alors
                    c'est les dernières personnes.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - persons list : List des personnes modifiées.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_last_modified_persons conf base =
  let params = get_params conf Mext.parse_last_modifications in
  let filters = get_filters conf in
  let wiz =
    match params.M.Last_modifications.wizard with
    | Some wiz -> wiz
    | None -> ""
  in
  let max_res =
    match params.M.Last_modifications.max_res with
    | Some i -> Int32.to_int i
    | None -> 10
  in
  let range =
    match params.M.Last_modifications.range with
    | Some range ->
        let date_begin = range.M.Filter_date_range.date_begin in
        let dmy1 =
          { day = Int32.to_int date_begin.M.Filter_date.day;
            month = Int32.to_int date_begin.M.Filter_date.month;
            year = Int32.to_int date_begin.M.Filter_date.year;
            prec = Sure; delta = 0 }
        in
        let date_end = range.M.Filter_date_range.date_end in
        let dmy2 =
          { day = Int32.to_int date_end.M.Filter_date.day;
            month = Int32.to_int date_end.M.Filter_date.month;
            year = Int32.to_int date_end.M.Filter_date.year;
            prec = Sure; delta = 0 }
        in
        let prec = range.M.Filter_date_range.only_exact in
        Some (dmy1, dmy2, prec)
    | None -> None
  in
  let is_time_included time =
    match range with
    | Some (date_begin, date_end, prec) ->
        (* time : 0000-00-00 00:00:00 *)
        let date =
          let y = int_of_string (String.sub time 0 4) in
          let m = int_of_string (String.sub time 5 2) in
          let d = int_of_string (String.sub time 8 2) in
          let dmy =
            { day = d; month = m; year = y; prec = Sure; delta = 0; }
          in
          Some (Dgreg (dmy, Dgregorian))
        in
        is_date_included prec date date_begin date_end
    | None -> true
  in
  let date_before_interval time =
    match range with
    | Some (date_begin, _, prec) ->
      (* time : 0000-00-00 00:00:00 *)
      let date =
        let y = int_of_string (String.sub time 0 4) in
        let m = int_of_string (String.sub time 5 2) in
        let d = int_of_string (String.sub time 8 2) in
        let dmy =
          { day = d; month = m; year = y; prec = Sure; delta = 0; }
        in
        Some (Dgreg (dmy, Dgregorian))
      in
      let dmy_zero = { day = 1; month = 1; year = 1970; prec = Sure; delta = 0; } in
      is_date_included prec date dmy_zero date_begin
    | None -> true
  in
  let p_mem ip list =
    let rec loop list =
      match list with
      | [] -> false
      | p :: list ->
          if ip = get_iper p then true
          else loop list
    in
    loop list
  in
  let list =
    match
      try Some (Secure.open_in_bin (History.file_name conf))
      with Sys_error _ -> None
    with
    | Some ic ->
        let () = Perso.build_sosa_ht conf base in
        let pos = in_channel_length ic in
        let vv = (ref (Bytes.create 0), ref 0) in
        let rec loop list res pos =
          if res = 0 then list
          else
            match
              try Some (History.rev_input_line ic pos vv)
              with History.Begin_of_file -> None
            with
            | Some (line, pos) ->
                (match History.line_fields line with
                | Some (time, user, action, keyo) ->
                    if (wiz = "" || user = wiz) then
                      if is_time_included time then
                        match keyo with
                        | Some key ->
                            (match action with
                            | "mn" | "dp" | "cp" | "cs" | "co" ->
                                loop list res pos
                            | _ ->
                                (match Gutil.person_ht_find_all base key with
                                | [ip] ->
                                    let p = poi base ip in
                                    if not (is_empty_or_quest_name p) &&
                                      apply_filters_p
                                        conf filters (Perso.get_sosa_person) p &&
                                      not (p_mem ip list)
                                    then loop (p :: list) (res - 1) pos
                                    else loop list res pos
                                | _ -> loop list res pos))
                        | None -> loop list res pos
                      else
                        if date_before_interval time then list
                        else loop list res pos
                    else loop list res pos
                | None -> loop list res pos)
            | None -> list
        in
        let list = loop [] max_res pos in
        close_in ic;
        List.rev list
    | None -> []
  in
  let data = conv_data_list_person conf base filters list in
  print_result conf data


(**/**) (* API_LAST_VISITED_PERSONS *)

(* ************************************************************************ *)
(*  [Fonc] print_last_visited_persons : config -> base -> persons list      *)
(** [Description] : Retourne la liste des dernières personnes visités
                    par le user donné en paramètre.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - persons list : Liste des personnes modifiées.
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let print_last_visited_persons conf base =
  let last_visits = get_params conf Mext.parse_last_visits in
  let user = last_visits.M.Last_visits.user in
  let filters = get_filters conf in
  let list =
    if user = "" then []
    else try Hashtbl.find (Util.read_visited conf) user with Not_found -> []
  in
  (* On ne supprime pas le fichier de cache, même après un envoi Gendcom, *)
  (* donc on vérifie que les personnes existent toujours dans la base.    *)
  let list =
    List.fold_right
      begin fun (ip, _) accu ->
        try
          let p = poi base ip in
          if apply_filters_p conf filters (Perso.get_single_sosa conf base) p
          then p :: accu
          else accu
        with _ -> accu
      end
      list []
  in
  let data = conv_data_list_person conf base filters list in
  print_result conf data


(**/**) (* API_MAX_ANCESTORS *)

(* ************************************************************************ *)
(*  [Fonc] print_max_ancestors : config -> base -> ReferencePerson          *)
(** [Description] : Recherche la personne qui a le plus d'ancêtres.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : ReferencePerson
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let print_max_ancestors =
  let module IperSet =
    Set.Make (struct type t = iper let compare = Stdlib.compare end)
  in
  fun conf base ->
  let ipers = Gwdb.ipers base in
  let ancestors = Gwdb.iper_marker ipers IperSet.empty in
  let mark = Gwdb.iper_marker ipers false in

  let has_children p =
    Array.exists
      (fun ifam -> Array.length (get_children @@ foi base ifam) > 0)
      (get_family p)
  in

  let rec nb_ancestors ip =
    if Gwdb.Marker.get mark ip then Gwdb.Marker.get ancestors ip
    else
      begin
        let anc =
          match get_parents (poi base ip) with
          | Some ifam ->
              let cpl = foi base ifam in
              let anc =
                IperSet.add (get_father cpl) @@ Gwdb.Marker.get ancestors ip
              in
              let anc =
                IperSet.add (get_mother cpl) anc
              in
              let anc2 =
                IperSet.union
                  (nb_ancestors (get_father cpl))
                  (nb_ancestors (get_mother cpl))
              in
              IperSet.union anc anc2
          | None -> IperSet.empty
        in
        Gwdb.Marker.set ancestors ip anc;
        Gwdb.Marker.set mark ip true;
        anc
      end
  in

  Gwdb.Collection.iter begin fun p ->
    if has_children p || Gwdb.Marker.get mark (get_iper p) then ()
    else
      begin
        let i = get_iper p in
        let anc = nb_ancestors i in
        Gwdb.Marker.set ancestors i anc;
        Gwdb.Marker.set mark i true
      end
  end (Gwdb.persons base) ;

  (* ip, nb_anc *)
  let res = ref (Gwdb.dummy_iper, 0) in
  Gwdb.Collection.iter begin fun i ->
    let nb = IperSet.cardinal @@ Gwdb.Marker.get ancestors i in
    if nb > snd !res then res := (i, nb)
  end ipers ;
  let ref_p = person_to_reference_person base @@ poi base (fst !res) in
  let data = Mext.gen_reference_person ref_p in
  print_result conf data

(**/**) (* API_IMAGE_ALL *)

let print_img_all conf base =
  let filters = get_filters conf in
  let aux fp fl =
    let list =
      Gwdb.Collection.fold begin fun acc p ->
        if not (is_empty_string (get_image p)) then
          let img = sou base (get_image p) in
          fp p img :: acc
        else
          match Api_util.find_image_file conf base p with
          | Some img -> fp p img :: acc
          | None -> acc
      end [] (Gwdb.persons base)
    in
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length list)}) in
      let data = Mext.gen_internal_int32 len in
      print_result conf data
    else
      print_result conf (fl list)
  in
  let base_loop = has_base_loop conf base in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    aux
      (fun p img ->
         let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person false in
         M.Full_image.({person = p; img = img;}))
      (fun list -> Mext.gen_list_full_images @@ M.List_full_images.({images = list}) )
  else
    aux
      (fun p img ->
         let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person false in
         M.Image.({person = p; img}))
      (fun list ->
         Mext.gen_list_images @@ M.List_images.({list_images = list}))


(**/**) (* API_IMAGE_APP *)

let print_img_person conf base =
  let id = get_params conf Mext.parse_index in
  let ip = Gwdb.iper_of_string @@ Int32.to_string id.M.Index.index in
  let p = poi base ip in
  let img_addr =
    match sou base (get_image p) with
    | "" ->
        (match Util.auto_image_file conf base p with
        | Some file -> file
        | None -> "")
    | s -> s
  in
  let img_from_ip = M.Image_address.({img = img_addr}) in
  let data = Mext.gen_image_address img_from_ip in
  print_result conf data



(**/**) (* API_UPDT_IMAGE *)

let print_updt_image conf base =
  let pers_img_l = get_params conf Mext.parse_list_pers_img in
  let pers_img_l = pers_img_l.M.List_pers_img.list_pers_img in
  List.iter
    (fun pers_img ->
      let pers = pers_img.M.Pers_img.person in
      let sn = pers.M.Reference_person.n in
      let fn = pers.M.Reference_person.p in
      let occ = pers.M.Reference_person.oc in
      let img = pers_img.M.Pers_img.img in
      match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
      | Some ip ->
          let p = poi base ip in
          let p =
            {(gen_person_of_person p) with image = Gwdb.insert_string base img}
          in
          patch_person base p.key_index p
      | None -> () )
    pers_img_l;
  Gwdb.commit_patches base

(**/**) (* API_CHECK_BASE *)

(* ********************************************************************* *)
(*  [Fonc] print_base_warnings : config -> base -> unit                  *)
(** [Description] : Renvoie les listes des erreurs et warnings d'une base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - base_warning : Les listes de tous les warnings de la base.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_base_warnings conf base =
  let filters = get_filters conf in
  let errors = ref [] in
  let warnings = ref [] in
  Check.check_base base
    (Api_warnings.set_list errors) (Api_warnings.set_list warnings) ignore ;
  (* On rend la liste unique, parce qu'il se peut qu'un warning soit *)
  (* levé par plusieurs fonctions différents selon le context.       *)
  let warnings =
    let ht = Hashtbl.create 1 in
    let rec loop wl accu =
      match wl with
      | [] -> accu
      | x :: wl ->
          if Hashtbl.mem ht (Hashtbl.hash x) then loop wl accu
          else begin
            Hashtbl.add ht (Hashtbl.hash x) true;
            loop wl (x :: accu)
          end
    in
    loop !warnings []
  in
  List.iter
    (Api_warnings.add_error_to_piqi_warning_list base)
    !errors ;
  List.iter
    (Api_warnings.add_warning_to_piqi_warning_list conf base)
    warnings ;
  let data =
    if filters.nb_results then
      let len = List.length !errors + List.length warnings in
      let len = M.Internal_int32.({value = Int32.of_int len}) in
      Mext.gen_internal_int32 len
    else
      let base_warnings = Api_warnings.create_piqi_warnings () in
      Mext.gen_base_warnings base_warnings
  in
  print_result conf data


(**/**) (* Récupération de toute une base. *)

let print_all_persons conf base =
  let params = get_params conf Mext.parse_all_persons_params in
  let filters = get_filters conf in
  let (from, until) =
    match (params.M.All_persons_params.from, params.M.All_persons_params.limit) with
    | (Some f, Some l) -> (Int32.to_int f, min (nb_of_persons base - 1) (Int32.to_int f + Int32.to_int l))
    | (Some f, None) -> (Int32.to_int f, nb_of_persons base - 1)
    | (None, Some l) -> (0, Int32.to_int l)
    | (None, None) -> (0, nb_of_persons base - 1)
  in
  let () = Perso.build_sosa_ht conf base in
  let list =
    Gwdb.Collection.fold ~from ~until (fun acc i -> i :: acc) [] (Gwdb.persons base)
  in
  let list = List.filter (apply_filters_p conf filters Perso.get_sosa_person) list in
  let data = conv_data_list_person conf base filters list in
  print_result conf data

module StringMap =
  Map.Make
    (struct
      type t = string      let compare = Gutil.alphabetic_order      end)

module IperSort =
  Set.Make
    (struct
      type t = string * string       let compare (sn1, fn1) (sn2, fn2) =
        let cmp = compare sn1 sn2 in
        if cmp = 0 then compare fn1 fn2
        else cmp(*
        let cmp = Gutil.alphabetic_order sn1 sn2 in
        if cmp = 0 then Gutil.alphabetic_order fn1 fn2
        else cmp*)
     end)

#endif
