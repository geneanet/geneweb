module MLink = Api_link_tree_piqi
module MLinkext = Api_link_tree_piqi_ext


open Config


let api_url = ref "127.0.0.1:2322" ;;


(**/**) (* Hashtbl pour gérer le cache. *)

(* La clé du cache est toujours (bname, ip) et on stocke les objets proto. *)

(* Cache qui permet de savoir si une personne a des parents. *)
let (ht_parents_cache : ((string * Def.iper), MLink.Family.t) Hashtbl.t) = Hashtbl.create 1 ;;

(* Cache qui permet de savoir si une personne des familles *)
let (ht_families_cache : ((string * Def.iper), MLink.Family_link.t list) Hashtbl.t) = Hashtbl.create 1 ;;

(* Cache qui permet de savoir si pour un ifam donné, on a une famille *)
let (ht_family_cache : ((string * Def.ifam), MLink.Family.t) Hashtbl.t) = Hashtbl.create 1 ;;

(* Cache qui permet de savoir si pour un iper donnée, on a une personne *)
let (ht_person_cache : ((string * Def.iper), MLink.Person.t) Hashtbl.t) = Hashtbl.create 1 ;;

(* Cache qui permet de connaître toutes les correspondances inter-arbre entre les personnes. *)
let (ht_corresp : ((string * Def.iper), (string * Def.iper)) Hashtbl.t) = Hashtbl.create 1 ;;


(**/**) (* Quelques outils. *)


let nsplit s c =
  let rec loop list i j =
    if i = 0 then
      if s.[i] = c then ("" :: list)
      else
        let ss = String.sub s 0 j in
        (ss :: list)
    else
      if s.[i] = c then
        let ss = String.sub s (i + 1) (j - i - 1) in
        loop (ss :: list) (i - 1) i
      else loop list (i - 1) j
  in
  if s = "" then []
  else
    let len = String.length s in
    loop [] (len - 1) len
;;

let ip_of_ref_person base ref_p =
  match nsplit ref_p '|' with
  | [n; p; oc] ->
      let oc = if oc = "" then 0 else int_of_string oc in
      Gwdb.person_of_key base p n oc
  | [n; p] -> Gwdb.person_of_key base p n 0
  | _ -> None
;;

let chop_base_prefix base_prefix =
  let len = String.length base_prefix in
  if len > 2 &&
     (base_prefix.[len-1] = 'w' || base_prefix.[len-1] = 'f') &&
     base_prefix.[len-2] = '_'
  then
    String.sub base_prefix 0 (len - 2)
  else base_prefix
;;


(**/**) (* IO Curl. *)


let writer accum data =
  Buffer.add_string accum data;
  String.length data
;;

let showContent content =
  Printf.printf "%s" (Buffer.contents content);
  flush stdout
;;

let showInfo connection =
  Printf.printf "Time: %f\nURL: %s\n"
    (Curl.get_totaltime connection)
    (Curl.get_effectiveurl connection)
;;

let getContent connection url =
  Curl.set_url connection url;
  Curl.set_timeoutms connection 1000;
  Curl.perform connection
;;


(**/**)


(* ************************************************************************** *)
(*  [Fonc] init_cache : conf -> base -> string list -> string -> iper ->
                          int -> int -> int -> unit                           *)
(** [Description] : Effecture les appels CURL afin d'initialiser le cache.
       Une fois que le cache est créé, on ne fait appel que à lui, ce qui veut
       dire que si on ne trouve rien dans le cache, c'est que l'information
       n'existe pas. Ex: si on ne trouve pas de parents dans le cache, ça veut
       dire que la personne n'a pas de parents.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - request : la requete actuelle
      - base_prefix : le nom de la base locale
      - ip : l'index de la personne sur qui on veut les informations
      - nb_asc : le nombre de génération en asc dont on veut le cache
      - from_gen_desc : la génération à partir de laquelle on veut la desc
      - nb_desc : le nombre de génération en des dont on veut le cache
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let init_cache conf base request base_prefix ip nb_asc from_gen_desc nb_desc =
  let index = Some (Int32.of_int (Adef.int_of_iper ip)) in
  let base_prefix = chop_base_prefix base_prefix in
  let data =
    MLink.Link_tree_params.({
      basename = base_prefix;
      ip = index;
      ref_person = None;
      ref_person2 = None;
      nb_asc = Int32.of_int nb_asc;
      from_gen_desc = Int32.of_int from_gen_desc;
      nb_desc = Int32.of_int nb_desc;
    })
  in
  let data = MLinkext.gen_link_tree_params data `pb in
  let url =
    Printf.sprintf
      "http://%s:%d/%s?m=API_LINK_TREE&input=pb&output=pb&sig=azerty&data=%s"
      conf.api_host conf.api_port base_prefix (Wserver.encode data)
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
        (* On ajoute dans les headers les informations pour l'auto-connection. *)
        (* Les droits wizard/friend suivront automatiquement.                  *)
        let auth = Wserver.extract_param "authorization: " '\r' request in
        if auth <> "" then
          ("Authorization: " ^ auth) :: ("Gw-Connection-Type: auto") :: headers
        else headers
      in
      let headers =
        (* On ajoute dans les headers l'inclusion des not validated. *)
        let include_not_validated =
          Wserver.extract_param "inter-tree-links-include-not-validated: " '\r' request
        in
        if include_not_validated <> "" then
          ("Inter-Tree-Links-Include-Not-Validated: " ^ include_not_validated) :: headers
        else headers
      in
      Curl.set_httpheader connection headers;
      Curl.set_errorbuffer connection errorBuffer;
      Curl.set_writefunction connection (writer result);
      Curl.set_followlocation connection true;
      Curl.set_url connection url;
      Curl.set_timeoutms connection 1000;
      Curl.perform connection;
      (*
        showContent result;
        showInfo connection;
      *)
      Curl.cleanup connection;
      res := Buffer.contents result
    with
    | Curl.CurlException (reason, code, str) ->
        Printf.fprintf stderr "Error: %s\n" !errorBuffer
    | Failure s ->
        Printf.fprintf stderr "Caught exception: %s\n" s
  end;
  Curl.global_cleanup ();

  (* Mise à jour du cache avec les résultats. *)
  let families = MLinkext.parse_link_tree !res `pb in
  List.iter
    (fun fam ->
       let base_prefix = fam.MLink.Family.baseprefix in
       let base_prefix = chop_base_prefix base_prefix in
       let ifam = Adef.ifam_of_int (Int32.to_int fam.MLink.Family.ifam) in
       Hashtbl.add ht_family_cache (base_prefix, ifam) fam;
       List.iter
         (fun c ->
           let base_prefix = c.MLink.Person_link.baseprefix in
           let base_prefix = chop_base_prefix base_prefix in
           let ic = Adef.iper_of_int (Int32.to_int c.MLink.Person_link.ip) in
           Hashtbl.add ht_parents_cache (base_prefix, ic) fam)
         fam.MLink.Family.children)
    families.MLink.Link_tree.families;

  let ht = Hashtbl.create 1 in
  List.iter
    (fun p ->
       let base_prefix = p.MLink.Person.baseprefix in
       let base_prefix = chop_base_prefix base_prefix in
       let ip = Adef.iper_of_int (Int32.to_int p.MLink.Person.ip) in
       Hashtbl.add ht_person_cache (base_prefix, ip) p;
       let faml_cache =
         try Hashtbl.find ht_families_cache (base_prefix, ip) with
         Not_found -> []
       in
       Hashtbl.replace ht_families_cache (base_prefix, ip)
         (p.MLink.Person.families @ faml_cache);
       let sn = p.MLink.Person.n in
       let fn = p.MLink.Person.p in
       let occ = Int32.to_int p.MLink.Person.oc in
       let key =
         sn ^ "|" ^ fn ^ "|" ^ if occ > 0 then string_of_int occ else ""
       in
       Hashtbl.add ht (base_prefix, key) ip)
    families.MLink.Link_tree.persons;

  (* Comme on a ajouté toutes les familles pour une personne donnée,  *)
  (* il se peut qu'il y ait des déblons. On les supprime ici.         *)
  Hashtbl.iter
    (fun k _ ->
      let l =
        try List.flatten (Hashtbl.find_all ht_families_cache k) with
        Not_found -> []
      in
      let ht = Hashtbl.create 1 in
      let rec loop accu l =
        match l with
        | [] -> accu
        | v :: l ->
            if Hashtbl.mem ht v then loop accu l
            else
              begin
                Hashtbl.add ht v ();
                loop (v :: accu) l
              end
      in
      let l = loop [] l in
      Hashtbl.replace ht_families_cache k l)
    ht_families_cache;

  (* Table temporaire qui sera utilisée pour le créer le cache définitif. *)
  let ht_corresp_tmp = Hashtbl.create 1 in
  List.iter
    (fun c ->
      let from_baseprefix = c.MLink.Connection.from_baseprefix in
      let from_ref = c.MLink.Connection.from_ref in
      let to_baseprefix = c.MLink.Connection.to_baseprefix in
      let to_ref = c.MLink.Connection.to_ref in
      try
        if from_baseprefix = base_prefix then
          match ip_of_ref_person base from_ref with
          | Some from_ip ->
              let to_ip = Hashtbl.find ht (to_baseprefix, to_ref) in
              Hashtbl.add ht_corresp_tmp
                (from_baseprefix, from_ip) (to_baseprefix, to_ip)
          | None -> ()
        else
          let from_ip = Hashtbl.find ht (from_baseprefix, from_ref) in
          let to_ip = Hashtbl.find ht (to_baseprefix, to_ref) in
          Hashtbl.add ht_corresp_tmp
            (from_baseprefix, from_ip) (to_baseprefix, to_ip)
      with Not_found -> ())
    families.MLink.Link_tree.connections;

  (* Il se peut qu'une même personne ait plusieurs correspondances.         *)
  (* On reparcours donc toutes les correspondances et on les rends uniques. *)
  (* On met à jour la vraie table de cache.                                 *)
  Hashtbl.iter
    (fun k _ ->
      let l =
        if Hashtbl.mem ht_corresp k then []
        else try Hashtbl.find_all ht_corresp_tmp k with Not_found -> []
      in
      let ht = Hashtbl.create 1 in
      let rec loop accu l =
        match l with
        | [] -> accu
        | v :: l ->
            if Hashtbl.mem ht v then loop accu l
            else
              begin
                Hashtbl.add ht v v;
                loop (v :: accu) l
              end
      in
      let l = loop [] l in
      List.iter
        (fun v -> Hashtbl.add ht_corresp k v)
        l)
    ht_corresp_tmp;
;;
