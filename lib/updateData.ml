(* $Id: source.ml,v 0.01 2012-07-12 10:19:08 flh Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open TemplAst
open Util

module DataSet =
  Set.Make
    (struct
       type t = Gwdb.istr * string * int
       let compare (_, s1, i1) (_, s2, i2) =
         let comp_s = Pervasives.compare s1 s2 in
         if comp_s = 0 then Pervasives.compare i1 i2 else comp_s
     end)

module PersMap = Map.Make (struct type t = int let compare = compare end)

module PersSet =
  Set.Make
    (struct
       type t = person
       let compare p1 p2 =
         let i1 = Adef.int_of_iper (get_key_index p1) in
         let i2 = Adef.int_of_iper (get_key_index p2) in
         Pervasives.compare i1 i2
     end)

module StringSet = Set.Make (struct type t = string let compare = compare end)

type fun_data_p_f =
    Person of (person -> istr)
  | Family of (family -> istr)
  | Pevent of ((iper, istr) gen_pers_event -> istr)
  | Fevent of ((iper, istr) gen_fam_event -> istr)


(* ******************************************************************** *)
(*  [Fonc] get_data : config -> (string * fun x -> istr) list * bool    *)
(** [Description] : Renvoie la liste des labels et fonctions pour
                    récupérer les données que l'on veut modifier.
    [Args] :
      - conf : configuration
    [Retour] :
      - (string * fun x -> istr) list * bool : La liste des données que
           l'on souhaite modifier ainsi qu'un boolean pour savoir s'il
           faut tester la famille.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let get_data conf =
  match p_getenv conf.env "data" with
    Some "occu" -> ["occu", Person get_occupation], false
  | Some "place" ->
      ["bi", Person get_birth_place; "bp", Person get_baptism_place;
       "de", Person get_death_place; "bu", Person get_burial_place;
       "pe", Pevent (fun evt -> evt.epers_place);
       "ma", Family get_marriage_place;
       "fe", Fevent (fun evt -> evt.efam_place)],
      true
  | Some "src" ->
      ["bi", Person get_birth_src; "bp", Person get_baptism_src;
       "de", Person get_death_src; "bu", Person get_burial_src;
       "pe", Pevent (fun evt -> evt.epers_src); "p", Person get_psources;
       "ma", Family get_marriage_src; "fe", Fevent (fun evt -> evt.efam_src);
       "f", Family get_fsources],
      true
  | Some "fn" -> ["fn", Person get_first_name], false
  | Some "sn" -> ["sn", Person get_surname], false
  | _ -> [], false


(* ******************************************************************** *)
(*  [Fonc] get_all_data : config -> base -> DataSet.elt list            *)
(** [Description] : Construit la liste de toutes les données de la base
                    que l'on veut modifier et leur associe une clé unique.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - (Gwdb.istr * int) list : La liste sans doublon de toutes les
          sources de la base avec leur clé.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let get_all_data conf base =
  let data_set = ref DataSet.empty in
  let (data, test_family) = get_data conf in
  (* Ajoute toutes les "data" liés à un individu *)
  let rec loop i =
    if i = nb_of_persons base then ()
    else
      let p = pget conf base (Adef.iper_of_int i) in
      List.iter
        (fun (label, fun_data) ->
           match fun_data with
             Person fun_data ->
               let istr = fun_data p in
               if not (is_empty_string istr) then
                 data_set :=
                   DataSet.add (istr, label, Hashtbl.hash istr) !data_set
           | Pevent fun_data ->
               List.iter
                 (fun evt ->
                    let istr = fun_data evt in
                    if not (is_empty_string istr) then
                      data_set :=
                        DataSet.add (istr, label, Hashtbl.hash istr)
                          !data_set)
                 (get_pevents p)
           | _ -> ())
        data;
      loop (i + 1)
  in
  loop 0;
  (* Ajoute toutes les "data" liés à une famille *)
  if test_family then
    begin let rec loop i =
      if i = nb_of_families base then ()
      else
        let fam = foi base (Adef.ifam_of_int i) in
        if is_deleted_family fam then ()
        else
          List.iter
            (fun (label, fun_data) ->
               match fun_data with
                 Family fun_data ->
                   let istr = fun_data fam in
                   if not (is_empty_string istr) then
                     data_set :=
                       DataSet.add (istr, label, Hashtbl.hash istr) !data_set
               | Fevent fun_data ->
                   List.iter
                     (fun evt ->
                        let istr = fun_data evt in
                        if not (is_empty_string istr) then
                          data_set :=
                            DataSet.add (istr, label, Hashtbl.hash istr)
                              !data_set)
                     (get_fevents fam)
               | _ -> ())
            data;
        loop (i + 1)
    in
      loop 0
    end;
  DataSet.elements !data_set


(* ************************************************************************** *)
(*  [Fonc] get_person_from_data :
             config -> base -> (Gwdb.istr * PersSet.elt list) list            *)
(** [Description] : Construit a partir de la clé d'une donnée, l'ensemble des
                    personnes en relation avec cette donnée.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - (Gwdb.istr * person list) : retourne la liste des adresses des données
          ainsi que la liste des personnes en relation avec cette donnée.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let get_person_from_data conf base =
  let (data, test_family) = get_data conf in
  let env_keys =
    let list = List.map fst data in
    List.map
      (fun key ->
         match p_getint conf.env key with
           Some hash -> key, hash
         | None -> key, -1)
      list
  in
  let pers_map = ref PersMap.empty in
  (* Fonction d'ajout dans la map des personnes (PersMap).         *)
  (* k = key, istr = data, p = person.                             *)
  (* A la clé k est associé le binding (istr, ensemble d'individu) *)
  let map_add k istr p =
    try
      let (istr, set) = PersMap.find k !pers_map in
      let set = PersSet.add p set in
      pers_map := PersMap.add k (istr, set) !pers_map
    with Not_found ->
      let set = PersSet.add p PersSet.empty in
      pers_map := PersMap.add k (istr, set) !pers_map
  in
  (* Parcours tous les individus et ajoute dans la map les    *)
  (* individus en relation avec la "data" donné par la clé k. *)
  let rec loop i =
    if i = nb_of_persons base then ()
    else
      let p = pget conf base (Adef.iper_of_int i) in
      List.iter
        (fun (label, fun_data) ->
           match fun_data with
             Person fun_data ->
               let istr = fun_data p in
               let hash_istr = Hashtbl.hash istr in
               let key = List.assoc label env_keys in
               if not (is_empty_string istr) && hash_istr = key then
                 map_add key istr p
           | Pevent fun_data ->
               List.iter
                 (fun evt ->
                    let istr = fun_data evt in
                    let hash_istr = Hashtbl.hash istr in
                    let key = List.assoc label env_keys in
                    if not (is_empty_string istr) && hash_istr = key then
                      map_add key istr p)
                 (get_pevents p)
           | _ -> ())
        data;
      loop (i + 1)
  in
  loop 0;
  (* Parcours toutes les familles et ajoute dans la map les    *)
  (* individus en relation avec la "data" donnée par la clé k. *)
  if test_family then
    begin let rec loop i =
      if i = nb_of_families base then ()
      else
        let fam = foi base (Adef.ifam_of_int i) in
        if is_deleted_family fam then ()
        else
          List.iter
            (fun (label, fun_data) ->
               match fun_data with
                 Family fun_data ->
                   let istr = fun_data fam in
                   let hash_istr = Hashtbl.hash istr in
                   let key = List.assoc label env_keys in
                   if not (is_empty_string istr) && hash_istr = key then
                     let p = pget conf base (get_father fam) in
                     map_add key istr p;
                     let p = pget conf base (get_mother fam) in
                     map_add key istr p
               | Fevent fun_data ->
                   List.iter
                     (fun evt ->
                        let istr = fun_data evt in
                        let hash_istr = Hashtbl.hash istr in
                        let key = List.assoc label env_keys in
                        if not (is_empty_string istr) && hash_istr = key then
                          let p = pget conf base (get_father fam) in
                          map_add key istr p;
                          let p = pget conf base (get_mother fam) in
                          map_add key istr p)
                     (get_fevents fam)
               | _ -> ())
            data;
        loop (i + 1)
    in
      loop 0
    end;
  (* On retourne la liste des couples ("data", persons list) *)
  let list = ref [] in
  PersMap.iter
    (fun _ (istr, pset) -> list := (istr, PersSet.elements pset) :: !list)
    !pers_map;
  !list


(* ************************************************************************** *)
(*  [Fonc] combine_by_ini : string -> (string * 'a * 'b) list ->
                              (string * ('a * 'b) list) list                  *)
(** [Description] : Retourne une liste en recombinant toutes les chaines en
                    fonction de leur début de chaîne.
    [Args] :
      - ini  : les premières lettres de la chaîne
      - list : la liste composée des premières lettres de la chaîne, 'a, 'b
    [Retour] :
      - (string * ('a * 'b) list) list
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let combine_by_ini ini list =
  let list =
    let rec loop new_list =
      function
        [] -> new_list
      | (k, s, cnt) :: list ->
          let ini_k =
            if String.length k > String.length ini then
              String.sub k 0 (index_of_next_char k (String.length ini))
            else k ^ String.make (String.length ini + 1 - String.length k) '_'
          in
          let ini_k = Bytes.unsafe_of_string ini_k in
          for i = 0 to Bytes.length ini_k - 1 do
            if Bytes.get ini_k i = ' ' then Bytes.set ini_k i '_'
          done;
          let ini_k = Bytes.unsafe_to_string ini_k in
          let new_list =
            if ini_k = "_" then new_list
            else
              match new_list with
                [] -> [ini_k, [s, cnt]]
              | (ini_k1, l) :: ll ->
                  if ini_k1 = ini_k then (ini_k1, (s, cnt) :: l) :: ll
                  else (ini_k, [s, cnt]) :: (ini_k1, l) :: ll
          in
          loop new_list list
    in
    loop [] list
  in
  List.fold_left (fun new_l (ini_k, l) -> (ini_k, l) :: new_l) [] list


(* ************************************************************************** *)
(*  [Fonc] combine : ('a * 'b * 'c) list -> ('a * ('b * 'c) list) list        *)
(** [Description] :
    [Args] :
      - list : la liste de triplets
    [Retour] :
      - ('a * ('b * 'c) list) list
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let combine list =
  let list =
    let rec loop new_list =
      function
        [] -> new_list
      | (k, s, cnt) :: list ->
          let new_list =
            match new_list with
              [] -> [k, [s, cnt]]
            | (ini_k1, l) :: ll ->
                if ini_k1 = k then (ini_k1, (s, cnt) :: l) :: ll
                else (k, [s, cnt]) :: (ini_k1, l) :: ll
          in
          loop new_list list
    in
    loop [] list
  in
  List.fold_left (fun new_l (ini_k, l) -> (ini_k, l) :: new_l) [] list


(* ************************************************************************** *)
(*  [Fonc] translate_title : config -> string * string                       *)
(** [Description] : Affiche le titre du dictionnaire
    [Args] :
      - conf : configuration
    [Retour] :
      - string : le nom du dictionnaire
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let translate_title conf =
  let title =
    match p_getenv conf.env "data" with
      Some "occu" -> transl_nth conf "occupation/occupations" 1
    | Some "place" -> transl conf "places"
    | Some "src" -> transl_nth conf "source/sources" 1
    | Some "fn" -> transl_nth conf "first name/first names" 1
    | Some "sn" -> transl_nth conf "surname/surnames" 1
    | _ -> ""
  in
  Printf.sprintf (ftransl conf "book of %s") title, title


(* ************************************************************************** *)
(*  [Fonc] print_title : config -> base -> string -> int -> unit              *)
(** [Description] : Affiche le titre du dictionnaire en fonction de data et
                    de la longueur de la liste
    [Args] :
      - conf : configuration
      - base : base
      - ini  : les premières lettres de la source
      - len  : le nombre de sources commençant par ini
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let print_title conf ini len =
  let (book_of, title) = translate_title conf in
  Wserver.printf "%s" (capitale book_of);
  if ini = "" then Wserver.printf " (%d %s)" len title
  else
    begin
      Wserver.printf " - ";
      Wserver.printf (fcapitale (ftransl conf "%d %s starting with %s")) len
        title ini
    end


(* ************************************************************************** *)
(*  [Fonc] print_long : config -> base -> (string * (string * int) list) list
                           -> int -> unit                                     *)
(** [Description] : Affiche le tableau des "data" en leur associant une clé
                    unique afin de pouvoir mettre un champs de modification.
    [Args] :
      - conf : configuration
      - base : base
      - list : liste de couple (data, (type (= bi, occu ...) * clé) list)
      - len  : la longueur de la liste
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let print_long conf list len =
  let env_keys =
    let list = ref [] in
    let keys = List.map fst (fst (get_data conf)) in
    let _ =
      List.map
        (fun key ->
           match p_getint conf.env key with
             Some hash -> list := (key, hash) :: !list
           | None -> ())
        keys
    in
    List.sort (fun (s1, _) (s2, _) -> compare s1 s2) !list
  in
  let data =
    match p_getenv conf.env "data" with
      Some s -> s
    | None -> ""
  in
  let ini =
    match p_getenv conf.env "s" with
      Some s -> s
    | None -> ""
  in
  (* Construit à partir de la liste de (src * hash) la liste dont      *)
  (* le premier composant est les premières lettres de la sources.     *)
  (* Attention, il ne faut pas faire String.length ini + 1 parce qu'en *)
  (* utf8, il se peut que le caractère soit codé sur plusieurs octets. *)
  let list =
    List.map
      (fun (s, k) ->
         let ini =
           if String.length s > String.length ini then
             String.sub s 0 (index_of_next_char s (String.length ini))
           else ini
         in
         ini, s, k)
      list
  in
  (* Re-combine la liste en fonction des premières *)
  (* lettres afin de pouvoir poser des ancres.     *)
  let list = combine_by_ini ini list in
  (* Astuce pour gérer les espaces. *)
  let list = List.map (fun (ini, l) -> Mutil.tr ' ' '_' ini, l) list in
  let list =
    List.sort (fun (ini1, _) (ini2, _) -> Gutil.alphabetic_order ini1 ini2)
      list
  in
  let title _ = print_title conf (Mutil.tr '_' ' ' ini) len in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<div class=\"tips\">\n";
  Wserver.printf "<table>\n";
  Wserver.printf "<tr>\n";
  Wserver.printf "<td>\n";
  Wserver.printf "%s" (capitale (transl conf "help modify data"));
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "</table>\n";
  Wserver.printf "</div>\n";
  Wserver.printf "<br%s>\n" conf.xhs;
  (* Ancre en haut de page afin de naviguer plus facilement. *)
  Wserver.printf "<table class=\"display_search\">\n";
  Wserver.printf "<tr>";
  List.iter
    (fun (ini, _) ->
       Wserver.printf "<td>";
       Wserver.printf "<a href=\"#%s\">" ini;
       Wserver.printf "%s" (no_html_tags ini);
       Wserver.printf "</a>\n";
       Wserver.printf "</td>")
    list;
  Wserver.printf "</tr>";
  (* Ancre en haut de page afin de naviguer plus facilement. *)
  Wserver.printf "</table>\n";
  Wserver.printf "<br%s>\n" conf.xhs;
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Wserver.printf "<ul>\n";
  List.iter
    (fun (ini_k, list) ->
       Wserver.printf "<li>\n";
       Wserver.printf "<a id=\"%s\">" ini_k;
       Wserver.printf "%s" (no_html_tags ini_k);
       Wserver.printf "</a>\n";
       Wserver.printf "<ul class=\"mod_data_ul\">\n";
       begin let list =
         List.sort (fun (s1, _) (s2, _) -> Gutil.alphabetic_order s1 s2) list
       in
         List.iter
           (fun (s, k) ->
              let k = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) k in
              Wserver.printf "<li>\n";
              if k <> env_keys then
                let k =
                  List.fold_left
                    (fun accu (k, i) ->
                       accu ^ k ^ "=" ^ string_of_int i ^ "&")
                    "" k
                in
                Wserver.printf "<a href=\"%sm=MOD_DATA&data=%s&%s&s=%s#mod\">"
                  (commd conf) data k (code_varenv ini);
                Wserver.printf "%s" (Util.escape_html s);
                Wserver.printf "</a>"
              else
                begin
                  Wserver.printf "<table class=\"mod_data_table\">\n";
                  begin
                    Wserver.printf "<tr>\n";
                    begin
                      Wserver.printf "<td>\n";
                      Wserver.printf "<a name=\"mod\">&nbsp;</a>";
                      (* envoie les données de façon masquée *)
                      Util.hidden_env conf;
                      List.iter
                        (fun (s, i) ->
                           Wserver.printf
                             "<input type=\"hidden\" name=\"%s\" value=\"%d\"%s>\n"
                             s i conf.xhs)
                        env_keys;
                      Wserver.printf
                        "<input type=\"hidden\" name=\"m\" value=\"MOD_DATA_OK\"%s>\n"
                        conf.xhs;
                      Wserver.printf
                        "<input type=\"hidden\" name=\"data\" value=\"%s\"%s>\n"
                        data conf.xhs;
                      Wserver.printf
                        "<input type=\"hidden\" name=\"s\" value=\"%s\"%s>\n"
                        ini conf.xhs;
                      Wserver.printf
                        "<input type=\"text\" name=\"nx_input\" size=\"80\" maxlength=\"%d\" value=\"%s\" id=\"nx_input\"%s>\n"
                        (if data = "src" then 300 else 200)
                        (Util.escape_html (only_printable s)) conf.xhs;
                      if data = "sn" then
                        begin
                          Wserver.printf
                            "<input type=\"checkbox\" name=\"surname_aliases\" value=\"yes\">\n";
                          Wserver.printf "%s"
                            (capitale
                               (transl conf
                                  "add the previous name as a surname alias"));
                          Wserver.printf "</input>\n"
                        end;
                      if data = "fn" then
                        begin
                          Wserver.printf
                            "<input type=\"checkbox\" name=\"firstname_aliases\" value=\"yes\">\n";
                          Wserver.printf "%s"
                            (capitale
                               (transl conf
                                  "add the previous name as a first name alias"));
                          Wserver.printf "</input>\n"
                        end;
                      Wserver.printf "</td>\n"
                    end;
                    begin
                      Wserver.printf "<td>\n";
                      begin
                        Wserver.printf
                          "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
                        Wserver.printf "%s"
                          (capitale (transl_nth conf "validate/delete" 0));
                        Wserver.printf "</button>\n"
                      end;
                      Wserver.printf "</td>\n"
                    end;
                    Wserver.printf "</tr>\n"
                  end;
                  Wserver.printf "</table>\n"
                end;
              Wserver.printf "</li>\n")
           list
       end;
       Wserver.printf "</ul>\n";
       Wserver.printf "</li>\n")
    list;
  Wserver.printf "</ul>\n";
  Wserver.printf "</form>\n";
  Hutil.trailer conf


(* ************************************************************************* *)
(*  [Fonc] print_short : config -> base -> (string * 'a) list -> int -> unit *)
(** [Description] : Si le nombre de "data" est trop grand, on affiche les
                    premières lettres de chaque "data".
    [Args] :
      - conf : configuration
      - base : base
      - list : la liste des "data"
      - len  : la longueur de la liste
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let print_short conf list len =
  let data =
    match p_getenv conf.env "data" with
      Some s -> s
    | None -> ""
  in
  let ini =
    match p_getenv conf.env "s" with
      Some s -> s
    | None -> ""
  in
  (* Construit la liste des string commençant par ini. *)
  (* Pour certaines données comme les sources, on peut *)
  (* avoir beaucoup de sources qui commencent par les  *)
  (* mêmes lettres. On calcul alors à partir de quelle *)
  (* lettre de ini, les sources sont différentes.      *)
  (* ex: eta -> etat -> etat_ -> ... -> etat_civil     *)
  let rec build_ini l len =
    (* Attention, il ne faut pas faire String.length     *)
    (* ini + 1 parce qu'en utf8, il se peut que le       *)
    (* caractère soit codé sur plusieurs octets.         *)
    let ini_list =
      List.rev_map
        (fun (s, _) ->
           if String.length s > len then
             String.sub s 0 (index_of_next_char s len)
           else s ^ String.make (len + 1 - String.length s) '_')
        l
    in
    (* Fonction pour supprimer les doublons. *)
    let remove_dup list =
      StringSet.elements
        (List.fold_left (fun accu ini -> StringSet.add ini accu)
           StringSet.empty list)
    in
    (* Astuce pour gérer les espaces. *)
    let ini_list = List.rev_map (fun p -> Mutil.tr ' ' '_' p) ini_list in
    let ini_list = remove_dup ini_list in
    (* Si la liste des ini n'a qu'un élément, on calcul on 'rang' d'après *)
    if List.length ini_list = 1 then build_ini list (len + 1)
    else List.sort Gutil.alphabetic_order ini_list
  in
  let ini_list = build_ini list (String.length ini) in
  let title _ = print_title conf (Mutil.tr '_' ' ' ini) len in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "%s :" (capitale (transl conf "select a letter"));
  Wserver.printf "<p class=\"list_ini\">\n";
  List.iter
    (fun s ->
       Wserver.printf "<a href=\"%sm=MOD_DATA&data=%s&s=%s\">" (commd conf)
         data (code_varenv s);
       Wserver.printf "%s" (no_html_tags s);
       Wserver.printf "</a>\n")
    ini_list;
  Wserver.printf "</p>\n";
  Hutil.trailer conf


let max_results = 1000

(* ************************************************************************** *)
(*  [Fonc] reduce_cpl_list : int -> ('a, 'b list) list -> ('a, 'b list) list  *)
(** [Description] : Retourne la sous liste telle que la somme des longueurs
                    des ('b list) soit égale à size.
    [Args] :
      - size : la taille de la liste retournée
      - list : la liste originale
    [Retour] :
      - list : la nouvelle liste dont la somme des ('b list) est égale à size
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let reduce_cpl_list size list =
  let rec loop size cnt reduced_list list =
    if cnt >= size then reduced_list
    else
      match list with
        [] -> reduced_list
      | (a, sl) :: l ->
          if List.length sl >= size - cnt then
            (a, Util.reduce_list (size - cnt) sl) :: reduced_list
          else loop size (cnt + List.length sl) ((a, sl) :: reduced_list) l
  in
  loop size 0 [] list


(* ************************************************************************** *)
(*  [Fonc] update_person : conf -> base -> string -> string -> person ->
                             gen_person iper istr                             *)
(** [Description] : Met à jour le/les champ(s) de la personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - old  : l'ancien contenu
      - new_input : le nouveau contenu
      - p : person
    [Retour] :
      - gen_person iper istr : gen_person avec les champs modifiés
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let update_person conf base old new_input p =
  match p_getenv conf.env "data" with
    Some "occu" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let occupation = get_occupation p in
      let s_occupation = sou base occupation in
      let occupation = if old = s_occupation then new_istr else occupation in
      {(gen_person_of_person p) with occupation = occupation}
  | Some "place" ->
      let new_istr =
        Gwdb.insert_string base (no_html_tags (only_printable new_input))
      in
      let pl_bi = get_birth_place p in
      let s_bi = sou base pl_bi in
      let pl_bp = get_baptism_place p in
      let s_bp = sou base pl_bp in
      let pl_de = get_death_place p in
      let s_de = sou base pl_de in
      let pl_bu = get_burial_place p in
      let s_bu = sou base pl_bu in
      let birth_place = if old = s_bi then new_istr else pl_bi in
      let baptism_place = if old = s_bp then new_istr else pl_bp in
      let death_place = if old = s_de then new_istr else pl_de in
      let burial_place = if old = s_bu then new_istr else pl_bu in
      let pevents =
        List.map
          (fun evt ->
             let pl_evt = evt.epers_place in
             let s_evt = sou base pl_evt in
             let place = if old = s_evt then new_istr else pl_evt in
             {evt with epers_place = place})
          (get_pevents p)
      in
      {(gen_person_of_person p) with birth_place = birth_place;
       baptism_place = baptism_place; death_place = death_place;
       burial_place = burial_place; pevents = pevents}
  | Some "src" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let src_bi = get_birth_src p in
      let s_bi = sou base src_bi in
      let src_bp = get_baptism_src p in
      let s_bp = sou base src_bp in
      let src_de = get_death_src p in
      let s_de = sou base src_de in
      let src_bu = get_burial_src p in
      let s_bu = sou base src_bu in
      let src_p = get_psources p in
      let s_p = sou base src_p in
      let birth_src = if old = s_bi then new_istr else src_bi in
      let baptism_src = if old = s_bp then new_istr else src_bp in
      let death_src = if old = s_de then new_istr else src_de in
      let burial_src = if old = s_bu then new_istr else src_bu in
      let psources_src = if old = s_p then new_istr else src_p in
      let pevents =
        List.map
          (fun evt ->
             let src_evt = evt.epers_src in
             let s_evt = sou base src_evt in
             let src = if old = s_evt then new_istr else src_evt in
             {evt with epers_src = src})
          (get_pevents p)
      in
      {(gen_person_of_person p) with birth_src = birth_src;
       baptism_src = baptism_src; death_src = death_src;
       burial_src = burial_src; psources = psources_src; pevents = pevents}
  | Some "fn" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let first_name = get_first_name p in
      let s_first_name = sou base first_name in
      let s_first_name_lower = Name.lower s_first_name in
      let (first_name, occ) =
        if old = s_first_name then
          new_istr,
          Gutil.find_free_occ base (sou base new_istr)
            (sou base (get_surname p)) 0
        else first_name, get_occ p
      in
      let first_names_aliases = get_first_names_aliases p in
      let first_names_aliases =
        if p_getenv conf.env "first_name_aliases" = Some "yes" then
          let has_first_name_alias =
            List.fold_left
              (fun has_first_name alias ->
                 has_first_name ||
                 s_first_name_lower = Name.lower (sou base alias))
              false first_names_aliases
          in
          if has_first_name_alias then first_names_aliases
          else get_first_name p :: first_names_aliases
        else first_names_aliases
      in
      {(gen_person_of_person p) with first_name = first_name; occ = occ;
       first_names_aliases = first_names_aliases}
  | Some "sn" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let surname = get_surname p in
      let s_surname = sou base surname in
      let s_surname_lower = Name.lower s_surname in
      let (surname, occ) =
        if old = s_surname then
          new_istr,
          Gutil.find_free_occ base (sou base (get_first_name p))
            (sou base new_istr) 0
        else surname, get_occ p
      in
      let surnames_aliases = get_surnames_aliases p in
      let surnames_aliases =
        if p_getenv conf.env "surname_aliases" = Some "yes" then
          let has_surname_alias =
            List.fold_left
              (fun has_surname alias ->
                 has_surname || s_surname_lower = Name.lower (sou base alias))
              false surnames_aliases
          in
          if has_surname_alias then surnames_aliases
          else get_surname p :: surnames_aliases
        else surnames_aliases
      in
      {(gen_person_of_person p) with surname = surname; occ = occ;
       surnames_aliases = surnames_aliases}
  | _ -> gen_person_of_person p


(* ************************************************************************** *)
(*  [Fonc] update_family : conf -> base -> string -> string -> person ->
                             gen_family ifam istr                             *)
(** [Description] : Met à jour le/les champ(s) de la famille.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - old       : l'ancien contenu
      - new_input : le nouveau contenu
      - fam       : family
    [Retour] :
      - gen_family ifam istr : gen_family avec les champs modifiés
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let update_family conf base old new_istr fam =
  match p_getenv conf.env "data" with
    Some "place" ->
      let new_istr =
        Gwdb.insert_string base (no_html_tags (only_printable new_istr))
      in
      let p_ma = get_marriage_place fam in
      let s_ma = sou base p_ma in
      let marriage_place = if old = s_ma then new_istr else p_ma in
      let fevents =
        List.map
          (fun evt ->
             let pl_evt = evt.efam_place in
             let s_evt = sou base pl_evt in
             let place = if old = s_evt then new_istr else pl_evt in
             {evt with efam_place = place})
          (get_fevents fam)
      in
      {(gen_family_of_family fam) with marriage_place = marriage_place;
       fevents = fevents}
  | Some "src" ->
      let new_istr = Gwdb.insert_string base (only_printable new_istr) in
      let src_ma = get_marriage_src fam in
      let s_ma = sou base src_ma in
      let src_f = get_fsources fam in
      let s_f = sou base src_f in
      let marriage_src = if old = s_ma then new_istr else src_ma in
      let fsources = if old = s_f then new_istr else src_f in
      let fevents =
        List.map
          (fun evt ->
             let src_evt = evt.efam_src in
             let s_evt = sou base src_evt in
             let src = if old = s_evt then new_istr else src_evt in
             {evt with efam_src = src})
          (get_fevents fam)
      in
      {(gen_family_of_family fam) with marriage_src = marriage_src;
       fsources = fsources; fevents = fevents}
  | _ -> gen_family_of_family fam


(* ********************************************************************** *)
(*  [Fonc] update_person_list :
             config -> base -> string -> (string * person) list -> int
               -> int -> unit                                             *)
(** [Description] :
    [Args] :
      - conf      : configuration
      - base      : base
      - new_input : le nouveau contenu
      - list      : la liste des (clé, person list)
      - nb_pers   : le nombre de personnes concernées par la mise à jour
      - max_updates = le nombre maximum de persons que l'on met à jour
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
let update_person_list conf base new_input list nb_pers max_updates =
  let (_, test_family) = get_data conf in
  let action =
    match p_getenv conf.env "data" with
      Some "occu" -> "co"
    | Some "place" -> "cp"
    | Some "src" -> "cs"
    | Some "fn" -> "fn"
    | Some "sn" -> "sn"
    | _ -> ""
  in
  let list =
    if nb_pers > max_updates then reduce_cpl_list max_updates list else list
  in
  List.iter
    (fun (old, perl) ->
       (* Mise à jour de toutes les personnes concernées. *)
       List.iter
         (fun p ->
            let o_p = Util.string_gen_person base (gen_person_of_person p) in
            let np = update_person conf base old new_input p in
            if action = "fn" || action = "sn" then
              begin let pi = np.key_index in
                let op = poi base pi in
                let key =
                  sou base np.first_name ^ " " ^ sou base np.surname
                in
                let ofn = p_first_name base op in
                let osn = p_surname base op in
                let oocc = get_occ op in
                delete_key base ofn osn oocc;
                patch_key base pi (sou base np.first_name)
                  (sou base np.surname) np.occ;
                patch_name base key pi;
                Update.update_misc_names_of_family base (get_sex p)
                  {family = get_family p};
                let sp =
                  Futil.map_person_ps (fun ip -> ip)
                    (fun istr -> sou base istr) np
                in
                UpdateIndOk.rename_image_file conf base op sp
              end;
            patch_person base np.key_index np;
            if test_family then
              begin let fam = Array.to_list (get_family p) in
                List.iter
                  (fun ifam ->
                     let fam = foi base ifam in
                     let nfam = update_family conf base old new_input fam in
                     patch_family base nfam.fam_index nfam)
                  fam
              end;
            (* On met aussi à jour l'historique. *)
            let changed =
              U_Multi
                (o_p, Util.string_gen_person base np,
                 (if action = "fn" || action = "sn" then true else false))
            in
            History.record conf base changed action)
         perl)
    list;
  Util.commit_patches conf base;
  (* On appelle explicitement notify_change car la base est modifiée.  *)
  (* On fait cet appel à la fin de chaque mise à jour de la liste des  *)
  (* personnes, car si l'administrateur de la base ne modifie pas tous *)
  (* les évènements liés à cette donnée, on ne sera pas mis au courant *)
  (* que la base à été mise à jour.                                    *)
  History.notify conf base action


(* ******************************************************************** *)
(*  [Fonc] print_mod_ok : config -> base -> unit                        *)
(** [Description] : Met à jour toutes les personnes en relation avec
                    la donnée que l'on veut modifié.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_mod_ok conf base =
  let data =
    match p_getenv conf.env "data" with
      Some s -> s
    | None -> ""
  in
  let ini =
    match p_getenv conf.env "s" with
      Some s -> s
    | None -> ""
  in
  let env_keys =
    let list = ref [] in
    let keys = List.map fst (fst (get_data conf)) in
    let _ =
      List.map
        (fun key ->
           match p_getint conf.env key with
             Some hash -> list := (key, hash) :: !list
           | None -> ())
        keys
    in
    List.sort (fun (s1, _) (s2, _) -> compare s1 s2) !list
  in
  let new_input =
    match p_getenv conf.env "nx_input" with
      Some s -> only_printable s
    | None -> ""
  in
  let list = get_person_from_data conf base in
  let list = List.map (fun (istr, perl) -> sou base istr, perl) list in
  let nb_pers =
    List.fold_left (fun accu (_, perl) -> accu + List.length perl) 0 list
  in
  let data_modified = List.for_all (fun (old, _) -> new_input <> old) list in
  (* Indication : 1000 fiches prend environ 1 seconde de traitement. *)
  (* Attention à ne pas mettre une limite trop grande (d'où le test) *)
  (* pour ne pas dépasser le time out du serveur.                    *)
  let max_updates =
    match p_getint conf.base_env "max_nb_update" with
      Some n -> if n > 50000 then 5000 else n
    | _ -> 5000
  in
  if nb_pers <> 0 && data_modified then
    begin
      update_person_list conf base new_input list nb_pers max_updates;
      let title _ =
        Wserver.printf "%s" (capitale (transl conf "modification successful"))
      in
      Hutil.header conf title;
      Hutil.print_link_to_welcome conf true;
      Wserver.printf "<p>\n";
      (* En attendant mieux ... *)
      Wserver.printf "%s%s %d "
        (capitale (transl conf "modification successful"))
        (Util.transl conf ":")
        (min nb_pers max_updates);
      if p_getenv conf.base_env "history" = Some "yes" then
        begin
          Wserver.printf "<a href=\"%sm=HIST&k=20\">" (commd conf);
          Wserver.printf "%s."
            (transl_nth conf "modification/modifications"
               (if nb_pers > 1 then 1 else 0));
          Wserver.printf "</a>"
        end
      else
        Wserver.printf "%s."
          (transl_nth conf "modification/modifications"
             (if nb_pers > 1 then 1 else 0));
      Wserver.printf "</p>\n";
      if nb_pers > max_updates then
        begin
          Wserver.printf "<form method=\"post\" action=\"%s\">\n"
            conf.command;
          begin
            Wserver.printf "<p>\n";
            Util.hidden_env conf;
            List.iter
              (fun (s, i) ->
                 Wserver.printf
                   "<input type=\"hidden\" name=\"%s\" value=\"%d\"%s>\n" s i
                   conf.xhs)
              env_keys;
            Wserver.printf
              "<input type=\"hidden\" name=\"m\" value=\"MOD_DATA_OK\"%s>\n"
              conf.xhs;
            Wserver.printf
              "<input type=\"hidden\" name=\"data\" value=\"%s\"%s>\n" data
              conf.xhs;
            Wserver.printf
              "<input type=\"hidden\" name=\"s\" value=\"%s\"%s>\n" ini
              conf.xhs;
            Wserver.printf
              "<input type=\"hidden\" name=\"nx_input\" size=\"80\" maxlength=\"200\" value=\"%s\" id=\"data\"%s>\n"
              (Util.escape_html (only_printable new_input)) conf.xhs;
            Wserver.printf "%s"
              (capitale (transl conf "continue correcting"));
            begin
              Wserver.printf
                "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
              Wserver.printf "%s"
                (capitale (transl_nth conf "validate/delete" 0));
              Wserver.printf "</button>\n"
            end;
            Wserver.printf "</p>\n"
          end;
          Wserver.printf "</form>\n"
        end;
      Wserver.printf "<p>\n";
      Wserver.printf "<a href=\"%sm=MOD_DATA&data=%s&s=%s\" id=\"reference\">"
        (commd conf) data ini;
      Wserver.printf "%s" (capitale (transl conf "new modification"));
      Wserver.printf "</a>";
      Wserver.printf "</p>\n";
      Hutil.trailer conf
    end
  else
    let title _ =
      Wserver.printf "%s" (capitale (transl conf "no modification"))
    in
    Hutil.header conf title;
    Hutil.print_link_to_welcome conf true;
    Wserver.printf "<p>\n";
    Wserver.printf "<a href=\"%sm=MOD_DATA&data=%s&s=%s\" id=\"reference\">" (commd conf) data
      ini;
    Wserver.printf "%s" (capitale (transl conf "new modification"));
    Wserver.printf "</a>";
    Wserver.printf "</p>\n";
    Hutil.trailer conf


(**/**) (* template *)


(* ********************************************************************* *)
(*  [Fonc] remove_suburb : string -> string                              *)
(** [Description] : Enlève le lieu-dit (de la forme
      "[Lieu-dit] - Commune...") d'une chaîne de caractères.
    [Args] :
      - s : chaîne de caractères contenant le lieu-dit.
    [Retour] : Retourne la chaîne de caractères dont le lieu-dit a été
      enlevé.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let remove_suburb s =
  let re = Str.regexp "^\\[.+\\] - " in
  let matched = Str.string_match re s 0 in
  if matched then
    let sub_start = Str.match_end () in
    String.sub s sub_start (String.length s - sub_start)
  else s

(* ********************************************************************* *)
(*  [Fonc] build_list : config -> base ->
                          (string * (string * int) list) list            *)
(** [Description] : Récupère la liste de toutes les "données" de la base.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] : Retourne la liste des données
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let build_list conf base =
  (* Paramètre pour savoir par quoi commence la chaine. *)
  let ini =
    match p_getenv conf.env "s" with
      Some s -> s
    | None -> ""
  in
  let list = get_all_data conf base in
  (* ! rev_map  = tail-rec ! *)
  let list = List.rev_map (fun (istr, s, k) -> sou base istr, s, k) list in
  (* On tri la liste avant de la combiner *)
  (* sinon on n'élimine pas les doublons. *)
  let list =
    List.sort
      (fun (s1, _, _) (s2, _, _) ->
         (fun s1 s2 ->
            let rss1 = remove_suburb s1 in
            let rss2 = remove_suburb s2 in
            (* Same place. *)
            if rss1 = rss2 then
              if s1 = rss2 then 1 else if rss1 = s2 then -1 else compare s1 s2
            else if rss1 > rss2 then 1
            else if rss1 < rss2 then -1
            else compare s1 s2)
           s1 s2)
      list
  in
  (* On combine la liste parce qu'en gwc2, les données peuvent être à  *)
  (* des adresses différentes. NB: on pourrait rassembler les lieux et *)
  (* les sources dans un seul index pour de meilleures performances.   *)
  let list = combine list in
  (* Fonction qui à une liste de données retourne la *)
  (* liste de toutes les données commençant par ini. *)
  let reduce l =
    List.filter
      (fun (data, _) ->
         Mutil.start_with ~wildcard:true ini 0 @@ remove_suburb data)
      l
  in
  if ini <> "" then reduce list else list


(* ************************************************************************* *)
(*  [Fonc] build_list_short : config -> base -> (string * 'a) list
                                -> string list                               *)
(** [Description] :
    [Args] :
      - conf : configuration
      - base : base
      - list : la liste des "data"
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let build_list_short conf list =
  let ini =
    match p_getenv conf.env "s" with
      Some s -> s
    | None -> ""
  in
  (* Construit la liste des string commençant par ini. *)
  (* Pour certaines données comme les sources, on peut *)
  (* avoir beaucoup de sources qui commencent par les  *)
  (* mêmes lettres. On calcul alors à partir de quelle *)
  (* lettre de ini, les sources sont différentes.      *)
  (* ex: eta -> etat -> etat_ -> ... -> etat_civil     *)
  let rec build_ini l len =
    (* Attention, il ne faut pas faire String.length     *)
    (* ini + 1 parce qu'en utf8, il se peut que le       *)
    (* caractère soit codé sur plusieurs octets.         *)
    let ini_list =
      List.rev_map
        (fun (s, _) ->
           let s = remove_suburb s in
           if String.length s > len then
             String.sub s 0 (index_of_next_char s len)
           else s ^ String.make (len + 1 - String.length s) '_')
        l
    in
    (* Fonction pour supprimer les doublons. *)
    let remove_dup list =
      StringSet.elements
        (List.fold_left (fun accu ini -> StringSet.add ini accu)
           StringSet.empty list)
    in
    (* Astuce pour gérer les espaces. *)
    let ini_list = List.rev_map (fun p -> Mutil.tr ' ' '_' p) ini_list in
    let ini_list = remove_dup ini_list in
    (* Si la liste des ini n'a qu'un élément, on calcul on 'rang' d'après *)
    if List.length ini_list = 1 then build_ini list (len + 1)
    else List.sort Gutil.alphabetic_order ini_list
  in
  build_ini list (String.length ini)


(* ************************************************************************* *)
(*  [Fonc] build_list_long : config -> base -> (string * 'a) list ->
                               (string * (string * 'a) list) list            *)
(** [Description] :
    [Args] :
      - conf : configuration
      - base : base
      - list : la liste des "data"
    [Retour] : La liste des couples (initiale de "data", "data")
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let build_list_long conf list =
  let ini =
    match p_getenv conf.env "s" with
      Some s -> s
    | None -> ""
  in
  (* Construit à partir de la liste de (src * hash) la liste dont      *)
  (* le premier composant est les premières lettres de la sources.     *)
  (* Attention, il ne faut pas faire String.length ini + 1 parce qu'en *)
  (* utf8, il se peut que le caractère soit codé sur plusieurs octets. *)
  let list =
    List.map
      (fun (s, k) ->
         let s1 = remove_suburb s in
         let ini =
           if String.length s1 > String.length ini then
             String.sub s1 0 (index_of_next_char s1 (String.length ini))
           else ini
         in
         ini, s, k)
      list
  in
  (* Re-combine la liste en fonction des premières *)
  (* lettres afin de pouvoir poser des ancres.     *)
  let list = combine_by_ini ini list in
  (* Astuce pour gérer les espaces. *)
  let list = List.map (fun (ini, l) -> Mutil.tr ' ' '_' ini, l) list in
  List.sort (fun (ini1, _) (ini2, _) -> Gutil.alphabetic_order ini1 ini2) list


type 'a env =
    Vlist_data of (string * (string * int) list) list
  | Vlist_ini of string list
  | Vlist_value of (string * (string * int) list) list
  | Venv_keys of (string * int) list
  | Vint of int
  | Vstring of string
  | Vother of 'a
  | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x
let bool_val x = VVbool x
let str_val x = VVstring x


let rec eval_var conf base env xx _loc sl =
  try eval_simple_var conf base env xx sl with
    Not_found -> eval_compound_var conf base env xx sl
and eval_simple_var conf base env xx =
  function
    [s] ->
      begin try bool_val (eval_simple_bool_var conf base env xx s) with
        Not_found -> str_val (eval_simple_str_var conf base env xx s)
      end
  | _ -> raise Not_found
and eval_simple_bool_var _conf _base env _xx =
  function
    "is_modified" ->
      let k =
        match get_env "keys" env with
          Venv_keys k -> k
        | _ -> []
      in
      let env_keys =
        match get_env "env_keys" env with
          Venv_keys env_keys -> env_keys
        | _ -> []
      in
      k = env_keys
  | _ -> raise Not_found
and eval_simple_str_var conf _base env _xx =
  function
    "entry_ini" -> eval_string_env "entry_ini" env
  | "entry_value" -> eval_string_env "entry_value" env
  | "ini" -> eval_string_env "ini" env
  | "keys" ->
      let k =
        match get_env "keys" env with
          Venv_keys k -> k
        | _ -> []
      in
      List.fold_left
        (fun accu (k, i) -> accu ^ k ^ "=" ^ string_of_int i ^ "&") "" k
  | "key_name" -> eval_string_env "key_name" env
  | "key_value" -> eval_int_env "key_value" env
  | "nb_results" ->
      begin match get_env "list" env with
        Vlist_data l -> string_of_int (List.length l)
      | _ -> "0"
      end
  | "title" ->
      let len =
        match get_env "list" env with
          Vlist_data l -> List.length l
        | _ -> 0
      in
      let ini =
        match p_getenv conf.env "s" with
          Some s -> s
        | None -> ""
      in
      let (book_of, title) = translate_title conf in
      let result =
        if ini = "" then Printf.sprintf " (%d %s)" len title
        else
          " - " ^
          Printf.sprintf (ftransl conf "%d %s starting with %s") len title ini
      in
      capitale book_of ^ result
  | _ -> raise Not_found
and eval_compound_var conf base env xx sl =
  let rec loop =
    function
      [s] -> eval_simple_str_var conf base env xx s
    | ["evar"; s] ->
        begin match p_getenv conf.env s with
          Some s -> s
        | None -> ""
        end
    | "encode" :: sl -> code_varenv (loop sl)
    | "escape" :: sl -> Util.escape_html (loop sl)
    | "html_encode" :: sl -> no_html_tags (loop sl)
    | "printable" :: sl -> only_printable (loop sl)
    | _ -> raise Not_found
  in
  str_val (loop sl)
and eval_string_env s env =
  match get_env s env with
    Vstring s -> s
  | _ -> raise Not_found
and eval_int_env s env =
  match get_env s env with
    Vint i -> string_of_int i
  | _ -> raise Not_found

let print_foreach conf print_ast _eval_expr =
  let rec print_foreach env xx _loc s sl el al =
    match s :: sl with
      ["initial"] -> print_foreach_initial env xx al
    | ["entry"] -> print_foreach_entry env xx el al
    | ["value"] -> print_foreach_value env xx al
    | ["env_keys"] -> print_foreach_env_keys env xx el al
    | _ -> raise Not_found
  and print_foreach_entry env xx _el al =
    let env_keys =
      let keys = List.map fst (fst (get_data conf)) in
      let list =
        List.fold_left
          (fun accu key ->
             match p_getint conf.env key with
               Some hash -> (key, hash) :: accu
             | None -> accu)
          [] keys
      in
      List.sort (fun (s1, _) (s2, _) -> compare s1 s2) list
    in
    let list =
      match get_env "list" env with
        Vlist_data l -> l
      | _ -> []
    in
    let list = build_list_long conf list in
    let env = ("env_keys", Venv_keys env_keys) :: env in
    let rec loop =
      function
        (ini_k, list_v) :: l ->
          let env =
            ("entry_ini", Vstring ini_k) ::
            ("list_value", Vlist_value list_v) :: env
          in
          List.iter (print_ast env xx) al; loop l
      | [] -> ()
    in
    loop list
  and print_foreach_value env xx al =
    let list =
      match get_env "list_value" env with
        Vlist_value l ->
          List.sort
            (fun (s1, _) (s2, _) ->
               let rss1 = remove_suburb s1 in
               let rss2 = remove_suburb s2 in
               if rss1 = rss2 then Gutil.alphabetic_order s1 s2
               else Gutil.alphabetic_order rss1 rss2)
            l
      | _ -> []
    in
    let rec loop =
      function
        (s, k) :: l ->
          let k = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) k in
          let env =
            ("entry_value", Vstring s) :: ("keys", Venv_keys k) :: env
          in
          List.iter (print_ast env xx) al; loop l
      | [] -> ()
    in
    loop list
  and print_foreach_initial env xx al =
    let list =
      match get_env "list" env with
        Vlist_data l -> l
      | _ -> []
    in
    let ini_list = build_list_short conf list in
    let rec loop =
      function
        ini :: l ->
          let env = ("ini", Vstring ini) :: env in
          List.iter (print_ast env xx) al; loop l
      | [] -> ()
    in
    loop ini_list
  and print_foreach_env_keys env xx _el al =
    let env_keys =
      match get_env "env_keys" env with
        Venv_keys env_keys -> env_keys
      | _ -> []
    in
    let rec loop =
      function
        (s, i) :: l ->
          let env = ("key_name", Vstring s) :: ("key_value", Vint i) :: env in
          List.iter (print_ast env xx) al; loop l
      | [] -> ()
    in
    loop env_keys
  in
  print_foreach

let print_mod conf base =
  match p_getenv conf.env "data" with
  | Some ("place" | "src" | "occu" | "fn" | "sn") ->
    let list = build_list conf base in
    let env = ["list", Vlist_data list] in
    Hutil.interp conf "upddata"
      {Templ.eval_var = eval_var conf base;
       Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
       Templ.eval_predefined_apply = (fun _ -> raise Not_found);
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach = print_foreach conf}
      env ()
  | _ ->
    Hutil.interp conf "upddatamenu"
      {Templ.eval_var = (fun _ -> raise Not_found);
       Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
       Templ.eval_predefined_apply = (fun _ -> raise Not_found);
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach = fun _ -> raise Not_found}
      [] ()
