#ifdef API

module M = Api_piqi
module Mext = Api_piqi_ext

open Config
open Gwdb
open Def
open Util
open Api_def
open Api_util

let string_start_with ini s =
  let rec loop i1 i2 =
    if i1 = String.length ini then true
    else if i2 = String.length s then
      if ini.[i1] = '_' then loop (i1 + 1) i2
      else false
    else if s.[i2] = ini.[i1] || s.[i2] = ' ' && ini.[i1] = '_' then
      loop (i1 + 1) (i2 + 1)
    else false
  in loop 0 0

(* Algo de Knuth-Morris-Pratt *)
let init_next p =
  let m = String.length p in
  let next = Array.make m 0 in
  let i = ref 1 and j = ref 0 in
  while !i < m - 1 do
    if p.[!i] = p.[!j] then begin incr i; incr j; next.(!i) <- !j end
    else if !j = 0 then begin incr i; next.(!i) <- 0 end
    else j := next.(!j)
  done;
  next

(* Algo de Knuth-Morris-Pratt *)
let kmp p s =
  let p = (Name.lower p) in
  let s = (Name.lower s) in
  (* Optimisation pour GeneWeb *)
  if s = "" || s = "?" then false
  else
    begin
      let next = init_next p and m = String.length p in
      let n = String.length s and i = ref 0 and j = ref 0 in
      while !j < m && !i < n do
        if s.[!i] = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.(!j)
      done;
      if !j >= m then true else false
    end

(* FIXME: DUPLICATE OF ALLN.SELECT ??? *)
(* ************************************************************************** *)
(*  [Fonc] get_list_of_select_start_with :
    config -> base -> bool -> string -> bool -> name -> letter                *)
(** [Description] : Fonction qui scanne l'ensemble des noms de la base
    pour une lettre donnée et retourne une liste de personne.
    [Args] :
      - conf            : configuration de la base
      - base            : base de donnée
      - is_surnames     : si True recherche sur les noms de famille
      - ini_p           : début du nom
      - ini_n           : début du prénom
      - need_whole_list : si True, remonte tout
      - letter          : la première lettre des noms/prénoms concernés
    [Retour] :
      - ListPersons : Retourne une liste de personnes.
                                                                              *)
(* ************************************************************************** *)
let get_list_of_select_start_with conf base ini_n ini_p need_whole_list letter =
    let name =
    (* Si le nom est défini, on parcourt un tableau de noms *)
    if "" <> ini_n
    then
        persons_of_surname base
    else
        (* Sinon, on parcourt un tableau de prénoms *)
        persons_of_first_name base
    in
    try
      (* Itère sur chaque entrée du tableau qui commence par la lettre letter *)
      let istr = spi_first name letter in
        let rec loop istr list =
          let s = Mutil.nominative (sou base istr) in
          let k = Util.name_key base s in
            (* Vérifie que le début du nom de famille de la personne correspond à celui demandé *)
            if "" = ini_n || string_start_with (Name.lower ini_n) (Name.lower k) then
              let list =
                if s <> "?" then
                  let my_list = spi_find name istr in
                  let my_list =
                    List.fold_left
                     (fun l ip ->
                        let p = poi base ip in
                        let isn = get_surname p in
                        if "" <> ini_n
                        then
                            if eq_istr isn istr then
                                if "" <> ini_p
                                then
                                    let isp = sou base (get_first_name p) in
                                    if eq_istr isn istr && string_start_with (Name.lower ini_p) (Name.lower isp)
                                    then
                                        (* Prénom===Prénom && Nom===Nom *)
                                        (ip :: l)
                                    else l
                                else
                                    (* Prénom===* && Nom===Nom *)
                                    (ip :: l)
                            else l
                        else
                            if "" <> ini_p
                            then
                                let isp = sou base (get_first_name p) in
                                if string_start_with (Name.lower ini_p) (Name.lower isp)
                                then
                                    (* Prénom===Prénom && Nom===* *)
                                    (ip :: l)
                                else l
                            else
                                (* Prénom===* && Nom===* *)
                                (ip :: l)
                        )
                     [] my_list
                  in
                  let my_list =
                    if conf.use_restrict then
                      List.fold_left
                        (fun l ip ->
                          if is_restricted conf base ip then l
                           else (ip :: l) )
                        [] my_list
                    else my_list
                  in
                  (* Ajoute à list les personnes trouvées *)
                  List.rev_append my_list list
                (* Sort totalement de l'itération puisque les personnes ne sont plus définies *)
                else list
              in
              match spi_next name istr need_whole_list with
              | (istr, _) -> loop istr list
              | exception Not_found -> list
            else
              match spi_next name istr need_whole_list with
              | (istr, _) -> loop istr list
              | exception Not_found -> list
        (* Première itération, on initialise au passage list comme tableau vide *)
        in loop istr []
    with Not_found -> []

(* ************************************************************************** *)
(*  [Fonc] select_start_with :
    config -> base -> string -> string -> bool                                  *)
(** [Description] : Retourne une liste de personne dont le nom OU le prénom
    commence par 'ini_n' ou 'ini_p'.
    [Args] :
      - conf            : configuration de la base
      - base            : base de donnée
      - ini_n           : début du nom à chercher
      - ini_p           : début du nom à chercher
      - need_whole_list : si True, remonte tout
    [Retour] :
      - ListPersons : Retourne une liste de personnes.
                                                                              *)
(* ************************************************************************** *)
let select_start_with conf base ini_n ini_p need_whole_list =
  let ini_n = Util.name_key base ini_n in
  let start =
    if ini_n <> ""
    then
      Mutil.tr '_' ' ' ini_n
    else
      Mutil.tr '_' ' ' ini_p
  in
  let list_min =
    let letter = String.lowercase_ascii (String.sub start 0 1) in
    get_list_of_select_start_with conf base ini_n ini_p need_whole_list letter
  in
  let list_maj =
    let letter = String.uppercase_ascii (String.sub start 0 1) in
    get_list_of_select_start_with conf base ini_n ini_p need_whole_list letter
  in
  List.rev_append list_maj list_min


let select_both_all base ini_n ini_p maiden_name =
  let find_sn p x = kmp x (sou base (get_surname p)) in
  let find_fn p x = kmp x (sou base (get_first_name p)) in
  let find_str s x = kmp x s in
  let ini_n = Util.name_key base ini_n in
  let ini_n = code_varenv ini_n in
  let ini_n =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_n []
  in
  let ini_n = List.filter (fun s -> s <> "") ini_n in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = code_varenv ini_p in
  let ini_p =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_p []
  in
  let add_maiden p ini_p l =
    let rec loop ifam_l l =
      match ifam_l with
      | [] -> ()
      | ifam :: q ->
          let fam = foi base ifam in
          let ip = Gutil.spouse (get_key_index p) fam in
          let sp = poi base ip in
          if List.for_all (fun s -> find_fn sp s) ini_p then l := ip :: !l;
          loop q l
    in
    if get_sex p = Male then
      loop (Array.to_list (get_family p)) l
  in
  let add_maiden2 p ini_n ini_p l =
    (* On sépare les noms avec tirets ... *)
    let ini_n =
      List.fold_left
        (fun accu s -> List.rev_append (String.split_on_char '-' s) accu)
        [] ini_n
    in
    let rec loop ifam_l l =
      match ifam_l with
      | [] -> ()
      | ifam :: q ->
          let fam = foi base ifam in
          let ip = Gutil.spouse (get_key_index p) fam in
          let sp = poi base ip in
          let names =
            sou base (get_surname p) ^ " " ^ sou base (get_surname sp)
          in
          if List.for_all (fun s -> find_str names s) ini_n then
            add_maiden p ini_p l;
          loop q l
    in
    if get_sex p = Male then
      if List.exists (fun s -> find_sn p s) ini_n then
        loop (Array.to_list (get_family p)) l
  in
  let list = ref [] in
  for i = 0 to nb_of_persons base - 1 do
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    if List.for_all (fun s -> find_sn p s) ini_n then
      begin
        if List.for_all (fun s -> find_fn p s) ini_p then
          list := ip :: !list
        else if maiden_name then add_maiden p ini_p list
      end
    else
      (* On cherche une partie du nom de jeune fille dans les noms donnés. *)
      if maiden_name then add_maiden2 p ini_n ini_p list
  done;
  !list

let select_all base is_surnames ini =
  let find p x =
    if is_surnames then kmp x (sou base (get_surname p))
    else kmp x (sou base (get_first_name p))
  in
  let ini =
    if is_surnames then Util.name_key base ini
    else ini
  in
  let ini = code_varenv ini in
  let ini =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini []
  in
  let list = ref [] in
  for i = 0 to nb_of_persons base - 1 do
    if List.for_all (fun s -> find (poi base (Adef.iper_of_int i)) s) ini
    then list := (Adef.iper_of_int i) :: !list
  done;
  !list


module Iper =
  struct
    type t = Adef.iper
    let compare i1 i2 =
      Pervasives.compare (Adef.int_of_iper i1) (Adef.int_of_iper i2)
  end

module IperSet = Set.Make(Iper)

let print_list conf base filters list =
  let person_l =
    IperSet.elements
      (List.fold_left
         (fun accu p -> IperSet.add p accu)
         IperSet.empty list)
  in
  let person_l = List.rev_map (poi base) person_l in
  let person_l =
    if filters.nb_results then person_l
    else
      List.sort
        (fun p1 p2 ->
          let sn1 = Name.lower (p_surname base p1) in
          let sn2 = Name.lower (p_surname base p2) in
          let comp = Gutil.alphabetic_order sn1 sn2 in
          if comp = 0 then
            let fn1 = Name.lower (p_first_name base p1) in
            let fn2 = Name.lower (p_first_name base p2) in
            Gutil.alphabetic_order fn1 fn2
          else comp)
        person_l
  in
  let data = data_list_person conf base filters person_l in
  print_result conf data

(*
   La différence entre la recherche approximative et lastname_or_surname est
   si on cherche un nom ET un prénom (dans les autres cas, on obtient les
   mêmes résultats.
   De ce fait, on utilise list_n = select_all n, list_p = select_all p et on
   fait l'union des deux ce qui est beaucoup plus efficace.
*)
let print_search conf base =
  let search_params = get_params conf (fun x f -> Mext.parse_search_params x f) in
  let filters = get_filters conf in
  match
     (search_params.M.Search_params.lastname,
      search_params.M.Search_params.firstname)
  with
   | (Some n, Some fs) ->
      let _ = load_strings_array base in
      let list =
        if Name.lower n = "" && Name.lower fs = "" then
          []
        else
          let maiden_name = search_params.M.Search_params.maiden_name in
          match search_params.M.Search_params.search_type with
          | `starting_with -> select_start_with conf base n fs true
          | `approximative -> select_both_all base n fs maiden_name
          | `lastname_or_firstname ->
               let list_n = select_all base true n in
               let list_p = select_all base false fs in
               List.rev_append list_n list_p
      in
      print_list conf base filters list
  | (Some n, None) ->
      let _ = load_strings_array base in
      let list =
        if Name.lower n = "" then
          []
        else
          match search_params.M.Search_params.search_type with
          | `starting_with -> select_start_with conf base n "" true
          | `approximative -> select_all base true n
          | `lastname_or_firstname -> select_all base true n
      in
      print_list conf base filters list
  | (None, Some fs) ->
      let _ = load_strings_array base in
      let list =
        if Name.lower fs = "" then
          []
        else
          match search_params.M.Search_params.search_type with
          | `starting_with -> select_start_with conf base "" fs true
          | `approximative -> select_all base false fs
          | `lastname_or_firstname -> select_all base false fs
      in
      print_list conf base filters list
  | (None, None) -> ()



(**/**) (* Recherche utilisée pour l'auto-completion ou relier personne. *)


module StrSetAutoComplete =
  Set.Make
    (struct
      type t = string
      let compare = compare
     end)

let rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i = ' ' then skip_spaces x (i + 1)
  else i

let rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i

let string_incl_start_with x y =
  let rec loop j_ini =
    if j_ini = String.length y then false
    else
      let rec loop1 i j =
        if i = String.length x then true
        else if j = String.length y then
          if x.[i] = '_' then loop1 (i + 1) j
          else false
        else if y.[j] = x.[i] || y.[j] = ' ' && x.[i] = '_' then
          loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
      in
      loop1 0 j_ini
  in
  loop 0

let select_both_start_with_person base ini_n ini_p =
  let find n x = string_start_with x n in
  let rec cut_at_space s acc =
    if String.contains s '+' then
      let index = String.index s '+' in
      let start = index + 1 in
      let len = String.length s - start in
      let ns = String.sub s start len in
      cut_at_space ns (decode_varenv (String.sub s 0 index) :: acc)
    else (decode_varenv s :: acc)
  in
  let ini_n = cut_at_space (code_varenv (Name.lower ini_n)) [] in
  let ini_p = cut_at_space (code_varenv (Name.lower ini_p)) [] in
  let rec loop i list nb_res =
    if (*nb_res < max_res &&*) i < nb_of_persons base then
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      let surnames =
        cut_at_space (code_varenv (Name.lower (sou base (get_surname p)))) []
      in
      let first_names =
        cut_at_space
          (code_varenv (Name.lower (sou base (get_first_name p)))) []
      in
      let start_surname =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) surnames)
          ini_n
      in
      let start_firstname =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) first_names)
          ini_p
      in
      if start_surname && start_firstname then
        loop (i + 1) (ip :: list) (nb_res + 1)
      else loop (i + 1) list nb_res
    else list
  in
  loop 0 [] 0

let select_start_with_person base get_field ini =
  let find n x = string_start_with x n in
  let rec cut_at_space s acc =
    if String.contains s '+' then
      let index = String.index s '+' in
      let start = index + 1 in
      let len = String.length s - start in
      let ns = String.sub s start len in
      cut_at_space ns (decode_varenv (String.sub s 0 index) :: acc)
    else (decode_varenv s :: acc)
  in
  let ini = cut_at_space (code_varenv (Name.lower ini)) [] in
  let rec loop i list nb_res =
    if (*nb_res < max_res &&*) i < nb_of_persons base then
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      let names =
        cut_at_space (code_varenv (Name.lower (sou base (get_field p)))) []
      in
      let start_name =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) names)
          ini
      in
      if start_name then loop (i + 1) (ip :: list) (nb_res + 1)
      else loop (i + 1) list nb_res
    else list
  in
  loop 0 [] 0


let select_start_with_auto_complete base mode max_res ini =
  let need_whole_list = true in
  let name =
    match mode with
    | `lastname -> persons_of_surname base
    | `firstname -> persons_of_first_name base
    | `place -> failwith "cannot use select_start_with_auto_complete"
    | `source -> failwith "cannot use select_start_with_auto_complete"
  in
  let string_set = ref StrSetAutoComplete.empty in
  let nb_res = ref 0 in
  (* Si la base est grosse > 100 000, on fait un vrai start_with. *)
  if Gwdb.nb_of_persons base > 100000 then
    begin
      (* majuscule *)
      let ini =
        match mode with
        | `lastname -> Util.name_key base ini
        | `firstname -> ini
        | `place -> failwith "cannot use select_start_with_auto_complete"
        | `source -> failwith "cannot use select_start_with_auto_complete"
      in
      let start_k = Mutil.tr '_' ' ' ini in
      let letter = String.uppercase_ascii (String.sub start_k 0 1) in
      match spi_first name letter with
      | istr ->
          let rec loop istr =
            let s = sou base istr in
            let k = Util.name_key base s in
            if string_start_with (Name.lower ini) (Name.lower k) then
              begin
                string_set := StrSetAutoComplete.add s !string_set;
                incr nb_res;
                match spi_next name istr need_whole_list with
                | (istr, _) when !nb_res < max_res && (String.sub k 0 1) = letter -> loop istr
                | _ -> ()
                | exception Not_found -> ()
              end
            else
              match spi_next name istr need_whole_list with
              | (istr, _) when !nb_res < max_res && (String.sub k 0 1) = letter -> loop istr
              | _ -> ()
              | exception Not_found -> ()
          in loop istr
      | exception Not_found -> ();
      (* minuscule *)
      if !nb_res < max_res then
        let ini =
          match mode with
          | `lastname -> Util.name_key base ini
          | `firstname -> ini
          | `place -> failwith "cannot use select_start_with_auto_complete"
          | `source -> failwith "cannot use select_start_with_auto_complete"
        in
        let start_k = Mutil.tr '_' ' ' ini in
        let letter = String.lowercase_ascii (String.sub start_k 0 1) in
        match spi_first name letter with
        | istr ->
            let rec loop istr =
              let s = sou base istr in
              let k = Util.name_key base s in
              if string_start_with (Name.lower ini) (Name.lower k) then
                begin
                  string_set := StrSetAutoComplete.add s !string_set;
                  incr nb_res;
                  match spi_next name istr need_whole_list with
                  | exception Not_found -> ()
                  | (istr, _) when !nb_res < max_res && (String.sub k 0 1) = letter -> loop istr
                  | _ -> ()
                end
              else
                match spi_next name istr need_whole_list with
                | exception Not_found -> ()
                | (istr, _) when !nb_res < max_res && (String.sub k 0 1) = letter -> loop istr
                | _ -> ()
            in loop istr
        | exception Not_found -> ()
    end
  else
    begin
      (* On commence à ? comme ça on fait MAJ et MIN. *)
      let start_k = Mutil.tr '_' ' ' "?" in
      let letter = String.uppercase_ascii (String.sub start_k 0 1) in
      match spi_first name letter with
      | exception Not_found -> ()
      | istr ->
          let rec loop istr list =
            let s = sou base istr in
            let k = Util.name_key base s in
            if string_incl_start_with (Name.lower ini) (Name.lower k) then
              begin
                string_set := StrSetAutoComplete.add (sou base istr) !string_set;
                incr nb_res;
                match spi_next name istr need_whole_list with
                | exception Not_found -> ()
                | (istr, _) when !nb_res < max_res -> loop istr list
                | _ -> ()
              end
            else
              match spi_next name istr need_whole_list with
              | exception Not_found -> ()
              | (istr, _) when !nb_res < max_res -> loop istr list
              | _ -> ()
          in loop istr []
    end;
  List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)


let select_all_auto_complete _ base get_field max_res ini =
  let find p x = kmp x (sou base (get_field p)) in
  let ini = code_varenv ini in
  let ini =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini []
  in
  let string_set = ref StrSetAutoComplete.empty in
  let nb_res = ref 0 in
  for i = 0 to nb_of_persons base - 1 do
    if !nb_res < max_res then
      if List.for_all (fun s -> find (poi base (Adef.iper_of_int i)) s) ini
      then
        begin
        let p = poi base (Adef.iper_of_int i) in
        string_set := StrSetAutoComplete.add (sou base (get_field p)) !string_set;
        incr nb_res;
        end
  done;
  List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)


let load_dico_lieu conf pl_mode =
  let fname =
    match pl_mode with
    | `town -> "dico_place_town_" ^ conf.lang ^ ".list"
    | `area_code -> "dico_place_area_code_" ^ conf.lang ^ ".list"
    | `county -> "dico_place_county_" ^ conf.lang ^ ".list"
    | `region -> "dico_place_region_" ^ conf.lang ^ ".list"
    | `country -> "dico_place_country_" ^ conf.lang ^ ".list"
    | _ -> ""
  in
  if fname = "" then []
  else
    let fname = Filename.concat "lang" fname in
    try
      let ic = Secure.open_in (Util.search_in_lang_path fname) in
      let list : (string list) = input_value ic in
      close_in ic;
      list
    with Sys_error _ -> []


let get_field mode =
  match mode with
  | `lastname -> get_surname
  | `firstname -> get_first_name
  | _ -> failwith "get_field"


let search_auto_complete conf base mode place_mode max_res n =
  if mode = `place then
    begin
      let conf = {(conf) with env = ("data", "place") :: conf.env } in
      let list = UpdateData.get_all_data conf base in
      (* On fait un rev_map (tail-rec) parce que si le nombre de *)
      (* données est trop important, on casser la pile d'appels. *)
      let list = List.rev_map (fun (istr, s, k) -> (sou base istr, s, k)) list in
      (* On tri la liste avant de la combiner *)
      (* sinon on n'élimine pas les doublons. *)
      let list = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) list in
      (* On combine la liste parce qu'en gwc2, les données peuvent être à  *)
      (* des adresses différentes. NB: on pourrait rassembler les lieux et *)
      (* les sources dans un seul index pour de meilleures performances.   *)
      let list = UpdateData.combine list in
      let string_set1 = ref StringSetAutoComplete.empty in
      (*
      let rec loop place =
        let (_, place) =
          try Api_util.split place ','
          with Not_found -> ("", "")
        in
        if place = "" then ()
        else
          begin
            string_set1 := StringSetAutoComplete.add place !string_set1;
            loop place
          end
      in
      *)
      let () =
        List.iter
          (fun (place, _) ->
             begin
               string_set1 := StringSetAutoComplete.add place !string_set1;
               (*loop place*)
             end)
          list
      in
      let list = StringSetAutoComplete.elements !string_set1 in
      (* Fonction qui à une liste de données retourne la *)
      (* liste de toutes les données commençant par ini. *)
      let nb_res = ref 0 in
      let string_set = ref StrSetAutoComplete.empty in
      let ini = Mutil.tr '_' ' ' n in
      let place_format =
        match p_getenv conf.base_env "places_format" with
        | None -> []
        | Some s ->
          try
            List.map
              (function
                | "Subdivision" -> `subdivision
                | "Town" -> `town
                | "Area code" -> `area_code
                | "County" -> `county
                | "Region" -> `region
                | "Country" -> `country
                | _ -> raise Not_found)
              (String.split_on_char ',' s)
          with Not_found -> []
      in
      let len_place_format = List.length place_format in
      let rec reduce l accu mode_dico max_res =
        match l with
        | [] -> ()
        | data :: l ->
            begin
              let k =  Mutil.tr '_' ' ' data in
              let (has_subdivision, i) =
                try
                  (true, String.rindex k ']')
                with Not_found -> (false, 0)
              in
              let k =
                if has_subdivision && place_mode <> Some `subdivision then
                  let s =
                    String.sub k (i + 1) (String.length k - i - 1)
                  in
                  Mutil.strip_all_trailing_spaces s
                else k
              in
              if string_start_with (Name.lower ini) (Name.lower k) then
                begin
                  let tmps =
                    if has_subdivision && place_mode = Some `subdivision then
                      let s = String.sub k (i + 1) (String.length k - i - 1) in
                      if s = "" then 1
                      else match Util.nb_char_occ ',' s with
                        | 0 -> 2
                        | x -> x + 2
                    else match Util.nb_char_occ ',' k with
                      | 0 -> 2
                      | x -> x + 2
                  in
                  if len_place_format = 0 || tmps = len_place_format || mode_dico then
                  begin
                    let data =
                      if mode_dico && len_place_format > 0 then
                        let expl_data = String.split_on_char ',' data |> List.rev in
                        String.concat ", " @@
                        Util.filter_map
                          (function
                            | `town -> List.nth_opt expl_data 4
                            | `area_code -> List.nth_opt expl_data 3
                            | `county -> List.nth_opt expl_data 2
                            | `region -> List.nth_opt expl_data 1
                            | `country -> List.nth_opt expl_data 0
                            | _ -> None)
                          place_format
                      else
                        data
                    in
                  string_set := StrSetAutoComplete.add data !string_set;
                  incr nb_res;
                  end
                end;
              if !nb_res > max_res then ()
              else reduce l accu mode_dico max_res
            end
      in
      let () = reduce list [] false max_res in
      let base_place = StrSetAutoComplete.elements !string_set in
      let found_res = StrSetAutoComplete.cardinal !string_set in
      let dico_place =
        if found_res >= max_res then []
        else
          begin
            match place_mode with
            | Some pl_mode ->
                let dico = load_dico_lieu conf pl_mode in
                string_set := StrSetAutoComplete.empty;
                reduce dico [] true (max_res - found_res);
                StrSetAutoComplete.elements !string_set;
                (*
                (match pl_mode with
                 | `town -> reduce towns []
                 | `area_code -> reduce area_codes []
                 | `county -> reduce countys []
                 | `region -> reduce regions []
                 | `country -> reduce countrys []
                 | _ -> ())
                *)
            | None -> []
          end;
      in
      let base_place = List.sort Gutil.alphabetic_order base_place in
      let dico_place = List.sort Gutil.alphabetic_order dico_place in
      base_place @ dico_place
      (*
      List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)
      *)
    end
  else
    begin
      if mode = `source then
        begin
          let conf = {(conf) with env = ("data", "src") :: conf.env } in
          let list = UpdateData.get_all_data conf base in
          (* On fait un rev_map (tail-rec) parce que si le nombre de *)
          (* données est trop important, on casser la pile d'appels. *)
          let list = List.rev_map (fun (istr, s, k) -> (sou base istr, s, k)) list in
          (* On tri la liste avant de la combiner *)
          (* sinon on n'élimine pas les doublons. *)
          let list = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) list in
          (* On combine la liste parce qu'en gwc2, les données peuvent être à  *)
          (* des adresses différentes. NB: on pourrait rassembler les lieux et *)
          (* les sources dans un seul index pour de meilleures performances.   *)
          let list = UpdateData.combine list in
          let string_set1 = ref StringSetAutoComplete.empty in
          let () =
            List.iter
              (fun (src, _) ->
                 begin
                   string_set1 := StringSetAutoComplete.add src !string_set1;
                 end)
              list
          in
          let list = StringSetAutoComplete.elements !string_set1 in
          (* Fonction qui à une liste de données retourne la *)
          (* liste de toutes les données commençant par ini. *)
          let nb_res = ref 0 in
          let string_set = ref StrSetAutoComplete.empty in
          let ini = Mutil.tr '_' ' ' n in
          let rec reduce l accu =
            match l with
            | [] -> ()
            | data :: l ->
                begin
                  let k =  Mutil.tr '_' ' ' data in
                  if string_start_with (Name.lower ini) (Name.lower k) then
                    begin
                      string_set := StrSetAutoComplete.add data !string_set;
                      incr nb_res;
                    end;
                  if !nb_res > max_res then ()
                  else reduce l accu
                end
          in
          let () = reduce list [] in
          List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)
        end
      else
        begin
          let _ = load_strings_array base in
          if Name.lower n = "" then []
          else select_start_with_auto_complete base mode max_res n
        end
    end


module IperSetLinkPerson =
  Set.Make
    (struct
      type t = iper
      let compare ip1 ip2 = compare (Adef.int_of_iper ip1) (Adef.int_of_iper ip2)
     end)

(*
let select_both_link_person conf base ini_n ini_p =
  let find_sn p x = kmp x (sou base (get_surname p)) in
  let find_fn p x = kmp x (sou base (get_first_name p)) in
  let find_str s x = kmp x s in
  let ini_n = Util.name_key base ini_n in
  let ini_n = code_varenv ini_n in
  let ini_n =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_n []
  in
  let ini_n = List.filter (fun s -> s <> "") ini_n in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = code_varenv ini_p in
  let ini_p =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_p []
  in
  let add_maiden p ini_p l =
    let rec loop ifam_l l =
      match ifam_l with
      | [] -> ()
      | ifam :: q ->
          let fam = foi base ifam in
          let ip = Gutil.spouse (get_key_index p) fam in
          let sp = poi base ip in
          if List.for_all (fun s -> find_fn sp s) ini_p then l := ip :: !l;
          loop q l
    in
    if get_sex p = Male then
      loop (Array.to_list (get_family p)) l
  in
  let add_maiden2 p ini_n ini_p l =
    (* On sépare les noms avec tirets ... *)
    let ini_n =
      List.fold_left
        (fun accu s -> (explode s '-') @ accu)
        [] ini_n
    in
    let rec loop ifam_l l =
      match ifam_l with
      | [] -> ()
      | ifam :: q ->
          let fam = foi base ifam in
          let ip = Gutil.spouse (get_key_index p) fam in
          let sp = poi base ip in
          let names =
            sou base (get_surname p) ^ " " ^ sou base (get_surname sp)
          in
          if List.for_all (fun s -> find_str names s) ini_n then
            add_maiden p ini_p l;
          loop q l
    in
    if get_sex p = Male then
      if List.exists (fun s -> find_sn p s) ini_n then
        loop (Array.to_list (get_family p)) l
  in
  let list = ref [] in
  for i = 0 to nb_of_persons base - 1 do
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    if List.for_all (fun s -> find_sn p s) ini_n then
      begin
        if List.for_all (fun s -> find_fn p s) ini_p then
          list := ip :: !list
        else if maiden_name then add_maiden p ini_p list
      end
    else
      (* On cherche une partie du nom de jeune fille dans les noms donnés. *)
      if maiden_name then add_maiden2 p ini_n ini_p list
  done;
  !list
*)


let select_both_link_person base ini_n ini_p max_res =
  let find_sn p x = kmp x (sou base (get_surname p)) in
  let find_fn p x = kmp x (sou base (get_first_name p)) in
  let ini_n = Util.name_key base ini_n in
  let ini_n = code_varenv ini_n in
  let ini_n =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_n []
  in
  let ini_n = List.filter (fun s -> s <> "") ini_n in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = code_varenv ini_p in
  let ini_p =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_p []
  in
  let rec loop i list nb_res =
    if nb_res < max_res && i < nb_of_persons base then
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      if List.for_all (fun s -> find_sn p s) ini_n then
        if List.for_all (fun s -> find_fn p s) ini_p then
          loop (i + 1) (Adef.iper_of_int i :: list) (nb_res + 1)
        else loop (i + 1) list nb_res
      else loop (i + 1) list nb_res
    else list
  in
  loop 0 [] 0

let select_link_person base get_field max_res ini =
  let find p x = kmp x (sou base (get_field p)) in
  let ini = code_varenv ini in
  let ini =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini []
  in
  let rec loop i list nb_res =
    if nb_res < max_res && i < nb_of_persons base then
      if List.for_all (fun s -> find (poi base (Adef.iper_of_int i)) s) ini
      then loop (i + 1) (Adef.iper_of_int i :: list) (nb_res + 1)
      else loop (i + 1) list nb_res
    else list
  in
  loop 0 [] 0

let search_person_list base surname first_name =
  let _ = load_strings_array base in
  let (surname, first_name) =
    match (surname, first_name) with
    | (Some n, Some fn) ->
        ((if Name.lower n = "" then None else Some n),
         (if Name.lower fn = "" then None else Some fn))
    | (Some n, None) -> ((if n = "" then None else Some n), None)
    | (None, Some fn) -> (None, (if fn = "" then None else Some fn))
    | (None, None) -> (None, None)
  in
  match (surname, first_name) with
  | (Some n, Some fn) ->
      select_both_start_with_person base n fn
  | (Some n, None) ->
      select_start_with_person base get_surname n
  | (None, Some fn) ->
      select_start_with_person base get_first_name fn
  | (None, None) -> []















(* ******************** SAVE ********************** *)
(*
let select_start_with_auto_complete conf base mode get_field max_res ini =
  let need_whole_list = true in
(*
  let ini =
    if is_surnames then Util.name_key base ini
    else ini
  in
*)
  let name =
    match mode with
    | `lastname -> persons_of_surname base
    | `firstname -> persons_of_first_name base
  in
  let string_set = ref StrSetAutoComplete.empty in
  (* Si la base est grosse, on fait un vrai start_with. *)
  let () =
    if nb_of_persons base > 100000 then
    else
      begin
          let () =
        (*    let start_k = Mutil.tr '_' ' ' ini in*)
            (* On commence à ? comme ça on fait MAJ et MIN. *)
            let start_k = Mutil.tr '_' ' ' "?" in
            let letter = String.uppercase_ascii (String.sub start_k 0 1) in
            match
              try Some (spi_first name letter) with
               Not_found -> None
            with
             | Some istr ->
                let rec loop istr list =
                  if StrSetAutoComplete.cardinal !string_set < max_res then
                    begin
                      let s = sou base istr in
                      let k = Util.name_key base s in
        (*              if (String.sub k 0 1) = letter then*)
                        if string_incl_start_with (Name.lower ini) (Name.lower k) then
                          begin
                            string_set := StrSetAutoComplete.add (sou base istr) !string_set;
        (*
                          let () =
                            if s <> "?" then
                              let my_list = spi_find name istr in
                              let () =
                                List.iter
                                  (fun ip ->
                                    if is_patched_person base ip then
                                      let p = poi base ip in
                                      let isn = get_field p in
                                      if eq_istr isn istr then
                                        string_set := StrSetAutoComplete.add (sou base isn) !string_set
                                      else ()
                                    else string_set := StrSetAutoComplete.add (sou base istr) !string_set)
                                  my_list
                              in
                              (*
                              let my_list =
                                if conf.use_restrict then
                                  List.fold_left
                                    (fun l ip ->
                                      if is_restricted conf base ip then l
                                      else (ip :: l) )
                                    [] my_list
                                else my_list
                              in
                              List.rev_append my_list list
                              *)
                              ()
                            else ()
                          in
        *)
                          match
                            try Some (spi_next name istr need_whole_list) with
                             Not_found -> None
                          with
                           | Some (istr, dlen) -> loop istr list
                           | None -> ()
                          end
                        else
                          match
                            try Some (spi_next name istr need_whole_list) with
                             Not_found -> None
                          with
                           | Some (istr, dlen) -> loop istr list
                           | None -> ()
        (*              else ()*)
                    end
                in loop istr []
             | None -> ()
          in
        (*
          let () =
            let start_k = Mutil.tr '_' ' ' ini in
            let letter = String.lowercase_ascii (String.sub start_k 0 1) in
            match
              try Some (spi_first name letter) with
               Not_found -> None
            with
             | Some istr ->
                let rec loop istr list =
                  if StrSetAutoComplete.cardinal !string_set < max_res then
                    begin
                      let s = sou base istr in
                      let k = Util.name_key base s in
        (*              if (String.sub k 0 1) = letter then*)
                        if string_incl_start_with (Name.lower ini) (Name.lower k) then
                          let () =
                            if s <> "?" then
                              let my_list = spi_find name istr in
                              let () =
                                List.iter
                                  (fun ip ->
                                    if is_patched_person base ip then
                                      let p = poi base ip in
                                      let isn = get_field p in
                                      if eq_istr isn istr then
                                        string_set := StrSetAutoComplete.add (sou base isn) !string_set
                                      else ()
                                    else string_set := StrSetAutoComplete.add (sou base istr) !string_set)
                                  my_list
                              in
                              (*
                              let my_list =
                                if conf.use_restrict then
                                  List.fold_left
                                    (fun l ip ->
                                      if is_restricted conf base ip then l
                                      else (ip :: l) )
                                    [] my_list
                                else my_list
                              in
                              List.rev_append my_list list
                              *)
                              ()
                            else ()
                          in
                          match
                            try Some (spi_next name istr need_whole_list) with
                              Not_found -> None
                          with
                           | Some (istr, dlen) -> loop istr list
                           | None -> ()
                        else
                          match
                            try Some (spi_next name istr need_whole_list) with
                              Not_found -> None
                          with
                           | Some (istr, dlen) -> loop istr list
                           | None -> ()
        (*              else ()*)
                    end
                in loop istr []
             | None -> ()
          in
        *)
      end
  in
  List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)
*)

#endif
