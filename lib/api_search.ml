#ifdef API

module M = Api_piqi
module Mext = Api_piqi_ext

open Config
open Gwdb
open Def
open Util
open Api_def
open Api_util

module StrSet = Mutil.StrSet

let string_start_with ini s = Mutil.start_with ~wildcard:true ini 0 s

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
    if get_sex p = Male then
      l :=
        Array.fold_left
          (fun acc ifam ->
             let fam = foi base ifam in
             let ip = Gutil.spouse (get_iper p) fam in
             let sp = poi base ip in
             if List.for_all (fun s -> find_fn sp s) ini_p then ip :: acc else acc)
          !l (get_family p)
  in
  let add_maiden2 p ini_n ini_p l =
    (* On sépare les noms avec tirets ... *)
    let ini_n =
      List.fold_left
        (fun accu s -> List.rev_append (String.split_on_char '-' s) accu)
        [] ini_n
    in
    if get_sex p = Male && List.exists (fun s -> find_sn p s) ini_n
    then
      Array.iter
        (fun ifam ->
           let fam = foi base ifam in
           let ip = Gutil.spouse (get_iper p) fam in
           let sp = poi base ip in
           let names =
             sou base (get_surname p) ^ " " ^ sou base (get_surname sp)
           in
           if List.for_all (fun s -> find_str names s) ini_n
           then add_maiden p ini_p l)
        (get_family p)
  in
  let list = ref [] in
  Gwdb.Collection.iter begin fun p ->
    let ip = get_iper p in
    if List.for_all (fun s -> find_sn p s) ini_n then
      begin
        if List.for_all (fun s -> find_fn p s) ini_p then
          list := ip :: !list
        else if maiden_name then add_maiden p ini_p list
      end
    else
      (* On cherche une partie du nom de jeune fille dans les noms donnés. *)
      if maiden_name then add_maiden2 p ini_n ini_p list
  end (Gwdb.persons base) ;
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
  Gwdb.Collection.iter begin fun p ->
    if List.for_all (fun s -> find p s) ini
    then list := get_iper p :: !list
  end (Gwdb.persons base) ;
  !list


module Iper =
  struct
    type t = Gwdb.person
    let compare i1 i2 = Stdlib.compare (get_iper i1) (get_iper i2)
  end

module IperSet = Set.Make(Iper)

let print_list conf base filters list =
  let person_l =
    IperSet.elements
      (List.fold_left
         begin fun acc p ->
           let p = poi base p in
           if apply_filters_p conf filters Perso.get_sosa_person p
           then IperSet.add p acc
           else acc
         end
         IperSet.empty list)
  in
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
  let data = conv_data_list_person conf base filters person_l in
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
  Gwdb.Collection.fold begin fun list p ->
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
      if start_surname && start_firstname then (get_iper p :: list)
      else list
  end [] (Gwdb.persons base)

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
  Gwdb.Collection.fold begin fun list p ->
      let names =
        cut_at_space (code_varenv (Name.lower (sou base (get_field p)))) []
      in
      let start_name =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) names)
          ini
      in
      if start_name then (get_iper p :: list)
      else list
  end [] (Gwdb.persons base)

let select_start_with_auto_complete base mode max_res ini =
  let need_whole_list = true in
  let name =
    match mode with
    | `lastname -> persons_of_surname base
    | `firstname -> persons_of_first_name base
    | `place -> failwith "cannot use select_start_with_auto_complete"
    | `source -> failwith "cannot use select_start_with_auto_complete"
  in
  let string_set = ref StrSet.empty in
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
                string_set := StrSet.add s !string_set;
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
                  string_set := StrSet.add s !string_set;
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
                string_set := StrSet.add (sou base istr) !string_set;
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
  List.sort Gutil.alphabetic_order (StrSet.elements !string_set)


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
  let string_set = ref StrSet.empty in
  let nb_res = ref 0 in
  Gwdb.Collection.fold_until (fun () -> !nb_res < max_res) begin fun () p ->
      if List.for_all (fun s -> find p s) ini
      then
        begin
        string_set := StrSet.add (sou base (get_field p)) !string_set;
        incr nb_res;
        end
  end () (Gwdb.persons base) ;
  List.sort Gutil.alphabetic_order (StrSet.elements !string_set)


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

(** [ini] must be in the form of [Name.lower @@ Mutil.tr '_' ' ' ini]
    Assume that [list] is already sorted.
*)
let complete_with_dico conf nb max mode ini list =
  let reduce_dico mode ignored format list =
    let rec loop acc = function
      | [] -> acc
      | hd :: tl ->
        let acc =
          let k =  Mutil.tr '_' ' ' hd in
          let k = if mode <> `subdivision then UpdateData.remove_suburb k else k in
          if string_start_with ini (Name.lower k) then begin
            let hd =
              if format <> []
              then
                let expl_hd = String.split_on_char ',' hd in
                String.concat ", " @@
                Util.filter_map begin function
                  | `town -> List.nth_opt expl_hd 0
                  | `area_code -> List.nth_opt expl_hd 1
                  | `county -> List.nth_opt expl_hd 2
                  | `region -> List.nth_opt expl_hd 3
                  | `country -> List.nth_opt expl_hd 4
                  | _ -> None
                end
                  format
              else
                hd
            in
            if List.mem hd ignored then acc
            else begin incr nb ; hd :: acc end
          end
          else acc
        in
        if !nb < max then loop acc tl else acc
    in loop [] list
  in
  match mode with
  | Some mode when !nb < max ->
    let format =
      match p_getenv conf.base_env "places_format" with
      | None -> []
      | Some s ->
        List.map begin function
          | "Subdivision" -> `subdivision
          | "Town" -> `town
          | "Area code" -> `area_code
          | "County" -> `county
          | "Region" -> `region
          | "Country" -> `country
          | _ -> raise Not_found
        end
          (String.split_on_char ',' s)
    in
    let dico_place = reduce_dico mode list format (load_dico_lieu conf mode) in
    List.append list (List.sort Gutil.alphabetic_order dico_place)
  | _ -> list

let search_auto_complete conf base mode place_mode max n =
  let aux data =
    let conf = { conf with env = ("data", data) :: conf.env } in
    UpdateData.get_all_data conf base
    |> List.rev_map (sou base)
    |> List.sort Gutil.alphabetic_order
  in
  match mode with

  | `place ->
    let list = aux "place" in
    let nb = ref 0 in
    let ini = Name.lower @@ Mutil.tr '_' ' ' n in
    let reduce_perso list =
      let rec loop acc = function
        | [] -> List.rev acc
        | hd :: tl ->
          let hd' =
            if place_mode <> Some `subdivision
            then UpdateData.remove_suburb hd
            else hd
          in
          let acc =
            if Mutil.start_with ~wildcard:true ini 0 @@ Name.lower @@ Mutil.tr '_' ' ' hd'
            then (incr nb ; hd :: acc)
            else acc
          in
          if !nb < max then loop acc tl else acc
      in
      loop [] list
    in
    complete_with_dico conf nb max place_mode ini (reduce_perso list)

  | `source ->
    let list = aux "src" in
    let nb = ref 0 in
    let ini = Name.lower @@ Mutil.tr '_' ' ' n in
    let rec reduce acc = function
      | [] -> List.rev acc
      | hd :: tl ->
        let k =  Mutil.tr '_' ' ' hd in
        let acc =
          if string_start_with ini (Name.lower k)
            then (incr nb ; hd :: acc)
            else acc
        in
        if !nb < max then reduce acc tl
        else acc
    in
    reduce [] list

  | _ ->
    if Name.lower n = "" then []
    else ( load_strings_array base
         ; select_start_with_auto_complete base mode max n )

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
  fst @@ Gwdb.Collection.fold_until (fun (_, n) -> n < max_res) begin fun (list, n) p ->
    if List.for_all (fun s -> find_sn p s) ini_n then
      if List.for_all (fun s -> find_fn p s) ini_p then
        (get_iper p :: list, n + 1)
      else (list, n)
    else (list, n)
  end ([], 0) (Gwdb.persons base)

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
  fst @@ Gwdb.Collection.fold_until (fun (_, n) -> n < max_res) begin fun (list, n) p ->
      if List.for_all (fun s -> find p s) ini
      then (get_iper p :: list, n + 1)
      else (list, n)
  end ([], 0) (Gwdb.persons base)

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

#endif
