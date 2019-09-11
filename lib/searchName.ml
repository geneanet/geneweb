(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let empty_surname_or_firsntame base p =
  is_empty_string (get_surname p) || is_quest_string (get_surname p) ||
  is_empty_string (get_first_name p) || is_quest_string (get_first_name p) ||
  Name.lower (sou base (get_surname p)) = "" ||
  Name.lower (sou base (get_first_name p)) = ""

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  if List.exists (fun n -> Name.strip n = k)
       (person_misc_names base p (nobtit conf base))
  then
    true
  else false

let person_is_approx_key base p k =
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (p_first_name base p) in
  let sn = Name.strip_lower (p_surname base p) in
  if k = fn ^ sn && fn <> "" && sn <> "" then true else false

let select_approx_key conf base pl k =
  List.fold_right
    (fun p pl ->
       if person_is_approx_key base p k then p :: pl
       else if person_is_misc_name conf base p k then p :: pl
       else pl)
    pl []

let cut_words str =
  let rec loop beg i =
    if i < String.length str then
      match str.[i] with
        ' ' ->
          if beg = i then loop (succ beg) (succ i)
          else String.sub str beg (i - beg) :: loop (succ i) (succ i)
      | _ -> loop beg (succ i)
    else if beg = i then []
    else [String.sub str beg (i - beg)]
  in
  loop 0 0

let try_find_with_one_first_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index_opt n1 ' ' with
    Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let (list, _) =
        Some.persons_of_fsname base base_strings_of_surname
          (spi_find (persons_of_surname base)) sn
      in
      List.fold_left
        (fun pl (_, _, ipl) ->
           List.fold_left
             (fun pl ip ->
                let p = pget conf base ip in
                let fn1 =
                  Name.abbrev (Name.lower (sou base (get_first_name p)))
                in
                if List.mem fn (cut_words fn1) then p :: pl else pl)
             pl ipl)
        [] list
  | None -> []

let compact_list base xl =
  let pl = Gutil.sort_person_list base xl in
  List.fold_right
    (fun p pl ->
       match pl with
         p1 :: _ when get_iper p = get_iper p1 -> pl
       | _ -> p :: pl)
    pl []

let name_with_roman_number str =
  let rec loop found len i =
    if i = String.length str then if found then Some (Buff.get len) else None
    else
      match str.[i] with
        '0'..'9' as c ->
          let (n, i) =
            let rec loop n i =
              if i = String.length str then n, i
              else
                match str.[i] with
                  '0'..'9' as c ->
                    loop (10 * n + Char.code c - Char.code '0') (i + 1)
                | _ -> n, i
            in
            loop (Char.code c - Char.code '0') (i + 1)
          in
          loop true (Buff.mstore len (Mutil.roman_of_arabian n)) i
      | c -> loop found (Buff.store len c) (i + 1)
  in
  loop false 0 0

(* search functions *)

let search_by_sosa conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match sosa_ref, sosa_nb with
    Some p, Some n ->
      if n <> Sosa.zero then
        match Util.branch_of_sosa conf base n (pget conf base @@ get_iper p) with
          Some (p :: _) -> [p]
        | _ -> []
      else []
  | _ -> []

let search_partial_key conf base an =
  let ipl = Gutil.person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
        Some an1 ->
          let ipl = Gutil.person_ht_find_all base an1 in
          if ipl = [] then an, [] else an1, ipl
      | None -> an, ipl
    else an, ipl
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in if is_hidden p then l else p :: l)
      [] ipl
  in
  let pl =
    if pl = [] then try_find_with_one_first_name conf base an else pl
  in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.authorized_age conf base p
           then
             p :: pl
           else pl)
        pl []
    else pl
  in
  compact_list base pl

let search_approx_key conf base an =
  let ipl = Gutil.person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
        Some an1 ->
          let ipl = Gutil.person_ht_find_all base an1 in
          if ipl = [] then an, [] else an1, ipl
      | None -> an, ipl
    else an, ipl
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in if is_hidden p then l else p :: l)
      [] ipl
  in
  let pl = select_approx_key conf base pl an in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.authorized_age conf base p
           then
             p :: pl
           else pl)
        pl []
    else pl
  in
  let pl =
    List.fold_right
      (fun p pl -> if empty_surname_or_firsntame base p then pl else p :: pl)
      pl []
  in
  compact_list base pl

(* recherche par clé, i.e. prenom.occ nom *)
let search_by_key conf base an =
  match Gutil.person_of_string_key base an with
    Some ip ->
      let pl = let p = pget conf base ip in if is_hidden p then [] else [p] in
      if not conf.wizard && not conf.friend then
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || Util.authorized_age conf base p
             then
               p :: pl
             else pl)
          pl []
      else pl
  | None -> []

(* main *)

type search_type =
  | Sosa
  | Key
  | Surname
  | FirstName
  | ApproxKey
  | PartialKey

let search conf base specify unknown one surname firstname fn sn search_order =
  let an = if fn <> "" then if sn <> "" then fn ^ " " ^ sn else fn else sn in
  let rec loop l =
    match l with
    | [] -> unknown conf an
    | Sosa :: l ->
      let pl = search_by_sosa conf base an in
      begin match pl with
        | [p] -> one conf base p
        | _ -> loop l
      end
    | Key :: l ->
      let pl = search_by_key conf base an in
      begin match pl with
        | [] -> loop l
        | [p] -> one conf base p
        | pl -> specify conf base an pl
      end
    | Surname :: l ->
      begin match Some.search_surnames base sn with
        | [_, (_, iperl)], _ as list when iperl <> [] ->
          surname conf base unknown an list
        | _ -> loop l
      end
    | FirstName :: l ->
      begin match Some.search_first_names base fn with
        | [] -> loop l
        | list when sn = "" -> firstname conf base an list
        | list ->
          begin match
              let iperl = Some.ipers @@ fst @@ Some.search_surnames base sn in
              List.filter (function (_, (_, [])) -> false | _ -> true) @@
              List.map
                begin fun (s, (i, ips)) ->
                  (s, (i, List.filter (fun i -> List.mem i iperl) ips))
                end
                list
            with
            | [] -> loop l
            | [ (_, (_, [p])) ] -> one conf base (Gwdb.poi base p)
            | list -> firstname conf base an list
          end
      end
    | ApproxKey :: l ->
      let pl = search_approx_key conf base an in
      begin match pl with
        | [] -> loop l
        | [p] -> one conf base p
        | pl -> specify conf base an pl
      end
    | PartialKey :: l ->
      let pl = search_partial_key conf base an in
      begin match pl with
        | [] -> loop l
        | [p] -> one conf base p
        | pl -> specify conf base an pl
      end
  in
  loop search_order

let print conf base specify unknown ~fn ~sn =
  let aux =
    search conf base specify unknown
      (fun conf base p -> record_visited conf (get_iper p) ; Perso.print conf base p)
      SomeDisplay.print_surname
      SomeDisplay.print_first_name
  in
  let real_input label =
    match p_getenv conf.env label with
    | Some s -> if s = "" then None else Some s
    | None -> None
  in
  match real_input fn, real_input sn with
  | Some fn, Some sn -> aux fn sn [ Key ; FirstName ; ApproxKey ; PartialKey ]
  | Some fn, None -> aux fn "" [ FirstName ]
  | None, Some sn -> aux "" sn [ Sosa ; Key ; Surname ; ApproxKey ; PartialKey ]
  | None, None -> Hutil.incorrect_request conf
