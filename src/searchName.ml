(* camlp5r ./pa_html.cmo *)
(* $Id: searchName.ml,v 1.00 2014-07-08 11:26:10 flh Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

(* tools *)

(*
value gen_person_is_std_key misc conf base p k =
  let k = Name.strip_lower k in
  if k = Name.strip_lower (p_first_name base p ^ " " ^ p_surname base p) then
    True
  else if
    misc &&
    List.exists (fun n -> Name.strip n = k)
      (person_misc_names base p (nobtit conf base))
  then
    True
  else False
;

value person_is_std_key_approx = gen_person_is_std_key True;
value person_is_std_key_exact = gen_person_is_std_key False;

value select_std_eq conf base pl k =
  List.fold_right
    (fun p pl ->
       if person_is_std_key_exact conf base p k then [p :: pl] else pl)
    pl []
;

value select_std_approx conf base pl k =
  List.fold_right
    (fun p pl ->
       if person_is_std_key_approx conf base p k then [p :: pl] else pl)
    pl []
;
*)

value empty_surname_or_firsntame base p =
  is_empty_string (get_surname p) || is_quest_string (get_surname p) ||
  is_empty_string (get_first_name p) || is_quest_string (get_first_name p) ||
  Name.lower (sou base (get_surname p)) = "" ||
  Name.lower (sou base (get_first_name p)) = ""
;

value person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  if
    List.exists (fun n -> Name.strip n = k)
      (person_misc_names base p (nobtit conf base))
  then True
  else False
;

value select_misc_name conf base pl k =
  List.fold_right
    (fun p pl ->
       if person_is_misc_name conf base p k then [p :: pl] else pl)
    pl []
;

value person_is_approx_key conf base p k =
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (p_first_name base p) in
  let sn = Name.strip_lower (p_surname base p) in
  if k = fn ^ sn && fn <> "" && sn <> "" then True
  else False
;

value select_approx_key conf base pl k =
  List.fold_right
    (fun p pl ->
       if person_is_approx_key conf base p k then [p :: pl]
       else if person_is_misc_name conf base p k then [p :: pl]
       else pl)
    pl []
;

value cut_words str =
  loop 0 0 where rec loop beg i =
    if i < String.length str then
      match str.[i] with
      [ ' ' ->
          if beg = i then loop (succ beg) (succ i)
          else [String.sub str beg (i - beg) :: loop (succ i) (succ i)]
      | _ -> loop beg (succ i) ]
    else if beg = i then []
    else [String.sub str beg (i - beg)]
;

value try_find_with_one_first_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match Mutil.lindex n1 ' ' with
  [ Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let (list, _) =
        Some.persons_of_fsname conf base base_strings_of_surname
          (spi_find (persons_of_surname base)) get_surname sn
      in
      let pl =
        List.fold_left
          (fun pl (_, _, ipl) ->
             List.fold_left
               (fun pl ip ->
                  let p = pget conf base ip in
                  let fn1 =
                    Name.abbrev (Name.lower (sou base (get_first_name p)))
                  in
                  if List.mem fn (cut_words fn1) then [p :: pl] else pl)
               pl ipl)
          [] list
      in
      pl
  | None -> [] ]
;

value compact_list conf base xl =
  let pl = sort_person_list base xl in
  let pl =
    List.fold_right
      (fun p pl ->
         match pl with
         [ [p1 :: _] when get_key_index p = get_key_index p1 -> pl
         | _ -> [p :: pl] ])
      pl []
  in
  pl
;

value name_with_roman_number str =
  loop False 0 0 where rec loop found len i =
    if i = String.length str then if found then Some (Buff.get len) else None
    else
      match str.[i] with
      [ '0'..'9' as c ->
          let (n, i) =
            loop (Char.code c - Char.code '0') (i + 1) where rec loop n i =
              if i = String.length str then (n, i)
              else
                match str.[i] with
                [ '0'..'9' as c ->
                    loop (10 * n + Char.code c - Char.code '0') (i + 1)
                | _ -> (n, i) ]
          in
          loop True (Buff.mstore len (Mutil.roman_of_arabian n)) i
      | c -> loop found (Buff.store len c) (i + 1) ]
;

(* search functions *)

value search_by_sosa conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Num.of_string an) with [ Failure _ -> None ] in
  match (sosa_ref, sosa_nb) with
  [ (Some p, Some n) ->
      if n <> Num.zero then
        match Util.branch_of_sosa conf base (get_key_index p) n with
        [ Some [(ip, _) :: _] -> [pget conf base ip]
        | _ -> [] ]
      else []
  | _ -> [] ]
;

(*
value gen_search_approx_key std_key conf base an =
  let ipl = person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
      [ Some an1 ->
          let ipl = person_ht_find_all base an1 in
          if ipl = [] then (an, []) else (an1, ipl)
      | None -> (an, ipl) ]
    else (an, ipl)
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in
         if is_hidden p then l else [p :: l])
    [] ipl
  in
  let spl =
    if std_key then select_std_approx conf base pl an
    else select_std_eq conf base pl an
  in
  let pl =
    if std_key then
      if spl = [] then
        if pl = [] then try_find_with_one_first_name conf base an else pl
      else spl
    else spl
  in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.fast_auth_age conf base p
           then [p :: pl]
           else pl)
        pl []
    else pl
  in
  compact_list conf base pl
;
*)

value search_partial_key conf base an =
  let ipl = person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
      [ Some an1 ->
          let ipl = person_ht_find_all base an1 in
          if ipl = [] then (an, []) else (an1, ipl)
      | None -> (an, ipl) ]
    else (an, ipl)
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in
         if is_hidden p then l else [p :: l])
    [] ipl
  in
  let pl =
    if pl = [] then try_find_with_one_first_name conf base an else pl
  in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.fast_auth_age conf base p
           then [p :: pl]
           else pl)
        pl []
    else pl
  in
  compact_list conf base pl
;

value search_approx_key conf base an =
  let ipl = person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
      [ Some an1 ->
          let ipl = person_ht_find_all base an1 in
          if ipl = [] then (an, []) else (an1, ipl)
      | None -> (an, ipl) ]
    else (an, ipl)
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in
         if is_hidden p then l else [p :: l])
    [] ipl
  in
  let pl = select_approx_key conf base pl an in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.fast_auth_age conf base p
           then [p :: pl]
           else pl)
        pl []
    else pl
  in
  let pl =
    List.fold_right
      (fun p pl ->
         if empty_surname_or_firsntame base p then pl
         else [p :: pl])
      pl []
  in
  compact_list conf base pl
;

(* recherche par clé, i.e. prenom.occ nom *)
value search_by_key conf base an =
  match person_of_string_key base an with
  [ Some ip ->
      let pl =
        let p = pget conf base ip in
        if is_hidden p then [] else [p]
      in
      if not conf.wizard && not conf.friend then
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || Util.fast_auth_age conf base p
             then [p :: pl]
             else pl)
          pl []
      else pl
  | None -> [] ]
;

value search_approx_surname conf base an =
  []
;

value search_approx_first_name conf base an =
  []
;

(* main *)

type search_type =
  [ Sosa | Key | Surname | FirstName | ApproxKey |
    ApproxSurname | ApproxFirstName | PartialKey |
    DefaultSurname ]
;

value search conf base an search_order specify unknown =
  loop search_order where rec loop l =
    match l with
    [ [] -> unknown conf an
    | [Sosa :: l] ->
        let pl = search_by_sosa conf base an in
        match pl with
        [ [p] -> do {
            record_visited conf (get_key_index p);
            Perso.print conf base p }
        | _ -> loop l ]
    | [Key :: l] ->
        let pl = search_by_key conf base an in
        match pl with
        [ [] ->  loop l
        | [p] -> do {
            record_visited conf (get_key_index p);
            Perso.print conf base p }
        | pl -> specify conf base an pl ]
    | [Surname :: l] ->
        let pl = Some.search_surname conf base an in
        match pl with
        [ [] -> loop l
        | pl ->
            do {
              conf.cancel_links := False;
              Some.search_surname_print conf base unknown an
            }]
    | [FirstName :: l] ->
        let pl = Some.search_first_name conf base an in
        match pl with
        [ [] -> loop l
        | pl ->
            do {
              conf.cancel_links := False;
              Some.search_first_name_print conf base an
            }]
    | [ApproxKey :: l] ->
        let pl = search_approx_key conf base an in
        match pl with
        [ [] -> loop l
        | [p] -> do {
            record_visited conf (get_key_index p);
            Perso.print conf base p }
        | pl -> specify conf base an pl ]
    | [ApproxSurname :: l] ->
        let pl = search_approx_surname conf base an in
        match pl with
        [ [] -> loop l
        | pl -> specify conf base an pl ]
    | [ApproxFirstName :: l] ->
        let pl = search_approx_first_name conf base an in
        match pl with
        [ [] -> loop l
        | pl -> specify conf base an pl ]
    | [PartialKey :: l] ->
        let pl = search_partial_key conf base an in
        match pl with
        [ [] -> loop l
        | [p] -> do {
            record_visited conf (get_key_index p);
            Perso.print conf base p }
        | pl -> specify conf base an pl ]
    | [DefaultSurname :: l] ->
        do {
          conf.cancel_links := False;
          Some.search_surname_print conf base unknown an
        }]
;


(* ************************************************************************ *)
(*  [Fonc] print : conf -> string -> unit                                   *)
(** [Description] : Recherche qui n'utilise que 2 inputs. On essai donc de
      trouver la meilleure combinaison de résultat pour afficher la réponse
      la plus probable.
    [Args] :
      - conf : configuration de la base
      - base : base
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
value print conf base specify unknown =
  let real_input label =
    match p_getenv conf.env label with
    [ Some s -> if s = "" then None else Some s
    | None -> None ]
  in
  match (real_input "p", real_input "n") with
  [ (Some fn, Some sn) ->
      let order = [ Key; ApproxKey; PartialKey ] in
      search conf base (fn ^ " " ^ sn) order specify unknown
  | (Some fn, None) ->
      let order = [ FirstName ] in
      search conf base fn order specify unknown
  | (None, Some sn) ->
      let order =
        [ Sosa; Key; Surname; ApproxKey; PartialKey; DefaultSurname ]
      in
      search conf base sn order specify unknown
  | (None, None) -> incorrect_request conf ]
;
