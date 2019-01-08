(* $Id: title.ml,v 5.30 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

type date_search = JustSelf | AddSpouse | AddChildren

module StrSet = Mutil.StrSet

let infinity = 10000

let date_interval conf base t x =
  let d1 =
    ref {day = 0; month = 0; year = infinity; prec = Sure; delta = 0}
  in
  let d2 = ref {day = 0; month = 0; year = 0; prec = Sure; delta = 0} in
  let found = ref false in
  let rec loop t x =
    let set d =
      if CheckItem.strictly_before_dmy d !d1 then d1 := d;
      if CheckItem.strictly_after_dmy d !d2 then d2 := d;
      found := true
    in
    begin match Adef.od_of_cdate (get_birth x) with
      Some (Dgreg (d, _)) -> set d
    | _ -> ()
    end;
    begin match Adef.od_of_cdate (get_baptism x) with
      Some (Dgreg (d, _)) -> set d
    | _ -> ()
    end;
    begin match CheckItem.date_of_death (get_death x) with
      Some (Dgreg (d, _)) -> set d
    | _ -> if get_death x = NotDead then set conf.today
    end;
    List.iter
      (fun t ->
         begin match Adef.od_of_cdate t.t_date_start with
           Some (Dgreg (d, _)) -> set d
         | _ -> ()
         end;
         match Adef.od_of_cdate t.t_date_end with
           Some (Dgreg (d, _)) -> set d
         | _ -> ())
      (Util.nobtit conf base x);
    match t with
      JustSelf -> ()
    | _ ->
        let u = pget conf base (get_key_index x) in
        Array.iter
          (fun ifam ->
             let fam = foi base ifam in
             let md = get_marriage fam in
             let conj = Gutil.spouse (get_key_index x) fam in
             begin match Adef.od_of_cdate md with
               Some (Dgreg (d, _)) -> set d
             | _ -> ()
             end;
             loop JustSelf (pget conf base conj);
             match t with
               AddChildren ->
                 Array.iter (fun e -> loop JustSelf (pget conf base e))
                   (get_children fam)
             | _ -> ())
          (get_family u)
  in
  loop t x; if !found then Some (!d1, !d2) else None

let compare_title_dates conf base (x1, t1) (x2, t2) =
  match
    (get_birth x1, Adef.od_of_cdate t1.t_date_start,
     Adef.od_of_cdate t1.t_date_end, get_death x1),
    (get_birth x2, Adef.od_of_cdate t2.t_date_start,
     Adef.od_of_cdate t2.t_date_end, get_death x2)
  with
    (_, Some (Dgreg (d1, _)), _, _), (_, Some (Dgreg (d2, _)), _, _) ->
      if CheckItem.strictly_before_dmy d1 d2 then -1
      else if d1.year = d2.year then
        match
          Adef.od_of_cdate t1.t_date_end, Adef.od_of_cdate t2.t_date_end
        with
          Some d1, Some d2 ->
            if not (CheckItem.strictly_after d1 d2) then -1 else 1
        | _ -> -1
      else 1
  | (_, _, Some (Dgreg (_, _) as d1), _),
    (_, _, Some (Dgreg (_, _) as d2), _) ->
      if not (CheckItem.strictly_before d2 d1) then -1 else 1
  | (_, _, _, Death (_, d1)), (_, Some d2, _, _)
    when not (CheckItem.strictly_before d2 (Adef.date_of_cdate d1)) ->
      -1
  | (_, Some (Dgreg (_, _) as d1), _, _), (_, _, _, Death (_, d2))
    when not (CheckItem.strictly_before d1 (Adef.date_of_cdate d2)) ->
      1
  | _ ->
      match
        date_interval conf base JustSelf x1,
        date_interval conf base JustSelf x2
      with
        Some (d11, d12), Some (d21, d22) ->
          if not (CheckItem.strictly_before_dmy d21 d12) then -1
          else if not (CheckItem.strictly_before_dmy d11 d22) then 1
          else if CheckItem.strictly_after_dmy d21 d11 then -1
          else 1
      | _ ->
          match
            date_interval conf base AddSpouse x1,
            date_interval conf base AddSpouse x2
          with
            Some (d11, d12), Some (d21, d22) ->
              if not (CheckItem.strictly_before_dmy d21 d12) then -1
              else if not (CheckItem.strictly_before_dmy d11 d22) then 1
              else if not (CheckItem.strictly_before_dmy d22 d12) then -1
              else 1
          | _ ->
              match
                date_interval conf base AddChildren x1,
                date_interval conf base AddChildren x2
              with
                Some (d11, d12), Some (d21, d22) ->
                  if not (CheckItem.strictly_before_dmy d21 d12) then -1
                  else if not (CheckItem.strictly_before_dmy d11 d22) then 1
                  else if not (CheckItem.strictly_before_dmy d22 d12) then -1
                  else 1
              | Some _, None -> -1
              | None, Some _ -> 1
              | None, None -> -1

let compare_title_order conf base (x1, t1) (x2, t2) =
  if t1.t_nth = 0 || t2.t_nth = 0 || t1.t_nth = t2.t_nth then
    compare_title_dates conf base (x1, t1) (x2, t2)
  else compare t1.t_nth t2.t_nth

let my_alphabetic n1 n2 =
  (*
    compare (Name.abbrev (Name.lower n1)) (Name.abbrev (Name.lower n2))
  *)
  compare (Name.lower n1) (Name.lower n2)

let string_cnt_list_uniq l =
  let l =
    List.fold_left
      (fun l (e, c) ->
         match l with
           [] -> [e, c]
         | (x, d) :: l1 ->
             if my_alphabetic e x = 0 then (x, c + d) :: l1 else (e, c) :: l)
      [] l
  in
  List.rev l

let compare_titles2 (t1, _) (t2, _) = my_alphabetic t1 t2

(**)

let select_title_place conf base title place =
  let list = ref [] in
  let clean_title = ref title in
  let clean_place = ref place in
  let all_names = ref [] in
  let title1 = Name.lower title in
  let place1 = Name.lower place in
  let absolute = p_getenv conf.env "a" = Some "A" in
  let select x t =
    if absolute && sou base t.t_ident = title && sou base t.t_place = place ||
       not absolute && Name.lower (sou base t.t_ident) = title1 &&
       Name.lower (sou base t.t_place) = place1
    then
      let tn = sou base t.t_ident in
      clean_title := tn;
      clean_place := sou base t.t_place;
      list := (x, t) :: !list;
      if not (List.mem tn !all_names) then all_names := tn :: !all_names
  in
  for i = 0 to nb_of_persons base - 1 do
    let x = pget conf base (Adef.iper_of_int i) in
    List.iter (select x) (nobtit conf base x)
  done;
  !list, !clean_title, !clean_place, !all_names

let select_all_with_place conf base place =
  let list = ref [] in
  let clean_place = ref place in
  let place = Name.lower place in
  let select x t =
    if Name.lower (sou base t.t_place) = place then
      begin clean_place := sou base t.t_place; list := (x, t) :: !list end
  in
  for i = 0 to nb_of_persons base - 1 do
    let x = pget conf base (Adef.iper_of_int i) in
    List.iter (select x) (nobtit conf base x)
  done;
  !list, !clean_place

let select_title conf base title =
  let set = ref StrSet.empty in
  let clean_name = ref title in
  let all_names = ref [] in
  let absolute = p_getenv conf.env "a" = Some "A" in
  let title2 = Name.lower title in
  let add_place t =
    let tn = sou base t.t_ident in
    if absolute && tn = title ||
       not absolute && Name.lower tn = title2
    then
      let pn = sou base t.t_place in
      if not (StrSet.mem pn !set) then
        begin clean_name := tn; set := StrSet.add pn !set end;
      if not (List.mem tn !all_names) then all_names := tn :: !all_names
  in
  for i = 0 to nb_of_persons base - 1 do
    let x = pget conf base (Adef.iper_of_int i) in
    List.iter add_place (nobtit conf base x)
  done;
  StrSet.elements !set, !clean_name, !all_names

let select_place conf base place =
  let list = ref [] in
  let clean_name = ref place in
  let place2 = Name.lower place in
  let add_title t =
    let pn = sou base t.t_place in
    if Name.lower pn = place2 then
      let tn = sou base t.t_ident in
      if not (List.mem tn !list) then
        begin clean_name := pn; list := tn :: !list end
  in
  for i = 0 to nb_of_persons base - 1 do
    let x = pget conf base (Adef.iper_of_int i) in
    List.iter add_title (nobtit conf base x)
  done;
  !list, !clean_name

let select_all proj conf base =
  let s =
    let rec loop i s =
      if i = nb_of_persons base then s
      else
        let x = pget conf base (Adef.iper_of_int i) in
        let s =
          List.fold_left (fun s t -> StrSet.add (sou base (proj t)) s) s
            (nobtit conf base x)
        in
        loop (i + 1) s
    in
    loop 0 StrSet.empty
  in
  StrSet.elements s

let select_all2 proj conf base =
  let ht = Hashtbl.create 1 in
  for i = 0 to nb_of_persons base - 1 do
    let x = pget conf base (Adef.iper_of_int i) in
    List.iter
      (fun t ->
         let s = sou base (proj t) in
         let cnt =
           try Hashtbl.find ht s with
             Not_found -> let cnt = ref 0 in Hashtbl.add ht s cnt; cnt
         in
         incr cnt)
      (nobtit conf base x)
  done;
  Hashtbl.fold (fun s cnt list -> (s, !cnt) :: list) ht []

let select_all_titles = select_all2 (fun t -> t.t_ident)
let select_all_places = select_all (fun t -> t.t_place)

let give_access_someone conf base (x, t) list =
  let t_date_start = Adef.od_of_cdate t.t_date_start in
  let t_date_end = Adef.od_of_cdate t.t_date_end in
  let has_dates =
    match t_date_start, t_date_end with
      Some (Dgreg (_, _)), _ | _, Some (Dgreg (_, _)) -> true
    | _ -> false
  in
  if has_dates then Wserver.printf "<em>";
  begin match t_date_start with
    Some (Dgreg (d, _)) -> Wserver.printf "%d" d.year
  | _ -> ()
  end;
  begin match t_date_end with
    Some (Dgreg (d, _)) -> Wserver.printf "-%d" d.year
  | _ -> ()
  end;
  if has_dates then Wserver.printf "</em>: ";
  if List.mem x list then Wserver.printf "<em>"
  else Wserver.printf "<a href=\"%s%s\">" (commd conf) (acces conf base x);
  begin match t.t_name, get_public_name x, get_qualifiers x with
    Tmain, pn, nn :: _ when sou base pn <> "" ->
      Wserver.printf "%s <em>%s</em> %s" (sou base pn) (sou base nn)
        (p_surname base x)
  | Tmain, pn, [] when sou base pn <> "" ->
      Wserver.printf "%s %s" (sou base pn) (p_surname base x)
  | Tname n, _, nn :: _ ->
      Wserver.printf "%s <em>%s</em> %s" (sou base n) (sou base nn)
        (p_surname base x)
  | Tname n, _, [] -> Wserver.printf "%s %s" (sou base n) (p_surname base x)
  | _ -> Wserver.printf "%s" (person_text conf base x)
  end;
  Wserver.printf "\n";
  Wserver.printf "%s" (Date.short_dates_text conf base x);
  if t.t_nth <> 0 then
    Wserver.printf " (%s)"
      (if t.t_nth >= 100 then string_of_int t.t_nth
       else transl_nth conf "nth" t.t_nth);
  if List.mem x list then Wserver.printf "</em>" else Wserver.printf "</a>"

let give_access_title conf t p =
  Wserver.printf "<a href=\"%sm=TT&sm=S&t=%s&p=%s\">" (commd conf)
    (code_varenv t) (code_varenv p);
  Wserver.printf "%s" (capitale t);
  Wserver.printf "</a>\n"

let give_access_all_titles conf t absolute =
  Wserver.printf "<a href=\"%sm=TT&sm=S&t=%s%s\">" (commd conf)
    (code_varenv t) (if absolute then "&a=A" else "");
  Wserver.printf "%s" (if absolute then t else capitale t);
  Wserver.printf "</a>"

let give_access_all_places conf t =
  Wserver.printf "<a href=\"%sm=TT&sm=S&p=%s\">" (commd conf) (code_varenv t);
  Wserver.printf "... %s" t;
  Wserver.printf "</a>\n"

let propose_tree_for_list list conf =
  let (list, _) =
    List.fold_left
      (fun (list, n) (p, _) ->
         let list = if List.mem_assq p list then list else (p, n) :: list in
         list, n + 1)
      ([], 1) list
  in
  begin match List.rev list with
    _ :: _ :: _ as list ->
      Wserver.printf "<p>\n";
      Wserver.printf "<a href=\"%sm=RLM" (commd conf);
      begin let _ =
        List.fold_left
          (fun i (p, n) ->
             Wserver.printf "&i%d=%d&t%d=%d" i
               (Adef.int_of_iper (get_key_index p)) i n;
             i + 1)
          1 list
      in
        Wserver.printf "&lim=6\">%s</a>\n" (capitale (transl conf "tree"))
      end;
      Wserver.printf "</p>\n"
  | _ -> ()
  end

let print_title_place_list conf base t p t_equiv list =
  let absolute = p_getenv conf.env "a" = Some "A" in
  let title h =
    if h || absolute then
      begin Wserver.printf "%s" t; if p <> "" then Wserver.printf " %s" p end
    else
      Mutil.list_iter_first
        (fun first t ->
           if not first then Wserver.printf ",\n";
           Wserver.printf "<a href=\"%sm=TT&sm=S&t=%s&a=A\">" (commd conf)
             (code_varenv t);
           Wserver.printf "%s</a>" t;
           if p <> "" then
             begin
               Wserver.printf "\n<a href=\"%sm=TT&sm=S&p=%s&a=A\">"
                 (commd conf) (code_varenv p);
               Wserver.printf "%s</a>" p
             end)
        t_equiv
  in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  begin let _ =
    List.fold_left
      (fun list x ->
         Wserver.printf "<li>";
         give_access_someone conf base x list;
         Wserver.printf "</li>\n";
         fst x :: list)
      [] list
  in
    ()
  end;
  Wserver.printf "</ul>\n";
  propose_tree_for_list list conf;
  Hutil.trailer conf

let print_all_with_place_list conf base p list =
  let title _ = Wserver.printf "... %s\n" p in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  let _ =
    List.fold_left
      (fun list (_, t as x) ->
         html_li conf;
         give_access_someone conf base x [];
         Wserver.printf ", %s\n" (sou base t.t_ident);
         Wserver.printf "\n";
         fst x :: list)
      [] list
  in
  Wserver.printf "</ul>\n";
  propose_tree_for_list list conf;
  Hutil.trailer conf

let print_title_place conf base t p =
  let (l, t, p, t_equiv) = select_title_place conf base t p in
  let list = List.sort (compare_title_order conf base) l in
  print_title_place_list conf base t p t_equiv list

let print_all_with_place conf base p =
  let (l, p) = select_all_with_place conf base p in
  let list = List.sort (compare_title_dates conf base) l in
  print_all_with_place_list conf base p list

let print_places_list conf base t t_equiv list =
  let title h =
    if h || List.length t_equiv = 1 then Wserver.printf "%s" t
    else
      Mutil.list_iter_first
        (fun first t ->
           Wserver.printf "%s" (if first then "" else ", ");
           give_access_all_titles conf t true)
        t_equiv
  in
  let order s = capitale (Name.lower (surname_without_particle base s)) in
  let list = List.sort (fun s1 s2 -> compare (order s1) (order s2)) list in
  let absolute = p_getenv conf.env "a" = Some "A" in
  let wprint_elem p =
    Wserver.printf "<a href=\"%sm=TT&sm=S&t=%s&p=%s%s\">" (commd conf)
      (code_varenv t) (code_varenv p) (if absolute then "&a=A" else "");
    if p = "" then Wserver.printf "..."
    else Wserver.printf "%s%s" (surname_without_particle base p) (surname_particle base p);
    Wserver.printf "</a>"
  in
  Hutil.header conf title;
  wprint_in_columns conf order wprint_elem list;
  Hutil.trailer conf

let print_places conf base t =
  let (l, t, t_equiv) = select_title conf base t in
  let list = List.sort_uniq my_alphabetic l in
  match list with
    [p] -> print_title_place conf base t p
  | _ -> print_places_list conf base t t_equiv list

let print_titles conf base p =
  let (l, p) = select_place conf base p in
  let list = List.sort_uniq my_alphabetic l in
  let title _ = Wserver.printf "... %s" p in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  List.iter
    (fun t ->
       Wserver.printf "<li>";
       give_access_title conf t p;
       Wserver.printf "</li>\n")
    list;
  Wserver.printf "</ul>\n";
  if List.length list > 1 then
    begin
      Wserver.printf "<a href=\"%sm=TT&sm=A&p=%s\">" (commd conf)
        (code_varenv p);
      Wserver.printf "%s" (capitale (transl conf "the whole list"));
      Wserver.printf "</a>\n"
    end;
  Hutil.trailer conf

let print_all_titles conf base =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "all the titles"))
  in
  let list =
    let l = select_all_titles conf base in
    string_cnt_list_uniq (List.sort compare_titles2 l)
  in
  let order (s, _) = capitale (Name.lower s) in
  let wprint_elem (t, cnt) =
    give_access_all_titles conf t false; Wserver.printf " (%d)" cnt
  in
  Hutil.header conf title;
  wprint_in_columns conf order wprint_elem list;
  Hutil.trailer conf

let print_all_places conf base =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "all the estates"))
  in
  let list =
    let l = select_all_places conf base in
    List.sort_uniq my_alphabetic l
  in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  List.iter
    (fun t ->
       Wserver.printf "<li>";
       give_access_all_places conf t;
       Wserver.printf "</li>\n")
    list;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let print conf base =
  match
    p_getenv conf.env "sm", p_getenv conf.env "t", p_getenv conf.env "p"
  with
    Some "S", Some t, Some p -> print_title_place conf base t p
  | Some "S", Some t, None -> print_places conf base t
  | Some "S", None, Some p -> print_titles conf base p
  | Some "A", None, Some p -> print_all_with_place conf base p
  | _, (Some "" | None), (Some "" | None) -> print_all_titles conf base
  | _, (Some "" | None), Some "*" -> print_all_places conf base
  | _, (Some "" | None), Some p -> print_titles conf base p
  | _, Some t, (Some "" | None) -> print_places conf base t
  | _, Some t, Some p -> print_title_place conf base t p
