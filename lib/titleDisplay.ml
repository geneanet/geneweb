open Config
open Def
open Gwdb
open Util
open Title

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
  Wserver.printf "%s" (DateDisplay.short_dates_text conf base x);
  if t.t_nth <> 0 then
    Wserver.printf " (%s)"
      (if t.t_nth >= 100 then string_of_int t.t_nth
       else transl_nth conf "nth" t.t_nth);
  if List.mem x list then Wserver.printf "</em>" else Wserver.printf "</a>"

let give_access_title conf t p =
  Wserver.printf "<a href=\"%sm=TT&sm=S&t=%s&p=%s\">" (commd conf)
    (code_varenv t) (code_varenv p);
  Wserver.printf "%s" (Utf8.capitalize t);
  Wserver.printf "</a>\n"

let give_access_all_titles conf t absolute =
  Wserver.printf "<a href=\"%sm=TT&sm=S&t=%s%s\">" (commd conf)
    (code_varenv t) (if absolute then "&a=A" else "");
  Wserver.printf "%s" (if absolute then t else Utf8.capitalize t);
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
             Wserver.printf "&i%d=%s&t%d=%d" i
               (Gwdb.string_of_iper (get_iper p)) i n;
             i + 1)
          1 list
      in
        Wserver.printf "&lim=6\">%s</a>\n" (Utf8.capitalize (transl conf "tree"))
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
  List.iter
    (fun ((_, t) as x) ->
       Wserver.printf "<li>" ;
       give_access_someone conf base x [];
       Wserver.printf ", %s<li>" (sou base t.t_ident) )
    list ;
  Wserver.printf "</ul>\n";
  propose_tree_for_list list conf;
  Hutil.trailer conf

let select_title_place conf base title place =
  select_title_place conf base title place ~absolute:(p_getenv conf.env "a" = Some "A")

let select_title conf base title =
  select_title conf base title  ~absolute:(p_getenv conf.env "a" = Some "A")

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
  let order s = Utf8.capitalize (Name.lower (surname_without_particle base s)) in
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
      Wserver.printf "%s" (Utf8.capitalize (transl conf "the whole list"));
      Wserver.printf "</a>\n"
    end;
  Hutil.trailer conf

let print_all_titles conf base =
  let title _ =
    Wserver.printf "%s" (Utf8.capitalize (transl conf "all the titles"))
  in
  let list =
    let l = select_all_titles conf base in
    string_cnt_list_uniq (List.sort compare_titles2 l)
  in
  let order (s, _) = Utf8.capitalize (Name.lower s) in
  let wprint_elem (t, cnt) =
    give_access_all_titles conf t false; Wserver.printf " (%d)" cnt
  in
  Hutil.header conf title;
  wprint_in_columns conf order wprint_elem list;
  Hutil.trailer conf

let print_all_places conf base =
  let title _ =
    Wserver.printf "%s" (Utf8.capitalize (transl conf "all the estates"))
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
