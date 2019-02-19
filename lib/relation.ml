(* $Id: relation.ml,v 5.22 2008-01-12 01:49:50 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util
open Dag2html

let round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0

(* find shortest path :
 * parents, siblings, mates and children are at distance 1.
 *)
type famlink = Self | Parent | Sibling | HalfSibling | Mate | Child

type 'a dag_ind =
  { di_val : 'a; mutable di_famc : 'a dag_fam; mutable di_fams : 'a dag_fam }
and 'a dag_fam =
  { mutable df_pare : 'a dag_ind list; df_chil : 'a dag_ind list }

let dag_ind_list_of_path path =
  let (indl, _) =
    let merge l1 l2 = if l1 == l2 then l1 else l1 @ l2 in
    List.fold_left
      (fun (indl, prev_ind) (ip, fl) ->
         let (ind, indl) =
           try List.find (fun di -> di.di_val = Some ip) indl, indl with
             Not_found ->
               let rec ind =
                 {di_val = Some ip; di_famc = famc; di_fams = fams}
               and famc = {df_pare = []; df_chil = [ind]}
               and fams = {df_pare = [ind]; df_chil = []} in
               ind, ind :: indl
         in
         let fam =
           match prev_ind with
             None -> {df_pare = []; df_chil = []}
           | Some p_ind ->
               match fl with
                 Parent ->
                   {df_pare = merge p_ind.di_famc.df_pare ind.di_fams.df_pare;
                    df_chil = merge p_ind.di_famc.df_chil ind.di_fams.df_chil}
               | Child ->
                   {df_pare = merge p_ind.di_fams.df_pare ind.di_famc.df_pare;
                    df_chil = merge p_ind.di_fams.df_chil ind.di_famc.df_chil}
               | Sibling | HalfSibling ->
                   {df_pare = merge p_ind.di_famc.df_pare ind.di_famc.df_pare;
                    df_chil = merge p_ind.di_famc.df_chil ind.di_famc.df_chil}
               | Mate ->
                   {df_pare = merge p_ind.di_fams.df_pare ind.di_fams.df_pare;
                    df_chil = merge p_ind.di_fams.df_chil ind.di_fams.df_chil}
               | Self -> {df_pare = []; df_chil = []}
         in
         List.iter (fun ind -> ind.di_famc <- fam) fam.df_chil;
         List.iter (fun ind -> ind.di_fams <- fam) fam.df_pare;
         indl, Some ind)
      ([], None) (List.rev path)
  in
  indl

let add_missing_parents_of_siblings conf base indl =
  List.fold_right
    (fun ind indl ->
       let indl =
         match ind.di_famc with
           {df_pare = []; df_chil = [_]} -> indl
         | {df_pare = []; df_chil = children} ->
             let ipl =
               List.fold_right
                 (fun ind ipl ->
                    match ind.di_val with
                      Some ip ->
                        let ip =
                          match get_parents (pget conf base ip) with
                            Some ifam -> get_father (foi base ifam)
                          | None -> assert false
                        in
                        if List.mem ip ipl then ipl else ip :: ipl
                    | _ -> assert false)
                 children []
             in
             let fams = {df_pare = []; df_chil = children} in
             let indl1 =
               List.fold_left
                 (fun indl ip ->
                    let rec indp =
                      {di_val = Some ip; di_famc = famc; di_fams = fams}
                    and famc = {df_pare = []; df_chil = [indp]} in
                    fams.df_pare <- indp :: fams.df_pare; indp :: indl)
                 [] ipl
             in
             List.iter (fun ind -> ind.di_famc <- fams) children; indl1 @ indl
         | _ -> indl
       in
       ind :: indl)
    indl []

let dag_fam_list_of_ind_list indl =
  List.fold_left
    (fun faml ind ->
       let faml =
         if List.mem ind.di_famc faml then faml else ind.di_famc :: faml
       in
       if List.mem ind.di_fams faml then faml else ind.di_fams :: faml)
    [] indl

let add_phony_children indl faml =
  List.fold_right
    (fun fam indl ->
       match fam with
         {df_pare = [_]; df_chil = []} -> indl
       | {df_pare = pare; df_chil = []} ->
           let rec ind = {di_val = None; di_famc = famc; di_fams = fams}
           and famc = {df_pare = pare; df_chil = [ind]}
           and fams = {df_pare = [ind]; df_chil = []} in
           List.iter (fun ind -> ind.di_fams <- famc) pare;
           ind :: indl
       | _ -> indl)
    faml indl

let dag_of_ind_dag_list indl =
  let (indl, _) =
    List.fold_right (fun ind (indl, cnt) -> (ind, cnt) :: indl, cnt + 1) indl
      ([], 0)
  in
  let idag_of_di_ind ind = idag_of_int (List.assq ind indl) in
  List.map
    (fun (ind, cnt) ->
       {pare = List.map idag_of_di_ind ind.di_famc.df_pare;
        valu =
          begin match ind.di_val with
            Some ic -> Def.Left ic
          | None -> Def.Right cnt
          end;
        chil = List.map idag_of_di_ind ind.di_fams.df_chil})
    indl

let dag_of_relation_path conf base path =
  let indl = dag_ind_list_of_path path in
  let indl = add_missing_parents_of_siblings conf base indl in
  let faml = dag_fam_list_of_ind_list indl in
  let indl = add_phony_children indl faml in
  let nl = dag_of_ind_dag_list indl in
  let d = {dag = Array.of_list (List.rev nl)} in
  let set =
    List.fold_left
      (fun set n ->
         match n.valu with
           Def.Left ip -> Dag.Pset.add ip set
         | Def.Right _ -> set)
      Dag.Pset.empty nl
  in
  set, d

let old_print_relationship_dag conf base elem_txt vbar_txt path next_txt =
  let invert =
    match Util.p_getenv conf.env "invert" with
      Some "on" -> true
    | _ -> false
  in
  let (set, d) = dag_of_relation_path conf base path in
  let page_title = capitale (transl conf "relationship") in
  let hts = Dag.make_tree_hts conf base elem_txt vbar_txt invert set [] d in
  Dag.print_slices_menu_or_dag_page conf page_title hts next_txt

let add_common_parent base ip1 ip2 set =
  let a1 = poi base ip1 in
  let a2 = poi base ip2 in
  match get_parents a1, get_parents a2 with
    Some ifam1, Some ifam2 ->
      let cpl1 = foi base ifam1 in
      let cpl2 = foi base ifam2 in
      if get_father cpl1 = get_father cpl2 then
        Dag.Pset.add (get_father cpl1) set
      else if get_mother cpl1 = get_mother cpl2 then
        Dag.Pset.add (get_mother cpl1) set
      else set
  | _ -> set

let ind_set_of_relation_path base path =
  let (set, _) =
    List.fold_left
      (fun (set, prev_ip) (ip, fl) ->
         let set =
           match fl with
             Parent | Child | Self | Mate -> set
           | Sibling | HalfSibling ->
               match prev_ip with
                 Some prev_ip -> add_common_parent base prev_ip ip set
               | None -> set
         in
         Dag.Pset.add ip set, Some ip)
      (Dag.Pset.empty, None) (List.rev path)
  in
  set

let print_relationship_dag conf base elem_txt vbar_txt path next_txt =
  if p_getenv conf.env "new" <> Some "on" then
    old_print_relationship_dag conf base elem_txt vbar_txt path next_txt
  else
    let invert =
      match Util.p_getenv conf.env "invert" with
        Some "on" -> true
      | _ -> false
    in
    let set = ind_set_of_relation_path base path in
    let page_title = capitale (transl conf "relationship") in
    Dag.make_and_print_dag conf base elem_txt vbar_txt invert set []
      page_title next_txt

let next_relation_link_txt conf ip1 ip2 excl_faml =
  let bd =
    match p_getenv conf.env "bd" with
      None | Some ("0" | "") -> ""
    | Some x -> "&bd=" ^ x
  in
  let color =
    match p_getenv conf.env "color" with
      None -> ""
    | Some x -> "&color=" ^ code_varenv x
  in
  let (sl, _) =
    List.fold_left
      (fun (sl, i) ifam ->
         "&ef" :: string_of_int i :: "=" ::
         string_of_int (Adef.int_of_ifam ifam) :: sl,
         i - 1)
      ([], List.length excl_faml - 1) excl_faml
  in
  let sl =
    commd conf :: "em=R&ei=" :: string_of_int (Adef.int_of_iper ip1) ::
    "&i=" :: string_of_int (Adef.int_of_iper ip2) ::
    (if p_getenv conf.env "spouse" = Some "on" then "&spouse=on" else "") ::
    (if conf.cancel_links then "&cgl=on" else "") :: bd :: color :: "&et=S" ::
    sl
  in
  String.concat "" sl

let print_relation_path conf base ip1 ip2 path ifam excl_faml =
  if path = [] then
    let title _ =
      Wserver.printf "%s" (capitale (transl conf "relationship"))
    in
    Hutil.header_no_page_title conf title; Hutil.trailer conf
  else
    let next_txt = next_relation_link_txt conf ip1 ip2 (ifam :: excl_faml) in
    let elem_txt p = Dag.Item (p, "") in
    let vbar_txt ip =
      let u = pget conf base ip in
      let excl_faml = Array.to_list (get_family u) @ excl_faml in
      next_relation_link_txt conf ip1 ip2 excl_faml
    in
    print_relationship_dag conf base elem_txt vbar_txt path next_txt

type node =
    NotVisited
  | Visited of (bool * iper * famlink)

let get_shortest_path_relation conf base ip1 ip2 excl_faml =
  let mark_per = Array.make (nb_of_persons base) NotVisited in
  let mark_fam = Array.make (nb_of_families base) false in
  List.iter
    (fun i ->
       let i = Adef.int_of_ifam i in
       if i < Array.length mark_fam then mark_fam.(i) <- true)
    excl_faml;
  let parse_fam ifam =
    if mark_fam.(Adef.int_of_ifam ifam) then []
    else
      let fam = foi base ifam in
      mark_fam.(Adef.int_of_ifam ifam) <- true;
      let result =
        [get_father fam, Parent, ifam; get_mother fam, Parent, ifam]
      in
      let result =
        result @
        List.fold_right
          (fun child children -> (child, Sibling, ifam) :: children)
          (Array.to_list (get_children (foi base ifam))) []
      in
      let result =
        result @
        List.fold_right
          (fun fam children ->
             if ifam = fam then children
             else if mark_fam.(Adef.int_of_ifam fam) then children
             else
               List.fold_right
                 (fun child children -> (child, HalfSibling, fam) :: children)
                 (Array.to_list (get_children (foi base fam))) children)
          (Array.to_list (get_family (pget conf base (get_father fam)))) []
      in
      result @
      List.fold_right
        (fun fam children ->
           if ifam = fam then children
           else if mark_fam.(Adef.int_of_ifam fam) then children
           else
             List.fold_right
               (fun child children -> (child, HalfSibling, fam) :: children)
               (Array.to_list (get_children (foi base fam))) children)
        (Array.to_list (get_family (pget conf base (get_mother fam)))) []
  in
  let neighbours iper =
    let result =
      List.fold_right
        (fun ifam nb ->
           if mark_fam.(Adef.int_of_ifam ifam) then nb
           else
             let fam = foi base ifam in
             mark_fam.(Adef.int_of_ifam ifam) <- true;
             List.fold_right
               (fun child children -> (child, Child, ifam) :: children)
               (Array.to_list (get_children fam))
               [get_father fam, Mate, ifam; get_mother fam, Mate, ifam] @
             nb)
        (Array.to_list (get_family (pget conf base iper))) []
    in
    result @
    (match get_parents (pget conf base iper) with
       Some ifam -> parse_fam ifam
     | _ -> [])
  in
  let rec make_path path vertex =
    match List.hd path with
    | _, Self -> path
    | _, _ ->
        match mark_per.(Adef.int_of_iper vertex) with
          NotVisited -> assert false
        | Visited (_, v, f) -> make_path ((vertex, f) :: path) v
  in
  let merge_path p1 p2 =
    let swap_order el =
      match el with
        iper, Parent -> iper, Child
      | iper, Child -> iper, Parent
      | _ -> el
    in
    List.rev_append
      (List.rev_map2 (fun (ip1, _) (_, fl2) -> swap_order (ip1, fl2))
         (List.rev (List.tl (List.rev p1)))
         (List.tl p1))
      (List.rev p2)
  in
  let one_step_further source queue =
    let rec loop1 newvertexlist =
      function
        vertex :: vertexlist ->
          let rec loop2 result =
            function
              (iper, fl, ifam) :: neighbourslist ->
                begin match mark_per.(Adef.int_of_iper iper) with
                  NotVisited ->
                    mark_per.(Adef.int_of_iper iper) <-
                      Visited (source, vertex, fl);
                    loop2 (iper :: result) neighbourslist
                | Visited (s, v, f) ->
                    if s = source then loop2 result neighbourslist
                    else
                      let p1 = make_path [iper, fl] vertex in
                      let p2 = make_path [iper, f] v in
                      let path =
                        if source then merge_path p2 p1 else merge_path p1 p2
                      in
                      Left (path, ifam)
                end
            | [] -> loop1 result vertexlist
          in
          loop2 newvertexlist (neighbours vertex)
      | [] -> Right newvertexlist
    in
    loop1 [] queue
  in
  let rec width_search queue1 visited1 queue2 visited2 =
    if queue1 = [] || queue2 = [] then None
    else if visited1 > visited2 then
      let visited2 = visited2 + List.length queue2 in
      match one_step_further false queue2 with
        Left (path, ifam) -> Some (path, ifam)
      | Right queue2 -> width_search queue1 visited1 queue2 visited2
    else
      let visited1 = visited1 + List.length queue1 in
      match one_step_further true queue1 with
        Left (path, ifam) -> Some (path, ifam)
      | Right queue1 -> width_search queue1 visited1 queue2 visited2
  in
  mark_per.(Adef.int_of_iper ip1) <- Visited (true, ip1, Self);
  mark_per.(Adef.int_of_iper ip2) <- Visited (false, ip2, Self);
  width_search [ip1] 0 [ip2] 0

let print_shortest_path conf base p1 p2 =
  let ip1 = get_key_index p1 in
  let ip2 = get_key_index p2 in
  if ip1 = ip2 then
    let title _ =
      Wserver.printf "%s" (capitale (transl conf "relationship"))
    in
    Hutil.header conf title;
    Wserver.printf "%s\n" (capitale (transl conf "it is the same person!"));
    Hutil.trailer conf
  else
    let excl_faml =
      let rec loop list i =
        match p_getint conf.env ("ef" ^ string_of_int i) with
          Some k -> loop (Adef.ifam_of_int k :: list) (i + 1)
        | None ->
            match find_person_in_env conf base ("ef" ^ string_of_int i) with
              Some p ->
                let n =
                  match p_getint conf.env ("fef" ^ string_of_int i) with
                    Some n -> n
                  | None -> 0
                in
                let u = p in
                let list =
                  if n < Array.length (get_family u) then
                    (get_family u).(n) :: list
                  else list
                in
                loop list (i + 1)
            | None -> list
      in
      loop [] 0
    in
    let title _ =
      Wserver.printf "%s" (capitale (transl conf "relationship"))
    in
    match get_shortest_path_relation conf base ip1 ip2 excl_faml with
      Some (path, ifam) ->
        print_relation_path conf base ip1 ip2 path ifam excl_faml
    | None ->
        let s1 = gen_person_title_text reference raw_access conf base p1 in
        let s2 = gen_person_title_text reference raw_access conf base p2 in
        Hutil.header_no_page_title conf title;
        if excl_faml = [] then
          begin
            Wserver.printf "<h1>";
            title false;
            Wserver.printf "</h1>\n";
            Hutil.print_link_to_welcome conf true;
            Wserver.printf "%s.\n"
              (capitale
                 (cftransl conf "no known relationship link between %s and %s"
                    [s1; s2]));
            Wserver.printf "<br%s>\n" conf.xhs;
            begin
              Wserver.printf "<p>\n";
              begin
                Wserver.printf "<span>";
                begin
                  Wserver.printf "<a href=\"%s&m=R&%s\">" (commd conf)
                    (acces conf base p1);
                  Wserver.printf "%s"
                    (capitale
                       (transl_nth conf "try another/relationship computing"
                          0));
                  Wserver.printf "</a>"
                end;
                Wserver.printf " %s.\n"
                  (transl_nth conf "try another/relationship computing" 1);
                Wserver.printf "</span>"
              end;
              Wserver.printf "</p>\n"
            end
          end
        else
          begin
            Wserver.printf "<ul>\n";
            Wserver.printf "<li>%s</li>\n" s1;
            Wserver.printf "<li>%s</li>\n" s2;
            Wserver.printf "</ul>\n"
          end;
        Hutil.trailer conf

let nb_fields s =
  let rec loop cnt i =
    if i = String.length s then cnt
    else if s.[i] = '/' then loop (cnt + 1) (i + 1)
    else loop cnt (i + 1)
  in
  loop 1 0

let rec belongs_to_branch ip dist =
  function
    (n, _, ipl) :: lens ->
      if n = dist && List.mem ip ipl then true
      else belongs_to_branch ip dist lens
  | [] -> false

let get_piece_of_branch conf base (((reltab, list), x), proj) (len1, len2) =
  let (anc, _) = List.hd list in
  let rec loop ip dist =
    if dist <= len1 then []
    else
      let lens = proj reltab.(Adef.int_of_iper ip) in
      let rec loop1 =
        function
          ifam :: ifaml ->
            let rec loop2 =
              function
                ipc :: ipl ->
                  if belongs_to_branch ipc dist lens then
                    let dist = dist - 1 in
                    if dist <= len2 then ipc :: loop ipc dist
                    else loop ipc dist
                  else loop2 ipl
              | [] -> loop1 ifaml
            in
            loop2 (Array.to_list (get_children (foi base ifam)))
        | [] -> []
      in
      loop1 (Array.to_list (get_family (pget conf base ip)))
  in
  loop (get_key_index anc) x

let parents_label conf base info =
  function
    1 -> transl conf "the parents"
  | 2 ->
      let txt = transl conf "grand-parents" in
      let is =
        if nb_fields txt = 2 then
          match get_piece_of_branch conf base info (1, 1) with
            [ip1] -> if get_sex (pget conf base ip1) = Male then 0 else 1
          | _ ->             (* must be a bug *)0
        else 0
      in
      nth_field txt is
  | 3 ->
      let txt = transl conf "great-grand-parents" in
      let is =
        if nb_fields txt = 2 then
          match get_piece_of_branch conf base info (1, 1) with
            [ip1] -> if get_sex (pget conf base ip1) = Male then 0 else 1
          | _ ->             (* must be a bug *)0
        else 0
      in
      nth_field txt is
  | n ->
      transl conf "ancestors (some)" ^ " " ^
      Printf.sprintf (ftransl conf "of the %s generation")
        (transl_nth conf "nth (generation)" n)

let parent_in_law_label conf child_sex parent_sex =
  let txt = transl conf "the father-in-law/the mother-in-law" in
  let is = index_of_sex parent_sex in
  if nb_fields txt = 2 then nth_field txt is
  else nth_field txt (2 * index_of_sex child_sex + is)

let ancestor_label conf base info x sex =
  let is = index_of_sex sex in
  match x with
    1 -> transl_nth conf "the father/the mother/a parent" is
  | 2 ->
      let txt = transl conf "a grandfather/a grandmother/a grandparent" in
      let is =
        if nb_fields txt = 6 then
          match get_piece_of_branch conf base info (1, 1) with
            [ip1] ->
              if get_sex (pget conf base ip1) = Male then is else is + 3
          | _ ->             (* must be a bug *)is
        else is
      in
      nth_field txt is
  | 3 ->
      let txt =
        transl conf
          "a great-grandfather/a great-grandmother/a great-grandparent"
      in
      let is =
        if nb_fields txt = 6 then
          match get_piece_of_branch conf base info (1, 1) with
            [ip1] ->
              if get_sex (pget conf base ip1) = Male then is else is + 3
          | _ ->             (* must be a bug *)is
        else is
      in
      nth_field txt is
  | n ->
      transl_nth conf "an ancestor" is ^ " " ^
      Printf.sprintf (ftransl conf "of the %s generation")
        (transl_nth conf "nth (generation)" n)

let child_in_law_label conf sex_child sex_parent =
  let txt = transl conf "a son-in-law/a daughter-in-law" in
  let is = index_of_sex sex_child in
  if nb_fields txt = 2 then nth_field txt is
  else nth_field txt (2 * index_of_sex sex_parent + is)

let descendant_label conf base info x p =
  let is = index_of_sex (get_sex p) in
  match x with
    1 -> transl_nth conf "a son/a daughter/a child" is
  | 2 ->
      let txt = transl conf "a grandson/a granddaughter/a grandchild" in
      let is =
        if nb_fields txt = 6 then
          let info = info, (fun r -> r.Consang.lens2) in
          match get_piece_of_branch conf base info (1, 1) with
            [ip1] ->
              if get_sex (pget conf base ip1) = Male then is else is + 3
          | _ ->             (* must be a bug *)is
        else is
      in
      nth_field txt is
  | 3 ->
      let txt =
        transl conf
          "a great-grandson/a great-granddaughter/a great-grandchild"
      in
      let is =
        if nb_fields txt = 12 then
          let info = info, (fun r -> r.Consang.lens2) in
          match get_piece_of_branch conf base info (1, 2) with
            [ip1; ip2] ->
              let is =
                if get_sex (pget conf base ip1) = Male then is else is + 6
              in
              if get_sex (pget conf base ip2) = Male then is else is + 3
          | _ ->             (* must be a bug *)is
        else is
      in
      nth_field txt is
  | n ->
      transl_nth conf "a descendant" is ^ " " ^
      Printf.sprintf (ftransl conf "of the %s generation")
        (transl_nth conf "nth (generation)" n)

let brother_label conf x sex =
  let is = index_of_sex sex in
  match x with
    1 -> transl_nth conf "a brother/a sister/a sibling" is
  | 2 -> transl_nth conf "a cousin" is
  | 3 -> transl_nth conf "a 2nd cousin" is
  | 4 -> transl_nth conf "a 3rd cousin" is
  | n ->
      Printf.sprintf (ftransl_nth conf "a %s cousin" is)
        (transl_nth conf "nth (cousin)" (n - 1))

let half_brother_label conf sex =
  let is = index_of_sex sex in
  transl_nth conf "a half-brother/a half-sister/a half-sibling" is

let brother_in_law_label conf brother_sex self_sex =
  let txt = transl conf "a brother-in-law/a sister-in-law" in
  let is = index_of_sex brother_sex in
  if nb_fields txt = 2 then nth_field txt is
  else nth_field txt (2 * index_of_sex self_sex + is)

let uncle_label conf base info x p =
  let is = index_of_sex (get_sex p) in
  match x with
    1 ->
      let txt = transl conf "an uncle/an aunt" in
      let is =
        if nb_fields txt = 4 then
          let info = info, (fun r -> r.Consang.lens1) in
          match get_piece_of_branch conf base info (1, 1) with
            [ip1] ->
              if get_sex (pget conf base ip1) = Male then is else is + 2
          | _ ->             (* must be a bug *)is
        else is
      in
      nth_field txt is
  | 2 ->
      let txt = transl conf "a great-uncle/a great-aunt" in
      let is =
        if nb_fields txt = 4 then
          let info = info, (fun r -> r.Consang.lens1) in
          match get_piece_of_branch conf base info (1, 1) with
            [ip1] ->
              if get_sex (pget conf base ip1) = Male then is else is + 2
          | _ ->             (* must be a bug *)is
        else is
      in
      nth_field txt is
  | n ->
      transl_nth conf "an uncle/an aunt" is ^ " " ^
      Printf.sprintf (ftransl conf "of the %s generation")
        (transl_nth conf "nth (generation)" n)

let nephew_label conf x p =
  let is = index_of_sex (get_sex p) in
  match x with
    1 -> transl_nth conf "a nephew/a niece" is
  | 2 -> transl_nth conf "a great-nephew/a great-niece" is
  | n ->
      transl_nth conf "a nephew/a niece" is ^ " " ^
      Printf.sprintf (ftransl conf "of the %s generation")
        (transl_nth conf "nth (generation)" n)

let same_parents conf base p1 p2 =
  get_parents (pget conf base (get_key_index p1)) =
    get_parents (pget conf base (get_key_index p2))

let print_link_name conf base n p1 p2 sol =
  let (pp1, pp2, (x1, x2, list), reltab) = sol in
  let info = reltab, list in
  Wserver.printf "%s"
    (if is_hide_names conf p2 && not (authorized_age conf base p2) then "x x"
     else person_title_text conf base p2);
  Wserver.printf " %s" (transl conf "is");
  if n > 1 then Wserver.printf " %s" (transl conf "also");
  Wserver.printf "\n";
  let (s, sp1, sp2) =
    let ini_p1 = p1
    and ini_p2 = p2 in
    let p1 =
      match pp1 with
        Some p1 -> p1
      | _ -> p1
    in
    let p2 =
      match pp2 with
        Some p2 -> p2
      | _ -> p2
    in
    let sp1 = pp1 <> None in
    let sp2 = pp2 <> None in
    if x2 = 0 then
      if sp1 && x1 = 1 then
        parent_in_law_label conf (get_sex ini_p1) (get_sex ini_p2), false, sp2
      else
        let info = (info, x1), (fun r -> r.Consang.lens1) in
        ancestor_label conf base info x1 (get_sex p2), sp1, sp2
    else if x1 = 0 then
      if sp2 && x2 = 1 then
        child_in_law_label conf (get_sex ini_p2) (get_sex ini_p1), sp1, false
      else descendant_label conf base (info, x2) x2 p2, sp1, sp2
    else if x2 = x1 then
      if x2 = 1 && not (same_parents conf base p2 p1) then
        half_brother_label conf (get_sex p2), sp1, sp2
      else if x2 = 1 && (sp2 || sp1) && get_sex p2 <> Neuter then
        brother_in_law_label conf (get_sex ini_p2) (get_sex ini_p1), false,
        false
      else brother_label conf x1 (get_sex p2), sp1, sp2
    else if x2 = 1 then
      uncle_label conf base (info, x1) (x1 - x2) p2, sp1, sp2
    else if x1 = 1 then nephew_label conf (x2 - x1) p2, sp1, sp2
    else if x2 < x1 then
      let s =
        let info = (info, x1), (fun r -> r.Consang.lens1) in
        let s = ancestor_label conf base info (x1 - x2) Neuter in
        transl_a_of_gr_eq_gen_lev conf (brother_label conf x2 (get_sex p2)) s s
      in
      s, sp1, sp2
    else
      let s =
        let sm = brother_label conf x1 Male in
        let sf = brother_label conf x1 Female in
        let d = descendant_label conf base (info, x2) (x2 - x1) p2 in
        let s =
          if sm = sf then sm
          else
            let info = (info, x2), (fun r -> r.Consang.lens2) in
            match get_piece_of_branch conf base info (x2 - x1, x2 - x1) with
              [ip2] -> if get_sex (pget conf base ip2) = Male then sm else sf
            | _ -> sm
        in
        transl_a_of_gr_eq_gen_lev conf d s s
      in
      s, sp1, sp2
  in
  let s =
    if sp2 then
      transl_a_of_gr_eq_gen_lev conf
        (transl_nth conf "the spouse" (index_of_sex (get_sex p2))) s s
    else s
  in
  let s =
    if sp1 then
      match pp1 with
        Some pp1 ->
        let s' = transl_nth conf "the spouse" (index_of_sex (get_sex pp1)) in
        transl_a_of_gr_eq_gen_lev conf s s' s'
      | None -> s
    else s
  in
  let s1 = "<strong>" ^ std_color conf s ^ "</strong>" in
  let s2 =
    if is_hide_names conf p1 && not (authorized_age conf base p1) then "x x"
    else gen_person_title_text no_reference raw_access conf base p1
  in
  let s =
    if x2 < x1 then transl_a_of_b conf s1 s2 s2
    else transl_a_of_gr_eq_gen_lev conf s1 s2 s2
  in
  Wserver.printf "%s.\n" (Util.translate_eval s)

let wprint_num conf n =
  Sosa.print (fun x -> Wserver.printf "%s" x)
    (transl conf "(thousand separator)") n

let string_of_big_int conf i =
  let sep = transl conf "(thousand separator)" in
  let rec glop i =
    if i = 0 then ""
    else
      let s = glop (i / 1000) in
      if s = "" then string_of_int (i mod 1000)
      else s ^ sep ^ Printf.sprintf "%03d" (i mod 1000)
  in
  glop i

let print_solution_ancestor conf base long p1 p2 pp1 pp2 x1 x2 list =
  let image_opt =
    match p_getenv conf.env "image" with
      Some "on" -> "&image=on"
    | _ -> ""
  in
  Wserver.printf "<ul>\n";
  List.iter
    (fun (a, n) ->
       Wserver.printf "<li>";
       Wserver.printf "<em>%s %s"
         (if n < 0 then "***" else string_of_big_int conf n)
         (transl_nth conf "branch/branches" (if n = 1 then 0 else 1));
       Wserver.printf "</em>\n";
       if not long then
         begin let propose_dag = n > 1 && n <= 10 in
           Wserver.printf ":\n ";
           let dp1 =
             match pp1 with
               Some p -> p
             | _ -> p1
           in
           let dp2 =
             match pp2 with
               Some p -> p
             | _ -> p2
           in
           Wserver.printf "<img src=\"%s/%s\" alt=\"\"%s>\n"
             (Util.image_prefix conf) "picto_rel_small.png" conf.xhs;
           Wserver.printf "<a href=\"%sm=RL&%s&l1=%d&%s&l2=%d&%s%s%s%s%s\">"
             (commd conf) (acces conf base a) x1 (acces_n conf base "1" dp1)
             x2 (acces_n conf base "2" dp2)
             (if pp1 = None then "" else "&" ^ acces_n conf base "3" p1)
             (if pp2 = None then "" else "&" ^ acces_n conf base "4" p2)
             (if propose_dag then "&dag=on" else "") image_opt;
           Wserver.printf "%s" (capitale (transl conf "see"));
           if n > 1 && not propose_dag then
             Wserver.printf "%s" (transl conf " the first branch");
           Wserver.printf "</a>"
         end;
       Wserver.printf "</li>")
    list;
  Wserver.printf "</ul>\n"

let print_solution_not_ancestor conf base long p1 p2 sol =
  let (pp1, pp2, (x1, x2, list), reltab) = sol in
  let image_opt =
    match p_getenv conf.env "image" with
      Some "on" -> "&image=on"
    | _ -> ""
  in
  Wserver.printf "<ul class=li_relationship>\n";
  Wserver.printf "<li>\n";
  Wserver.printf "%s\n" (capitale (transl conf "indeed,"));
  Wserver.printf "<ul>\n";
  List.iter
    (fun (a, n) ->
       Wserver.printf "<li>\n";
       Wserver.printf "%s" (person_title_text conf base a);
       Wserver.printf "\n<em>(";
       Wserver.printf "%d %s" n
         (transl_nth conf "relationship link/relationship links"
            (if n = 1 then 0 else 1));
       Wserver.printf ")</em>\n&nbsp;";
       if not long then
         begin let propose_dag = n > 1 && n <= 10 in
           let dp1 =
             match pp1 with
               Some p -> p
             | _ -> p1
           in
           let dp2 =
             match pp2 with
               Some p -> p
             | _ -> p2
           in
           Wserver.printf "<img src=\"%s/%s\" alt=\"\"%s>\n"
             (Util.image_prefix conf) "picto_rel_small.png" conf.xhs;
           Wserver.printf "<a href=\"%sm=RL&%s&l1=%d&%s&l2=%d&%s%s%s%s%s\">"
             (commd conf) (acces conf base a) x1 (acces_n conf base "1" dp1)
             x2 (acces_n conf base "2" dp2)
             (if pp1 = None then "" else "&" ^ acces_n conf base "3" p1)
             (if pp2 = None then "" else "&" ^ acces_n conf base "4" p2)
             (if propose_dag then "&dag=on" else "") image_opt;
           Wserver.printf "%s" (capitale (transl conf "see"));
           Wserver.printf "</a>"
         end;
       Wserver.printf "</li>\n")
    list;
  Wserver.printf "</ul>\n";
  begin let is_are =
    match list with
      [_] -> transl conf "is"
    | _ -> transl conf "are"
  in
    Wserver.printf "%s %s\n" is_are (transl conf "at the same time")
  end;
  begin let lab proj x =
    let info = ((reltab, list), x), proj in
    match list with
      [a, _] -> ancestor_label conf base info x (get_sex a)
    | _ -> parents_label conf base info x
  in
    let print pp p alab =
      let s = gen_person_title_text no_reference raw_access conf base p in
      let s =
        match pp with
          None -> transl_a_of_b conf alab s s
        | Some pp ->
            transl_a_of_gr_eq_gen_lev conf
              (let s = transl_nth conf "the spouse" (index_of_sex (get_sex pp)) in
               transl_a_of_b conf alab s s)
              s s
      in
      Wserver.printf "%s\n" (Util.translate_eval s)
    in
    Wserver.printf "<ul>\n";
    Wserver.printf "<li>\n";
    print pp1 p1 (lab (fun r -> r.Consang.lens1) x1);
    Wserver.printf "</li>\n";
    Wserver.printf "<li>\n";
    print pp2 p2 (lab (fun r -> r.Consang.lens2) x2);
    Wserver.printf "</li>\n";
    Wserver.printf "</ul>\n"
  end;
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n"

let print_solution conf base long n p1 p2 sol =
  let (pp1, pp2, (x1, x2, list), _) = sol in
  Wserver.printf "<p>\n";
  Wserver.printf "<img src=\"%s/%s\" alt=\"\"%s>\n" (Util.image_prefix conf)
    "picto_fleche_bleu.png" conf.xhs;
  print_link_name conf base n p1 p2 sol;
  Wserver.printf "</p>\n";
  if x1 = 0 || x2 = 0 then
    print_solution_ancestor conf base long p1 p2 pp1 pp2 x1 x2 list
  else print_solution_not_ancestor conf base long p1 p2 sol;
  Wserver.printf "\n"

let max_br = 33

let print_dag_links conf base p1 p2 rl =
  let module O = struct type t = iper let compare = compare end in
  let module M = Map.Make (O) in
  let anc_map =
    List.fold_left
      (fun anc_map (pp1, pp2, (x1, x2, list), _) ->
         List.fold_left
           (fun anc_map (p, n) ->
              let (pp1, pp2, nn, nt, maxlev) =
                try M.find (get_key_index p) anc_map with
                  Not_found -> pp1, pp2, 0, 0, 0
              in
              if nn >= max_br then anc_map
              else
                let v = pp1, pp2, nn + n, nt + 1, max maxlev (max x1 x2) in
                M.add (get_key_index p) v anc_map)
           anc_map list)
      M.empty rl
  in
  let is_anc =
    match rl with
      (_, _, (x1, x2, _), _) :: _ -> x1 = 0 || x2 = 0
    | _ -> false
  in
  let something =
    M.fold
      (fun _ (_, _, nn, nt, _) something ->
         something || nt > 1 && nn > 1 && nn < max_br)
      anc_map false
  in
  if something then
    let rest = ref false in
    if is_anc then
      Wserver.printf "<img src=\"%s/%s\" alt=\"\"%s>\n"
        (Util.image_prefix conf) "picto_fleche_bleu.png" conf.xhs
    else Wserver.printf "<ul>\n";
    M.iter
      (fun ip (pp1, pp2, nn, nt, _) ->
         let dp1 =
           match pp1 with
             Some p -> p
           | _ -> p1
         in
         let dp2 =
           match pp2 with
             Some p -> p
           | _ -> p2
         in
         if nt > 1 && nn > 1 && nn < max_br then
           let a = pget conf base ip in
           if is_anc then () else Wserver.printf "<li>\n";
           if not is_anc then
             Wserver.printf "%s%s\n" (person_title_text conf base a) (Util.transl conf ":");
           Wserver.printf "<a href=\"%sm=RL" (commd conf);
           Wserver.printf ";%s" (acces conf base a);
           Wserver.printf ";%s" (acces_n conf base "1" dp1);
           Wserver.printf ";%s" (acces_n conf base "2" dp2);
           if pp1 = None then ()
           else Wserver.printf ";%s" (acces_n conf base "3" p1);
           if pp2 = None then ()
           else Wserver.printf ";%s" (acces_n conf base "4" p2);
           let (l1, l2) =
             List.fold_left
               (fun (l1, l2) (_, _, (x1, x2, list), _) ->
                  List.fold_left
                    (fun (l1, l2) (a, _) ->
                       if get_key_index a = ip then
                         let l1 = if List.mem x1 l1 then l1 else x1 :: l1 in
                         let l2 = if List.mem x2 l2 then l2 else x2 :: l2 in
                         l1, l2
                       else l1, l2)
                    (l1, l2) list)
               ([], []) rl
           in
           Wserver.printf "&l1=";
           begin let _ =
             List.fold_right (fun x sep -> Wserver.printf "%s%d" sep x; ",")
               l1 ""
           in
             Wserver.printf "&l2=";
             let _ =
               List.fold_right (fun x sep -> Wserver.printf "%s%d" sep x; ",")
                 l2 ""
             in
             ()
           end;
           let image_opt =
             match p_getenv conf.env "image" with
               Some "on" -> "&image=on"
             | _ -> ""
           in
           let border =
             match p_getenv conf.env "bd" with
               Some "on" -> "&bd=on"
             | _ -> ""
           in
           Wserver.printf "&dag=on%s%s\">" image_opt border;
           if is_anc then Wserver.printf "%s" (transl conf "tree")
           else
             Wserver.printf "%d %s" nn
               (transl_nth conf "relationship link/relationship links" 1);
           Wserver.printf "</a>";
           if is_anc then () else Wserver.printf "\n</li>\n"
         else rest := true)
      anc_map;
    if !rest then
      begin
        Wserver.printf "<li>";
        Wserver.printf "...";
        Wserver.printf "</li>\n"
      end;
    if is_anc then Wserver.printf "\n" else Wserver.printf "</ul>\n"

let print_propose_upto conf base p1 p2 rl =
  match rl with
    (None, None, (x1, x2, _), _) :: _ when x1 = 0 || x2 = 0 ->
      let maxlen =
        List.fold_right
          (fun (_, _, (x1, x2, _), _) maxlen -> max maxlen (max x1 x2)) rl 0
      in
      let (p, a) = if x1 = 0 then p2, p1 else p1, p2 in
      Wserver.printf "<p>\n";
      Wserver.printf "<img src=\"%s/%s\" alt=\"\"%s>\n"
        (Util.image_prefix conf) "picto_fleche_bleu.png" conf.xhs;
      Wserver.printf "<span class=\"smaller\">";
      Wserver.printf "%s"
        (let s = person_title_text conf base p in
         capitale
           (translate_eval
              (transl_a_of_b conf (transl conf "ancestors") s s)));
      Wserver.printf " %s"
        (transl_decline conf "up to" (person_title_text conf base a));
      Wserver.printf "\n&nbsp;";
      Wserver.printf "<img src=\"%s/%s\" alt=\"\"%s>\n"
        (Util.image_prefix conf) "picto_rel_asc.png" conf.xhs;
      Wserver.printf "<a href=\"%sm=A&t=D&%s&%s&l=%d\">" (commd conf)
        (acces conf base p) (acces_n conf base "1" a) maxlen;
      Wserver.printf "%s" (capitale (transl conf "see"));
      Wserver.printf "</a>";
      Wserver.printf "</span>\n";
      Wserver.printf "</p>\n"
  | _ -> ()

let compute_simple_relationship conf base tstab ip1 ip2 =
  let tab = Consang.make_relationship_info base tstab in
  let (relationship, ancestors) =
    Consang.relationship_and_links base tab true ip1 ip2
  in
  if ancestors = [] then None
  else
    let total =
      try
        List.fold_left
          (fun n i ->
             let u = tab.Consang.reltab.(i) in
             List.fold_left
               (fun n (_, n1, _) ->
                  let n1 = if n1 < 0 then raise Exit else Sosa.of_int n1 in
                  List.fold_left
                    (fun n (_, n2, _) -> Sosa.add n (Sosa.mul n1 n2)) n
                    u.Consang.lens1)
               n u.Consang.lens2)
          Sosa.zero ancestors
      with Exit -> Sosa.zero
    in
    let rl =
      List.fold_left
        (fun rl i ->
           let u = tab.Consang.reltab.(i) in
           let p = pget conf base (Adef.iper_of_int i) in
           List.fold_left
             (fun rl (len1, n1, _) ->
                List.fold_left
                  (fun rl (len2, n2, _) ->
                     let n = n1 * n2 in
                     let n = if n1 < 0 || n2 < 0 || n < 0 then -1 else n in
                     (len1, len2, (p, n)) :: rl)
                  rl u.Consang.lens2)
             rl u.Consang.lens1)
        [] ancestors
    in
    let rl =
      List.sort
        (fun (len11, len12, _) (len21, len22, _) ->
           if len11 + len12 > len21 + len22 then -1
           else if len11 + len12 < len21 + len22 then 1
           else compare len21 len11)
        rl
    in
    let rl =
      List.fold_left
        (fun l (len1, len2, sol) ->
           match l with
             (l1, l2, sols) :: l when len1 = l1 && len2 = l2 ->
               (l1, l2, sol :: sols) :: l
           | _ -> (len1, len2, [sol]) :: l)
        [] rl
    in
    Some (rl, total, relationship, tab.Consang.reltab)

let known_spouses_list conf base p excl_p =
  let u = p in
  List.fold_left
    (fun spl ifam ->
       let sp = pget conf base (Gutil.spouse (get_key_index p) (foi base ifam)) in
       if sou base (get_first_name sp) <> "?" &&
          sou base (get_surname sp) <> "?" &&
          get_key_index sp <> get_key_index excl_p
       then
         sp :: spl
       else spl)
    [] (Array.to_list (get_family u))

let merge_relations rl1 rl2 =
  List.merge
    (fun (po11, po12, (l11, l12, _), _) (po21, po22, (l21, l22, _), _) ->
       if l11 + l12 < l21 + l22 then -1
       else if l11 + l12 > l21 + l22 then 1
       else if l11 < l21 then -1
       else if l11 > l21 then 1
       else if po11 = None && po12 = None then -1
       else if po21 = None && po22 = None then 1
       else if po11 = None || po21 = None then -1
       else if po21 = None || po22 = None then 1
       else -1)
    rl1 rl2

let combine_relationship conf base tstab pl1 pl2 f_sp1 f_sp2 sl =
  List.fold_right
    (fun p1 sl ->
       List.fold_right
         (fun p2 sl ->
            let sol =
              compute_simple_relationship conf base tstab (get_key_index p1)
                (get_key_index p2)
            in
            match sol with
              Some (rl, total, _, reltab) ->
                let s = List.map (fun r -> f_sp1 p1, f_sp2 p2, r) rl in
                (s, total, reltab) :: sl
            | None -> sl)
         pl2 sl)
    pl1 sl

let sp p = Some p
let no_sp _ = None

let compute_relationship conf base by_marr p1 p2 =
  let ip1 = get_key_index p1 in
  let ip2 = get_key_index p2 in
  if ip1 = ip2 then None
  else
    let tstab = Util.create_topological_sort conf base in
    let sol = compute_simple_relationship conf base tstab ip1 ip2 in
    let sol_by_marr =
      if by_marr then
        let spl1 = known_spouses_list conf base p1 p2 in
        let spl2 = known_spouses_list conf base p2 p1 in
        let sl = [] in
        let sl =
          match sol with
            Some ((_, 0, _) :: _, _, _, _) -> sl
          | _ -> combine_relationship conf base tstab [p1] spl2 no_sp sp sl
        in
        let sl =
          match sol with
            Some ((0, _, _) :: _, _, _, _) -> sl
          | _ -> combine_relationship conf base tstab spl1 [p2] sp no_sp sl
        in
        match sol, sl with
          Some ((x1, x2, _) :: _, _, _, _), _ when x1 = 0 || x2 = 0 -> sl
        | _, ((_, _, (x1, x2, _)) :: _, _, _) :: _ when x1 = 0 || x2 = 0 -> sl
        | _ -> combine_relationship conf base tstab spl1 spl2 sp sp sl
      else []
    in
    let (all_sol, rel) =
      match sol with
        Some (rl, total, rel, reltab) ->
          let s = List.map (fun r -> None, None, r) rl in
          (s, total, reltab) :: sol_by_marr, rel
      | None -> sol_by_marr, 0.0
    in
    let (sl, total) =
      List.fold_right
        (fun (rl1, total1, reltab) (rl, total) ->
           let rl1 =
             List.map (fun (po1, po2, list) -> po1, po2, list, reltab) rl1
           in
           merge_relations rl1 rl, Sosa.add total1 total)
        all_sol ([], Sosa.zero)
    in
    if sl = [] then None else Some (sl, total, rel)

let print_one_path conf base found a p1 p2 pp1 pp2 l1 l2 =
  let ip = get_key_index a in
  let sp1 =
    match pp1 with
      Some _ -> Some p1
    | _ -> None
  in
  let sp2 =
    match pp2 with
      Some _ -> Some p2
    | _ -> None
  in
  let p1 =
    match pp1 with
      Some p1 -> p1
    | _ -> p1
  in
  let p2 =
    match pp2 with
      Some p2 -> p2
    | _ -> p2
  in
  let ip1 = get_key_index p1 in
  let ip2 = get_key_index p2 in
  let dist = RelationLink.make_dist_tab conf base ip (max l1 l2 + 1) in
  let b1 = RelationLink.find_first_branch conf base dist ip l1 ip1 Neuter in
  let b2 = RelationLink.find_first_branch conf base dist ip l2 ip2 Neuter in
  match b1, b2 with
    Some b1, Some b2 ->
      let bd =
        match p_getint conf.env "bd" with
          Some x -> x
        | None -> 0
      in
      let td_prop =
        match Util.p_getenv conf.env "td" with
          Some x -> " " ^ x
        | _ ->
            match Util.p_getenv conf.env "color" with
              None | Some "" -> ""
            | Some x -> " class=\"" ^ x ^ "\""
      in
      let info =
        {RelationLink.ip = ip; sp = get_sex a; ip1 = ip1; ip2 = ip2; b1 = b1; b2 = b2;
         c1 = 1; c2 = 1; pb1 = None; pb2 = None; nb1 = None; nb2 = None;
         sp1 = sp1; sp2 = sp2; bd = bd; td_prop = td_prop}
      in
      if List.mem (b1, b2) !found then ()
      else
        begin
          Wserver.printf "<table width=\"100%%\"><tr><td align=\"center\">\n";
          begin
            Wserver.printf "<table>\n";
            begin
              Wserver.printf "<tr>\n";
              begin
                Wserver.printf "<td>\n";
                RelationLink.print_relation_path conf base info;
                Wserver.printf "</td>\n"
              end;
              Wserver.printf "</tr>\n"
            end;
            Wserver.printf "</table>\n"
          end;
          Wserver.printf "</td></tr></table>\n";
          found := (b1, b2) :: !found
        end
  | _ -> ()

let print_path conf base p1 p2 (pp1, pp2, (l1, l2, list), _) =
  let found = ref [] in
  List.iter (fun (a, _) -> print_one_path conf base found a p1 p2 pp1 pp2 l1 l2) list;
  Wserver.printf "\n"

let print_main_relationship conf base long p1 p2 rel =
  let total =
    match rel with
      None -> Sosa.zero
    | Some (_, total, _) -> total
  in
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "relationship"));
    if Sosa.eq total Sosa.zero then ()
    else
      begin
        Wserver.printf " (";
        wprint_num conf total;
        Wserver.printf " %s)"
          (transl_nth conf "relationship link/relationship links"
             (if Sosa.eq total Sosa.one then 0 else 1))
      end
  in
  let conf =
    if long then
      let doctype =
        match p_getenv conf.base_env "doctype" with
          Some ("html-4.01" | "html-4.01-trans") -> "html-4.01-trans"
        | _ -> "xhtml-1.0-trans"
      in
      {conf with base_env = ("doctype", doctype) :: conf.base_env}
    else conf
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  begin match p_getenv conf.env "spouse" with
    Some "on" -> conf.senv <- conf.senv @ ["spouse", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "cgl" with
    Some "on" -> conf.senv <- conf.senv @ ["cgl", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "bd" with
    None | Some ("0" | "") -> ()
  | Some x -> conf.senv <- conf.senv @ ["bd", x]
  end;
  begin match p_getenv conf.env "color" with
    None | Some "" -> ()
  | Some x -> conf.senv <- conf.senv @ ["color", code_varenv x]
  end;
  begin match rel with
    None ->
      if get_key_index p1 = get_key_index p2 then
        Wserver.printf "%s\n"
          (capitale (transl conf "it is the same person!"))
      else
        begin
          Wserver.printf "%s.\n"
            (capitale
               (cftransl conf "no known relationship link between %s and %s"
                  [gen_person_title_text reference raw_access conf base p1;
                   gen_person_title_text reference raw_access conf base p2]));
          Wserver.printf "<br%s>\n" conf.xhs;
          begin
            Wserver.printf "<p>\n";
            begin
              Wserver.printf "<span>";
              begin
                Wserver.printf "<a href=\"%s&m=R&%s\">" (commd conf)
                  (acces conf base p1);
                Wserver.printf "%s"
                  (capitale
                     (transl_nth conf "try another/relationship computing"
                        0));
                Wserver.printf "</a>"
              end;
              Wserver.printf " %s.\n"
                (transl_nth conf "try another/relationship computing" 1);
              Wserver.printf "</span>"
            end;
            Wserver.printf "</p>\n"
          end
        end
  | Some (rl, _, relationship) ->
      let a1 = p1 in
      let a2 = p2 in
      let all_by_marr =
        List.for_all
          (function
             Some _, _, _, _ | _, Some _, _, _ -> true
           | _ -> false)
          rl
      in
      let _ =
        List.fold_left
          (fun i sol ->
             print_solution conf base long i p1 p2 sol;
             if long then print_path conf base p1 p2 sol;
             succ i)
          1 rl
      in
      Wserver.printf "\n";
      if long then () else print_dag_links conf base p1 p2 rl;
      if not all_by_marr && authorized_age conf base p1 &&
         authorized_age conf base p2 && get_consang a1 != Adef.fix (-1) &&
         get_consang a2 != Adef.fix (-1)
      then
        begin
          Wserver.printf "<p>\n";
          Wserver.printf "<em>%s%s %s%%</em>"
            (capitale (transl conf "relationship"))
            (Util.transl conf ":")
            (string_of_decimal_num conf
               (round_2_dec
                  (Adef.float_of_fix (Adef.fix_of_float relationship) *.
                   100.0)));
          Wserver.printf "</p>\n"
        end;
      print_propose_upto conf base p1 p2 rl
  end;
  Hutil.trailer conf

let multi_relation_next_txt conf pl2 lim assoc_txt =
  match pl2 with
    [] -> ""
  | _ ->
      let sl = if lim > 0 then ["&lim="; string_of_int lim] else [] in
      let (sl, _) =
        List.fold_left
          (fun (sl, n) p ->
             let sl =
               try
                 let t = Hashtbl.find assoc_txt (get_key_index p) in
                 "&t" :: string_of_int n :: "=" :: t :: sl
               with Not_found -> sl
             in
             let sl =
               "&i" :: string_of_int n :: "=" ::
               string_of_int (Adef.int_of_iper (get_key_index p)) :: sl
             in
             sl, n - 1)
          (sl, List.length pl2) (List.rev pl2)
      in
      let sl = commd conf :: "m=RLM" :: sl in String.concat "" sl

let print_no_relationship conf base pl =
  let title _ = Wserver.printf "%s" (capitale (transl conf "tree")) in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  List.iter
    (fun p ->
       Wserver.printf "<li>%s\n" (referenced_person_title_text conf base p))
    pl;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let print_multi_relation conf base pl lim assoc_txt =
  let (pl1, pl2) =
    if lim <= 0 then pl, []
    else
      let rec loop n rev_pl1 pl2 =
        match n, pl2 with
          _, [] | _, [_] -> pl, []
        | 0, _ ->
            begin match rev_pl1 with
              p :: _ -> List.rev rev_pl1, p :: pl2
            | _ -> pl, []
            end
        | n, p :: pl -> loop (n - 1) (p :: rev_pl1) pl
      in
      loop lim [] pl
  in
  let path =
    let rec loop path =
      function
        p1 :: (p2 :: _ as pl) ->
          let ip1 = get_key_index p1 in
          let ip2 = get_key_index p2 in
          begin match get_shortest_path_relation conf base ip1 ip2 [] with
            Some (path1, _) ->
              let path =
                match path with
                  [] -> path1
                | _ ->
                    match List.rev path1 with
                      _ :: path1 -> List.rev path1 @ path
                    | [] -> path
              in
              loop path pl
          | None -> loop path pl
          end
      | [_] | [] -> path
    in
    loop [] pl1
  in
  if path = [] then print_no_relationship conf base pl
  else
    let elem_txt p =
      Dag.Item
        (p,
         (try
            let t = Hashtbl.find assoc_txt (get_key_index p) in
            "<b>(" ^ t ^ ")</b>"
          with Not_found -> ""))
    in
    let vbar_txt _ = "" in
    let next_txt = multi_relation_next_txt conf pl2 lim assoc_txt in
    print_relationship_dag conf base elem_txt vbar_txt path next_txt

let print_base_loop conf base p =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf
    (fcapitale (ftransl conf "loop in database: %s is his/her own ancestor"))
    (Util.update_family_loop conf base p (Gutil.designation base p));
  Wserver.printf ".\n";
  Hutil.trailer conf

let relmenu_print = Perso.interp_templ "relmenu"

let print conf base p =
  function
    Some p1 ->
      begin match p_getenv conf.env "et" with
        Some "S" -> print_shortest_path conf base p1 p
      | x ->
          let by_marr = x = Some "M" in
          let long =
            match p_getenv conf.env "long" with
              Some "on" -> true
            | _ -> false
          in
          match
            try Left (compute_relationship conf base by_marr p1 p) with
              Consang.TopologicalSortError p -> Right p
          with
            Left rel -> print_main_relationship conf base long p1 p rel
          | Right p -> print_base_loop conf base p
      end
  | None -> relmenu_print conf base p

let print_multi conf base =
  let assoc_txt = Hashtbl.create 53 in
  let pl =
    let rec loop pl i =
      let k = string_of_int i in
      match find_person_in_env conf base k with
        Some p ->
          begin match p_getenv conf.env ("t" ^ k) with
            Some x -> Hashtbl.add assoc_txt (get_key_index p) x
          | None -> ()
          end;
          loop (p :: pl) (i + 1)
      | None -> List.rev pl
    in
    loop [] 1
  in
  let lim =
    match p_getint conf.env "lim" with
      Some x -> x
    | None -> 0
  in
  print_multi_relation conf base pl lim assoc_txt
