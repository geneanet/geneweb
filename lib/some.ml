(* $Id: some.ml,v 5.44 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

module StrSet = Mutil.StrSet

let not_found conf txt x =
  let title _ = Wserver.printf "%s: \"%s\"" (capitale txt) x in
  Hutil.rheader conf title; Hutil.print_link_to_welcome conf false; Hutil.trailer conf

let first_name_not_found conf =
  not_found conf (transl conf "first name not found")

let surname_not_found conf = not_found conf (transl conf "surname not found")


(* **********************************************************************)
(*  [Fonc] print_branch_to_alphabetic : conf -> string -> int -> unit   *)
(** [Description] : A partir de l'affichage par branches, permet
                    d'afficher les liens pour un affichage par ordre
                    alphabétique.
    [Args] :
      - conf      : configuration de la base
      - base      : base
      - x         : 'nom/prénom/sosa...' recherché
      - nb_branch : nombre de branches dans le résultat de la recherche
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_branch_to_alphabetic conf x nb_branch =
  Wserver.printf "<table class=\"display_search\">\n";
  Wserver.printf "<tr>";
  Wserver.printf "<td>";
  Wserver.printf "<b>";
  Wserver.printf "%s"
    (capitale (transl_nth conf "display by/branch/alphabetic order" 0));
  Wserver.printf "</b>";
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf
    "<img src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\"%s>\n"
    (Util.image_prefix conf) "picto_branch.png" conf.xhs;
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf "%s (%d)"
    (transl_nth conf "display by/branch/alphabetic order" 1) nb_branch;
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf
    "<img src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\"%s>\n"
    (Util.image_prefix conf) "picto_alphabetic_order.png" conf.xhs;
  Wserver.printf "</td>";
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Wserver.printf "<td>";
  if p_getenv conf.env "t" = Some "A" then
    begin
      Wserver.printf "<a href=\"%sm=N&o=i&v=%s\" rel=\"nofollow\">"
        (commd conf) (code_varenv x ^ "&t=A");
      Wserver.printf "%s"
        (transl_nth conf "display by/branch/alphabetic order" 2);
      Wserver.printf "</a>"
    end
  else
    begin
      Wserver.printf "<a href=\"%sm=N&o=i&v=%s\" rel=\"nofollow\">"
        (commd conf) (code_varenv x ^ "&t=N");
      Wserver.printf "%s"
        (transl_nth conf "display by/branch/alphabetic order" 2);
      Wserver.printf "</a>"
    end;
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Wserver.printf "</td>";
  Wserver.printf "</tr>";
  Wserver.printf "</table>\n";
  Wserver.printf "<br%s>\n" conf.xhs


(* **********************************************************************)
(*  [Fonc] print_alphabetic_to_branch : conf -> string -> int -> unit   *)
(** [Description] : A partir de l'affichage alphabétique, permet
                    d'afficher les liens pour un affichage par branches.
    [Args] :
      - conf      : configuration de la base
      - base      : base
      - x         : 'nom/prénom/sosa...' recherché
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_alphabetic_to_branch conf x =
  Wserver.printf "<table class=\"display_search\">";
  Wserver.printf "<tr>";
  Wserver.printf "<td>";
  Wserver.printf "<b>";
  Wserver.printf "%s"
    (capitale (transl_nth conf "display by/branch/alphabetic order" 0));
  Wserver.printf "</b>";
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf
    "<img src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\"%s>\n"
    (Util.image_prefix conf) "picto_branch.png" conf.xhs;
  Wserver.printf "</td>";
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Wserver.printf "<td>";
  if p_getenv conf.env "t" = Some "A" then
    begin
      Wserver.printf "<a href=\"%sm=N&v=%s\" rel=\"nofollow\">" (commd conf)
        (code_varenv x ^ "&t=A");
      Wserver.printf "%s"
        (transl_nth conf "display by/branch/alphabetic order" 1);
      Wserver.printf "</a>"
    end
  else
    begin
      Wserver.printf "<a href=\"%sm=NG&sn=%s\" rel=\"nofollow\">" (commd conf)
        (code_varenv x);
      Wserver.printf "%s"
        (transl_nth conf "display by/branch/alphabetic order" 1);
      Wserver.printf "</a>"
    end;
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf
    "<img src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\"%s>\n"
    (Util.image_prefix conf) "picto_alphabetic_order.png" conf.xhs;
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf "%s"
    (transl_nth conf "display by/branch/alphabetic order" 2);
  Wserver.printf "</td>";
  Wserver.printf "</tr>";
  Wserver.printf "</table>";
  Wserver.printf "<br%s>\n" conf.xhs

let persons_of_fsname conf base base_strings_of_fsname find proj x =
  (* list of strings index corresponding to the crushed lower first name
     or surname "x" *)
  let istrl = base_strings_of_fsname base x in
  (* selecting the persons who have this first name or surname *)
  let l =
    let x = Name.crush_lower x in
    List.fold_right
      (fun istr l ->
         let str = Mutil.nominative (sou base istr) in
         if Name.crush_lower str = x ||
            List.mem x (List.map Name.crush_lower (Mutil.surnames_pieces str))
         then
           let iperl = find istr in
           (* maybe they are not the good ones because of changes in the
              database; checking... *)
           let iperl =
             List.fold_left
               (fun iperl iper ->
                  if eq_istr (proj (pget conf base iper)) istr then
                    iper :: iperl
                  else iperl)
               [] iperl
           in
           if iperl = [] then l else (str, istr, iperl) :: l
         else l)
      istrl []
  in
  let (l, name_inj) =
    let (l1, name_inj) =
      let x = Name.lower x in
      List.fold_right
        (fun (str, istr, iperl) l ->
           if x = Name.lower str then (str, istr, iperl) :: l else l)
        l [],
      Name.lower
    in
    let (l1, name_inj) =
      if l1 = [] then
        let x = Name.strip_lower x in
        List.fold_right
          (fun (str, istr, iperl) l ->
             if x = Name.strip_lower str then (str, istr, iperl) :: l else l)
          l [],
        Name.strip_lower
      else l1, name_inj
    in
    if l1 = [] then l, Name.crush_lower else l1, name_inj
  in
  l, name_inj

let print_elem conf base is_surname (p, xl) =
  Mutil.list_iter_first
    (fun first x ->
       let iper = get_key_index x in
       if not first then Wserver.printf "</li>\n<li>\n  ";
       Perso.print_sosa conf base x true;
       Wserver.printf "<a href=\"%s%s\" id=\"i%s\">" (commd conf)
         (acces conf base x) (string_of_int (Adef.int_of_iper iper));
       if is_surname then
         Wserver.printf "%s%s" (surname_without_particle base p) (surname_particle base p)
       else Wserver.printf "%s" (if p = "" then "?" else p);
       Wserver.printf "</a>";
       Wserver.printf "%s" (Date.short_dates_text conf base x);
       Wserver.printf "<em>";
       specify_homonymous conf base x true;
       Wserver.printf "</em>")
    xl

let first_char s =
  (* Si la personne n'a pas de prénom/nom, on renvoie '?' *)
  if s = "" then "?"
  else
    let len = Name.nbc s.[0] in
    if len < String.length s then String.sub s 0 len else s

let name_unaccent s =
  let rec copy i len =
    if i = String.length s then Buff.get len
    else
      let (t, j) = Name.unaccent_utf_8 false s i in copy j (Buff.mstore len t)
  in
  copy 0 0

let first_name_print_list conf base x1 xl liste =
  let liste =
    let l =
      List.sort
        (fun x1 x2 ->
           match Gutil.alphabetic (p_surname base x1) (p_surname base x2) with
             0 ->
               begin match
                 Adef.od_of_cdate (get_birth x1),
                 Adef.od_of_cdate (get_birth x2)
               with
                 Some d1, Some d2 ->
                   if CheckItem.strictly_after d1 d2 then -1 else 1
               | Some _, _ -> 1
               | _ -> -1
               end
           | n -> -n)
        liste
    in
    List.fold_left
      (fun l x ->
         let px = p_surname base x in
         match l with
           (p, l1) :: l when Gutil.alphabetic px p = 0 -> (p, x :: l1) :: l
         | _ -> (px, [x]) :: l)
      [] l
  in
  let title h =
    if h || p_getenv conf.env "t" = Some "A" then Wserver.printf "%s" x1
    else
      Mutil.list_iter_first
        (fun first x ->
           Wserver.printf "%s<a href=\"%sm=P&v=%s&t=A\">%s</a>"
             (if first then "" else ", ") (commd conf) (code_varenv x) x)
        (StrSet.elements xl)
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  let list =
    List.map
      (fun (sn, ipl) ->
         let txt = Util.surname_without_particle base sn ^ Util.surname_particle base sn in
         let ord = name_unaccent txt in ord, txt, ipl)
      liste
  in
  let list = List.sort compare list in
  print_alphab_list (fun (ord, _, _) -> first_char ord)
    (fun (_, txt, ipl) -> print_elem conf base true (txt, ipl)) list;
  Hutil.trailer conf

let select_first_name conf n list =
  let title _ =
    Wserver.printf "%s \"%s\" : %s"
      (capitale (transl_nth conf "first name/first names" 0)) n
      (transl conf "specify")
  in
  Hutil.header conf title;
  Wserver.printf "<ul>";
  List.iter
    (fun (sstr, (strl, _)) ->
       Wserver.printf "\n";
       html_li conf;
       Wserver.printf "<a href=\"%sm=P&v=%s\">" (commd conf)
         (code_varenv sstr);
       Mutil.list_iter_first
         (fun first str ->
            Wserver.printf "%s%s" (if first then "" else ", ") str)
         (StrSet.elements strl);
       Wserver.printf "</a>\n")
    list;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let rec merge_insert (sstr, (strl, iperl) as x) =
  function
    (sstr1, (strl1, iperl1) as y) :: l ->
      if sstr < sstr1 then x :: y :: l
      else if sstr > sstr1 then y :: merge_insert x l
      else (sstr, (StrSet.union strl strl1, iperl @ iperl1)) :: l
  | [] -> [x]

let persons_of_absolute_first_name conf base x =
  let istrl = base_strings_of_first_name base x in
  List.fold_right
    (fun istr l ->
       let str = sou base istr in
       if str = x then
         let iperl = spi_find (persons_of_first_name base) istr in
         let iperl =
           List.fold_left
             (fun iperl iper ->
                if eq_istr (get_first_name (pget conf base iper)) istr then
                  iper :: iperl
                else iperl)
             [] iperl
         in
         if iperl = [] then l else (str, istr, iperl) :: l
       else l)
    istrl []

let first_name_print conf base x =
  let (list, _) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_first_name conf base x,
      (fun _ -> raise (Match_failure ("src/some.ml", 347, 51)))
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 348, 29)))
    else
      persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  match list with
    [] -> first_name_not_found conf x
  | [_, (strl, iperl)] ->
      let iperl = List.sort_uniq compare iperl in
      let pl = List.map (pget conf base) iperl in
      let pl =
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || authorized_age conf base p then
               p :: pl
             else pl)
          pl []
      in
      first_name_print_list conf base x strl pl
  | _ -> select_first_name conf x list

let has_children_with_that_name conf base des name =
  let compare_name n1 n2 =
    if p_getenv conf.env "t" = Some "A" then n1 = n2
    else Name.lower n1 = Name.lower n2
  in
  List.exists
    (fun ip -> compare_name (p_surname base (pget conf base ip)) name)
    (Array.to_list (get_children des))

(* List selection bullets *)

let bullet_sel_txt = "<tt>o</tt>"
let bullet_unsel_txt = "<tt>+</tt>"
let bullet_nosel_txt = "<tt>o</tt>"
let print_selection_bullet conf =
  function
    Some (txt, sel) ->
      let req =
        List.fold_left
          (fun req (k, v) ->
             if not sel && k = "u" && v = txt then req
             else
               let s = k ^ "=" ^ v in if req = "" then s else req ^ "&" ^ s)
          "" conf.env
      in
      if conf.cancel_links then ()
      else
        Wserver.printf "<a id=\"i%s\" href=\"%s%s%s%s\" rel=\"nofollow\">" txt
          (commd conf) req (if sel then "&u=" ^ txt else "")
          (if sel || List.mem_assoc "u" conf.env then "#i" ^ txt else "");
      Wserver.printf "%s" (if sel then bullet_sel_txt else bullet_unsel_txt);
      if conf.cancel_links then () else Wserver.printf "</a>";
      Wserver.printf "\n"
  | None -> Wserver.printf "%s\n" bullet_nosel_txt

let unselected_bullets conf =
  List.fold_left
    (fun sl (k, v) ->
       try if k = "u" then int_of_string v :: sl else sl with Failure _ -> sl)
    [] conf.env

let alphabetic1 n1 n2 = Gutil.alphabetic_utf_8 n1 n2

type 'a branch_head = { bh_ancestor : 'a; bh_well_named_ancestors : 'a list }

let print_branch conf base psn name =
  let unsel_list = unselected_bullets conf in
  let rec loop is_first_level p =
    let u = pget conf base (get_key_index p) in
    let family_list =
      List.map
        (fun ifam ->
           let fam = foi base ifam in
           let c = Gutil.spouse (get_key_index p) fam in
           let c = pget conf base c in
           let down = has_children_with_that_name conf base fam name in
           let down =
             if get_sex p = Female && p_surname base c = name then false
             else down
           in
           let i = Adef.int_of_ifam ifam in
           let sel = not (List.mem i unsel_list) in
           fam, c, (if down then Some (string_of_int i, sel) else None))
        (Array.to_list (get_family u))
    in
    let first_select =
      match family_list with
        (_, _, select) :: _ -> select
      | _ -> None
    in
    print_selection_bullet conf first_select;
    Perso.print_sosa conf base p true;
    Wserver.printf "<strong>";
    Wserver.printf "%s"
      (Util.reference conf base p
         (if is_hide_names conf p && not (authorized_age conf base p) then "x"
          else if not psn && p_surname base p = name then
            person_text_without_surname conf base p
          else person_text conf base p));
    Wserver.printf "</strong>";
    Wserver.printf "%s" (Date.short_dates_text conf base p);
    Wserver.printf "\n";
    if Array.length (get_family u) = 0 then ()
    else
      let _ =
        List.fold_left
          (fun first (fam, c, select) ->
             if not first then
               begin
                 if is_first_level then Wserver.printf "<br%s>\n" conf.xhs
                 else Wserver.printf "</dd>\n<dd>\n";
                 print_selection_bullet conf select;
                 Perso.print_sosa conf base p false;
                 begin
                   Wserver.printf "<em>";
                   Wserver.printf "%s"
                     (if is_hide_names conf p &&
                         not (authorized_age conf base p)
                      then
                        "x"
                      else if not psn && p_surname base p = name then
                        person_text_without_surname conf base p
                      else person_text conf base p);
                   Wserver.printf "</em>"
                 end;
                 Wserver.printf "%s" (Date.short_dates_text conf base p);
                 Wserver.printf "\n"
               end;
             Wserver.printf "  &amp;";
             Wserver.printf "%s\n"
               (Date.short_marriage_date_text conf base fam p c);
             Perso.print_sosa conf base c true;
             Wserver.printf "<strong>";
             Wserver.printf "%s"
               (reference conf base c
                  (if is_hide_names conf c && not (authorized_age conf base c)
                   then
                     "x"
                   else person_text conf base c));
             Wserver.printf "</strong>";
             Wserver.printf "%s" (Date.short_dates_text conf base c);
             Wserver.printf "\n";
             let children = get_children fam in
             begin match select with
               Some (_, true) ->
                 Wserver.printf "<dl>\n";
                 List.iter
                   (fun e ->
                      Wserver.printf "<dd>\n";
                      loop false (pget conf base e);
                      Wserver.printf "</dd>\n")
                   (Array.to_list children);
                 Wserver.printf "</dl>\n"
             | Some (_, false) -> ()
             | None ->
                 if Array.length children <> 0 then
                   begin
                     Wserver.printf "<dl>";
                     begin
                       Wserver.printf "<dd>";
                       Wserver.printf "...";
                       Wserver.printf "</dd>"
                     end;
                     Wserver.printf "</dl>\n"
                   end
             end;
             false)
          true family_list
      in
      ()
  in
  loop

let print_one_branch conf base bh psn lev =
  let p = bh.bh_ancestor in
  if bh.bh_well_named_ancestors = []
  then let x = sou base (get_surname p) in print_branch conf base psn x lev p
  else begin
    if is_hidden p then Wserver.printf "&lt;&lt;"
    else wprint_geneweb_link conf (Util.acces conf base p) "&lt;&lt;" ;
    Wserver.printf "\n";
    List.iter
      (fun p ->
         let x = sou base (get_surname p) in
         Wserver.printf "<dl>\n";
         Wserver.printf "<dd>\n";
         print_branch conf base psn x false p;
         Wserver.printf "</dd>\n";
         Wserver.printf "</dl>\n")
      bh.bh_well_named_ancestors
  end

let print_one_surname_by_branch conf base x xl (bhl, str) =
  let ancestors =
    match p_getenv conf.env "order" with
      Some "d" ->
        let born_before p1 p2 =
          match
            Adef.od_of_cdate (get_birth p1), Adef.od_of_cdate (get_birth p2)
          with
            Some d1, Some d2 ->
              if CheckItem.strictly_after d2 d1 then -1 else 1
          | _, None -> -1
          | None, _ -> 1
        in
        List.sort (fun p1 p2 -> born_before p1.bh_ancestor p2.bh_ancestor) bhl
    | _ ->
        List.sort
          (fun p1 p2 ->
             alphabetic1 (p_first_name base p1.bh_ancestor)
               (p_first_name base p2.bh_ancestor))
          bhl
  in
  let len = List.length ancestors in
  let psn =
    match p_getenv conf.env "alwsurn" with
      Some x -> x = "yes"
    | None ->
        try List.assoc "always_surname" conf.base_env = "yes" with
          Not_found -> false
  in
  let title h =
    if h || p_getenv conf.env "t" = Some "A" then Wserver.printf "%s" x
    else
      Mutil.list_iter_first
        (fun first x ->
           Wserver.printf "%s<a href=\"%sm=N&v=%s&t=A\">%s</a>"
             (if first then "" else ", ") (commd conf) (code_varenv x) x)
        (StrSet.elements xl)
  in
  let br = p_getint conf.env "br" in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  (* Menu afficher par branche/ordre alphabetique *)
  if br = None then print_branch_to_alphabetic conf x len;
  Wserver.printf "<div style=\"white-space:nowrap\">\n";
  if len > 1 && br = None then
    begin
      Wserver.printf "<dl>\n";
      begin let _ =
        List.fold_left
          (fun n bh ->
             Wserver.printf "<dt>";
             if conf.cancel_links then Wserver.printf "%d." n
             else
               begin
                 Wserver.printf
                   "<a href=\"%sm=N&v=%s&br=%d\" rel=\"nofollow\">"
                   (commd conf) (Util.code_varenv str) n;
                 Wserver.printf "%d." n;
                 Wserver.printf "</a>"
               end;
             Wserver.printf "</dt>\n";
             Wserver.printf "<dd>\n";
             print_one_branch conf base bh psn false;
             Wserver.printf "</dd>\n";
             n + 1)
          1 ancestors
      in
        ()
      end;
      Wserver.printf "</dl>\n"
    end
  else
    begin let _ =
      List.fold_left
        (fun n bh ->
           if br = None || br = Some n then
             print_one_branch conf base bh psn true;
           n + 1)
        1 ancestors
    in
      ()
    end;
  Wserver.printf "</div>\n";
  Hutil.trailer conf

let print_several_possible_surnames x conf base (_, homonymes) =
  let fx = x in
  let x =
    match homonymes with
      x :: _ -> x
    | _ -> x
  in
  let title _ =
    Wserver.printf "%s \"%s\" : %s"
      (capitale (transl_nth conf "surname/surnames" 0)) fx
      (transl conf "specify")
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  let list =
    List.map
      (fun sn ->
         let txt = Util.surname_without_particle base sn ^ Util.surname_particle base sn in
         let ord = name_unaccent txt in ord, txt, sn)
      homonymes
  in
  let list = List.sort compare list in
  let access txt sn =
    geneweb_link conf ("m=N&v=" ^ code_varenv sn ^ "&t=N") txt
  in
  Util.wprint_in_columns conf (fun (ord, _, _) -> ord)
    (fun (_, txt, sn) -> Wserver.printf "%s" (access txt sn)) list;
  Wserver.printf "<p>\n";
  Wserver.printf "<em style=\"font-size:80%%\">\n";
  Wserver.printf "%s " (capitale (transl conf "click"));
  Wserver.printf "<a href=\"%sm=N&o=i&v=%s\">%s</a>\n" (commd conf)
    (if List.length homonymes = 1 then code_varenv x ^ "&t=A"
     else code_varenv fx)
    (transl conf "here");
  Wserver.printf "%s" (transl conf "for the first names by Gutil.alphabetic order");
  Wserver.printf ".</em>\n";
  Wserver.printf "</p>\n";
  Hutil.trailer conf

let print_family_alphabetic x conf base liste =
  let homonymes =
    let list =
      List.fold_left
        (fun list p ->
           if List.exists (eq_istr (get_surname p)) list then list
           else get_surname p :: list)
        [] liste
    in
    let set =
      List.fold_left (fun set istr -> StrSet.add (sou base istr) set)
        StrSet.empty list
    in
    List.sort compare (StrSet.elements set)
  in
  let liste =
    let l =
      List.sort
        (fun x1 x2 ->
           match
             alphabetic1 (p_first_name base x2) (p_first_name base x1)
           with
             0 -> compare (get_occ x1) (get_occ x2)
           | n -> n)
        liste
    in
    List.fold_left
      (fun l x ->
         let px = p_first_name base x in
         match l with
           (p, l1) :: l when alphabetic1 px p = 0 -> (p, x :: l1) :: l
         | _ -> (px, [x]) :: l)
      [] l
  in
  match liste with
    [] -> surname_not_found conf x
  | _ ->
      let title h =
        let access x =
          if h || List.length homonymes = 1 then x
          else geneweb_link conf ("m=N&o=i&v=" ^ code_varenv x ^ "&t=A") x
        in
        Mutil.list_iter_first
          (fun first x ->
             Wserver.printf "%s%s" (if first then "" else ", ") (access x))
          homonymes
      in
      Hutil.header conf title;
      Hutil.print_link_to_welcome conf true;
      (* Si on est dans un calcul de parenté, on affiche *)
      (* l'aide sur la sélection d'un individu.          *)
      Util.print_tips_relationship conf;
      (* Menu afficher par branche/ordre alphabetique *)
      print_alphabetic_to_branch conf x;
      print_alphab_list (fun (p, _) -> first_char p)
        (print_elem conf base false) liste;
      Hutil.trailer conf

let insert_at_position_in_family children ip ipl =
  let rec loop child_list ipl =
    match child_list, ipl with
      ip1 :: ipl1, ip2 :: ipl2 ->
        if ip1 = ip2 then if ip = ip1 then ipl else ip2 :: loop ipl1 ipl2
        else if ip = ip1 then ip1 :: ipl
        else loop ipl1 ipl
    | _ :: _, [] -> [ip]
    | [], _ -> assert false
  in
  loop (Array.to_list children) ipl

let select_ancestors conf base name_inj ipl =
  let str_inj s = name_inj (sou base s) in
  List.fold_left
    (fun bhl ip ->
       let p = pget conf base ip in
       match get_parents p with
         Some ifam ->
           let fam = foi base ifam in
           let ifath = get_father fam in
           let imoth = get_mother fam in
           let fath = pget conf base ifath in
           let moth = pget conf base imoth in
           let s = str_inj (get_surname p) in
           if str_inj (get_surname fath) <> s &&
              str_inj (get_surname moth) <> s
           then
             let rec loop =
               function
                 bh :: bhl ->
                   if bh.bh_ancestor = ifath || bh.bh_ancestor = imoth then
                     let bh =
                       {bh with bh_well_named_ancestors =
                         insert_at_position_in_family (get_children fam) ip
                           bh.bh_well_named_ancestors}
                     in
                     bh :: bhl
                   else bh :: loop bhl
               | [] -> [{bh_ancestor = ifath; bh_well_named_ancestors = [ip]}]
             in
             loop bhl
           else bhl
       | _ ->
           let bh = {bh_ancestor = ip; bh_well_named_ancestors = []} in
           bh :: bhl)
    [] ipl

let persons_of_absolute_surname conf base x =
  let istrl = base_strings_of_surname base x in
  List.fold_right
    (fun istr l ->
       let str = sou base istr in
       if str = x then
         let iperl = spi_find (persons_of_surname base) istr in
         let iperl =
           List.fold_left
             (fun iperl iper ->
                if eq_istr (get_surname (pget conf base iper)) istr then
                  iper :: iperl
                else iperl)
             [] iperl
         in
         if iperl = [] then l else (str, istr, iperl) :: l
       else l)
    istrl []

module PerSet = Set.Make (struct type t = iper let compare = compare end)

let surname_print conf base not_found_fun x =
  let (list, name_inj) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_surname conf base x, (fun x -> x)
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 825, 29)))
    else
      persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let (iperl, _) =
    List.fold_right
      (fun (str, (_, iperl1)) (iperl, strl) ->
         let len = List.length iperl1 in
         let strl =
           try
             let len1 = List.assoc str strl in
             (str, len + len1) :: List.remove_assoc str strl
           with Not_found -> (str, len) :: strl
         in
         List.fold_right PerSet.add iperl1 iperl, strl)
      list (PerSet.empty, [])
  in
  let iperl = PerSet.elements iperl in
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  match p_getenv conf.env "o" with
    Some "i" ->
      let pl =
        List.fold_right (fun ip ipl -> pget conf base ip :: ipl) iperl []
      in
      let pl =
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || authorized_age conf base p then
               p :: pl
             else pl)
          pl []
      in
      print_family_alphabetic x conf base pl
  | _ ->
      let bhl = select_ancestors conf base name_inj iperl in
      let bhl =
        List.map
          (fun bh ->
             {bh_ancestor = pget conf base bh.bh_ancestor;
              bh_well_named_ancestors =
                List.map (pget conf base) bh.bh_well_named_ancestors})
          bhl
      in
      match bhl, list with
        [], _ -> not_found_fun conf x
      | _, [s, (strl, _)] ->
          print_one_surname_by_branch conf base x strl (bhl, s)
      | _ ->
          let strl = List.map fst list in
          print_several_possible_surnames x conf base (bhl, strl)


(**/**)
(* TODO: refactoring avec les fonctions ci-dessus !!! *)


let search_surname conf base x =
  let (list, name_inj) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_surname conf base x, (fun x -> x)
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 896, 29)))
    else
      persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let (iperl, _) =
    List.fold_right
      (fun (str, (_, iperl1)) (iperl, strl) ->
         let len = List.length iperl1 in
         let strl =
           try
             let len1 = List.assoc str strl in
             (str, len + len1) :: List.remove_assoc str strl
           with Not_found -> (str, len) :: strl
         in
         List.fold_right PerSet.add iperl1 iperl, strl)
      list (PerSet.empty, [])
  in
  let iperl = PerSet.elements iperl in
  let bhl = select_ancestors conf base name_inj iperl in
  let bhl =
    List.map
      (fun bh ->
         {bh_ancestor = pget conf base bh.bh_ancestor;
          bh_well_named_ancestors =
            List.map (pget conf base) bh.bh_well_named_ancestors})
      bhl
  in
  match bhl, list with
    [], _ -> []
  | _, [_, (_, iperl)] -> iperl
  | _ -> []

let search_surname_print conf base not_found_fun x =
  let (list, name_inj) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_surname conf base x, (fun x -> x)
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 942, 29)))
    else
      persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let (iperl, _) =
    List.fold_right
      (fun (str, (_, iperl1)) (iperl, strl) ->
         let len = List.length iperl1 in
         let strl =
           try
             let len1 = List.assoc str strl in
             (str, len + len1) :: List.remove_assoc str strl
           with Not_found -> (str, len) :: strl
         in
         List.fold_right PerSet.add iperl1 iperl, strl)
      list (PerSet.empty, [])
  in
  let iperl = PerSet.elements iperl in
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  match p_getenv conf.env "o" with
    Some "i" ->
      let pl =
        List.fold_right (fun ip ipl -> pget conf base ip :: ipl) iperl []
      in
      let pl =
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || authorized_age conf base p then
               p :: pl
             else pl)
          pl []
      in
      print_family_alphabetic x conf base pl
  | _ ->
      let bhl = select_ancestors conf base name_inj iperl in
      let bhl =
        List.map
          (fun bh ->
             {bh_ancestor = pget conf base bh.bh_ancestor;
              bh_well_named_ancestors =
                List.map (pget conf base) bh.bh_well_named_ancestors})
          bhl
      in
      match bhl, list with
        [], _ -> not_found_fun conf x
      | _, [s, (strl, _)] ->
          print_one_surname_by_branch conf base x strl (bhl, s)
      | _ ->
          let strl = List.map (fun (s, _) -> s) list in
          print_several_possible_surnames x conf base (bhl, strl)

let search_first_name conf base x =
  let (list, _) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_first_name conf base x,
      (fun _ -> raise (Match_failure ("src/some.ml", 1007, 51)))
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 1008, 29)))
    else
      persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  List.fold_right merge_insert list []

let search_first_name_print conf base x =
  let (list, _) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_first_name conf base x,
      (fun _ -> raise (Match_failure ("src/some.ml", 1025, 51)))
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 1026, 29)))
    else
      persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  match list with
    [] -> first_name_not_found conf x
  | [_, (strl, iperl)] ->
      let iperl = List.sort_uniq compare iperl in
      let pl = List.map (pget conf base) iperl in
      let pl =
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || authorized_age conf base p then
               p :: pl
             else pl)
          pl []
      in
      first_name_print_list conf base x strl pl
  | _ -> select_first_name conf x list
