(* $Id: updateFamOk.ml,v 5.53 2008-01-08 02:08:00 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

(* Liste des string dont on a supprimé un caractère.       *)
(* Utilisé pour le message d'erreur lors de la validation. *)
let removed_string = ref []

type create_info =
  Update.create_info =
    { ci_birth_date : date option;
      ci_birth_place : string;
      ci_death : death;
      ci_death_date : date option;
      ci_death_place : string;
      ci_occupation : string;
      ci_public : bool }

let raw_get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get_nth conf key cnt = p_getenv conf.env (key ^ string_of_int cnt)

let getn conf var key =
  match p_getenv conf.env (var ^ "_" ^ key) with
    Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound")

let reconstitute_somebody conf var =
  let first_name = no_html_tags (only_printable (getn conf var "fn")) in
  let surname = no_html_tags (only_printable (getn conf var "sn")) in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if List.exists contain_fn Name.forbidden_char ||
       List.exists contain_sn Name.forbidden_char
    then
      begin
        removed_string :=
          (Name.purge first_name ^ " " ^ Name.purge surname) ::
          !removed_string;
        Name.purge first_name, Name.purge surname
      end
    else first_name, surname
  in
  let occ = try int_of_string (getn conf var "occ") with Failure _ -> 0 in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
      Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter
  in
  let create =
    match getn conf var "p" with
      "create" -> Update.Create (sex, None)
    | _ -> Update.Link
  in
  first_name, surname, occ, create, var

let reconstitute_parent_or_child conf var default_surname =
  let first_name = no_html_tags (only_printable (getn conf var "fn")) in
  let surname =
    let surname = no_html_tags (only_printable (getn conf var "sn")) in
    if surname = "" then default_surname else surname
  in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if List.exists contain_fn Name.forbidden_char ||
       List.exists contain_sn Name.forbidden_char
    then
      begin
        removed_string :=
          (Name.purge first_name ^ " " ^ Name.purge surname) ::
          !removed_string;
        Name.purge first_name, Name.purge surname
      end
    else first_name, surname
  in
  let occ = try int_of_string (getn conf var "occ") with Failure _ -> 0 in
  let create_info =
    let b = Update.reconstitute_date conf (var ^ "b") in
    let bpl = getn conf (var ^ "b") "pl" in
    let death =
      match p_getenv conf.env (var ^ "d_yyyy") with
        Some "+" -> DeadDontKnowWhen
      | Some ("-" | "=") -> NotDead
      | _ -> DontKnowIfDead
    in
    let d = Update.reconstitute_date conf (var ^ "d") in
    let dpl = getn conf (var ^ "d") "pl" in
    let occupation = only_printable (getn conf var "occupation") in
    let public = getn conf (var ^ "b") "yyyy" = "p" in
    {ci_birth_date = b; ci_birth_place = bpl; ci_death = death;
     ci_death_date = d; ci_death_place = dpl; ci_occupation = occupation;
     ci_public = public}
  in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
      Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter
  in
  let create =
    match getn conf var "p" with
      "create" -> Update.Create (sex, Some create_info)
    | _ -> Update.Link
  in
  first_name, surname, occ, create, var

let invert_children conf (c, children, ext) i =
  let var = "inv_ch" ^ string_of_int (i + 1) in
  match p_getenv conf.env var, children with
    Some "on", c1 :: children -> c1, c :: children, true
  | _ -> c, children, ext

let insert_child conf (children, ext) i =
  let var = "ins_ch" ^ string_of_int i in
  match p_getenv conf.env var, p_getint conf.env (var ^ "_n") with
    _, Some n when n > 1 ->
      let children =
        let rec loop children n =
          if n > 0 then
            let new_child = "", "", 0, Update.Create (Neuter, None), "" in
            loop (new_child :: children) (n - 1)
          else children
        in
        loop children n
      in
      children, true
  | Some "on", _ ->
      let new_child = "", "", 0, Update.Create (Neuter, None), "" in
      new_child :: children, true
  | _ -> children, ext

let insert_parent conf (parents, ext) i =
  let var = "ins_pa" ^ string_of_int i in
  match p_getenv conf.env var, p_getint conf.env (var ^ "_n") with
    _, Some n when n > 1 ->
      let parents =
        let rec loop parents n =
          if n > 0 then
            let new_parent = "", "", 0, Update.Create (Neuter, None), "" in
            loop (new_parent :: parents) (n - 1)
          else parents
        in
        loop parents n
      in
      parents, true
  | Some "on", _ ->
      let new_parent = "", "", 0, Update.Create (Neuter, None), "" in
      new_parent :: parents, true
  | _ -> parents, ext

let reconstitute_insert_event conf ext cnt el =
  let var = "ins_event" ^ string_of_int cnt in
  let n =
    match p_getenv conf.env var, p_getint conf.env (var ^ "_n") with
      _, Some n when n > 1 -> n
    | Some "on", _ -> 1
    | _ -> 0
  in
  if n > 0 then
    let el =
      let rec loop el n =
        if n > 0 then
          let e1 =
            {efam_name = Efam_Name ""; efam_date = Adef.cdate_None;
             efam_place = ""; efam_reason = ""; efam_note = ""; efam_src = "";
             efam_witnesses = [| |]}
          in
          loop (e1 :: el) (n - 1)
        else el
      in
      loop el n
    in
    el, true
  else el, ext

let rec reconstitute_events conf ext cnt =
  match get_nth conf "e_name" cnt with
    Some efam_name ->
      let efam_name =
        match efam_name with
          "#marr" -> Efam_Marriage
        | "#nmar" -> Efam_NoMarriage
        | "#nmen" -> Efam_NoMention
        | "#enga" -> Efam_Engage
        | "#div" -> Efam_Divorce
        | "#sep" -> Efam_Separated
        | "#anul" -> Efam_Annulation
        | "#marb" -> Efam_MarriageBann
        | "#marc" -> Efam_MarriageContract
        | "#marl" -> Efam_MarriageLicense
        | "#pacs" -> Efam_PACS
        | "#resi" -> Efam_Residence
        | n -> Efam_Name (no_html_tags (only_printable n))
      in
      let efam_date =
        Update.reconstitute_date conf ("e_date" ^ string_of_int cnt)
      in
      let efam_place =
        match get_nth conf "e_place" cnt with
          Some place -> no_html_tags (only_printable place)
        | _ -> ""
      in
      let efam_note =
        match get_nth conf "e_note" cnt with
          Some note -> only_printable_or_nl (Mutil.strip_all_trailing_spaces note)
        | _ -> ""
      in
      let efam_src =
        match get_nth conf "e_src" cnt with
          Some src -> only_printable src
        | _ -> ""
      in
      let (witnesses, ext) =
        let rec loop i ext =
          match
            try
              Some
                (reconstitute_somebody conf
                   ("e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i))
            with Failure _ -> None
          with
            Some c ->
              let (witnesses, ext) = loop (i + 1) ext in
              let c =
                match
                  p_getenv conf.env
                    ("e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i ^
                     "_kind")
                with
                  Some "godp" -> c, Witness_GodParent
                | _ -> c, Witness
              in
              begin match
                p_getenv conf.env
                  ("e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i)
              with
                Some "on" ->
                  let ins_witn_n =
                    "e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i ^
                    "_n"
                  in
                  begin match p_getint conf.env ins_witn_n with
                    Some n when n > 1 ->
                      let rec loop_witn n witnesses =
                        if n = 0 then c :: witnesses, true
                        else
                          let new_witn =
                            ("", "", 0, Update.Create (Neuter, None), ""),
                            Witness
                          in
                          let witnesses = new_witn :: witnesses in
                          loop_witn (n - 1) witnesses
                      in
                      loop_witn n witnesses
                  | _ ->
                      let new_witn =
                        ("", "", 0, Update.Create (Neuter, None), ""), Witness
                      in
                      c :: new_witn :: witnesses, true
                  end
              | _ -> c :: witnesses, ext
              end
          | None -> [], ext
        in
        loop 1 ext
      in
      let (witnesses, ext) =
        let evt_ins = "e" ^ string_of_int cnt ^ "_ins_witn0" in
        match p_getenv conf.env evt_ins with
          Some "on" ->
            let ins_witn_n = "e" ^ string_of_int cnt ^ "_ins_witn0_n" in
            begin match p_getint conf.env ins_witn_n with
              Some n when n > 1 ->
                let rec loop_witn n witnesses =
                  if n = 0 then witnesses, true
                  else
                    let new_witn =
                      ("", "", 0, Update.Create (Neuter, None), ""), Witness
                    in
                    let witnesses = new_witn :: witnesses in
                    loop_witn (n - 1) witnesses
                in
                loop_witn n witnesses
            | _ ->
                let new_witn =
                  ("", "", 0, Update.Create (Neuter, None), ""), Witness
                in
                new_witn :: witnesses, true
            end
        | _ -> witnesses, ext
      in
      let e =
        {efam_name = efam_name; efam_date = Adef.cdate_of_od efam_date;
         efam_place = efam_place; efam_reason = ""; efam_note = efam_note;
         efam_src = efam_src; efam_witnesses = Array.of_list witnesses}
      in
      let (el, ext) = reconstitute_events conf ext (cnt + 1) in
      let (el, ext) = reconstitute_insert_event conf ext cnt el in
      e :: el, ext
  | _ -> [], ext

let rec reconstitute_sorted_fevents conf cnt =
  match get_nth conf "e_id" cnt, get_nth conf "e_pos" cnt with
    Some id, Some pos ->
      let (id, pos) =
        try int_of_string id, int_of_string pos with Failure _ -> 0, 0
      in
      let el = reconstitute_sorted_fevents conf (cnt + 1) in (id, pos) :: el
  | _ -> []

let reconstitute_from_fevents nsck empty_string fevents =
  (* On tri les évènements pour être sûr. *)
  let fevents =
    CheckItem.sort_events
      (fun evt -> CheckItem.Fsort evt.efam_name) (fun evt -> evt.efam_date)
      fevents
  in
  let found_marriage = ref None in
  let found_divorce = ref None in
  let mk_marr evt kind =
    let e = Some (kind, evt.efam_date, evt.efam_place, evt.efam_note, evt.efam_src, evt.efam_witnesses) in
    match !found_marriage with
    | None -> found_marriage := e
    | Some ((NoMention|Residence), _, _, _, _, _)
      when kind <> NoMention && kind <> Residence -> found_marriage := e
    | Some (Married, _, _, _, _, _) -> ()
    | _ -> if kind = Married then found_marriage := e
  in
  let mk_div kind =
    match !found_divorce with
    | None -> found_divorce := Some kind
    | Some _ -> ()
  in
  (* Marriage is more important than any other relation.
     For other cases, latest event is the most important,
     except for NotMention and Residence. *)
  (* FIXME: For now, we ignore Annulation since it gives a wrong date
     (relation on [annulation date] makes no sense) *)
  let rec loop = function
    | [] -> ()
    | evt :: l -> match evt.efam_name with
      | Efam_Engage -> mk_marr evt Engaged ; loop l
      | Efam_Marriage -> mk_marr evt Married
      | Efam_MarriageContract -> mk_marr evt MarriageContract ; loop l
      | Efam_NoMention -> mk_marr evt NoMention ; loop l
      | Efam_MarriageBann -> mk_marr evt MarriageBann ; loop l
      | Efam_MarriageLicense -> mk_marr evt MarriageLicense ; loop l
      | Efam_PACS -> mk_marr evt Pacs ; loop l
      | Efam_Residence -> mk_marr evt Residence ; loop l
      | Efam_NoMarriage -> mk_marr evt NotMarried ; loop l
      | Efam_Divorce -> mk_div (Divorced evt.efam_date) ; loop l
      | Efam_Separated -> mk_div Separated ; loop l
      | Efam_Annulation -> loop l
      | Efam_Name _ -> loop l
  in
  loop (List.rev fevents) ;
  (* Il faut gérer le cas où l'on supprime délibérément l'évènement. *)
  let marr, wit = match !found_marriage with
    | None -> (NoMention, Adef.cdate_None, empty_string, empty_string, empty_string), [||]
    | Some (kind, date, place, note, src, wit) -> ((kind, date, place, note, src), wit)
  in
  (* Parents de même sexe. *)
  let marr =
    if nsck then
      let (relation, date, place, note, src) = marr in
      let relation =
        match relation with
        | Married -> NoSexesCheckMarried
        | x -> x
      in
      relation, date, place, note, src
    else marr
  in
  let div = Opt.default NotDivorced !found_divorce in
  marr, div, wit

let reconstitute_family conf base =
  let (events, ext) = reconstitute_events conf false 1 in
  let (events, ext) = reconstitute_insert_event conf ext 0 events in
  let surname = getn conf "pa1" "sn" in
  let (children, ext) =
    let rec loop i ext =
      match
        try
          Some
            (reconstitute_parent_or_child conf ("ch" ^ string_of_int i)
               surname)
        with Failure _ -> None
      with
        Some c ->
          let (children, ext) = loop (i + 1) ext in
          let (c, children, ext) =
            invert_children conf (c, children, ext) i
          in
          let (children, ext) = insert_child conf (children, ext) i in
          c :: children, ext
      | None -> [], ext
    in
    loop 1 ext
  in
  let (children, ext) = insert_child conf (children, ext) 0 in
  let (parents, ext) =
    let rec loop i ext =
      match
        try
          Some (reconstitute_parent_or_child conf ("pa" ^ string_of_int i) "")
        with Failure _ -> None
      with
        Some c ->
          let (parents, ext) = loop (i + 1) ext in
          let (parents, ext) = insert_parent conf (parents, ext) i in
          c :: parents, ext
      | None -> [], ext
    in
    loop 1 ext
  in
  let comment =
    only_printable_or_nl (Mutil.strip_all_trailing_spaces (get conf "comment"))
  in
  let fsources = only_printable (get conf "src") in
  let origin_file =
    match p_getenv conf.env "origin_file" with
      Some x -> x
    | None -> ""
  in
  let fam_index =
    match p_getenv conf.env "i" with
      Some i -> Gwdb.ifam_of_string i
    | None -> Gwdb.dummy_ifam
  in
  (* Mise à jour des évènements principaux. *)
  (* Attention, dans le cas où fevent est vide, i.e. on a valider   *)
  (* avec un texte vide, par exemple lors de l'ajout d'une famille, *)
  (* il faut ajouter un evenement no_mention.                       *)
  let events =
    if events = [] then
      let evt =
        {efam_name = Efam_NoMention; efam_date = Adef.cdate_None;
         efam_place = ""; efam_reason = ""; efam_note = ""; efam_src = "";
         efam_witnesses = [| |]}
      in
      [evt]
    else events
  in
  (* Attention, surtout pas les witnesses, parce que si on en créé un, *)
  (* on le créé aussi dans witness et on ne pourra jamais valider.     *)
  let (marr, div, _) =
    (* FIXME: Use witnesses (and Array.map fst witnesses)
       when witnesses will be added inplace *)
    reconstitute_from_fevents (p_getenv conf.env "nsck" = Some "on") "" events
  in
  let (relation, marriage, marriage_place, marriage_note, marriage_src) =
    marr
  in
  (* Si parents de même sex ... Pas de mode multi parent. *)
  let relation =
    match parents with
      [father; mother] ->
        let father_sex =
          match father with
            _, _, _, Update.Create (sex, _), _ -> sex
          | f, s, o, Update.Link, _ ->
              match person_of_key base f s o with
                Some ip -> get_sex (poi base ip)
              | _ -> Neuter
        in
        let mother_sex =
          match mother with
            _, _, _, Update.Create (sex, _), _ -> sex
          | f, s, o, Update.Link, _ ->
              match person_of_key base f s o with
                Some ip -> get_sex (poi base ip)
              | _ -> Neuter
        in
        begin match father_sex, mother_sex with
          Male, Male | Female, Female ->
            begin match relation with
              Married -> NoSexesCheckMarried
            | _ -> NoSexesCheckNotMarried
            end
        | _ -> relation
        end
    | _ -> relation
  in
  let divorce = div in
  let fam =
    {marriage = marriage; marriage_place = marriage_place;
     marriage_note = marriage_note; marriage_src = marriage_src;
     witnesses = [||]; relation = relation;
     divorce = divorce; fevents = events; comment = comment;
     origin_file = origin_file; fsources = fsources;
     fam_index = fam_index}
  and cpl = Futil.parent conf.multi_parents (Array.of_list parents)
  and des = {children = Array.of_list children} in
  fam, cpl, des, ext

let strip_events fevents =
  let strip_array_witness pl =
    Array.of_list @@
    Array.fold_right
      (fun ((f, _, _, _, _), _ as p) pl -> if f = "" then pl else p :: pl)
      pl []
  in
  List.fold_right
    (fun e accu ->
       let has_name =
         match e.efam_name with
           Efam_Name s -> s <> ""
         | _ -> true
       in
       if has_name then
         let witnesses = strip_array_witness e.efam_witnesses in
         {e with efam_witnesses = witnesses} :: accu
       else accu)
    fevents []

let strip_array_persons pl =
  Array.of_list @@
  Array.fold_right
    (fun (f, _, _, _, _ as p) pl -> if f = "" then pl else p :: pl)
    pl []

let error_family conf err =
  let err' =
    Printf.sprintf "%s%s%s"
      (capitale (transl conf "error"))
      (transl conf ":")
      err
  in
#ifdef API
  if not !Api_conf.mode_api then begin
#endif
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s\n" (capitale err);
  Update.print_return conf;
  Hutil.trailer conf;
#ifdef API
    end;
#endif
  raise @@ Update.ModErr err'

let check_event_witnesses conf witnesses =
  let len = Array.length witnesses in
  let rec loop i =
    if i < len then begin
      let ((fn, sn, _, _, _), _) = Array.unsafe_get witnesses i in
      if fn = "" && sn = "" then loop (i + 1)
      else if fn = "" || fn = "?" then
        Some
          (transl_nth conf "witness/witnesses" 0 ^ " : " ^
           transl conf "first name missing")
      else if sn = "" || sn = "?" then
        Some
          (transl_nth conf "witness/witnesses" 0 ^ " : " ^
           transl conf "surname missing")
      else loop (i + 1)
    end
    else None
  in
  loop 0

let check_parents conf cpl =
  let (fa_fn, fa_sn, _, _, _) = Gutil.father cpl in
  let (mo_fn, mo_sn, _, _, _) = Gutil.mother cpl in
  match (fa_fn = "", fa_sn = ""), (mo_fn = "", mo_sn = "") with
    (true, true), (true, true) | (true, true), (false, false) |
    (false, false), (true, true) | (false, false), (false, false) ->
      None
  | (false, true), _ ->
      Some
        (transl_nth conf "father/mother" 0 ^ " : " ^
         transl conf "surname missing")
  | (true, false), _ ->
      Some
        (transl_nth conf "father/mother" 0 ^ " : " ^
         transl conf "first name missing")
  | _, (false, true) ->
      Some
        (transl_nth conf "father/mother" 1 ^ " : " ^
         transl conf "surname missing")
  | _, (true, false) ->
      Some
        (transl_nth conf "father/mother" 1 ^ " : " ^
         transl conf "first name missing")

let check_family conf fam cpl =
  let err_parents = check_parents conf cpl in
  let err_fevent_witness =
    (* On regarde si les témoins sont bien renseignés. *)
    let rec loop fevents =
      match fevents with
        [] -> None
      | evt :: l ->
          match check_event_witnesses conf evt.efam_witnesses with
            Some err -> Some err
          | _ -> loop l
    in
    loop fam.fevents
  in
  err_fevent_witness, err_parents

let strip_family fam des =
  let fam =
    {fam with witnesses = strip_array_persons fam.witnesses;
     fevents = strip_events fam.fevents}
  in
  let des = {children = strip_array_persons des.children} in fam, des

let print_err_parents conf base p =
  let err =
    Printf.sprintf (fcapitale (ftransl conf "%t already has parents"))
      (fun _ -> Printf.sprintf "\n%s" (referenced_person_text conf base p))
  in
#ifdef API
  if not !Api_conf.mode_api then begin
#endif
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "\n%s<p><ul><li>%s%s %d</li></ul>"
    err
    (capitale (transl conf "first free number"))
    (Util.transl conf ":")
    (Gutil.find_free_occ base (p_first_name base p) (p_surname base p));
  Update.print_return conf;
  Hutil.trailer conf;
#ifdef API
    end;
#endif
  raise @@ Update.ModErr err

let print_err_sex conf base p err =
  let err =
    Printf.sprintf "\n%s\n%s\n" (referenced_person_text conf base p) err
  in
#ifdef API
  if not !Api_conf.mode_api then begin
#endif
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Wserver.printf "%s" err ;
  Update.print_return conf;
  Hutil.trailer conf;
#ifdef API
  end ;
#endif
  raise @@ Update.ModErr err

let print_err_father_sex conf base p =
  print_err_sex conf base p (transl conf "should be male")

let print_err_mother_sex conf base p =
  print_err_sex conf base p (transl conf "should be female")

let print_err conf =
  let err = Printf.sprintf "%s" (capitale (transl conf "error")) in
#ifdef API
  if not !Api_conf.mode_api then begin
#endif
  let title _ = Wserver.printf "%s" err in
  Hutil.rheader conf title;
  Update.print_return conf;
  Hutil.trailer conf;
#ifdef API
  end;
#endif
  raise @@ Update.ModErr err

let print_error_disconnected conf =
  let err = Printf.sprintf "%s" (capitale (transl conf "msg error disconnected")) in
#ifdef API
  if not !Api_conf.mode_api then begin
#endif
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "%s" err;
  Hutil.trailer conf;
#ifdef API
  end ;
#endif
  raise @@ Update.ModErr err

let family_exclude pfams efam =
  let pfaml =
    Array.fold_right
      (fun fam faml -> if fam = efam then faml else fam :: faml)
      pfams []
  in
  Array.of_list pfaml

let infer_origin_file_from_other_marriages base ifam ip =
  let u = poi base ip in
  let ufams = get_family u in
  let rec loop i =
    if i = Array.length ufams then None
    else if ufams.(i) = ifam then loop (i + 1)
    else
      let r = get_origin_file (foi base ufams.(i)) in
      if sou base r <> "" then Some r else loop (i + 1)
  in
  loop 0

let infer_origin_file conf base ifam ncpl ndes =
  let r =
    infer_origin_file_from_other_marriages base ifam (Adef.father ncpl)
  in
  let r =
    if r = None then
      infer_origin_file_from_other_marriages base ifam (Adef.mother ncpl)
    else r
  in
  let r =
    match r with
      Some r -> r
    | None ->
        let afath = poi base (Adef.father ncpl) in
        let amoth = poi base (Adef.mother ncpl) in
        match get_parents afath, get_parents amoth with
          Some if1, _ when sou base (get_origin_file (foi base if1)) <> "" ->
            get_origin_file (foi base if1)
        | _, Some if2 when sou base (get_origin_file (foi base if2)) <> "" ->
            get_origin_file (foi base if2)
        | _ ->
            let rec loop i =
              if i = Array.length ndes.children then
                Gwdb.insert_string base ""
              else
                let cifams = get_family (poi base ndes.children.(i)) in
                if Array.length cifams = 0 then loop (i + 1)
                else if
                  sou base (get_origin_file (foi base cifams.(0))) <> ""
                then
                  get_origin_file (foi base cifams.(0))
                else loop (i + 1)
            in
            loop 0
  in
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no" with
      Not_found -> false
  in
  if no_dec && sou base r = "" then print_error_disconnected conf else r

let fwitnesses_of fevents =
  List.fold_left
    (fun ipl e ->
       Array.fold_left (fun ipl (ip, _) -> ip :: ipl) ipl e.efam_witnesses)
    [] fevents


(* Lorsqu'on ajout naissance décès par exemple en créant une personne. *)
let patch_person_with_pevents base ip =
  let p = poi base ip in
  let p = gen_person_of_person p in
  let empty_string = Gwdb.insert_string base "" in
  let evt ~name ?(date = Adef.cdate_None) ~place ~src ~note () =
    {epers_name = name; epers_date = date;
     epers_place = place; epers_reason = empty_string;
     epers_note = note; epers_src = src;
     epers_witnesses = [| |]}
  in
  let evt_birth =
    let evt ?date () =
      let name = Epers_Birth in
      let place = p.birth_place in
      let note = p.birth_note in
      let src = p.birth_src in
      Some (evt ~name ?date ~place ~note ~src ())
    in
    if Adef.od_of_cdate p.birth <> None
    then evt ~date:p.birth ()
    else if sou base p.birth_place = "" then None
    else evt ()
  in
  let evt_baptism =
    let evt ?date () =
      let name = Epers_Baptism in
      let place = p.baptism_place in
      let note = p.baptism_note in
      let src = p.baptism_src in
      Some (evt ~name ?date ~place ~note ~src ())
    in
    if Adef.od_of_cdate p.baptism <> None
    then evt ~date:p.baptism ()
    else if sou base p.baptism_place = "" then None
    else evt ()
  in
  let evt_death =
    let evt ?date () =
      let name = Epers_Death in
      let place = p.death_place in
      let note = p.death_note in
      let src = p.death_src in
      Some (evt ~name ?date ~place ~note ~src ())
    in
    match p.death with
    | Death (_, cd) ->
        let date = Adef.cdate_of_od (Some (Adef.date_of_cdate cd)) in
        evt ~date ()
    | _ ->
        if sou base p.death_place = "" then None
        else evt ()
  in
  (* Attention, on prend aussi les autres évènements sinon,  *)
  (* on va tout effacer et ne garder que naissance et décès. *)
  let found_birth = ref false in
  let found_death = ref false in
  let pevents =
    let rec loop pevents accu =
      match pevents with
        [] ->
          let accu =
            if !found_birth then accu
            else
              match evt_birth, evt_baptism with
                Some evt, None -> evt :: accu
              | None, Some evt -> evt :: accu
              | _ -> accu
          in
          let accu =
            if !found_death then accu
            else
              match evt_death with
                Some evt -> evt :: accu
              | None -> accu
          in
          List.rev accu
      | evt :: l ->
          match evt.epers_name with
            Epers_Birth | Epers_Baptism ->
              if !found_birth then loop l (evt :: accu)
              else
                begin match evt_birth, evt_baptism with
                  Some evt2, None ->
                    let () = found_birth := true in
                    (* Si il y avait des témoins, on les remets en place. *)
                    let evt =
                      {evt2 with epers_witnesses = evt.epers_witnesses}
                    in
                    loop l (evt :: accu)
                | None, Some evt2 ->
                    let () = found_birth := true in
                    (* Si il y avait des témoins, on les remets en place. *)
                    let evt =
                      {evt2 with epers_witnesses = evt.epers_witnesses}
                    in
                    loop l (evt :: accu)
                | _ -> loop l (evt :: accu)
                end
          | Epers_Death ->
              if !found_death then loop l (evt :: accu)
              else
                begin match evt_death with
                  Some evt2 ->
                    let () = found_death := true in
                    (* Si il y avait des témoins, on les remets en place. *)
                    let evt =
                      {evt2 with epers_witnesses = evt.epers_witnesses}
                    in
                    loop l (evt :: accu)
                | None -> loop l (evt :: accu)
                end
          | _ -> loop l (evt :: accu)
    in
    loop p.pevents []
  in
  let p = {p with pevents = pevents} in patch_person base p.key_index p

let patch_parent_with_pevents base cpl =
  Array.iter (patch_person_with_pevents base) (Adef.parent_array cpl)

let patch_children_with_pevents base des =
  Array.iter (patch_person_with_pevents base) des.children

(* On met à jour les témoins maintenant. *)
let update_family_with_fevents conf base fam =
  let (marr, div, witnesses) =
    reconstitute_from_fevents (p_getenv conf.env "nsck" = Some "on")
      (Gwdb.insert_string base "") fam.fevents
  in
  let (relation, marriage, marriage_place, marriage_note, marriage_src) =
    marr
  in
  let divorce = div in
  let witnesses = Array.map fst witnesses in
  {fam with marriage = marriage; marriage_place = marriage_place;
            marriage_note = marriage_note; marriage_src = marriage_src;
            relation = relation; divorce = divorce;
            witnesses = witnesses}

let effective_mod conf base sfam scpl sdes =
  let fi = sfam.fam_index in
  let (oorigin, owitnesses, ofevents) =
    let ofam = foi base fi in
    get_origin_file ofam, get_witnesses ofam, get_fevents ofam
  in
  let (oarr, ofather, omother) =
    let ocpl = foi base fi in
    get_parent_array ocpl, get_father ocpl, get_mother ocpl
  in
  let ochildren = get_children (foi base fi) in
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with
      Some s -> String.trim s
    | None -> ""
  in
  let ncpl =
    Futil.map_couple_p conf.multi_parents
      (Update.insert_person conf base psrc created_p) scpl
  in
  let nfam =
    Futil.map_family_ps
      (Update.insert_person conf base psrc created_p)
      (fun f -> f)
      (Gwdb.insert_string base)
      sfam
  in
  let ndes =
    Futil.map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let nfath = poi base (Adef.father ncpl) in
  let nmoth = poi base (Adef.mother ncpl) in
  let nfam = update_family_with_fevents conf base nfam in
#ifdef API
  let nfam =
    (* En mode api, on gère directement la relation de même sexe. *)
    if !(Api_conf.mode_api) then {nfam with relation = sfam.relation}
    else nfam
  in
#endif
  let sfam = {sfam with relation = nfam.relation} in
  if sfam.relation <> NoSexesCheckNotMarried &&
     sfam.relation <> NoSexesCheckMarried
  then
    begin
      begin match get_sex nfath with
        Female -> print_err_father_sex conf base nfath
      | Male -> ()
      | Neuter ->
          let nfath = {(gen_person_of_person nfath) with sex = Male} in
          patch_person base nfath.key_index nfath
      end;
      match get_sex nmoth with
        Male -> print_err_mother_sex conf base nmoth
      | Female -> ()
      | Neuter ->
          let nmoth = {(gen_person_of_person nmoth) with sex = Female} in
          patch_person base nmoth.key_index nmoth
    end;
  if Adef.father ncpl = Adef.mother ncpl then print_err conf ;
  let nfam =
    let origin_file =
      if sfam.origin_file = "" then
        if sou base oorigin <> "" then oorigin
        else infer_origin_file conf base fi ncpl ndes
      else nfam.origin_file
    in
    {nfam with origin_file = origin_file; fam_index = fi}
  in
  patch_family base fi nfam;
  patch_couple base fi ncpl;
  patch_descend base fi ndes;
  let narr = Adef.parent_array ncpl in
  for i = 0 to Array.length oarr - 1 do
    if not (Array.mem oarr.(i) narr) then
      let ou = poi base oarr.(i) in
      let ou = {family = family_exclude (get_family ou) fi} in
      patch_union base oarr.(i) ou
  done;
  for i = 0 to Array.length narr - 1 do
    if not (Array.mem narr.(i) oarr) then
      let nu = poi base narr.(i) in
      let nu = {family = Array.append (get_family nu) [| fi |]} in
      patch_union base narr.(i) nu
  done;
  let cache = Hashtbl.create 101 in
  let find_asc ip =
    try Hashtbl.find cache ip with
      Not_found ->
        let a = poi base ip in
        let a = {parents = get_parents a; consang = get_consang a} in
        Hashtbl.add cache ip a; a
  in
  let same_parents =
    Adef.father ncpl = ofather && Adef.mother ncpl = omother
  in
  Array.iter
    (fun ip ->
       let a = find_asc ip in
       let a =
         {parents = None;
          consang =
            if not (Array.mem ip ndes.children) then Adef.fix (-1)
            else a.consang}
       in
       Hashtbl.replace cache ip a)
    ochildren;
  Array.iter
    (fun ip ->
       let a = find_asc ip in
       match a.parents with
         Some _ -> print_err_parents conf base (poi base ip)
       | None ->
           let a =
             {parents = Some fi;
              consang =
                if not (Array.mem ip ochildren) || not same_parents then
                  Adef.fix (-1)
                else a.consang}
           in
           Hashtbl.replace cache ip a)
    ndes.children;
  Array.iter
    (fun ip ->
       if not (Array.mem ip ndes.children) then
         patch_ascend base ip (find_asc ip))
    ochildren;
  Array.iter
    (fun ip ->
       if not (Array.mem ip ochildren) || not same_parents then
         patch_ascend base ip (find_asc ip))
    ndes.children;
  let ol =
    Array.fold_right (fun x acc -> x :: acc) owitnesses (fwitnesses_of ofevents)
  in
  let nl =
    Array.fold_right (fun x acc -> x :: acc) nfam.witnesses (fwitnesses_of nfam.fevents)
  in
  let pi = Adef.father ncpl in
  Update.update_related_pointers base pi ol nl; fi, nfam, ncpl, ndes

let effective_add conf base sfam scpl sdes =
  let fi = insert_family base (empty_family base dummy_ifam) in
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with
      Some s -> String.trim s
    | None -> ""
  in
  let ncpl =
    Futil.map_couple_p conf.multi_parents
      (Update.insert_person conf base psrc created_p) scpl
  in
  let nfam =
    Futil.map_family_ps
      (Update.insert_person conf base psrc created_p)
      (fun f -> f)
      (Gwdb.insert_string base)
      sfam
  in
  let ndes =
    Futil.map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let origin_file = infer_origin_file conf base fi ncpl ndes in
  let nfath_p = poi base (Adef.father ncpl) in
  let nmoth_p = poi base (Adef.mother ncpl) in
  let nfam = update_family_with_fevents conf base nfam in
#ifdef API
  let nfam =
    (* En mode api, on gère directement la relation de même sexe. *)
    if !(Api_conf.mode_api) then {nfam with relation = sfam.relation}
    else nfam
  in
#endif
  let sfam = {sfam with relation = nfam.relation} in
  if sfam.relation <> NoSexesCheckNotMarried &&
     sfam.relation <> NoSexesCheckMarried
  then
    begin
      begin match get_sex nfath_p with
        Female -> print_err_father_sex conf base nfath_p
      | Male -> ()
      | _ ->
          let nfath_p = {(gen_person_of_person nfath_p) with sex = Male} in
          patch_person base nfath_p.key_index nfath_p
      end;
      match get_sex nmoth_p with
        Male -> print_err_mother_sex conf base nmoth_p
      | Female -> ()
      | _ ->
          let nmoth_p = {(gen_person_of_person nmoth_p) with sex = Female} in
          patch_person base nmoth_p.key_index nmoth_p
    end
  else if Adef.father ncpl = Adef.mother ncpl then print_err conf;
  let nfam = {nfam with origin_file = origin_file; fam_index = fi} in
  patch_family base fi nfam;
  patch_couple base fi ncpl;
  patch_descend base fi ndes;
  let nfath_u = {family = Array.append (get_family nfath_p) [| fi |]} in
  let nmoth_u = {family = Array.append (get_family nmoth_p) [| fi |]} in
  patch_union base (Adef.father ncpl) nfath_u;
  patch_union base (Adef.mother ncpl) nmoth_u;
  Array.iter
    (fun ip ->
       let p = poi base ip in
       match get_parents p with
         Some _ -> print_err_parents conf base p
       | None ->
           let a = {parents = Some fi; consang = Adef.fix (-1)} in
           patch_ascend base (get_iper p) a)
    ndes.children;
  let nl_witnesses = Array.to_list nfam.witnesses in
  let nl_fevents = fwitnesses_of nfam.fevents in
  let nl = List.append nl_witnesses nl_fevents in
  Update.update_related_pointers base (Adef.father ncpl) [] nl;
  fi, nfam, ncpl, ndes

let effective_inv conf base ip u ifam =
  let rec loop =
    function
      ifam1 :: ifam2 :: ifaml ->
        if ifam2 = ifam then ifam2 :: ifam1 :: ifaml
        else ifam1 :: loop (ifam2 :: ifaml)
    | _ -> Hutil.incorrect_request conf; raise @@ Update.ModErr __LOC__
  in
  let u = {family = Array.of_list (loop (Array.to_list (get_family u)))} in
  patch_union base ip u


(* ************************************************************************ *)
(*  [Fonc] effective_chg_order : base -> iper -> person -> ifam -> int -> unit        *)
(** [Description] : Modifie l'ordre de la famille en positionnant la famille
      ifam à la position n. Exemple : [f1 f2 f3 f4] f1 3 => [f2 f3 f1 f4].
    [Args] :
      - base : base de donnée
      - ip   : iper
      - u    : person
      - ifam : famille à changer de place
      - n    : nouvelle position de la famille
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let effective_chg_order base ip u ifam n =
  let fam = UpdateFam.change_order u ifam n in
  let u = {family = Array.of_list fam} in patch_union base ip u

let kill_family base ifam1 ip =
  let u = poi base ip in
  let l =
    Array.fold_right
      (fun ifam ifaml -> if ifam = ifam1 then ifaml else ifam :: ifaml)
      (get_family u) []
  in
  let u = {family = Array.of_list l} in patch_union base ip u

let kill_parents base ip =
  let a = {parents = None; consang = Adef.fix (-1)} in patch_ascend base ip a

let effective_del base ifam fam =
  kill_family base ifam (get_father fam);
  kill_family base ifam (get_mother fam);
  Array.iter (kill_parents base) (get_children fam);
  delete_family base ifam

let array_forall2 f a1 a2 =
  if Array.length a1 <> Array.length a2 then invalid_arg "array_forall2"
  else
    let rec loop i =
      if i = Array.length a1 then true
      else if f a1.(i) a2.(i) then loop (i + 1)
      else false
    in
    loop 0

let is_a_link =
  function
    _, _, _, Update.Link, _ -> true
  | _ -> false

let is_created_or_already_there ochil_arr nchil schil =
  not (is_a_link schil) || Array.mem nchil ochil_arr

(* need_check_noloop: optimization
     The no-loop check being a big work on large databases, this
   optimization tests if this is really necessary or not. It is not
   necessary if:
   1/ either all parents are created,
   2/ or all children are created,
   3/ or the new family have the same parents than the old one *and*
      all linked (not created) new children were already children.
*)
(* Replaced && by || to do more checks. *)
(* Improvement : check the name on the parents/children if they linked *)

let need_check_noloop (scpl, sdes, onfs) =
  if Array.exists is_a_link (Gutil.parent_array scpl) ||
     Array.exists is_a_link sdes.children
  then
    match onfs with
      Some ((opar, ochil), (npar, nchil)) ->
        not
          (array_forall2 (is_created_or_already_there opar) npar
             (Gutil.parent_array scpl)) ||
        not
          (array_forall2 (is_created_or_already_there ochil) nchil
             sdes.children)
    | None -> true
  else false

let all_checks_family conf base ifam gen_fam cpl des scdo =
  let wl = ref [] in
  let ml = ref [] in
  let error = Update.error conf base in
  let warning w = wl := w :: !wl in
  let misc m = ml := m :: !ml in
  if need_check_noloop scdo then
    Consang.check_noloop_for_person_list base error
      (Array.to_list (Adef.parent_array cpl));
  let fam = family_of_gen_family base (gen_fam, cpl, des) in
  CheckItem.family base warning ifam fam;
  CheckItem.check_other_fields base misc ifam fam;
  let (wl, ml) = List.sort_uniq compare !wl, List.sort_uniq compare !ml in
  List.iter
    (function
       ChangedOrderOfMarriages (p, _, after) ->
         patch_union base (get_iper p) {family = after}
     | ChangedOrderOfFamilyEvents (ifam, _, after) ->
         patch_family base ifam {gen_fam with fevents = after}
     | _ -> ())
    wl;
  wl, ml

let print_family conf base (wl, ml) cpl des =
  let rdsrc =
    match p_getenv conf.env "rdsrc" with
      Some "on" -> p_getenv conf.env "src"
    | _ -> p_getenv conf.env "dsrc"
  in
  begin match rdsrc with
    Some x ->
      conf.henv <- List.remove_assoc "dsrc" conf.henv;
      if x <> "" then conf.henv <- ("dsrc", code_varenv x) :: conf.henv
  | None -> ()
  end;
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>";
  Wserver.printf "%s"
    (referenced_person_text conf base (poi base (Adef.father cpl)));
  Wserver.printf "</li>";
  Wserver.printf "\n";
  Wserver.printf "<li>";
  Wserver.printf "%s"
    (referenced_person_text conf base (poi base (Adef.mother cpl)));
  Wserver.printf "</li>";
  Wserver.printf "</ul>\n";
  if des.children <> [| |] then
    begin
      Wserver.printf "<ul>\n";
      Array.iter
        (fun ip ->
           Wserver.printf "<li>";
           Wserver.printf "%s"
             (referenced_person_text conf base (poi base ip));
           Wserver.printf "</li>")
        des.children;
      Wserver.printf "</ul>\n"
    end;
  Update.print_warnings_and_miscs conf base wl ml

let print_mod_ok conf base (wl, ml) cpl des =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "family modified"))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then
    begin
      Wserver.printf "<h3 class=\"error\">";
      Wserver.printf (fcapitale (ftransl conf "%s forbidden char"))
        (List.fold_left (fun acc c -> acc ^ "'" ^ Char.escaped c ^ "' ") " "
           Name.forbidden_char);
      Wserver.printf "</h3>\n";
      List.iter (Wserver.printf "<p>%s</p>") !removed_string
    end;
  print_family conf base (wl, ml) cpl des;
  Hutil.trailer conf

let print_change_event_order_ok conf base (wl, ml) cpl des =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "family modified"))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  print_family conf base (wl, ml) cpl des;
  Hutil.trailer conf

let print_add_ok conf base (wl, ml) cpl des =
  let title _ = Wserver.printf "%s" (capitale (transl conf "family added")) in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then
    begin
      Wserver.printf "<h2 class=\"error\">%s</h2>\n"
        (capitale (transl conf "forbidden char"));
      List.iter (Wserver.printf "<p>%s</p>") !removed_string
    end;
  print_family conf base (wl, ml) cpl des;
  Hutil.trailer conf

let print_del_ok conf base wl =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "family deleted"))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  begin match p_getenv conf.env "ip" with
    Some i ->
      let p = poi base (iper_of_string i) in
      Wserver.printf "<ul>\n";
      Wserver.printf "<li>\n";
      Wserver.printf "%s\n" (reference conf base p (person_text conf base p));
      Wserver.printf "</ul>\n"
  | _ -> ()
  end;
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let print_inv_ok conf base p =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "inversion done"))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "\n%s" (referenced_person_text conf base p);
  Wserver.printf "\n";
  Hutil.trailer conf

let get_create (_, _, _, create, _) = create

let forbidden_disconnected conf scpl sdes =
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no" with
      Not_found -> false
  in
  if no_dec then
    if get_create (Gutil.father scpl) = Update.Link ||
       get_create (Gutil.mother scpl) = Update.Link
    then
      false
    else
      Array.for_all
        (fun p -> get_create p <> Update.Link)
        sdes.children
  else false

let print_add o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let conf = Update.update_conf o_conf in
  let (sfam, scpl, sdes, ext) = reconstitute_family conf base in
  let redisp =
    match p_getenv conf.env "return" with
      Some _ -> true
    | _ -> false
  in
  let digest =
    match p_getenv conf.env "ip" with
      Some ip ->
      string_of_int
        (Array.length (get_family (poi base (iper_of_string ip))))
    | None -> ""
  in
  let sdigest = raw_get conf "digest" in
  if digest <> "" && sdigest <> "" && digest <> sdigest then
    Update.error_digest conf
  else if ext || redisp then
    UpdateFam.print_update_fam conf base (sfam, scpl, sdes) ""
  else if forbidden_disconnected conf scpl sdes then
    print_error_disconnected conf
  else
    match check_family conf sfam scpl with
      Some err, _ | _, Some err -> error_family conf err
    | None, None ->
      let (sfam, sdes) = strip_family sfam sdes in
      let (ifam, fam, cpl, des) =
        effective_add conf base sfam scpl sdes
      in
      let () = patch_parent_with_pevents base cpl in
      let () = patch_children_with_pevents base des in
      let (wl, ml) =
        all_checks_family conf base ifam fam cpl des (scpl, sdes, None)
      in
      let (changed, act) =
        let fam = Util.string_gen_family base fam in
        let (ip, act) =
          match p_getenv conf.env "ip" with
            Some i ->
            let i = iper_of_string i in
            if Adef.mother cpl = i then
              Adef.mother cpl, "af"
            else
              let a = poi base i in
              begin match get_parents a with
                  Some x when x = ifam -> i, "aa"
                | _ -> Adef.father cpl, "af"
              end
          | None -> Adef.father cpl, "af"
        in
        match act with
          "af" ->
          let gen_p =
            Util.string_gen_person base
              (gen_person_of_person (poi base ip))
          in
          U_Add_family (gen_p, fam), "af"
        | _ ->
          let gen_p =
            Util.string_gen_person base
              (gen_person_of_person (poi base ip))
          in
          U_Add_parent (gen_p, fam), "aa"
      in
      Util.commit_patches conf base;
      History.record conf base changed act;
      Update.delete_topological_sort conf base;
      print_add_ok conf base (wl, ml) cpl des

let print_del conf base =
  match p_getenv conf.env "i" with
    Some i ->
      let ifam = ifam_of_string i in
      let fam = foi base ifam in
      effective_del base ifam fam;
      Util.commit_patches conf base;
      let changed =
        let gen_p =
          let p =
            match p_getenv conf.env "ip" with
              Some i when get_mother fam = iper_of_string i ->
              poi base (get_mother fam)
            | _ -> poi base (get_father fam)
          in
          Util.string_gen_person base (gen_person_of_person p)
        in
        let gen_fam =
          Util.string_gen_family base (gen_family_of_family fam)
        in
        U_Delete_family (gen_p, gen_fam)
      in
      History.record conf base changed "df";
      Update.delete_topological_sort conf base ;
      print_del_ok conf base []
  | _ -> Hutil.incorrect_request conf

let print_mod_aux conf base callback =
  let (sfam, scpl, sdes, ext) = reconstitute_family conf base in
  let redisp =
    match p_getenv conf.env "return" with
      Some _ -> true
    | _ -> false
  in
  let digest =
    let ini_sfam = UpdateFam.string_family_of conf base sfam.fam_index in
    Update.digest_family ini_sfam
  in
  if digest = raw_get conf "digest" then
    if ext || redisp then
      UpdateFam.print_update_fam conf base (sfam, scpl, sdes) digest
    else
      match check_family conf sfam scpl with
        Some err, _ | _, Some err -> error_family conf err
      | None, None ->
        let (sfam, sdes) = strip_family sfam sdes in
        callback sfam scpl sdes
  else Update.error_digest conf

let family_structure base ifam =
  let fam = foi base ifam in get_parent_array fam, get_children fam

let print_mod o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let o_f =
    let ifam =
      match p_getenv o_conf.env "i" with
        Some i -> ifam_of_string i
      | None -> dummy_ifam
    in
    Util.string_gen_family base (gen_family_of_family (foi base ifam))
  in
  let conf = Update.update_conf o_conf in
  let callback sfam scpl sdes =
    let ofs = family_structure base sfam.fam_index in
    let (ifam, fam, cpl, des) = effective_mod conf base sfam scpl sdes in
    let () = patch_parent_with_pevents base cpl in
    let () = patch_children_with_pevents base des in
    let s =
      let sl =
        [fam.comment; fam.fsources; fam.marriage_note; fam.marriage_src]
      in
      let sl =
        let rec loop l accu =
          match l with
            [] -> accu
          | evt :: l -> loop l (evt.efam_note :: evt.efam_src :: accu)
        in
        loop fam.fevents sl
      in
      String.concat " " (List.map (sou base) sl)
    in
    Notes.update_notes_links_db conf (NotesLinks.PgFam ifam) s;
    let nfs = Adef.parent_array cpl, des.children in
    let onfs = Some (ofs, nfs) in
    let (wl, ml) =
      all_checks_family conf base ifam fam cpl des (scpl, sdes, onfs)
    in
    Util.commit_patches conf base;
    let changed =
      let ip =
        match p_getenv o_conf.env "ip" with
          Some i -> iper_of_string i
        | None -> dummy_iper
      in
      let p =
        Util.string_gen_person base (gen_person_of_person (poi base ip))
      in
      let n_f = Util.string_gen_family base fam in
      U_Modify_family (p, o_f, n_f)
    in
    History.record conf base changed "mf";
    Update.delete_topological_sort conf base;
    print_mod_ok conf base (wl, ml) cpl des
  in
  print_mod_aux conf base callback

let print_inv conf base =
  match p_getenv conf.env "i", p_getenv conf.env "f" with
    Some ip, Some ifam ->
      let ip = iper_of_string ip in
      let ifam = ifam_of_string ifam in
      let p = poi base ip in
      effective_inv conf base (get_iper p) p ifam;
      Util.commit_patches conf base;
      let changed =
        let gen_p = Util.string_gen_person base (gen_person_of_person p) in
        U_Invert_family (gen_p, ifam)
      in
      History.record conf base changed "if"; print_inv_ok conf base p
  | _ -> Hutil.incorrect_request conf

let print_change_order_ok conf base =
  match
    p_getenv conf.env "i", p_getenv conf.env "f", p_getint conf.env "n"
  with
    Some ip, Some ifam, Some n ->
      let ip = iper_of_string ip in
      let ifam = ifam_of_string ifam in
      let p = poi base ip in
      effective_chg_order base (get_iper p) p ifam n;
      Util.commit_patches conf base;
      let changed =
        let gen_p = Util.string_gen_person base (gen_person_of_person p) in
        U_Invert_family (gen_p, ifam)
      in
      History.record conf base changed "if"; print_inv_ok conf base p
  | _ -> Hutil.incorrect_request conf

let print_change_event_order conf base =
  match p_getenv conf.env "i" with
  | Some ifam ->
    let ifam = Gwdb.ifam_of_string ifam in
    let fam = foi base ifam in
    let o_f = Util.string_gen_family base (gen_family_of_family fam) in
    let ht = Hashtbl.create 50 in
    let _ =
      List.fold_left (fun id evt -> Hashtbl.add ht id evt; succ id) 1
        (get_fevents fam)
    in
    let sorted_fevents =
      List.sort (fun (_, pos1) (_, pos2) -> compare pos1 pos2)
        (reconstitute_sorted_fevents conf 1)
    in
    let fevents =
      List.fold_right
        (fun (id, _) accu ->
           try Hashtbl.find ht id :: accu with
             Not_found -> failwith "Sorting event")
        sorted_fevents []
    in
    let fam = gen_family_of_family fam in
    let fam = {fam with fevents = fevents} in
    let fam = update_family_with_fevents conf base fam in
    patch_family base fam.fam_index fam;
    let a = foi base fam.fam_index in
    let cpl = Futil.parent conf.multi_parents (get_parent_array a) in
    let des = {children = get_children a} in
    let wl =
      let wl = ref [] in
      let warning w = wl := w :: !wl in
      let nfam = family_of_gen_family base (fam, cpl, des) in
      CheckItem.family base warning fam.fam_index nfam;
      List.iter
        (function
            ChangedOrderOfFamilyEvents (ifam, _, after) ->
            patch_family base ifam {fam with fevents = after}
          | _ -> ())
        !wl;
      List.rev !wl
    in
    Util.commit_patches conf base;
    let changed =
      let ip =
        match p_getenv conf.env "ip" with
          Some i -> iper_of_string i
        | None -> dummy_iper
      in
      let p =
        Util.string_gen_person base (gen_person_of_person (poi base ip))
      in
      let n_f = Util.string_gen_family base fam in
      U_Modify_family (p, o_f, n_f)
    in
    History.record conf base changed "mf";
    print_change_event_order_ok conf base (wl, []) cpl des
  | _ -> Hutil.incorrect_request conf
