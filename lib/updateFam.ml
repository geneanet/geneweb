(* $Id: updateFam.ml,v 5.24 2008-01-09 03:34:36 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open TemplAst
open Util

type create_info =
  Update.create_info =
    { ci_birth_date : date option;
      ci_birth_place : string;
      ci_death : death;
      ci_death_date : date option;
      ci_death_place : string;
      ci_occupation : string;
      ci_public : bool }

let bogus_family_index = Adef.ifam_of_int (-1)

let default_source conf =
  match p_getenv conf.env "dsrc" with
    Some s -> s
  | None -> ""

let person_key base ip =
  let p = poi base ip in
  let first_name = sou base (get_first_name p) in
  let surname = sou base (get_surname p) in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper ip
    else get_occ p
  in
  first_name, surname, occ, Update.Link, ""

let string_family_of conf base ifam =
  let fam = foi base ifam in
  let sfam =
    Futil.map_family_ps (person_key base) (sou base)
      (gen_family_of_family fam)
  in
  let scpl =
    Futil.map_couple_p conf.multi_parents (person_key base)
      (gen_couple_of_couple fam)
  in
  let sdes =
    Futil.map_descend_p (person_key base) (gen_descend_of_descend fam)
  in
  sfam, scpl, sdes

(* Interpretation of template file 'updfam.txt' *)

type 'a env =
    Vstring of string
  | Vint of int
  | Vother of 'a
  | Vbool of bool
  | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""

let obsolete_list = ref []

let obsolete version var new_var r =
  if List.mem var !obsolete_list then r
  else if Sys.unix then
    begin
      Printf.eprintf "*** <W> updfam.txt: \"%s\" obsolete since v%s%s\n" var
        version
        (if new_var = "" then "" else "; rather use \"" ^ new_var ^ "\"");
      flush stderr;
      obsolete_list := var :: !obsolete_list;
      r
    end
  else r

let bool_val x = VVbool x
let str_val x = VVstring x

let rec eval_var conf base env (fam, cpl, des) _loc sl =
  try eval_special_var conf base sl with
    Not_found -> eval_simple_var conf base env (fam, cpl, des) sl
and eval_simple_var conf base env (fam, cpl, des) =
  function
    ["bvar"; v] ->
      begin try VVstring (List.assoc v conf.base_env) with
        Not_found -> VVstring ""
      end
  | "child" :: sl ->
      let k =
        match get_env "cnt" env with
          Vint i ->
            let i = i - 1 in
            if i >= 0 && i < Array.length des.children then des.children.(i)
            else if i >= 0 && i < 1 && Array.length des.children = 0 then
              "", "", 0, Update.Create (Neuter, None), ""
            else raise Not_found
        | _ -> raise Not_found
      in
      eval_key k sl
  | ["cnt"] -> eval_int_env "cnt" env
  | ["comment"] -> str_val (Util.escape_html fam.comment)
  | ["digest"] -> eval_string_env "digest" env
  | ["divorce"] ->
      let s =
        match fam.divorce with
          Divorced _ -> "divorced"
        | NotDivorced -> "not_divorced"
        | Separated -> "separated"
      in
      str_val s
  | ["divorce"; s] ->
      let d =
        match fam.divorce with
          Divorced d -> Adef.od_of_cdate d
        | _ -> None
      in
      eval_date_var d s
  | "father" :: sl -> eval_key (Gutil.father cpl) sl
  | ["fsources"] -> str_val (Util.escape_html fam.fsources)
  | ["is_first"] ->
      begin match get_env "first" env with
        Vbool x -> bool_val x
      | _ -> raise Not_found
      end
  | ["is_last"] ->
      begin match get_env "last" env with
        Vbool x -> bool_val x
      | _ -> raise Not_found
      end
  | ["marriage"; s] -> eval_date_var (Adef.od_of_cdate fam.marriage) s
  | ["marriage_place"] -> str_val (Util.escape_html fam.marriage_place)
  | ["marriage_note"] -> str_val (Util.escape_html fam.marriage_note)
  | ["marriage_src"] -> str_val (Util.escape_html fam.marriage_src)
  | ["mrel"] -> str_val (eval_relation_kind fam.relation)
  | ["nb_fevents"] -> str_val (string_of_int (List.length fam.fevents))
  | ["origin_file"] -> str_val (Util.escape_html fam.origin_file)
  | "parent" :: sl ->
      let k =
        match get_env "cnt" env with
          Vint i ->
            let arr = Gutil.parent_array cpl in
            let i = i - 1 in
            if i >= 0 && i < Array.length arr then arr.(i)
            else if i >= 0 && i < 1 && Array.length arr = 0 then
              "", "", 0, Update.Create (Neuter, None), ""
            else raise Not_found
        | _ -> raise Not_found
      in
      eval_parent conf env k sl
  | ["wcnt"] -> eval_int_env "wcnt" env
  | "witness" :: sl ->
      let k =
        match get_env "cnt" env with
          Vint i ->
            let i = i - 1 in
            if i >= 0 && i < Array.length fam.witnesses then fam.witnesses.(i)
            else if i >= 0 && i < 2 && Array.length fam.witnesses < 2 then
              "", "", 0, Update.Create (Neuter, None), ""
            else raise Not_found
        | _ -> raise Not_found
      in
      eval_key k sl
  | ["has_fevents"] -> bool_val (fam.fevents <> [])
  | "event" :: sl ->
      let e =
        match get_env "cnt" env with
          Vint i ->
            (try Some (List.nth fam.fevents (i - 1)) with Failure _ -> None)
        | _ -> None
      in
      eval_event_var e sl
  | ["event_date"; s] ->
      let od =
        match get_env "cnt" env with
          Vint i ->
            begin try
              let e = List.nth fam.fevents (i - 1) in
              Adef.od_of_cdate e.efam_date
            with Failure _ -> None
            end
        | _ -> None
      in
      eval_date_var od s
  | ["event_str"] ->
      begin match get_env "cnt" env with
        Vint i ->
          begin try
            let fam = foi base fam.fam_index in
            let e = List.nth (get_fevents fam) (i - 1) in
            let name =
              capitale (Util.string_of_fevent_name conf base e.efam_name)
            in
            let date =
              match Adef.od_of_cdate e.efam_date with
                Some d -> Date.string_of_date conf d
              | None -> ""
            in
            let place = Util.string_of_place conf (sou base e.efam_place) in
            let note = sou base e.efam_note in
            let src = sou base e.efam_src in
            let wit =
              List.fold_right
                (fun (w, _) accu ->
                   (transl_nth conf "witness/witnesses" 0 ^ ": " ^
                    Util.person_text conf base (poi base w)) ::
                   accu)
                (Array.to_list e.efam_witnesses) []
            in
            let s = String.concat ", " [name; date; place; note; src] in
            let sw = String.concat ", " wit in str_val (s ^ ", " ^ sw)
          with Failure _ -> str_val ""
          end
      | _ -> str_val ""
      end
  | ["has_fwitness"] ->
      begin match get_env "cnt" env with
        Vint i ->
          let e =
            try Some (List.nth fam.fevents (i - 1)) with Failure _ -> None
          in
          begin match e with
            Some e -> bool_val (e.efam_witnesses <> [| |])
          | None -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | "fwitness" :: sl ->
      begin match get_env "cnt" env with
        Vint i ->
          let e =
            try Some (List.nth fam.fevents (i - 1)) with Failure _ -> None
          in
          begin match e with
            Some e ->
              begin match get_env "wcnt" env with
                Vint i ->
                  let i = i - 1 in
                  let k =
                    if i >= 0 && i < Array.length e.efam_witnesses then
                      fst e.efam_witnesses.(i)
                    else if
                      i >= 0 && i < 2 && Array.length e.efam_witnesses < 2
                    then
                      "", "", 0, Update.Create (Neuter, None), ""
                    else raise Not_found
                  in
                  eval_key k sl
              | _ -> raise Not_found
              end
          | None -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | ["fwitness_kind"] ->
      begin match get_env "cnt" env with
        Vint i ->
          let e =
            try Some (List.nth fam.fevents (i - 1)) with Failure _ -> None
          in
          begin match e with
            Some e ->
              begin match get_env "wcnt" env with
                Vint i ->
                  let i = i - 1 in
                  if i >= 0 && i < Array.length e.efam_witnesses then
                    match snd e.efam_witnesses.(i) with
                      Witness_GodParent -> str_val "godp"
                    | _ -> str_val ""
                  else if
                    i >= 0 && i < 2 && Array.length e.efam_witnesses < 2
                  then
                    str_val ""
                  else raise Not_found
              | _ -> raise Not_found
              end
          | None -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | [s] ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
          Some vv -> str_val (Util.escape_html vv)
        | None -> str_val ""
      else
        let v = extract_var "bvar_" s in
        let v = if v = "" then extract_var "cvar_" s else v in
        if v <> "" then
          str_val (try List.assoc v conf.base_env with Not_found -> "")
        else raise Not_found
  | _ -> raise Not_found
and eval_date_var od s = str_val (eval_date_var_aux od s)
and eval_date_var_aux od =
  function
    "calendar" ->
      begin match od with
        Some (Dgreg (_, Dgregorian)) -> "gregorian"
      | Some (Dgreg (_, Djulian)) -> "julian"
      | Some (Dgreg (_, Dfrench)) -> "french"
      | Some (Dgreg (_, Dhebrew)) -> "hebrew"
      | _ -> ""
      end
  | "day" ->
      begin match eval_date_field od with
        Some d -> if d.day = 0 then "" else string_of_int d.day
      | None -> ""
      end
  | "month" ->
      begin match eval_date_field od with
        Some d ->
          if d.month = 0 then ""
          else
            begin match od with
              Some (Dgreg (_, Dfrench)) -> short_f_month d.month
            | _ -> string_of_int d.month
            end
      | None -> ""
      end
  | "orday" ->
      begin match eval_date_field od with
        Some d ->
          begin match d.prec with
            OrYear d2 | YearInt d2 ->
              if d2.day2 = 0 then "" else string_of_int d2.day2
          | _ -> ""
          end
      | None -> ""
      end
  | "ormonth" ->
      begin match eval_date_field od with
        Some d ->
          begin match d.prec with
            OrYear d2 | YearInt d2 ->
              if d2.month2 = 0 then ""
              else
                begin match od with
                  Some (Dgreg (_, Dfrench)) -> short_f_month d2.month2
                | _ -> string_of_int d2.month2
                end
          | _ -> ""
          end
      | None -> ""
      end
  | "oryear" ->
      begin match eval_date_field od with
        Some d ->
          begin match d.prec with
            OrYear d2 | YearInt d2 -> string_of_int d2.year2
          | _ -> ""
          end
      | None -> ""
      end
  | "prec" ->
      begin match od with
        Some (Dgreg ({prec = Sure}, _)) -> "sure"
      | Some (Dgreg ({prec = About}, _)) -> "about"
      | Some (Dgreg ({prec = Maybe}, _)) -> "maybe"
      | Some (Dgreg ({prec = Before}, _)) -> "before"
      | Some (Dgreg ({prec = After}, _)) -> "after"
      | Some (Dgreg ({prec = OrYear _}, _)) -> "oryear"
      | Some (Dgreg ({prec = YearInt _}, _)) -> "yearint"
      | _ -> ""
      end
  | "text" ->
      begin match od with
        Some (Dtext s) -> Util.safe_html s
      | _ -> ""
      end
  | "year" ->
      begin match eval_date_field od with
        Some d -> string_of_int d.year
      | None -> ""
      end
  | _ -> raise Not_found
and eval_date_field =
  function
    Some d ->
      begin match d with
        Dgreg (d, Dgregorian) -> Some d
      | Dgreg (d, Djulian) -> Some (Calendar.julian_of_gregorian d)
      | Dgreg (d, Dfrench) -> Some (Calendar.french_of_gregorian d)
      | Dgreg (d, Dhebrew) -> Some (Calendar.hebrew_of_gregorian d)
      | _ -> None
      end
  | None -> None
and eval_event_var e =
  function
    ["e_name"] ->
      begin match e with
        Some {efam_name = name} ->
          begin match name with
            Efam_Marriage -> str_val "#marr"
          | Efam_NoMarriage -> str_val "#nmar"
          | Efam_NoMention -> str_val "#nmen"
          | Efam_Engage -> str_val "#enga"
          | Efam_Divorce -> str_val "#div"
          | Efam_Separated -> str_val "#sep"
          | Efam_Annulation -> str_val "#anul"
          | Efam_MarriageBann -> str_val "#marb"
          | Efam_MarriageContract -> str_val "#marc"
          | Efam_MarriageLicense -> str_val "#marl"
          | Efam_PACS -> str_val "#pacs"
          | Efam_Residence -> str_val "#resi"
          | Efam_Name x -> str_val (Util.escape_html x)
          end
      | _ -> str_val ""
      end
  | ["e_place"] ->
      begin match e with
        Some {efam_place = x} -> str_val (Util.escape_html x)
      | _ -> str_val ""
      end
  | ["e_note"] ->
      begin match e with
        Some {efam_note = x} -> str_val (Util.escape_html x)
      | _ -> str_val ""
      end
  | ["e_src"] ->
      begin match e with
        Some {efam_src = x} -> str_val (Util.escape_html x)
      | _ -> str_val ""
      end
  | _ -> raise Not_found
and eval_parent conf env k =
  function
    ["himher"] ->
      let s =
        match get_env "cnt" env with
          Vint 1 -> capitale (transl_nth conf "him/her" 0)
        | Vint 2 -> capitale (transl_nth conf "him/her" 1)
        | Vint _ -> transl conf "him/her"
        | _ -> "???"
      in
      str_val s
  | sl -> eval_key k sl
and eval_key (fn, sn, oc, create, _) =
  function
    ["create"] -> str_val (if create <> Update.Link then "create" else "link")
  | ["create"; s] -> str_val (eval_create create s)
  | ["first_name"] -> str_val (Util.escape_html fn)
  | ["occ"] -> str_val (if oc = 0 then "" else string_of_int oc)
  | ["surname"] -> str_val (Util.escape_html sn)
  | x ->
      match x with
        ["sex"] ->
          obsolete "5.00" "sex" "create.sex"
            (str_val (eval_create create "sex"))
      | _ -> raise Not_found
and eval_create c =
  function
    "birth_day" ->
      begin match c with
        Update.Create
          (_, Some {ci_birth_date = Some (Dgreg (dmy, Dfrench))}) ->
          let dmy = Calendar.french_of_gregorian dmy in
          if dmy.day <> 0 then string_of_int dmy.day else ""
      | Update.Create (_, Some {ci_birth_date = Some (Dgreg ({day = d}, _))})
        when d <> 0 ->
          string_of_int d
      | _ -> ""
      end
  | "birth_month" ->
      begin match c with
        Update.Create
          (_, Some {ci_birth_date = Some (Dgreg (dmy, Dfrench))}) ->
          let dmy = Calendar.french_of_gregorian dmy in
          if dmy.month <> 0 then short_f_month dmy.month else ""
      | Update.Create
          (_, Some {ci_birth_date = Some (Dgreg ({month = m}, _))})
        when m <> 0 ->
          string_of_int m
      | _ -> ""
      end
  | "birth_place" ->
      begin match c with
        Update.Create (_, Some {ci_birth_place = pl}) -> Util.escape_html pl
      | _ -> ""
      end
  | "birth_year" ->
      begin match c with
        Update.Create (_, Some ci) ->
          begin match ci.ci_birth_date with
            Some (Dgreg (dmy, Dfrench)) ->
              let dmy = Calendar.french_of_gregorian dmy in
              add_precision (string_of_int dmy.year) dmy.prec
          | Some (Dgreg ({year = y; prec = p}, _)) ->
              add_precision (string_of_int y) p
          | Some _ -> ""
          | None -> if ci.ci_public then "p" else ""
          end
      | _ -> ""
      end
  | "death_day" ->
      begin match c with
        Update.Create
          (_, Some {ci_death_date = Some (Dgreg (dmy, Dfrench))}) ->
          let dmy = Calendar.french_of_gregorian dmy in
          if dmy.day <> 0 then string_of_int dmy.day else ""
      | Update.Create (_, Some {ci_death_date = Some (Dgreg ({day = d}, _))})
        when d <> 0 ->
          string_of_int d
      | _ -> ""
      end
  | "death_month" ->
      begin match c with
        Update.Create
          (_, Some {ci_death_date = Some (Dgreg (dmy, Dfrench))}) ->
          let dmy = Calendar.french_of_gregorian dmy in
          short_f_month dmy.month
      | Update.Create
          (_, Some {ci_death_date = Some (Dgreg ({month = m}, _))})
        when m <> 0 ->
          string_of_int m
      | _ -> ""
      end
  | "death_place" ->
      begin match c with
        Update.Create (_, Some {ci_death_place = pl}) -> Util.escape_html pl
      | _ -> ""
      end
  | "death_year" ->
      begin match c with
        Update.Create
          (_, Some {ci_death_date = Some (Dgreg (dmy, Dfrench))}) ->
          let dmy = Calendar.french_of_gregorian dmy in
          add_precision (string_of_int dmy.year) dmy.prec
      | Update.Create
          (_,
           Some {ci_death_date = Some (Dgreg ({year = y; prec = p}, _))}) ->
          add_precision (string_of_int y) p
      | Update.Create (_, Some {ci_death = death; ci_death_date = None}) ->
          begin match death with
            DeadDontKnowWhen -> "+"
          | NotDead -> "-"
          | _ -> ""
          end
      | _ -> ""
      end
  | "occupation" ->
      begin match c with
        Update.Create (_, Some {ci_occupation = occupation}) ->
          Util.escape_html occupation
      | _ -> ""
      end
  | "sex" ->
      begin match c with
        Update.Create (Male, _) -> "male"
      | Update.Create (Female, _) -> "female"
      | Update.Create (Neuter, _) -> "neuter"
      | _ -> ""
      end
  | _ -> raise Not_found
and add_precision s p =
  match p with
    Maybe -> "?" ^ s
  | Before -> "&lt;" ^ s
  | After -> "&gt;" ^ s
  | About -> "/" ^ s ^ "/"
  | _ -> s
and eval_relation_kind =
  function
    Married -> "marr"
  | NotMarried -> "not_marr"
  | Engaged -> "engaged"
  | NoSexesCheckNotMarried -> "nsck"
  | NoSexesCheckMarried -> "nsckm"
  | NoMention -> "no_ment"
and eval_special_var conf base =
  function
    ["include_perso_header"] ->
      begin match p_getint conf.env "ip" with
        Some i ->
          let has_base_loop =
            try let _ = Util.create_topological_sort conf base in false with
              Consang.TopologicalSortError _ -> true
          in
          if has_base_loop then VVstring ""
          else
            let p = poi base (Adef.iper_of_int i) in
            Perso.interp_templ_with_menu (fun _ -> ()) "perso_header" conf
              base p;
            VVstring ""
      | None -> VVstring ""
      end
  | _ -> raise Not_found
and eval_int_env var env =
  match get_env var env with
    Vint x -> str_val (string_of_int x)
  | _ -> raise Not_found
and eval_string_env var env =
  match get_env var env with
    Vstring x -> str_val (Util.escape_html x)
  | _ -> str_val ""

(* print *)

let print_foreach print_ast _eval_expr =
  let rec print_foreach env (fam, cpl, des as fcd) _ s sl _ al =
    match s :: sl with
      ["child"] -> print_foreach_child env fcd al des.children
    | ["fevent"] -> print_foreach_fevent env fcd al fam.fevents
    | ["fwitness"] -> print_foreach_fwitness env fcd al fam.fevents
    | ["witness"] -> print_foreach_witness env fcd al fam.witnesses
    | ["parent"] -> print_foreach_parent env fcd al (Gutil.parent_array cpl)
    | _ -> raise Not_found
  and print_foreach_child env fcd al arr =
    for i = 0 to max 1 (Array.length arr) - 1 do
      let env = ("cnt", Vint (i + 1)) :: env in
      List.iter (print_ast env fcd) al
    done
  and print_foreach_fevent env fcd al list =
    let rec loop first cnt =
      function
        _ :: l ->
          let env =
            ("cnt", Vint cnt) :: ("first", Vbool first) ::
            ("last", Vbool (l = [])) :: env
          in
          List.iter (print_ast env fcd) al; loop false (cnt + 1) l
      | [] -> ()
    in
    loop true 1 list
  and print_foreach_fwitness env fcd al list =
    match get_env "cnt" env with
      Vint i ->
        begin match
          (try Some (List.nth list (i - 1)) with Failure _ -> None)
        with
          Some e ->
            let rec loop first wcnt =
              function
                _ :: l ->
                  let env =
                    ("wcnt", Vint wcnt) :: ("first", Vbool first) ::
                    ("last", Vbool (l = [])) :: env
                  in
                  List.iter (print_ast env fcd) al; loop false (wcnt + 1) l
              | [] -> ()
            in
            loop true 1 (Array.to_list e.efam_witnesses)
        | None -> ()
        end
    | _ -> ()
  and print_foreach_witness env fcd al arr =
    for i = 0 to max 2 (Array.length arr) - 1 do
      let env = ("cnt", Vint (i + 1)) :: env in
      List.iter (print_ast env fcd) al
    done
  and print_foreach_parent env fcd al arr =
    for i = 0 to Array.length arr - 1 do
      let env = ("cnt", Vint (i + 1)) :: env in
      List.iter (print_ast env fcd) al
    done
  in
  print_foreach

let print_update_fam conf base fcd digest =
  match p_getenv conf.env "m" with
    Some
      ("ADD_FAM" | "ADD_FAM_OK" | "ADD_PAR" | "MOD_FAM" | "MOD_FAM_OK" |
       "MRG_DUP_FAM_Y_N" | "MRG_FAM" | "MRG_FAM_OK" | "MRG_MOD_FAM_OK") ->
      let env = ["digest", Vstring digest] in
      Hutil.interp conf "updfam"
        {Templ.eval_var = eval_var conf base;
         Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
         Templ.eval_predefined_apply = (fun _ -> raise Not_found);
         Templ.get_vother = get_vother; Templ.set_vother = set_vother;
         Templ.print_foreach = print_foreach}
        env fcd
  | _ -> Hutil.incorrect_request conf

let print_del1 conf base ifam =
  let title _ =
    let s = transl_nth conf "family/families" 0 in
    Wserver.printf "%s" (capitale (transl_decline conf "delete" s))
  in
  let p =
    match p_getint conf.env "ip" with
      Some ip -> poi base (Adef.iper_of_int ip)
    | None -> Gwdb.empty_person base (Adef.iper_of_int (-1))
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Wserver.printf "<h2>\n";
  title false;
  Wserver.printf "</h2>\n";
  Wserver.printf "\n";
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Wserver.printf "<p>\n";
  Util.hidden_env conf;
  Wserver.printf "<input type=\"hidden\" name=\"i\" value=\"%d\"%s>\n"
    (Adef.int_of_ifam ifam) conf.xhs;
  begin match p_getenv conf.env "ip" with
    Some ip ->
      Wserver.printf "<input type=\"hidden\" name=\"ip\" value=\"%s\"%s>\n" ip
        conf.xhs
  | None -> ()
  end;
  Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"DEL_FAM_OK\"%s>\n"
    conf.xhs;
  Wserver.printf "</p>\n";
  Wserver.printf "<p>\n";
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
  Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</p>\n";
  Wserver.printf "</form>\n";
  Wserver.printf "\n";
  Hutil.trailer conf

let print_inv1 conf base p ifam1 ifam2 =
  let title _ =
    Wserver.printf "%s" (capitale (transl_decline conf "invert" ""))
  in
  let cpl1 = foi base ifam1 in
  let cpl2 = foi base ifam2 in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Wserver.printf "%s%s"
    (capitale (transl conf "invert the order of the following families"))
    (Util.transl conf ":");
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>\n";
  Update.print_someone conf base (poi base (get_father cpl1));
  Wserver.printf " %s " (transl_nth conf "and" 0);
  Update.print_someone conf base (poi base (get_mother cpl1));
  Wserver.printf "</li>\n";
  Wserver.printf "<li>\n";
  Update.print_someone conf base (poi base (get_father cpl2));
  Wserver.printf " %s " (transl_nth conf "and" 0);
  Update.print_someone conf base (poi base (get_mother cpl2));
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n";
  Wserver.printf "\n";
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Wserver.printf "<p>\n";
  Util.hidden_env conf;
  Wserver.printf "<input type=\"hidden\" name=\"i\" value=\"%d\"%s>\n"
    (Adef.int_of_iper (get_key_index p)) conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"f\" value=\"%d\"%s>\n"
    (Adef.int_of_ifam ifam2) conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"INV_FAM_OK\"%s>\n"
    conf.xhs;
  Wserver.printf "</p>\n";
  Wserver.printf "<p>\n";
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
  Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</p>\n";
  Wserver.printf "</form>\n";
  Wserver.printf "\n";
  Hutil.trailer conf

let print_add conf base =
  let (fath, moth, digest) =
    match p_getint conf.env "ip" with
      Some i ->
        let p = poi base (Adef.iper_of_int i) in
        let fath =
          if get_sex p = Male ||
             get_sex p = Neuter && p_getenv conf.env "sex" = Some "M"
          then
            person_key base (get_key_index p)
          else "", "", 0, Update.Create (Male, None), ""
        in
        let moth =
          if get_sex p = Female ||
             get_sex p = Neuter && p_getenv conf.env "sex" = Some "F"
          then
            person_key base (get_key_index p)
          else "", "", 0, Update.Create (Female, None), ""
        in
        let digest = string_of_int (Array.length (get_family p)) in
        fath, moth, digest
    | None ->
        ("", "", 0, Update.Create (Male, None), ""),
        ("", "", 0, Update.Create (Female, None), ""), ""
  in
  let fam =
    {marriage = Adef.cdate_None; marriage_place = ""; marriage_note = "";
     marriage_src = ""; witnesses = [| |]; relation = Married;
     divorce = NotDivorced; fevents = []; comment = ""; origin_file = "";
     fsources = default_source conf; fam_index = bogus_family_index}
  and cpl = Gutil.couple conf.multi_parents fath moth
  and des = {children = [| |]} in
  print_update_fam conf base (fam, cpl, des) digest

let print_add_parents conf base =
  match p_getint conf.env "ip" with
    Some i ->
      let p = poi base (Adef.iper_of_int i) in
      let fam =
        {marriage = Adef.cdate_None; marriage_place = ""; marriage_note = "";
         marriage_src = ""; witnesses = [| |]; relation = Married;
         divorce = NotDivorced; fevents = []; comment = ""; origin_file = "";
         fsources = default_source conf; fam_index = bogus_family_index}
      and cpl =
        Gutil.couple conf.multi_parents
          ("", sou base (get_surname p), 0, Update.Create (Neuter, None), "")
          ("", "", 0, Update.Create (Neuter, None), "")
      and des =
        {children =
          [| sou base (get_first_name p), sou base (get_surname p), get_occ p,
             Update.Link, "" |]}
      in
      print_update_fam conf base (fam, cpl, des) ""
  | _ -> Hutil.incorrect_request conf

let print_mod conf base =
  match p_getint conf.env "i" with
    Some i ->
      let sfam = string_family_of conf base (Adef.ifam_of_int i) in
      let digest = Update.digest_family sfam in
      print_update_fam conf base sfam digest
  | _ -> Hutil.incorrect_request conf

let print_del conf base =
  match p_getint conf.env "i" with
    Some i -> print_del1 conf base (Adef.ifam_of_int i)
  | _ -> Hutil.incorrect_request conf

let rec find_families ifam =
  function
    ifam1 :: ifam2 :: ifaml ->
      if ifam2 = ifam then Some (ifam1, ifam2)
      else find_families ifam (ifam2 :: ifaml)
  | _ -> None

let print_inv conf base =
  match p_getint conf.env "i", p_getint conf.env "f" with
    Some ip, Some ifam ->
      let u = poi base (Adef.iper_of_int ip) in
      begin match
        find_families (Adef.ifam_of_int ifam) (Array.to_list (get_family u))
      with
        Some (ifam1, ifam2) ->
          let p = poi base (Adef.iper_of_int ip) in
          print_inv1 conf base p ifam1 ifam2
      | _ -> Hutil.incorrect_request conf
      end
  | _ -> Hutil.incorrect_request conf

let change_order u ifam n =
  let rec loop i =
    function
      [] -> if i = n then [ifam] else []
    | fam :: faml ->
        if ifam = fam then
          if i = n then ifam :: loop (i + 1) (fam :: faml) else loop i faml
        else if i = n then ifam :: loop (i + 1) (fam :: faml)
        else fam :: loop (i + 1) faml
  in
  loop 1 (Array.to_list (get_family u))

let print_change_order conf base =
  match
    p_getint conf.env "i", p_getint conf.env "f", p_getint conf.env "n"
  with
    Some ip, Some ifam, Some n ->
      let p = poi base (Adef.iper_of_int ip) in
      let print_list arr diff_arr =
        Array.iteri
          (fun i ifam ->
             let fam = foi base ifam in
             let sp = Gutil.spouse (get_key_index p) fam in
             let sp = poi base sp in
             Wserver.printf "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             Wserver.printf "%s%s" (p_first_name base p)
               (if get_occ p = 0 then ""
                else "." ^ string_of_int (get_occ p));
             Wserver.printf "  &amp;";
             Wserver.printf "%s\n"
               (Date.short_marriage_date_text conf base fam p sp);
             Wserver.printf "%s%s %s" (p_first_name base sp)
               (if get_occ sp = 0 then ""
                else "." ^ string_of_int (get_occ sp))
               (p_surname base sp);
             Wserver.printf "\n";
             Wserver.printf "</li>\n")
          arr
      in
      let after = change_order p (Adef.ifam_of_int ifam) n in
      let (before, after) = get_family p, Array.of_list after in
      let (bef_d, aft_d) = Difference.f before after in
      let title _ =
        Wserver.printf "%s" (capitale (transl_decline conf "invert" ""))
      in
      Perso.interp_templ_with_menu title "perso_header" conf base p;
      Wserver.printf "<h2>\n";
      title false;
      Wserver.printf "</h2>\n";
      Wserver.printf "%s%s"
        (capitale (transl conf "invert the order of the following families"))
        (Util.transl conf ":");
      Wserver.printf "<table style=\"margin:1em\">\n";
      Wserver.printf "<tr>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "<td>\n";
      Wserver.printf "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Wserver.printf "</ul>\n";
      Wserver.printf "</td>\n";
      Wserver.printf "</tr>\n";
      Wserver.printf "</table>\n";
      Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
      Wserver.printf "<p>\n";
      Util.hidden_env conf;
      Wserver.printf "<input type=\"hidden\" name=\"i\" value=\"%d\"%s>\n" ip
        conf.xhs;
      Wserver.printf "<input type=\"hidden\" name=\"f\" value=\"%d\"%s>\n"
        ifam conf.xhs;
      Wserver.printf "<input type=\"hidden\" name=\"n\" value=\"%d\"%s>\n" n
        conf.xhs;
      Wserver.printf
        "<input type=\"hidden\" name=\"m\" value=\"CHG_FAM_ORD_OK\"%s>\n"
        conf.xhs;
      Wserver.printf "</p>\n";
      Wserver.printf "<p>\n";
      Wserver.printf
        "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
      Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
      Wserver.printf "</button>\n";
      Wserver.printf "</p>\n";
      Wserver.printf "</form>\n";
      Wserver.printf "\n";
      Hutil.trailer conf
  | _ -> Hutil.incorrect_request conf

let print_change_event_order conf base =
  match p_getint conf.env "i" with
    Some i ->
      let sfam = string_family_of conf base (Adef.ifam_of_int i) in
      Hutil.interp conf "updfamevt"
        {Templ.eval_var = eval_var conf base;
         Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
         Templ.eval_predefined_apply = (fun _ -> raise Not_found);
         Templ.get_vother = get_vother; Templ.set_vother = set_vother;
         Templ.print_foreach = print_foreach}
        [] sfam
  | _ -> Hutil.incorrect_request conf
