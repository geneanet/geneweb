(* $Id: gwcomp.ml,v 5.12 2018-09-27 13:03:13 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def

let magic_gwo = "GnWo000o"

(* Option qui force a créé les clés des individus. De fait, *)
(* si la clé est incomplète, on l'enregistre tout de même.  *)
let create_all_keys = ref false

type key = { pk_first_name : string; pk_surname : string; pk_occ : int }

type somebody =
    Undefined of key
  | Defined of (iper, string) gen_person

type gw_syntax =
    Family of
      somebody gen_couple * sex * sex * (somebody * sex) list *
        (string gen_fam_event_name * cdate * string * string * string *
           string * (somebody * sex * witness_kind) list)
          list *
        ((iper, string) gen_person, string) gen_family *
        (iper, string) gen_person gen_descend
  | Notes of key * string
  | Relations of somebody * sex * (somebody, string) gen_relation list
  | Pevent of
      somebody * sex *
        (string gen_pers_event_name * cdate * string * string * string *
           string * (somebody * sex * witness_kind) list)
          list
  | Bnotes of string * string
  | Wnotes of string * string

type encoding = E_utf_8 | E_iso_8859_1

let copy_decode s i1 i2 =
  let len =
    let rec loop len i =
      if i >= i2 then len
      else if i = i2 - 1 then len + 1
      else if s.[i] = '\\' then loop (len + 1) (i + 2)
      else loop (len + 1) (i + 1)
    in
    loop 0 i1
  in
  let rec loop_copy t i j =
    if i >= i2 then Bytes.unsafe_to_string t
    else if i = i2 - 1 && s.[i] <> '_' then
      begin Bytes.set t j s.[i]; Bytes.unsafe_to_string t end
    else
      let (c, i) =
        match s.[i] with
          '_' -> ' ', i
        | '\\' -> s.[i+1], i + 1
        | x -> x, i
      in
      Bytes.set t j c; loop_copy t (succ i) (succ j)
  in
  loop_copy (Bytes.create len) i1 0

let fields str =
  let rec loop beg i =
    if i < String.length str then
      match str.[i] with
        ' ' | '\t' ->
          if beg = i then loop (succ beg) (succ i)
          else copy_decode str beg i :: loop (succ i) (succ i)
      | _ -> loop beg (succ i)
    else if beg = i then []
    else [copy_decode str beg i]
  in
  loop 0 0

let cut_space x =
  let len = String.length x in
  if len = 0 then x
  else if x = " " then ""
  else
    let start = if x.[0] = ' ' then 1 else 0 in
    let stop = if x.[len-1] = ' ' then len - 1 else len in
    if start = 0 && stop = len then x else String.sub x start (stop - start)

let get_field lab l =
  match l with
    lab1 :: x :: l' when lab1 = lab -> cut_space x, l'
  | _ -> "", l

let date_of_string s i =
  let champ i =
    let (neg, i) =
      if i < String.length s && s.[i] = '-' then true, i + 1 else false, i
    in
    let rec loop i n =
      if i = String.length s then (if neg then -n else n), i
      else
        match s.[i] with
          '0'..'9' as c ->
            loop (succ i) (10 * n + Char.code c - Char.code '0')
        | _ -> (if neg then -n else n), i
    in
    loop i 0
  in
  let skip_slash i =
    if i < String.length s && s.[i] = '/' then Some (succ i) else None
  in
  let (precision, i) =
    match s.[i] with
      '~' -> About, succ i
    | '?' -> Maybe, succ i
    | '>' -> After, succ i
    | '<' -> Before, succ i
    | _ -> Sure, i
  in
  let (undefined, year, i) =
    let (year, j) = champ i in
    if j = i + 1 && s.[i] = '0' then true, year, j else false, year, j
  in
  let error n = failwith (Printf.sprintf "date_of_string%d %s" n s) in
  let dmy2 year2 i =
    match skip_slash i with
      Some i ->
        let month2 = year2 in
        let (year2, i) = champ i in
        begin match skip_slash i with
          Some i ->
            let day2 = month2 in
            let month2 = year2 in
            let (year2, i) = champ i in
            if month2 < 1 || month2 > 13 then error 2
            else if day2 < 1 || day2 > 31 then error 3
            else (day2, month2, year2), i
        | None ->
            if month2 < 1 || month2 > 13 then error 4
            else (0, month2, year2), i
        end
    | None -> (0, 0, year2), i
  in
  let date =
    match skip_slash i with
      Some i ->
        let month = year in
        let (year, i) = champ i in
        begin match skip_slash i with
          Some i ->
            let day = month in
            let month = year in
            let (year, i) = champ i in
            (*
                        if year = 0 then if i = String.length s then None else error 1
                        else
            *)
            if month < 1 || month > 13 then error 2
            else if day < 1 || day > 31 then error 3
            else
              let d =
                {day = day; month = month; year = year; prec = precision;
                 delta = 0}
              in
              Some (Dgreg (d, Dgregorian), i)
        | None ->
            if year = 0 then None
            else if month < 1 || month > 13 then error 4
            else
              let d =
                {day = 0; month = month; year = year; prec = precision;
                 delta = 0}
              in
              Some (Dgreg (d, Dgregorian), i)
        end
    | None ->
        if undefined then
          if i = String.length s then None
          else if s.[i] = '(' && s.[String.length s - 1] = ')' then
            let txt = String.sub s (i + 1) (String.length s - i - 2) in
            let txt = cut_space txt in Some (Dtext txt, String.length s)
          else failwith ("date_of_string " ^ s)
        else
          let d =
            {day = 0; month = 0; year = year; prec = precision; delta = 0}
          in
          Some (Dgreg (d, Dgregorian), i)
  in
  let date =
    match date with
      Some ((Dgreg (d, cal) as dt), i) ->
        if i = String.length s then Some (dt, i)
        else if s.[i] = '|' then
          let (year2, i) = champ (succ i) in
          let ((day2, month2, year2), i) = dmy2 year2 i in
          let dmy2 =
            {day2 = day2; month2 = month2; year2 = year2; delta2 = 0}
          in
          Some (Dgreg ({d with prec = OrYear dmy2}, cal), i)
        else if i + 1 < String.length s && s.[i] = '.' && s.[i+1] = '.' then
          let (year2, i) = champ (i + 2) in
          let ((day2, month2, year2), i) = dmy2 year2 i in
          let dmy2 =
            {day2 = day2; month2 = month2; year2 = year2; delta2 = 0}
          in
          Some (Dgreg ({d with prec = YearInt dmy2}, cal), i)
        else Some (dt, i)
    | Some ((Dtext _ as dt), i) -> Some (dt, i)
    | None -> None
  in
  let date =
    match date with
      Some (Dgreg (d, _), i) ->
        if i = String.length s then Some (Dgreg (d, Dgregorian), i)
        else
          begin match s.[i] with
            'G' -> Some (Dgreg (d, Dgregorian), i + 1)
          | 'J' ->
              let d = Calendar.gregorian_of_julian d in
              Some (Dgreg (d, Djulian), i + 1)
          | 'F' ->
              let d = Calendar.gregorian_of_french d in
              Some (Dgreg (d, Dfrench), i + 1)
          | 'H' ->
              let d = Calendar.gregorian_of_hebrew d in
              Some (Dgreg (d, Dhebrew), i + 1)
          | _ -> Some (Dgreg (d, Dgregorian), i)
          end
    | d -> d
  in
  match date with
    Some (dt, i) -> if i = String.length s then Some dt else error 5
  | None -> None

let line_cnt = ref 0
let no_fail = ref false
let no_picture = ref false

let input_line0 ic =
  let line = input_line ic in
  incr line_cnt;
  if String.length line > 0 && line.[String.length line - 1] = '\r' then
    String.sub line 0 (String.length line - 1)
  else line

let input_a_line (ic, encoding) =
  let line = input_line0 ic in
  match encoding with
    E_utf_8 -> line
  | E_iso_8859_1 -> Mutil.utf_8_of_iso_8859_1 line

let rec input_real_line ic =
  let x = input_a_line ic in
  if x = "" || x.[0] = '#' then input_real_line ic else x

let get_optional_birthdate l =
  match l with
    x :: l' ->
      let i = 0 in
      if x.[i] = '!' then None, l
      else
        begin match x.[i] with
          '~' | '?' | '<' | '>' | '-' | '0'..'9' ->
            let d = date_of_string x i in Some d, l'
        | _ -> None, l
        end
  | _ -> None, l

let get_optional_baptdate l =
  match l with
    x :: l' ->
      let i = 0 in
      if x.[i] = '!' then
        let i = succ i in
        match x.[i] with
          '~' | '?' | '<' | '>' | '-' | '0'..'9' ->
            let d = date_of_string x i in Some d, l'
        | _ -> None, l
      else None, l
  | _ -> None, l

let get_optional_deathdate l =
  match l with
    "?" :: l' -> Some DontKnowIfDead, l'
  | "mj" :: l' -> Some DeadYoung, l'
  | "od" :: l' -> Some OfCourseDead, l'
  | x :: l' ->
      let i = 0 in
      let (dr, i) =
        match x.[i] with
          'k' -> Killed, i + 1
        | 'm' -> Murdered, i + 1
        | 'e' -> Executed, i + 1
        | 's' -> Disappeared, i + 1
        | _ -> Unspecified, i
      in
      if i < String.length x then
        match x.[i] with
          '~' | '?' | '>' | '<' | '-' | '0'..'9' ->
            let d =
              match date_of_string x i with
                None -> DeadDontKnowWhen
              | Some d -> Death (dr, Adef.cdate_of_date d)
            in
            Some d, l'
        | _ -> None, l
      else None, l
  | _ -> None, l

let get_burial l =
  match l with
    "#buri" :: l ->
      begin match l with
        x :: l' ->
          let i = 0 in
          let (od, l) =
            match x.[i] with
              '~' | '?' | '>' | '<' | '-' | '0'..'9' -> date_of_string x i, l'
            | _ -> None, l
          in
          Buried (Adef.cdate_of_od od), l
      | [] -> Buried Adef.cdate_None, l
      end
  | "#crem" :: l ->
      begin match l with
        x :: l' ->
          let i = 0 in
          let (od, l) =
            match x.[i] with
              '~' | '?' | '>' | '<' | '-' | '0'..'9' -> date_of_string x i, l'
            | _ -> None, l
          in
          Cremated (Adef.cdate_of_od od), l
      | [] -> Cremated Adef.cdate_None, l
      end
  | _ -> UnknownBurial, l

let get_optional_sexe =
  function
    "h" :: l -> Male, l
  | "f" :: l -> Female, l
  | l -> Neuter, l

let make_int x =
  let rec loop found n i =
    if i = String.length x then if found then n else raise Not_found
    else
      match x.[i] with
        '0'..'9' as c ->
          loop true (10 * n + Char.code c - Char.code '0') (succ i)
      | _ -> raise Not_found
  in
  loop false 0

let get_fst_name str l =
  match l with
    x :: l' ->
      begin match x.[0] with
      (*'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' *)
        'a'..'z' | 'A'..'Z' | '\xE0'..'\xFF' | '\xC0'..'\xDD' |
        '[' | '0'..'9' | '?' | ' ' ->
          let x = cut_space x in
          let (x, occ) =
            match String.rindex_opt x '.' with
              Some i ->
                begin try String.sub x 0 i, make_int x (succ i) with
                  Not_found -> x, 0
                end
            | None -> x, 0
          in
          x, occ, l'
      | _ -> failwith str
      end
  | _ -> failwith str

let rec get_fst_names_aliases str l =
  match l with
    x :: l' ->
      if x.[0] = '{' && x.[String.length x - 1] = '}' then
        let n = String.sub x 1 (String.length x - 2) in
        let (nl, l) = get_fst_names_aliases str l' in cut_space n :: nl, l
      else [], l
  | [] -> [], l

let rec get_surnames_aliases str l =
  match l with
    "#salias" :: x :: l' ->
      let (nl, l) = get_surnames_aliases str l' in cut_space x :: nl, l
  | _ -> [], l

let rec get_qualifiers str l =
  match l with
    "#nick" :: x :: l' ->
      let (nl, l) = get_qualifiers str l' in cut_space x :: nl, l
  | _ -> [], l

let rec get_aliases str l =
  match l with
    "#alias" :: x :: l' ->
      let (nl, l) = get_aliases str l' in cut_space x :: nl, l
  | _ -> [], l

let get_name l =
  match l with
  | "#nick" :: _ | "#alias" :: _ -> "", l
  | x :: l' ->
    begin match x.[0] with
      |  '{' -> "", l
      (*'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' *)
      | 'a'..'z' | 'A'..'Z' | '\xE0'..'\xFF' | '\xC0'..'\xDD' |
        '0'..'9' | '?' | ' ' ->
        cut_space x, l'
      | _ -> "", l
    end
  | _ -> "", l

let get_pub_name l =
  match l with
  | x :: l' ->
    if x.[0] = '(' && x.[String.length x - 1] = ')' then
      let a = String.sub x 1 (String.length x - 2) in cut_space a, l'
    else "", l
  | _ -> "", l

let get_image l =
  match l with
  | ("#image" | "#photo") :: x :: l' ->
    if !no_picture then "", l' else cut_space x, l'
  | _ -> "", l

let get_occu l =
  match l with
  | "#occu" :: x :: l' -> cut_space x, l'
  | _ -> "", l

let get_sources l =
  match l with
    "#src" :: x :: l' -> cut_space x, l'
  | _ -> "", l

let get_access l =
  match l with
    "#apubl" :: l' -> Public, l'
  | "#apriv" :: l' -> Private, l'
  | _ -> IfTitles, l

let scan_title t =
  let next_field i =
    let rec loop s i =
      if i < String.length t then
        match t.[i] with
          ':' -> s, i + 1
        | '\\' -> loop (s ^ String.make 1 t.[i+1]) (i + 2)
        | c -> loop (s ^ String.make 1 c) (i + 1)
      else s, i
    in
    loop "" i
  in
  let i = 0 in
  let (name, i) =
    let (s, i) = next_field i in
    if i = String.length t then failwith t else s, i
  in
  let name =
    match name with
      "" -> Tnone
    | "*" -> Tmain
    | _ -> Tname name
  in
  let (title, i) =
    let (s, i) = next_field i in if t.[i-1] <> ':' then failwith t else s, i
  in
  let (place, i) = next_field i in
  let (date_start, i) =
    let (d, i) = next_field i in
    (if d = "" then None else date_of_string d 0), i
  in
  let (date_end, i) =
    let (d, i) = next_field i in
    (if d = "" then None else date_of_string d 0), i
  in
  let (nth, i) =
    let (d, i) = next_field i in (if d = "" then 0 else int_of_string d), i
  in
  if i <> String.length t then failwith t
  else
    {t_name = name; t_ident = title; t_place = place;
     t_date_start = Adef.cdate_of_od date_start;
     t_date_end = Adef.cdate_of_od date_end; t_nth = nth}

let rec get_titles str l =
  match l with
    x :: l' ->
      if x.[0] = '[' && x.[String.length x - 1] = ']' then
        let t = String.sub x 1 (String.length x - 2) in
        let t = scan_title t in
        let (al, l') = get_titles str l' in
        (if t.t_ident = "" then al else t :: al), l'
      else [], l
  | _ -> [], l

let get_pevent_name str l =
  match l with
    "#birt" :: l' -> Epers_Birth, l'
  | "#bapt" :: l' -> Epers_Baptism, l'
  | "#deat" :: l' -> Epers_Death, l'
  | "#buri" :: l' -> Epers_Burial, l'
  | "#crem" :: l' -> Epers_Cremation, l'
  | "#acco" :: l' -> Epers_Accomplishment, l'
  | "#acqu" :: l' -> Epers_Acquisition, l'
  | "#adhe" :: l' -> Epers_Adhesion, l'
  | "#awar" :: l' -> Epers_Decoration, l'
  | "#bapl" :: l' -> Epers_BaptismLDS, l'
  | "#barm" :: l' -> Epers_BarMitzvah, l'
  | "#basm" :: l' -> Epers_BatMitzvah, l'
  | "#bles" :: l' -> Epers_Benediction, l'
  | "#cens" :: l' -> Epers_Recensement, l'
  | "#chgn" :: l' -> Epers_ChangeName, l'
  | "#circ" :: l' -> Epers_Circumcision, l'
  | "#conf" :: l' -> Epers_Confirmation, l'
  | "#conl" :: l' -> Epers_ConfirmationLDS, l'
  | "#degr" :: l' -> Epers_Diploma, l'
  | "#demm" :: l' -> Epers_DemobilisationMilitaire, l'
  | "#dist" :: l' -> Epers_Distinction, l'
  | "#dotl" :: l' -> Epers_DotationLDS, l'
  | "#educ" :: l' -> Epers_Education, l'
  | "#elec" :: l' -> Epers_Election, l'
  | "#emig" :: l' -> Epers_Emigration, l'
  | "#endl" :: l' -> Epers_Dotation, l'
  | "#exco" :: l' -> Epers_Excommunication, l'
  | "#fcom" :: l' -> Epers_FirstCommunion, l'
  | "#flkl" :: l' -> Epers_FamilyLinkLDS, l'
  | "#fune" :: l' -> Epers_Funeral, l'
  | "#grad" :: l' -> Epers_Graduate, l'
  | "#hosp" :: l' -> Epers_Hospitalisation, l'
  | "#illn" :: l' -> Epers_Illness, l'
  | "#immi" :: l' -> Epers_Immigration, l'
  | "#lpas" :: l' -> Epers_ListePassenger, l'
  | "#mdis" :: l' -> Epers_MilitaryDistinction, l'
  | "#mobm" :: l' -> Epers_MobilisationMilitaire, l'
  | "#mpro" :: l' -> Epers_MilitaryPromotion, l'
  | "#mser" :: l' -> Epers_MilitaryService, l'
  | "#natu" :: l' -> Epers_Naturalisation, l'
  | "#occu" :: l' -> Epers_Occupation, l'
  | "#ordn" :: l' -> Epers_Ordination, l'
  | "#prop" :: l' -> Epers_Property, l'
  | "#resi" :: l' -> Epers_Residence, l'
  | "#reti" :: l' -> Epers_Retired, l'
  | "#slgc" :: l' -> Epers_ScellentChildLDS, l'
  | "#slgp" :: l' -> Epers_ScellentParentLDS, l'
  | "#slgs" :: l' -> Epers_ScellentSpouseLDS, l'
  | "#vteb" :: l' -> Epers_VenteBien, l'
  | "#will" :: l' -> Epers_Will, l'
  | s :: l' ->
      if s.[0] = '#' then
        Epers_Name (String.sub s 1 (String.length s - 1)), l'
      else failwith str
  | _ -> failwith str

let get_fevent_name str l =
  match l with
    "#marr" :: l' -> Efam_Marriage, l'
  | "#nmar" :: l' -> Efam_NoMarriage, l'
  | "#nmen" :: l' -> Efam_NoMention, l'
  | "#enga" :: l' -> Efam_Engage, l'
  | "#div" :: l' -> Efam_Divorce, l'
  | "#sep" :: l' -> Efam_Separated, l'
  | "#anul" :: l' -> Efam_Annulation, l'
  | "#marb" :: l' -> Efam_MarriageBann, l'
  | "#marc" :: l' -> Efam_MarriageContract, l'
  | "#marl" :: l' -> Efam_MarriageLicense, l'
  | "#resi" :: l' -> Efam_Residence, l'
  | s :: l' ->
      if s.[0] = '#' then Efam_Name (String.sub s 1 (String.length s - 1)), l'
      else failwith str
  | _ -> failwith str

let get_optional_event_date l =
  match l with
    x :: l' ->
      let i = 0 in
      if x.[i] = '!' then None, l
      else
        begin match x.[i] with
          '~' | '?' | '<' | '>' | '-' | '0'..'9' ->
            let d = date_of_string x i in Some d, l'
        | _ -> None, l
        end
  | _ -> None, l

let get_event_witness_kind l =
  match l with
    "#godp" :: l' -> Witness_GodParent, l'
  | _ -> Witness, l

let get_mar_date str =
  function
    x :: l ->
      let (mar, l) =
        match x.[0] with
          '+' ->
            (if String.length x > 1 then
               Adef.cdate_of_od (date_of_string x 1)
             else Adef.cdate_None),
            l
        | _ -> failwith str
      in
      let (relation, l) =
        match l with
          "#nm" :: l -> (NotMarried, Male, Female), l
        | "#eng" :: l -> (Engaged, Male, Female), l
        | "#nsck" :: c :: l when String.length c = 2 ->
            let decode_sex i =
              match c.[i] with
                'm' -> Male
              | 'f' -> Female
              | _ -> Neuter
            in
            (NoSexesCheckNotMarried, decode_sex 0, decode_sex 1), l
        | "#nsckm" :: c :: l when String.length c = 2 ->
            let decode_sex i =
              match c.[i] with
                'm' -> Male
              | 'f' -> Female
              | _ -> Neuter
            in
            (NoSexesCheckMarried, decode_sex 0, decode_sex 1), l
        | "#noment" :: l -> (NoMention, Male, Female), l
        | _ -> (Married, Male, Female), l
      in
      let (place, l) = get_field "#mp" l in
      let (note, l) = get_field "#mn" l in
      let (src, l) = get_field "#ms" l in
      let (divorce, l) =
        match l with
          x :: l when x.[0] = '-' ->
            if String.length x > 1 then
              Divorced (Adef.cdate_of_od (date_of_string x 1)), l
            else Divorced Adef.cdate_None, l
        | "#sep" :: l -> Separated, l
        | _ -> NotDivorced, l
      in
      relation, mar, place, note, src, divorce, l
  | [] -> failwith str

let read_line ic =
  try let str = input_real_line ic in Some (str, fields str) with
    End_of_file -> None

let create_person () =
  {first_name = ""; surname = ""; occ = 0; image = ""; public_name = "";
   qualifiers = []; aliases = []; first_names_aliases = [];
   surnames_aliases = []; titles = []; rparents = []; related = [];
   occupation = ""; sex = Neuter; access = IfTitles; birth = Adef.cdate_None;
   birth_place = ""; birth_note = ""; birth_src = "";
   baptism = Adef.cdate_None; baptism_place = ""; baptism_note = "";
   baptism_src = ""; death = DontKnowIfDead; death_place = "";
   death_note = ""; death_src = ""; burial = UnknownBurial; burial_place = "";
   burial_note = ""; burial_src = ""; pevents = []; notes = ""; psources = "";
   key_index = Adef.iper_of_int (-1)}

let bogus_def p n = p = "?" || n = "?"

let set_infos fn sn occ sex comm_psources comm_birth_place str u l =
  let (first_names_aliases, l) = get_fst_names_aliases str l in
  let (surnames_aliases, l) = get_surnames_aliases str l in
  let (public_name, l) = get_pub_name l in
  let (image, l) = get_image l in
  let (qualifiers, l) = get_qualifiers str l in
  let (aliases, l) = get_aliases str l in
  let (titles, l) = get_titles str l in
  let (access, l) = get_access l in
  let (occupation, l) = get_occu l in
  let (psources, l) = get_sources l in
  let (naissance, l) = get_optional_birthdate l in
  let (birth_place, l) = get_field "#bp" l in
  let (birth_note, l) = get_field "#bn" l in
  let (birth_src, l) = get_field "#bs" l in
  let (baptism, l) = get_optional_baptdate l in
  let (baptism_place, l) =
    let (pp, l) = get_field "#pp" l in
    if pp = "" then get_field "#bp" l else pp, l
  in
  let (bapt_note, l) = get_field "#pn" l in
  let (bapt_src, l) = get_field "#ps" l in
  let (mort, l) = get_optional_deathdate l in
  let (death_place, l) = get_field "#dp" l in
  let (death_note, l) = get_field "#dn" l in
  let (death_src, l) = get_field "#ds" l in
  let mort =
    match naissance, mort with
      None, _ | _, Some _ | Some None, _ ->
        begin match mort with
          Some m -> m
        | None -> DontKnowIfDead
        end
    | Some _, None -> NotDead
  in
  let naissance =
    match naissance with
      None -> Adef.cdate_None
    | Some x -> Adef.cdate_of_od x
  in
  let baptism =
    match baptism with
      None -> Adef.cdate_None
    | Some x -> Adef.cdate_of_od x
  in
  let (burial, l) = get_burial l in
  let (burial_place, l) = get_field "#rp" l in
  let (burial_note, l) = get_field "#rn" l in
  let (burial_src, l) = get_field "#rs" l in
  let u =
    {first_name = fn; surname = sn; occ = occ; rparents = u.rparents;
     related = u.related; sex = sex; notes = u.notes; key_index = u.key_index;
     first_names_aliases = first_names_aliases;
     surnames_aliases = surnames_aliases; public_name = public_name;
     image = image; qualifiers = qualifiers; aliases = aliases;
     titles = titles; access = access; occupation = occupation;
     psources = if psources <> "" then psources else comm_psources;
     birth = naissance;
     birth_place =
       if birth_place <> "" then birth_place else comm_birth_place;
     birth_note = birth_note; birth_src = birth_src; baptism = baptism;
     baptism_place = baptism_place; baptism_note = bapt_note;
     baptism_src = bapt_src; death = mort; death_place = death_place;
     death_note = death_note; death_src = death_src; burial = burial;
     burial_place = burial_place; burial_note = burial_note;
     burial_src = burial_src; pevents = u.pevents}
  in
  u, l

let parse_parent str l =
  let (np, l) = get_name l in
  let (pp, op, l) = get_fst_name str l in
  let defined =
    if bogus_def pp np then true
    else
      match l with
        [] -> false
      | s :: _ when s.[0] = '+' -> false
      | _ -> true
  in
  if not defined then
    let key = {pk_first_name = pp; pk_surname = np; pk_occ = op} in
    Undefined key, np, l
  else
    let u = create_person () in
    let (u, l) = set_infos pp np op u.sex "" "" str u l in Defined u, np, l

let parse_child str surname sex csrc cbp l =
  let u = create_person () in
  let (prenom, occ, l) = get_fst_name str l in
  let (nom, l) =
    match l with
      "?" :: _ -> get_name l
    | x :: _ ->
        begin match x.[0] with
          '<' | '>' | '!' | '~' | '?' | '-' | '0'..'9' | '{' | '#' ->
            surname, l
        | '(' | '[' -> (if prenom = "" then "" else surname), l
        | _ -> get_name l
        end
    | _ -> surname, []
  in
  set_infos prenom nom occ sex csrc cbp str u l

let get_relation str =
  function
    "-" :: x :: l ->
      let rtyp =
        match x with
          "adop" | "adop:" -> Adoption
        | "reco" | "reco:" -> Recognition
        | "cand" | "cand:" -> CandidateParent
        | "godp" | "godp:" -> GodParent
        | "fost" | "fost:" -> FosterParent
        | _ -> failwith str
      in
      if String.length x = 5 && x.[4] = ':' then
        let (fk, _, l) = parse_parent str l in
        let l =
          match l with
            "+" :: l -> l
          | _ -> failwith str
        in
        let (mk, _, l) = parse_parent str l in
        if l <> [] then failwith str;
        {r_type = rtyp; r_fath = Some fk; r_moth = Some mk; r_sources = ""}
      else
        begin match l with
          "fath:" :: l ->
            let (fk, _, l) = parse_parent str l in
            if l <> [] then failwith str;
            {r_type = rtyp; r_fath = Some fk; r_moth = None; r_sources = ""}
        | "moth:" :: l ->
            let (mk, _, l) = parse_parent str l in
            if l <> [] then failwith str;
            {r_type = rtyp; r_fath = None; r_moth = Some mk; r_sources = ""}
        | _ -> failwith str
        end
  | _ -> failwith str

let read_notes ic =
  let notes =
    try
      let rec loop =
        function
          "end notes" -> ""
        | l -> l ^ "\n" ^ loop (input_a_line ic)
      in
      loop (input_a_line ic)
    with End_of_file -> failwith "end of file"
  in
  Mutil.strip_all_trailing_spaces notes

(* from version 5.00 *)
let read_notes_db ic end_txt =
  let notes =
    try
      let rec loop s =
        if s = end_txt then ""
        else
          let len = String.length s in
          let s =
            if len > 2 && s.[0] = ' ' && s.[1] = ' ' then
              String.sub s 2 (len - 2)
            else s
          in
          s ^ "\n" ^ loop (input_a_line ic)
      in
      loop (input_a_line ic)
    with End_of_file -> failwith "end of file"
  in
  Mutil.strip_all_trailing_spaces notes

type 'a read_family =
    F_some of 'a
  | F_enc_utf_8
  | F_gw_plus
  | F_none
  | F_fail of string

let loop_note line ic =
  let rec loop_note acc str =
    match fields str with
    | "note" :: tl ->
      let note =
        if tl = [] then ""
        else
          String.sub
            str
            (String.length "note" + 1)
            (String.length str - String.length "note" - 1)
      in
      loop_note (note :: acc) (input_a_line ic)
    | _ -> String.concat "\n" (List.rev @@ "" :: acc), str
  in
  loop_note [] line

let loop_witn line ic =
  let rec loop_witn acc str =
    match fields str with
      ("wit" | "wit:") :: l ->
      let (sex, l) =
        match l with
          "m:" :: l -> Male, l
        | "f:" :: l -> Female, l
        | l -> Neuter, l
      in
      let (wkind, l) = get_event_witness_kind l in
      let (wk, _, l) = parse_parent str l in
      if l <> [] then failwith str;
      loop_witn ((wk, sex, wkind) :: acc) (input_a_line ic)
    | _ -> (List.rev acc, str)
  in
  loop_witn [] line

let read_family ic fname =
  function
    Some (_, ["encoding:"; "utf-8"]) -> F_enc_utf_8
  | Some (_, ["gwplus"]) -> F_gw_plus
  | Some (str, "fam" :: l) ->
      let (fath_key, surname, l) = parse_parent str l in
      let (relation_ss, marriage, marr_place, marr_note, marr_src, divorce,
       l) =
        get_mar_date str l
      in
      let (relation, fath_sex, moth_sex) = relation_ss in
      let (moth_key, _, l) = parse_parent str l in
      if l <> [] then failwith str;
      let line = read_line ic in
      let (witn, line) =
        let rec loop =
          function
            Some (str, ("wit" | "wit:") :: l) ->
              let (sex, l) =
                match l with
                  "m:" :: l -> Male, l
                | "f:" :: l -> Female, l
                | l -> Neuter, l
              in
              let (wk, _, l) = parse_parent str l in
              if l <> [] then failwith str;
              let (witn, line) = loop (read_line ic) in
              (wk, sex) :: witn, line
          | line -> [], line
        in
        loop line
      in
      let (fsrc, line) =
        match line with
          Some (_, ["src"; x]) -> cut_space x, read_line ic
        | Some (str, "src" :: _) -> failwith str
        | _ -> "", line
      in
      let (csrc, line) =
        match line with
          Some (_, ["csrc"; x]) -> cut_space x, read_line ic
        | Some (str, "csrc" :: _) -> failwith str
        | _ -> "", line
      in
      let (cbp, line) =
        match line with
          Some (_, ["cbp"; x]) -> cut_space x, read_line ic
        | Some (str, "cbp" :: _) -> failwith str
        | _ -> "", line
      in
      let co = Adef.couple fath_key moth_key in
      let (comm, line) =
        match line with
          Some (str, "comm" :: _) ->
            let comm = String.sub str 5 (String.length str - 5) in
            comm, read_line ic
        | _ -> "", line
      in
      let (fevents, line) =
        match line with
          Some (_, "fevt" :: _) ->
            let (fevents, line) =
              let rec loop fevents =
                function
                  "end fevt" -> fevents, read_line ic
                | x ->
                    let (str, l) = x, fields x in
                    (* On récupère le nom, date, lieu, source, cause *)
                    let (name, l) = get_fevent_name str l in
                    let (date, l) = get_optional_event_date l in
                    let (place, l) = get_field "#p" l in
                    let (cause, l) = get_field "#c" l in
                    let (src, l) = get_field "#s" l in
                    let date =
                      match date with
                        None -> Adef.cdate_None
                      | Some x -> Adef.cdate_of_od x
                    in
                    if l <> [] then failwith str;
                    (* On récupère les témoins *)
                    let (witn, line) = loop_witn (input_a_line ic) ic in
                    (* On récupère les notes *)
                    let (notes, line) = loop_note line ic in
                    let notes = Mutil.strip_all_trailing_spaces notes in
                    let evt = name, date, place, cause, src, notes, witn in
                    loop (evt :: fevents) line
              in
              loop [] (input_a_line ic)
            in
            List.rev fevents, line
        | _ -> [], line
      in
      begin match line with
        Some (_, ["beg"]) ->
          let cles_enfants =
            let rec loop children =
              match read_line ic with
                Some (str, "-" :: l) ->
                  let (sex, l) = get_optional_sexe l in
                  let (child, l) = parse_child str surname sex csrc cbp l in
                  if l <> [] then failwith str else loop (child :: children)
              | Some (_, ["end"]) -> children
              | Some (str, _) -> failwith str
              | _ -> failwith "eof"
            in
            List.rev (loop [])
          in
          let fo =
            {marriage = marriage; marriage_place = marr_place;
             marriage_note = marr_note; marriage_src = marr_src;
             witnesses = [| |]; relation = relation; divorce = divorce;
             fevents = []; comment = comm; origin_file = fname;
             fsources = fsrc; fam_index = Adef.ifam_of_int (-1)}
          in
          let deo = {children = Array.of_list cles_enfants} in
          F_some
            (Family (co, fath_sex, moth_sex, witn, fevents, fo, deo),
             read_line ic)
      | line ->
          let fo =
            {marriage = marriage; marriage_place = marr_place;
             marriage_note = marr_note; marriage_src = marr_src;
             witnesses = [| |]; relation = relation; divorce = divorce;
             fevents = []; comment = comm; origin_file = fname;
             fsources = fsrc; fam_index = Adef.ifam_of_int (-1)}
          in
          let deo = {children = [| |]} in
          F_some
            (Family (co, fath_sex, moth_sex, witn, fevents, fo, deo), line)
      end
  | Some (_, ["notes-db"]) ->
      let notes = read_notes_db ic "end notes-db" in
      F_some (Bnotes ("", notes), read_line ic)
  | Some (str, ["page-ext"; _]) ->
      let p =
        let len = String.length "page-ext" + 1 in
        String.sub str len (String.length str - len)
      in
      let notes = read_notes_db ic "end page-ext" in
      F_some (Bnotes (p, notes), read_line ic)
  | Some (_, ["notes"]) ->
      (* used before version 5.00 *)
      let notes = read_notes ic in F_some (Bnotes ("", notes), read_line ic)
  | Some (str, "notes" :: l) ->
      let (surname, l) = get_name l in
      let (first_name, occ, l) = get_fst_name str l in
      if l <> [] then failwith "str"
      else
        begin match read_line ic with
          Some (_, ["beg"]) ->
            let notes = read_notes ic in
            let key =
              {pk_first_name = first_name; pk_surname = surname; pk_occ = occ}
            in
            F_some (Notes (key, notes), read_line ic)
        | Some (str, _) -> failwith str
        | None -> failwith "end of file"
        end
  | Some (str, "wizard-note" :: _) ->
      let wizid =
        let len = String.length "wizard-note " in
        String.sub str len (String.length str - len)
      in
      let notes = read_notes_db ic "end wizard-note" in
      F_some (Wnotes (wizid, notes), read_line ic)
  | Some (str, "rel" :: l) ->
      let (sb, _, l) = parse_parent str l in
      let (sex, l) =
        match l with
          "#h" :: l -> Male, l
        | "#f" :: l -> Female, l
        | l -> Neuter, l
      in
      if l <> [] then failwith "str"
      else
        begin match read_line ic with
          Some (_, ["beg"]) ->
            let rl =
              try
                let rec loop =
                  function
                    "end" -> []
                  | x -> get_relation x (fields x) :: loop (input_a_line ic)
                in
                loop (input_a_line ic)
              with End_of_file -> failwith "missing end rel"
            in
            F_some (Relations (sb, sex, rl), read_line ic)
        | Some (str, _) -> failwith str
        | None -> failwith "end of file"
        end
  | Some (str, "pevt" :: l) ->
      let (sb, _, l) = parse_parent str l in
      if l <> [] then failwith str
      else
        let pevents =
          let rec loop pevents =
            function
              "end pevt" -> pevents
            | x ->
                let (str, l) = x, fields x in
                (* On récupère le nom, date, lieu, source, cause *)
                let (name, l) = get_pevent_name str l in
                let (date, l) = get_optional_event_date l in
                let (place, l) = get_field "#p" l in
                let (cause, l) = get_field "#c" l in
                let (src, l) = get_field "#s" l in
                let date =
                  match date with
                    None -> Adef.cdate_None
                  | Some x -> Adef.cdate_of_od x
                in
                if l <> [] then failwith str;
                (* On récupère les témoins *)
                let (witn, line) = loop_witn (input_a_line ic) ic in
                (* On récupère les notes *)
                let (notes, line) = loop_note line ic in
                let notes = Mutil.strip_all_trailing_spaces notes in
                let evt = name, date, place, cause, src, notes, witn in
                loop (evt :: pevents) line
          in
          loop [] (input_a_line ic)
        in
        let pevents = List.rev pevents in
        F_some (Pevent (sb, Neuter, pevents), read_line ic)
  | Some (str, _) -> failwith str
  | None -> F_none

let read_family_1 ic fname line =
  if !no_fail then
    try read_family ic fname line with Failure str -> F_fail str
  else read_family ic fname line

let comp_families x =
  let out_file = Filename.chop_suffix x ".gw" ^ ".gwo" in
  line_cnt := 0;
  let oc = open_out_bin out_file in
  begin try
    let ic = open_in x in
    output_string oc magic_gwo;
    output_value oc (x : string);
    let rec loop line encoding =
      match read_family_1 (ic, encoding) x line with
        F_some (family, line) ->
          output_value oc (family : gw_syntax); loop line encoding
      | F_enc_utf_8 -> loop (read_line (ic, E_utf_8)) E_utf_8
      | F_gw_plus ->
          create_all_keys := true; loop (read_line (ic, encoding)) encoding
      | F_none -> ()
      | F_fail str ->
          Printf.printf "File \"%s\", line %d:\n" x !line_cnt;
          Printf.printf "Error: %s\n" str;
          flush stdout;
          loop (read_line (ic, encoding)) encoding
    in
    loop (read_line (ic, E_iso_8859_1)) E_iso_8859_1; close_in ic
  with e ->
    close_out oc; (try Sys.remove out_file with Sys_error _ -> ()); raise e
  end;
  close_out oc
