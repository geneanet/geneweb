(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open TemplAst
open Util
open HistoryDiff

(* ************************************************************************ *)
(*  [Fonc] print_clean : config -> unit                                     *)
(** [Description] :
    [Args] :
      - conf : configuration de la base
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let print_clean conf =
  match p_getenv conf.env "f" with
    Some f when f <> "" ->
      let title _ =
        Wserver.printf "%s" (Utf8.capitalize (transl conf "clean history"))
      in
      Hutil.header conf title;
      Hutil.print_link_to_welcome conf true;
      Util.gen_print_tips conf
        (Utf8.capitalize
           (transl conf
              "select the input you want to erase from the history"));
      let history = load_person_history conf f in
      Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
      Wserver.printf
        "<input type=\"hidden\" name=\"m\" value=\"HIST_CLEAN_OK\"%s>\n"
        conf.xhs;
      Wserver.printf "<input type=\"hidden\" name=\"f\" value=\"%s\"%s>\n" f
        conf.xhs;
      Wserver.printf "<ul>\n";
      begin let rec loop i =
        function
          [] -> ()
        | gr :: l ->
            Wserver.printf "<li>\n";
            Wserver.printf "<label>\n";
            Wserver.printf
              "<input type=\"checkbox\" name=\"i%d\" value=\"on\"%s>\n" i
              conf.xhs;
            Wserver.printf "%s %s" gr.date gr.HistoryDiff.wizard;
            Wserver.printf "</label>\n";
            Wserver.printf "</li>\n";
            loop (i + 1) l
      in
        loop 0 history
      end;
      Wserver.printf "</ul>\n";
      Wserver.printf
        "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
      Wserver.printf "%s" (Utf8.capitalize (transl_nth conf "validate/delete" 0));
      Wserver.printf "</button>\n";
      Wserver.printf "</form>\n";
      Hutil.trailer conf
  | _ -> Hutil.incorrect_request conf

(* ************************************************************************ *)
(*  [Fonc] print_clean_ok : config -> unit                                  *)
(** [Description] : Ré-écrit le fichier historique lié à une personne en
      ayant supprimé les entrées non désirées.
    [Args] :
      - conf : configuration de la base
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let print_clean_ok conf =
  let rec clean_history i history new_history =
    match history with
      [] -> new_history
    | gr :: l ->
        let lab = "i" ^ string_of_int i in
        if p_getenv conf.env lab = Some "on" then
          clean_history (i + 1) l new_history
        else clean_history (i + 1) l (gr :: new_history)
  in
  match p_getenv conf.env "f" with
    Some f when f <> "" ->
      let title _ =
        Wserver.printf "%s" (Utf8.capitalize (transl conf "history cleaned"))
      in
      Hutil.header conf title;
      Hutil.print_link_to_welcome conf true;
      let history = load_person_history conf f in
      let new_history = clean_history 0 history [] in
      let fname = history_path conf f in
      if new_history = [] then Mutil.rm fname
      else
        begin let ext_flags =
          [Open_wronly; Open_trunc; Open_creat; Open_binary; Open_nonblock]
        in
          match
            try Some (Secure.open_out_gen ext_flags 0o644 fname) with
              Sys_error _ -> None
          with
            Some oc ->
              List.iter (fun v -> output_value oc (v : gen_record))
                new_history;
              close_out oc
          | None -> ()
        end;
      Hutil.trailer conf
  | _ -> Hutil.incorrect_request conf

(**/**) (* Template *)

let person_of_gen_p_key base gen_p =
  match person_of_key base gen_p.first_name gen_p.surname gen_p.occ with
    Some ip -> poi base ip
  | None -> Gwdb.empty_person base Gwdb.dummy_iper

(* N'est pas forcément très précis. En effet, on enregistre que     *)
(* les ipers. Or lors d'un nettoyage de la base, il se peut que     *)
(* ces ipers changent. On peut donc pointer vers une autre persone. *)
let person_of_iper conf base ip =
  try
    let p = pget conf base ip in
    if authorized_age conf base p then Util.person_text conf base p else ""
  with _ -> ""

let person_of_iper_array conf base ipl =
  String.concat ", " @@
  Array.fold_right
    (fun ip accu ->
       match person_of_iper conf base ip with "" -> accu | p -> p :: accu)
    ipl []

let string_of_cdate conf cod =
  match Adef.od_of_cdate cod with
    Some d -> DateDisplay.string_slash_of_date conf d
  | None -> ""

let string_of_death conf death =
  match death with
    Death (_, cd) -> DateDisplay.string_slash_of_date conf (Adef.date_of_cdate cd)
  | _ -> ""

let string_of_burial conf burial =
  match burial with
    Buried cod | Cremated cod -> string_of_cdate conf cod
  | _ -> ""

let string_of_title conf titles =
  let string_of_t_name t =
    match t.t_name with
      Tname s -> s
    | _ -> ""
  in
  let one_title t =
    let name = t.t_ident ^ " " ^ t.t_place in
    let name = if name = " " then "" else name in
    let dates =
      string_of_cdate conf t.t_date_start ^ "-" ^
      string_of_cdate conf t.t_date_end
    in
    let dates = if dates = "-" then "" else "(" ^ dates ^ ")" in
    let nth = if t.t_nth = 0 then "" else string_of_int t.t_nth in
    let nth =
      if string_of_t_name t = "" then nth
      else string_of_t_name t ^ " " ^ string_of_int t.t_nth
    in
    let nth = if nth = "" || nth = " " then "" else "[" ^ nth ^ "]" in
    name ^ (if name = "" then "" else " ") ^ nth ^
    (if nth = "" then "" else " ") ^ dates
  in
  List.fold_left
    (fun accu t ->
       if accu = "" then one_title t else accu ^ ", " ^ one_title t)
    "" titles

let string_of_related conf base ip related =
  let related =
    List.fold_right
      (fun ic accu ->
         let p = person_of_iper conf base ip in
         if p = "" then accu
         else
           let c =
             try pget conf base ic with _ -> Gwdb.empty_person base ic
           in
           let rel =
             let rec loop rp =
               match rp with
                 r :: l ->
                   begin match r.r_fath with
                     Some ifath when ifath = ip ->
                       Util.rchild_type_text conf r.r_type 2
                   | _ -> loop l
                   end
               | [] -> ""
             in
             loop (get_rparents c)
           in
           (Utf8.capitalize rel ^ ": " ^ p) :: accu)
      related []
  in
  String.concat ", " related

let string_of_rparents conf base rparents =
  let rparents =
    List.fold_right
      (fun rp accu ->
         match rp.r_fath, rp.r_moth with
           Some ip1, Some ip2 ->
             let rel = Utf8.capitalize (Util.relation_type_text conf rp.r_type 2) in
             let fath = person_of_iper conf base ip1 in
             let moth = person_of_iper conf base ip2 in
             begin match fath, moth with
               "", "" -> accu
             | p, "" -> (rel ^ ": " ^ p) :: accu
             | "", p -> (rel ^ ": " ^ p) :: accu
             | p1, p2 -> (rel ^ ": " ^ p1 ^ ", " ^ p2) :: accu
             end
         | Some ip, None ->
             let p = person_of_iper conf base ip in
             if p = "" then accu
             else
               let rel =
                 Utf8.capitalize (Util.relation_type_text conf rp.r_type 2)
               in
               (rel ^ ": " ^ p) :: accu
         | None, Some ip ->
             let p = person_of_iper conf base ip in
             if p = "" then accu
             else
               let rel =
                 Utf8.capitalize (Util.relation_type_text conf rp.r_type 2)
               in
               (rel ^ ": " ^ p) :: accu
         | None, None -> accu)
      rparents []
  in
  String.concat ", " rparents

let string_of_marriage conf marriage =
  match marriage with
  | NotMarried | NoSexesCheckNotMarried -> transl conf "with"
  | Married | NoSexesCheckMarried -> transl conf "married"
  | Engaged -> transl conf "engaged"
  | NoMention
  | MarriageBann
  | MarriageContract
  | MarriageLicense
  | Pacs
  | Residence
    -> transl conf "with"

let string_of_divorce conf divorce =
  match divorce with
    NotDivorced -> ""
  | Divorced cod -> transl conf "divorced" ^ " " ^ string_of_cdate conf cod
  | Separated -> transl conf "separated"

let string_of_event_witness conf base witnesses =
  let witnesses =
    Array.fold_right
      (fun (ip, wk) accu ->
         let witn = person_of_iper conf base ip in
         let kind = Util.string_of_witness_kind conf (get_sex @@ poi base ip) wk in
         if witn = "" then (kind ^ ": " ^ witn) :: accu else accu)
      witnesses []
  in
  String.concat ", " witnesses

let string_of_epers_name conf epers_name =
  match epers_name with
    Epers_Birth -> Utf8.capitalize (transl conf "birth")
  | Epers_Baptism -> Utf8.capitalize (transl conf "baptism")
  | Epers_Death -> Utf8.capitalize (transl conf "death")
  | Epers_Burial -> Utf8.capitalize (transl conf "burial")
  | Epers_Cremation -> Utf8.capitalize (transl conf "cremation")
  | Epers_Accomplishment -> Utf8.capitalize (transl conf "accomplishment")
  | Epers_Acquisition -> Utf8.capitalize (transl conf "acquisition")
  | Epers_Adhesion -> Utf8.capitalize (transl conf "adhesion")
  | Epers_BaptismLDS -> Utf8.capitalize (transl conf "baptismLDS")
  | Epers_BarMitzvah -> Utf8.capitalize (transl conf "bar mitzvah")
  | Epers_BatMitzvah -> Utf8.capitalize (transl conf "bat mitzvah")
  | Epers_Benediction -> Utf8.capitalize (transl conf "benediction")
  | Epers_ChangeName -> Utf8.capitalize (transl conf "change name")
  | Epers_Circumcision -> Utf8.capitalize (transl conf "circumcision")
  | Epers_Confirmation -> Utf8.capitalize (transl conf "confirmation")
  | Epers_ConfirmationLDS -> Utf8.capitalize (transl conf "confirmation LDS")
  | Epers_Decoration -> Utf8.capitalize (transl conf "decoration")
  | Epers_DemobilisationMilitaire ->
      Utf8.capitalize (transl conf "demobilisationMilitaire")
  | Epers_Diploma -> Utf8.capitalize (transl conf "diploma")
  | Epers_Distinction -> Utf8.capitalize (transl conf "distinction")
  | Epers_Dotation -> Utf8.capitalize (transl conf "dotation")
  | Epers_DotationLDS -> Utf8.capitalize (transl conf "dotationLDS")
  | Epers_Education -> Utf8.capitalize (transl conf "education")
  | Epers_Election -> Utf8.capitalize (transl conf "election")
  | Epers_Emigration -> Utf8.capitalize (transl conf "emigration")
  | Epers_Excommunication -> Utf8.capitalize (transl conf "excommunication")
  | Epers_FamilyLinkLDS -> Utf8.capitalize (transl conf "familyLinkLDS")
  | Epers_FirstCommunion -> Utf8.capitalize (transl conf "firstCommunion")
  | Epers_Funeral -> Utf8.capitalize (transl conf "funeral")
  | Epers_Graduate -> Utf8.capitalize (transl conf "graduate")
  | Epers_Hospitalisation -> Utf8.capitalize (transl conf "hospitalisation")
  | Epers_Illness -> Utf8.capitalize (transl conf "illness")
  | Epers_Immigration -> Utf8.capitalize (transl conf "immigration")
  | Epers_ListePassenger -> Utf8.capitalize (transl conf "listePassenger")
  | Epers_MilitaryDistinction -> Utf8.capitalize (transl conf "militaryDistinction")
  | Epers_MilitaryPromotion -> Utf8.capitalize (transl conf "militaryPromotion")
  | Epers_MilitaryService -> Utf8.capitalize (transl conf "militaryService")
  | Epers_MobilisationMilitaire ->
      Utf8.capitalize (transl conf "mobilisationMilitaire")
  | Epers_Naturalisation -> Utf8.capitalize (transl conf "naturalisation")
  | Epers_Occupation -> Utf8.capitalize (transl_nth conf "occupation/occupations" 0)
  | Epers_Ordination -> Utf8.capitalize (transl conf "ordination")
  | Epers_Property -> Utf8.capitalize (transl conf "property")
  | Epers_Recensement -> Utf8.capitalize (transl conf "recensement")
  | Epers_Residence -> Utf8.capitalize (transl conf "residence")
  | Epers_Retired -> Utf8.capitalize (transl conf "retired")
  | Epers_ScellentChildLDS -> Utf8.capitalize (transl conf "scellentChildLDS")
  | Epers_ScellentParentLDS -> Utf8.capitalize (transl conf "scellentParentLDS")
  | Epers_ScellentSpouseLDS -> Utf8.capitalize (transl conf "scellentSpouseLDS")
  | Epers_VenteBien -> Utf8.capitalize (transl conf "venteBien")
  | Epers_Will -> Utf8.capitalize (transl conf "will")
  | Epers_Name n -> Utf8.capitalize n

let string_of_efam_name conf efam_name =
  match efam_name with
    Efam_Marriage -> Utf8.capitalize (transl conf "marriage event")
  | Efam_NoMarriage -> Utf8.capitalize (transl conf "no marriage event")
  | Efam_NoMention -> Utf8.capitalize (transl conf "no mention")
  | Efam_Engage -> Utf8.capitalize (transl conf "engage event")
  | Efam_Divorce -> Utf8.capitalize (transl conf "divorce event")
  | Efam_Separated -> Utf8.capitalize (transl conf "separate event")
  | Efam_Annulation -> Utf8.capitalize (transl conf "annulation")
  | Efam_MarriageBann -> Utf8.capitalize (transl conf "marriage bann")
  | Efam_MarriageContract -> Utf8.capitalize (transl conf "marriage contract")
  | Efam_MarriageLicense -> Utf8.capitalize (transl conf "marriage licence")
  | Efam_PACS -> Utf8.capitalize (transl conf "PACS")
  | Efam_Residence -> Utf8.capitalize (transl conf "residence")
  | Efam_Name n -> Utf8.capitalize n

(* ************************************************************************ *)
(*  [Fonc] highlight_diff : char array -> bool array -> string              *)
(** [Description] : Converti un tableau de char en string, avec les parties
      modifiées encadrées par des balises <span>.
    [Args] :
      - arr : tableau à convertir
      - diff_arr : tableau des différences
    [Retour] :
      - string
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let highlight_diff arr diff_arr =
  let rec loop i s =
    if i >= Array.length arr then s
    else if diff_arr.(i) then
      let j = ref i in
      let accu = ref s in
      accu := !accu ^ "<span class=\"mark\">";
      while !j < Array.length diff_arr && diff_arr.(!j) do
        accu := !accu ^ Printf.sprintf "%c" arr.(!j);
        incr j
      done;
      accu := !accu ^ "</span>";
      loop !j !accu
    else loop (i + 1) (s ^ Printf.sprintf "%c" arr.(i))
  in
  loop 0 ""

(* ************************************************************************ *)
(*  [Fonc] array_of_string : string -> char array                           *)
(** [Description] : Converti une string en tableau de char afin de pouvoir
      faire un diff.
    [Args] :
      - s : string à convertir
    [Retour] :
      - char array
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let array_of_string s =
  let len = String.length s in
  let a = Array.make len ' ' in
  let rec loop i =
    if i = len then a else begin a.(i) <- s.[i]; loop (i + 1) end
  in
  loop 0

(* ************************************************************************ *)
(*  [Fonc] diff_string : string -> string -> (string * string)              *)
(** [Description] : Renvoie les deux string avec mise en évidence des
      différences entre les deux.
    [Args] :
      - before : string avant modification
      - after  : string après modification
    [Retour] :
      - string * string
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let diff_string before after =
  if before = after then before, after
  else if before = "" then before, "<span class=\"mark\">" ^ after ^ "</span>"
  else if after = "" then "<span class=\"mark\">" ^ before ^ "</span>", after
  else
    let aa = array_of_string after in
    let bb = array_of_string before in
    let (bef_d, aft_d) = Difference.f bb aa in
    let bef_s = highlight_diff bb bef_d in
    let aft_s = highlight_diff aa aft_d in bef_s, aft_s

type 'a env =
    Vgen_record of gen_record
  | Vfam of
      (iper, ifam, string) gen_family option * (iper, ifam, string) gen_family option *
        bool
  | Vchild of iper array option * iper array option
  | Vfevent of
      (iper, string) gen_fam_event option *
        (iper, string) gen_fam_event option * bool
  | Vpevent of
      (iper, string) gen_pers_event option *
        (iper, string) gen_pers_event option
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vother of 'a
  | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x
let str_val x = VVstring x
let bool_val x = VVbool x

let rec eval_var conf base env (bef, aft, p_auth) _loc sl =
  try eval_simple_var conf base env (bef, aft, p_auth) sl with
    Not_found -> eval_compound_var conf base env (bef, aft, p_auth) sl
and eval_simple_var conf base env (bef, aft, p_auth) =
  function
    [s] -> str_val (eval_simple_str_var conf base env (bef, aft, p_auth) s)
  | _ -> raise Not_found
and eval_compound_var conf base env (bef, aft, p_auth) sl =
  let loop =
    function
      [s] -> eval_simple_str_var conf base env (bef, aft, p_auth) s
    | ["evar"; s] ->
        begin match p_getenv conf.env s with
          Some s -> s
        | None -> ""
        end
    | "before" :: sl ->
        fst (eval_gen_record conf base env (bef, aft, p_auth) sl)
    | "after" :: sl ->
        snd (eval_gen_record conf base env (bef, aft, p_auth) sl)
    | _ -> raise Not_found
  in
  str_val (loop sl)
and eval_gen_record conf base env (bef, aft, p_auth) =
  function
    ["date"] -> bef.date, aft.date
  | ["wizard"] -> bef.HistoryDiff.wizard, aft.HistoryDiff.wizard
  | [s] -> eval_str_gen_record conf base env (bef, aft, p_auth) s
  | _ -> raise Not_found
and eval_str_gen_record conf base env (bef, aft, p_auth) =
  function
    "first_name" ->
      if p_auth then
        let b = bef.gen_p.first_name in
        let a = aft.gen_p.first_name in diff_string b a
      else "", ""
  | "surname" ->
      if p_auth then
        let b = bef.gen_p.surname in
        let a = aft.gen_p.surname in diff_string b a
      else "", ""
  | "occ" ->
      if p_auth then
        let b = string_of_int bef.gen_p.occ in
        let a = string_of_int aft.gen_p.occ in diff_string b a
      else "", ""
  | "image" ->
      if p_auth && not conf.no_image then
        let b = bef.gen_p.image in let a = aft.gen_p.image in diff_string b a
      else "", ""
  | "public_name" ->
      if p_auth then
        let b = bef.gen_p.public_name in
        let a = aft.gen_p.public_name in diff_string b a
      else "", ""
  | "qualifiers" ->
      if p_auth then
        let b = String.concat ", " bef.gen_p.qualifiers in
        let a = String.concat ", " aft.gen_p.qualifiers in diff_string b a
      else "", ""
  | "aliases" ->
      if p_auth then
        let b = String.concat ", " bef.gen_p.aliases in
        let a = String.concat ", " aft.gen_p.aliases in diff_string b a
      else "", ""
  | "first_names_aliases" ->
      if p_auth then
        let b = String.concat ", " bef.gen_p.first_names_aliases in
        let a = String.concat ", " aft.gen_p.first_names_aliases in
        diff_string b a
      else "", ""
  | "surnames_aliases" ->
      if p_auth then
        let b = String.concat ", " bef.gen_p.surnames_aliases in
        let a = String.concat ", " aft.gen_p.surnames_aliases in
        diff_string b a
      else "", ""
  | "titles" ->
      if p_auth then
        let b = string_of_title conf bef.gen_p.titles in
        let a = string_of_title conf aft.gen_p.titles in diff_string b a
      else "", ""
  | "relations" ->
      if p_auth then
        let br =
          string_of_related conf base bef.gen_p.key_index bef.gen_p.related
        in
        let ar =
          string_of_related conf base aft.gen_p.key_index aft.gen_p.related
        in
        let brp = string_of_rparents conf base bef.gen_p.rparents in
        let arp = string_of_rparents conf base aft.gen_p.rparents in
        let b = if br = "" then brp else br ^ ". " ^ brp in
        let a = if ar = "" then arp else ar ^ ". " ^ brp in diff_string b a
      else "", ""
  | "occupation" ->
      if p_auth then
        let b = bef.gen_p.occupation in
        let a = aft.gen_p.occupation in diff_string b a
      else "", ""
  | "sex" ->
      if p_auth then
        let b =
          transl_nth conf "male/female/neuter"
            (Util.index_of_sex bef.gen_p.sex)
        in
        let a =
          transl_nth conf "male/female/neuter"
            (Util.index_of_sex aft.gen_p.sex)
        in
        diff_string b a
      else "", ""
  | "access" ->
      if p_auth then
        let b =
          match bef.gen_p.access with
            IfTitles -> transl_nth conf "iftitles/public/private" 0
          | Public -> transl_nth conf "iftitles/public/private" 1
          | Private -> transl_nth conf "iftitles/public/private" 2
        in
        let a =
          match aft.gen_p.access with
            IfTitles -> transl_nth conf "iftitles/public/private" 0
          | Public -> transl_nth conf "iftitles/public/private" 1
          | Private -> transl_nth conf "iftitles/public/private" 2
        in
        diff_string b a
      else "", ""
  | "birth" ->
      if p_auth then
        let b = string_of_cdate conf bef.gen_p.birth in
        let a = string_of_cdate conf aft.gen_p.birth in diff_string b a
      else "", ""
  | "birth_place" ->
      if p_auth then
        let b = bef.gen_p.birth_place in
        let a = aft.gen_p.birth_place in diff_string b a
      else "", ""
  | "birth_note" ->
      if p_auth then
        let b = Util.escape_html bef.gen_p.birth_note in
        let a = Util.escape_html aft.gen_p.birth_note in diff_string b a
      else "", ""
  | "birth_src" ->
      if p_auth then
        let b = Util.escape_html bef.gen_p.birth_src in
        let a = Util.escape_html aft.gen_p.birth_src in diff_string b a
      else "", ""
  | "baptism" ->
      if p_auth then
        let b = string_of_cdate conf bef.gen_p.baptism in
        let a = string_of_cdate conf aft.gen_p.baptism in diff_string b a
      else "", ""
  | "baptism_place" ->
      if p_auth then
        let b = bef.gen_p.baptism_place in
        let a = aft.gen_p.baptism_place in diff_string b a
      else "", ""
  | "baptism_note" ->
      if p_auth then
        let b = Util.escape_html bef.gen_p.baptism_note in
        let a = Util.escape_html aft.gen_p.baptism_note in diff_string b a
      else "", ""
  | "baptism_src" ->
      if p_auth then
        let b = Util.escape_html bef.gen_p.baptism_src in
        let a = Util.escape_html aft.gen_p.baptism_src in diff_string b a
      else "", ""
  | "death" ->
      if p_auth then
        let b = string_of_death conf bef.gen_p.death in
        let a = string_of_death conf aft.gen_p.death in diff_string b a
      else "", ""
  | "death_place" ->
      if p_auth then
        let b = bef.gen_p.death_place in
        let a = aft.gen_p.death_place in diff_string b a
      else "", ""
  | "death_note" ->
      if p_auth then
        let b = Util.escape_html bef.gen_p.death_note in
        let a = Util.escape_html aft.gen_p.death_note in diff_string b a
      else "", ""
  | "death_src" ->
      if p_auth then
        let b = Util.escape_html bef.gen_p.death_src in
        let a = Util.escape_html aft.gen_p.death_src in diff_string b a
      else "", ""
  | "burial" ->
      if p_auth then
        let b = string_of_burial conf bef.gen_p.burial in
        let a = string_of_burial conf aft.gen_p.burial in diff_string b a
      else "", ""
  | "burial_place" ->
      if p_auth then
        let b = bef.gen_p.burial_place in
        let a = aft.gen_p.burial_place in diff_string b a
      else "", ""
  | "burial_note" ->
      if p_auth then
        let b = Util.escape_html bef.gen_p.burial_note in
        let a = Util.escape_html aft.gen_p.burial_note in diff_string b a
      else "", ""
  | "burial_src" ->
      if p_auth then
        let b = Util.escape_html bef.gen_p.burial_src in
        let a = Util.escape_html aft.gen_p.burial_src in diff_string b a
      else "", ""
  | "pevent_name" ->
      begin match get_env "pevent" env with
        Vpevent (bef, aft) ->
          if p_auth then
            match bef, aft with
              Some b, Some a ->
                let b = string_of_epers_name conf b.epers_name in
                let a = string_of_epers_name conf a.epers_name in
                diff_string b a
            | None, Some a -> "", string_of_epers_name conf a.epers_name
            | Some b, None -> string_of_epers_name conf b.epers_name, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "pevent_date" ->
      begin match get_env "pevent" env with
        Vpevent (bef, aft) ->
          if p_auth then
            match bef, aft with
              Some b, Some a ->
                let b = string_of_cdate conf b.epers_date in
                let a = string_of_cdate conf a.epers_date in diff_string b a
            | None, Some a -> "", string_of_cdate conf a.epers_date
            | Some b, None -> string_of_cdate conf b.epers_date, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "pevent_place" ->
      begin match get_env "pevent" env with
        Vpevent (bef, aft) ->
          if p_auth then
            match bef, aft with
              Some b, Some a ->
                let b = b.epers_place in
                let a = a.epers_place in diff_string b a
            | None, Some a -> "", a.epers_place
            | Some b, None -> b.epers_place, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "pevent_note" ->
      begin match get_env "pevent" env with
        Vpevent (bef, aft) ->
          if p_auth && not conf.no_note then
            match bef, aft with
              Some b, Some a ->
                let b = Util.escape_html b.epers_note in
                let a = Util.escape_html a.epers_note in diff_string b a
            | None, Some a -> "", Util.escape_html a.epers_note
            | Some b, None -> Util.escape_html b.epers_note, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "pevent_src" ->
      begin match get_env "pevent" env with
        Vpevent (bef, aft) ->
          if p_auth then
            match bef, aft with
              Some b, Some a ->
                let b = Util.escape_html b.epers_src in
                let a = Util.escape_html a.epers_src in diff_string b a
            | None, Some a -> "", Util.escape_html a.epers_src
            | Some b, None -> Util.escape_html b.epers_src, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "pevent_witness" ->
      begin match get_env "pevent" env with
        Vpevent (bef, aft) ->
          if p_auth then
            match bef, aft with
              Some b, Some a ->
                let b = string_of_event_witness conf base b.epers_witnesses in
                let a = string_of_event_witness conf base a.epers_witnesses in
                diff_string b a
            | None, Some a ->
                "", string_of_event_witness conf base a.epers_witnesses
            | Some b, None ->
                string_of_event_witness conf base b.epers_witnesses, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "notes" ->
      if p_auth && not conf.no_note then
        let b = Util.escape_html bef.gen_p.notes in
        let a = Util.escape_html aft.gen_p.notes in diff_string b a
      else "", ""
  | "psources" ->
      if p_auth then
        let b = bef.gen_p.psources in
        let a = aft.gen_p.psources in diff_string b a
      else "", ""
  | "spouse" ->
      begin match get_env "fam" env with
        Vfam (_f_bef, _f_aft, m_auth) ->
          if m_auth then
            eval_string_env "spouse_bef" env, eval_string_env "spouse_aft" env
          else "", ""
      | _ -> raise Not_found
      end
  | "marriage" ->
      begin match get_env "fam" env with
        Vfam (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = string_of_cdate conf b.marriage in
                let a = string_of_cdate conf a.marriage in diff_string b a
            | None, Some a -> "", string_of_cdate conf a.marriage
            | Some b, None -> string_of_cdate conf b.marriage, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "marriage_place" ->
      begin match get_env "fam" env with
        Vfam (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = b.marriage_place in
                let a = a.marriage_place in diff_string b a
            | None, Some a -> "", a.marriage_place
            | Some b, None -> b.marriage_place, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "marriage_src" ->
      begin match get_env "fam" env with
        Vfam (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = Util.escape_html b.marriage_src in
                let a = Util.escape_html a.marriage_src in diff_string b a
            | None, Some a -> "", Util.escape_html a.marriage_src
            | Some b, None -> Util.escape_html b.marriage_src, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "witnesses" ->
      begin match get_env "fam" env with
        Vfam (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = person_of_iper_array conf base b.witnesses in
                let a = person_of_iper_array conf base a.witnesses in
                diff_string b a
            | None, Some a -> "", person_of_iper_array conf base a.witnesses
            | Some b, None -> person_of_iper_array conf base b.witnesses, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "marriage_type" ->
      begin match get_env "fam" env with
        Vfam (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = string_of_marriage conf b.relation in
                let a = string_of_marriage conf a.relation in diff_string b a
            | None, Some a -> "", string_of_marriage conf a.relation
            | Some b, None -> string_of_marriage conf b.relation, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "divorce" ->
      begin match get_env "fam" env with
        Vfam (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = string_of_divorce conf b.divorce in
                let a = string_of_divorce conf a.divorce in diff_string b a
            | None, Some a -> "", string_of_divorce conf a.divorce
            | Some b, None -> string_of_divorce conf b.divorce, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "fevent_name" ->
      begin match get_env "fevent" env with
        Vfevent (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = string_of_efam_name conf b.efam_name in
                let a = string_of_efam_name conf a.efam_name in
                diff_string b a
            | None, Some a -> "", string_of_efam_name conf a.efam_name
            | Some b, None -> string_of_efam_name conf b.efam_name, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "fevent_date" ->
      begin match get_env "fevent" env with
        Vfevent (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = string_of_cdate conf b.efam_date in
                let a = string_of_cdate conf a.efam_date in diff_string b a
            | None, Some a -> "", string_of_cdate conf a.efam_date
            | Some b, None -> string_of_cdate conf b.efam_date, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "fevent_place" ->
      begin match get_env "fevent" env with
        Vfevent (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = b.efam_place in
                let a = a.efam_place in diff_string b a
            | None, Some a -> "", a.efam_place
            | Some b, None -> b.efam_place, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "fevent_note" ->
      begin match get_env "fevent" env with
        Vfevent (bef, aft, m_auth) ->
          if m_auth && not conf.no_note then
            match bef, aft with
              Some b, Some a ->
                let b = Util.escape_html b.efam_note in
                let a = Util.escape_html a.efam_note in diff_string b a
            | None, Some a -> "", Util.escape_html a.efam_note
            | Some b, None -> Util.escape_html b.efam_note, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "fevent_src" ->
      begin match get_env "fevent" env with
        Vfevent (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = Util.escape_html b.efam_src in
                let a = Util.escape_html a.efam_src in diff_string b a
            | None, Some a -> "", Util.escape_html a.efam_src
            | Some b, None -> Util.escape_html b.efam_src, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "fevent_witness" ->
      begin match get_env "fevent" env with
        Vfevent (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = string_of_event_witness conf base b.efam_witnesses in
                let a = string_of_event_witness conf base a.efam_witnesses in
                diff_string b a
            | None, Some a ->
                "", string_of_event_witness conf base a.efam_witnesses
            | Some b, None ->
                string_of_event_witness conf base b.efam_witnesses, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "comment" ->
      begin match get_env "fam" env with
        Vfam (bef, aft, m_auth) ->
          if m_auth && not conf.no_note then
            match bef, aft with
              Some b, Some a ->
                let b = Util.escape_html b.comment in
                let a = Util.escape_html a.comment in diff_string b a
            | None, Some a -> "", Util.escape_html a.comment
            | Some b, None -> Util.escape_html b.comment, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "origin_file" ->
      begin match get_env "fam" env with
        Vfam (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = b.origin_file in
                let a = a.origin_file in diff_string b a
            | None, Some a -> "", a.origin_file
            | Some b, None -> b.origin_file, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "fsources" ->
      begin match get_env "fam" env with
        Vfam (bef, aft, m_auth) ->
          if m_auth then
            match bef, aft with
              Some b, Some a ->
                let b = b.fsources in let a = a.fsources in diff_string b a
            | None, Some a -> "", a.fsources
            | Some b, None -> b.fsources, ""
            | None, None -> "", ""
          else "", ""
      | _ -> raise Not_found
      end
  | "children" ->
      begin match get_env "fam" env with
        Vfam (_, _, m_auth) ->
          if m_auth then
            match get_env "child" env with
              Vchild (bef, aft) ->
                begin match bef, aft with
                  Some b, Some a ->
                    let b = person_of_iper_array conf base b in
                    let a = person_of_iper_array conf base a in
                    diff_string b a
                | None, Some a -> "", person_of_iper_array conf base a
                | Some b, None -> person_of_iper_array conf base b, ""
                | None, None -> "", ""
                end
            | _ -> raise Not_found
          else "", ""
      | _ -> raise Not_found
      end
  | _ -> raise Not_found
and eval_simple_str_var conf base env (bef, aft, p_auth) =
  function
    "acces" -> let p = person_of_gen_p_key base aft.gen_p in acces conf base p
  | "date" -> eval_string_env "date" env
  | "history_len" -> eval_int_env "history_len" env
  | "line" -> eval_int_env "line" env
  | "nb_families" ->
      let nb_fam = max (List.length bef.gen_f) (List.length aft.gen_f) in
      string_of_int nb_fam
  | "person" ->
      if p_auth then
        let p = person_of_gen_p_key base aft.gen_p in
        Util.person_text conf base p
      else eval_string_env "history_file" env
  | "wizard" -> eval_string_env "wizard" env
  | _ -> raise Not_found
and eval_string_env s env =
  match get_env s env with
    Vstring s -> s
  | _ -> raise Not_found
and eval_int_env s env =
  match get_env s env with
    Vint i -> string_of_int i
  | _ -> raise Not_found

let print_foreach conf base print_ast _eval_expr =
  let rec print_foreach env xx _loc s sl _el al =
    match s :: sl with
      ["family"] -> print_foreach_family env xx al
    | ["fevent"] -> print_foreach_fevent env xx al
    | ["pevent"] -> print_foreach_pevent env xx al
    | ["history_line"] -> print_foreach_history_line env xx al
    | _ -> raise Not_found
  and print_foreach_family env xx al =
    let (bef, aft, p_auth) = xx in
    let rec loop bef_f bef_c aft_f aft_c =
      match bef_f, aft_f with
        [], [] -> ()
      | [], gen_f :: l ->
          let fam = foi base gen_f.fam_index in
          let isp = Gutil.spouse aft.gen_p.key_index fam in
          let sp = person_of_iper conf base isp in
          let m_auth = authorized_age conf base (poi base isp) && p_auth in
          let vfam = Vfam (None, Some gen_f, m_auth) in
          let (vchild, c) =
            match bef_c, aft_c with
              [], gen_c :: l -> Vchild (None, Some gen_c), l
            | _ ->               (* pas normal*)Vchild (None, None), []
          in
          let env =
            ("fam", vfam) :: ("spouse_bef", Vstring "") ::
            ("spouse_aft", Vstring sp) :: ("child", vchild) :: env
          in
          List.iter (print_ast env xx) al; loop [] bef_c l c
      | gen_f :: l, [] ->
          let fam = foi base gen_f.fam_index in
          let isp = Gutil.spouse aft.gen_p.key_index fam in
          let sp = person_of_iper conf base isp in
          let m_auth = authorized_age conf base (poi base isp) && p_auth in
          let vfam = Vfam (Some gen_f, None, m_auth) in
          let (vchild, c) =
            match bef_c, aft_c with
              gen_c :: l, [] -> Vchild (Some gen_c, None), l
            | _ ->               (* pas normal*)Vchild (None, None), []
          in
          let env =
            ("fam", vfam) :: ("spouse_bef", Vstring sp) ::
            ("spouse_aft", Vstring "") :: ("child", vchild) :: env
          in
          List.iter (print_ast env xx) al; loop l c [] aft_c
      | gen_f1 :: l1, gen_f2 :: l2 ->
          let fam = foi base gen_f2.fam_index in
          let isp1 = Gutil.spouse bef.gen_p.key_index fam in
          let isp2 = Gutil.spouse aft.gen_p.key_index fam in
          let sp1 = person_of_iper conf base isp1 in
          let sp2 = person_of_iper conf base isp2 in
          let m_auth = authorized_age conf base (poi base isp2) && p_auth in
          let vfam = Vfam (Some gen_f1, Some gen_f2, m_auth) in
          let (vchild, c1, c2) =
            match bef_c, aft_c with
              gen_c1 :: l1, gen_c2 :: l2 ->
                Vchild (Some gen_c1, Some gen_c2), l1, l2
            | _ ->               (* pas normal*)Vchild (None, None), [], []
          in
          let env =
            ("fam", vfam) :: ("spouse_bef", Vstring sp1) ::
            ("spouse_aft", Vstring sp2) :: ("child", vchild) :: env
          in
          List.iter (print_ast env xx) al; loop l1 c1 l2 c2
    in
    loop bef.gen_f bef.gen_c aft.gen_f aft.gen_c
  and print_foreach_fevent env xx al =
    let rec loop m_auth bef_fevents aft_fevents =
      match bef_fevents, aft_fevents with
        [], [] -> ()
      | [], aft_evt :: l ->
          let env = ("fevent", Vfevent (None, Some aft_evt, m_auth)) :: env in
          List.iter (print_ast env xx) al; loop m_auth [] l
      | bef_evt :: l, [] ->
          let env = ("fevent", Vfevent (Some bef_evt, None, m_auth)) :: env in
          List.iter (print_ast env xx) al; loop m_auth l []
      | bef_evt :: l1, aft_evt :: l2 ->
          let env =
            ("fevent", Vfevent (Some bef_evt, Some aft_evt, m_auth)) :: env
          in
          List.iter (print_ast env xx) al; loop m_auth l1 l2
    in
    match get_env "fam" env with
      Vfam (bef, aft, m_auth) ->
        begin match bef, aft with
          Some b, Some a -> loop m_auth b.fevents a.fevents
        | None, Some a -> loop m_auth [] a.fevents
        | Some b, None -> loop m_auth b.fevents []
        | None, None -> ()
        end
    | _ -> ()
  and print_foreach_pevent env xx al =
    let (bef, aft, _p_auth) = xx in
    let rec loop bef_pevents aft_pevents =
      match bef_pevents, aft_pevents with
        [], [] -> ()
      | [], aft_evt :: l ->
          let env = ("pevent", Vpevent (None, Some aft_evt)) :: env in
          List.iter (print_ast env xx) al; loop [] l
      | bef_evt :: l, [] ->
          let env = ("pevent", Vpevent (Some bef_evt, None)) :: env in
          List.iter (print_ast env xx) al; loop l []
      | bef_evt :: l1, aft_evt :: l2 ->
          let env = ("pevent", Vpevent (Some bef_evt, Some aft_evt)) :: env in
          List.iter (print_ast env xx) al; loop l1 l2
    in
    loop bef.gen_p.pevents aft.gen_p.pevents
  and print_foreach_history_line env xx al =
    match get_env "history_file" env with
      Vstring fname ->
        let history = load_person_history conf fname in
        let rec loop i list =
          match list with
            [] -> ()
          | gr :: l ->
              let env =
                ("line", Vint i) :: ("date", Vstring gr.date) ::
                ("wizard", Vstring gr.HistoryDiff.wizard) :: env
              in
              List.iter (print_ast env xx) al; loop (i + 1) l
        in
        loop 0 history
    | _ -> ()
  in
  print_foreach

let eval_predefined_apply conf _env f vl =
  let vl =
    List.map
      (function
         VVstring s -> s
       | _ -> raise Not_found)
      vl
  in
  match f, vl with
    "transl_date", [date_txt] ->
      (* date_tpl = "0000-00-00 00:00:00" *)
      begin try
        let year = int_of_string (String.sub date_txt 0 4) in
        let month = int_of_string (String.sub date_txt 5 2) in
        let day = int_of_string (String.sub date_txt 8 2) in
        let date =
          Dgreg
            ({day = day; month = month; year = year; prec = Sure; delta = 0},
             Dgregorian)
        in
        let time = String.sub date_txt 11 8 in
        DateDisplay.string_of_date conf date ^ ", " ^ time
      with Failure _ -> date_txt
      end
  | _ -> raise Not_found

let print conf base =
  match p_getenv conf.env "t" with
    Some ("SUM" | "DIFF") ->
      begin match p_getenv conf.env "f" with
        Some file when file <> "" ->
          let history = load_person_history conf file in
          let len = List.length history in
          let (before, after) =
            match p_getint conf.env "old", p_getint conf.env "new" with
              Some o, Some n ->
                let o =
                  if o < 0 then 0 else if o > len - 1 then len - 1 else o
                in
                let n =
                  if n < 0 then 0 else if n > len - 1 then len - 1 else n
                in
                o, n
            | _ -> 0, 0
          in
          let before = List.nth history before in
          let after = List.nth history after in
          let p = person_of_gen_p_key base after.gen_p in
          let p_auth = authorized_age conf base p in
          let env = ["history_file", Vstring file; "history_len", Vint len] in
          Hutil.interp conf "updhist_diff"
            {Templ.eval_var = eval_var conf base;
             Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
             Templ.eval_predefined_apply = eval_predefined_apply conf;
             Templ.get_vother = get_vother; Templ.set_vother = set_vother;
             Templ.print_foreach = print_foreach conf base}
            env (before, after, p_auth)
      | _ -> Hutil.incorrect_request conf
      end
  | _ -> Hutil.incorrect_request conf
