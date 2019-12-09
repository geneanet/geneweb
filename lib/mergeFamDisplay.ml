(* $Id: mergeFam.ml,v 5.13 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

let compatible_fevents fevt1 fevt2 = fevt1 = [] && fevt2 = []

let need_differences_selection conf base fam1 fam2 =
  let need_selection proj =
    let x1 = proj fam1 in
    let x2 = proj fam2 in x1 <> "" && x2 <> "" && x1 <> x2
  in
  need_selection
    (fun fam ->
       match get_relation fam with
       | Married -> "married"
       | NotMarried -> "not married"
       | Engaged -> "engaged"
       | NoSexesCheckNotMarried -> "no sexes check"
       | NoSexesCheckMarried -> "no sexes check married"
       | NoMention -> "no mention"
       | MarriageBann -> "marriage banns"
       | MarriageContract -> "marriage contract"
       | MarriageLicense -> "marriage license"
       | Pacs -> "pacs"
       | Residence -> "residence") ||
  need_selection
    (fun fam ->
       match Adef.od_of_cdate (get_marriage fam) with
         None -> ""
       | Some d -> DateDisplay.string_of_ondate conf d) ||
  need_selection (fun fam -> sou base (get_marriage_place fam)) ||
  need_selection
    (fun fam ->
       match get_divorce fam with
         NotDivorced -> "not divorced"
       | Separated -> "separated"
       | Divorced cod ->
           match Adef.od_of_cdate cod with
             Some d -> DateDisplay.string_of_ondate conf d
           | None -> "divorced")

let print_differences conf base branches (ifam1, fam1) (ifam2, fam2) =
  let string_field title name proj =
    let x1 = proj fam1 in
    let x2 = proj fam2 in
    if x1 <> "" && x2 <> "" && x1 <> x2 then
      Wserver.printf
        "<h4>%s</h4>\
         <ul>\
         <li><input type=\"radio\" class=\"form-control\" name=\"%s\" value=\"1\" checked>%s</li>\
         <li><input type=\"radio\" class=\"form-control\" name=\"%s\" value=\"2\">%s</li>\
         </ul>"
        (Utf8.capitalize title) name x1 name x2;
  in
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Util.hidden_env conf;
  Wserver.printf "<input type=\"hidden\" name=\"m\" value=\"MRG_FAM_OK\">\n";
  Wserver.printf "<input type=\"hidden\" name=\"i\" value=\"%s\">\n"
    (string_of_ifam ifam1);
  Wserver.printf "<input type=\"hidden\" name=\"i2\" value=\"%s\">\n"
    (string_of_ifam ifam2);
  begin match p_getenv conf.env "ip" with
    Some ip ->
      Wserver.printf "<input type=\"hidden\" name=\"ip\" value=\"%s\">\n" ip
  | None -> ()
  end;
  begin let rec loop =
    function
      [ip1, ip2] ->
        Wserver.printf "<input type=\"hidden\" name=\"ini1\" value=\"%s\">\n"
          (string_of_iper ip1);
        Wserver.printf "<input type=\"hidden\" name=\"ini2\" value=\"%s\">\n"
          (string_of_iper ip2)
    | _ :: branches -> loop branches
    | _ -> ()
  in
    loop branches
  end;
  Wserver.printf "<p>" ;
  string_field (transl_nth conf "relation/relations" 0) "relation"
    (fun fam ->
       match get_relation fam with
         Married -> transl conf "married"
       | NotMarried -> transl conf "not married"
       | Engaged -> transl conf "engaged"
       | NoSexesCheckNotMarried -> transl conf "no sexes check"
       | NoSexesCheckMarried -> transl conf "married"
       | NoMention -> transl conf "no mention"
       | MarriageBann -> "marriage banns"
       | MarriageContract -> "marriage contract"
       | MarriageLicense -> "marriage license"
       | Pacs -> "pacs"
       | Residence -> "residence");
  string_field (Util.translate_eval (transl_nth conf "marriage/marriages" 0))
    "marriage"
    (fun fam ->
       match Adef.od_of_cdate (get_marriage fam) with
         None -> ""
       | Some d -> DateDisplay.string_of_ondate conf d);
  string_field
    (Util.translate_eval (transl_nth conf "marriage/marriages" 0) ^ " / " ^
     transl conf "place")
    "marriage_place" (fun fam -> sou base (get_marriage_place fam));
  string_field (transl conf "divorce") "divorce"
    (fun fam ->
       match get_divorce fam with
         NotDivorced -> transl conf "not divorced"
       | Separated -> transl conf "separated"
       | Divorced cod ->
           let ds =
             match Adef.od_of_cdate cod with
               Some d -> " " ^ DateDisplay.string_of_ondate conf d
             | None -> ""
           in
           transl conf "divorced" ^ ds);
  Wserver.printf
    "</p><p><button type=\"submit\" class=\"btn btn-secondary btn-lg\">%s</button></form>"
    (Utf8.capitalize (transl_nth conf "validate/delete" 0))

let merge_fam1 conf base fam1 fam2 =
  let title _ =
    let s = transl_nth conf "family/families" 1 in
    Wserver.printf "%s" (Utf8.capitalize (transl_decline conf "merge" s))
  in
  Hutil.header conf title; print_differences conf base [] fam1 fam2; Hutil.trailer conf

let merge_fam conf base (ifam1, fam1) (ifam2, fam2) =
  let cpl1 = foi base ifam1 in
  let cpl2 = foi base ifam2 in
  (* Vérifie que les deux couples sont identiques. Il est possible dans certains cas (couple de même sexe) que les personnes soient inversées dans l'union. *)
  if get_father cpl1 = get_father cpl2 && get_mother cpl1 = get_mother cpl2 ||
     get_father cpl1 = get_mother cpl2 && get_mother cpl1 = get_father cpl2
  then
    if need_differences_selection conf base fam1 fam2 &&
       compatible_fevents (get_fevents fam1) (get_fevents fam2)
    then
      merge_fam1 conf base (ifam1, fam1) (ifam2, fam2)
    else MergeFamOk.print_merge conf base
  else Hutil.incorrect_request conf

let print conf base =
  match p_getenv conf.env "i", p_getenv conf.env "i2" with
    Some f1, Some f2 ->
      let ifam1 = ifam_of_string f1 in
      let ifam2 = ifam_of_string f2 in
      let fam1 = foi base ifam1 in
      let fam2 = foi base ifam2 in
      merge_fam conf base (ifam1, fam1) (ifam2, fam2)
  | _ -> Hutil.incorrect_request conf
