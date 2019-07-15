(* $Id: place.ml,v 5.21 2007-09-18 19:12:08 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let normalize =
  (* petit hack en attendant une vraie gestion des lieux transforme
     "[foo-bar] - boobar (baz)" en "foo-bar, boobar (baz)" *)
  let r = Str.regexp "^\\[\\([^]]+\\)\\] *- *\\(.*\\)" in
  fun s -> Str.global_replace r "\\1, \\2" s

(* [String.length s > 0] is always true because we already tested [is_empty_string].
   If it is not true, then the base should be cleaned. *)
let fold_place_long inverted s =
  let len = String.length s in
  (* Trimm spaces after ',' and build reverse String.split_on_char ',' *)
  let rec loop iend list i ibeg =
    if i = iend
    then if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
    else
      let (list, ibeg) =
        match String.unsafe_get s i with
        | ',' ->
          let list =
            if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
          in
          list, i + 1
        | ' ' when i = ibeg -> (list, i + 1)
        | _ -> list, ibeg
      in
      loop iend list (i + 1) ibeg
  in
  let list =
    if String.unsafe_get s (len - 1) = ')'
    then match String.rindex_opt s '(' with
      | Some i when i < len - 2 ->
        let j =
          let rec loop i =
            if i >= 0 && String.unsafe_get s i = ' ' then loop (i - 1) else i + 1
          in
          loop (i - 1)
        in
        String.sub s (i + 1) (len - i - 2) :: loop j [] 0 0
      | _ -> loop len [] 0 0
    else loop len [] 0 0
  in
  if inverted then List.rev list else list

let fold_place_short inverted s =
  if inverted
  then match String.index_opt s ',' with
    | Some i -> String.sub s 0 i
    | None -> s
  else begin
    let len = String.length s in
    let default () =
      let i =
        match String.rindex_opt s ',' with
        | Some i ->
          let rec l i = if i < len && String.unsafe_get s i = ' ' then l (i + 1) else i in l (i + 1)
        | None -> 0
      in
      let i = if i = len then 0 else i in
      String.sub s i (len - i)
    in
    if String.unsafe_get s (len - 1) = ')'
    then match String.rindex_opt s '(' with
      | Some i when i < len - 2 ->
        String.sub s (i + 1) (len - i - 2)
      | _ -> default ()
    else default ()
  end

exception List_too_long

let get_all =
  fun conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage
    (dummy_key : 'a)
    (dummy_value : 'c)
    (fold_place : string -> 'a)
    (filter : 'a -> bool)
    (mk_value : 'b option -> person -> 'b)
    (fn : 'b -> 'c)
    (max_length : int) :
    ('a * 'c) array ->
  let ht_size = 2048 in (* FIXME: find the good heuristic *)
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
  let ht_add istr p =
    let key : 'a = sou base istr |> normalize |> fold_place in
    if filter key then begin
      begin match Hashtbl.find_opt ht key with
      | Some _ as prev -> Hashtbl.replace ht key (mk_value prev p)
      | None ->
        Hashtbl.add ht key (mk_value None p) ;
        if Hashtbl.length ht > max_length then raise List_too_long
      end
    end
  in
  if add_birth || add_death || add_baptism || add_burial then begin
    let aux b fn p =
      if b then let x = fn p in if not (is_empty_string x) then ht_add x p
    in
    Gwdb.Collection.iter (fun i ->
        let p = pget conf base i in
        if authorized_age conf base p then begin
          aux add_birth get_birth_place p ;
          aux add_baptism get_baptism_place p ;
          aux add_death get_death_place p ;
          aux add_burial get_burial_place p ;
        end)
    (Gwdb.ipers base) ;
  end ;
  if add_marriage then begin
    Gwdb.Collection.iter (fun i ->
        let fam = foi base i in
          let pl_ma = get_marriage_place fam in
          if not (is_empty_string pl_ma) then
            let fath = pget conf base (get_father fam) in
            let moth = pget conf base (get_mother fam) in
            if authorized_age conf base fath
            && authorized_age conf base moth
            then begin
              ht_add pl_ma fath ;
              ht_add pl_ma moth
        end)
      (Gwdb.ifams base) ;
  end ;
  let len = Hashtbl.length ht in
  let array = Array.make len (dummy_key, dummy_value) in
  let i = ref 0 in
  Hashtbl.iter
    (fun k v ->
       Array.unsafe_set array !i (k, fn v) ;
       incr i)
    ht ;
  array

let print_html_places_surnames conf base (array : (string list * (string * iper list) list) array) =
  let link_to_ind =
    match p_getenv conf.base_env "place_surname_link_to_ind" with
      Some "yes" -> true
    | _ -> false
  in
  let print_sn (sn, ips) =
    let len = List.length ips in
    Wserver.printf "<a href=\"%s" (commd conf);
    if link_to_ind && len = 1
    then Wserver.printf "%s" (acces conf base @@ pget conf base @@ List.hd ips)
    else Wserver.printf "m=N&v=%s" (code_varenv sn);
    Wserver.printf "\">%s</a> (%d)" sn len
  in
  let print_sn_list (snl : (string * iper list) list) =
    let snl = List.sort (fun (sn1, _) (sn2, _) -> Gutil.alphabetic_order sn1 sn2) snl in
    Wserver.printf "<li>\n";
    Mutil.list_iter_first (fun first x -> if not first then Wserver.printf ",\n" ; print_sn x) snl ;
    Wserver.printf "\n";
    Wserver.printf "</li>\n"
  in
  let rec loop prev =
    function
      (pl, snl) :: list ->
        let rec loop1 prev pl =
          match prev, pl with
            [], l2 -> List.iter (fun x -> Wserver.printf "<li>%s<ul>\n" x) l2
          | x1 :: l1, x2 :: l2 ->
              if x1 = x2 then loop1 l1 l2
              else
                begin
                  List.iter (fun _ -> Wserver.printf "</ul></li>\n")
                    (x1 :: l1);
                  loop1 [] (x2 :: l2)
                end
          | _ -> assert false
        in
        loop1 prev pl;
        print_sn_list snl;
        loop pl list
    | [] -> List.iter (fun _ -> Wserver.printf "</ul></li>\n") prev
  in
  Wserver.printf "<ul>\n";
  loop [] (Array.to_list array) ;
  Wserver.printf "</ul>\n"

let print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage =
    (if add_birth then "&bi=on" else "") ^
    (if add_baptism then "&bp=on" else "") ^
    (if add_death then "&de=on" else "") ^
    (if add_burial then "&bu=on" else "") ^
    (if add_marriage then "&ma=on" else "")

let print_aux conf title fn =
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  fn () ;
  Hutil.trailer conf

let print_all_places_surnames_short conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage =
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let array =
    get_all
      conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage
      "" 0
      (fold_place_short inverted)
      (fun _ -> true)
      (fun prev _ -> match prev with Some n -> n + 1 | None -> 1)
      (fun x -> x)
      max_int
  in
  Array.sort (fun (s1, _) (s2, _) -> Gutil.alphabetic_order s1 s2) array ;
  let title _ = Wserver.printf "%s" (capitale (transl conf "place")) in
  print_aux conf title begin fun () ->
    let opt = print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage in
    Wserver.printf
      "<p><a href=\"%sm=PS%s&display=long\">%s</a></p><p>"
      (commd conf) opt (transl conf "long display") ;
    let last = Array.length array - 1 in
    Array.iteri
      (fun i (s, x) ->
         Wserver.printf "<a href=\"%sm=PS%s&k=%s\">%s</a> (%d)%s"
           (commd conf) opt (Util.code_varenv s) s x (if i = last then "" else ",\n"))
      array ;
    Wserver.printf "</p>\n"
  end

let print_all_places_surnames_long conf base ini ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage max_length =
  let filter = if ini = "" then fun _ -> true else fun x -> List.hd x = ini in
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let array =
    get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage
      [] [] (fold_place_long inverted) filter
      (fun prev p ->
         let value = (get_surname p, get_iper p) in
         match prev with Some list -> value :: list | None -> [ value ])
      (fun v ->
         let v = List.sort (fun (a, _) (b, _) -> compare a b) v in
         let rec loop acc list = match list, acc with
           | [], _ -> acc
           | (sn, iper) :: tl_list, (sn', iper_list) :: tl_acc when (sou base sn) = sn' ->
             loop ((sn', iper:: iper_list) :: tl_acc) tl_list
           | (sn, iper) :: tl_list, _ ->
             loop ((sou base sn, [iper]) :: acc) tl_list
         in
         loop [] v)
      max_length
  in
  let rec sort_place_utf8 pl1 pl2 =
    match pl1, pl2 with
    | _, [] -> 1
    | [], _ -> -1
    | s1 :: pl11, s2 :: pl22 ->
      match Gutil.alphabetic_order s1 s2 with
      | 0 -> sort_place_utf8 pl11 pl22
      | x -> x
  in
  Array.sort (fun (pl1, _) (pl2, _) -> sort_place_utf8 pl1 pl2) array ;
  let title _ =
    Wserver.printf "%s / %s" (capitale (transl conf "place"))
      (capitale (transl_nth conf "surname/surnames" 0))
  in
  print_aux conf title begin fun () ->
    if ini = ""
    then
      Wserver.printf "<p><a href=\"%sm=PS%s&display=short\">%s</a></p><p>"
        (commd conf)
        (print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage)
        (transl conf "short display") ;
    if array <> [||] then print_html_places_surnames conf base array;
  end

let print_all_places_surnames conf base =
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  match p_getenv conf.env "k" with
  | Some ini ->
    print_all_places_surnames_long conf base ini ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage max_int
  | None ->
    match p_getenv conf.env "display" with
    | Some "long" ->
      print_all_places_surnames_long conf base "" ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage max_int
    | Some "short" -> print_all_places_surnames_short conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage
    | Some _ -> assert false
    | None ->
      try
        let lim = try int_of_string @@ List.assoc "short_place_threshold" conf.base_env with _ -> 500 in
        print_all_places_surnames_long conf base "" ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage lim
      with List_too_long -> print_all_places_surnames_short conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage


(* ********************************8 *)

open Def

let key = Name.strip_lower

let make_ht list =
  let ht = Hashtbl.create (Array.fold_left (fun acc (_, a) -> acc + Array.length a) 0 list) in
  Array.iter
    (fun (v, list) ->
       Array.map key list
       |> Array.to_list
       |> List.sort_uniq compare
       |> List.iter (fun k -> Hashtbl.add ht k v)
       )
    list ;
  ht

type country =
  | Afghanistan
  | Albania
  | Algeria
  | Andorra
  | Angola
  | Antigua_and_Barbuda
  | Argentina
  | Armenia
  | Australia
  | Austria
  | Azerbaijan
  | Bahrain
  | Bangladesh
  | Barbados
  | Belarus
  | Belgium
  | Belize
  | Benin
  | Bhutan
  | Bolivia
  | Bosnia_and_Herzegovina
  | Botswana
  | Brazil
  | Brunei
  | Bulgaria
  | Burkina_Faso
  | Burundi
  | Cabo_Verde
  | Cambodia
  | Cameroon
  | Canada
  | Central_African_Republic
  | Chad
  | Chile
  | China
  | Colombia
  | Comoros
  | Costa_Rica
  | Croatia
  | Cuba
  | Cyprus
  | Czech_Republic
  | Cote_d_Ivoire
  | Democratic_Republic_of_the_Congo
  | Denmark
  | Djibouti
  | Dominica
  | Dominican_Republic
  | East_Timor
  | Ecuador
  | Egypt
  | El_Salvador
  | Equatorial_Guinea
  | Eritrea
  | Estonia
  | Eswatini
  | Ethiopia
  | Federated_States_of_Micronesia
  | Fiji
  | Finland
  | France
  | Gabon
  | Georgia
  | Germany
  | Ghana
  | Greece
  | Grenada
  | Guatemala
  | Guinea
  | Guinea_Bissau
  | Guyana
  | Haiti
  | Honduras
  | Hungary
  | Iceland
  | India
  | Indonesia
  | Iran
  | Iraq
  | Ireland
  | Israel
  | Italy
  | Jamaica
  | Japan
  | Jordan
  | Kazakhstan
  | Kenya
  | Kiribati
  | Kosovo
  | Kuwait
  | Kyrgyzstan
  | Laos
  | Latvia
  | Lebanon
  | Lesotho
  | Liberia
  | Libya
  | Liechtenstein
  | Lithuania
  | Luxembourg
  | Madagascar
  | Malawi
  | Malaysia
  | Maldives
  | Mali
  | Malta
  | Marshall_Islands
  | Mauritania
  | Mauritius
  | Mexico
  | Moldova
  | Monaco
  | Mongolia
  | Montenegro
  | Morocco
  | Mozambique
  | Myanmar
  | Namibia
  | Nauru
  | Nepal
  | Netherlands
  | New_Zealand
  | Nicaragua
  | Niger
  | Nigeria
  | North_Korea
  | North_Macedonia
  | Norway
  | Oman
  | Pakistan
  | Palau
  | Panama
  | Papua_New_Guinea
  | Paraguay
  | Peru
  | Philippines
  | Poland
  | Portugal
  | Qatar
  | Republic_of_the_Congo
  | Romania
  | Russia
  | Rwanda
  | Saint_Kitts_and_Nevis
  | Saint_Lucia
  | Saint_Vincent_and_the_Grenadines
  | Samoa
  | San_Marino
  | Sao_Tome_and_Principe
  | Saudi_Arabia
  | Senegal
  | Serbia
  | Seychelles
  | Sierra_Leone
  | Singapore
  | Slovakia
  | Slovenia
  | Solomon_Islands
  | Somalia
  | South_Africa
  | South_Korea
  | South_Sudan
  | Spain
  | Sri_Lanka
  | Sudan
  | Suriname
  | Sweden
  | Switzerland
  | Syria
  | Taiwan
  | Tajikistan
  | Tanzania
  | Thailand
  | The_Bahamas
  | The_Gambia
  | Togo
  | Tonga
  | Trinidad_and_Tobago
  | Tunisia
  | Turkey
  | Turkmenistan
  | Tuvalu
  | Uganda
  | Ukraine
  | United_Arab_Emirates
  | United_Kingdom
  | United_States
  | Uruguay
  | Uzbekistan
  | Vanuatu
  | Vatican_City
  | Venezuela
  | Vietnam
  | Yemen
  | Zambia
  | Zimbabwe

let country_list =
  [|Afghanistan,[|"Afghanistan"|]
   ;Albania,[|"Albanie";"Albania"|]
   ;Algeria,[|"Algérie";"Algeria";"Algérie française"|]
   ;Andorra,[|"Andorre";"Andorra"|]
   ;Angola,[|"Angola"|]
   ;Antigua_and_Barbuda,[|"Antigua-et-Barbuda";"Antigue-et-Barbude";"Antigua and Barbuda"|]
   ;Argentina,[|"Argentine";"Argentina"|]
   ;Armenia,[|"Arménie";"Armenia"|]
   ;Australia,[|"Australie";"Australia"|]
   ;Austria,[|"Autriche";"Austria"|]
   ;Azerbaijan,[|"Azerbaïdjan";"Azerbaijan"|]
   ;The_Bahamas,[|"Bahamas";"The Bahamas"|]
   ;Bahrain,[|"Bahreïn";"Bahrain"|]
   ;Bangladesh,[|"Bangladesh"|]
   ;Barbados,[|"Barbade";"Barbados"|]
   ;Belarus,[|"Biélorussie";"Belarus"|]
   ;Belgium,[|"Belgique";"Belgium"|]
   ;Belize,[|"Belize"|]
   ;Benin,[|"Bénin";"Benin"|]
   ;Bhutan,[|"Bhoutan";"Bhutan"|]
   ;Bolivia,[|"Bolivie";"Bolivia"|]
   ;Bosnia_and_Herzegovina,[|"Bosnie-Herzégovine";"Bosnia and Herzegovina"|]
   ;Botswana,[|"Botswana"|]
   ;Brazil,[|"Brésil";"Brazil"|]
   ;Brunei,[|"Brunei"|]
   ;Bulgaria,[|"Bulgarie";"Bulgaria"|]
   ;Burkina_Faso,[|"Burkina Faso"|]
   ;Burundi,[|"Burundi"|]
   ;Cabo_Verde,[|"Cap-Vert";"Cabo Verde"|]
   ;Cambodia,[|"Cambodge";"Cambodia"|]
   ;Cameroon,[|"Cameroun";"Cameroon"|]
   ;Canada,[|"Canada"|]
   ;Central_African_Republic,[|"République centrafricaine";"Central African Republic"|]
   ;Chad,[|"Tchad";"Chad"|]
   ;Chile,[|"Chili";"Chile"|]
   ;China,[|"Chine";"China"|]
   ;Colombia,[|"Colombie";"Colombia"|]
   ;Comoros,[|"Comores";"Comoros"|]
   ;Democratic_Republic_of_the_Congo,[|"Congo-Kinshasa";"République démocratique du Congo";"Zaïre";"Democratic Republic of the Congo"|]
   ;Republic_of_the_Congo,[|"Congo";"Congo-Brazzaville";"Republic of the Congo"|]
   ;Costa_Rica,[|"Costa Rica"|]
   ;Cote_d_Ivoire,[|"Côte d’Ivoire";"Ivory Coast"|]
   ;Croatia,[|"Croatie";"Croatia"|]
   ;Cuba,[|"Cuba"|]
   ;Cyprus,[|"Chypre";"Cyprus"|]
   ;Czech_Republic,[|"Tchéquie";"République tchèque";"Czech Republic"|]
   ;Denmark,[|"Danemark";"Denmark"|]
   ;Djibouti,[|"Djibouti"|]
   ;Dominica,[|"Dominique";"Dominica"|]
   ;Dominican_Republic,[|"République dominicaine";"Dominican Republic"|]
   ;East_Timor,[|"Timor oriental";"East Timor";"Timor-Leste"|]
   ;Ecuador,[|"Équateur";"Ecuador"|]
   ;Egypt,[|"Égypte";"Egypt"|]
   ;El_Salvador,[|"Salvador";"El Salvador"|]
   ;Equatorial_Guinea,[|"Guinée équatoriale";"Equatorial Guinea"|]
   ;Eritrea,[|"Érythrée";"Eritrea"|]
   ;Estonia,[|"Estonie";"Estonia"|]
   ;Eswatini,[|"Eswatini"|]
   ;Ethiopia,[|"Éthiopie";"Ethiopia"|]
   ;Fiji,[|"Fidji"|]
   ;Finland,[|"Finlande";"Finland"|]
   ;France,[|"France"|]
   ;Gabon,[|"Gabon"|]
   ;The_Gambia,[|"Gambie";"The Gambia"|]
   ;Georgia,[|"Géorgie";"Georgia"|]
   ;Germany,[|"Allemagne";"Germany"|]
   ;Ghana,[|"Ghana"|]
   ;Greece,[|"Grèce";"Greece"|]
   ;Grenada,[|"Grenade";"Grenada"|]
   ;Guatemala,[|"Guatemala"|]
   ;Guinea,[|"Guinée";"Guinea"|]
   ;Guinea_Bissau,[|"Guinée-Bissau";"Guinea-Bissau"|]
   ;Guyana,[|"Guyana"|]
   ;Haiti,[|"Haïti";"Haiti"|]
   ;Honduras,[|"Honduras"|]
   ;Hungary,[|"Hongrie";"Hungary"|]
   ;Iceland,[|"Islande";"Iceland"|]
   ;India,[|"Inde";"India"|]
   ;Indonesia,[|"Indonésie";"Indonesia"|]
   ;Iran,[|"Iran"|]
   ;Iraq,[|"Irak";"Iraq"|]
   ;Ireland,[|"Irlande"|]
   ;Israel,[|"Israël";"Israel"|]
   ;Italy,[|"Italy";"Italie"|]
   ;Jamaica,[|"Jamaïque";"Jamaica"|]
   ;Japan,[|"Japon";"Japan"|]
   ;Jordan,[|"Jordanie";"Jordan"|]
   ;Kazakhstan,[|"Kazakhstan"|]
   ;Kenya,[|"Kenya"|]
   ;Kiribati,[|"Kiribati"|]
   ;North_Korea,[|"Corée du Nord"|]
   ;South_Korea,[|"Corée du Sud"|]
   ;Kosovo,[|"Kosovo"|]
   ;Kuwait,[|"Koweït";"Kuwait"|]
   ;Kyrgyzstan,[|"Kirghizistan";"Kyrgyzstan"|]
   ;Laos,[|"Laos"|]
   ;Latvia,[|"Lettonie";"Latvia"|]
   ;Lebanon,[|"Liban";"Lebanon"|]
   ;Lesotho,[|"Lesotho"|]
   ;Liberia,[|"Libéria";"Liberia"|]
   ;Libya,[|"Libye";"Libya"|]
   ;Liechtenstein,[|"Liechtenstein"|]
   ;Lithuania,[|"Lituanie";"Lithuania"|]
   ;Luxembourg,[|"Luxembourg"|]
   ;Madagascar,[|"Madagascar"|]
   ;Malawi,[|"Malawi"|]
   ;Malaysia,[|"Malaisie";"Malaysia"|]
   ;Maldives,[|"Maldives"|]
   ;Mali,[|"Mali"|]
   ;Malta,[|"Malte"|]
   ;Marshall_Islands,[|"Îles Marshall";"Marshall Islands"|]
   ;Mauritania,[|"Mauritanie"|]
   ;Mauritius,[|"Maurice";"Mauritius"|]
   ;Mexico,[|"Mexique";"Mexico"|]
   ;Moldova,[|"Moldavie";"Moldova"|]
   ;Monaco,[|"Monaco"|]
   ;Mongolia,[|"Mongolie";"Mongolia"|]
   ;Montenegro,[|"Monténégro";"Montenegro"|]
   ;Morocco,[|"Maroc";"Morocco"|]
   ;Mozambique,[|"Mozambique"|]
   ;Myanmar,[|"Myanmar";"Birmanie";"Burma"|]
   ;Namibia,[|"Namibie"|]
   ;Nauru,[|"Nauru"|]
   ;Nepal,[|"Népal";"Nepal"|]
   ;Netherlands,[|"Pays-Bas";"Netherlands"|]
   ;New_Zealand,[|"Nouvelle-Zélande";"New Zealand"|]
   ;Nicaragua,[|"Nicaragua"|]
   ;Niger,[|"Niger"|]
   ;Nigeria,[|"Nigéria";"Nigeria"|]
   ;North_Macedonia,[|"Macédoine du nord";"North Macedonia"|]
   ;Norway,[|"Norvège"|]
   ;Oman,[|"Oman"|]
   ;Pakistan,[|"Pakistan"|]
   ;Palau,[|"Palaos";"Palau"|]
   ;Panama,[|"Panama"|]
   ;Papua_New_Guinea,[|"Papouasie-Nouvelle-Guinée";"Papua New Guinea"|]
   ;Paraguay,[|"Paraguay"|]
   ;Peru,[|"Pérou";"Peru"|]
   ;Philippines,[|"Philippines"|]
   ;Poland,[|"Pologne";"Poland"|]
   ;Portugal,[|"Portugal"|]
   ;Qatar,[|"Qatar"|]
   ;Romania,[|"Roumanie";"Romania"|]
   ;Russia,[|"Russie";"Russia"|]
   ;Rwanda,[|"Rwanda"|]
   ;Saint_Kitts_and_Nevis,[|"Saint-Christophe-et-Niévès";"Saint-Christophe-et-Nevis";"Saint Kitts and Nevis"|]
   ;Saint_Lucia,[|"Sainte-Lucie";"Saint Lucia"|]
   ;Saint_Vincent_and_the_Grenadines,[|"Saint-Vincent-et-les-Grenadines"|]
   ;Samoa,[|"Samoa"|]
   ;San_Marino,[|"Saint-Marin";"San Marino"|]
   ;Sao_Tome_and_Principe,[|"Sao Tomé-et-Principe";"Sao Tome and Principe"|]
   ;Saudi_Arabia,[|"Arabie saoudite";"Saudi Arabia"|]
   ;Senegal,[|"Sénégal";"Senegal"|]
   ;Serbia,[|"Serbie";"Serbia"|]
   ;Seychelles,[|"Seychelles"|]
   ;Sierra_Leone,[|"Sierra Leone"|]
   ;Singapore,[|"Singapour"|]
   ;Slovakia,[|"Slovaquie";"Slovakia"|]
   ;Slovenia,[|"Slovénie";"Slovenia"|]
   ;Solomon_Islands,[|"Îles Salomon";"Solomon Islands"|]
   ;Somalia,[|"Somalie";"Somalia"|]
   ;South_Africa,[|"Afrique du Sud";"South Africa"|]
   ;Spain,[|"Espagne";"Spain"|]
   ;Sri_Lanka,[|"Sri Lanka"|]
   ;Sudan,[|"Soudan";"Sudan"|]
   ;South_Sudan,[|"Soudan du Sud";"South_Sudan"|]
   ;Suriname,[|"Suriname"|]
   ;Sweden,[|"Suède";"Sweden"|]
   ;Switzerland,[|"Suisse";"Switzerland"|]
   ;Syria,[|"Syrie";"Syria"|]
   ;Taiwan,[|"Taïwan";"Taiwan"|]
   ;Tajikistan,[|"Tadjikistan";"Tajikistan"|]
   ;Tanzania,[|"Tanzanie";"Tanzania"|]
   ;Thailand,[|"Thaïlande";"Thailand"|]
   ;Togo,[|"Togo"|]
   ;Tonga,[|"Tonga"|]
   ;Trinidad_and_Tobago,[|"Trinité et Tobago";"Trinidad and Tobago"|]
   ;Tunisia,[|"Tunisie";"Tunisia"|]
   ;Turkey,[|"Turquie";"Turkey"|]
   ;Turkmenistan,[|"Turkménistan";"Turkmenistan"|]
   ;Tuvalu,[|"Tuvalu"|]
   ;Uganda,[|"Ouganda";"Uganda"|]
   ;Ukraine,[|"Ukraine"|]
   ;United_Arab_Emirates,[|"Émirats arabes unis";"United Arab Emirates"|]
   ;United_Kingdom,[|"UK";"Royaume-Uni";"United Kingdom"|]
   ;United_States,[|"US";"USA";"États-Unis";"United States"|]
   ;Uruguay,[|"Uruguay"|]
   ;Uzbekistan,[|"Ouzbékistan";"Uzbekistan"|]
   ;Vanuatu,[|"Vanuatu"|]
   ;Vatican_City,[|"Vatican";"Vatican City"|]
   ;Venezuela,[|"Venezuela"|]
   ;Vietnam,[|"Vietnam"|]
   ;Yemen,[|"Yémen"|]
   ;Zambia,[|"Zambie";"Zambia";"Rhodésie du Nord";"Northern Rhodesia"|]
   ;Zimbabwe,[|"Zimbabwe";"Rhodésie du Sud";"Southern Rhodesia"|]
  |]

let country_ht = make_ht country_list

type region =

  | FR_Auvergne_Rhone_Alpes
  | FR_Bourgogne_Franche_Comte
  | FR_Bretagne
  | FR_Centre_Val_de_Loire
  | FR_Corse
  | FR_Grand_Est
  | FR_Hauts_de_France
  | FR_Ile_de_France
  | FR_Normandie
  | FR_Nouvelle_Aquitaine
  | FR_Occitanie
  | FR_Pays_de_la_Loire
  | FR_Provence_Alpes_Cote_d_Azur
  | FR_Guadeloupe
  | FR_Martinique
  | FR_Guyane
  | FR_La_reunion
  | FR_Mayotte

  | DE_Baden_Wurttemberg
  | DE_Niedersachsen
  | DE_Bayern
  | DE_Berlin
  | DE_Brandenburg
  | DE_Bremen
  | DE_Hamburg
  | DE_Hessen
  | DE_Mecklenburg_Vorpommern
  | DE_Nordrhein_Westfalen
  | DE_Rheinland_Pfalz
  | DE_Saarland
  | DE_Sachsen_Anhalt
  | DE_Sachsen
  | DE_Schleswig_Holstein
  | DE_Thuringen

  | AU_South_Australia
  | AU_Western_Australia
  | AU_New_South_Wales
  | AU_Queensland
  | AU_Tasmania
  | AU_Victoria

  | AT_Niederosterreich
  | AT_Burgenland
  | AT_Karnten
  | AT_Oberosterreich
  | AT_Salzburg
  | AT_Steiermark
  | AT_Tirol
  | AT_Wien
  | AT_Vorarlberg

  | BE_Bruxelles_Capitale
  | BE_Flandre
  | BE_Wallonie

  | US_Alabama
  | US_Alaska
  | US_Arizona
  | US_Arkansas
  | US_California
  | US_North_Carolina
  | US_South_Carolina
  | US_Colorado
  | US_Connecticut
  | US_North_Dakota
  | US_South_Dakota
  | US_Delaware
  | US_Florida
  | US_Georgia
  | US_Hawaii
  | US_Idaho
  | US_Illinois
  | US_Indiana
  | US_Iowa
  | US_Kansas
  | US_Kentucky
  | US_Louisiana
  | US_Maine
  | US_Maryland
  | US_Massachusetts
  | US_Michigan
  | US_Minnesota
  | US_Mississippi
  | US_Missouri
  | US_Montana
  | US_Nebraska
  | US_Nevada
  | US_New_Hampshire
  | US_New_Jersey
  | US_New_Mexico
  | US_New_York
  | US_Ohio
  | US_Oklahoma
  | US_Oregon
  | US_Pennsylvania
  | US_Rhode_Island
  | US_Tennessee
  | US_Texas
  | US_Utah
  | US_Vermont
  | US_Virginia
  | US_West_Virginia
  | US_Washington
  | US_Wisconsin
  | US_Wyoming

  | ZA_Eastern_Cape
  | ZA_Free_State
  | ZA_Gauteng
  | ZA_KwaZulu_Natal
  | ZA_Limpopo
  | ZA_Mpumalanga
  | ZA_North_West
  | ZA_Northern_Cape
  | ZA_Western_Cape

  | CA_Alberta
  | CA_British_Columbia
  | CA_Prince_Edward_Island
  | CA_Manitoba
  | CA_New_Brunswick
  | CA_Nova_Scotia
  | CA_Ontario
  | CA_Quebec
  | CA_Saskatchewan
  | CA_Newfoundland_and_Labrador
  | CA_Nunavut
  | CA_Northwest_Territories
  | CA_Yukon

  | DZ_Adrar
  | DZ_Chlef
  | DZ_Laghouat
  | DZ_Oum_El_Bouaghi
  | DZ_Batna
  | DZ_Beaia
  | DZ_Biskra
  | DZ_Bechar
  | DZ_Blida
  | DZ_Bouira
  | DZ_Tamanrasset
  | DZ_Tebessa
  | DZ_Tlemcen
  | DZ_Tiaret
  | DZ_Tizi_Ouzou
  | DZ_Alger
  | DZ_Djelfa
  | DZ_Jijel
  | DZ_Setif
  | DZ_Saida
  | DZ_Skikda
  | DZ_Sidi_Bel_Abbes
  | DZ_Annaba
  | DZ_Guelma
  | DZ_Constantine
  | DZ_Medea
  | DZ_Mostaganem
  | DZ_M_Sila
  | DZ_Mascara
  | DZ_Ouargla
  | DZ_Oran
  | DZ_El_Bayadh
  | DZ_Illizi
  | DZ_Bordj_Bou_Arreridj
  | DZ_Boumerdes
  | DZ_El_Tarf
  | DZ_Tindouf
  | DZ_Tissemsilt
  | DZ_El_Oued
  | DZ_Khenchela
  | DZ_Souk_Ahras
  | DZ_Tipaza
  | DZ_Mila
  | DZ_Ain_Defla
  | DZ_Naaa
  | DZ_Ai_Teouchent
  | DZ_Ghardai
  | DZ_Relizane

let algeria_region =
  [|DZ_Adrar,[|"Adrar"|]
   ;DZ_Chlef,[|"Chlef"|]
   ;DZ_Laghouat,[|"Laghouat"|]
   ;DZ_Oum_El_Bouaghi,[|"Oum El Bouaghi"|]
   ;DZ_Batna,[|"Batna"|]
   ;DZ_Beaia,[|"Béjaïa"|]
   ;DZ_Biskra,[|"Biskra"|]
   ;DZ_Bechar,[|"Béchar"|]
   ;DZ_Blida,[|"Blida"|]
   ;DZ_Bouira,[|"Bouira"|]
   ;DZ_Tamanrasset,[|"Tamanrasset"|]
   ;DZ_Tebessa,[|"Tébessa"|]
   ;DZ_Tlemcen,[|"Tlemcen"|]
   ;DZ_Tiaret,[|"Tiaret"|]
   ;DZ_Tizi_Ouzou,[|"Tizi Ouzou"|]
   ;DZ_Alger,[|"Alger"|]
   ;DZ_Djelfa,[|"Djelfa"|]
   ;DZ_Jijel,[|"Jijel"|]
   ;DZ_Setif,[|"Sétif"|]
   ;DZ_Saida,[|"Saïda"|]
   ;DZ_Skikda,[|"Skikda"|]
   ;DZ_Sidi_Bel_Abbes,[|"Sidi Bel Abbès"|]
   ;DZ_Annaba,[|"Annaba"|]
   ;DZ_Guelma,[|"Guelma"|]
   ;DZ_Constantine,[|"Constantine"|]
   ;DZ_Medea,[|"Médéa"|]
   ;DZ_Mostaganem,[|"Mostaganem"|]
   ;DZ_M_Sila,[|"M'Sila"|]
   ;DZ_Mascara,[|"Mascara"|]
   ;DZ_Ouargla,[|"Ouargla"|]
   ;DZ_Oran,[|"Oran"|]
   ;DZ_El_Bayadh,[|"El Bayadh"|]
   ;DZ_Illizi,[|"Illizi"|]
   ;DZ_Bordj_Bou_Arreridj,[|"Bordj Bou Arreridj"|]
   ;DZ_Boumerdes,[|"Boumerdès"|]
   ;DZ_El_Tarf,[|"El Tarf"|]
   ;DZ_Tindouf,[|"Tindouf"|]
   ;DZ_Tissemsilt,[|"Tissemsilt"|]
   ;DZ_El_Oued,[|"El Oued"|]
   ;DZ_Khenchela,[|"Khenchela"|]
   ;DZ_Souk_Ahras,[|"Souk Ahras"|]
   ;DZ_Tipaza,[|"Tipaza"|]
   ;DZ_Mila,[|"Mila"|]
   ;DZ_Ain_Defla,[|"Aïn Defla"|]
   ;DZ_Naaa,[|"Naâma"|]
   ;DZ_Ai_Teouchent,[|"Aïn Témouchent"|]
   ;DZ_Ghardai,[|"Ghardaïa"|]
   ;DZ_Relizane,[|"Relizane"|]
  |]

let algeria_region_ht = make_ht algeria_region

type subregion =

  | FR_Ain
  | FR_Aisne
  | FR_Allier
  | FR_Alpes_de_Haute_Provence
  | FR_Hautes_alpes
  | FR_Alpes_maritimes
  | FR_Ardeche
  | FR_Ardennes
  | FR_Ariege
  | FR_Aube
  | FR_Aude
  | FR_Aveyron
  | FR_Bouches_du_Rhone
  | FR_Calvados
  | FR_Cantal
  | FR_Charente
  | FR_Charente_maritime
  | FR_Cher
  | FR_Correze
  | FR_Corse
  (* | FR_Corse_du_sud
   * | FR_Haute_Corse *)
  | FR_Cote_d_Or
  | FR_Cotes_d_Armor
  | FR_Creuse
  | FR_Dordogne
  | FR_Doubs
  | FR_Drome
  | FR_Eure
  | FR_Eure_et_loir
  | FR_Finistere
  | FR_Gard
  | FR_Haute_garonne
  | FR_Gers
  | FR_Gironde
  | FR_Herault
  | FR_Ille_et_vilaine
  | FR_Indre
  | FR_Indre_et_loire
  | FR_Isere
  | FR_Jura
  | FR_Landes
  | FR_Loir_et_cher
  | FR_Loire
  | FR_Haute_loire
  | FR_Loire_atlantique
  | FR_Loiret
  | FR_Lot
  | FR_Lot_et_garonne
  | FR_Lozere
  | FR_Maine_et_loire
  | FR_Manche
  | FR_Marne
  | FR_Haute_marne
  | FR_Mayenne
  | FR_Meurthe_et_moselle
  | FR_Meuse
  | FR_Morbihan
  | FR_Moselle
  | FR_Nievre
  | FR_Nord
  | FR_Oise
  | FR_Orne
  | FR_Pas_de_calais
  | FR_Puy_de_dome
  | FR_Pyrenees_atlantiques
  | FR_Hautes_Pyrenees
  | FR_Pyrenees_orientales
  | FR_Bas_rhin
  | FR_Haut_rhin
  | FR_Rhone
  | FR_Haute_saone
  | FR_Saone_et_loire
  | FR_Sarthe
  | FR_Savoie
  | FR_Haute_savoie
  | FR_Paris
  | FR_Seine_maritime
  | FR_Seine_et_marne
  | FR_Yvelines
  | FR_Deux_sevres
  | FR_Somme
  | FR_Tarn
  | FR_Tarn_et_garonne
  | FR_Var
  | FR_Vaucluse
  | FR_Vendee
  | FR_Vienne
  | FR_Haute_vienne
  | FR_Vosges
  | FR_Yonne
  | FR_Territoire_de_belfort
  | FR_Essonne
  | FR_Hauts_de_seine
  | FR_Seine_Saint_Denis
  | FR_Val_de_marne
  | FR_Val_d_oise
  | FR_Guadeloupe
  | FR_Martinique
  | FR_Guyane
  | FR_La_reunion
  | FR_Mayotte

  | BE_Anvers
  | BE_Brabant_flamand
  | BE_Brabant_wallon
  | BE_Bruxelles_Capitale
  | BE_Flandre_Occidentale
  | BE_Flandre_Orientale
  | BE_Hainaut
  | BE_Liege
  | BE_Limbourg
  | BE_Luxembourg
  | BE_Namur

(* type region =
 * | FR_Ile_de_France
 * | FR_Champagne_Ardenne
 * | FR_Picardie
 * | FR_Haute_Normandie
 * | FR_Centre
 * | FR_Basse_Normandie
 * | FR_Bourgogne
 * | FR_Nord_Pas_de_Calais
 * | FR_Lorraine
 * | FR_Alsace
 * | FR_Franche_Comté
 * | FR_Pays_de_la_Loire
 * | FR_Bretagne
 * | FR_Poitou_Charentes
 * | FR_Aquitaine
 * | FR_Midi_Pyrénées
 * | FR_Limousin
 * | FR_Rhône_Alpes
 * | FR_Auvergne
 * | FR_Languedoc_Roussillon
 * | FR_Provence_Alpes_Côte
 * | FR_Corse *)

let [@warning "-42"] france_region =
  [|FR_Auvergne_Rhone_Alpes,[|"Auvergne-Rhône-Alpes";"Auvergne";"Rhône-Alpes"|]
   ;FR_Bourgogne_Franche_Comte,[|"Bourgogne-Franche-Comté";"Bourgogne";"Franche-Comté"|]
   ;FR_Bretagne,[|"Bretagne"|]
   ;FR_Centre_Val_de_Loire,[|"Centre-Val de Loire";"Centre"|]
   ;FR_Corse,[|"Corse"|]
   ;FR_Grand_Est,[|"Grand Est";"Alsace";"Champagne-Ardenne";"Lorraine"|]
   ;FR_Hauts_de_France,[|"Hauts-de-France";"Nord-Pas-de-Calais";"Picardie"|]
   ;FR_Ile_de_France,[|"Île-de-France"|]
   ;FR_Normandie,[|"Normandie";"Haute-Normandie";"Basse-Normandie"|]
   ;FR_Nouvelle_Aquitaine,[|"Nouvelle-Aquitaine";"Aquitaine";"Limousin";"Poitou-Charentes"|]
   ;FR_Occitanie,[|"Occitanie";"Languedoc-Roussillon";"Midi-Pyrénées"|]
   ;FR_Pays_de_la_Loire,[|"Pays de la Loire"|]
   ;FR_Provence_Alpes_Cote_d_Azur,[|"Provence-Alpes-Côte d'Azur"|]
   ;FR_Guadeloupe,[|"Guadeloupe"|]
   ;FR_Martinique,[|"Martinique"|]
   ;FR_Guyane,[|"Guyane"|]
   ;FR_La_reunion,[|"La réunion"|]
   ;FR_Mayotte,[|"Mayotte"|]
  |]

let france_region_ht = make_ht france_region

let south_africa_region =
  [|ZA_Eastern_Cape,[|"Cap-Oriental";"Eastern Cape"|]
   ;ZA_Free_State,[|"État-Libre";"Free State"|]
   ;ZA_Gauteng,[|"Gauteng"|]
   ;ZA_KwaZulu_Natal,[|"KwaZulu-Natal";"KZN";"Natal";"KwaZulu"|]
   ;ZA_Limpopo,[|"Limpopo"|]
   ;ZA_Mpumalanga,[|"Mpumalanga"|]
   ;ZA_North_West,[|"Nord-Ouest";"North West"|]
   ;ZA_Northern_Cape,[|"Cap-Nord";"Northern Cape"|]
   ;ZA_Western_Cape,[|"Cap-Occidental";"Western Cape"|]
  |]

let south_africa_region_ht = make_ht south_africa_region

let united_states_region =
  [|US_Alabama,[|"Alabama";"Alabama"|]
   ;US_Alaska,[|"Alaska";"Alaska"|]
   ;US_Arizona,[|"Arizona";"Arizona"|]
   ;US_Arkansas,[|"Arkansas";"Arkansas"|]
   ;US_California,[|"Californie";"California"|]
   ;US_North_Carolina,[|"Caroline du Nord";"North Carolina"|]
   ;US_South_Carolina,[|"Caroline du Sud";"South Carolina"|]
   ;US_Colorado,[|"Colorado";"Colorado"|]
   ;US_Connecticut,[|"Connecticut";"Connecticut"|]
   ;US_North_Dakota,[|"Dakota du Nord";"North Dakota"|]
   ;US_South_Dakota,[|"Dakota du Sud";"South Dakota"|]
   ;US_Delaware,[|"Delaware";"Delaware"|]
   ;US_Florida,[|"Floride";"Florida"|]
   ;US_Georgia,[|"Géorgie";"Georgia"|]
   ;US_Hawaii,[|"Hawaï";"Hawaii"|]
   ;US_Idaho,[|"Idaho";"Idaho"|]
   ;US_Illinois,[|"Illinois";"Illinois"|]
   ;US_Indiana,[|"Indiana";"Indiana"|]
   ;US_Iowa,[|"Iowa";"Iowa"|]
   ;US_Kansas,[|"Kansas";"Kansas"|]
   ;US_Kentucky,[|"Kentucky";"Kentucky"|]
   ;US_Louisiana,[|"Louisiane";"Louisiana"|]
   ;US_Maine,[|"Maine";"Maine"|]
   ;US_Maryland,[|"Maryland";"Maryland"|]
   ;US_Massachusetts,[|"Massachusetts";"Massachusetts"|]
   ;US_Michigan,[|"Michigan";"Michigan"|]
   ;US_Minnesota,[|"Minnesota";"Minnesota"|]
   ;US_Mississippi,[|"Mississippi";"Mississippi"|]
   ;US_Missouri,[|"Missouri";"Missouri"|]
   ;US_Montana,[|"Montana";"Montana"|]
   ;US_Nebraska,[|"Nebraska";"Nebraska"|]
   ;US_Nevada,[|"Nevada";"Nevada"|]
   ;US_New_Hampshire,[|"New Hampshire";"New Hampshire"|]
   ;US_New_Jersey,[|"New Jersey";"New Jersey"|]
   ;US_New_Mexico,[|"Nouveau-Mexique";"New Mexico"|]
   ;US_New_York,[|"New York";"New York"|]
   ;US_Ohio,[|"Ohio";"Ohio"|]
   ;US_Oklahoma,[|"Oklahoma";"Oklahoma"|]
   ;US_Oregon,[|"Oregon";"Oregon"|]
   ;US_Pennsylvania,[|"Pennsylvanie";"Pennsylvania"|]
   ;US_Rhode_Island,[|"Rhode Island";"Rhode Island"|]
   ;US_Tennessee,[|"Tennessee";"Tennessee"|]
   ;US_Texas,[|"Texas";"Texas"|]
   ;US_Utah,[|"Utah";"Utah"|]
   ;US_Vermont,[|"Vermont";"Vermont"|]
   ;US_Virginia,[|"Virginie";"Virginia"|]
   ;US_West_Virginia,[|"Virginie-Occidentale";"West Virginia"|]
   ;US_Washington,[|"Washington";"Washington"|]
   ;US_Wisconsin,[|"Wisconsin";"Wisconsin"|]
   ;US_Wyoming,[|"Wyoming";"Wyoming"|]
  |]

let united_states_region_ht = make_ht united_states_region

let canada_region =
  [|CA_Alberta,[|"Alberta";"Alberta"|]
   ;CA_British_Columbia,[|"Colombie-Britannique";"British Columbia"|]
   ;CA_Prince_Edward_Island,[|"Île-du-Prince-Édouard";"Prince Edward Island"|]
   ;CA_Manitoba,[|"Manitoba";"Manitoba"|]
   ;CA_New_Brunswick,[|"Nouveau-Brunswick";"New Brunswick"|]
   ;CA_Nova_Scotia,[|"Nouvelle-Écosse";"Nova Scotia"|]
   ;CA_Ontario,[|"Ontario";"Ontario"|]
   ;CA_Quebec,[|"Québec";"Québec"|]
   ;CA_Saskatchewan,[|"Saskatchewan";"Saskatchewan"|]
   ;CA_Newfoundland_and_Labrador,[|"Terre-Neuve-et-Labrador";"Newfoundland and Labrador"|]
   ;CA_Nunavut,[|"Nunavut";"Nunavut"|]
   ;CA_Northwest_Territories,[|"Territoires du Nord-Ouest";"Northwest Territories"|]
   ;CA_Yukon,[|"Yukon";"Yukon"|]
  |]

let canada_region_ht = make_ht canada_region

let [@warning "-42"] belgium_region : (region * string array) array =
  [|BE_Bruxelles_Capitale,[|"Bruxelles-Capitale";"Région bruxelloise";"Région de Bruxelles-Capitale";"Brussels Hoofdstedelijk Gewest"|]
   ;BE_Flandre,[|"Flandre";"Région flamande";"Vlaams Gewest";"Flämische Region"|]
   ;BE_Wallonie,[|"Wallonie";"Région wallonne";"Waals Gewest";"Wallonische Region"|]
  |]

let belgium_region_ht = make_ht belgium_region

let belgium_subregion =
  [|BE_Anvers,[|"Anvers";"Antwerpen"|]
   ;BE_Bruxelles_Capitale,[|"Bruxelles-Capitale";"Région bruxelloise";"Région de Bruxelles-Capitale";"Brussels Hoofdstedelijk Gewest"|]
   ;BE_Limbourg,[|"Limbourg;Limburg"|]
   ;BE_Flandre_Orientale,[|"Flandre-Orientale";"Oost-Vlaanderen"|]
   ;BE_Flandre_Occidentale,[|"Brabant flamand";"Vlaams-Brabant"|]
   ;BE_Brabant_wallon,[|"Flandre-Occidentale";"West-Vlaanderen"|]
   ;BE_Hainaut,[|"Brabant wallon";"Waals-Brabant"|]
   ;BE_Liege,[|"Hainaut";"Henegouwen"|]
   ;BE_Luxembourg,[|"Liège";"Luik"|]
   ;BE_Namur,[|"Luxembourg";"Luxemburg"|]
   ;BE_Namur,[|"Namur";"Namen"|]
  |]

let belgium_subregion_ht = make_ht belgium_subregion

let austria_region =
  [|AT_Niederosterreich,[|"Basse-Autriche";"Niederösterreich"|]
   ;AT_Burgenland,[|"Burgenland";"Burgenland"|]
   ;AT_Karnten,[|"Carinthie";"Kärnten"|]
   ;AT_Oberosterreich,[|"Haute-Autriche";"Oberösterreich"|]
   ;AT_Salzburg,[|"Salzbourg";"Salzburg"|]
   ;AT_Steiermark,[|"Styrie";"Steiermark"|]
   ;AT_Tirol,[|"Tyrol";"Tirol"|]
   ;AT_Wien,[|"Vienne";"Wien"|]
   ;AT_Vorarlberg,[|"Vorarlberg"|]
  |]

let austria_region_ht = make_ht austria_region

let australia_region =
  [|AU_South_Australia,[|"Australie-Méridionale";"South Australia";"SA"|]
   ;AU_Western_Australia,[|"Australie-Occidentale";"Western Australia";"WA"|]
   ;AU_New_South_Wales,[|"Nouvelle-Galles du Sud";"New South Wales";"NSW"|]
   ;AU_Queensland,[|"Queensland";"Queensland";"QLD"|]
   ;AU_Tasmania,[|"Tasmanie";"Tasmania";"TAS"|]
   ;AU_Victoria,[|"Victoria";"Victoria";"VIC"|]
  |]

let australia_region_ht = make_ht australia_region

(* Corse 	Bastia 	1790 	1793
 * Meurthe 	Nancy 	1790 	1871
 * Rhône-et-Loire 	Lyon 	1790 	1793
 * Seine 	Paris 	1790 	1968
 * Seine-et-Oise 	Versailles 	1790 	1968
 * Mont-Blanc 	Chambéry 	1792 	1815
 * Mont-Terrible 	Porrentruy 	1793 	1800
 * Golo 	Bastia 	1793 	1811
 * Liamone 	Ajaccio 	1793 	1811
 * Jemmapes 	Mons 	1793 	1814
 * Dyle 	Bruxelles 	1795 	1814
 * Deux-Nèthes 	Anvers 	1795 	1814
 * Escaut 	Gand 	1795 	1814
 * Forêts 	Luxembourg 	1795 	1814
 * Lys 	Bruges 	1795 	1814
 * Ourthe 	Liège 	1795 	1814
 * Meuse-Inférieure 	Maastricht 	1795 	1814
 * Sambre-et-Meuse 	Namur 	1795 	1814
 * Corcyre 	Corfou 	1797 	1802
 * Ithaque 	Argostoli 	1797 	1802
 * Mer-Égée 	Zante 	1797 	1802
 * Mont-Tonnerre 	Mayence 	1798 	1814
 * Rhin-et-Moselle 	Coblence 	1798 	1814
 * Roer 	Aix-la-Chapelle 	1798 	1814
 * Sarre 	Trèves 	1798 	1814
 * Léman 	Genève 	1798 	1815
 * Doire 	Ivrée 	1802 	1814
 * Marengo 	Alexandrie 	1802 	1814
 * Pô 	Turin 	1802 	1814
 * Sésia 	Verceil 	1802 	1814
 * Stura 	Coni 	1802 	1814
 * Tanaro 	Asti 	1802 	1805
 * Apennins 	Chiavari 	1805 	1814
 * Gênes 	Gênes 	1805 	1814
 * Montenotte 	Savone 	1805 	1814
 * Arno 	Florence 	1808 	1814
 * Méditerranée 	Livourne 	1808 	1814
 * Ombrone 	Sienne 	1808 	1814
 * Taro 	Parme 	1808 	1814
 * Rome 	Rome 	1809 	1814
 * Trasimène 	Spolète 	1809 	1814
 * Bouches-du-Rhin 	Bois-le-Duc 	1810 	1814
 * Bouches-de-l'Escaut 	Middelbourg 	1810 	1814
 * Simplon 	Sion 	1810 	1814
 * Corse 	Ajaccio 	1811 	1976
 * Bouches-de-la-Meuse 	La Haye 	1811 	1814
 * Bouches-de-l'Yssel 	Zwolle 	1811 	1814
 * Ems-Occidental 	Groningue 	1811 	1814
 * Ems-Oriental 	Aurich 	1811 	1814
 * Frise 	Leeuwarden 	1811 	1814
 * Yssel-Supérieur 	Arnhem 	1811 	1814
 * Zuyderzée 	Amsterdam 	1811 	1814
 * Bouches-de-l'Elbe 	Hambourg 	1811 	1814
 * Bouches-du-Weser 	Brême 	1811 	1814
 * Ems-Supérieur 	Osnabrück 	1811 	1814
 * Lippe 	Munster 	1811 	1814
 * Bouches-de-l'Èbre 	Lérida 	1812 	1813
 * Montserrat 	Barcelone 	1812 	1813
 * Sègre 	Puigcerda 	1812 	1813
 * Ter 	Gérone 	1812 	1813
 * Bouches-de-l'Èbre-Montserrat 	Barcelone 	1813 	1814
 * Sègre-Ter 	Gérone 	1813 	1814
 * Alger 	Alger 	1848 	1962
 * Constantine 	Constantine 	1848 	1962
 * Oran 	Oran 	1848 	1962
 * Bône 	Bône 	1955 	1962
 * Batna 	Batna 	1957 	1962
 * Médéa 	Médéa 	1957 	1962
 * Mostaganem 	Mostaganem 	1957 	1962
 * Oasis 	Ouargla 	1957 	1962
 * Orléansville 	Orléansville 	1957 	1962
 * Saoura 	Colomb-Béchar 	1957 	1962
 * Sétif 	Sétif 	1957 	1962
 * Tiaret 	Tiaret 	1957 	1962
 * Tizi-Ouzou 	Tizi Ouzou 	1957 	1962
 * Tlemcen 	Tlemcen 	1957 	1962
 * Aumale 	Aumale 	1958 	1959
 * Bougie 	Bougie 	1958 	1959
 * Saïda 	Saïda 	1958 	1962
 * Saint-Pierre-et-Miquelon 	Saint-Pierre 	1976 	1985  *)

let [@warning "-42"] france_subregion =
  [|FR_Ain,[|"Ain";"01"|]
   ;FR_Aisne,[|"Aisne";"02"|]
   ;FR_Allier,[|"Allier";"03"|]
   ;FR_Alpes_de_Haute_Provence,[|"Alpes-de-Haute-Provence";"04"|]
   ;FR_Hautes_alpes,[|"Hautes-alpes";"05"|]
   ;FR_Alpes_maritimes,[|"Alpes-maritimes";"06"|]
   ;FR_Ardeche,[|"Ardèche";"07"|]
   ;FR_Ardennes,[|"Ardennes";"08"|]
   ;FR_Ariege,[|"Ariège";"09"|]
   ;FR_Aube,[|"Aube";"10"|]
   ;FR_Aude,[|"Aude";"11"|]
   ;FR_Aveyron,[|"Aveyron";"12"|]
   ;FR_Bouches_du_Rhone,[|"Bouches-du-Rhône";"13"|]
   ;FR_Calvados,[|"Calvados";"14"|]
   ;FR_Cantal,[|"Cantal";"15"|]
   ;FR_Charente,[|"Charente";"16"|]
   ;FR_Charente_maritime,[|"Charente-maritime";"17"|]
   ;FR_Cher,[|"Cher";"18"|]
   ;FR_Correze,[|"Corrèze";"19"|]
   ;FR_Corse,[|"Corse";"20";"2a";"Corse-du-sud";"Liamone";"2b";"Haute-Corse";"Golo"|]
   (* ;FR_Corse_du_sud,[|"2a";"Corse-du-sud";"Liamone"|]
    * ;FR_Haute_Corse,[|"2b";"Haute-Corse";"Golo"|] *)
   ;FR_Cote_d_Or,[|"Côte-d'Or";"21"|]
   ;FR_Cotes_d_Armor,[|"Côtes-d'Armor";"22"|]
   ;FR_Creuse,[|"Creuse";"23"|]
   ;FR_Dordogne,[|"Dordogne";"24"|]
   ;FR_Doubs,[|"Doubs";"25"|]
   ;FR_Drome,[|"Drôme";"26"|]
   ;FR_Eure,[|"Eure";"27"|]
   ;FR_Eure_et_loir,[|"Eure-et-loir";"28"|]
   ;FR_Finistere,[|"Finistère";"29"|]
   ;FR_Gard,[|"Gard";"30"|]
   ;FR_Haute_garonne,[|"Haute-garonne";"31"|]
   ;FR_Gers,[|"Gers";"32"|]
   ;FR_Gironde,[|"Gironde";"33"|]
   ;FR_Herault,[|"Hérault";"34"|]
   ;FR_Ille_et_vilaine,[|"Ille-et-vilaine";"35"|]
   ;FR_Indre,[|"Indre";"36"|]
   ;FR_Indre_et_loire,[|"Indre-et-loire";"37"|]
   ;FR_Isere,[|"Isère";"38"|]
   ;FR_Jura,[|"Jura";"39"|]
   ;FR_Landes,[|"Landes";"40"|]
   ;FR_Loir_et_cher,[|"Loir-et-cher";"41"|]
   ;FR_Loire,[|"Loire";"42"|]
   ;FR_Haute_loire,[|"Haute-loire";"43"|]
   ;FR_Loire_atlantique,[|"Loire-atlantique";"44"|]
   ;FR_Loiret,[|"Loiret";"45"|]
   ;FR_Lot,[|"Lot";"46"|]
   ;FR_Lot_et_garonne,[|"Lot-et-garonne";"47"|]
   ;FR_Lozere,[|"Lozère";"48"|]
   ;FR_Maine_et_loire,[|"Maine-et-loire";"49"|]
   ;FR_Manche,[|"Manche";"50"|]
   ;FR_Marne,[|"Marne";"51"|]
   ;FR_Haute_marne,[|"Haute-marne";"52"|]
   ;FR_Mayenne,[|"Mayenne";"53"|]
   ;FR_Meurthe_et_moselle,[|"Meurthe-et-moselle";"54"|]
   ;FR_Meuse,[|"Meuse";"55"|]
   ;FR_Morbihan,[|"Morbihan";"56"|]
   ;FR_Moselle,[|"Moselle";"57"|]
   ;FR_Nievre,[|"Nièvre";"58"|]
   ;FR_Nord,[|"Nord";"59"|]
   ;FR_Oise,[|"Oise";"60"|]
   ;FR_Orne,[|"Orne";"61"|]
   ;FR_Pas_de_calais,[|"Pas-de-calais";"62"|]
   ;FR_Puy_de_dome,[|"Puy-de-dôme";"63"|]
   ;FR_Pyrenees_atlantiques,[|"Pyrénées-atlantiques";"64"|]
   ;FR_Hautes_Pyrenees,[|"Hautes-Pyrénées";"65"|]
   ;FR_Pyrenees_orientales,[|"Pyrénées-orientales";"66"|]
   ;FR_Bas_rhin,[|"Bas-rhin";"67"|]
   ;FR_Haut_rhin,[|"Haut-rhin";"68"|]
   ;FR_Rhone,[|"Rhône";"69"|]
   ;FR_Haute_saone,[|"Haute-saône";"70"|]
   ;FR_Saone_et_loire,[|"Saône-et-loire";"71"|]
   ;FR_Sarthe,[|"Sarthe";"72"|]
   ;FR_Savoie,[|"Savoie";"73"|]
   ;FR_Haute_savoie,[|"Haute-savoie";"74"|]
   ;FR_Paris,[|"Paris";"75";"Paris I";"Paris II";"Paris III";"Paris IV";"Paris V";"Paris VI";"Paris VII";"Paris VIII";"Paris IX";"Paris X";"Paris XI";"Paris XII";"Paris XIII";"Paris XIV";"Paris XV";"Paris XVI";"Paris XVII";"Paris XVIII";"Paris XIX";"Paris XX"|]
   ;FR_Seine_maritime,[|"Seine-maritime";"76"|]
   ;FR_Seine_et_marne,[|"Seine-et-marne";"77"|]
   ;FR_Yvelines,[|"Yvelines";"78"|]
   ;FR_Deux_sevres,[|"Deux-sèvres";"79"|]
   ;FR_Somme,[|"Somme";"80"|]
   ;FR_Tarn,[|"Tarn";"81"|]
   ;FR_Tarn_et_garonne,[|"Tarn-et-garonne";"82"|]
   ;FR_Var,[|"Var";"83"|]
   ;FR_Vaucluse,[|"Vaucluse";"84"|]
   ;FR_Vendee,[|"Vendée";"85"|]
   ;FR_Vienne,[|"Vienne";"86"|]
   ;FR_Haute_vienne,[|"Haute-vienne";"87"|]
   ;FR_Vosges,[|"Vosges";"88"|]
   ;FR_Yonne,[|"Yonne";"89"|]
   ;FR_Territoire_de_belfort,[|"Territoire de belfort";"90"|]
   ;FR_Essonne,[|"Essonne";"91"|]
   ;FR_Hauts_de_seine,[|"Hauts-de-seine";"92"|]
   ;FR_Seine_Saint_Denis,[|"Seine-Saint-Denis";"93"|]
   ;FR_Val_de_marne,[|"Val-de-marne";"94"|]
   ;FR_Val_d_oise,[|"Val-d'oise";"95"|]
   ;FR_Guadeloupe,[|"Guadeloupe";"971"|]
   ;FR_Martinique,[|"Martinique";"972"|]
   ;FR_Guyane,[|"Guyane";"973"|]
   ;FR_La_reunion,[|"La réunion";"974"|]
   ;FR_Mayotte,[|"Mayotte";"975"|]
  |]

let france_subregion_ht = make_ht france_subregion

let germany_region =
  [|DE_Baden_Wurttemberg, [|"Bade-Wurtemberg";"Baden-Württemberg"|]
   ;DE_Niedersachsen,[|"Basse-Saxe";"Niedersachsen"|]
   ;DE_Bayern,[|"Bavière";"Bayern"|]
   ;DE_Berlin,[|"Berlin"|]
   ;DE_Brandenburg,[|"Brandebourg";"Brandenburg"|]
   ;DE_Bremen,[|"Brême";"Bremen"|]
   ;DE_Hamburg,[|"Hambourg";"Hamburg"|]
   ;DE_Hessen,[|"Hesse";"Hessen"|]
   ;DE_Mecklenburg_Vorpommern,[|"Mecklembourg-Poméranie-Occidentale";"Mecklenburg-Vorpommern"|]
   ;DE_Nordrhein_Westfalen,[|"Rhénanie-du-Nord-Westphalie";"Nordrhein-Westfalen"|]
   ;DE_Rheinland_Pfalz,[|"Rhénanie-Palatinat";"Rheinland-Pfalz"|]
   ;DE_Saarland,[|"Sarre";"Saarland"|]
   ;DE_Sachsen_Anhalt,[|"Saxe-Anhalt";"Sachsen-Anhalt"|]
   ;DE_Sachsen,[|"Saxe";"Sachsen"|]
   ;DE_Schleswig_Holstein,[|"Schleswig-Holstein"|]
   ;DE_Thuringen,[|"Thuringe";"Thüringen"|]
  |]

let germany_region_ht = make_ht germany_region

let find ht k = Hashtbl.find ht (key k)

let array_assoc_aux test return not_found array =
  let rec loop i =
    if i = Array.length array
    then not_found ()
    else
      let x = Array.unsafe_get array i in
      if test x then return x
      else loop (i + 1)
  in
  loop 0

let array_assoc key array =
  array_assoc_aux (fun (k, _) -> k = key) (fun (_, v) -> v) (fun () -> raise Not_found) array

let [@warning "-42"] string_of_region d = match d with

  | FR_Auvergne_Rhone_Alpes
  | FR_Bourgogne_Franche_Comte
  | FR_Bretagne
  | FR_Centre_Val_de_Loire
  | FR_Corse
  | FR_Grand_Est
  | FR_Hauts_de_France
  | FR_Ile_de_France
  | FR_Normandie
  | FR_Nouvelle_Aquitaine
  | FR_Occitanie
  | FR_Pays_de_la_Loire
  | FR_Provence_Alpes_Cote_d_Azur
  | FR_Guadeloupe
  | FR_Martinique
  | FR_Guyane
  | FR_La_reunion
  | FR_Mayotte
    -> Array.get (array_assoc d france_region) 0

  | DE_Baden_Wurttemberg
  | DE_Niedersachsen
  | DE_Bayern
  | DE_Berlin
  | DE_Brandenburg
  | DE_Bremen
  | DE_Hamburg
  | DE_Hessen
  | DE_Mecklenburg_Vorpommern
  | DE_Nordrhein_Westfalen
  | DE_Rheinland_Pfalz
  | DE_Saarland
  | DE_Sachsen_Anhalt
  | DE_Sachsen
  | DE_Schleswig_Holstein
  | DE_Thuringen
    -> Array.get (array_assoc d germany_region) 0

  | AU_South_Australia
  | AU_Western_Australia
  | AU_New_South_Wales
  | AU_Queensland
  | AU_Tasmania
  | AU_Victoria
    -> Array.get (array_assoc d australia_region) 0

  | AT_Niederosterreich
  | AT_Burgenland
  | AT_Karnten
  | AT_Oberosterreich
  | AT_Salzburg
  | AT_Steiermark
  | AT_Tirol
  | AT_Wien
  | AT_Vorarlberg
    -> Array.get (array_assoc d austria_region) 0

  | BE_Bruxelles_Capitale
  | BE_Flandre
  | BE_Wallonie
    -> Array.get (array_assoc d belgium_region) 0

  | US_Alabama
  | US_Alaska
  | US_Arizona
  | US_Arkansas
  | US_California
  | US_North_Carolina
  | US_South_Carolina
  | US_Colorado
  | US_Connecticut
  | US_North_Dakota
  | US_South_Dakota
  | US_Delaware
  | US_Florida
  | US_Georgia
  | US_Hawaii
  | US_Idaho
  | US_Illinois
  | US_Indiana
  | US_Iowa
  | US_Kansas
  | US_Kentucky
  | US_Louisiana
  | US_Maine
  | US_Maryland
  | US_Massachusetts
  | US_Michigan
  | US_Minnesota
  | US_Mississippi
  | US_Missouri
  | US_Montana
  | US_Nebraska
  | US_Nevada
  | US_New_Hampshire
  | US_New_Jersey
  | US_New_Mexico
  | US_New_York
  | US_Ohio
  | US_Oklahoma
  | US_Oregon
  | US_Pennsylvania
  | US_Rhode_Island
  | US_Tennessee
  | US_Texas
  | US_Utah
  | US_Vermont
  | US_Virginia
  | US_West_Virginia
  | US_Washington
  | US_Wisconsin
  | US_Wyoming
    -> Array.get (array_assoc d united_states_region) 0

  | ZA_Eastern_Cape
  | ZA_Free_State
  | ZA_Gauteng
  | ZA_KwaZulu_Natal
  | ZA_Limpopo
  | ZA_Mpumalanga
  | ZA_North_West
  | ZA_Northern_Cape
  | ZA_Western_Cape
    -> Array.get (array_assoc d south_africa_region) 0

  | CA_Alberta
  | CA_British_Columbia
  | CA_Prince_Edward_Island
  | CA_Manitoba
  | CA_New_Brunswick
  | CA_Nova_Scotia
  | CA_Ontario
  | CA_Quebec
  | CA_Saskatchewan
  | CA_Newfoundland_and_Labrador
  | CA_Nunavut
  | CA_Northwest_Territories
  | CA_Yukon
    -> Array.get (array_assoc d canada_region) 0

  | DZ_Adrar
  | DZ_Chlef
  | DZ_Laghouat
  | DZ_Oum_El_Bouaghi
  | DZ_Batna
  | DZ_Beaia
  | DZ_Biskra
  | DZ_Bechar
  | DZ_Blida
  | DZ_Bouira
  | DZ_Tamanrasset
  | DZ_Tebessa
  | DZ_Tlemcen
  | DZ_Tiaret
  | DZ_Tizi_Ouzou
  | DZ_Alger
  | DZ_Djelfa
  | DZ_Jijel
  | DZ_Setif
  | DZ_Saida
  | DZ_Skikda
  | DZ_Sidi_Bel_Abbes
  | DZ_Annaba
  | DZ_Guelma
  | DZ_Constantine
  | DZ_Medea
  | DZ_Mostaganem
  | DZ_M_Sila
  | DZ_Mascara
  | DZ_Ouargla
  | DZ_Oran
  | DZ_El_Bayadh
  | DZ_Illizi
  | DZ_Bordj_Bou_Arreridj
  | DZ_Boumerdes
  | DZ_El_Tarf
  | DZ_Tindouf
  | DZ_Tissemsilt
  | DZ_El_Oued
  | DZ_Khenchela
  | DZ_Souk_Ahras
  | DZ_Tipaza
  | DZ_Mila
  | DZ_Ain_Defla
  | DZ_Naaa
  | DZ_Ai_Teouchent
  | DZ_Ghardai
  | DZ_Relizane
    -> Array.get (array_assoc d algeria_region) 0

let string_of_subregion d = match d with

  | FR_Ain
  | FR_Aisne
  | FR_Allier
  | FR_Alpes_de_Haute_Provence
  | FR_Hautes_alpes
  | FR_Alpes_maritimes
  | FR_Ardeche
  | FR_Ardennes
  | FR_Ariege
  | FR_Aube
  | FR_Aude
  | FR_Aveyron
  | FR_Bouches_du_Rhone
  | FR_Calvados
  | FR_Cantal
  | FR_Charente
  | FR_Charente_maritime
  | FR_Cher
  | FR_Correze
  | FR_Corse
  (* | FR_Corse_du_sud
   * | FR_Haute_Corse *)
  | FR_Cote_d_Or
  | FR_Cotes_d_Armor
  | FR_Creuse
  | FR_Dordogne
  | FR_Doubs
  | FR_Drome
  | FR_Eure
  | FR_Eure_et_loir
  | FR_Finistere
  | FR_Gard
  | FR_Haute_garonne
  | FR_Gers
  | FR_Gironde
  | FR_Herault
  | FR_Ille_et_vilaine
  | FR_Indre
  | FR_Indre_et_loire
  | FR_Isere
  | FR_Jura
  | FR_Landes
  | FR_Loir_et_cher
  | FR_Loire
  | FR_Haute_loire
  | FR_Loire_atlantique
  | FR_Loiret
  | FR_Lot
  | FR_Lot_et_garonne
  | FR_Lozere
  | FR_Maine_et_loire
  | FR_Manche
  | FR_Marne
  | FR_Haute_marne
  | FR_Mayenne
  | FR_Meurthe_et_moselle
  | FR_Meuse
  | FR_Morbihan
  | FR_Moselle
  | FR_Nievre
  | FR_Nord
  | FR_Oise
  | FR_Orne
  | FR_Pas_de_calais
  | FR_Puy_de_dome
  | FR_Pyrenees_atlantiques
  | FR_Hautes_Pyrenees
  | FR_Pyrenees_orientales
  | FR_Bas_rhin
  | FR_Haut_rhin
  | FR_Rhone
  | FR_Haute_saone
  | FR_Saone_et_loire
  | FR_Sarthe
  | FR_Savoie
  | FR_Haute_savoie
  | FR_Paris
  | FR_Seine_maritime
  | FR_Seine_et_marne
  | FR_Yvelines
  | FR_Deux_sevres
  | FR_Somme
  | FR_Tarn
  | FR_Tarn_et_garonne
  | FR_Var
  | FR_Vaucluse
  | FR_Vendee
  | FR_Vienne
  | FR_Haute_vienne
  | FR_Vosges
  | FR_Yonne
  | FR_Territoire_de_belfort
  | FR_Essonne
  | FR_Hauts_de_seine
  | FR_Seine_Saint_Denis
  | FR_Val_de_marne
  | FR_Val_d_oise
  | FR_Guadeloupe
  | FR_Martinique
  | FR_Guyane
  | FR_La_reunion
  | FR_Mayotte
    -> Array.get (array_assoc d france_subregion) 0

  | BE_Anvers
  | BE_Bruxelles_Capitale
  | BE_Limbourg
  | BE_Flandre_Orientale
  | BE_Brabant_flamand
  | BE_Flandre_Occidentale
  | BE_Brabant_wallon
  | BE_Hainaut
  | BE_Liege
  | BE_Luxembourg
  | BE_Namur
    -> Array.get (array_assoc d belgium_subregion) 0

let string_of_country x = Array.get (array_assoc x country_list) 0

let region country s =
  match find country_ht country with
  | Australia -> find australia_region_ht s
  | Austria -> find austria_region_ht s
  | Belgium -> find belgium_region_ht s
  | Canada -> find canada_region_ht s
  | France -> find france_region_ht s
  | Germany -> find germany_region_ht s
  | South_Africa -> find south_africa_region_ht s
  | United_States -> find united_states_region_ht s
  | Algeria -> find algeria_region_ht s
  | _ -> raise Not_found

let string_exists string filter =
  let len = String.length string in
  let rec loop i =
    if i = len then false else filter (String.unsafe_get string i) || loop (i + 1)
  in loop 0

let string_forall string filter =
  let len = String.length string in
  let rec loop i =
    if i = len then true else filter (String.unsafe_get string i) && loop (i + 1)
  in loop 0

let is_num = function '0'..'9' -> true | _ -> false
let is_alphanum = function '0'..'9'|'A'..'Z'|'a'..'z'|'('|')' -> true | _ -> false

let subregion country s =
  match find country_ht country with
  | France ->
    begin
      if String.length s < 2 then raise Not_found ;
      if (String.unsafe_get s 0 = '2'
          && match String.unsafe_get s 1 with 'A'|'B'|'0'..'9' -> false | _ -> true)
      || string_forall s is_num
      then
        try Hashtbl.find france_subregion_ht (String.sub s 0 2)
        with Not_found when String.length s > 2 ->
           Hashtbl.find france_subregion_ht (String.sub s 0 3)
      else find france_subregion_ht s
    end
  | Belgium ->
    if String.length s = 4 && string_forall s is_num
    then match String.unsafe_get s 0 with
      | '1' -> begin match String.unsafe_get s 1 with
          | '0'..'2' -> BE_Bruxelles_Capitale
          | '3' | '4' -> BE_Brabant_wallon
          | '5'..'9' -> BE_Brabant_flamand
          | _ -> raise Not_found
        end
      | '2' -> BE_Anvers
      | '3' -> begin match String.unsafe_get s 1 with
          | '0'..'4' -> BE_Brabant_flamand
          | '5'..'9' -> BE_Limbourg
          | _ -> raise Not_found
        end
      | '4' -> BE_Liege
      | '5' -> BE_Namur
      | '6' -> begin match String.unsafe_get s 1 with
          | '0'..'5' -> BE_Hainaut
          | '6'..'9' -> BE_Luxembourg
          | _ -> raise Not_found
        end
      | '7' -> BE_Hainaut
      | '8' -> BE_Flandre_Occidentale
      | '9' -> BE_Flandre_Orientale
      | _ -> raise Not_found
    else find belgium_subregion_ht s
  (* | Germany -> find germany_region_ht s
   * | Australia -> find australia_region_ht s
   * | Austria -> find austria_region_ht s
   * | United_States -> find united_states_region_ht s
   * | South_Africa -> find south_africa_region_ht s *)
  | _ -> raise Not_found


let str_debug = "Rochefort/Mer, 17, Charentes Maritimes, FRANCE"

let finalize p =
  let debug = p.place_raw = str_debug in
  if key p.place_country = "france" then begin
    let () = if debug then print_endline __LOC__ in
    let p =
      if p.place_region = "" then
        let x = key p.place_subregion in
        let () = if debug then print_endline @@ Printf.sprintf "%s: %s" __LOC__ x in

        if x = key "Ain"
        || x = key "Allier"
        || x = key "Ardèche"
        || x = key "Cantal"
        || x = key "Drôme"
        || x = key "Isère"
        || x = key "Loire"
        || x = key "Haute-loire"
        || x = key "Puy-de-dôme"
        || x = key "Rhône"
        || x = key "Savoie"
        || x = key "Haute-savoie"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Auvergne_Rhone_Alpes }

        else if x = key "Côte-d'Or"
             || x = key "Doubs"
             || x = key "Jura"
             || x = key "Nièvre"
             || x = key "Haute-saône"
             || x = key "Saône-et-loire"
             || x = key "Yonne"
             || x = key "Territoire de belfort"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Bourgogne_Franche_Comte }

        else if x = key "Côtes-d'Armor"
             || x = key "Finistère"
             || x = key "Ille-et-vilaine"
             || x = key "Morbihan"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Bretagne }

        else if x = key "Cher"
             || x = key "Eure-et-loir"
             || x = key "Indre"
             || x = key "Indre-et-loire"
             || x = key "Loir-et-cher"
             || x = key "Loiret"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Centre_Val_de_Loire }

        else if x = key "Corse" then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Corse
                 ; place_subregion = "" } [@warning "-42"]

        else if x = key "Ardennes"
        || x = key "Aube"
        || x = key "Marne"
        || x = key "Haute-marne"
        || x = key "Meurthe-et-moselle"
        || x = key "Meuse"
        || x = key "Moselle"
        || x = key "Bas-rhin"
        || x = key "Haut-rhin"
        || x = key "Vosges"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Grand_Est }

        else if x = key "Aisne"
        || x = key "Nord"
        || x = key "Oise"
        || x = key "Pas-de-calais"
        || x = key "Somme"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Hauts_de_France }

        else if x = key "Paris"
        || x = key "Seine-et-marne"
        || x = key "Yvelines"
        || x = key "Essonne"
        || x = key "Hauts-de-seine"
        || x = key "Seine-Saint-Denis"
        || x = key "Val-de-marne"
        || x = key "Val-d'oise"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Ile_de_France }

        else if x = key "Calvados"
        || x = key "Eure"
        || x = key "Manche"
        || x = key "Orne"
        || x = key "Seine-maritime"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Normandie }

        else if x = key "Charente"
        || x = key "Charente-maritime"
        || x = key "Corrèze"
        || x = key "Creuse"
        || x = key "Dordogne"
        || x = key "Gironde"
        || x = key "Landes"
        || x = key "Lot-et-garonne"
        || x = key "Pyrénées-atlantiques"
        || x = key "Deux-sèvres"
        || x = key "Vienne"
        || x = key "Haute-vienne"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Nouvelle_Aquitaine }

        else if x = key "Ariège"
             || x = key "Aude"
             || x = key "Aveyron"
             || x = key "Gard"
             || x = key "Haute-garonne"
             || x = key "Gers"
             || x = key "Hérault"
             || x = key "Lot"
             || x = key "Lozère"
             || x = key "Hautes-Pyrénées"
             || x = key "Pyrénées-orientales"
             || x = key "Tarn"
             || x = key "Tarn-et-garonne"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Occitanie }

        else if x = key "Loire-atlantique"
             || x = key "Maine-et-loire"
             || x = key "Mayenne"
             || x = key "Sarthe"
             || x = key "Vendée"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Pays_de_la_Loire }

        else if x = key "Alpes-de-Haute-Provence"
             || x = key "Hautes-alpes"
             || x = key "Alpes-maritimes"
             || x = key "Bouches-du-Rhône"
             || x = key "Var"
             || x = key "Vaucluse"
        then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Provence_Alpes_Cote_d_Azur }

        else if x = key "Guadeloupe" then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Guadeloupe
                 ; place_subregion = "" } [@warning "-42"]

        else if x = key "Martinique" then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Martinique
                 ; place_subregion = "" } [@warning "-42"]

        else if x = key "Guyane" then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Guyane
                 ; place_subregion = "" } [@warning "-42"]

        else if x = key "La réunion" then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_La_reunion
                 ; place_subregion = "" } [@warning "-42"]

        else if x = key "Mayotte" then
          let () = if debug then print_endline __LOC__ in
          { p with place_region = string_of_region FR_Mayotte
                 ; place_subregion = "" } [@warning "-42"]

        else p
      else p
    in
    let p =
      if key p.place_subregion = key "Paris" then { p with place_city = "Paris" }
      else p
    in
    p
  end
  else if key p.place_country = "belgique" then begin
    let p =
      if p.place_region = "" then begin
        let x = key p.place_subregion in
        if x = key "Brabant wallon"
        || x = key "Hainaut"
        || x = key "Liège"
        || x = key "Luxembourg"
        || x = key "Namur"
        then { p with place_region = string_of_region BE_Wallonie }
        else if x = key "Anvers"
             || x = key "Brabant flamand"
             || x = key "Flandre-Occidentale"
             || x = key "Flandre-Orientale"
             || x = key "Limbourg"
        then { p with place_region = string_of_region BE_Flandre }
        else if x = key "Bruxelles-Capitale"
        then { p with place_region = string_of_region BE_Bruxelles_Capitale } [@warning "-42"]
        else p
      end
      else p
    in
    let p =
      if key p.place_subregion = key "Bruxelles-Capitale"
      then { p with place_city = "Bruxelles" }
      else p
    in
    p
  end
  else p

let aux_assert array =
  let ht = Hashtbl.create 2048 in
  Array.iter begin fun a ->
    Array.iter begin fun (_, vs) ->
      List.iter begin fun v ->
        let h = Hashtbl.hash @@ key v in
        if Hashtbl.mem ht h
        then begin
          print_string @@ Printf.sprintf "%s: %d: \"%s\"" __LOC__ h v ;
          List.iter (fun v -> print_string @@ Printf.sprintf " \"%s\"" v) (Hashtbl.find_all ht h) ;
          print_newline ()
        end ;
        Hashtbl.add ht h v
      end (List.sort_uniq compare @@ Array.to_list vs)
    end a
  end array

let () =
  aux_assert
    [|australia_region
     ;austria_region
     ;belgium_region
     ;canada_region
     ;france_region
     ;germany_region
     ;south_africa_region
     ;united_states_region
     ;algeria_region
    |]

let () =
  aux_assert
    [|france_subregion
     ;belgium_subregion
    |]

let empty_place place_raw =
  { Def.place_street = ""
  ; place_city = ""
  ; place_subregion = ""
  ; place_region = ""
  ; place_country = ""
  ; place_lieu_dit = ""
  ; place_raw
  }

let normalize =
  (* petit hack en attendant une vraie gestion des lieux transforme
     "[foo-bar] - boobar (baz)" en "foo-bar, boobar (baz)" *)
  let r = Str.regexp "^\\[\\([^]]+\\)\\] *- *\\(.*\\)" in
  fun s -> Str.global_replace r "\\1, \\2" s

(* TODO: start with num + alphanumspace uniquement: street *)
let split_place str =
  let debug = str = str_debug in
  let len = String.length str in
  let pick i j =
    if debug then print_endline @@ Printf.sprintf "%s: String.sub \"%s\" %d %d %d" __LOC__ str i j (String.length str) ;
    try
      if i <= j - 1
      then begin
        if debug then print_endline @@ Printf.sprintf "%s = \"%s\"" __LOC__ (String.trim @@ String.sub str i (j - i + 1)) ;
        String.trim @@ String.sub str i (j - i + 1)
      end
      else ""
    with e -> if debug then print_endline @@ Printf.sprintf "%s: String.sub \"%s\" %d %d %d" __LOC__ str i j (String.length str) ; raise e
  in
  let add i j comment acc =
    match pick i j with "" -> if comment = "" then acc else (comment, "") :: acc | x -> (x, comment) :: acc
  in
  let rec loop comment acc i j =
    if i < 0 then add 0 j comment acc
    else match String.unsafe_get str i with
      | ']' when let rec loop k =
                   if k >= j then false
                   else match String.unsafe_get str k with
                     | ' ' -> loop (k + 1)
                     | '-' -> true
                     | _ -> false
          in loop (i + 1) ->
        begin match String.rindex_from_opt str (i - 1) '[' with
          | Some k -> loop "" (add k j comment acc) (k - 1) (k - 1)
          | None -> loop comment acc (i - 1) j
        end
      | ')' | ']' as c ->
        begin match String.rindex_from_opt str (i - 1) (if c = ')' then '(' else '[') with
          | Some i' ->
            loop (pick (i' + 1) (i - 1)) (add (i + 1) j comment acc) (i' - 1) (i' - 1)
          | None -> loop comment acc (i - 1) j
        end
      | ',' | ';' -> loop "" (add (i + 1) j comment acc) (i - 1) (i - 1)
      | _ -> loop comment acc (i - 1) j
  in
  loop "" [] (len - 1) (len - 1)

let guess_place conf str =
  let list = split_place str in
  let debug = str = str_debug in
  if debug then List.iter (fun (s, c) -> print_endline @@ Printf.sprintf "%s: %s (%s)" __LOC__ s c) list ;
  let place_country, list =
    match List.rev list with
    | [] -> "", []
    | (main, comment) :: tl ->
      try string_of_country @@ find country_ht main, List.rev tl
      with Not_found ->
        if comment = ""
        then Opt.map_default "France" key (p_getenv conf.base_env "place_default_country"), list
        else
          try string_of_country @@ find country_ht comment, List.rev @@ (main, "") :: tl
          with Not_found -> Opt.map_default "France" key (p_getenv conf.base_env "place_default_country"), list
  in
  let p = { (empty_place str) with Def.place_country } in
  let rec loop i p = function
    | [] ->
      if debug then print_endline @@ Printf.sprintf "%s: \"%s\" \"%s\" \"%s\" \"%s\"" __LOC__ p.place_street p.place_city p.place_subregion p.place_region ;
      if p.place_street <> ""
      && string_forall p.place_street is_num
      && p.place_city = "" && p.place_region = ""
      then
        let () =       if debug then print_endline @@ Printf.sprintf "%s: %s" __LOC__ p.place_city ;
        in
        try finalize
              { p with place_street = ""
                     ; place_subregion = string_of_subregion @@ subregion p.place_country p.place_street }
        with Not_found -> finalize p
      else finalize p
    | (main, comment) :: tl ->
      if debug then print_endline @@ Printf.sprintf "%s: \"%s\" \"%s\"" __LOC__ main comment ;
      loop (i + 1)
      begin
        let place_city p s =
          let place_lieu_dit, place_city  =
            if String.unsafe_get s 0 = '['
            then match String.index_opt s ']' with
              | Some i ->
                let rec loop j =
                  if j = String.length s then "", s
                  else match String.unsafe_get s j with
                    | ' ' | '-' -> loop (j + 1)
                    | _ -> String.sub s 1 (i - 1), String.sub s j (String.length s - j)
                in loop (i + 1)
              | None -> "", s
            else "", s
          in
          { p with place_city ; place_lieu_dit }
        in
        if i = 0 && string_exists main is_num && string_forall main is_alphanum
        then { p with place_street = main ^ if comment = "" then "" else "(" ^ comment ^ ")" }
        else
          let aux s =
            try `Subregion (string_of_subregion @@ subregion p.place_country s)
            with Not_found ->
            try `Region (string_of_region @@ region p.place_country s)
            with Not_found -> `None s
          in
          match aux main, aux comment with

          | (`Region a, `Region b) ->
            assert (a = b) ;
            if p.place_region = "" then
              let () = if debug then print_endline __LOC__ in { p with place_region = a }
            else if key p.place_region <> key a then
              let () = if debug then print_endline __LOC__ in { p with place_region = "" } (* FIXME *)
            else
              let () = if debug then print_endline __LOC__ in p

          | (`Subregion b, `Subregion b') ->
            if key b <> key b'
            then print_endline @@ Printf.sprintf "%s: %s <> %s (%s)" __LOC__ b b' str ;
            if p.place_subregion <> "" && key p.place_subregion <> key b
            then print_endline @@ Printf.sprintf "%s: %s <> %s (%s)" __LOC__ p.place_subregion b str ;
            { p with place_subregion = b }

          | (`None s, `Subregion b) | (`Subregion b, `None s) ->
            if debug then print_endline @@ Printf.sprintf "%s: %s // %s" __LOC__ s b ;
            if s = "" then (if p.place_subregion <> "" && key p.place_subregion <> key b
                            then print_endline @@ Printf.sprintf "%s: %s <> %s (%s)" __LOC__ p.place_subregion b str ;
                            { p with place_subregion = b } )
            else if p.place_city = "" then { (place_city p s) with place_subregion = b }
            else p (* !!! *)

          | (`Region a, `Subregion b) | (`Subregion b, `Region a) ->
            if debug then print_endline @@ Printf.sprintf "%s: %s // %s" __LOC__ a b ;
            { p with place_region = a ; place_subregion = b }

          | (`Region a, `None s) | (`None s, `Region a) ->
            if debug then print_endline @@ Printf.sprintf "%s: %s // %s" __LOC__ a s ;
            if p.place_city = "" && s <> "" then { (place_city p s) with place_region = a }
            else if s = "" then { p with place_region = a }
            else p (* !!! *)

          | (`None s1, `None s2) ->
            if debug then print_endline @@ Printf.sprintf "%s: %s // %s" __LOC__ s1 s2 ;
            if string_exists p.place_street is_num && string_forall p.place_street is_alphanum then
              let () = if debug then print_endline __LOC__ in
              { p with place_street = p.place_street ^ main }
            else if p.place_street = ""
                 && is_num (String.unsafe_get main 0)
                 && match String.index_opt main ' ' with Some i -> String.index_from_opt main (i + 1) ' ' <> None | _ -> false
            then
              let () = if debug then print_endline __LOC__ in
              { p with place_street = main }
            else if p.place_city = "" then let () = if debug then print_endline __LOC__ in place_city p (s1 ^ s2) (* !!! *)
            else if p.place_street = "" && p.place_region = ""
            then let () = if debug then print_endline __LOC__ in { p with place_street = p.place_city ; place_city = main }
            else let () = if debug then print_endline __LOC__ in p
      end
      tl
  in loop 0 p list

let place_of_string conf place =
  guess_place conf place
