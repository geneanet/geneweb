(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util
open Place

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
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "place")) in
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
    Wserver.printf "%s / %s" (Utf8.capitalize (transl conf "place"))
      (Utf8.capitalize (transl_nth conf "surname/surnames" 0))
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
