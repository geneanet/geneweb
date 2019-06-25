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
    let len = nb_of_persons base in
    let aux b fn p =
      if b then let x = fn p in if not (is_empty_string x) then ht_add x p
    in
    let rec loop i =
      if i < len then begin
        let p = pget conf base (Adef.iper_of_int i) in
        if authorized_age conf base p then begin
          aux add_birth get_birth_place p ;
          aux add_baptism get_baptism_place p ;
          aux add_death get_death_place p ;
          aux add_burial get_burial_place p ;
        end ;
        loop (i + 1)
      end
    in
    loop 0 ;
  end ;
  if add_marriage then begin
    let rec loop i =
      let len = nb_of_families base in
      if i < len then begin
        let fam = foi base (Adef.ifam_of_int i) in
        if not @@ is_deleted_family fam then begin
          let pl_ma = get_marriage_place fam in
          if not (is_empty_string pl_ma) then
            let fath = pget conf base (get_father fam) in
            let moth = pget conf base (get_mother fam) in
            if authorized_age conf base fath
            && authorized_age conf base moth
            then begin
              ht_add pl_ma fath ;
              ht_add pl_ma moth
            end
        end ;
        loop (i + 1) ;
      end
    in
    loop 0 ;
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

let print_html_places_surnames conf base (array : (string list * (string * Adef.iper list) list) array) =
  let list = Array.to_list array in
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
  let print_sn_list (snl : (string * Adef.iper list) list) =
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
  loop [] list;
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
    try List.assoc "places_inverted" conf.base_env <> "yes"
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
    try List.assoc "places_inverted" conf.base_env <> "yes"
    with Not_found -> false
  in
  let array =
    get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage
      [] [] (fold_place_long inverted) filter
      (fun prev p ->
         let value = (get_surname p, get_key_index p) in
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
