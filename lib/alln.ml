(* $Id: alln.ml,v 5.24 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let default_max_cnt = 2000

(* tools *)
let ini len k =
  let ini_k = Util.str_sub ~pad:'_' k 0 len in
  (* ini_k is "a fresh string": we can use unsafe. *)
  Util.str_replace ~unsafe:true ' ' ~by:'_' ini_k

let particle_at_the_end base is_surnames s =
  if is_surnames then
    surname_without_particle base s ^ surname_particle base s
  else s

let compare_particle_at_the_end base is_surnames a b =
  Gutil.alphabetic_order
    (particle_at_the_end base is_surnames a)
    (particle_at_the_end base is_surnames b)

let groupby_ini len list =
  list
  |> Util.groupby
    ~key:(fun (k, _, _) -> ini len k)
    ~value:(fun (_, s, c) -> (s, c))
  |> List.sort (fun (a, _) (b, _) -> Gutil.alphabetic_order a b)

let groupby_count list =
  list
  |> Util.groupby
    ~key:(fun (_, _, c) -> c)
    ~value:(fun (_, s, _) -> s)
  |> List.sort (fun (a, _) (b, _) -> compare b a)

(* print *)

let print_title conf base is_surnames ini len =
  if len >= 2 then
    if is_surnames then
      Wserver.printf (fcapitale (ftransl conf "the %d surnames")) len
    else Wserver.printf (fcapitale (ftransl conf "the %d first names")) len
  else if is_surnames then
    Wserver.printf "%s" (capitale (transl_nth conf "surname/surnames" 0))
  else
    Wserver.printf "%s"
      (capitale (transl_nth conf "first name/first names" 0));
  if ini <> "" then
    Wserver.printf " %s %s" (transl conf "starting with") ini
  else
    Wserver.printf " (%d %s)" (Util.real_nb_of_persons conf base)
      (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1))

let displayify s = s
  (* let rec loop i len =
   *   if i = String.length s then Buff.get len
   *   else
   *     let nbc = Name.nbc s.[i] in
   *     if nbc < 0 || i + nbc > String.length s then
   *       Buff.get (Buff.mstore len "...")
   *     else loop (i + nbc) (Buff.gstore len s i nbc)
   * in
   * loop 0 0 *)

let tr c1 s2 s =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if String.unsafe_get s i = c1 then loop (i + 1) (Buff.mstore len s2)
    else loop (i + 1) (Buff.store len (String.unsafe_get s i))
  in
  loop 0 0

let print_alphabetic_big conf base is_surnames ini list len too_big =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  Hutil.header conf title;
  Wserver.printf "<p class=\"search_name\">\n";
  List.iter
    (fun (ini_k, _) ->
       Wserver.printf "<a href=\"%sm=%s&tri=A&k=%s\">" (commd conf) mode
         (Util.code_varenv ini_k);
       Wserver.printf "%s" (tr '_' "&nbsp;" (displayify ini_k));
       Wserver.printf "</a>\n")
    list;
  Wserver.printf "</p>\n";
  if len <= default_max_cnt && not too_big then
    begin
      begin
        Wserver.printf "<p>";
        Wserver.printf "%s:" (capitale (transl conf "the whole list"));
        Wserver.printf "</p>\n"
      end;
      begin
        Wserver.printf "<ul>\n";
        begin
          Wserver.printf "<li>";
          begin
            Wserver.printf "<a href=\"%sm=%s&tri=A&o=A&k=%s\">" (commd conf)
              mode ini;
            Wserver.printf "%s" (transl conf "long display");
            Wserver.printf "</a>"
          end;
          Wserver.printf "</li>\n"
        end;
        begin
          Wserver.printf "<li>";
          begin
            Wserver.printf "<a href=\"%sm=%s&tri=S&o=A&k=%s\">" (commd conf)
              mode ini;
            Wserver.printf "%s" (transl conf "short display");
            Wserver.printf "</a>"
          end;
          Wserver.printf "</li>\n"
        end;
        begin
          Wserver.printf "<li>";
          begin
            Wserver.printf "<a href=\"%sm=%s&tri=S&o=A&k=%s&cgl=on\">"
              (commd conf) mode ini;
            Wserver.printf "%s + %s" (transl conf "short display")
              (transl conf "cancel GeneWeb links");
            Wserver.printf "</a>"
          end;
          Wserver.printf "</li>\n"
        end;
        Wserver.printf "</ul>\n"
      end
    end;
  Hutil.trailer conf

let print_alphabetic_all conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  Hutil.header conf title;
  Wserver.printf "<p class=\"search_name\">\n";
  List.iter
    (fun (ini_k, _) ->
       let ini = ini_k in
       Wserver.printf "<a href=\"#a%s\">" ini;
       Wserver.printf "%s" (Mutil.tr '_' ' ' ini);
       Wserver.printf "</a>\n")
    list;
  Wserver.printf "</p>\n";
  Wserver.printf "<ul>\n";
  List.iter
    (fun (ini_k, l) ->
       let ini = ini_k in
       Wserver.printf "<li>\n";
       Wserver.printf "<a id=\"a%s\">" ini_k;
       Wserver.printf "%s" (Mutil.tr '_' ' ' ini);
       Wserver.printf "</a>\n";
       Wserver.printf "<ul>\n";
       List.iter
         (fun (s, cnt) ->
            Wserver.printf "<li>";
            begin let href =
              "m=" ^ mode ^ "&v=" ^ code_varenv s ^ "&t=A"
            in
              wprint_geneweb_link conf href
                (particle_at_the_end base is_surnames s)
            end;
            Wserver.printf " (%d)" cnt;
            Wserver.printf "</li>\n")
         (List.sort (fun (a, _) (b, _) -> compare_particle_at_the_end base is_surnames a b) l);
       Wserver.printf "</ul>\n";
       Wserver.printf "</li>\n")
    list;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let print_alphabetic_small conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  Hutil.header conf title;
  if list = [] then ()
  else
    begin
      Wserver.printf "<ul>\n";
      List.iter
        (fun (_, s, cnt) ->
           Wserver.printf "<li>";
           Wserver.printf "<a href=\"%sm=%s&v=%s&t=A\">" (commd conf) mode
             (code_varenv s);
           Wserver.printf "%s" (particle_at_the_end base is_surnames s);
           Wserver.printf "</a>";
           Wserver.printf " (%d)" cnt;
           Wserver.printf "</li>\n")
        (List.sort (fun (_, a, _) (_, b, _) ->
             compare_particle_at_the_end base is_surnames a b) list);
      Wserver.printf "</ul>\n"
    end;
  Hutil.trailer conf

let print_frequency_any conf base is_surnames list len =
  let title _ = print_title conf base is_surnames "" len in
  let mode = if is_surnames then "N" else "P" in
  let n = ref 0 in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  List.iter
    (fun (cnt, l) ->
       if !n > default_max_cnt then ()
       else
         begin
           Wserver.printf "<li>\n";
           Wserver.printf "%d\n" cnt;
           begin
             Wserver.printf "<ul>\n";
             List.iter
               (fun s ->
                  Wserver.printf "<li>";
                  Wserver.printf "<a href=\"%sm=%s&v=%s\">" (commd conf) mode
                    (code_varenv (Name.lower s));
                  Wserver.printf "%s" (particle_at_the_end base is_surnames s);
                  Wserver.printf "</a>";
                  incr n;
                  Wserver.printf "</li>\n")
               l;
             Wserver.printf "</ul>\n"
           end;
           Wserver.printf "</li>\n"
         end)
    list;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

(* selection *)

let select_names conf base is_surnames ini need_whole_list =
  let iii =
    if is_surnames then persons_of_surname base
    else persons_of_first_name base
  in
  let get_name =
    if is_surnames then get_surname
    else get_first_name
  in
  let (list, len) =
    let start_k = Mutil.tr '_' ' ' ini in
    try
      let istr = spi_first iii start_k in
        let rec loop istr len list =
          let s = Translate.eval (Mutil.nominative (sou base istr)) in
          let k = Util.name_key base s in
          if Mutil.start_with ~wildcard:true ini 0 k then
            let (list, len) =
              if s <> "?" then
                let my_list = spi_find iii istr in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn = get_name p in
                         if eq_istr isn istr then ip :: l else l
                       else ip :: l)
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l else ip :: l)
                      [] my_list
                  else my_list
                in
                let cnt = List.length my_list in
                if cnt = 0 then list, len
                else
                  match list with
                    (k1, s1, cnt1) :: list1 ->
                      if k = k1 then (k1, s1, cnt1 + cnt) :: list1, len - 1
                      else (k, s, cnt) :: list, len
                  | [] -> [k, s, cnt], len
              else list, len
            in
            match spi_next iii istr need_whole_list with
            | (istr, dlen) -> loop istr (len + dlen) list
            | exception Not_found -> list, len
          else list, len
        in
        loop istr 0 []
    with Not_found -> [], 0
  in
  let (list, len) =
    let lim =
      match p_getint conf.env "atleast" with
        Some x -> x
      | None -> 0
    in
    List.fold_left
      (fun (list, len) (k, s, cnt) ->
         if cnt >= lim then (k, s, cnt) :: list, len else list, len - 1)
      ([], len) list
  in
  list, len

let print_frequency conf base is_surnames =
  let () = load_strings_array base in
  let (list, len) = select_names conf base is_surnames "" true in
  let list = groupby_count list in
  print_frequency_any conf base is_surnames list len

let print_alphabetic conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
      Some k -> k
    | _ -> ""
  in
  let fast =
    p_getenv conf.base_env "fast_alphabetic" = Some "yes" && ini = ""
  in
  let _ = if fast || String.length ini < 2 then load_strings_array base in
  let all =
    match p_getenv conf.env "o" with
      Some "A" -> true
    | _ -> false
  in
  let (list, len) =
    if fast then
      let rec loop list len c =
        let list = (String.make 1 c, "", 1) :: list in
        if c = 'A' then list, len
        else loop list (len + 1) (Char.chr (Char.code c - 1))
      in
      loop [] 0 'Z'
    else
      select_names conf base is_surnames ini all
  in
  if fast then
    let list = List.map (fun (s, _, _) -> s, 1) list in
    print_alphabetic_big conf base is_surnames ini list 1 true
  else if len >= 50 || ini = "" then
    let list = groupby_ini (String.length ini + 1) list in
    if all then
      if len > default_max_cnt then Hutil.incorrect_request conf
      else print_alphabetic_all conf base is_surnames ini list len
    else print_alphabetic_big conf base is_surnames ini list len false
  else print_alphabetic_small conf base is_surnames ini list len

(* short print *)

let print_alphabetic_short conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  let need_ref = len >= 250 in
  Hutil.header conf title;
  if need_ref then
    begin
      Wserver.printf "<p>\n";
      List.iter
        (fun (ini_k, _) ->
           let ini = ini_k in
           Wserver.printf "<a href=\"#a%s\">" ini;
           Wserver.printf "%s" (Mutil.tr '_' ' ' ini);
           Wserver.printf "</a>\n")
        list;
      Wserver.printf "</p>\n"
    end;
  List.iter
    (fun (ini_k, l) ->
       let ini = ini_k in
       Wserver.printf "<p>\n";
       Mutil.list_iter_first
         (fun first (s, cnt) ->
            let href =
              if not conf.cancel_links then
                " href=\"" ^ commd conf ^ "m=" ^ mode ^ "&v=" ^
                code_varenv s ^ "&t=A\""
              else ""
            in
            let name =
              if first && need_ref then " id=\"a" ^ ini ^ "\"" else ""
            in
            if not first then Wserver.printf ",\n";
            if href <> "" || name <> "" then
              Wserver.printf "<a%s%s>" href name;
            Wserver.printf "%s" (particle_at_the_end base is_surnames s);
            if href <> "" || name <> "" then Wserver.printf "</a>";
            Wserver.printf " (%d)" cnt)
         (List.sort (fun (a, _) (b, _) -> Gutil.alphabetic_order a b) l);
       Wserver.printf "\n";
       Wserver.printf "</p>\n")
    list;
  Hutil.trailer conf

let print_short conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
      Some k -> k
    | _ -> ""
  in
  let _ = if String.length ini < 2 then load_strings_array base in
  let (list, len) = select_names conf base is_surnames ini true in
  if len > default_max_cnt then Hutil.incorrect_request conf
  else
    let list = groupby_ini (String.length ini + 1) list in
    print_alphabetic_short conf base is_surnames ini list len

(* main *)

let print_surnames conf base =
  match p_getenv conf.env "tri" with
    Some "F" -> print_frequency conf base true
  | Some "S" -> print_short conf base true
  | _ -> print_alphabetic conf base true

let print_first_names conf base =
  match p_getenv conf.env "tri" with
    Some "F" -> print_frequency conf base false
  | Some "S" -> print_short conf base false
  | _ -> print_alphabetic conf base false
