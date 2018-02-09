(* camlp5r ../../src/pa_lock.cmo *)
(* $Id: public2.ml,v 4.1 2008/03/31 11:34:34 deraugla Exp $ *)

open Printf;

open Def;
open Gwdb;

value year_of p =
  match
    (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
     get_death p, CheckItem.date_of_death (get_death p))
  with
  [ (_, _, NotDead, _) -> None
  | (Some (Dgreg d _), _, _, _) -> Some d.year
  | (_, Some (Dgreg d _), _, _) -> Some d.year
  | (_, _, _, Some (Dgreg d _)) -> Some d.year
  | _ -> None ]
;

value find_dated_ancestor base p =
  let mark = Array.make (nb_of_persons base) False in
  loop 1 [get_key_index p] where rec loop nb_gen iplist =
    if iplist = [] then None
    else
      let anc_list =
        List.fold_left
          (fun anc_list ip ->
             match get_parents (poi base ip) with
             [ Some ifam ->
                 let fam = foi base ifam in
                 [get_mother fam; get_father fam :: anc_list]
             | None -> anc_list ])
          [] iplist
      in
      (* Dans le cas où le nombre d'implexes est très élevé, le calcul *)
      (* peut être très long car on le refait plusieurs fois pour les  *)
      (* mêmes personnes. On rend donc la liste unique.                *)
      let anc_list = Mutil.list_uniq anc_list in
      let anc_list =
        List.filter (fun ip -> not mark.(Adef.int_of_iper ip)) anc_list
      in
      let () =
        List.iter (fun ip -> mark.(Adef.int_of_iper ip) := True) anc_list
      in
      loop_ind anc_list where rec loop_ind =
        fun
        [ [ip :: iplist] ->
            let p = poi base ip in
            match year_of p with
            [ Some year -> Some (p, year, nb_gen)
            | None -> loop_ind iplist ]
        | [] -> loop (nb_gen + 1) anc_list ]
;

value nb_years_by_gen = 30;

value change_somebody_access base lim_year trace p year_of_p spouse =
  (* Dans le cas d'un calcul par rapport au conjoint, on ignore *)
  (* le statu parce qu'il a pu être modifié précédemment.       *)
  if year_of_p = None && (get_access p = IfTitles || spouse) then
    match find_dated_ancestor base p with
    [ Some (a, year, nb_gen) -> do {
        let acc =
          if year + nb_gen * nb_years_by_gen > lim_year then Private
          else Public
        in
        let gp = {(gen_person_of_person p) with access = acc} in
        patch_person base gp.key_index gp;
        if trace then do {
          printf "%s -> " (Gutil.designation base p);
          if acc = Private then printf "private" else printf "public";
          printf " (anc %d gen %s year %d)" nb_gen
            (Gutil.designation base a) year;
          printf "\n";
          flush stdout;
        }
        else ();
        Some acc
        }
    | None -> None ]
  else None
;

value public_all bname lim_year trace patched = do {
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let n = nb_of_persons base in
  let changes = ref False in
  Consang.check_noloop base
        (fun
         [ OwnAncestor p -> do {
             printf "I cannot deal this database.\n";
             printf "%s is his own ancestors\n" (Gutil.designation base p);
             flush stdout;
             exit 2
           }
         | _ -> assert False ]);
  ProgrBar.start ();
  for i = 0 to n - 1 do {
    ProgrBar.run i n;
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    if year_of p = None && get_access p = IfTitles && (patched && is_patched_person base ip || patched == False) then do {
      match change_somebody_access base lim_year trace p (year_of p) False with
      [ Some _ -> changes.val := True
      | None ->
          let fama = get_family p in
          loop 0 where rec loop i =
            if i = Array.length fama then ()
            else
              let ifam = fama.(i) in
              let isp = Gutil.spouse ip (foi base ifam) in
              let sp = poi base isp in
              let year_of_sp = year_of sp in
              let acc_opt =
                match year_of_sp with
                [ Some year ->
                    Some (if year > lim_year then Private else Public)
                | None ->
                    change_somebody_access base lim_year trace sp year_of_sp True ]
              in
              match acc_opt with
              [ Some acc -> do {
                  let gp = {(gen_person_of_person p) with access = acc} in
                  patch_person base gp.key_index gp;
                  changes.val := True;
                  if trace then do {
                    printf "%s -> " (Gutil.designation base p);
                    if acc = Private then printf "private"
                    else printf "public";
                    printf " (inherited from spouse %s)"
                      (Gutil.designation base sp);
                    printf "\n";
                    flush stdout;
                  }
                  else ();
                }
              | None -> loop (i + 1) ] ]
    }
    else ();
  };
  if changes.val then commit_patches base else ();
  ProgrBar.finish ();
};

value lim_year = ref 1900;
value trace = ref False;
value patched = ref False;
value bname = ref "";

value speclist =
  [("-y", Arg.Int (fun i -> lim_year.val := i),
    "limit year (default = " ^ string_of_int lim_year.val ^ ")");
   ("-t", Arg.Set trace, "trace changed persons");
   ("-p", Arg.Set patched, "compute patched persons only")]
;
value anonfun i = bname.val := i;
value usage = "Usage: public [-y #] [-t] base";

value main () = do {
  Arg.parse speclist anonfun usage;
  if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
  lock (Mutil.lock_file bname.val) with
  [ Accept ->
      public_all bname.val lim_year.val trace.val patched.val
  | Refuse -> do {
      eprintf "Base is locked. Waiting... ";
      flush stderr;
      lock_wait (Mutil.lock_file bname.val) with
      [ Accept -> do {
          eprintf "Ok\n";
          flush stderr;
          public_all bname.val lim_year.val trace.val patched.val;
        }
      | Refuse -> do {
          printf "\nSorry. Impossible to lock base.\n";
          flush stdout;
          exit 2
        } ]
  } ];
};

main ();
