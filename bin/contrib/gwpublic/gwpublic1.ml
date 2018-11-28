open Geneweb
open Def
open Gwdb

(** This script is used to set people old enough privacy to [Public]. *)
(** Set privacy of persons older than X years as [Public]. Set the
    ancestors and descendants with no dates [Public] as well (counting 3
    generations by century for descendants).
*)

(**
   If [NotDead], return [None].
   Otherwise, try to find a year in [[ birth ; baptism ; death ]]
   an return it.
*)
let oldest_year_of p =
  match get_death p with
  | NotDead -> None
  | death -> match Adef.od_of_cdate (get_birth p) with
    | Some (Dgreg (d, _)) -> Some d.year
    | _ -> match Adef.od_of_cdate (get_baptism p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | _ -> match CheckItem.date_of_death death with
        | Some (Dgreg (d, _)) -> Some d.year
        | _ -> None

(**
   If [NotDead], return [None].
   Otherwise, try to find a year in [[ death ; baptism ; birth ]]
   an return it.
*)
let most_recent_year_of p =
  match get_death p with
  | NotDead -> None
  | death -> match CheckItem.date_of_death death with
    | Some (Dgreg (d, _)) -> Some d.year
    | _ -> match Adef.od_of_cdate (get_baptism p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | _ -> match Adef.od_of_cdate (get_birth p) with
        | Some (Dgreg (d, _)) -> Some d.year
        | _ -> None

let nb_gen_by_century = 3

let changes = ref false

(** Compute the number of (descending) generation to be considered as old
    starting from [p] included. i.e. [0] means that [p] is not considered old.
*)
let compute_ndgen treshold y =
  (treshold - y) * nb_gen_by_century / 100

(** Recursively mark descendants and spouses as old,
    as long as a date allow you to do so, or until
    the number of generations that should be considered old according
    to latest known date is reached.
*)
let mark_descendants base scanned old treshold =
  let rec loop p ndgen =
    let p_key_index = get_key_index p in
    let i = Adef.int_of_iper p_key_index in
    if scanned.(i) < ndgen then begin
      (* If we did not already scanned with ndgen >= current ndgen *)
      let ndgen = match most_recent_year_of p with
        | Some y ->
          (* We have a date: we do not want to scan this person again with a higher ndgen *)
          scanned.(i) <- max_int ;
          compute_ndgen treshold y
        | None ->
          scanned.(i) <- ndgen ;
          ndgen
      in
      if ndgen > 0 then
        begin
          let ndgen' = ndgen - 1 in
          old.(i) <- true ;
          Array.iter
            (fun ifam ->
               let fam = foi base ifam in
               let sp = Gutil.spouse p_key_index fam in
               let i = Adef.int_of_iper sp in
               if scanned.(i) < ndgen then begin
                 let ndgen'' =
                   Opt.map_default ndgen
                     (compute_ndgen treshold)
                     (most_recent_year_of (poi base sp))
                 in
                 if ndgen'' <> 0 then old.(i) <- true ;
                 Array.iter
                   (fun c -> loop (poi base c) (min ndgen' ndgen''))
                   (get_children fam)
               end)
            (get_family p)
        end
    end
  in
  loop

let mark_ancestors base scanned treshold =
  let rec loop p =
    let i = Adef.int_of_iper (get_key_index p) in
    if not scanned.(i) then begin
      scanned.(i) <- true ;
      begin match oldest_year_of p with
        | Some y when y >= treshold ->
          Printf.eprintf "Problem of date ! %s %d\n" (Gutil.designation base p) y;
          flush stderr
        | _ ->
          if get_access p <> Public
          && not (is_quest_string (get_first_name p))
          && not (is_quest_string (get_surname p))
          then begin
            let p = {(gen_person_of_person p) with access = Public} in
            patch_person base p.key_index p ;
            changes := true
          end
      end ;
      Opt.iter
        (fun ifam ->
           let cpl = foi base ifam in
           loop (poi base (get_father cpl)) ;
           loop (poi base (get_mother cpl)) )
        (get_parents p)
    end
  in
  loop

let public_everybody ~mem bname =
  let base = Gwdb.open_base bname in
  if not mem then load_persons_array base ;
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    if get_access p <> Public then
      let p = {(gen_person_of_person p) with access = Public} in
      patch_person base p.key_index p
  done;
  commit_patches base

let public_all ~mem bname treshold =
  let base = Gwdb.open_base bname in
  if not mem then begin
    load_persons_array base ;
    load_ascends_array base ;
    load_couples_array base ;
  end ;
  Consang.check_noloop base
    (function
        OwnAncestor p ->
        Printf.printf "I cannot deal this database.\n";
        Printf.printf "%s is his own ancestors\n" (Gutil.designation base p);
        flush stdout;
        exit 2
      | _ -> assert false);
  let nb = nb_of_persons base in
  let old = Array.make nb false in
  let scanned = Array.make nb (-1) in
  ProgrBar.start () ;
  for i = 0 to nb - 1 do
    ProgrBar.run i nb ;
    if scanned.(i) < 0 then
      let p = poi base (Adef.iper_of_int i) in
      mark_descendants base scanned old treshold p 0
  done;
  ProgrBar.finish () ;
  let scanned = Array.make nb false in
  ProgrBar.start () ;
  for i = 0 to nb_of_persons base - 1 do
    ProgrBar.run i nb ;
    if old.(i) && not scanned.(i) then
      let p = poi base (Adef.iper_of_int i) in
      mark_ancestors base scanned treshold p
  done;
  ProgrBar.finish () ;
  if not mem then begin
    clear_persons_array base ;
    clear_ascends_array base ;
    clear_couples_array base ;
  end ;
  if !changes then commit_patches base

let public_some bname treshold key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
  | [ip] ->
    let p = poi base ip in
    let scanned = Array.make (nb_of_persons base) false in
    let () = load_ascends_array base in
    let () = load_couples_array base in
    mark_ancestors base scanned treshold p;
    let () = clear_ascends_array base in
    let () = clear_couples_array base in
    if !changes then commit_patches base
  | _ ->
    match Gutil.person_of_string_dot_key base key with
    | Some ip ->
      let p = poi base ip in
      if get_access p <> Private then begin
        let p = {(gen_person_of_person p) with access = Private} in
        patch_person base p.key_index p
      end ;
      commit_patches base
    | None ->
      Printf.eprintf "Bad key %s\n" key; flush stderr; exit 2

let treshold = ref 1900
let ind = ref ""
let bname = ref ""
let everybody = ref false
let mem = ref false

let speclist =
  ["-y", Arg.Int (fun i -> treshold := i),
   "treshold year. Anybody born before this year is considered old (default = " ^ string_of_int !treshold ^ ")";
   "-everybody", Arg.Set everybody, "set flag public to everybody.";
   "-ind", Arg.String (fun x -> ind := x), "individual key. Process only this individual and its ancestors.";
   "-mem", Arg.Set mem, "save memory (slower)";
  ]
let anonfun i = bname := i
let usage = "Usage: public1 [-everybody] [-mem] [-y #] [-ind key] base"

let () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage ; exit 2 end ;
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry (Mutil.lock_file !bname) ~onerror:Lock.print_error_and_exit @@ fun () ->
  if !everybody then
    if !ind <> "" then failwith "-everybody and -ind options are mutually exclusive"
    else if !treshold <> 1900 then failwith "-everybody and -y options are mutually exclusive"
    else public_everybody ~mem:!mem !bname
  else if !ind = "" then public_all ~mem:!mem !bname !treshold
  else public_some !bname !treshold !ind
