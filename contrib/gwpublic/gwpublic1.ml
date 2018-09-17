(* $Id: public.ml,v 4.26 2007/01/19 09:03:02 deraugla Exp $ *)

open Def
open Gwdb

let year_of p =
  match get_death p with
  | NotDead -> None
  | death -> match Adef.od_of_cdate (get_birth p) with
    | Some (Dgreg (d, _)) -> Some d.year
    | None -> match Adef.od_of_cdate (get_baptism p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | None -> match CheckItem.date_of_death death with
        | Some (Dgreg (d, _)) -> Some d.year
        | None -> None

let most_recent_year_of p =
  match get_death p with
  | NotDead -> None
  | death -> match CheckItem.date_of_death death with
    | Some (Dgreg (d, _)) -> Some d.year
    | None -> match Adef.od_of_cdate (get_baptism p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | None -> match Adef.od_of_cdate (get_birth p) with
        | Some (Dgreg (d, _)) -> Some d.year
        | None -> None

let is_old lim_year p =
  match year_of p with
    Some y -> y < lim_year
  | None -> false

let nb_gen_by_century = 3

let nb_desc_gen lim_year p =
  match most_recent_year_of p with
    Some year -> (lim_year - year) * nb_gen_by_century / 100
  | None -> 0

let changes = ref false

let mark_descendants base scanned old lim_year =
  let rec loop p ndgen =
    let p_key_index = get_key_index p in
    if not scanned.(Adef.int_of_iper p_key_index) then
      let dt = most_recent_year_of p in
      (* a t-il plus de 100 ans *)
      let ndgen =
        match dt with
        | Some y ->
          scanned.(Adef.int_of_iper p_key_index) <- true;
          if y < lim_year then nb_desc_gen lim_year p else 0
        | None -> ndgen
      in
      if ndgen > 0 then
        begin
          old.(Adef.int_of_iper p_key_index) <- true;
          let ndgen = ndgen - 1 in
          Array.iter
            (fun ifam ->
               let fam = foi base ifam in
               let sp = Gutil.spouse p_key_index fam in
               old.(Adef.int_of_iper sp) <- true ;
               Array.iter
                 (fun c -> loop (poi base c) ndgen)
                 (get_children fam) )
            (get_family p)
        end
  in
  loop

let mark_ancestors base scanned lim_year titled is_quest_string =
  let rec loop p =
    if not scanned.(Adef.int_of_iper (get_key_index p)) then
      begin
        scanned.(Adef.int_of_iper (get_key_index p)) <- true;
        (* si pas de date ou date > lim_year *)
        if not (is_old lim_year p) && get_access p <> Public &&
           (titled || get_titles p = []) &&
           not (is_quest_string (get_first_name p)) &&
           not (is_quest_string (get_surname p))
        then
          begin
            begin match year_of p with
              | Some y when y >= lim_year ->
                Printf.eprintf "Problem of date ! %s %d\n"
                  (Gutil.designation base p) y;
                flush stderr
              | None -> ()
            end;
            let p = {(gen_person_of_person p) with access = Public} in
            patch_person base p.key_index p; changes := true
          end;
        match get_parents p with
          Some ifam ->
            let cpl = foi base ifam in
            loop (poi base (get_father cpl)); loop (poi base (get_mother cpl))
        | None -> ()
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

let public_all ~mem bname lim_year titled =
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
  let old = Array.make (nb_of_persons base) false in
  let scanned = Array.make (nb_of_persons base) false in
  for i = 0 to nb_of_persons base - 1 do
    if not scanned.(i) then
      let p = poi base (Adef.iper_of_int i) in
      mark_descendants base scanned old lim_year p 0
  done;
  let scanned = Array.make (nb_of_persons base) false in
  for i = 0 to nb_of_persons base - 1 do
    if old.(i) && not scanned.(i) then
      let p = poi base (Adef.iper_of_int i) in
      mark_ancestors base scanned lim_year titled is_quest_string p
  done;
  if !changes then commit_patches base

let public_some bname lim_year titled key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
    [ip] ->
      let p = poi base ip in
      let scanned = Array.make (nb_of_persons base) false in
      let () = load_ascends_array base in
      let () = load_couples_array base in
      mark_ancestors base scanned lim_year titled is_quest_string p;
      if !changes then commit_patches base
  | _ ->
      match Gutil.person_of_string_dot_key base key with
        Some ip ->
          let p = poi base ip in
          if get_access p <> Private then
            begin let p = {(gen_person_of_person p) with access = Private} in
              patch_person base p.key_index p
            end;
          commit_patches base
      | None -> Printf.eprintf "Bad key %s\n" key; flush stderr; exit 2

let lim_year = ref 1900
let ind = ref ""
let bname = ref ""
let everybody = ref false
let titled = ref true
let mem = ref false

let speclist =
  ["-y", Arg.Int (fun i -> lim_year := i),
   "limit year (default = " ^ string_of_int !lim_year ^ ")";
   "-ct", Arg.Clear titled,
   "check if the person has a title (default = don't check)";
   "-everybody", Arg.Set everybody, "set flag public to everybody";
   "-ind", Arg.String (fun x -> ind := x), "individual key";
   "-mem", Arg.Set mem, "save memory (slower)";
  ]
let anonfun i = bname := i
let usage = "Usage: public1 [-everybody] [-mem] [-y #] [-ind key] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let gcc = Gc.get () in
  gcc.Gc.max_overhead <- 100;
  Gc.set gcc;
  Lock.control_retry (Mutil.lock_file !bname)
    ~onerror:Lock.print_error_and_exit
    (fun () ->
       if !everybody then public_everybody ~mem:!mem !bname
       else if !ind = "" then public_all ~mem:!mem !bname !lim_year !titled
       else public_some !bname !lim_year !titled !ind)

let _ = main ()
