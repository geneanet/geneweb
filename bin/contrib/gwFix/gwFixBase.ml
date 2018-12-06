open Geneweb
open Def
open Gwdb

let suspend_with msg = ProgrBar.suspend (); msg () ; flush stdout
let restart_with_fixed i n = Printf.printf "*** fixed\n"; flush stdout; ProgrBar.restart i n

let check_keys ~verbosity1 ~verbosity2 base nb_ind fix =
  if verbosity1 then (Printf.printf "Check keys\n"; flush stdout);
  if verbosity1 then ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    if verbosity1 then ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    let occ = get_occ p in
    if fn <> "?" && sn <> "?" then
      let patch ?msg base ip fn sn occ =
        Opt.iter suspend_with msg ;
        Gwdb.patch_key base ip fn sn occ;
        incr fix ;
        Opt.iter (fun _ -> restart_with_fixed i nb_ind) msg
      in
      match Gwdb.person_of_key base fn sn occ with
      | Some ip2 when ip2 <> ip ->
        let msg =
          if verbosity2
          then Some (fun () ->
              Printf.printf
                "*** key %s.%d %s is \"%s\"\n"
                fn occ sn (Gutil.designation base (poi base ip2)) )
          else None
        in
        patch ?msg base ip fn sn occ
      | None ->
        let msg =
          if verbosity2
          then Some (fun () -> Printf.printf "*** key %s.%d %s = no anwser\n" fn occ sn)
          else None
        in
        patch ?msg base ip fn sn occ
      | _ -> ()
  done;
  if verbosity1 then ProgrBar.finish ()

let check_families_parents ~verbosity1 base nb_fam =
  if verbosity1 then begin
    Printf.printf "Check families' parents\n";
    flush stdout;
    ProgrBar.start () ;
    for i = 0 to nb_fam - 1 do
      if verbosity1 then ProgrBar.run i nb_fam;
      let ifam = Adef.ifam_of_int i in
      let fam = foi base ifam in
      if not @@ is_deleted_family fam then
        let a = get_parent_array fam in
        for j = 0 to Array.length a - 1 do
          let ip = a.(j) in
          if not @@ Array.mem ifam (get_family (poi base ip)) then
            begin
              suspend_with (fun () ->
                  Printf.printf
                    "*** no family for : %s\n"
                    (Gutil.designation base (poi base ip)) ) ;
              flush stdout ;
              ProgrBar.restart i nb_fam
            end
        done
    done;
    ProgrBar.finish ()
  end

let check_families_children ~verbosity1 ~verbosity2 base nb_fam fix =
  if verbosity1 then begin
    Printf.printf "Check families' children\n";
    flush stdout;
    ProgrBar.start ()
  end;
  for i = 0 to nb_fam - 1 do
    if verbosity1 then ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if not (is_deleted_family fam) then
      let children = get_children fam in
      for j = 0 to Array.length children - 1 do
        let ip = children.(j) in
        let a = poi base ip in
        match get_parents a with
        | Some ifam1 when ifam1 != ifam && verbosity1 ->
          Printf.printf "*** bad parents : %s\n" (Gutil.designation base (poi base ip));
          flush stdout
        | None ->
          if verbosity2 then begin
            ProgrBar.suspend ();
            Printf.printf "*** no parents : %s in family\n    %s & %s\n"
              (Gutil.designation base (poi base ip))
              (let ip = get_father fam in Gutil.designation base (poi base ip))
              (let ip = get_mother fam in Gutil.designation base (poi base ip));
            flush stdout
          end ;
          patch_ascend base ip
            {parents = Some ifam; consang = get_consang a};
          incr fix ;
          if verbosity1 then ProgrBar.restart i nb_fam ;
        | _ -> ()
      done
  done;
  if verbosity1 then ProgrBar.finish ()

let check_persons_parents ~verbosity1 ~verbosity2 base nb_ind fix =
  if verbosity1 then begin
    Printf.printf "Check persons' parents\n";
    flush stdout;
    ProgrBar.start ()
  end;
  for i = 0 to nb_ind - 1 do
    if verbosity1 then ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let a = poi base ip in
    get_parents a |> Opt.iter @@ fun ifam ->
    let fam = foi base ifam in
    if is_deleted_family fam then begin
      if verbosity2 then begin
        Printf.printf
          "*** parent family deleted: %s\n"
          (Gutil.designation base (poi base ip)) ;
        flush stdout
      end ;
      patch_ascend base ip {parents = None; consang = Adef.fix (-1)};
      incr fix
    end
    else
      let children = get_children fam in
      if not @@ Array.mem ip children then
        begin
          if verbosity2 then begin
            Printf.printf "*** not in parent's family: %s\n"
              (Gutil.designation base (poi base ip));
            flush stdout
          end ;
          let children = Array.append children [| ip |] in
          patch_descend base ifam {children = children}; incr fix
        end
  done;
  if verbosity1 then ProgrBar.finish ()

let check_persons_families ~verbosity1 ~verbosity2 base nb_ind fix =
  if verbosity1 then begin
    Printf.printf "Check persons' families\n";
    flush stdout;
    ProgrBar.start ()
  end;
  for i = 0 to nb_ind - 1 do
    if verbosity1 then ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let u = poi base ip in
    let ifams = get_family u in
    for j = 0 to Array.length ifams - 1 do
      let ifam = ifams.(j) in
      let cpl = foi base ifam in
      if not @@ Array.mem ip (get_parent_array cpl) then
        begin
          if verbosity2 then
            suspend_with (fun () ->
                Printf.printf "*** not father or mother of hir family: %s\n"
              (Gutil.designation base (poi base ip)) );
          let ifams =
            Array.append (Array.sub ifams 0 j)
              (Array.sub ifams (j + 1) (Array.length ifams - j - 1))
          in
          patch_union base ip {family = ifams};
          incr fix;
          if verbosity2 then restart_with_fixed i nb_ind ;
        end
    done
  done;
  if verbosity1 then ProgrBar.finish ()

let check_witnesses ~verbosity1 ~verbosity2 base nb_fam fix =
  if verbosity1 then begin
    Printf.printf "Check witnesses\n";
    flush stdout;
    ProgrBar.start ()
  end;
  for i = 0 to nb_fam - 1 do
    if verbosity1 then ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    let witn = get_witnesses fam in
    let ifath = get_father fam in
    for j = 0 to Array.length witn - 1 do
      let ip = witn.(j) in
      let p = poi base ip in
      if not (List.memq ifath (get_related p)) then
        begin
          if verbosity2 then
            suspend_with (fun () ->
                let imoth = get_mother fam in
                Printf.printf "*** in marriage: %s & %s\n"
                  (Gutil.designation base (poi base ifath))
                  (Gutil.designation base (poi base imoth));
                Printf.printf "*** witness has no pointer to marriage: %s\n"
                  (Gutil.designation base p) ) ;
          patch_person base ip
            {(gen_person_of_person p) with related = ifath :: get_related p};
          incr fix;
          if verbosity2 then restart_with_fixed i nb_fam
        end
    done
  done;
  if verbosity1 then ProgrBar.finish ()

let check_pevents_witnesses ~verbosity1 ~verbosity2 base nb_ind fix =
  if verbosity1 then begin
    Printf.printf "Check persons' events witnesses\n";
    flush stdout;
    ProgrBar.start ()
  end;
  for i = 0 to nb_ind - 1 do
    if verbosity1 then ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    List.iter
      (fun evt ->
         let witn = Array.map fst evt.epers_witnesses in
         for j = 0 to Array.length witn - 1 do
           let ip2 = witn.(j) in
           let p2 = poi base ip2 in
           if not (List.memq ip (get_related p2)) then
             begin
               if verbosity2 then suspend_with (fun () ->
                   Printf.printf "*** in persons' event: %s\n"
                     (Gutil.designation base (poi base ip2));
                   Printf.printf "*** witness has no pointer to persons' event: %s\n"
                     (Gutil.designation base p) ) ;
               patch_person base ip2
                 {(gen_person_of_person p2)
                  with related = ip :: get_related p2};
               incr fix;
               if verbosity2 then restart_with_fixed i nb_ind
             end
         done)
      (get_pevents p)
  done;
  if verbosity1 then ProgrBar.finish ()

let check_fevents_witnesses ~verbosity1 ~verbosity2 base nb_fam fix =
  if verbosity1 then begin
    Printf.printf "Check family events witnesses\n";
    flush stdout;
    ProgrBar.start ()
  end;
  for i = 0 to nb_fam - 1 do
    if verbosity1 then ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    let ifath = get_father fam in
    List.iter
      (fun evt ->
         let witn = Array.map fst evt.efam_witnesses in
         for j = 0 to Array.length witn - 1 do
           let ip = witn.(j) in
           let p = poi base ip in
           if not (List.memq ifath (get_related p)) then
             begin
               if verbosity2 then
                 suspend_with (fun () ->
                     let imoth = get_mother fam in
                     Printf.printf "*** in family event: %s & %s\n"
                       (Gutil.designation base (poi base ifath))
                       (Gutil.designation base (poi base imoth));
                     Printf.printf "*** witness has no pointer to family event: %s\n"
                       (Gutil.designation base p) ) ;
               patch_person base ip
                 {(gen_person_of_person p)
                  with related = ifath :: get_related p};
               incr fix;
               if verbosity2 then restart_with_fixed i nb_fam
             end
         done)
      (get_fevents fam)
  done;
  if verbosity1 then ProgrBar.finish ()


(* FIXME: copy/paste depuis mk_consang *)
let init_cache_info bname base =
  (* Reset le nombre réel de personnes d'une base. *)
  let nb_real_persons = ref 0 in
  let nb_ind = Gwdb.nb_of_persons base in
  let is_empty_name p =
    (Gwdb.is_empty_string (Gwdb.get_surname p) ||
     Gwdb.is_quest_string (Gwdb.get_surname p)) &&
    (Gwdb.is_empty_string (Gwdb.get_first_name p) ||
     Gwdb.is_quest_string (Gwdb.get_first_name p))
  in
  for i = 0 to nb_ind - 1 do
    let ip = Adef.iper_of_int i in
    let p = Gwdb.poi base ip in
    if not @@ is_empty_name p then incr nb_real_persons
  done;
  (* Il faudrait que cache_nb_base_persons ne soit pas dans util.ml *)
  let ht = Hashtbl.create 1 in
  let () =
    Hashtbl.add ht "cache_nb_persons" (string_of_int !nb_real_persons)
  in
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let fname = Filename.concat bdir "cache_info" in
  match try Some (Secure.open_out_bin fname) with Sys_error _ -> None with
    Some oc -> output_value oc ht; close_out oc
  | None -> ()

let check
    ~verbosity
    ~fast
    ~keys
    ~f_parents
    ~f_children
    ~p_parents
    ~p_families
    ~witnesses
    ~pevents_witnesses
    ~fevents_witnesses
    bname =
  let verbosity1 = !verbosity >= 1 in
  let verbosity2 = !verbosity >= 2 in
  if not verbosity1 then Mutil.verbose := false ;
  let fast = !fast in
  let base = Gwdb.open_base bname in
  let fix = ref 0 in
  let nb_fam = nb_of_families base in
  let nb_ind = nb_of_persons base in
  if fast then begin load_strings_array base ; load_persons_array base end ;
  if !keys then check_keys ~verbosity1 ~verbosity2 base nb_ind fix;
  if !f_parents then check_families_parents ~verbosity1 base nb_fam;
  if !f_children then check_families_children ~verbosity1 ~verbosity2 base nb_fam fix;
  if !p_parents then check_persons_parents ~verbosity1 ~verbosity2 base nb_ind fix;
  if !p_families then check_persons_families ~verbosity1 ~verbosity2 base nb_ind fix;
  if !witnesses then check_witnesses ~verbosity1 ~verbosity2 base nb_fam fix;
  if !pevents_witnesses then check_pevents_witnesses ~verbosity1 ~verbosity2 base nb_ind fix;
  if !fevents_witnesses then check_fevents_witnesses ~verbosity1 ~verbosity2 base nb_fam fix;
  if fast then begin clear_strings_array base ; clear_persons_array base end ;
  if !fix <> 0 then begin
    Gwdb.commit_patches base ;
    if verbosity1 then begin
      Printf.printf "%n changes commited\n" !fix ;
      flush stdout
    end
  end
  else if verbosity1 then begin
    Printf.printf "No change\n" ;
    flush stdout
  end ;
  if verbosity1 then (Printf.printf "Rebuilding the indexes..\n" ; flush stdout) ;
  (* FIXME: this one only works with base1 *)
  Gwdb.apply_base1 base
    (fun base -> Outbase.gen_output false base.Dbdisk.data.Dbdisk.bdir base) ;
  (* On recalcul le nombre reel de personnes. *)
  init_cache_info bname base ;
  if verbosity1 then (Printf.printf "Done" ; flush stdout)

(**/**)

let bname = ref ""
let verbosity = ref 2
let fast = ref false
let keys = ref false
let f_parents = ref false
let f_children = ref false
let p_parents = ref false
let p_families = ref false
let witnesses = ref false
let pevents_witnesses = ref false
let fevents_witnesses = ref false

let speclist =
  [ ("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode")
  ; ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode")
  ; ("-fast", Arg.Set fast, " fast mode. Needs more memory.")
  ; ("-keys", Arg.Set keys, " missing doc")
  ; ("-families-parents", Arg.Set f_parents, " missing doc")
  ; ("-families-children", Arg.Set f_children, " missing doc")
  ; ("-persons-parents", Arg.Set p_parents, " missing doc")
  ; ("-persons-families", Arg.Set p_families, " missing doc")
  ; ("-witnesses", Arg.Set witnesses, " missing doc")
  ; ("-pevents-witnesses", Arg.Set pevents_witnesses, " missing doc")
  ; ("-fevents-witnesses", Arg.Set fevents_witnesses, " missing doc")
  ]

let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  Secure.set_base_dir (Filename.dirname !bname);
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Lock.control (Mutil.lock_file !bname) false ~onerror:Lock.print_try_again @@
  fun () ->
  if !keys
  || !f_parents
  || !f_children
  || !p_parents
  || !p_families
  || !witnesses
  || !pevents_witnesses
  || !fevents_witnesses
  then ()
  else begin
    keys := true ;
    f_parents := true ;
    f_children := true ;
    p_parents := true ;
    p_families := true ;
    witnesses := true ;
    pevents_witnesses := true ;
    fevents_witnesses := true
  end ;
  check
    ~fast
    ~verbosity
    ~keys
    ~f_parents
    ~f_children
    ~p_parents
    ~p_families
    ~witnesses
    ~pevents_witnesses
    ~fevents_witnesses
    !bname

let _ = main ()
