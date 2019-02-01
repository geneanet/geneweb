(* Copyright (c) 1998-2007 INRIA *)

open Geneweb

let fname = ref ""
let indexes = ref false
let scratch = ref false
let verbosity = ref 2
let tlim = ref (-1)
let fast = ref false

let errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>"
let speclist =
  [("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode");
   ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode");
   ("-fast", Arg.Set fast, " faster, but use more memory");
   "-i", Arg.Set indexes, ": build the indexes again";
   "-t", Arg.Int (fun i -> tlim := i), " <int>: time limit in seconds";
   "-scratch", Arg.Set scratch, ": from scratch";
   "-mem", Arg.Set Outbase.save_mem,
   ": Save memory, but slower when rewritting database";
   "-nolock", Arg.Set Lock.no_lock_flag, ": do not lock database."]
let anonfun s =
  if !fname = "" then fname := s
  else raise (Arg.Bad "Cannot treat several databases")

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

let rebuild_field_array len pad bdir compress f =
  if !(Mutil.verbose) then
    begin
      Printf.eprintf "rebuilding %s..." (Filename.basename bdir);
      flush stderr
    end;
  if compress then Db2out.output_value_array_compress bdir "" len pad f
  else Db2out.output_value_array_no_compress bdir "" len pad f;
  if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end

type ('index, 'item) field_info =
  { fi_nb : int;
    fi_ht : ('index, 'item) Hashtbl.t;
    fi_index_of_int : int -> 'index;
    fi_dir : string }

let rebuild_any_field_array db2 fi pad compress (f2, get) =
  let f1 = fi.fi_dir in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  rebuild_field_array fi.fi_nb pad bdir compress
    (fun oc_acc output_item ->
       (* put pad as 1st elem; not necessary, just for beauty *)
       if compress then ignore (output_item pad : int);
       for i = 0 to fi.fi_nb - 1 do
         let x =
           try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
             Not_found ->
               let pos = Db2disk.get_field_acc db2 i (f1, f2) in
               Db2disk.get_field_data db2 pos (f1, f2) "data"
         in
         let pos = output_item x in output_binary_int oc_acc pos
       done)

let rebuild_option_field_array db2 fi pad (f2, get) =
  let f1 = fi.fi_dir in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  rebuild_field_array fi.fi_nb pad bdir true
    (fun oc_acc output_item ->
       for i = 0 to fi.fi_nb - 1 do
         let x =
           try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
             Not_found ->
               let pos = Db2disk.get_field_acc db2 i (f1, f2) in
               if pos = -1 then None
               else Some (Db2disk.get_field_data db2 pos (f1, f2) "data")
         in
         match x with
           None -> output_binary_int oc_acc (-1)
         | Some x -> let pos = output_item x in output_binary_int oc_acc pos
       done)

let rebuild_list_field_array db2 fi (f2, get) =
  let f1 = fi.fi_dir in
  let f oc_acc oc_dat =
    for i = 0 to fi.fi_nb - 1 do
      let x =
        try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
          Not_found ->
            let pos = Db2disk.get_field_acc db2 i (f1, f2) in
            if pos = -1 then []
            else Db2disk.get_field_data db2 pos (f1, f2) "data"
      in
      if x = [] then output_binary_int oc_acc (-1)
      else
        let pos = pos_out oc_dat in
        Iovalue.output oc_dat x; output_binary_int oc_acc pos
    done
  in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  if !(Mutil.verbose) then
    begin
      Printf.eprintf "rebuilding %s..." (Filename.basename bdir);
      flush stderr
    end;
  let oc_dat = open_out_bin (Filename.concat bdir "data") in
  let oc_acc = open_out_bin (Filename.concat bdir "access") in
  f oc_acc oc_dat;
  close_out oc_acc;
  close_out oc_dat;
  if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end

let rebuild_list2_field_array db2 fi (f2, get) =
  let f1 = fi.fi_dir in
  let f oc_acc oc_dat =
    for i = 0 to fi.fi_nb - 1 do
      let rxl =
        try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
          Not_found ->
            let pos = Db2disk.get_field_acc db2 i (f1, f2) in
            let rec loop list pos =
              if pos = -1 then list
              else
                let (x, pos) =
                  Db2disk.get_field_2_data db2 pos (f1, f2) "data"
                in
                loop (x :: list) pos
            in
            loop [] pos
      in
      let pos =
        let rec loop pos =
          function
            [] -> pos
          | x :: xl ->
              let new_pos = pos_out oc_dat in
              Iovalue.output oc_dat x;
              Iovalue.output oc_dat pos;
              loop new_pos xl
        in
        loop (-1) rxl
      in
      output_binary_int oc_acc pos
    done
  in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  if !(Mutil.verbose) then
    begin
      Printf.eprintf "rebuilding %s..." (Filename.basename bdir);
      flush stderr
    end;
  let oc_dat = open_out_bin (Filename.concat bdir "data") in
  let oc_acc = open_out_bin (Filename.concat bdir "access") in
  f oc_acc oc_dat;
  close_out oc_acc;
  close_out oc_dat;
  if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end

let rebuild_string_field db2 fi (f2, get) =
  let f1 = fi.fi_dir in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  rebuild_field_array fi.fi_nb "" bdir true
    (fun oc_acc output_item ->
       for i = 0 to fi.fi_nb - 1 do
         let s =
           try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
             Not_found ->
               let pos = Db2disk.get_field_acc db2 i (f1, f2) in
               Db2disk.string_of_istr2 db2 (f1, f2) pos
         in
         let pos = output_item s in output_binary_int oc_acc pos
       done)

let rebuild_list_with_string_field_array g h db2 fi (f2, get) =
  let f1 = fi.fi_dir in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  let oc_ext = open_out_bin (Filename.concat bdir "data2.ext") in
  rebuild_field_array fi.fi_nb "" bdir true
    (fun oc_acc output_item ->
       for i = 0 to fi.fi_nb - 1 do
         let sl =
           try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
             Not_found ->
               let list : 'a list =
                 let pos = Db2disk.get_field_acc db2 i (f1, f2) in
                 if pos = -1 then []
                 else Db2disk.get_field_data db2 pos (f1, f2) "data2.ext"
               in
               List.map (g (Db2disk.string_of_istr2 db2 (f1, f2))) list
         in
         let pl = List.map (h output_item) sl in
         if pl = [] then output_binary_int oc_acc (-1)
         else
           begin
             output_binary_int oc_acc (pos_out oc_ext);
             let (s32, s64) = !(Iovalue.size_32), !(Iovalue.size_64) in
             Iovalue.output oc_ext (pl : 'a list);
             Iovalue.size_32 := s32;
             Iovalue.size_64 := s64
           end
       done);
  close_out oc_ext

let unique_key_string (ht, scnt) s =
  let s = Name.lower (Mutil.nominative s) in
  try Hashtbl.find ht s with
    Not_found ->
      let istr = Adef.istr_of_int !scnt in
      Hashtbl.add ht s istr; incr scnt; istr

let make_key_index db2 nb_per bdir =
  if !(Mutil.verbose) then begin Printf.eprintf "key index..."; flush stderr end;
  let person_of_key_d = Filename.concat bdir "person_of_key" in
  (try Mutil.mkdir_p person_of_key_d with _ -> ());
  let ht_index_of_key = Hashtbl.create 1 in
  let ht_strings = Hashtbl.create 1, ref 0 in
  let f1f2_fn = Filename.concat "new_d" "person", "first_name" in
  let f1f2_sn = Filename.concat "new_d" "person", "surname" in
  let f1f2_oc = Filename.concat "new_d" "person", "occ" in
  for i = 0 to nb_per - 1 do
    let fn =
      let pos = Db2disk.get_field_acc db2 i f1f2_fn in
      Db2disk.string_of_istr2 db2 f1f2_fn pos
    in
    assert (Obj.tag (Obj.repr fn) = Obj.string_tag);
    let sn =
      let pos = Db2disk.get_field_acc db2 i f1f2_sn in
      Db2disk.string_of_istr2 db2 f1f2_sn pos
    in
    assert (Obj.tag (Obj.repr sn) = Obj.string_tag);
    if fn = "?" || sn = "?" then ()
    else
      let fn = unique_key_string ht_strings fn in
      let sn = unique_key_string ht_strings sn in
      let oc = Db2disk.get_field db2 i f1f2_oc in
      Hashtbl.add ht_index_of_key (Db2.key2_of_key (fn, sn, oc))
        (Adef.iper_of_int i)
  done;
  Db2out.output_hashtbl person_of_key_d "iper_of_key.ht"
    (ht_index_of_key : (Db2.key2, Def.iper) Hashtbl.t);
  Hashtbl.clear ht_index_of_key;
  Db2out.output_hashtbl person_of_key_d "istr_of_string.ht"
    (fst ht_strings : (string, Adef.istr) Hashtbl.t);
  Hashtbl.clear (fst ht_strings);
  if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end

let rebuild_fields2 db2 =
  let fi_per =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_per;
     fi_ht = db2.Db2disk.patches.Db2disk.h_person;
     fi_index_of_int = Adef.iper_of_int; fi_dir = "person"}
  in
  let fi_asc =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_per;
     fi_ht = db2.Db2disk.patches.Db2disk.h_ascend;
     fi_index_of_int = Adef.iper_of_int; fi_dir = "person"}
  in
  let fi_uni =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_per;
     fi_ht = db2.Db2disk.patches.Db2disk.h_union;
     fi_index_of_int = Adef.iper_of_int; fi_dir = "person"}
  in
  List.iter (rebuild_string_field db2 fi_per)
    ["first_name", (fun p -> p.Def.first_name);
     "surname", (fun p -> p.Def.surname); "image", (fun p -> p.Def.image);
     "public_name", (fun p -> p.Def.public_name);
     "occupation", (fun p -> p.Def.occupation);
     "birth_place", (fun p -> p.Def.birth_place);
     "birth_note", (fun p -> p.Def.birth_note);
     "birth_src", (fun p -> p.Def.birth_src);
     "baptism_place", (fun p -> p.Def.baptism_place);
     "baptism_note", (fun p -> p.Def.baptism_note);
     "baptism_src", (fun p -> p.Def.baptism_src);
     "death_place", (fun p -> p.Def.death_place);
     "death_note", (fun p -> p.Def.death_note);
     "death_src", (fun p -> p.Def.death_src);
     "burial_place", (fun p -> p.Def.burial_place);
     "burial_note", (fun p -> p.Def.burial_note);
     "burial_src", (fun p -> p.Def.burial_src);
     "notes", (fun p -> p.Def.notes); "psources", (fun p -> p.Def.psources)];
  rebuild_any_field_array db2 fi_per 0 true ("occ", (fun p -> p.Def.occ));
  List.iter
    (rebuild_list_with_string_field_array (fun f -> f) (fun f -> f) db2
       fi_per)
    ["qualifiers", (fun p -> p.Def.qualifiers);
     "aliases", (fun p -> p.Def.aliases);
     "first_names_aliases", (fun p -> p.Def.first_names_aliases);
     "surnames_aliases", (fun p -> p.Def.surnames_aliases)];
  rebuild_list_with_string_field_array Futil.map_title_strings
    Futil.map_title_strings db2 fi_per ("titles", (fun p -> p.Def.titles));
  rebuild_list_field_array db2 fi_per ("rparents", (fun p -> p.Def.rparents));
  rebuild_list2_field_array db2 fi_per ("related", (fun p -> p.Def.related));
  rebuild_any_field_array db2 fi_per Def.Neuter true
    ("sex", (fun p -> p.Def.sex));
  rebuild_any_field_array db2 fi_per Def.IfTitles true
    ("access", (fun p -> p.Def.access));
  List.iter (rebuild_any_field_array db2 fi_per Adef.cdate_None true)
    ["birth", (fun p -> p.Def.birth); "baptism", (fun p -> p.Def.baptism)];
  rebuild_any_field_array db2 fi_per Def.NotDead true
    ("death", (fun p -> p.Def.death));
  rebuild_any_field_array db2 fi_per Def.UnknownBurial true
    ("burial", (fun p -> p.Def.burial));
  rebuild_list_field_array db2 fi_per ("pevents", (fun p -> p.Def.pevents));
  rebuild_option_field_array db2 fi_asc (Adef.ifam_of_int (-1))
    ("parents", (fun p -> p.Def.parents));
  rebuild_any_field_array db2 fi_asc Adef.no_consang false
    ("consang", (fun p -> p.Def.consang));
  rebuild_any_field_array db2 fi_uni [| |] false
    ("family", (fun p -> p.Def.family));
  let fi_fam =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_fam;
     fi_ht = db2.Db2disk.patches.Db2disk.h_family;
     fi_index_of_int = Adef.ifam_of_int; fi_dir = "family"}
  in
  let fi_cpl =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_fam;
     fi_ht = db2.Db2disk.patches.Db2disk.h_couple;
     fi_index_of_int = Adef.ifam_of_int; fi_dir = "family"}
  in
  let fi_des =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_fam;
     fi_ht = db2.Db2disk.patches.Db2disk.h_descend;
     fi_index_of_int = Adef.ifam_of_int; fi_dir = "family"}
  in
  rebuild_any_field_array db2 fi_fam Adef.cdate_None true
    ("marriage", (fun f -> f.Def.marriage));
  List.iter (rebuild_string_field db2 fi_fam)
    ["marriage_place", (fun f -> f.Def.marriage_place);
     "marriage_note", (fun f -> f.Def.marriage_note);
     "marriage_src", (fun f -> f.Def.marriage_src);
     "comment", (fun f -> f.Def.comment);
     "origin_file", (fun f -> f.Def.origin_file);
     "fsources", (fun f -> f.Def.fsources)];
  rebuild_any_field_array db2 fi_fam [| |] true
    ("witnesses", (fun f -> f.Def.witnesses));
  rebuild_any_field_array db2 fi_fam Def.Married true
    ("relation", (fun f -> f.Def.relation));
  rebuild_any_field_array db2 fi_fam Def.NotDivorced true
    ("divorce", (fun f -> f.Def.divorce));
  rebuild_list_field_array db2 fi_fam ("fevents", (fun f -> f.Def.fevents));
  List.iter (rebuild_any_field_array db2 fi_cpl (Adef.iper_of_int (-1)) true)
    ["father", (fun f -> Adef.father f); "mother", (fun f -> Adef.mother f)];
  rebuild_any_field_array db2 fi_des [| |] false
    ("children", (fun f -> f.Def.children));
  let nb_per = fi_per.fi_nb in
  let new_d = Filename.concat db2.Db2disk.bdir2 "new_d" in
  make_key_index db2 nb_per new_d;
  Gc.compact ();
  let particles =
    Mutil.input_particles (Filename.concat db2.Db2disk.bdir2 "particles.txt")
  in
  Db2out.make_indexes new_d nb_per particles;
  let old_d = Filename.concat db2.Db2disk.bdir2 "old_d" in
  Mutil.remove_dir old_d;
  Mutil.mkdir_p old_d;
  List.iter
    (fun f ->
       Sys.rename (Filename.concat db2.Db2disk.bdir2 f)
         (Filename.concat old_d f))
    ["family"; "person"; "person_of_key"; "person_of_name"; "patches"];
  List.iter
    (fun f ->
       Sys.rename (Filename.concat new_d f)
         (Filename.concat db2.Db2disk.bdir2 f))
    ["family"; "person"; "person_of_key"; "person_of_name"]

let simple_output bname base carray =
  match carray with
    Some tab ->
      Gwdb.apply_base2 base
        (fun db2 ->
           let bdir = db2.Db2disk.bdir2 in
           let dir =
             List.fold_left Filename.concat bdir ["person"; "consang"]
           in
           Mutil.mkdir_p dir;
           let oc = open_out_bin (Filename.concat dir "data") in
           output_value oc tab;
           close_out oc;
           let oc = open_out_bin (Filename.concat dir "access") in
           let _ =
             (Iovalue.output_array_access oc (Array.get tab)
                (Array.length tab) 0 :
              int)
           in
           close_out oc;
           let has_patches =
             Sys.file_exists (Filename.concat bdir "patches")
           in
           if has_patches then
             let list =
               Hashtbl.fold
                 (fun ip a list ->
                    let a =
                      {a with Def.consang = tab.(Adef.int_of_iper ip)}
                    in
                    (ip, a) :: list)
                 db2.Db2disk.patches.Db2disk.h_ascend []
             in
             List.iter
               (fun (ip, a) ->
                  Hashtbl.replace db2.Db2disk.patches.Db2disk.h_ascend ip a)
               list;
             Db2disk.commit_patches2 db2;
             rebuild_fields2 db2)
  | None ->
      Gwdb.apply_base1 base
        (fun base ->
           let bname = base.Dbdisk.data.Dbdisk.bdir in
           let no_patches =
             not (Sys.file_exists (Filename.concat bname "patches"))
           in
           Outbase.gen_output (no_patches && not !indexes) bname base);
      (* On recalcul le nombre reel de personnes. *)
      init_cache_info bname base

let main () =
  Argl.parse speclist anonfun errmsg;
  if !fname = "" then
    begin
      Printf.eprintf "Missing file name\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2
    end;
  if !verbosity = 0 then Mutil.verbose := false ;
  Secure.set_base_dir (Filename.dirname !fname);
  Lock.control_retry
    (Mutil.lock_file !fname)
    ~onerror:Lock.print_error_and_exit
    (fun () ->
       let base = Gwdb.open_base !fname in
       if !fast then begin
         Gwdb.load_persons_array base;
         Gwdb.load_families_array base;
         Gwdb.load_ascends_array base;
         Gwdb.load_unions_array base;
         Gwdb.load_couples_array base;
         Gwdb.load_descends_array base;
         Gwdb.load_strings_array base
       end ;
       try
         Sys.catch_break true;
         let carray = ConsangAll.compute ~verbosity:!verbosity base !tlim !scratch in
         simple_output !fname base carray
       with Consang.TopologicalSortError p ->
         Printf.printf "\nError: loop in database, %s is his/her own ancestor.\n"
           (Gutil.designation base p);
         flush stdout;
         exit 2)

let _ = Printexc.print main ()
