(* $Id: mk_consang.ml,v 5.56 2012-01-18 21:03:02 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)


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


let unique_key_string (ht, scnt) s =
  let s = Name.lower (Mutil.nominative s) in
  try Hashtbl.find ht s with
    Not_found ->
      let istr = Adef.istr_of_int !scnt in
      Hashtbl.add ht s istr; incr scnt; istr

(* let make_key_index db2 nb_per bdir =
 *   if !(Mutil.verbose) then begin Printf.eprintf "key index..."; flush stderr end;
 *   let person_of_key_d = Filename.concat bdir "person_of_key" in
 *   (try Mutil.mkdir_p person_of_key_d with _ -> ());
 *   let ht_index_of_key = Hashtbl.create 1 in
 *   let ht_strings = Hashtbl.create 1, ref 0 in
 *   let f1f2_fn = Filename.concat "new_d" "person", "first_name" in
 *   let f1f2_sn = Filename.concat "new_d" "person", "surname" in
 *   let f1f2_oc = Filename.concat "new_d" "person", "occ" in
 *   for i = 0 to nb_per - 1 do
 *     let fn =
 *       let pos = Db2disk.get_field_acc db2 i f1f2_fn in
 *       Db2disk.string_of_istr2 db2 f1f2_fn pos
 *     in
 *     assert (Obj.tag (Obj.repr fn) = Obj.string_tag);
 *     let sn =
 *       let pos = Db2disk.get_field_acc db2 i f1f2_sn in
 *       Db2disk.string_of_istr2 db2 f1f2_sn pos
 *     in
 *     assert (Obj.tag (Obj.repr sn) = Obj.string_tag);
 *     if fn = "?" || sn = "?" then ()
 *     else
 *       let fn = unique_key_string ht_strings fn in
 *       let sn = unique_key_string ht_strings sn in
 *       let oc = Db2disk.get_field db2 i f1f2_oc in
 *       Hashtbl.add ht_index_of_key (Db2.key2_of_key (fn, sn, oc))
 *         (Adef.iper_of_int i)
 *   done;
 *   Db2out.output_hashtbl person_of_key_d "iper_of_key.ht"
 *     (ht_index_of_key : (Db2.key2, Def.iper) Hashtbl.t);
 *   Hashtbl.clear ht_index_of_key;
 *   Db2out.output_hashtbl person_of_key_d "istr_of_string.ht"
 *     (fst ht_strings : (string, Adef.istr) Hashtbl.t);
 *   Hashtbl.clear (fst ht_strings);
 *   if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end *)

let simple_output bname base _carray =
  (* match carray with
   *   Some tab ->
   *   Gwdb.apply base
   *       (fun db2 ->
   *         let bdir = db2.Db2disk.bdir2 in
   *          let dir =
   *            List.fold_left Filename.concat bdir ["person"; "consang"]
   *          in
   *          Mutil.mkdir_p dir;
   *          let oc = open_out_bin (Filename.concat dir "data") in
   *          output_value oc tab;
   *          close_out oc;
   *          let oc = open_out_bin (Filename.concat dir "access") in
   *          let _ =
   *            (Iovalue.output_array_access oc (Array.get tab)
   *               (Array.length tab) 0 :
   *             int)
   *          in
   *          close_out oc;
   *          let has_patches =
   *            Sys.file_exists (Filename.concat bdir "patches")
   *          in
   *          if has_patches then
   *            let list =
   *              Hashtbl.fold
   *                (fun ip a list ->
   *                   let a =
   *                     {a with Def.consang = tab.(Adef.int_of_iper ip)}
   *                   in
   *                   (ip, a) :: list)
   *                db2.Db2disk.patches.Db2disk.h_ascend []
   *            in
   *            List.iter
   *              (fun (ip, a) ->
   *                 Hashtbl.replace db2.Db2disk.patches.Db2disk.h_ascend ip a)
   *              list;
   *            Db2disk.commit_patches2 db2;
   *            rebuild_fields2 db2)
   * | None -> *)
      Gwdb.apply base
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
