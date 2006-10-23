(* camlp4r ./pa_lock.cmo *)
(* $Id: mk_consang.ml,v 5.12 2006-10-23 20:06:31 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

value fname = ref "";
value indexes = ref False;
value scratch = ref False;
value quiet = ref False;

value errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>";
value speclist =
  [("-q", Arg.Set quiet, ": quiet mode");
   ("-i", Arg.Set indexes, ": build the indexes again");
   ("-scratch", Arg.Set scratch, ": from scratch");
   ("-mem", Arg.Set Outbase.save_mem,
    ": Save memory, but slower when rewritting database");
   ("-nolock", Arg.Set Lock.no_lock_flag, ": do not lock database.")]
;
value anonfun s =
  if fname.val = "" then fname.val := s
  else raise (Arg.Bad "Cannot treat several databases")
;

value simple_output bname base carray =
  match carray with
  [ Some tab -> Gwdb.output_consang_tab base tab
  | None ->
      let no_patches =
        let bname =
          if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
        in
        not (Sys.file_exists (Filename.concat bname "patches"))
      in
      Gwdb.apply_as_dsk_base
        (Outbase.gen_output (no_patches && not indexes.val) bname) base ]
;

value designation base p =
  let first_name = Gwdb.p_first_name base p in
  let nom = Gwdb.p_surname base p in
  Mutil.iso_8859_1_of_utf_8
    (first_name ^ "." ^ string_of_int (Gwdb.get_occ p) ^ " " ^ nom)
;

value open_base bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  if Sys.file_exists (Filename.concat bname "base_d") then do {
    Printf.eprintf "*** database new implementation\n";
    flush stderr; 
    Gwdb.base_of_base2 bname
  }
  else Gwdb.base_of_dsk_base (Database.opendb bname)
;

value main () =
  do {
    Argl.parse speclist anonfun errmsg;
    if fname.val = "" then do {
      Printf.eprintf "Missing file name\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2;
    }
    else ();
    Secure.set_base_dir (Filename.dirname fname.val);
    let f () =
      let base = open_base fname.val in
      try
        do {
          Sys.catch_break True;
          let carray = ConsangAll.compute base scratch.val quiet.val in
          simple_output fname.val base carray;
        }
      with
      [ Consang.TopologicalSortError p ->
          do {
            Printf.printf
              "\nError: loop in database, %s is his/her own ancestor.\n"
              (designation base p);
            flush stdout;
            exit 2
          } ]
    in
    lock (Mutil.lock_file fname.val) with
    [ Accept -> f ()
    | Refuse ->
        do {
          Printf.eprintf "Base is locked. Waiting... ";
          flush stderr;
          lock_wait (Mutil.lock_file fname.val) with
          [ Accept -> do { Printf.eprintf "Ok\n"; flush stderr; f () }
          | Refuse ->
              do {
                Printf.printf "\nSorry. Impossible to lock base.\n";
                flush stdout;
                exit 2
              } ]
        } ]
  }
;

Printexc.catch main ();
