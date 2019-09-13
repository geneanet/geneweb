(* $Id: outbase.ml,v 5.21 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk
open Def
open Type

let load_ascends_array base = base.data.ascends.load_array ()
let load_unions_array base = base.data.unions.load_array ()
let load_couples_array base = base.data.couples.load_array ()
let load_descends_array base = base.data.descends.load_array ()
let load_strings_array base = base.data.strings.load_array ()

let save_mem = ref false
let verbose = Mutil.verbose

let trace s =
  if !verbose then begin Printf.eprintf "*** %s\n" s; flush stderr end

let count_error computed found =
  Printf.eprintf "Count error. Computed %d. Found %d.\n" computed found;
  flush stderr;
  exit 2

let just_copy bname what oc oc_acc =
  if !verbose then Printf.eprintf "*** copying %s\n" what;
  flush stderr;
  let ic =
    let ic = Secure.open_in_bin (Filename.concat bname "base") in
    Dutil.check_magic ic; ic
  in
  let ic_acc = Secure.open_in_bin (Filename.concat bname "base.acc") in
  let persons_len = input_binary_int ic in
  let families_len = input_binary_int ic in
  let strings_len = input_binary_int ic in
  let persons_array_pos = input_binary_int ic in
  let ascends_array_pos = input_binary_int ic in
  let unions_array_pos = input_binary_int ic in
  let families_array_pos = input_binary_int ic in
  let couples_array_pos = input_binary_int ic in
  let descends_array_pos = input_binary_int ic in
  let strings_array_pos = input_binary_int ic in
  let _ = input_value ic in
  let (beg_pos, end_pos, beg_acc_pos, array_len) =
    match what with
      "persons" ->
        let pos = 0 in persons_array_pos, ascends_array_pos, pos, persons_len
    | "ascends" ->
        let pos = persons_len * Iovalue.sizeof_long in
        ascends_array_pos, unions_array_pos, pos, persons_len
    | "unions" ->
        let pos = 2 * persons_len * Iovalue.sizeof_long in
        unions_array_pos, families_array_pos, pos, persons_len
    | "families" ->
        let pos = 3 * persons_len * Iovalue.sizeof_long in
        families_array_pos, couples_array_pos, pos, families_len
    | "couples" ->
        let pos = (3 * persons_len + families_len) * Iovalue.sizeof_long in
        couples_array_pos, descends_array_pos, pos, families_len
    | "descends" ->
        let pos =
          (3 * persons_len + 2 * families_len) * Iovalue.sizeof_long
        in
        descends_array_pos, strings_array_pos, pos, families_len
    | "strings" ->
        let pos =
          (3 * persons_len + 3 * families_len) * Iovalue.sizeof_long
        in
        strings_array_pos, in_channel_length ic, pos, strings_len
    | _ -> failwith ("just copy " ^ what)
  in
  let shift = pos_out oc - beg_pos in
  seek_in ic beg_pos;
  let rec loop pos =
    if pos = end_pos then close_in ic
    else begin output_char oc (input_char ic); loop (pos + 1) end
  in
  loop beg_pos;
  seek_in ic_acc beg_acc_pos;
  let rec loop len =
    if len = array_len then close_in ic_acc
    else
      begin
        output_binary_int oc_acc (input_binary_int ic_acc + shift);
        loop (len + 1)
      end
  in
  loop 0

let split_sname base i = Mutil.split_sname @@ base.data.strings.get i

let split_fname base i = Mutil.split_fname @@ base.data.strings.get i

(* /!\ Keep it sync with Database.name_index_key /!\ *)
let name_index_key s =
  Hashtbl.hash (Name.crush_lower (Name.abbrev s)) mod Dutil.table_size

let make_name_index base =
  let t = Array.make Dutil.table_size [| |] in
  let add_name key value =
    let i = name_index_key key in
    if not @@ Array.mem value t.(i)
    then t.(i) <- Array.append [| value |] t.(i)
  in
  for i = 0 to base.data.persons.len - 1 do
    let p = base.data.persons.get i in
    let first_name = Dutil.p_first_name base p in
    let surname = Dutil.p_surname base p in
    if first_name <> "?" && surname <> "?" then begin
      let names =
        Name.lower (first_name ^ " " ^ surname)
        :: List.map Name.lower (split_fname base p.first_name)
        @ List.map Name.lower (split_sname base p.surname)
        @ Dutil.dsk_person_misc_names base p (fun p -> p.titles)
      in
      List.iter (fun i -> add_name i p.key_index) names
    end
  done;
  (* Array.iter (fun i -> print_endline @@ base.data.strings.get i) t.(i) ; *)
  t

let create_name_index oc_inx oc_inx_acc base =
  let ni = make_name_index base in
  let bpos = pos_out oc_inx in
  Mutil.output_value_no_sharing oc_inx (ni : Dutil.name_index_data);
  let epos =
    Iovalue.output_array_access oc_inx_acc (Array.get ni) (Array.length ni)
      bpos
  in
  if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)

let add_name t key value =
  let i = name_index_key key in
  if not @@ Array.mem value @@ Array.unsafe_get t i
  then Array.unsafe_set t i @@ Array.append [| value |] @@ Array.unsafe_get t i

(* let add_name t key valu =
 *   let key = Name.crush_lower key in
 *   let i = Hashtbl.hash key mod Array.length t in
 *   if Array.mem valu t.(i) then () else t.(i) <- Array.append [| valu |] t.(i) *)

let make_strings_of_fsname base =
  (* Specialised versions of Database.insert_string
     for performance reasons. *)
  let rev = Hashtbl.create base.data.strings.len in
  base.data.persons.load_array ();
  base.data.strings.load_array () ;
  for i = 0 to base.data.strings.len - 1 do
    Hashtbl.add rev (base.data.strings.get i) i
  done ;
  let insert_string s =
    match Hashtbl.find_opt rev s with
    | Some i -> i
    | None ->
      let i = base.func.insert_string s in
      Hashtbl.add rev s i ;
      i
  in
  let t = Array.make Dutil.table_size [| |] in
  for i = 0 to base.data.persons.len - 1 do
    let p = base.data.persons.get i in
    if p.first_name <> 1 then begin
      let fn = base.data.strings.get p.first_name in
      add_name t fn p.first_name ;
      List.iter (fun s -> add_name t s @@ insert_string s) (split_fname base p.first_name) ;
    end ;
    if p.surname <> 1 then begin
      let sn = base.data.strings.get p.surname in
      add_name t sn p.surname ;
      List.iter (fun s -> add_name t s @@ insert_string s) (split_sname base p.surname) ;
    end
  done ;
  t

let create_strings_of_fsname oc_inx oc_inx_acc base =
  let t = make_strings_of_fsname base in
  let bpos = pos_out oc_inx in
  Mutil.output_value_no_sharing oc_inx (t : Dutil.strings_of_fsname);
  let epos =
    Iovalue.output_array_access oc_inx_acc (Array.get t) (Array.length t) bpos
  in
  if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)

let is_prime a =
  let rec loop b =
    if a / b < b then true else if a mod b = 0 then false else loop (b + 1)
  in
  loop 2

let rec prime_after n = if is_prime n then n else prime_after (n + 1)

let output_strings_hash oc2 base =
  let () = base.data.strings.load_array () in
  let strings_array = base.data.strings in
  let taba =
    Array.make
      (min Sys.max_array_length
         (prime_after (max 2 (10 * strings_array.len))))
      (-1)
  in
  let tabl = Array.make strings_array.len (-1) in
  for i = 0 to strings_array.len - 1 do
    let ia = Hashtbl.hash (base.data.strings.get i) mod Array.length taba in
    tabl.(i) <- taba.(ia); taba.(ia) <- i
  done;
  output_binary_int oc2 (Array.length taba);
  output_binary_int oc2 0; (* legacy *)
  output_binary_int oc2 0; (* legacy *)
  for i = 0 to Array.length taba - 1 do output_binary_int oc2 taba.(i) done;
  for i = 0 to Array.length tabl - 1 do output_binary_int oc2 tabl.(i) done;
  base.data.strings.clear_array ()

let output_name_index_aux fn oc2 base inx dat =
  let module IstrTree =
    Btree.Make
      (struct
        type t = istr
        let compare = Dutil.compare_istr_fun base.data
      end)
  in
  let bt = ref IstrTree.empty in
  for i = 0 to base.data.persons.len - 1 do
    let p = Dutil.poi base (Type.iper_of_int i) in
    List.iter begin fun k ->
      let a = try IstrTree.find k !bt with Not_found -> [] in
      bt := IstrTree.add k (p.key_index :: a) !bt
    end (fn p)
  done;
  (* obsolete table: saved by compatibility with GeneWeb versions <= 4.09,
     i.e. the created database can be still read by these versions but this
     table will not be used in versions >= 4.10 *)
  Mutil.output_value_no_sharing oc2 (!bt : iper list IstrTree.t);
  (* new table created from version >= 4.10 *)
  let oc_dat = Secure.open_out_bin dat in
  let bt2 =
    IstrTree.map
      (fun ipl ->
         let i = pos_out oc_dat in
         output_binary_int oc_dat (List.length ipl);
         List.iter
           (fun ip -> output_binary_int oc_dat (Type.int_of_iper ip)) ipl;
         i)
      !bt
  in
  close_out oc_dat;
  let oc_inx = Secure.open_out_bin inx in
  Mutil.output_value_no_sharing oc_inx (bt2 : int IstrTree.t);
  close_out oc_inx

let output_surname_index oc2 base =
  output_name_index_aux
    begin fun p ->
      split_sname base p.surname
      |> List.map base.func.insert_string
      |> List.cons p.surname
    end
 oc2 base

let output_first_name_index oc2 base =
  output_name_index_aux
    begin fun p ->
      split_fname base p.first_name
      |> List.map base.func.insert_string
      |> List.cons p.first_name
    end
    oc2 base

let gen_output no_patches bname base =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  (try Unix.mkdir bname 0o755 with _ -> ());
  let tmp_base = Filename.concat bname "1base" in
  let tmp_base_acc = Filename.concat bname "1base.acc" in
  let tmp_names_inx = Filename.concat bname "1names.inx" in
  let tmp_names_acc = Filename.concat bname "1names.acc" in
  let tmp_snames_inx = Filename.concat bname "1snames.inx" in
  let tmp_snames_dat = Filename.concat bname "1snames.dat" in
  let tmp_fnames_inx = Filename.concat bname "1fnames.inx" in
  let tmp_fnames_dat = Filename.concat bname "1fnames.dat" in
  let tmp_strings_inx = Filename.concat bname "1strings.inx" in
  let tmp_notes = Filename.concat bname "1notes" in
  let tmp_notes_d = Filename.concat bname "1notes_d" in
  if not no_patches then
    begin
      (* Specialised versions of Database.insert_string
         for performance reasons. *)
      let rev = Hashtbl.create base.data.strings.len in
      for i = 0 to base.data.strings.len - 1 do
        Hashtbl.add rev (base.data.strings.get i) i
      done ;
      let insert_string s =
        if not @@ Hashtbl.mem rev s
        then Hashtbl.add rev s (base.func.insert_string s)
      in
      (* prepare name indices strings *)
      for i = 0 to base.data.persons.len - 1 do
        let p = Dutil.poi base (Type.iper_of_int i) in
        base.data.strings.get p.surname
        |> Mutil.split_sname
        |> List.iter insert_string ;
        base.data.strings.get p.first_name
        |> Mutil.split_fname
        |> List.iter insert_string ;
      done ;
      load_ascends_array base;
      load_unions_array base;
      load_couples_array base;
      load_descends_array base;
      load_strings_array base
    end;
  let oc = Secure.open_out_bin tmp_base in
  let oc_acc = Secure.open_out_bin tmp_base_acc in
  let output_array arrname arr =
    let bpos = pos_out oc in
    if !verbose then Printf.eprintf "*** saving %s array\n" arrname;
    flush stderr;
    arr.output_array oc;
    let epos = Iovalue.output_array_access oc_acc arr.get arr.len bpos in
    if epos <> pos_out oc then count_error epos (pos_out oc)
  in
  begin try
    output_string oc Dutil.magic_gwb;
    output_binary_int oc base.data.persons.len;
    output_binary_int oc base.data.families.len;
    output_binary_int oc base.data.strings.len;
    let array_start_indexes = pos_out oc in
    output_binary_int oc 0;
    output_binary_int oc 0;
    output_binary_int oc 0;
    output_binary_int oc 0;
    output_binary_int oc 0;
    output_binary_int oc 0;
    output_binary_int oc 0;
    Mutil.output_value_no_sharing oc (base.data.bnotes.norigin_file : string);
    let persons_array_pos = pos_out oc in
    if not no_patches then output_array "persons" base.data.persons
    else just_copy bname "persons" oc oc_acc;
    let ascends_array_pos = pos_out oc in
    if not no_patches then () else trace "saving ascends";
    output_array "ascends" base.data.ascends;
    let unions_array_pos = pos_out oc in
    if not no_patches then output_array "unions" base.data.unions
    else just_copy bname "unions" oc oc_acc;
    let families_array_pos = pos_out oc in
    if not no_patches then output_array "families" base.data.families
    else just_copy bname "families" oc oc_acc;
    let couples_array_pos = pos_out oc in
    if not no_patches then output_array "couples" base.data.couples
    else just_copy bname "couples" oc oc_acc;
    let descends_array_pos = pos_out oc in
    if not no_patches then output_array "descends" base.data.descends
    else just_copy bname "descends" oc oc_acc;
    let strings_array_pos = pos_out oc in
    if not no_patches then output_array "strings" base.data.strings
    else just_copy bname "strings" oc oc_acc;
    seek_out oc array_start_indexes;
    output_binary_int oc persons_array_pos;
    output_binary_int oc ascends_array_pos;
    output_binary_int oc unions_array_pos;
    output_binary_int oc families_array_pos;
    output_binary_int oc couples_array_pos;
    output_binary_int oc descends_array_pos;
    output_binary_int oc strings_array_pos;
    base.data.families.clear_array ();
    base.data.descends.clear_array ();
    close_out oc;
    close_out oc_acc;
    if not no_patches then
      begin
        let oc_inx = Secure.open_out_bin tmp_names_inx in
        let oc_inx_acc = Secure.open_out_bin tmp_names_acc in
        let oc2 = Secure.open_out_bin tmp_strings_inx in
        try
          trace "create name index";
          output_binary_int oc_inx 0;
          create_name_index oc_inx oc_inx_acc base;
          base.data.ascends.clear_array ();
          base.data.unions.clear_array ();
          base.data.couples.clear_array ();
          if !save_mem then begin trace "compacting"; Gc.compact () end;
          let surname_or_first_name_pos = pos_out oc_inx in
          trace "create strings of fsname";
          create_strings_of_fsname oc_inx oc_inx_acc base;
          seek_out oc_inx 0;
          output_binary_int oc_inx surname_or_first_name_pos;
          close_out oc_inx;
          close_out oc_inx_acc;
          if !save_mem then begin trace "compacting"; Gc.compact () end;
          trace "create string index";
          output_strings_hash oc2 base;
          if !save_mem then begin trace "compacting"; Gc.compact () end;
          let surname_pos = pos_out oc2 in
          trace "create surname index";
          output_surname_index oc2 base tmp_snames_inx tmp_snames_dat;
          if !save_mem then begin trace "compacting"; Gc.compact () end;
          let first_name_pos = pos_out oc2 in
          trace "create first name index";
          output_first_name_index oc2 base tmp_fnames_inx tmp_fnames_dat;
          seek_out oc2 Mutil.int_size;
          output_binary_int oc2 surname_pos;
          output_binary_int oc2 first_name_pos;
          let s = base.data.bnotes.nread "" RnAll in
          if s = "" then ()
          else
            begin let oc_not = Secure.open_out tmp_notes in
              output_string oc_not s; close_out oc_not
            end;
          close_out oc2;
          List.iter
            (fun f ->
               let s = base.data.bnotes.nread f RnAll in
               let fname = Filename.concat tmp_notes_d (f ^ ".txt") in
               Mutil.mkdir_p (Filename.dirname fname);
               let oc = open_out fname in output_string oc s; close_out oc)
            (List.rev (base.data.bnotes.efiles ()))
        with e ->
          (try close_out oc_inx with _ -> ());
          (try close_out oc_inx_acc with _ -> ());
          (try close_out oc2 with _ -> ());
          raise e
      end;
    trace "ok"
  with e ->
    (try close_out oc with _ -> ());
    (try close_out oc_acc with _ -> ());
    Mutil.remove_file tmp_base;
    Mutil.remove_file tmp_base_acc;
    if not no_patches then
      begin
        Mutil.remove_file tmp_names_inx;
        Mutil.remove_file tmp_names_acc;
        Mutil.remove_file tmp_strings_inx;
        Mutil.remove_dir tmp_notes_d
      end;
    raise e
  end;

  base.func.cleanup () ;
  Mutil.remove_file (Filename.concat bname "base");
  Sys.rename tmp_base (Filename.concat bname "base");
  Mutil.remove_file (Filename.concat bname "base.acc");
  Sys.rename tmp_base_acc (Filename.concat bname "base.acc");
  if not no_patches then
    begin
      Mutil.remove_file (Filename.concat bname "names.inx");
      Sys.rename tmp_names_inx (Filename.concat bname "names.inx");
      Mutil.remove_file (Filename.concat bname "names.acc");
      Sys.rename tmp_names_acc (Filename.concat bname "names.acc");
      Mutil.remove_file (Filename.concat bname "snames.dat");
      Sys.rename tmp_snames_dat (Filename.concat bname "snames.dat");
      Mutil.remove_file (Filename.concat bname "snames.inx");
      Sys.rename tmp_snames_inx (Filename.concat bname "snames.inx");
      Mutil.remove_file (Filename.concat bname "fnames.dat");
      Sys.rename tmp_fnames_dat (Filename.concat bname "fnames.dat");
      Mutil.remove_file (Filename.concat bname "fnames.inx");
      Sys.rename tmp_fnames_inx (Filename.concat bname "fnames.inx");
      Mutil.remove_file (Filename.concat bname "strings.inx");
      Sys.rename tmp_strings_inx (Filename.concat bname "strings.inx");
      Mutil.remove_file (Filename.concat bname "notes");
      if Sys.file_exists tmp_notes then
        Sys.rename tmp_notes (Filename.concat bname "notes");
      if Sys.file_exists tmp_notes_d then
        begin let notes_d = Filename.concat bname "notes_d" in
          Mutil.remove_dir notes_d; Sys.rename tmp_notes_d notes_d
        end;
      Mutil.remove_file (Filename.concat bname "patches");
      Mutil.remove_file (Filename.concat bname "patches~");
      Mutil.remove_file (Filename.concat bname "tstab");
      Mutil.remove_file (Filename.concat bname "tstab_visitor");
      Mutil.remove_file (Filename.concat bname "restrict")
    end

let output = gen_output false
