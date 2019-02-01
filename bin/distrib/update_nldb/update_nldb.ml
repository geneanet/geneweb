(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Def
open Gwdb

let fname = ref ""

let errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>"
let speclist = []
let anonfun s =
  if !fname = "" then fname := s
  else raise (Arg.Bad "Cannot treat several databases")

let notes_links s =
  let slen = String.length s in
  let rec loop list_nt list_ind pos i =
    if i = slen then list_nt, list_ind
    else if i + 1 < slen && s.[i] = '%' then loop list_nt list_ind pos (i + 2)
    else
      match NotesLinks.misc_notes_link s i with
        NotesLinks.WLpage (j, _, lfname, _, _) ->
          let list_nt =
            if List.mem lfname list_nt then list_nt else lfname :: list_nt
          in
          loop list_nt list_ind pos j
      | NotesLinks.WLperson (j, key, _, text) ->
          let list_ind =
            let link = {NotesLinks.lnTxt = text; NotesLinks.lnPos = pos} in
            (key, link) :: list_ind
          in
          loop list_nt list_ind (pos + 1) j
      | NotesLinks.WLwizard (j, _, _) -> loop list_nt list_ind (pos + 1) j
      | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1)
  in
  loop [] [] 1 0

let read_file_contents fname =
  match try Some (open_in fname) with Sys_error _ -> None with
    Some ic ->
      let len = ref 0 in
      begin try
        let rec loop () = len := Buff.store !len (input_char ic); loop () in
        loop ()
      with End_of_file -> Buff.get !len
      end
  | None -> ""

let compute base bdir =
  let bdir =
    if Filename.check_suffix bdir ".gwb" then bdir else bdir ^ ".gwb"
  in
  let nb_ind = nb_of_persons base in
  let nb_fam = nb_of_families base in
  Printf.eprintf "--- database notes\n";
  flush stderr;
  let list = notes_links (base_notes_read base "") in
  if list = ([], []) then ()
  else (let pg = NotesLinks.PgNotes in NotesLinks.update_db bdir pg list);
  Printf.eprintf "--- wizard notes\n";
  flush stderr;
  begin try
    let files = Sys.readdir (Filename.concat bdir (base_wiznotes_dir base)) in
    for i = 0 to Array.length files - 1 do
      let file = files.(i) in
      if Filename.check_suffix file ".txt" then
        let wizid = Filename.chop_suffix file ".txt" in
        let wfile =
          List.fold_left Filename.concat bdir [base_wiznotes_dir base; file]
        in
        let list = notes_links (read_file_contents wfile) in
        if list = ([], []) then ()
        else
          begin
            Printf.eprintf "%s... " wizid;
            flush stderr;
            let pg = NotesLinks.PgWizard wizid in
            NotesLinks.update_db bdir pg list
          end
    done;
    Printf.eprintf "\n";
    flush stderr
  with Sys_error _ -> ()
  end;
  let db = ref (NotesLinks.read_db bdir) in
  Printf.eprintf "--- misc notes\n";
  flush stderr;
  let ndir = Filename.concat bdir (base_notes_dir base) in
  let rec loop dir name =
    try
      let cdir = Filename.concat ndir dir in
      let files = Sys.readdir cdir in
      for i = 0 to Array.length files - 1 do
        let file = files.(i) in
        if Filename.check_suffix file ".txt" then
          let fnotes = Filename.chop_suffix file ".txt" in
          let file = Filename.concat dir fnotes in
          let list = notes_links (base_notes_read base file) in
          if list = ([], []) then ()
          else
            let fnotes =
              if name = "" then fnotes
              else Printf.sprintf "%s%c%s" name NotesLinks.char_dir_sep fnotes
            in
            Printf.eprintf "%s...\n" fnotes;
            flush stderr;
            let pg = NotesLinks.PgMisc fnotes in
            db := NotesLinks.add_in_db !db pg list
        else
          loop (Filename.concat dir file)
            (if name = "" then file
             else Printf.sprintf "%s%c%s" name NotesLinks.char_dir_sep file)
      done;
      flush stderr
    with Sys_error _ -> ()
  in
  loop Filename.current_dir_name "";
  Printf.eprintf "--- individual notes\n";
  flush stderr;
  ProgrBar.full := '*';
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let s =
      let sl =
        [get_notes p; get_occupation p; get_birth_note p; get_birth_src p;
         get_baptism_note p; get_baptism_src p; get_death_note p;
         get_death_src p; get_burial_note p; get_burial_src p; get_psources p]
      in
      let sl =
        let rec loop l accu =
          match l with
            [] -> accu
          | evt :: l -> loop l (evt.epers_note :: evt.epers_src :: accu)
        in
        loop (get_pevents p) sl
      in
      String.concat " " (List.map (sou base) sl)
    in
    let list = notes_links s in
    if list = ([], []) then ()
    else
      begin let pg = NotesLinks.PgInd (Adef.iper_of_int i) in
        db := NotesLinks.add_in_db !db pg list
      end;
    ProgrBar.run i nb_ind
  done;
  ProgrBar.finish ();
  Printf.eprintf "--- families notes\n";
  flush stderr;
  ProgrBar.full := '*';
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do
    let fam = foi base (Adef.ifam_of_int i) in
    if not (is_deleted_family fam) then
      begin let s =
        let sl =
          [get_comment fam; get_fsources fam; get_marriage_note fam;
           get_marriage_src fam]
        in
        let sl =
          let rec loop l accu =
            match l with
              [] -> accu
            | evt :: l -> loop l (evt.efam_note :: evt.efam_src :: accu)
          in
          loop (get_fevents fam) sl
        in
        String.concat " " (List.map (sou base) sl)
      in
        let list = notes_links s in
        if list = ([], []) then ()
        else
          let pg = NotesLinks.PgFam (Adef.ifam_of_int i) in
          db := NotesLinks.add_in_db !db pg list
      end;
    ProgrBar.run i nb_fam
  done;
  ProgrBar.finish ();
  NotesLinks.write_db bdir !db

let main () =
  Argl.parse speclist anonfun errmsg;
  if !fname = "" then
    begin
      Printf.eprintf "Missing database name\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2
    end;
  Secure.set_base_dir (Filename.dirname !fname);
  let base = Gwdb.open_base !fname in
  Sys.catch_break true;
  let () = load_strings_array base in
  let () = load_unions_array base in
  try compute base !fname with
    Sys.Break -> Printf.eprintf "\n"; flush stderr; ()

let _ = Printexc.print main ()
