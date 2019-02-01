
(* Copyright (c) 2006-2007 INRIA *)

open Geneweb
open Gwcomp

(* ******************************************************************** *)
(*  [Fonc] check_magic : string -> in_channel -> unit                   *)
(** [Description] : Vérifie le header du fichier passé en paramètre tel
                    que défini par magic_gwo
    [Args] :
      - fname : nom du fichier.
      - ic : descripteur du fichier.
    [Retour] : Si le header n'est pas compatible, on quite en lançant
               une exception Failure suivie du message d'erreur.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let check_magic fname ic =
  let b = really_input_string ic (String.length magic_gwo) in
  if b <> magic_gwo then
    if String.sub magic_gwo 0 4 = String.sub b 0 4 then
      failwith
        ("\"" ^ fname ^ "\" is a GeneWeb object file, but not compatible")
    else
      failwith
        ("\"" ^ fname ^
         "\" is not a GeneWeb object file, or it is a very old version")

let next_family_fun_templ gwo_list fi =
  let ngwo = List.length gwo_list in
  let run =
    if ngwo < 10 || not !(Mutil.verbose) then fun () -> ()
    else if ngwo < 60 then fun () -> Printf.eprintf "."; flush stderr
    else
      let bar_cnt = ref 0 in
      let run () = ProgrBar.run !bar_cnt ngwo; incr bar_cnt in
      ProgrBar.empty := 'o'; ProgrBar.full := '*'; ProgrBar.start (); run
  in
  let ic_opt = ref None in
  let gwo_list = ref gwo_list in
  fi.Db2link.f_sep_file_inx <- 0;
  fun () ->
    let rec loop () =
      let r =
        match !ic_opt with
          Some ic ->
            begin match
              (try Some (input_value ic : gw_syntax) with End_of_file -> None)
            with
              Some fam -> Some fam
            | None ->
                close_in ic;
                ic_opt := None;
                fi.Db2link.f_sep_file_inx <- fi.Db2link.f_sep_file_inx + 1;
                None
            end
        | None -> None
      in
      match r with
        Some fam -> Some fam
      | None ->
          match !gwo_list with
            (x, separate, _) :: rest ->
              run ();
              gwo_list := rest;
              let ic = open_in_bin x in
              check_magic x ic;
              fi.Db2link.f_curr_src_file <- input_value ic;
              fi.Db2link.f_curr_gwo_file <- x;
              fi.Db2link.f_separate <- separate;
              fi.Db2link.f_has_separates <-
                fi.Db2link.f_has_separates || separate;
              ic_opt := Some ic;
              loop ()
          | [] ->
              if ngwo < 10 || not !(Mutil.verbose) then ()
              else if ngwo < 60 then
                begin Printf.eprintf "\n"; flush stderr end
              else ProgrBar.finish ();
              None
    in
    loop ()

let just_comp = ref false
let out_file = ref (Filename.concat Filename.current_dir_name "a")
let force = ref false

let separate = ref false
let shift = ref 0
let files = ref []

(* ******************************************************************** *)
(*  [Var] speclist : (string * Arg.spec * string) list                  *)
(** [Description] : Positionne les variables en fonction des options
                    données à gwc2
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let speclist =
  ["-c", Arg.Set just_comp, "Only compiling";
   "-o", Arg.String (fun s -> out_file := s),
   "<file> Output database (default: a.gwb)";
   "-f", Arg.Set force, " Remove database if already existing";
   "-stats", Arg.Set Db2link.pr_stats, "Print statistics";
   "-nc", Arg.Clear Db2link.do_check, "No consistency check";
   "-cg", Arg.Set Db2link.do_consang, "Compute consanguinity";
   "-sep", Arg.Set separate, " Separate all persons in next file";
   "-sh", Arg.Int (fun x -> shift := x),
   "<int> Shift all persons numbers in next files";
   "-ds", Arg.String (fun s -> Db2link.default_source := s), "\
     <str> Set the source field for persons and families without source data";
   "-part", Arg.String (fun s -> Db2link.particules_file := s), "\
     <file> Particles file (default = predefined particles)";
   "-mem", Arg.Unit (fun () -> ()), " (obsolete option)";
   "-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.";
   "-nofail", Arg.Set Gwcomp.no_fail, " no failure in case of error.";
   "-nopicture", Arg.Set Gwcomp.no_picture,
   " do not create associative pictures";
   "-q", Arg.Clear Mutil.verbose, " no verbose";
   "-v", Arg.Set Mutil.verbose, " verbose"]

let anonfun x =
  let sep = !separate in
  if Filename.check_suffix x ".gw" then ()
  else if Filename.check_suffix x ".gwo" then ()
  else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\""));
  separate := false;
  files := (x, sep, !shift) :: !files

let errmsg =
  "Usage: gwc2 [options] [files]\n\
   where [files] are a list of files:\n  \
   source files end with .gw\n  \
   object files end with .gwo\n\
   and [options] are:"

(* ******************************************************************** *)
(*  [Fonc] main : unit -> unit                                          *)
(** [Description] : Fonction principale de création d'une base au
                    format gwb2.
    [Args] : Néant.
    [Retour] : Néant.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let main () =
  Mutil.verbose := false;
  Argl.parse speclist anonfun errmsg;
  Secure.set_base_dir (Filename.dirname !out_file);
  let gwo = ref [] in
  List.iter
    (fun (x, separate, shift) ->
       if Filename.check_suffix x ".gw" then
         begin
           begin try Gwcomp.comp_families x with
             e -> Printf.printf "File \"%s\", line %d:\n" x !line_cnt; raise e
           end;
           gwo := (x ^ "o", separate, shift) :: !gwo
         end
       else if Filename.check_suffix x ".gwo" then
         gwo := (x, separate, shift) :: !gwo
       else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\"")))
    (List.rev !files);
  if not !just_comp then
    let bdir =
      if Filename.check_suffix !out_file ".gwb" then !out_file
      else !out_file ^ ".gwb"
    in
    if not !force && Sys.file_exists bdir then
      begin
        Printf.printf "The database \"%s\" already exists. \
                Use option -f to overwrite it."
          !out_file;
        flush stdout;
        exit 2
      end;
      Lock.control (Mutil.lock_file !out_file) false
        ~onerror:Lock.print_error_and_exit
        (fun () ->
           let bdir =
             if Filename.check_suffix !out_file ".gwb" then !out_file
             else !out_file ^ ".gwb"
           in
           let next_family_fun = next_family_fun_templ (List.rev !gwo) in
           if Db2link.link next_family_fun bdir then ()
           else
             begin
               Printf.eprintf "*** database not created\n";
               flush stderr;
               exit 2
             end)

let print_exc =
  function
    Failure txt -> Printf.printf "Failed: %s\n" txt; flush stdout; exit 2
  | exc -> Printexc.print raise exc

let _ = try main () with exc -> print_exc exc
