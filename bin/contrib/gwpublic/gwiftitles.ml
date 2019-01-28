open Geneweb
open Def
open Gwdb

let private_everybody bname =
  let base = Gwdb.open_base bname in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    if get_access p <> IfTitles then
      let p = {(gen_person_of_person p) with access = IfTitles} in
      patch_person base p.key_index p
  done;
  commit_patches base

let private_some bname key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
    [ip] ->
      let p = poi base ip in
      if get_access p <> IfTitles then
        begin let p = {(gen_person_of_person p) with access = IfTitles} in
          patch_person base p.key_index p
        end;
      commit_patches base
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

let ind = ref ""
let bname = ref ""
let everybody = ref false

let speclist =
  ["-everybody", Arg.Set everybody, "set flag iftitles to everybody [lent!]";
   "-ind", Arg.String (fun x -> ind := x), "individual key"]
let anonfun i = bname := i
let usage = "Usage: gwiftitles [-everybody] [-ind key] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry
    (Mutil.lock_file !bname) ~onerror:Lock.print_error_and_exit @@ fun () ->
  if !everybody then private_everybody !bname
  else private_some !bname !ind

let _ = main ()
