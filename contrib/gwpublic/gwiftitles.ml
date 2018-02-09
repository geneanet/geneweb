(* camlp5r ../../src/pa_lock.cmo *)
(* $Id: public.ml,v 4.26 2007/01/19 09:03:02 deraugla Exp $ *)

open Def;
open Gwdb;
open Printf;


value private_everybody bname =
  let base = Gwdb.open_base bname in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      if get_access p <> IfTitles then
        let p = {(gen_person_of_person p) with access = IfTitles} in
        patch_person base p.key_index p
      else ();
    };
    commit_patches base;
  }
;


value private_some bname key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
  [ [ip] ->
      let p = poi base ip in
      do {
         if get_access p <> IfTitles then
         let p = {(gen_person_of_person p) with access = IfTitles} in
         patch_person base p.key_index p
         else ();
         commit_patches base;
      }
  | _ ->
      match Gutil.person_of_string_dot_key base key with
      [ Some ip ->
          let p = poi base ip in
          do {
             if get_access p <> Private then
             let p = {(gen_person_of_person p) with access = Private} in
             patch_person base p.key_index p
             else ();
             commit_patches base;
          }
      | None ->
          do {
            Printf.eprintf "Bad key %s\n" key;
            flush stderr;
            exit 2
          } ] ]
;

value ind = ref "";
value bname = ref "";
value everybody = ref False;

value speclist =
   [("-everybody", Arg.Set everybody, "set flag iftitles to everybody [lent!]");
   ("-ind", Arg.String (fun x -> ind.val := x),
    "individual key")]
;
value anonfun i = bname.val := i;
value usage = "Usage: gwiftitles [-everybody] [-ind key] base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    lock (Mutil.lock_file bname.val) with
    [ Accept ->
        if everybody.val then private_everybody bname.val
        else private_some bname.val ind.val
    | Refuse -> do {
        eprintf "Base is locked. Waiting... ";
        flush stderr;
        lock_wait (Mutil.lock_file bname.val) with
        [ Accept -> do {
            eprintf "Ok\n";
            flush stderr;
            if everybody.val then private_everybody bname.val
            else private_some bname.val ind.val
          }
        | Refuse -> do {
            printf "\nSorry. Impossible to lock base.\n";
            flush stdout;
            exit 2
          } ]
    } ];
  }
;

main ();
