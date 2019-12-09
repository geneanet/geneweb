(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util
open MergeIndOk

let print_merge conf base =
  match p_getenv conf.env "i1", p_getenv conf.env "i2" with
    Some i1, Some i2 ->
      let p1 = poi base (iper_of_string i1) in
      let p2 = poi base (iper_of_string i2) in
      let p = reconstitute conf base p1 p2 in
      let sp = UpdateInd.string_person_of base p1 in
      let digest = Update.digest_person sp in
      UpdateInd.print_update_ind conf base p digest
  | _ -> Hutil.incorrect_request conf

let print_mod_merge_ok conf base wl p pgl1 ofn1 osn1 oocc1 pgl2 ofn2 osn2 oocc2 =
  let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "merge done")) in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "\n%s\n"
    (referenced_person_text conf base (poi base p.key_index));
  Update.print_warnings conf base wl;
  let pi = p.key_index in
  let np = poi base pi in
  let nfn = p_first_name base np in
  let nsn = p_surname base np in
  let nocc = get_occ np in
  if ((ofn1 <> nfn || osn1 <> nsn || oocc1 <> nocc) && pgl1 <> [] ||
      (ofn2 <> nfn || osn2 <> nsn || oocc2 <> nocc) && pgl2 <> []) then
    begin
      Wserver.printf
        "<div class='alert alert-danger mx-auto mt-1' role='alert'>\n";
      Wserver.printf (ftransl conf "name changed. update linked pages");
      Wserver.printf "</div>\n";
      let snocc = if nocc <> 0 then Printf.sprintf "/%d" nocc else "" in
      Wserver.printf "<span class=\"unselectable float-left\">%s%s</span>\n\
                      <span class=\"float-left ml-1\">%s/%s%s</span>\n<br>"
        (Utf8.capitalize (transl conf "new name")) (transl conf ":") nfn nsn snocc;
      let soocc1 = if oocc1 <> 0 then Printf.sprintf "/%d" oocc1 else "" in
      Wserver.printf "<span class=\"unselectable float-left\">%s 1%s</span>\n\
                      <span class=\"float-left ml-1\">%s/%s%s</span>\n<br>"
        (Utf8.capitalize (transl conf "old name")) (transl conf ":") ofn1 osn1 soocc1;
      Wserver.printf "<span>%s%s</span>"
        (Utf8.capitalize (transl conf "linked pages")) (transl conf ":");
      NotesDisplay.print_linked_list conf base pgl1;
      let soocc2 = if oocc2 <> 0 then Printf.sprintf "/%d" oocc2 else "" in
      Wserver.printf "<span class=\"unselectable float-left\">%s 2%s</span>\n\
                      <span class=\"float-left ml-1\">%s/%s%s</span>\n<br>"
        (Utf8.capitalize (transl conf "old name")) (transl conf ":") ofn2 osn2 soocc2;
      Wserver.printf "<span>%s%s</span>"
        (Utf8.capitalize (transl conf "linked pages")) (transl conf ":");
      NotesDisplay.print_linked_list conf base pgl2
    end;

  MergeDisplay.print_possible_continue_merging conf base;
  Hutil.trailer conf

let print_mod_merge o_conf base =
  let get_gen_person i =
    match p_getenv o_conf.env i with
      Some i ->
        Util.string_gen_person base
          (gen_person_of_person (poi base (iper_of_string i)))
    | None ->
        Util.string_gen_person base
          (gen_person_of_person (poi base dummy_iper))
  in
  let o_p1 = get_gen_person "i" in
  let o_p2 = get_gen_person "i2" in
  let conf = Update.update_conf o_conf in
  UpdateIndOk.print_mod_aux conf base
    (fun p -> effective_mod_merge conf base o_p1 o_p2 p print_mod_merge_ok)
