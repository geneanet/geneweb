open Geneweb
open Gwdb
open Config

let lower_fst =
  let b = Buffer.create 6 in
  fun s ->
    Buffer.reset b ;
    let _, i0, _ = Utf8.C.unaccent false s 0 (String.length s) in
    let u = Utf8.C.cp s i0 in
    begin match Uucp.Case.Map.to_lower u with
      | `Self -> Uutf.Buffer.add_utf_8 b u
      | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us
    end ;
    Buffer.contents b

let list_ind_file conf =
  Filename.concat (Util.base_path [] (conf.bname ^ ".gwb")) @@
  if conf.wizard || conf.friend then "cache_list_ind_friend" else "cache_list_ind_visitor"

let is_cache_iper_inorder_uptodate conf base =
  let cache_path = list_ind_file conf in
  try
    let cache_stat = Unix.stat (cache_path) in
    let cache_timeof_modif = cache_stat.Unix.st_mtime in
    let base_timeof_modif = Gwdb.date_of_last_change base in
    (base_timeof_modif < cache_timeof_modif)
  with
    Unix.Unix_error _ -> false

let build_cache_iper_inorder progress conf base =
  let module PerSet =
    Set.Make (struct
      type t = (string * istr * int * iper)
      let compare (s1, f1, o1, _) (s2, f2, o2, _) =
        match Utf8.compare s1 s2 with
        | 0 -> begin
            match Utf8.compare (Gwdb.sou base f1) (Gwdb.sou base f2) with
            | 0 -> compare o1 o2
            | x -> x
          end
        | x -> x
    end)
  in
  Gwdb.load_persons_array base ;
  Gwdb.load_strings_array base ;
  let (_, set) =
    let len = Gwdb.nb_of_persons base in
    let per = len / 100 in
    Gwdb.Collection.fold begin fun (n, set) p ->
      if n mod per = 0 then progress "1" (n / per) ;
      (* FIXME: stop checking is_empty_name when possible *)
      if (Util.is_empty_name p) || not (Util.authorized_age conf base p) then (n + 1, set)
      else (n + 1, PerSet.add (Util.name_key base (sou base @@ get_surname p)
                              , get_first_name p
                              , get_occ p
                              , get_iper p) set)
    end (0, PerSet.empty) (Gwdb.persons base)
  in
  progress "1" 100 ;
  Gwdb.clear_persons_array base ;
  Gwdb.clear_strings_array base ;
  let cache_filename = list_ind_file conf in
  let cnt = PerSet.cardinal set in
  let _, letters =
    PerSet.fold begin fun (x, _, _, _) (idx, acc) ->
      let c = lower_fst x in
      if List.mem_assoc c acc then (succ idx, acc) else (succ idx, (c, idx) :: acc)
    end set (0, [])
  in
  Geneweb.Lock.control
    (cache_filename ^ ".lock") true ~onerror:Lock.print_try_again @@ fun () ->
  let letters = List.rev letters in
  let oc = Secure.open_out_bin cache_filename in
  output_binary_int oc cnt;
  output_value oc letters;
  let m1 = pos_out oc in
  PerSet.iter (fun _ -> output_binary_int oc 0) set ;
  let m2 = pos_out oc in
  let per = cnt / 100 in
  ignore @@
  PerSet.fold begin fun (_, _, _, i) (c, m1, m2) ->
    if c mod per = 0 then progress "2" (c / per) ;
    seek_out oc m1 ;
    output_binary_int oc m2 ;
    let m1 = pos_out oc in
    seek_out oc m2 ;
    output_value oc i ;
    let m2 = pos_out oc in
    (c + 1, m1, m2)
  end set (0, m1, m2) ;
  progress "2" 100 ;
  close_out oc

let read_cache_iper_inorder conf page page_size =
  let cache_filename = list_ind_file conf in
  let ic = Secure.open_in_bin cache_filename in
  let person_count = input_binary_int ic in
  let page_count = person_count / page_size + if person_count mod page_size == 0 then 0 else 1 in
  let first_letters : (string * int) list = input_value ic in
  let page = min (page_count - 1) @@ max 0 page in
  let first_idx = page * page_size in
  seek_in ic (pos_in ic + (first_idx * 4));
  let iper_offset = input_binary_int ic in
  seek_in ic iper_offset;
  let page_size = if page_size * (page + 1) > person_count
    then person_count - (page_size * page)
    else page_size
  in
  let ipers = Array.make page_size Gwdb.dummy_iper in
  for i = 0 to page_size - 1 do
    Array.unsafe_set ipers i @@ input_value ic
  done ;
  close_in ic ;
  page_count, first_letters, ipers, page
