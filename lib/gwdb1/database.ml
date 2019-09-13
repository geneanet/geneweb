(* $Id: database.ml,v 5.19 2007-06-06 15:22:35 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk
open Def
open Type

type person = dsk_person
type ascend = dsk_ascend
type union = dsk_union
type family = dsk_family
type couple = dsk_couple
type descend = dsk_descend

(*
 Files in base (directory .gwb)

    base - the base itself
       magic number (magic_gwb)                 : string of length 8
       number of persons                        : binary_int
       number of families                       : binary_int
       number of strings                        : binary_int
       persons array offset in file             : binary_int
       ascends array offset in file             : binary_int
       unions array offset in file              : binary_int
       families array offset in file            : binary_int
       couples array offset in file             : binary_int
       descends array offset in file            : binary_int
       strings array offset in file             : binary_int
       notes origin file                        : value
       persons array                            : value
       ascends array                            : value
       unions array                             : value
       families array                           : value
       couples array                            : value
       descends array                           : value
       strings array                            : value

    base.acc - direct accesses to arrays inside base
       persons offsets   : array of binary_ints
       ascends offsets   : array of binary_ints
       unions offsets    : array of binary_ints
       families offsets  : array of binary_ints
       couples offsets   : array of binary_ints
       descends offsets  : array of binary_ints
       strings offsets   : array of binary_ints

    names.inx - index for names, strings of first names and surnames
       offset to 2nd index : binary_int
       1st index (names) : value
         array, length = "table_size", associating:
          - a hash value of a "crushed" (module "Name") name (modulo length)
          - to the array of indexes of the corresponding persons
       2nd index (first names and surnames strings) : value
         array, length = "table_size", associating:
          - a hash value of the "crushed" (module "Name") first name or
            surname (modulo length)
          - to the array of the corresponding string indexes

    names.acc - direct accesses to arrays inside names.inx

    strings.inx - index for strings, surnames, first names
       length of the strings offset array : binary_int
       unused (legacy)                    : binary_int
       unused (legacy)                    : binary_int
       strings offset array (length = prime after 10 * strings array length)
         - associating a hash value of the string modulo length
         - to its index in the string array
       strings list array (length = string array length)
         - associating a string index
         - to the index of the next index holding the same hash value

    snames.inx - index for surnames
       binary tree
        - associating the string index of a surname
        - to a pointer (int) to snames.dat

    snames.dat - data associated with snames.inx
      table of list of persons holding a surname

    fnames.inx - index for first names
       binary tree
        - associating the string index of a first name
        - to a pointer (int) to fnames.dat

    fnames.dat - data associated with fnames.inx
      table of list of persons holding a first name

the corresponding list of persons holding this surname

    patches - patches
       When updated, none of the previous files are modified. Only this one
       is written and rewritten. It holds a record of type "patches", composed
       of association lists "index" - "new value".
*)

exception Found of int

let split_sname strings i = Mutil.split_sname @@ strings.get i

let split_fname strings i = Mutil.split_fname @@ strings.get i

let hashtbl_right_assoc s ht =
  try
    Hashtbl.iter (fun i1 s1 -> if s = s1 then raise (Found i1)) ht;
    raise Not_found
  with Found x -> x

let index_of_string strings ic start_pos hash_len string_patches s =
  try hashtbl_right_assoc s string_patches
  with Not_found -> match ic, hash_len with
    | Some ic, Some hash_len ->
      let ia = Hashtbl.hash s mod hash_len in
      seek_in ic (start_pos + ia * Mutil.int_size);
      let i1 = input_binary_int ic in
      let rec loop i =
        if i = -1 then raise Not_found
        else if strings.get i = s then i
        else
          begin
            seek_in ic (start_pos + (hash_len + i) * Mutil.int_size);
            loop (input_binary_int ic)
          end
      in
      loop i1
    | _ -> raise Not_found

(* /!\ Keep it sync with Outbase.name_index_key /!\ *)
let name_index_key s =
  Hashtbl.hash (Name.crush_lower (Name.abbrev s)) mod Dutil.table_size

let persons_of_first_name_or_surname base_data strings persons params =
  let (_
      , _
      , proj
      , (rm : (istr, iper list) Hashtbl.t)
      , (add : (istr, iper list) Hashtbl.t)
      , names_inx
      , names_dat
      , bname) = params in
  let module IstrTree =
    Btree.Make
      (struct
        type t = istr
        let compare = Dutil.compare_istr_fun base_data
      end)
  in
  let fname_dat = Filename.concat bname names_dat in
  let bt =
    lazy begin
      let fname_inx = Filename.concat bname names_inx in
      let ic_inx = Secure.open_in_bin fname_inx in
      let bt : int IstrTree.t = input_value ic_inx in
      close_in ic_inx;
      bt
    end
  in
  let find istr =
    let add : iper list = try Hashtbl.find add istr with Not_found -> [] in
    let rm : iper list = try Hashtbl.find rm istr with Not_found -> [] in
    let ipera =
      try
        let pos = IstrTree.find istr @@ Lazy.force bt in
        let ic_dat = Secure.open_in_bin fname_dat in
        seek_in ic_dat pos;
        let len = input_binary_int ic_dat in
        let rec read_loop ipera len =
          if len = 0 then ipera
          else
            let iper = input_binary_int ic_dat in
            if List.mem iper rm
            then read_loop ipera (len - 1)
            else read_loop (iper :: ipera) (len - 1)
        in
        let ipera = read_loop [] len in
        close_in ic_dat;
        ipera
      with Not_found -> []
    in
    List.fold_left
      begin fun acc i ->
        let () = print_endline @@ Printf.sprintf "%s: %d" __LOC__ i in
        if not (List.mem i acc) then
          let proj = proj (persons.get i) in
          if List.mem istr proj
          then i :: acc
          else acc
        else acc
      end
      ipera add
  in
  let cursor str =
    IstrTree.key_after
      (fun key -> Dutil.compare_names base_data str (strings.get key))
      (Lazy.force bt)
  in
  let next key = IstrTree.next key (Lazy.force bt) in
  {find = find; cursor = cursor; next = next}

(* Search index for a given name in file names.inx *)

let persons_of_name bname rm add =
  let t = ref None in
  fun s ->
    let i = name_index_key s in
    let ai =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          seek_in ic_inx_acc (Iovalue.sizeof_long * i);
          let pos = input_binary_int ic_inx_acc in
          close_in ic_inx_acc;
          seek_in ic_inx pos;
          (Iovalue.input ic_inx : iper array)
        else
          let a =
            match !t with
              Some a -> a
            | None ->
              seek_in ic_inx Mutil.int_size;
              let a : Dutil.name_index_data = input_value ic_inx in
              t := Some a;
              a
          in
          a.(i)
      in
      close_in ic_inx; ai
    in
    let add = try Hashtbl.find add i with Not_found -> [] in
    let rm = try Hashtbl.find rm i with Not_found -> [] in
    Array.to_list ai
    |> List.filter (fun i -> not @@ List.mem i rm)
    |> List.rev_append add

let strings_of_fsname bname patch_string_keys =
  let t = ref None in
  fun s ->
    let i = name_index_key s in
    let ai =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          seek_in ic_inx_acc (Iovalue.sizeof_long * (Dutil.table_size + i));
          let pos = input_binary_int ic_inx_acc in
          close_in ic_inx_acc;
          seek_in ic_inx pos;
          (Iovalue.input ic_inx : istr array)
        else
          let a =
            match !t with
            | Some a -> a
            | None ->
              let pos = input_binary_int ic_inx in
              seek_in ic_inx pos;
              let a : Dutil.strings_of_fsname = input_value ic_inx in
              t := Some a;
              a
          in
          a.(i)
      in
      close_in ic_inx ; ai
    in
    try
      List.rev_append
        (Hashtbl.find patch_string_keys i)
        (Array.to_list ai)
    with Not_found -> Array.to_list ai

(**)

(* Restrict file *)

type visible_state = VsNone | VsTrue | VsFalse

let verbose = Mutil.verbose

let make_visible_record_access bname persons =
  let visible_ref = ref None in
  let fname = Filename.concat bname "restrict" in
  let read_or_create_visible () =
    let visible =
      try
        let ic = Secure.open_in fname in
        if Sys.unix && !verbose then
          begin
            Printf.eprintf "*** read restrict file\n";
            flush stderr
          end ;
        let visible = input_value ic in
        close_in ic ;
        visible
      with Sys_error _ -> Array.make persons.len VsNone
    in
    visible_ref := Some visible ;
    visible
  in
  let v_write () =
    match !visible_ref with
      Some visible ->
        begin try
          let oc = Secure.open_out fname in
          if Sys.unix && !verbose then
            begin
              Printf.eprintf "*** write restrict file\n";
              flush stderr
            end;
          output_value oc visible;
          close_out oc
          with Sys_error _ -> ()
        end
    | None -> ()
  in
  let v_get fct i =
    let visible =
      match !visible_ref with
        Some visible -> visible
      | None -> read_or_create_visible ()
    in
    if i < Array.length visible then
      match visible.(i) with
        VsNone ->
          let status = fct (persons.get i) in
          visible.(i) <- if status then VsTrue else VsFalse;
          visible_ref := Some visible;
          status
      | VsTrue -> true
      | VsFalse -> false
    else fct (persons.get i)
  in
  {v_write = v_write; v_get = v_get}

(*
   Synchro:
     - synchro_person contient la liste des ip des personnes patchées.
     - synchro_family contient la liste des ifam des familles patchées.
     - synchro_patch contient :
         * le timestamp de la modification
         * la liste des personnes modifiées
         * la liste des familles modifiées
*)
let synchro_person = ref []
let synchro_family = ref []
type synchro_patch =
  { mutable synch_list : (string * int list * int list) list }

(* Input *)

let apply_patches tab f patches plen =
  if plen = 0 then tab
  else
    let new_tab =
      if plen > Array.length tab then
        let new_tab = Array.make plen (Obj.magic 0) in
        Array.blit tab 0 new_tab 0 (Array.length tab); new_tab
      else tab
    in
    Hashtbl.iter (fun i v -> new_tab.(i) <- f v) patches; new_tab

type patches_ht =
  { h_person : int ref * (int, person) Hashtbl.t;
    h_ascend : int ref * (int, ascend) Hashtbl.t;
    h_union : int ref * (int, union) Hashtbl.t;
    h_family : int ref * (int, family) Hashtbl.t;
    h_couple : int ref * (int, couple) Hashtbl.t;
    h_descend : int ref * (int, descend) Hashtbl.t;
    h_string : int ref * (int, string) Hashtbl.t;
    h_name_add : (int, iper list) Hashtbl.t;
    h_name_rm : (int, iper list) Hashtbl.t;
    h_name_add_key : (int, iper list) Hashtbl.t;
    h_name_rm_key : (int, iper list) Hashtbl.t;
    h_string_key : (int, istr list) Hashtbl.t;
  }

(* Old structures of file "patches", kept for backward compatibility.
   After conversion, a new change will be saved with a magic number
   (magic_patch) and a record "patch_ht" above. *)

module Old = struct
  type patches =
    { p_person : (int * person) list ref
    ; p_ascend : (int * ascend) list ref
    ; p_union : (int * union) list ref
    ; p_family : (int * family) list ref
    ; p_couple : (int * couple) list ref
    ; p_descend : (int * descend) list ref
    ; p_string : (int * string) list ref
    ; p_name : (int * iper list) list ref
    }
end

module OldGnPa0001 = struct
  type patches =
      { h_person : int ref * (int, person) Hashtbl.t
      ; h_ascend : int ref * (int, ascend) Hashtbl.t
      ; h_union : int ref * (int, union) Hashtbl.t
      ; h_family : int ref * (int, family) Hashtbl.t
      ; h_couple : int ref * (int, couple) Hashtbl.t
      ; h_descend : int ref * (int, descend) Hashtbl.t
      ; h_string : int ref * (int, string) Hashtbl.t
      ; h_name : (int, iper list) Hashtbl.t
      }
end

let phony_person =
  {first_name = 0; surname = 0; occ = 0; image = 0; first_names_aliases = [];
   surnames_aliases = []; public_name = 0; qualifiers = []; aliases = [];
   titles = []; rparents = []; related = []; occupation = 0; sex = Neuter;
   access = IfTitles; birth = Adef.cdate_None; birth_place = 0;
   birth_note = 0; birth_src = 0; baptism = Adef.cdate_None;
   baptism_place = 0; baptism_note = 0; baptism_src = 0;
   death = DontKnowIfDead; death_place = 0; death_note = 0; death_src = 0;
   burial = UnknownBurial; burial_place = 0; burial_note = 0; burial_src = 0;
   pevents = []; notes = 0; psources = 0; key_index = Type.iper_of_int 0}

let phony_family =
  {marriage = Adef.cdate_None; marriage_place = 0; marriage_note = 0;
   marriage_src = 0; witnesses = [| |]; relation = Married;
   divorce = NotDivorced; fevents = []; comment = 0; origin_file = 0;
   fsources = 0; fam_index = Type.ifam_of_int 0}

let ext phony v =
  let rlen = Array.length (Obj.magic v) in
  let alen = Array.length (Obj.magic phony) in
  if rlen = alen then v
  else if rlen < alen then
    let x = Array.copy (Obj.magic phony) in
    Array.blit (Obj.magic v) 0 x 0 rlen; Obj.magic x
  else failwith "this is a GeneWeb base, but not compatible; please upgrade"

let array_ext phony fa =
  let a = Obj.magic fa in
  if Array.length a = 0 then fa
  else
    let rlen = Array.length a.(0) in
    let alen = Array.length (Obj.magic phony) in
    if rlen = alen then fa
    else if rlen < alen then
      begin
        if Sys.unix then
          if !verbose then
            begin
              Printf.eprintf "*** extending records from size %d to size %d\n"
                rlen alen;
              flush stderr
            end;
        for i = 0 to Array.length a - 1 do
          let x = Array.copy (Obj.magic phony) in
          Array.blit a.(i) 0 x 0 rlen; a.(i) <- x
        done;
        fa
      end
    else failwith "this is a GeneWeb base, but not compatible; please upgrade"

let make_record_access ic ic_acc shift array_pos (plenr, patches) len name
    input_array input_item =
  let v_ext v =
    if name = "persons" then ext phony_person v
    else if name = "families" then ext phony_family v
    else v
  in
  let v_arr_ext v =
    if name = "persons" then array_ext phony_person v
    else if name = "families" then array_ext phony_family v
    else v
  in
  let tab = ref None in
  let cleared = ref false in
  let gen_get i =
    match !tab with
      Some x -> x.(i)
    | None ->
        try let v = Hashtbl.find patches i in v_ext v with
          Not_found ->
            if i < 0 || i >= len then
              failwith
                ("access " ^ name ^ " out of bounds; i = " ^ string_of_int i)
            else
              match ic_acc with
                Some ic_acc ->
                  seek_in ic_acc (shift + Iovalue.sizeof_long * i);
                  let pos = input_binary_int ic_acc in
                  seek_in ic pos; let v = input_item ic in v_ext v
              | None ->
                  Printf.eprintf "Sorry; I really need base.acc\n";
                  flush stderr;
                  failwith "cannot access database"
  in
  let rec array () =
    match !tab with
      Some x -> x
    | None ->
        if Sys.unix then
          if !verbose then
            begin
              Printf.eprintf "*** read %s%s\n" name
                (if !cleared then " (again)" else "");
              flush stderr
            end;
        seek_in ic array_pos;
        let v = input_array ic in
        let v = v_arr_ext v in
        let t = apply_patches v v_ext patches r.len in tab := Some t; t
  and r =
    {load_array = (fun () -> let _ = array () in ()); get = gen_get;
     set = (fun i v -> (array ()).(i) <- v); len = max len !plenr;
     output_array =
       (fun oc -> Mutil.output_value_no_sharing oc (array () : _ array));
     clear_array = fun () -> cleared := true; tab := None}
  in
  r

let magic_patch_1 = "GnPa0001"
let magic_patch_2 = "GnPa0002"
let magic_patch = magic_patch_2
let check_patch_magic ic magic =
  really_input_string ic (String.length magic) = magic

let input_patches bname =
  try
    let ic = Secure.open_in_bin (Filename.concat bname "patches") in
    let r =
      if check_patch_magic ic magic_patch_2 then (input_value ic : patches_ht)
      else if seek_in ic 0; check_patch_magic ic magic_patch_1
      then
        let patches = (input_value ic : OldGnPa0001.patches) in
        { h_person = patches.OldGnPa0001.h_person
        ; h_ascend = patches.OldGnPa0001.h_ascend
        ; h_union = patches.OldGnPa0001.h_union
        ; h_family = patches.OldGnPa0001.h_family
        ; h_couple = patches.OldGnPa0001.h_couple
        ; h_descend = patches.OldGnPa0001.h_descend
        ; h_string = patches.OldGnPa0001.h_string
        ; h_name_add = patches.OldGnPa0001.h_name
        ; h_name_rm = Hashtbl.create 16
        ; h_name_rm_key = Hashtbl.create 16
        ; h_name_add_key = Hashtbl.create 16
        ; h_string_key = Hashtbl.create 16
        }
      else begin
        seek_in ic 0;
        let patches : Old.patches = input_value ic in
        let ht =
          { h_person = ref 0, Hashtbl.create 16
          ; h_ascend = ref 0, Hashtbl.create 16
          ; h_union = ref 0, Hashtbl.create 16
          ; h_family = ref 0, Hashtbl.create 16
          ; h_couple = ref 0, Hashtbl.create 16
          ; h_descend = ref 0, Hashtbl.create 16
          ; h_string = ref 0, Hashtbl.create 16
          ; h_name_add = Hashtbl.create 16
          ; h_name_rm =  Hashtbl.create 16
          ; h_name_rm_key = Hashtbl.create 16
          ; h_name_add_key = Hashtbl.create 16
          ; h_string_key = Hashtbl.create 16
          }
        in
        let add (ir, ht) (k, v) =
          if k >= !ir then ir := k + 1; Hashtbl.add ht k v
        in
        List.iter (add ht.h_person) !(patches.Old.p_person);
        List.iter (add ht.h_ascend) !(patches.Old.p_ascend);
        List.iter (add ht.h_union) !(patches.Old.p_union);
        List.iter (add ht.h_family) !(patches.Old.p_family);
        List.iter (add ht.h_couple) !(patches.Old.p_couple);
        List.iter (add ht.h_descend) !(patches.Old.p_descend);
        List.iter (add ht.h_string) !(patches.Old.p_string);
        List.iter (add (ref 0, ht.h_name_add)) !(patches.Old.p_name);
        ht
      end
    in
    close_in ic;
    r
  with _ ->
    { h_person = ref 0, Hashtbl.create 16
    ; h_ascend = ref 0, Hashtbl.create 16
    ; h_union = ref 0, Hashtbl.create 16
    ; h_family = ref 0, Hashtbl.create 16
    ; h_couple = ref 0, Hashtbl.create 16
    ; h_descend = ref 0, Hashtbl.create 16
    ; h_string = ref 0, Hashtbl.create 16
    ; h_name_add = Hashtbl.create 16
    ; h_name_rm = Hashtbl.create 16
    ; h_name_rm_key = Hashtbl.create 16
    ; h_name_add_key = Hashtbl.create 16
    ; h_string_key = Hashtbl.create 16
    }

let input_synchro bname =
  try
    let ic = Secure.open_in_bin (Filename.concat bname "synchro_patches") in
    let r : synchro_patch = input_value ic in
    close_in ic ;
    r
  with _ -> {synch_list = []}

let person_of_key persons strings persons_of_name first_name surname occ =
  let ipl = persons_of_name (first_name ^ " " ^ surname) in
  let first_name = Name.lower first_name in
  let surname = Name.lower surname in
  let rec find = function
    | ip :: ipl ->
      let p = persons.get ip in
      if occ = p.occ
      && first_name = Name.lower (strings.get p.first_name)
      && surname = Name.lower (strings.get p.surname)
      then Some ip
      else find ipl
    | _ -> None
  in
  find ipl

let opendb bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let patches = input_patches bname in
  let synchro = input_synchro bname in
  let particles =
    Mutil.input_particles (Filename.concat bname "particles.txt")
  in
  let ic =
    let ic = Secure.open_in_bin (Filename.concat bname "base") in
    Dutil.check_magic ic;
    ic
  in
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
  let norigin_file = input_value ic in
  let ic_acc =
    try Some (Secure.open_in_bin (Filename.concat bname "base.acc")) with
      Sys_error _ ->
        Printf.eprintf "File base.acc not found; trying to continue...\n";
        flush stderr;
        None
  in
  let ic2 =
    try Some (Secure.open_in_bin (Filename.concat bname "strings.inx")) with
      Sys_error _ ->
        Printf.eprintf "File strings.inx not found; trying to continue...\n";
        flush stderr;
        None
  in
  let ic2_string_start_pos = 3 * Mutil.int_size in
  let ic2_string_hash_len =
    match ic2 with
      Some ic2 -> Some (input_binary_int ic2)
    | None -> None
  in
  let ic2_surname_start_pos =
    match ic2 with
      Some ic2 -> Some (input_binary_int ic2)
    | None -> None
  in
  let ic2_first_name_start_pos =
    match ic2 with
      Some ic2 -> Some (input_binary_int ic2)
    | None -> None
  in
  let shift = 0 in
  let persons =
    make_record_access ic ic_acc shift persons_array_pos patches.h_person
      persons_len "persons" (input_value : _ -> person array)
      (Iovalue.input : _ -> person)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let ascends =
    make_record_access ic ic_acc shift ascends_array_pos patches.h_ascend
      persons_len "ascends" (input_value : _ -> ascend array)
      (Iovalue.input : _ -> ascend)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let unions =
    make_record_access ic ic_acc shift unions_array_pos patches.h_union
      persons_len "unions" (input_value : _ -> union array)
      (Iovalue.input : _ -> union)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let families =
    make_record_access ic ic_acc shift families_array_pos patches.h_family
      families_len "families" (input_value : _ -> family array)
      (Iovalue.input : _ -> family)
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let couples =
    make_record_access ic ic_acc shift couples_array_pos patches.h_couple
      families_len "couples" (input_value : _ -> couple array)
      (Iovalue.input : _ -> couple)
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let descends =
    make_record_access ic ic_acc shift descends_array_pos patches.h_descend
      families_len "descends" (input_value : _ -> descend array)
      (Iovalue.input : _ -> descend)
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let strings =
    make_record_access ic ic_acc shift strings_array_pos patches.h_string
      strings_len "strings" (input_value : _ -> string array)
      (Iovalue.input : _ -> string)
  in
  let cleanup () =
    close_in ic;
    begin match ic_acc with
        Some ic_acc -> close_in ic_acc
      | None -> ()
    end;
    match ic2 with
      Some ic2 -> close_in ic2
    | None -> ()
  in
  let commit_synchro () =
    let tmp_fname = Filename.concat bname "1synchro_patches" in
    let fname = Filename.concat bname "synchro_patches" in
    let oc9 =
      try Secure.open_out_bin tmp_fname with
        Sys_error _ ->
          raise (Adef.Request_failure "the database is not writable")
    in
    let synchro =
      let timestamp = string_of_float (Unix.time ()) in
      let timestamp = String.sub timestamp 0 (String.index timestamp '.') in
      let v = timestamp, !synchro_person, !synchro_family in
      {synch_list = v :: synchro.synch_list}
    in
    Mutil.output_value_no_sharing oc9 (synchro : synchro_patch);
    close_out oc9;
    Mutil.remove_file (fname ^ "~");
    (try Sys.rename fname (fname ^ "~") with Sys_error _ -> ());
    try Sys.rename tmp_fname fname with Sys_error _ -> ()
  in
  let commit_patches () =
    let tmp_fname = Filename.concat bname "1patches" in
    let fname = Filename.concat bname "patches" in
    let oc9 =
      try Secure.open_out_bin tmp_fname with
        Sys_error _ ->
          raise (Adef.Request_failure "the database is not writable")
    in
    output_string oc9 magic_patch;
    Mutil.output_value_no_sharing oc9 (patches : patches_ht);
    close_out oc9;
    Mutil.remove_file (fname ^ "~");
    (try Sys.rename fname (fname ^ "~") with Sys_error _ -> ());
    (try Sys.rename tmp_fname fname with Sys_error _ -> ());
    commit_synchro ()
  in
  let patched_ascends () =
    let r = ref [] in
    Hashtbl.iter (fun i _ -> r := Type.iper_of_int i :: !r)
      (snd patches.h_ascend);
    !r
  in
  let patch_person i p =
    let i = Type.int_of_iper i in
    persons.len <- max persons.len (i + 1);
    fst patches.h_person := persons.len;
    Hashtbl.replace (snd patches.h_person) i p;
    synchro_person := i :: !synchro_person
  in
  let patch_ascend i a =
    let i = Type.int_of_iper i in
    ascends.len <- max ascends.len (i + 1);
    fst patches.h_ascend := ascends.len;
    Hashtbl.replace (snd patches.h_ascend) i a;
    synchro_person := i :: !synchro_person
  in
  let patch_union i a =
    let i = Type.int_of_iper i in
    unions.len <- max unions.len (i + 1);
    fst patches.h_union := ascends.len;
    Hashtbl.replace (snd patches.h_union) i a;
    synchro_person := i :: !synchro_person
  in
  let patch_family i f =
    let i = Type.int_of_ifam i in
    families.len <- max families.len (i + 1);
    fst patches.h_family := families.len;
    Hashtbl.replace (snd patches.h_family) i f;
    synchro_family := i :: !synchro_family
  in
  let patch_couple i c =
    let i = Type.int_of_ifam i in
    couples.len <- max couples.len (i + 1);
    fst patches.h_couple := couples.len;
    Hashtbl.replace (snd patches.h_couple) i c;
    synchro_family := i :: !synchro_family
  in
  let patch_descend i c =
    let i = Type.int_of_ifam i in
    descends.len <- max descends.len (i + 1);
    fst patches.h_descend := descends.len;
    Hashtbl.replace (snd patches.h_descend) i c;
    synchro_family := i :: !synchro_family
  in
  let index_of_string =
    index_of_string strings ic2 ic2_string_start_pos ic2_string_hash_len
      (snd patches.h_string)
  in
  let insert_string s =
    try index_of_string s
    with Not_found ->
      let i = strings.len in
      let k = name_index_key s in
      strings.len <- max strings.len (i + 1);
      fst patches.h_string := strings.len;
      Hashtbl.replace (snd patches.h_string) i s;
      Hashtbl.replace patches.h_string_key k
        (try i :: Hashtbl.find patches.h_string_key k with Not_found -> [i]);
      Type.istr_of_int i
  in
  let patch_name rm add ip =
    let aux fn add rm l =
      let aux h found not_found =
        List.iter begin fun s ->
          let i = fn s in
          try found (i, Hashtbl.find h i)
          with Not_found -> not_found i
        end l
      in
      aux rm
        (fun (istr, ipl) -> Hashtbl.replace rm istr @@ List.filter ((<>) ip) ipl)
        ignore ;
      aux add
        (fun (istr, ipl) ->
           if not (List.mem ip ipl) then Hashtbl.replace add istr (ip :: ipl))
        (fun istr -> Hashtbl.add add istr [ip])
    in
    aux insert_string patches.h_name_rm patches.h_name_add rm ;
    aux insert_string patches.h_name_add patches.h_name_rm add ;
    aux name_index_key patches.h_name_rm_key patches.h_name_add_key rm ;
    aux name_index_key patches.h_name_add_key patches.h_name_rm_key add ;
  in
  let read_notes fnotes rn_mode =
    let fname =
      if fnotes = "" then "notes"
      else Filename.concat "notes_d" (fnotes ^ ".txt")
    in
    try
      let ic = Secure.open_in (Filename.concat bname fname) in
      let str =
        match rn_mode with
          RnDeg -> if in_channel_length ic = 0 then "" else " "
        | Rn1Ln -> (try input_line ic with End_of_file -> "")
        | RnAll ->
          let rec loop len =
            match input_char ic with
            | exception End_of_file -> Buff.get len
            | c -> loop (Buff.store len c)
          in
          loop 0
      in
      close_in ic ;
      str
    with Sys_error _ -> ""
  in
  let commit_notes fnotes s =
    let fname =
      if fnotes = "" then "notes"
      else
        begin
          begin try Unix.mkdir (Filename.concat bname "notes_d") 0o755 with
            _ -> ()
          end;
          Filename.concat "notes_d" (fnotes ^ ".txt")
        end
    in
    let fname = Filename.concat bname fname in
    (try Sys.remove (fname ^ "~") with Sys_error _ -> ());
    (try Sys.rename fname (fname ^ "~") with _ -> ());
    if s = "" then ()
    else
      let oc = Secure.open_out fname in output_string oc s; close_out oc; ()
  in
  let ext_files () =
    let top = Filename.concat bname "notes_d" in
    let rec loop list subdir =
      let dir = Filename.concat top subdir in
      try
        let files = Sys.readdir dir in
        Array.fold_left
          (fun files file ->
             let f = Filename.concat subdir file in
             if Filename.check_suffix f ".txt" then
               Filename.chop_suffix f ".txt" :: files
             else loop files f)
          list files
      with Sys_error _ -> list
    in
    loop [] Filename.current_dir_name
  in
  let bnotes =
    {nread = read_notes; norigin_file = norigin_file; efiles = ext_files}
  in
  let base_data =
    {persons = persons; ascends = ascends; unions = unions;
     visible = make_visible_record_access bname persons; families = families;
     couples = couples; descends = descends; strings = strings;
     particles = particles; bnotes = bnotes; bdir = bname}
  in
  let persons_of_name = persons_of_name bname patches.h_name_rm_key patches.h_name_add_key in
  let base_func =
    { person_of_key = person_of_key persons strings persons_of_name
    ; persons_of_name = persons_of_name
    ; strings_of_fsname = strings_of_fsname bname patches.h_string_key
    ; persons_of_surname =
        persons_of_first_name_or_surname base_data strings persons
          (ic2, ic2_surname_start_pos, (fun p -> List.map insert_string @@ split_sname strings p.surname),
           patches.h_name_rm, patches.h_name_add, "snames.inx", "snames.dat", bname)
    ; persons_of_first_name =
        persons_of_first_name_or_surname base_data strings persons
          (ic2, ic2_first_name_start_pos, (fun p -> List.map insert_string @@ split_fname strings p.first_name),
           patches.h_name_rm, patches.h_name_add, "fnames.inx", "fnames.dat", bname)
    ; patch_person = patch_person; patch_ascend = patch_ascend
    ; patch_union = patch_union; patch_family = patch_family
    ; patch_couple = patch_couple; patch_descend = patch_descend
    ; patch_name = patch_name; insert_string = insert_string
    ; commit_patches = commit_patches; patched_ascends = patched_ascends
    ; commit_notes = commit_notes
    ; cleanup = cleanup
    }
  in
  {data = base_data; func = base_func}
