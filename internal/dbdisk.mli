(* $Id: dbdisk.mli,v 5.14 2006-12-23 23:41:28 ddr Exp $ *)

open Def

type dsk_istr = Adef.istr

type dsk_person = (iper, dsk_istr) gen_person
type dsk_ascend = ifam gen_ascend
type dsk_union = ifam gen_union
type dsk_family = (iper, dsk_istr) gen_family
type dsk_couple = iper gen_couple
type dsk_descend = iper gen_descend

type dsk_title = dsk_istr gen_title

type notes =
  { nread : string -> rn_mode -> string;
    norigin_file : string;
    efiles : unit -> string list }

type 'a record_access =
  { load_array : unit -> unit;
    get : int -> 'a;
    set : int -> 'a -> unit;
    mutable len : int;
    output_array : out_channel -> unit;
    clear_array : unit -> unit }

type 'istr string_person_index =
  { find : 'istr -> iper list;
    cursor : string -> 'istr;
    next : 'istr -> 'istr }

type visible_record_access =
  { v_write : unit -> unit; v_get : (dsk_person -> bool) -> int -> bool }

type base_data =
  { persons : dsk_person record_access;
    ascends : dsk_ascend record_access;
    unions : dsk_union record_access;
    visible : visible_record_access;
    families : dsk_family record_access;
    couples : dsk_couple record_access;
    descends : dsk_descend record_access;
    strings : string record_access;
    particles : string list;
    bnotes : notes;
    bdir : string }

type base_func =
  { person_of_key : string -> string -> int -> iper option;
    persons_of_name : string -> iper list;
    strings_of_fsname : string -> dsk_istr list;
    persons_of_surname : dsk_istr string_person_index;
    persons_of_first_name : dsk_istr string_person_index;
    patch_person : iper -> dsk_person -> unit;
    patch_ascend : iper -> dsk_ascend -> unit;
    patch_union : iper -> dsk_union -> unit;
    patch_family : ifam -> dsk_family -> unit;
    patch_couple : ifam -> dsk_couple -> unit;
    patch_descend : ifam -> dsk_descend -> unit;
    patch_name : string -> iper -> unit;
    insert_string : string -> dsk_istr;
    commit_patches : unit -> unit;
    commit_notes : string -> string -> unit;
    patched_ascends : unit -> iper list;
    is_patched_person : iper -> bool;
    cleanup : unit -> unit
}

type dsk_base = { data : base_data
                ; func : base_func }
