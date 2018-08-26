(* $Id: gwdb.mli,v 5.102 2007-03-02 11:44:13 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Adef

type istr

type person
type family

type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

type string_person_index

type base = Dbdisk.dsk_base

val open_base : string -> base
val close_base : base -> unit

val eq_istr : istr -> istr -> bool
val is_empty_string : istr -> bool
val is_quest_string : istr -> bool
val empty_person : base -> iper -> person

val get_access : person -> Def.access
val get_aliases : person -> istr list
val get_baptism : person -> cdate
val get_baptism_place : person -> istr
val get_baptism_note : person -> istr
val get_baptism_src : person -> istr
val get_birth : person -> cdate
val get_birth_place : person -> istr
val get_birth_note : person -> istr
val get_birth_src : person -> istr
val get_burial : person -> Def.burial
val get_burial_place : person -> istr
val get_burial_note : person -> istr
val get_burial_src : person -> istr
val get_death : person -> Def.death
val get_death_place : person -> istr
val get_death_note : person -> istr
val get_death_src : person -> istr
val get_first_name : person -> istr
val get_first_names_aliases : person -> istr list
val get_image : person -> istr
val get_key_index : person -> iper
val get_notes : person -> istr
val get_occ : person -> int
val get_occupation : person -> istr
val get_pevents : person -> pers_event list
val get_psources : person -> istr
val get_public_name : person -> istr
val get_qualifiers : person -> istr list
val get_related : person -> iper list
val get_rparents : person -> relation list
val get_sex : person -> Def.sex
val get_surname : person -> istr
val get_surnames_aliases : person -> istr list
val get_titles : person -> title list

val get_parents : person -> ifam option
val get_consang : person -> Adef.fix

val get_family : person -> ifam array

val gen_person_of_person : person -> (iper, istr) Def.gen_person

val get_comment : family -> istr
val get_divorce : family -> Def.divorce
val get_fevents : family -> fam_event list
val get_fsources : family -> istr
val get_marriage : family -> cdate
val get_marriage_place : family -> istr
val get_marriage_note : family -> istr
val get_marriage_src : family -> istr
val get_origin_file : family -> istr
val get_relation : family -> Def.relation_kind
val get_witnesses : family -> iper array

val get_father : family -> iper
val get_mother : family -> iper
val get_parent_array : family -> iper array

val get_children : family -> iper array

val gen_family_of_family : family -> (iper, istr) Def.gen_family
val gen_couple_of_couple : family -> iper Def.gen_couple
val gen_descend_of_descend : family -> iper Def.gen_descend

val person_of_gen_person :
  base ->
    (iper, istr) Def.gen_person * ifam Def.gen_ascend * ifam Def.gen_union ->
    person

val family_of_gen_family :
  base ->
    (iper, istr) Def.gen_family * iper Def.gen_couple *
      iper Def.gen_descend ->
    family

val poi : base -> iper -> person
val foi : base -> ifam -> family
val sou : base -> istr -> string

val nb_of_persons : base -> int
val nb_of_families : base -> int

val patch_person : base -> iper -> (iper, istr) Def.gen_person -> unit
val patch_ascend : base -> iper -> ifam Def.gen_ascend -> unit
val patch_union : base -> iper -> ifam Def.gen_union -> unit
val patch_family : base -> ifam -> (iper, istr) Def.gen_family -> unit
val patch_descend : base -> ifam -> iper Def.gen_descend -> unit
val patch_couple : base -> ifam -> iper Def.gen_couple -> unit

val patch_name : base -> string -> iper -> unit
val patch_key : base -> iper -> string -> string -> int -> unit
val delete_key : base -> string -> string -> int -> unit
val insert_string : base -> string -> istr
val commit_patches : base -> unit
val commit_notes : base -> string -> string -> unit
val is_patched_person : base -> iper -> bool
val patched_ascends : base -> iper list

val delete_family : base -> ifam -> unit
val is_deleted_family : family -> bool

val person_of_key : base -> string -> string -> int -> iper option
val persons_of_name : base -> string -> iper list
val persons_of_first_name : base -> string_person_index
val persons_of_surname : base -> string_person_index

val spi_first : string_person_index -> string -> istr
  (* first [first/sur]name starting with that string *)
val spi_next : string_person_index -> istr -> (* bool -> *) istr * int
  (* next [first/sur]name by alphabetical order *)
val spi_find : string_person_index -> istr -> iper list
  (* all persons having that [first/sur]name *)

val base_visible_get : base -> (person -> bool) -> int -> bool
val base_visible_write : base -> unit
val base_particles : base -> string list
val base_strings_of_first_name : base -> string -> istr list
val base_strings_of_surname : base -> string -> istr list

val load_ascends_array : base -> unit
val load_unions_array : base -> unit
val load_couples_array : base -> unit
val load_descends_array : base -> unit
val load_strings_array : base -> unit
val load_persons_array : base -> unit
val load_families_array : base -> unit

val clear_ascends_array : base -> unit
val clear_unions_array : base -> unit
val clear_couples_array : base -> unit
val clear_descends_array : base -> unit
val clear_strings_array : base -> unit
val clear_persons_array : base -> unit
val clear_families_array : base -> unit

val persons_array :
  base ->
    (int -> (iper, istr) Def.gen_person) *
      (int -> (iper, istr) Def.gen_person -> unit)
val ascends_array :
  base ->
    (int -> ifam option) * (int -> Adef.fix) * (int -> Adef.fix -> unit) *
      Adef.fix array option

val base_notes_read : base -> string -> string
val base_notes_read_first_line : base -> string -> string
val base_notes_are_empty : base -> string -> bool
val base_notes_origin_file : base -> string
val base_notes_dir : base -> string
val base_wiznotes_dir : base -> string

val gen_person_misc_names :
  base -> (iper, istr) Def.gen_person ->
    ((iper, istr) Def.gen_person -> istr Def.gen_title list) -> string list

val person_misc_names :
  base -> person -> (person -> title list) -> string list
val nobtit :
  base -> string list Lazy.t -> string list Lazy.t -> person -> title list

val p_first_name : base -> person -> string
val p_surname : base -> person -> string

val date_of_last_change : base -> float

(**/**)
(** For database builders *)

val dsk_person_of_person : person -> Dbdisk.dsk_person

val apply : base -> (Dbdisk.dsk_base -> unit) -> unit
