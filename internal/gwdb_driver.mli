
module type S = sig

  type istr = Dbdisk.dsk_istr
  type iper = Def.iper
  type ifam = Def.ifam
  type string_person_index
  type relation = (iper, istr) Def.gen_relation
  type title = istr Def.gen_title
  type pers_event = (iper, istr) Def.gen_pers_event
  type fam_event = (iper, istr) Def.gen_fam_event

  type p = (iper, istr) Def.gen_person
  type a = ifam Def.gen_ascend
  type u = ifam Def.gen_union

  type person_dat
  type person

  type person_per = Dbdisk.dsk_person
  type person_asc = Dbdisk.dsk_ascend
  type person_uni = Dbdisk.dsk_union

  type f = (iper, istr) Def.gen_family
  type c = iper Def.gen_couple
  type d = iper Def.gen_descend

  type family_dat
  type family
  type family_fam = Dbdisk.dsk_family
  type family_cpl = Dbdisk.dsk_couple
  type family_des = Dbdisk.dsk_descend

  type base = Dbdisk.dsk_base

  val spi_first : string_person_index -> string -> istr
  (* first [first/sur]name starting with that string *)
  val spi_next : string_person_index -> istr -> (* bool -> *) istr * int
  (* next [first/sur]name by alphabetical order *)
  val spi_find : string_person_index -> istr -> iper list
  (* all persons having that [first/sur]name *)

  val is_quest_string : istr -> bool
  val is_empty_string : istr -> bool

  (* module Person : sig *)

    val get_access : p -> Def.access
    val get_aliases : p -> istr list
    val get_baptism : p -> Def.cdate
    val get_baptism_place : p -> istr
    val get_baptism_note : p -> istr
    val get_baptism_src : p -> istr
    val get_birth : p -> Def.cdate
    val get_birth_place : p -> istr
    val get_birth_note : p -> istr
    val get_birth_src : p -> istr
    val get_burial : p -> Def.burial
    val get_burial_place : p -> istr
    val get_burial_note : p -> istr
    val get_burial_src : p -> istr
    val get_death : p -> Def.death
    val get_death_place : p -> istr
    val get_death_note : p -> istr
    val get_death_src : p -> istr
    val get_first_name : p -> istr
    val get_first_names_aliases : p -> istr list
    val get_image : p -> istr
    val get_key_index : p -> iper
    val get_notes : p -> istr
    val get_occ : p -> int
    val get_occupation : p -> istr
    val get_psources : p -> istr
    val get_public_name : p -> istr
    val get_qualifiers : p -> istr list
    val get_related : p -> iper list
    val get_rparents : p -> relation list
    val get_sex : p -> Def.sex
    val get_surname : p -> istr
    val get_surnames_aliases : p -> istr list
    val get_titles : p -> title list
    val get_pevents : p -> pers_event list
    val gen_person_of_person : p -> (iper, istr) Def.gen_person
    val dsk_person_of_person : p -> Dbdisk.dsk_person
    val get_consang : a -> Adef.fix
    val get_parents : a -> ifam option
    val get_family : u -> ifam array

    val get_per : person_dat -> person_per option
    val set_per : person_dat -> person_per option -> unit

    val get_asc : person_dat -> person_asc option
    val set_asc : person_dat -> person_asc option -> unit

    val get_uni : person_dat -> person_uni option
    val set_uni : person_dat -> person_uni option -> unit

  (* end
   * 
   * module Family : sig *)

    val get_comment : f -> istr
    val get_divorce : f -> Def.divorce
    val get_fsources : f -> istr
    val get_fevents : f -> fam_event list
    val get_marriage : f -> Def.cdate
    val get_marriage_place : f -> istr
    val get_marriage_note : f -> istr
    val get_marriage_src : f -> istr
    val get_origin_file : f -> istr
    val get_relation : f -> Def.relation_kind
    val get_witnesses : f -> iper array
    val gen_family_of_family : f -> (iper, istr) Def.gen_family
    val is_deleted_family : f -> bool
    val get_father : c -> iper
    val get_mother : c -> iper
    val get_parent_array : c -> iper array
    val gen_couple_of_couple : c -> iper Def.gen_couple
    val get_children : d -> iper array
    val gen_descend_of_descend : d -> iper Def.gen_descend

    val get_fam1 : family_dat -> family_fam option
    val get_cpl1 : family_dat -> family_cpl option
    val get_des1 : family_dat -> family_des option

    val set_fam1 : family_dat -> family_fam option -> unit
    val set_cpl1 : family_dat -> family_cpl option -> unit
    val set_des1 : family_dat -> family_des option -> unit

  (* end
   * 
   * module Base : sig *)

    val close_base : base -> unit
    val empty_person : base -> iper -> person
    val person_of_gen_person :
      base -> (iper, istr) Def.gen_person * ifam Def.gen_ascend * ifam Def.gen_union -> person
    val family_of_gen_family :
      base -> (iper, istr) Def.gen_family * iper Def.gen_couple * iper Def.gen_descend -> family
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
    val person_of_key : base -> string -> string -> int -> iper option
    val persons_of_name : base -> string -> iper list
    val persons_of_first_name : base -> string_person_index
    val persons_of_surname : base -> string_person_index
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
    val date_of_last_change : base -> float
    val apply : base -> (base -> 'a) -> 'a

    val open_base : string -> base

  (* end *)

end
