type iper
type ifam
type istr
type person
type family
type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event
type string_person_index
type base
let string_of_iper _ = assert false
let string_of_ifam _ = assert false
let string_of_istr _ = assert false
let iper_of_string _ = assert false
let ifam_of_string _ = assert false
let istr_of_string _ = assert false
let open_base _ = assert false
let close_base _ = assert false
let dummy_iper = Obj.magic 0
let dummy_ifam = Obj.magic 0
let dummy_istr = Obj.magic 0
let eq_istr _ = assert false
let is_empty_string _ = assert false
let is_quest_string _ = assert false
let empty_person _ = assert false
let empty_family _ = assert false
let get_access _ = assert false
let get_aliases _ = assert false
let get_baptism _ = assert false
let get_baptism_place _ = assert false
let get_baptism_note _ = assert false
let get_baptism_src _ = assert false
let get_birth _ = assert false
let get_birth_place _ = assert false
let get_birth_note _ = assert false
let get_birth_src _ = assert false
let get_burial _ = assert false
let get_burial_place _ = assert false
let get_burial_note _ = assert false
let get_burial_src _ = assert false
let get_death _ = assert false
let get_death_place _ = assert false
let get_death_note _ = assert false
let get_death_src _ = assert false
let get_first_name _ = assert false
let get_first_names_aliases _ = assert false
let get_image _ = assert false
let get_iper _ = assert false
let get_notes _ = assert false
let get_occ _ = assert false
let get_occupation _ = assert false
let get_pevents _ = assert false
let get_psources _ = assert false
let get_public_name _ = assert false
let get_qualifiers _ = assert false
let get_related _ = assert false
let get_rparents _ = assert false
let get_sex _ = assert false
let get_surname _ = assert false
let get_surnames_aliases _ = assert false
let get_titles _ = assert false
let get_parents _ = assert false
let get_consang _ = assert false
let get_family _ = assert false
let gen_person_of_person _ = assert false
let get_comment _ = assert false
let get_ifam _ = assert false
let get_divorce _ = assert false
let get_fevents _ = assert false
let get_fsources _ = assert false
let get_marriage _ = assert false
let get_marriage_place _ = assert false
let get_marriage_note _ = assert false
let get_marriage_src _ = assert false
let get_origin_file _ = assert false
let get_relation _ = assert false
let get_witnesses _ = assert false
let get_father _ = assert false
let get_mother _ = assert false
let get_parent_array _ = assert false
let get_children _ = assert false
let gen_family_of_family _ = assert false
let gen_couple_of_couple _ = assert false
let gen_descend_of_descend _ = assert false
let person_of_gen_person _ = assert false
let family_of_gen_family _ = assert false
let poi _ = assert false
let poi_batch _ = assert false
let foi _ = assert false
let foi_batch _ = assert false
let sou _ = assert false
let nb_of_persons _ = assert false
let nb_of_families _ = assert false
let patch_person _ = assert false
let patch_ascend _ = assert false
let patch_union _ = assert false
let patch_family _ = assert false
let patch_descend _ = assert false
let patch_couple _ = assert false
let insert_string _ = assert false
let commit_patches _ = assert false
let commit_notes _ = assert false
let patched_ascends _ = assert false
let insert_person _ = assert false
let insert_family _ = assert false
let delete_family _ = assert false
let person_of_key _ = assert false
let persons_of_name _ = assert false
let persons_of_first_name _ = assert false
let persons_of_surname _ = assert false
let spi_first _ = assert false
let spi_next _ = assert false
let spi_find _ = assert false
let base_visible_get _ = assert false
let base_visible_write _ = assert false
let base_particles _ = assert false
let base_strings_of_first_name _ = assert false
let base_strings_of_surname _ = assert false
let load_ascends_array _ = assert false
let load_unions_array _ = assert false
let load_couples_array _ = assert false
let load_descends_array _ = assert false
let load_strings_array _ = assert false
let load_persons_array _ = assert false
let load_families_array _ = assert false
let clear_ascends_array _ = assert false
let clear_unions_array _ = assert false
let clear_couples_array _ = assert false
let clear_descends_array _ = assert false
let clear_strings_array _ = assert false
let clear_persons_array _ = assert false
let clear_families_array _ = assert false
let persons_array _ = assert false
let ascends_array _ = assert false
let base_notes_read _ = assert false
let base_notes_read_first_line _ = assert false
let base_notes_are_empty _ = assert false
let base_notes_origin_file _ = assert false
let base_notes_dir _ = assert false
let base_wiznotes_dir _ = assert false
let gen_person_misc_names _ = assert false
let person_misc_names _ = assert false
let nobtit _ = assert false
let p_first_name _ = assert false
let p_surname _ = assert false
let date_of_last_change _ = assert false
module Collection = struct
  type 'a t
  let length _ = assert false
  let map _ = assert false
  let iter _ = assert false
  let iteri _ = assert false
  let fold ?from:_ ?until:_ _ = assert false
  let fold_until _ = assert false
  let iterator _ = assert false
end
module Marker = struct
  type ('k, 'v) t
  let get _ = assert false
  let set _ = assert false
end
let ipers _ = assert false
let persons _ = assert false
let ifams _ = assert false
let families _ = assert false
let dummy_collection _ = assert false
let iper_marker _ = assert false
let ifam_marker _ = assert false
let dummy_marker _ = assert false
