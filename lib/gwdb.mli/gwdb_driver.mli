(* Copyright (c) 1998-2007 INRIA *)

type iper
type ifam
type istr

val string_of_iper : iper -> string
val string_of_ifam : ifam -> string
val string_of_istr : istr -> string

val iper_of_string : string -> iper
val ifam_of_string : string -> ifam
val istr_of_string : string -> istr

type person
type family

type string_person_index

type base

val open_base : string -> base
val close_base : base -> unit

val dummy_iper : iper
val dummy_ifam : ifam
val dummy_istr : istr

val eq_istr : istr -> istr -> bool
val is_empty_string : istr -> bool
val is_quest_string : istr -> bool

val sou : base -> istr -> string

val nb_of_persons : base -> int
val nb_of_families : base -> int

val new_iper : base -> iper
val new_ifam : base -> ifam

val insert_person : base -> iper -> (iper, iper, istr) Def.gen_person -> unit
val insert_ascend : base -> iper -> ifam Def.gen_ascend -> unit
val insert_union : base -> iper -> ifam Def.gen_union -> unit
val insert_family : base -> ifam -> (iper, ifam, istr) Def.gen_family -> unit
val insert_descend : base -> ifam -> iper Def.gen_descend -> unit
val insert_couple : base -> ifam -> iper Def.gen_couple -> unit

val patch_person : base -> iper -> (iper, iper, istr) Def.gen_person -> unit
val patch_ascend : base -> iper -> ifam Def.gen_ascend -> unit
val patch_union : base -> iper -> ifam Def.gen_union -> unit
val patch_family : base -> ifam -> (iper, ifam, istr) Def.gen_family -> unit
val patch_descend : base -> ifam -> iper Def.gen_descend -> unit
val patch_couple : base -> ifam -> iper Def.gen_couple -> unit

val delete_person : base -> iper -> unit
val delete_ascend : base -> iper -> unit
val delete_union : base -> iper -> unit
val delete_family : base -> ifam -> unit
val delete_descend : base -> ifam -> unit
val delete_couple : base -> ifam -> unit

val insert_string : base -> string -> istr
val commit_patches : base -> unit
val commit_notes : base -> string -> string -> unit
val patched_ascends : base -> iper list

val person_of_key : base -> string -> string -> int -> iper option
val persons_of_name : base -> string -> iper list
val persons_of_first_name : base -> string_person_index
val persons_of_surname : base -> string_person_index

(** first [first/sur]name starting with that string *)
val spi_first : string_person_index -> string -> istr

(** next [first/sur]name by Gutil.alphabetical order *)
val spi_next : string_person_index -> istr -> bool -> istr * int

(** all persons having that [first/sur]name *)
val spi_find : string_person_index -> istr -> iper list

val base_particles : base -> string list

(** [base_strings_of_first_name base x]
    Return the list of first names (as [istr]) being equal to [x]
    using  {!val:Name.crush_lower} comparison.
*)
val base_strings_of_first_name : base -> string -> istr list

(** [base_strings_of_surname base x]
    Return the list of surnames (as [istr]) being equal to [x]
    using  {!val:Name.crush_lower} comparison.
*)
val base_strings_of_surname : base -> string -> istr list

(** Cache loading *)
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

val date_of_last_change : base -> float

(** {2 collections} *)

type 'a cursor = { length : int ; get : int -> 'a option }

val persons : base -> iper cursor
val families : base -> ifam cursor

val get_person : base -> iper -> (iper, iper, istr) Def.gen_person
val get_ascend : base -> iper -> ifam Def.gen_ascend
val get_union : base -> iper -> ifam Def.gen_union

val get_family : base -> ifam -> (iper, ifam, istr) Def.gen_family
val get_couple : base -> ifam -> iper Def.gen_couple
val get_descend : base -> ifam -> iper Def.gen_descend
