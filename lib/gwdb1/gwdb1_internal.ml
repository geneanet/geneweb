(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk
open Def

type istr = Type.istr
type ifam = Type.ifam
type iper = Type.iper

type person = (iper, iper, istr) gen_person
type ascend = ifam gen_ascend
type union = ifam gen_union
type family = (iper, ifam, istr) gen_family
type couple = iper gen_couple
type descend = ifam gen_descend

let string_of_iper = string_of_int
let string_of_ifam = string_of_int
let string_of_istr = string_of_int

let iper_of_string = int_of_string
let ifam_of_string = int_of_string
let istr_of_string = int_of_string

let dummy_iper = Type.dummy_iper
let dummy_ifam = Type.dummy_ifam
let dummy_istr = Type.dummy_istr

type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

let eq_istr i1 i2 = Type.int_of_istr i1 = Type.int_of_istr i2
let is_empty_string istr = Type.int_of_istr istr = 0
let is_quest_string istr = Type.int_of_istr istr = 1

type string_person_index = istr Dbdisk.string_person_index

let spi_find spi = spi.find
let spi_first spi = spi.cursor
let spi_next (spi : string_person_index) istr (_need_whole_list : bool) = spi.next istr, 1

type base = dsk_base

let base_strings_of_first_name_or_surname base s = base.func.strings_of_fsname s

let open_base bname : base =
  let bname = if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb" in
  Database.opendb bname

let close_base base = base.func.cleanup ()

let get_person base i = base.data.persons.get i
let get_ascend base i = base.data.ascends.get i
let get_union base i = base.data.unions.get i

let get_family base i = base.data.families.get i
let get_couple base i = base.data.couples.get i
let get_descend base i = base.data.descends.get i

let sou base i = base.data.strings.get (Type.int_of_istr i)
let nb_of_persons base = base.data.persons.len

let nb_of_families base = base.data.families.len

let insert_string base s = base.func.Dbdisk.insert_string s
let commit_patches base = base.func.Dbdisk.commit_patches ()
let commit_notes base s = base.func.Dbdisk.commit_notes s

let patched_ascends base = base.func.Dbdisk.patched_ascends ()
let person_of_key base = base.func.Dbdisk.person_of_key
let persons_of_name base = base.func.Dbdisk.persons_of_name
let persons_of_first_name base = base.func.Dbdisk.persons_of_first_name
let persons_of_surname base = base.func.Dbdisk.persons_of_surname

let base_particles base = base.data.particles
let base_strings_of_first_name = base_strings_of_first_name_or_surname
let base_strings_of_surname = base_strings_of_first_name_or_surname

let load_ascends_array base = base.data.ascends.load_array ()
let load_unions_array base = base.data.unions.load_array ()
let load_couples_array base = base.data.couples.load_array ()
let load_descends_array base = base.data.descends.load_array ()
let load_strings_array base = base.data.strings.load_array ()
let load_persons_array base = base.data.persons.load_array ()
let load_families_array base = base.data.families.load_array ()

let clear_ascends_array base = base.data.ascends.clear_array ()
let clear_unions_array base = base.data.unions.clear_array ()
let clear_couples_array base = base.data.couples.clear_array ()
let clear_descends_array base = base.data.descends.clear_array ()
let clear_strings_array base = base.data.strings.clear_array ()
let clear_persons_array base = base.data.persons.clear_array ()
let clear_families_array base = base.data.families.clear_array ()

let date_of_last_change base =
  let s =
    let bdir = base.data.bdir in
    try Unix.stat (Filename.concat bdir "patches")
    with Unix.Unix_error (_, _, _) -> Unix.stat (Filename.concat bdir "base")
  in
  s.Unix.st_mtime

let apply_base1 base f = f base

let patch_misc_names _base _ip _p = ignore @@ failwith __LOC__

let patch_person base ip (p : (iper, iper, istr) Def.gen_person) =
  base.func.Dbdisk.patch_person ip p ;
  let s = sou base p.first_name ^ " " ^ sou base p.surname in
  base.func.Dbdisk.patch_name s ip ;
  patch_misc_names base ip p

let patch_ascend base ip a = base.func.Dbdisk.patch_ascend ip a

let patch_union base ip u = base.func.Dbdisk.patch_union ip u

let patch_family base ifam f = base.func.Dbdisk.patch_family ifam f

let patch_couple base ifam c = base.func.Dbdisk.patch_couple ifam c

let patch_descend base ifam d = base.func.Dbdisk.patch_descend ifam d

let insert_person = patch_person
let insert_ascend = patch_ascend
let insert_union = patch_union
let insert_family = patch_family
let insert_couple = patch_couple
let insert_descend = patch_descend

let delete_person base ip =
  patch_person base ip
    { first_name = dummy_istr
    ; surname = dummy_istr
    ; occ = 0
    ; image = dummy_istr
    ; first_names_aliases = []
    ; surnames_aliases = []
    ; public_name = dummy_istr
    ; qualifiers = []
    ; titles = []
    ; rparents = []
    ; related = []
    ; aliases = []
    ; occupation = dummy_istr
    ; sex = Neuter
    ; access = Private
    ; birth = Adef.cdate_None
    ; birth_place = dummy_istr
    ; birth_note = dummy_istr
    ; birth_src = dummy_istr
    ; baptism = Adef.cdate_None
    ; baptism_place = dummy_istr
    ; baptism_note = dummy_istr
    ; baptism_src = dummy_istr
    ; death = DontKnowIfDead
    ; death_place = dummy_istr
    ; death_note = dummy_istr
    ; death_src = dummy_istr
    ; burial = UnknownBurial
    ; burial_place = dummy_istr
    ; burial_note = dummy_istr
    ; burial_src = dummy_istr
    ; pevents = []
    ; notes = dummy_istr
    ; psources = dummy_istr
    ; key_index = dummy_iper
    }

let delete_ascend base ip =
  patch_ascend base ip { parents = None ; consang = Adef.no_consang }

let delete_union base ip =
  patch_union base ip { family = [||] }

let delete_family base ifam =
  patch_family base ifam
    { marriage = Adef.cdate_None
    ; marriage_place = dummy_istr
    ; marriage_note = dummy_istr
    ; marriage_src = dummy_istr
    ; relation = Married
    ; divorce = NotDivorced
    ; fevents = []
    ; witnesses = [||]
    ; comment = dummy_istr
    ; origin_file = dummy_istr
    ; fsources = dummy_istr
    ; fam_index = dummy_ifam
    }

let delete_couple base ifam =
  patch_couple base ifam (Adef.couple dummy_iper dummy_iper)

let delete_descend base ifam =
  patch_descend base ifam { children = [||] }

let new_iper base = nb_of_persons base

let new_ifam base = nb_of_families base

type 'a cursor = { length : int ; get : int -> 'a option }

let persons base =
  { length = nb_of_persons base
  ; get = begin fun i ->
      (* FIXME: avoid fetching *)
      let p = base.data.persons.get i in
      if p.key_index = dummy_iper then None else Some p.key_index
    end
  } [@ocaml.warning "-42"]

let families base =
  { length = nb_of_families base
  ; get = begin fun i ->
      (* FIXME: avoid fetching *)
      let f = base.data.families.get i in
      if f.fam_index = dummy_ifam then None else Some f.fam_index
    end
  } [@ocaml.warning "-42"]
