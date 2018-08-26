open Dbdisk
open Def

type 'istr gen_string_person_index =
  'istr Dbdisk.string_person_index =
    { find : 'istr -> iper list;
      cursor : string -> 'istr;
      next : 'istr -> 'istr }

let is_empty_string = (fun istr -> Adef.int_of_istr istr = 0)
let is_quest_string = (fun istr -> Adef.int_of_istr istr = 1)

let spi_first spi istr = spi.cursor istr
let spi_next spi istr (* _need_whole_list *) = spi.next istr, 1
let spi_find spi istr = spi.find istr

type string_person_index = dsk_istr gen_string_person_index

type istr = Dbdisk.dsk_istr
type iper = Def.iper
type ifam = Def.ifam
type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

type person_dat =
  { mutable per1 : dsk_person option
  ; mutable asc1 : dsk_ascend option
  ; mutable uni1 : dsk_union option
  }
type person = dsk_base * int * person_dat

type person_per = Dbdisk.dsk_person
type person_asc = Dbdisk.dsk_ascend
type person_uni = Dbdisk.dsk_union

type p = (iper, istr) Def.gen_person
type a = ifam Def.gen_ascend
type u = ifam Def.gen_union

type family_dat =
  { mutable fam1 : dsk_family option;
    mutable cpl1 : dsk_couple option;
    mutable des1 : dsk_descend option }
type family = dsk_base * int * family_dat

type f = (iper, istr) Def.gen_family
type c = iper Def.gen_couple
type d = iper Def.gen_descend

type family_fam = Dbdisk.dsk_family
type family_cpl = Dbdisk.dsk_couple
type family_des = Dbdisk.dsk_descend


type base = Dbdisk.dsk_base

(* module Person = struct *)

  let get_per = (fun p -> p.per1)
  let set_per = (fun p v -> p.per1 <- v)

  let get_asc = (fun p -> p.asc1)
  let set_asc = (fun p v -> p.asc1 <- v)

  let get_uni = (fun p -> p.uni1)
  let set_uni = (fun p v -> p.uni1 <- v)

  let get_access = (fun p -> p.Def.access)
  let get_aliases = (fun p -> p.Def.aliases)
  let get_baptism = (fun p -> p.Def.baptism)
  let get_baptism_place = (fun p -> p.Def.baptism_place)
  let get_baptism_note = (fun p -> p.Def.baptism_note)
  let get_baptism_src = (fun p -> p.Def.baptism_src)
  let get_birth = (fun p -> p.Def.birth)
  let get_birth_place = (fun p -> p.Def.birth_place)
  let get_birth_note = (fun p -> p.Def.birth_note)
  let get_birth_src = (fun p -> p.Def.birth_src)
  let get_burial = (fun p -> p.Def.burial)
  let get_burial_place = (fun p -> p.Def.burial_place)
  let get_burial_note = (fun p -> p.Def.burial_note)
  let get_burial_src = (fun p -> p.Def.burial_src)
  let get_death = (fun p -> p.Def.death)
  let get_death_place = (fun p -> p.Def.death_place)
  let get_death_note = (fun p -> p.Def.death_note)
  let get_death_src = (fun p -> p.Def.death_src)
  let get_first_name = (fun p -> p.Def.first_name)
  let get_first_names_aliases = (fun p -> p.Def.first_names_aliases)
  let get_image = (fun p -> p.Def.image)
  let get_key_index = (fun p -> p.Def.key_index)
  let get_notes = (fun p -> p.Def.notes)
  let get_occ = (fun p -> p.Def.occ)
  let get_occupation = (fun p -> p.Def.occupation)
  let get_psources = (fun p -> p.Def.psources)
  let get_public_name = (fun p -> p.Def.public_name)
  let get_qualifiers = (fun p -> p.Def.qualifiers)
  let get_related = (fun p -> p.Def.related)
  let get_rparents =
    (fun p ->
       List.map (Futil.map_relation_ps (fun x -> x) (fun i -> i))
         p.Def.rparents)
  let get_sex = (fun p -> p.Def.sex)
  let get_surname = (fun p -> p.Def.surname)
  let get_surnames_aliases =
    (fun p -> List.map (fun i -> i) p.Def.surnames_aliases)
  let get_titles =
    (fun p ->
       List.map (fun t -> Futil.map_title_strings (fun i -> i) t)
         p.Def.titles)
  let get_pevents =
    (fun p ->
       List.map (fun t -> Futil.map_pers_event (fun x -> x) (fun i -> i) t)
         p.Def.pevents)
  let gen_person_of_person =
    (fun p -> Futil.map_person_ps (fun p -> p) (fun s -> s) p)
  let dsk_person_of_person = (fun p -> p)
  let get_consang = (fun a -> a.Def.consang)
  let get_parents = (fun a -> a.Def.parents)
  let get_family = fun u -> u.Def.family

(* end
 * 
 * module Family = struct *)

  let get_comment = (fun f -> f.Def.comment)
  let get_divorce = (fun f -> f.Def.divorce)
  let get_fsources = (fun f -> f.Def.fsources)
  let get_fevents =
    (fun f ->
       List.map (fun t -> Futil.map_fam_event (fun x -> x) (fun i -> i) t)
         f.Def.fevents)
  let get_marriage = (fun f -> f.Def.marriage)
  let get_marriage_place = (fun f -> f.Def.marriage_place)
  let get_marriage_note = (fun f -> f.Def.marriage_note)
  let get_marriage_src = (fun f -> f.Def.marriage_src)
  let get_origin_file = (fun f -> f.Def.origin_file)
  let get_relation = (fun f -> f.Def.relation)
  let get_witnesses = (fun f -> f.Def.witnesses)
  let gen_family_of_family =
    (fun f -> Futil.map_family_ps (fun p -> p) (fun s -> s) f)
  let is_deleted_family = (fun f -> f.Def.fam_index = Adef.ifam_of_int (-1))
  let get_father = (fun c -> Adef.father c)
  let get_mother = (fun c -> Adef.mother c)
  let get_parent_array = (fun c -> Adef.parent_array c)
  let gen_couple_of_couple = (fun c -> c)
  let get_children = (fun d -> d.Def.children)
  let gen_descend_of_descend = fun d -> d

  let get_fam1 = (fun p -> p.fam1)
  let get_cpl1 = (fun p -> p.cpl1)
  let get_des1 = (fun p -> p.des1)

  let set_fam1 = (fun p v -> p.fam1 <- v)
  let set_cpl1 = (fun p v -> p.cpl1 <- v)
  let set_des1 = (fun p v -> p.des1 <- v)

(* end *)

(* module Base = struct *)

  let no_person empty_string ip =
    {first_name = empty_string; surname = empty_string; occ = 0;
     image = empty_string; first_names_aliases = []; surnames_aliases = [];
     public_name = empty_string; qualifiers = []; titles = []; rparents = [];
     related = []; aliases = []; occupation = empty_string; sex = Neuter;
     access = Private; birth = Adef.cdate_None; birth_place = empty_string;
     birth_note = empty_string; birth_src = empty_string;
     baptism = Adef.cdate_None; baptism_place = empty_string;
     baptism_note = empty_string; baptism_src = empty_string;
     death = DontKnowIfDead; death_place = empty_string;
     death_note = empty_string; death_src = empty_string;
     burial = UnknownBurial; burial_place = empty_string;
     burial_note = empty_string; burial_src = empty_string; pevents = [];
     notes = empty_string; psources = empty_string; key_index = ip}
  let no_ascend = { parents = None; consang = Adef.no_consang }
  let no_union = { family = [| |] }

  let close_base base = base.func.cleanup ()

  let empty_person (base : base) (ip : iper) : person =
    (base, Adef.int_of_iper ip, { per1 = Some (no_person (Adef.istr_of_int 0) ip)
               ; asc1 = Some no_ascend
               ; uni1 = Some no_union } )

  let person_of_gen_person
      base ((p, a, u) : (iper, istr) gen_person * ifam gen_ascend * ifam gen_union) : person =
    (base, 0, { per1 = Some p
              ; asc1 = Some a
              ; uni1 = Some u } )

  let family_of_gen_family base (f, c, d) =
    (base, 0, { fam1 = Some f
               ; cpl1 = Some c
               ; des1 = Some d } )

  let poi base i =
    (base, Adef.int_of_iper i, { per1 = None; asc1 = None; uni1 = None} )

  let foi base i =
    (base, Adef.int_of_ifam i, { fam1 = None; cpl1 = None; des1 = None } )

  let sou base i = base.data.strings.get (Adef.int_of_istr i)

  let nb_of_persons base = base.data.persons.len

  let nb_of_families base =  base.data.families.len

  let patch_person base ip p = base.func.Dbdisk.patch_person ip p

  let patch_ascend base ip a = base.func.Dbdisk.patch_ascend ip a

  let patch_union base ip u = base.func.Dbdisk.patch_union ip u

  let patch_family base ifam f = base.func.Dbdisk.patch_family ifam f

  let patch_descend base ifam d = base.func.Dbdisk.patch_descend ifam d

  let patch_couple base ifam c = base.func.Dbdisk.patch_couple ifam c

  let patch_name base s ip = base.func.Dbdisk.patch_name s ip

  let patch_key _base _ip _fn _sn _occ = ()

  let delete_key _base _fn _sn _occ = ()

  let insert_string base s = base.func.Dbdisk.insert_string s

  let commit_patches base = base.func.Dbdisk.commit_patches ()

  let commit_notes base = base.func.Dbdisk.commit_notes

  let is_patched_person base ip = base.func.Dbdisk.is_patched_person ip

  let patched_ascends base = base.func.Dbdisk.patched_ascends ()

  (* FIXME *)
  (* let delete_family base ifam = C_base.delete_family self ifam *)

  let person_of_key base = base.func.Dbdisk.person_of_key

  let persons_of_name base = base.func.Dbdisk.persons_of_name

  let persons_of_first_name base = base.func.Dbdisk.persons_of_first_name

  let persons_of_surname base = base.func.Dbdisk.persons_of_surname

  let base_visible_get base f =
    base.data.visible.v_get
      (fun p -> f (base, 0, { per1 = Some p ; asc1 = None ; uni1 = None }))

  let base_visible_write base = base.data.visible.v_write ()

  let base_particles base = base.data.particles

  (* FIXME !!! *)
  let base_strings_of_first_name base s = base.func.strings_of_fsname s

  (* FIXME !!! *)
  let base_strings_of_surname base s = base.func.strings_of_fsname s

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

  let persons_array base =
    let get i = base.data.persons.get i in
    let set i p = base.data.persons.set i p in
    get, set

  let ascends_array base =
    let fget i = (base.data.ascends.get i).parents in
    let cget i = (base.data.ascends.get i).consang in
    let cset i v =
      base.data.ascends.set i {(base.data.ascends.get i) with consang = v}
    in
    fget, cget, cset, None

  let base_notes_read base fnotes = base.data.bnotes.nread fnotes RnAll

  let base_notes_read_first_line base fnotes = base.data.bnotes.nread fnotes Rn1Ln

  let base_notes_are_empty base fnotes = base.data.bnotes.nread fnotes RnDeg = ""

  let base_notes_origin_file base = base.data.bnotes.norigin_file

  let base_notes_dir _base = "notes_d"

  let base_wiznotes_dir _base = "wiznotes"

  let date_of_last_change base =
    let s =
      let bdir = base.data.bdir in
      try Unix.stat (Filename.concat bdir "patches") with
        Unix.Unix_error (_, _, _) ->
        Unix.stat (Filename.concat bdir "base")
    in
    s.Unix.st_mtime

  let apply base f = f base

  let open_base bname : base =
    let bname = if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb" in
    Database.opendb bname

(* end *)
