(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk
open Def

type istr = Type.istr
type ifam = Type.ifam
type iper = Type.iper

let string_of_iper = string_of_int
let string_of_ifam = string_of_int
let string_of_istr = string_of_int

let iper_of_string = int_of_string
let ifam_of_string = int_of_string
let istr_of_string = int_of_string

let cache f a (get, set) x =
  match get x with
  | Some v -> v
  | None -> let v = f a in set x (Some v) ; v

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
let no_ascend = {parents = None; consang = Adef.no_consang}
let no_union = {family = [| |]}

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

type person = dsk_base * int * person_dat
and person_dat =
  { mutable p : dsk_person option;
    mutable a : dsk_ascend option;
    mutable u : dsk_union option }

let cache_per f (base, i, p) =
  f (cache base.data.persons.get i ((fun p -> p.p), (fun p v -> p.p <- v)) p)

let cache_asc f (base, i, p) =
  f (cache base.data.ascends.get i ((fun p -> p.a), (fun p v -> p.a <- v)) p)

let cache_uni f (base, i, p) =
  f (cache base.data.unions.get i ((fun p -> p.u), (fun p v -> p.u <- v)) p)

let dsk_person_of_person = cache_per (fun p -> p)
let gen_person_of_person = cache_per (fun p -> p)
let get_access = cache_per (fun p -> p.Def.access)
let get_aliases = cache_per (fun p -> p.Def.aliases)
let get_baptism = cache_per (fun p -> p.Def.baptism)
let get_baptism_note = cache_per (fun p -> p.Def.baptism_note)
let get_baptism_place = cache_per (fun p -> p.Def.baptism_place)
let get_baptism_src = cache_per (fun p -> p.Def.baptism_src)
let get_birth = cache_per (fun p -> p.Def.birth)
let get_birth_note = cache_per (fun p -> p.Def.birth_note)
let get_birth_place = cache_per (fun p -> p.Def.birth_place)
let get_birth_src = cache_per (fun p -> p.Def.birth_src)
let get_burial = cache_per (fun p -> p.Def.burial)
let get_burial_note = cache_per (fun p -> p.Def.burial_note)
let get_burial_place = cache_per (fun p -> p.Def.burial_place)
let get_burial_src = cache_per (fun p -> p.Def.burial_src)
let get_consang = cache_asc (fun a -> a.Def.consang)
let get_death = cache_per (fun p -> p.Def.death)
let get_death_note = cache_per (fun p -> p.Def.death_note)
let get_death_place = cache_per (fun p -> p.Def.death_place)
let get_death_src = cache_per (fun p -> p.Def.death_src)
let get_family = cache_uni (fun u -> u.Def.family)
let get_first_name = cache_per (fun p -> p.Def.first_name)
let get_first_names_aliases = cache_per (fun p -> p.Def.first_names_aliases)
let get_image = cache_per (fun p -> p.Def.image)
let get_iper = cache_per (fun p -> p.Def.key_index)
let get_notes = cache_per (fun p -> p.Def.notes)
let get_occ = cache_per (fun p -> p.Def.occ)
let get_occupation = cache_per (fun p -> p.Def.occupation)
let get_parents = cache_asc (fun a -> a.Def.parents)
let get_pevents = cache_per (fun p -> p.Def.pevents)
let get_psources = cache_per (fun p -> p.Def.psources)
let get_public_name = cache_per (fun p -> p.Def.public_name)
let get_qualifiers = cache_per (fun p -> p.Def.qualifiers)
let get_related = cache_per (fun p -> p.Def.related)
let get_rparents = cache_per (fun p -> p.Def.rparents)
let get_sex = cache_per (fun p -> p.Def.sex)
let get_surname = cache_per (fun p -> p.Def.surname)
let get_surnames_aliases = cache_per (fun p -> p.Def.surnames_aliases)
let get_titles = cache_per (fun p -> p.Def.titles)

type family_dat =
  { mutable f : dsk_family option;
    mutable c : dsk_couple option;
    mutable d : dsk_descend option }
type family = dsk_base * int * family_dat

let cache_fam f (base, i, d) =
  f (cache base.data.families.get i ((fun f -> f.f), (fun f v -> f.f <- v)) d)

let cache_cpl f (base, i, d) =
  f (cache base.data.couples.get i ((fun f -> f.c), (fun f v -> f.c <- v)) d)

let cache_des f (base, i, d) =
  f (cache base.data.descends.get i ((fun f -> f.d), (fun f v -> f.d <- v)) d)

let gen_couple_of_couple = cache_cpl (fun c -> c)
let gen_descend_of_descend = cache_des (fun d -> d)
let gen_family_of_family = cache_fam (fun f -> f)
let get_children = cache_des (fun d -> d.Def.children)
let get_comment = cache_fam (fun f -> f.Def.comment)
let get_ifam = cache_fam (fun f -> f.Def.fam_index)
let get_divorce = cache_fam (fun f -> f.Def.divorce)
let get_father = cache_cpl (fun c -> Adef.father c)
let get_fevents = cache_fam (fun f -> f.Def.fevents)
let get_fsources = cache_fam (fun f -> f.Def.fsources)
let get_marriage = cache_fam (fun f -> f.Def.marriage)
let get_marriage_note = cache_fam (fun f -> f.Def.marriage_note)
let get_marriage_place = cache_fam (fun f -> f.Def.marriage_place)
let get_marriage_src = cache_fam (fun f -> f.Def.marriage_src)
let get_mother = cache_cpl (fun c -> Adef.mother c)
let get_origin_file = cache_fam (fun f -> f.Def.origin_file)
let get_parent_array = cache_cpl (fun c -> Adef.parent_array c)
let get_relation = cache_fam (fun f -> f.Def.relation)
let get_witnesses = cache_fam (fun f -> f.Def.witnesses)

type base = dsk_base

let base_strings_of_first_name_or_surname base s = base.func.strings_of_fsname s
let open_base bname : base =
  let bname = if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb" in
  Database.opendb bname

let close_base base = base.func.cleanup ()
let empty_person base ip =
  (base, Type.int_of_iper ip, {p = Some (no_person (Type.istr_of_int 0) ip);a = Some no_ascend; u = Some no_union})
let person_of_gen_person base (p, a, u) =
  (base, 0, {p = Some p; a = Some a; u = Some u})
let family_of_gen_family base (f, c, d) =
  (base, 0, {f = Some f; c = Some c; d = Some d})
let poi base i =
  (base, Type.int_of_iper i,{p = None; a = None; u = None})
let poi_batch base = List.map (poi base)

let no_family empty_string ifam =
  { marriage = Adef.cdate_None
  ; marriage_place = empty_string
  ; marriage_note = empty_string
  ; marriage_src = empty_string
  ; witnesses = [||]
  ; relation = Def.NoMention
  ; divorce = Def.NotDivorced
  ; fevents = []
  ; comment = empty_string
  ; origin_file = empty_string
  ; fsources = empty_string
  ; fam_index = ifam
  }

let no_couple =
  Adef.couple dummy_iper dummy_iper

let empty_family base i =
  ( base, Type.int_of_ifam i
  , { f = Some (no_family (Type.istr_of_int 0) i)
    ; c = Some no_couple
    ; d = Some { Def.children = [||] } })

let foi base i =
  (base, Type.int_of_ifam i, {f = None; c = None; d = None})
let foi_batch base = List.map (foi base)

let sou base i = base.data.strings.get (Type.int_of_istr i)
let nb_of_persons base = base.data.persons.len
let nb_of_families base = base.data.families.len
let patch_ascend base ip a = base.func.Dbdisk.patch_ascend ip a
let patch_union base ip u = base.func.Dbdisk.patch_union ip u
let patch_family base ifam f = base.func.Dbdisk.patch_family ifam f
let patch_descend base ifam d = base.func.Dbdisk.patch_descend ifam d
let patch_couple base ifam c = base.func.Dbdisk.patch_couple ifam c
let insert_string base s = base.func.Dbdisk.insert_string s
let commit_patches base = base.func.Dbdisk.commit_patches ()
let commit_notes base s = base.func.Dbdisk.commit_notes s

let patched_ascends base = base.func.Dbdisk.patched_ascends ()
let person_of_key base = base.func.Dbdisk.person_of_key
let persons_of_name base = base.func.Dbdisk.persons_of_name
let persons_of_first_name base = base.func.Dbdisk.persons_of_first_name
let persons_of_surname base = base.func.Dbdisk.persons_of_surname
let base_visible_get base f =
  base.data.visible.v_get
    (fun p -> f ( (base, 0, {p = Some p; a = None; u = None})))

let base_visible_write base = base.data.visible.v_write ()
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
let persons_array base =
  let get i = base.data.persons.get i  in
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
    try Unix.stat (Filename.concat bdir "patches")
    with Unix.Unix_error (_, _, _) -> Unix.stat (Filename.concat bdir "base")
  in
  s.Unix.st_mtime

let apply_base1 base f = f base

let insert_family base = function
  | (_, _, { f = Some f ; c = Some c ; d = Some d }) ->
    let ifam = nb_of_families base in
    patch_family base ifam f ;
    patch_couple base ifam c ;
    patch_descend base ifam d ;
    ifam
  | _ -> assert false

let delete_family base ifam =
  let cpl = Adef.couple dummy_iper dummy_iper in
  let fam =
    let empty = insert_string base "" in
    {marriage = Adef.cdate_None; marriage_place = empty;
     marriage_note = empty; marriage_src = empty; relation = Married;
     divorce = NotDivorced; fevents = []; witnesses = [| |];
     comment = empty; origin_file = empty; fsources = empty;
     fam_index = dummy_ifam}
  in
  let des = {children = [| |]} in
  patch_family base ifam fam;
  patch_couple base ifam cpl;
  patch_descend base ifam des

let nobtit base allowed_titles denied_titles p =
  let list = get_titles p in
  match Lazy.force allowed_titles with
    [] -> list
  | allowed_titles ->
    let list =
      List.fold_right
        (fun t l ->
           let id = Name.lower (sou base t.t_ident) in
           let pl = Name.lower (sou base t.t_place) in
           if pl = "" then
             if List.mem id allowed_titles then t :: l else l
           else if
             List.mem (id ^ "/" ^ pl) allowed_titles ||
             List.mem (id ^ "/*") allowed_titles
           then
             t :: l
           else l)
        list []
    in
    match Lazy.force denied_titles with
      [] -> list
    | denied_titles ->
      List.filter
        (fun t ->
           let id = Name.lower (sou base t.t_ident) in
           let pl = Name.lower (sou base t.t_place) in
           if List.mem (id ^ "/" ^ pl) denied_titles ||
              List.mem ("*/" ^ pl) denied_titles
           then
             false
           else true)
        list

let p_first_name base p = Mutil.nominative (sou base (get_first_name p))
let p_surname base p = Mutil.nominative (sou base (get_surname p))

let husbands base gp =
  let p = poi base gp.key_index in
  Array.map
    (fun ifam ->
       let fam = foi base ifam in
       let husband = poi base (get_father fam) in
       let husband_surname = p_surname base husband in
       let husband_surnames_aliases =
         List.map (sou base) (get_surnames_aliases husband)
       in
       husband_surname, husband_surnames_aliases)
    (get_family p)

let father_titles_places base p nobtit =
  match get_parents (poi base p.key_index) with
  | Some ifam ->
    let fam = foi base ifam in
    let fath = poi base (get_father fam) in
    List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> []

let gen_gen_person_misc_names base p nobtit nobtit_fun =
  let sou = sou base in
  Futil.gen_person_misc_names (sou p.first_name) (sou p.surname)
    (sou p.public_name) (List.map sou p.qualifiers) (List.map sou p.aliases)
    (List.map sou p.first_names_aliases) (List.map sou p.surnames_aliases)
    (List.map (Futil.map_title_strings sou) nobtit)
    (if p.sex = Female then Array.to_list (husbands base p) else [])
    (father_titles_places base p nobtit_fun)

let gen_person_misc_names base p nobtit =
  gen_gen_person_misc_names base p (nobtit p)
    (fun p -> nobtit (gen_person_of_person p))

let person_misc_names base p nobtit =
  gen_gen_person_misc_names base (gen_person_of_person p) (nobtit p) nobtit

let patch_misc_names base ip p =
  List.iter
    (fun s -> base.func.Dbdisk.patch_name s ip)
    (gen_gen_person_misc_names base p p.titles (fun p -> (gen_person_of_person p).titles))

let patch_person base ip (p : (iper, iper, istr) Def.gen_person) =
  base.func.Dbdisk.patch_person ip p ;
  let s = sou base p.first_name ^ " " ^ sou base p.surname in
  base.func.Dbdisk.patch_name s ip ;
  patch_misc_names base ip p ;
  Array.iter
    begin fun i ->
      let cpl = base.data.couples.get i in
      let m = Adef.mother cpl in
      let f = Adef.father cpl in
      patch_misc_names base m (gen_person_of_person @@ poi base m) ;
      patch_misc_names base f (gen_person_of_person @@ poi base f) ;
      Array.iter
        begin
          fun i -> patch_misc_names base i (gen_person_of_person @@ poi base i)
        end
        (base.data.descends.get i).children
    end
    (base.data.unions.get ip).Def.family

let insert_person base = function
  | (_, _, { p = Some p ; a = Some a ; u = Some u }) ->
    let iper = nb_of_persons base in
    let p = { p with key_index = iper } in
    patch_ascend base iper a ;
    patch_union base iper u ;
    patch_person base iper p ;
    iper
  | _ -> assert false

module Collection = struct

  type 'a t =
    { length : int
    ; get : int -> 'a option
    }

  let map (fn : 'a -> 'b) c =
    { length = c.length
    ; get = (fun i ->  match c.get i with Some x -> Some (fn x) | None -> None)
    }

  let length { length ; _ } = length

  let iter fn { get ; length } =
    for i = 0 to length - 1 do match get i with Some x -> fn x | None -> () done

  let iteri fn { get ; length } =
    for i = 0 to length - 1 do match get i with Some x -> fn i x | None -> () done

  let fold ?from ?until fn acc { get ; length } =
    let from = match from with Some x -> x | None -> 0 in
    let until = match until with Some x -> x + 1 | None -> length in
    let rec loop acc i =
      if i = until then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc from

  let fold_until continue fn acc { get ; length } =
    let rec loop acc i =
      if not (continue acc) || i = length then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc 0

  let iterator { get ; length } =
    let cursor = ref 0 in
    let rec next () =
      if !cursor < length then
        match get !cursor with
        | None -> incr cursor ; next ()
        |  v -> incr cursor ; v
      else None
    in
    next

end

module Marker = struct

  type ('k, 'v) t =
    { get : 'k -> 'v
    ; set : 'k -> 'v -> unit
    }

  let make (k : 'a -> 'k) (c : 'a Collection.t) (i : 'v) : ('a, 'v) t =
    let a = Array.make c.Collection.length i in
    { get = (fun x -> Array.get a (k x) )
    ; set = (fun x v -> Array.set a (k x) v) }

  let get ({ get ; _ } : _ t) k = get k
  let set ({ set ; _ } : _ t) k = set k

end

let pers_colletion fn base =
  { Collection.length = nb_of_persons base
  ; get = (fun i ->
        let p = poi base i in
        if get_iper p = dummy_iper then None else Some (fn p))
  }

let ipers = pers_colletion get_iper
let persons = pers_colletion (fun p -> p)

let person_marker c i = Marker.make (fun p -> (Type.int_of_iper @@ get_iper p)) c i
let iper_marker c i = Marker.make Type.int_of_iper c i

let fam_collection fn base =
  { Collection.length = nb_of_families base
  ; get = (fun i ->
        let f = foi base i in
        if get_ifam f = dummy_ifam then None else Some (fn f))
  }

let ifams = fam_collection get_ifam
let families = fam_collection (fun f -> f)

let dummy_collection _ =
  { Collection.length = -1
  ; get = fun _ -> None
  }

let family_marker c i = Marker.make (fun f -> (Type.int_of_ifam @@ get_ifam f)) c i
let ifam_marker c i = Marker.make Type.int_of_ifam c i

let dummy_marker (_ : 'a) (v : 'b) : ('a, 'b) Marker.t =
  { Marker.get = begin fun _ -> v end
  ; set = begin fun _ _ -> () end
  }
