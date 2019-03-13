(* $Id: gwdb.ml,v 5.244 2012-01-18 20:49:57 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk
open Db2disk
open Def

type 'istr gen_string_person_index =
  'istr Dbdisk.string_person_index =
    { find : 'istr -> iper list;
      cursor : string -> 'istr;
      next : 'istr -> 'istr }

let milazy_force f a (get, set) p =
  match get p with
    Some v -> v
  | None -> let v = f a in set p (Some v); v

let ht_find ht i = Hashtbl.find_opt ht i

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

(* Strings - common definitions *)

type istr =
    Istr of dsk_istr
  | Istr2 of db2 * (string * string) * int
  | Istr2New of db2 * string

type 'a istr_fun =
  { is_empty_string : 'a -> bool;
    is_quest_string : 'a -> bool;
    un_istr : 'a -> Adef.istr;
    un_istr2 : 'a -> string }

type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

let eq_istr i1 i2 =
  match i1, i2 with
    Istr i1, Istr i2 -> Adef.int_of_istr i1 = Adef.int_of_istr i2
  | Istr2 (_, (f11, f12), i1), Istr2 (_, (f21, f22), i2) ->
      i1 = i2 && f11 = f21 && f12 = f22
  | Istr2New (_, s1), Istr2New (_, s2) -> s1 = s2
  | Istr2 (db2, f, pos), Istr2New (_, s2) -> string_of_istr2 db2 f pos = s2
  | Istr2New (_, s1), Istr2 (db2, f, pos) -> s1 = string_of_istr2 db2 f pos
  | _ -> failwith "eq_istr"

(* Strings - implementation database 1 *)

let istr1_fun =
  {is_empty_string = (fun istr -> Adef.int_of_istr istr = 0);
   is_quest_string = (fun istr -> Adef.int_of_istr istr = 1);
   un_istr = (fun i -> i); un_istr2 = fun _i -> failwith "un_istr2 1"}

(* Strings - implementation database 2 *)

let istr2_fun =
  {is_empty_string =
    (fun (db2, path, pos) -> string_of_istr2 db2 path pos = "");
   is_quest_string =
     (fun (db2, path, pos) -> string_of_istr2 db2 path pos = "?");
   un_istr = (fun _ -> failwith "un_istr");
   un_istr2 = fun (db2, path, pos) -> string_of_istr2 db2 path pos}

let istr2new_fun =
  {is_empty_string = (fun (_db2, s) -> s = "");
   is_quest_string = (fun (_db2, s) -> s = "?");
   un_istr = (fun (_db2, _s) -> failwith "un_istr");
   un_istr2 = fun (_db2, s) -> s}

(* Strings - user functions *)

let wrap_istr f g h =
  function
    Istr istr -> f istr1_fun istr
  | Istr2 (db2, path, pos) -> g istr2_fun (db2, path, pos)
  | Istr2New (db2, s) -> h istr2new_fun (db2, s)

let is_empty_string i = let f pf = pf.is_empty_string in wrap_istr f f f i
let is_quest_string i = let f pf = pf.is_quest_string in wrap_istr f f f i
let un_istr i = let f pf = pf.un_istr in wrap_istr f f f i
let un_istr2 i = let f pf = pf.un_istr2 in wrap_istr f f f i

(* String person index - common definitions *)

type string_person_index =
    Spi of dsk_istr gen_string_person_index
  | Spi2 of db2 * string_person_index2

type 'a spi =
  { spi_first : 'a -> string -> istr;
    spi_next : 'a -> istr -> bool -> istr * int;
    spi_find : 'a -> istr -> iper list }

(* String person index - implementation database 1 *)

let spi1_fun =
  {spi_first = (fun spi s -> Istr (spi.cursor s));
   spi_next =
     (fun spi istr _need_whole_list ->
        match istr with
          Istr s -> Istr (spi.next s), 1
        | _ -> failwith "not impl spi_next");
   spi_find =
     fun spi s ->
       match s with
         Istr s -> spi.find s
       | _ -> failwith "not impl spi_find"}

(* String person index - implementation database 2 *)

let spi2_fun =
  {spi_first =
    (fun (db2, spi) s ->
       let f1 = "person" in
       let f2 = if spi.is_first_name then "first_name" else "surname" in
       match spi2_first db2 spi (f1, f2) s with
         Sp pos -> Istr2 (db2, (f1, f2), pos)
       | SpNew s2 -> Istr2New (db2, s2));
   spi_next =
     (fun (db2, spi) _istr need_whole_list ->
        let f1 = "person" in
        let f2 = if spi.is_first_name then "first_name" else "surname" in
        let (sp, dlen) = spi2_next db2 spi (f1, f2) need_whole_list in
        let r =
          match sp with
            Sp pos -> Istr2 (db2, (f1, f2), pos)
          | SpNew s2 -> Istr2New (db2, s2)
        in
        r, dlen);
   spi_find =
     fun (_db2, spi) s ->
       match s with
         Istr2 (db2, (f1, f2), pos) -> spi2_find db2 spi (f1, f2) pos
       | Istr2New (db2, s) -> spi2gen_find db2 spi s
       | _ -> failwith "not impl spi_find"}

(* String person index - user functions *)

let wrap_spi f g =
  function
    Spi spi -> f spi1_fun spi
  | Spi2 (db2, spi2) -> g spi2_fun (db2, spi2)

let spi_find = let f pf = pf.spi_find in wrap_spi f f
let spi_first = let f pf = pf.spi_first in wrap_spi f f
let spi_next = let f pf = pf.spi_next in wrap_spi f f

(* Persons - common definitions *)

type person =
    Person of dsk_base * int * person1_dat
  | Person2 of db2 * int * person2_dat
and person1_dat =
  { mutable per1 : dsk_person option;
    mutable asc1 : dsk_ascend option;
    mutable uni1 : dsk_union option }
and person2_dat =
  { mutable per2 : (iper, string) gen_person option option;
    mutable asc2 : ifam gen_ascend option option;
    mutable uni2 : ifam gen_union option option }

type ('p, 'a, 'u) person_fun =
  { get_access : 'p -> access;
    get_aliases : 'p -> istr list;
    get_baptism : 'p -> cdate;
    get_baptism_place : 'p -> istr;
    get_baptism_note : 'p -> istr;
    get_baptism_src : 'p -> istr;
    get_birth : 'p -> cdate;
    get_birth_place : 'p -> istr;
    get_birth_note : 'p -> istr;
    get_birth_src : 'p -> istr;
    get_burial : 'p -> Def.burial;
    get_burial_place : 'p -> istr;
    get_burial_note : 'p -> istr;
    get_burial_src : 'p -> istr;
    get_death : 'p -> Def.death;
    get_death_place : 'p -> istr;
    get_death_note : 'p -> istr;
    get_death_src : 'p -> istr;
    get_first_name : 'p -> istr;
    get_first_names_aliases : 'p -> istr list;
    get_image : 'p -> istr;
    get_key_index : 'p -> iper;
    get_notes : 'p -> istr;
    get_occ : 'p -> int;
    get_occupation : 'p -> istr;
    get_psources : 'p -> istr;
    get_public_name : 'p -> istr;
    get_qualifiers : 'p -> istr list;
    get_related : 'p -> iper list;
    get_rparents : 'p -> relation list;
    get_sex : 'p -> Def.sex;
    get_surname : 'p -> istr;
    get_surnames_aliases : 'p -> istr list;
    get_titles : 'p -> title list;
    get_pevents : 'p -> pers_event list;
    gen_person_of_person : 'p -> (iper, istr) Def.gen_person;
    dsk_person_of_person : 'p -> Dbdisk.dsk_person;
    get_consang : 'a -> Adef.fix;
    get_parents : 'a -> ifam option;
    get_family : 'u -> ifam array }

(* Persons - implementation database 1 *)

let person1_fun =
  {get_access = (fun p -> p.Def.access);
   get_aliases = (fun p -> List.map (fun i -> Istr i) p.Def.aliases);
   get_baptism = (fun p -> p.Def.baptism);
   get_baptism_place = (fun p -> Istr p.Def.baptism_place);
   get_baptism_note = (fun p -> Istr p.Def.baptism_note);
   get_baptism_src = (fun p -> Istr p.Def.baptism_src);
   get_birth = (fun p -> p.Def.birth);
   get_birth_place = (fun p -> Istr p.Def.birth_place);
   get_birth_note = (fun p -> Istr p.Def.birth_note);
   get_birth_src = (fun p -> Istr p.Def.birth_src);
   get_burial = (fun p -> p.Def.burial);
   get_burial_place = (fun p -> Istr p.Def.burial_place);
   get_burial_note = (fun p -> Istr p.Def.burial_note);
   get_burial_src = (fun p -> Istr p.Def.burial_src);
   get_death = (fun p -> p.Def.death);
   get_death_place = (fun p -> Istr p.Def.death_place);
   get_death_note = (fun p -> Istr p.Def.death_note);
   get_death_src = (fun p -> Istr p.Def.death_src);
   get_first_name = (fun p -> Istr p.Def.first_name);
   get_first_names_aliases =
     (fun p -> List.map (fun i -> Istr i) p.Def.first_names_aliases);
   get_image = (fun p -> Istr p.Def.image);
   get_key_index = (fun p -> p.Def.key_index);
   get_notes = (fun p -> Istr p.Def.notes); get_occ = (fun p -> p.Def.occ);
   get_occupation = (fun p -> Istr p.Def.occupation);
   get_psources = (fun p -> Istr p.Def.psources);
   get_public_name = (fun p -> Istr p.Def.public_name);
   get_qualifiers = (fun p -> List.map (fun i -> Istr i) p.Def.qualifiers);
   get_related = (fun p -> p.Def.related);
   get_rparents =
     (fun p ->
        List.map (Futil.map_relation_ps (fun x -> x) (fun i -> Istr i))
          p.Def.rparents);
   get_sex = (fun p -> p.Def.sex);
   get_surname = (fun p -> Istr p.Def.surname);
   get_surnames_aliases =
     (fun p -> List.map (fun i -> Istr i) p.Def.surnames_aliases);
   get_titles =
     (fun p ->
        List.map (fun t -> Futil.map_title_strings (fun i -> Istr i) t)
          p.Def.titles);
   get_pevents =
     (fun p ->
        List.map (fun t -> Futil.map_pers_event (fun x -> x) (fun i -> Istr i) t)
          p.Def.pevents);
   gen_person_of_person =
     (fun p -> Futil.map_person_ps (fun p -> p) (fun s -> Istr s) p);
   dsk_person_of_person = (fun p -> p);
   get_consang = (fun a -> a.Def.consang);
   get_parents = (fun a -> a.Def.parents); get_family = fun u -> u.Def.family}

(* Persons - implementation database 2 *)

let make_istr2 db2 path i = Istr2 (db2, path, get_field_acc db2 i path)

let get_list_field db2 i f1f2 =
  let pos = get_field_acc db2 i f1f2 in
  if pos = -1 then [] else get_field_data db2 pos f1f2 "data2.ext"

let person2_fun =
  let rec self =
    {get_access = (fun (db2, i) -> get_field db2 i ("person", "access"));
     get_aliases =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "aliases") in
          List.map (fun pos -> Istr2 (db2, ("person", "aliases"), pos)) list);
     get_baptism = (fun (db2, i) -> get_field db2 i ("person", "baptism"));
     get_baptism_place =
       (fun (db2, i) -> make_istr2 db2 ("person", "baptism_place") i);
     get_baptism_note =
       (fun (db2, i) -> make_istr2 db2 ("person", "baptism_note") i);
     get_baptism_src =
       (fun (db2, i) -> make_istr2 db2 ("person", "baptism_src") i);
     get_birth = (fun (db2, i) -> get_field db2 i ("person", "birth"));
     get_birth_place =
       (fun (db2, i) -> make_istr2 db2 ("person", "birth_place") i);
     get_birth_note =
       (fun (db2, i) -> make_istr2 db2 ("person", "birth_note") i);
     get_birth_src =
       (fun (db2, i) -> make_istr2 db2 ("person", "birth_src") i);
     get_burial = (fun (db2, i) -> get_field db2 i ("person", "burial"));
     get_burial_place =
       (fun (db2, i) -> make_istr2 db2 ("person", "burial_place") i);
     get_burial_note =
       (fun (db2, i) -> make_istr2 db2 ("person", "burial_note") i);
     get_burial_src =
       (fun (db2, i) -> make_istr2 db2 ("person", "burial_src") i);
     get_death = (fun (db2, i) -> get_field db2 i ("person", "death"));
     get_death_place =
       (fun (db2, i) -> make_istr2 db2 ("person", "death_place") i);
     get_death_note =
       (fun (db2, i) -> make_istr2 db2 ("person", "death_note") i);
     get_death_src =
       (fun (db2, i) -> make_istr2 db2 ("person", "death_src") i);
     get_first_name =
       (fun (db2, i) -> make_istr2 db2 ("person", "first_name") i);
     get_first_names_aliases =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "first_names_aliases") in
          List.map
            (fun pos -> Istr2 (db2, ("person", "first_names_aliases"), pos))
            list);
     get_image = (fun (db2, i) -> make_istr2 db2 ("person", "image") i);
     get_key_index = (fun (_db2, i) -> Adef.iper_of_int i);
     get_notes = (fun (db2, i) -> make_istr2 db2 ("person", "notes") i);
     get_occ = (fun (db2, i) -> get_field db2 i ("person", "occ"));
     get_occupation =
       (fun (db2, i) -> make_istr2 db2 ("person", "occupation") i);
     get_psources = (fun (db2, i) -> make_istr2 db2 ("person", "psources") i);
     get_public_name =
       (fun (db2, i) -> make_istr2 db2 ("person", "public_name") i);
     get_qualifiers =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "qualifiers") in
          List.map (fun pos -> Istr2 (db2, ("person", "qualifiers"), pos))
            list);
     get_related =
       (fun (db2, i) ->
          let pos = get_field_acc db2 i ("person", "related") in
          let rec loop list pos =
            if pos = -1 then List.rev list
            else
              let (ip, pos) =
                get_field_2_data db2 pos ("person", "related") "data"
              in
              loop (ip :: list) pos
          in
          loop [] pos);
     get_rparents =
       (fun (db2, i) ->
          let pos = get_field_acc db2 i ("person", "rparents") in
          if pos = -1 then []
          else
            let rl = get_field_data db2 pos ("person", "rparents") "data" in
            List.map
              (Futil.map_relation_ps (fun x -> x)
                 (fun _ -> Istr2 (db2, ("", ""), -1)))
              rl);
     get_sex = (fun (db2, i) -> get_field db2 i ("person", "sex"));
     get_surname = (fun (db2, i) -> make_istr2 db2 ("person", "surname") i);
     get_surnames_aliases =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "surnames_aliases") in
          List.map
            (fun pos -> Istr2 (db2, ("person", "surnames_aliases"), pos))
            list);
     get_titles =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "titles") in
          List.map
            (Futil.map_title_strings
               (fun pos -> Istr2 (db2, ("person", "titles"), pos)))
            list);
     get_pevents =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "pevents") in
          List.map
            (Futil.map_pers_event (fun x -> x)
               (fun pos -> Istr2 (db2, ("person", "pevents"), pos)))
            list);
     gen_person_of_person =
       (fun pp ->
          {first_name = self.get_first_name pp; surname = self.get_surname pp;
           occ = self.get_occ pp; image = self.get_image pp;
           public_name = self.get_public_name pp;
           qualifiers = self.get_qualifiers pp; aliases = self.get_aliases pp;
           first_names_aliases = self.get_first_names_aliases pp;
           surnames_aliases = self.get_surnames_aliases pp;
           titles = self.get_titles pp; rparents = self.get_rparents pp;
           related = self.get_related pp; occupation = self.get_occupation pp;
           sex = self.get_sex pp; access = self.get_access pp;
           birth = self.get_birth pp; birth_place = self.get_birth_place pp;
           birth_note = self.get_birth_note pp;
           birth_src = self.get_birth_src pp; baptism = self.get_baptism pp;
           baptism_place = self.get_baptism_place pp;
           baptism_note = self.get_baptism_note pp;
           baptism_src = self.get_baptism_src pp; death = self.get_death pp;
           death_place = self.get_death_place pp;
           death_note = self.get_death_note pp;
           death_src = self.get_death_src pp; burial = self.get_burial pp;
           burial_place = self.get_burial_place pp;
           burial_note = self.get_burial_note pp;
           burial_src = self.get_burial_src pp; pevents = self.get_pevents pp;
           notes = self.get_notes pp; psources = self.get_psources pp;
           key_index = self.get_key_index pp});
     dsk_person_of_person =
       (fun _p -> failwith "not impl dsk_person_of_person");
     get_consang =
       (fun (db2, i) ->
          match db2.consang_array with
            Some tab -> tab.(i)
          | None ->
              let f = "person", "consang" in
              if field_exists db2 f then get_field db2 i f
              else Adef.no_consang);
     get_parents =
       (fun (db2, i) ->
          match db2.parents_array with
            Some tab -> tab.(i)
          | None ->
              let pos = get_field_acc db2 i ("person", "parents") in
              if pos = -1 then None
              else
                Some (get_field_data db2 pos ("person", "parents") "data"));
     get_family =
       fun (db2, i) ->
         match db2.family_array with
           Some tab -> tab.(i)
         | None -> get_field db2 i ("person", "family")}
  in
  self

let person2gen_fun =
  {get_access = (fun (_db2, _i, p) -> p.Def.access);
   get_aliases =
     (fun (db2, _i, p) -> List.map (fun s -> Istr2New (db2, s)) p.Def.aliases);
   get_baptism = (fun (_db2, _i, p) -> p.Def.baptism);
   get_baptism_place =
     (fun (db2, _i, p) -> Istr2New (db2, p.Def.baptism_place));
   get_baptism_note = (fun (db2, _i, p) -> Istr2New (db2, p.Def.baptism_note));
   get_baptism_src = (fun (db2, _i, p) -> Istr2New (db2, p.Def.baptism_src));
   get_birth = (fun (_db2, _i, p) -> p.Def.birth);
   get_birth_place = (fun (db2, _i, p) -> Istr2New (db2, p.Def.birth_place));
   get_birth_note = (fun (db2, _i, p) -> Istr2New (db2, p.Def.birth_note));
   get_birth_src = (fun (db2, _i, p) -> Istr2New (db2, p.Def.birth_src));
   get_burial = (fun (_db2, _i, p) -> p.Def.burial);
   get_burial_place = (fun (db2, _i, p) -> Istr2New (db2, p.Def.burial_place));
   get_burial_note = (fun (db2, _i, p) -> Istr2New (db2, p.Def.burial_note));
   get_burial_src = (fun (db2, _i, p) -> Istr2New (db2, p.Def.burial_src));
   get_death = (fun (_db2, _i, p) -> p.Def.death);
   get_death_place = (fun (db2, _i, p) -> Istr2New (db2, p.Def.death_place));
   get_death_note = (fun (db2, _i, p) -> Istr2New (db2, p.Def.death_note));
   get_death_src = (fun (db2, _i, p) -> Istr2New (db2, p.Def.death_src));
   get_first_name = (fun (db2, _i, p) -> Istr2New (db2, p.Def.first_name));
   get_first_names_aliases =
     (fun (db2, _i, p) ->
        List.map (fun s -> Istr2New (db2, s)) p.Def.first_names_aliases);
   get_image = (fun (db2, _i, p) -> Istr2New (db2, p.Def.image));
   get_key_index = (fun (_db2, _i, p) -> p.Def.key_index);
   get_notes = (fun (db2, _i, p) -> Istr2New (db2, p.Def.notes));
   get_occ = (fun (_db2, _i, p) -> p.Def.occ);
   get_occupation = (fun (db2, _i, p) -> Istr2New (db2, p.Def.occupation));
   get_psources = (fun (db2, _i, p) -> Istr2New (db2, p.Def.psources));
   get_public_name = (fun (db2, _i, p) -> Istr2New (db2, p.Def.public_name));
   get_qualifiers =
     (fun (db2, _i, p) ->
        List.map (fun s -> Istr2New (db2, s)) p.Def.qualifiers);
   get_related = (fun (_db2, _i, p) -> p.Def.related);
   get_rparents =
     (fun (db2, _i, p) ->
        List.map (Futil.map_relation_ps (fun x -> x) (fun s -> Istr2New (db2, s)))
          p.Def.rparents);
   get_sex = (fun (_db2, _i, p) -> p.Def.sex);
   get_surname = (fun (db2, _i, p) -> Istr2New (db2, p.Def.surname));
   get_surnames_aliases =
     (fun (db2, _i, p) ->
        List.map (fun s -> Istr2New (db2, s)) p.Def.surnames_aliases);
   get_titles =
     (fun (db2, _i, p) ->
        List.map (fun t -> Futil.map_title_strings (fun s -> Istr2New (db2, s)) t)
          p.Def.titles);
   get_pevents =
     (fun (db2, _i, p) ->
        List.map
          (fun t ->
             Futil.map_pers_event (fun x -> x) (fun s -> Istr2New (db2, s)) t)
          p.Def.pevents);
   gen_person_of_person =
     (fun (db2, _i, p) ->
        Futil.map_person_ps (fun p -> p) (fun s -> Istr2New (db2, s)) p);
   dsk_person_of_person =
     (fun (_db2, _i, _p) -> failwith "not impl dsk_person_of_person (gen)");
   get_consang = (fun (_db2, _i, a) -> a.Def.consang);
   get_parents = (fun (_db2, _i, a) -> a.Def.parents);
   get_family = fun (_db2, _i, u) -> u.Def.family}

(* Persons - user functions *)

let get_set_per1 = (fun p -> p.per1), (fun p v -> p.per1 <- v)
let get_set_asc1 = (fun p -> p.asc1), (fun p v -> p.asc1 <- v)
let get_set_uni1 = (fun p -> p.uni1), (fun p v -> p.uni1 <- v)

let get_set_per2 = (fun p -> p.per2), (fun p v -> p.per2 <- v)
let get_set_asc2 = (fun p -> p.asc2), (fun p v -> p.asc2 <- v)
let get_set_uni2 = (fun p -> p.uni2), (fun p v -> p.uni2 <- v)

let wrap_per f g h =
  function
    Person (base, i, p) ->
      let per = milazy_force base.data.persons.get i get_set_per1 p in
      f person1_fun per
  | Person2 (db2, i, p) ->
      let per =
        milazy_force (ht_find db2.patches.h_person) (Adef.iper_of_int i)
          get_set_per2 p
      in
      match per with
        Some p -> h person2gen_fun (db2, i, p)
      | None -> g person2_fun (db2, i)

let wrap_asc f g h =
  function
    Person (base, i, p) ->
      let asc = milazy_force base.data.ascends.get i get_set_asc1 p in
      f person1_fun asc
  | Person2 (db2, i, p) ->
      let asc =
        milazy_force (ht_find db2.patches.h_ascend) (Adef.iper_of_int i)
          get_set_asc2 p
      in
      match asc with
        Some a -> h person2gen_fun (db2, i, a)
      | None -> g person2_fun (db2, i)

let wrap_uni f g h =
  function
    Person (base, i, p) ->
      let uni = milazy_force base.data.unions.get i get_set_uni1 p in
      f person1_fun uni
  | Person2 (db2, i, p) ->
      let uni =
        milazy_force (ht_find db2.patches.h_union) (Adef.iper_of_int i)
          get_set_uni2 p
      in
      match uni with
        Some u -> h person2gen_fun (db2, i, u)
      | None -> g person2_fun (db2, i)

let get_access p = let f pf = pf.get_access in wrap_per f f f p
let get_aliases p = let f pf = pf.get_aliases in wrap_per f f f p
let get_baptism p = let f pf = pf.get_baptism in wrap_per f f f p
let get_baptism_place p = let f pf = pf.get_baptism_place in wrap_per f f f p
let get_baptism_note p = let f pf = pf.get_baptism_note in wrap_per f f f p
let get_baptism_src p = let f pf = pf.get_baptism_src in wrap_per f f f p
let get_birth p = let f pf = pf.get_birth in wrap_per f f f p
let get_birth_place p = let f pf = pf.get_birth_place in wrap_per f f f p
let get_birth_note p = let f pf = pf.get_birth_note in wrap_per f f f p
let get_birth_src p = let f pf = pf.get_birth_src in wrap_per f f f p
let get_burial p = let f pf = pf.get_burial in wrap_per f f f p
let get_burial_place p = let f pf = pf.get_burial_place in wrap_per f f f p
let get_burial_note p = let f pf = pf.get_burial_note in wrap_per f f f p
let get_burial_src p = let f pf = pf.get_burial_src in wrap_per f f f p
let get_death p = let f pf = pf.get_death in wrap_per f f f p
let get_death_place p = let f pf = pf.get_death_place in wrap_per f f f p
let get_death_note p = let f pf = pf.get_death_note in wrap_per f f f p
let get_death_src p = let f pf = pf.get_death_src in wrap_per f f f p
let get_pevents p = let f pf = pf.get_pevents in wrap_per f f f p
let get_first_name p = let f pf = pf.get_first_name in wrap_per f f f p
let get_first_names_aliases p =
  let f pf = pf.get_first_names_aliases in wrap_per f f f p
let get_image p = let f pf = pf.get_image in wrap_per f f f p
let get_key_index p = let f pf = pf.get_key_index in wrap_per f f f p
let get_notes p = let f pf = pf.get_notes in wrap_per f f f p
let get_occ p = let f pf = pf.get_occ in wrap_per f f f p
let get_occupation p = let f pf = pf.get_occupation in wrap_per f f f p
let get_psources p = let f pf = pf.get_psources in wrap_per f f f p
let get_public_name p = let f pf = pf.get_public_name in wrap_per f f f p
let get_qualifiers p = let f pf = pf.get_qualifiers in wrap_per f f f p
let get_related p = let f pf = pf.get_related in wrap_per f f f p
let get_rparents p = let f pf = pf.get_rparents in wrap_per f f f p
let get_sex p = let f pf = pf.get_sex in wrap_per f f f p
let get_surname p = let f pf = pf.get_surname in wrap_per f f f p
let get_surnames_aliases p =
  let f pf = pf.get_surnames_aliases in wrap_per f f f p
let get_titles p = let f pf = pf.get_titles in wrap_per f f f p

let gen_person_of_person p =
  let f pf = pf.gen_person_of_person in wrap_per f f f p
let dsk_person_of_person p =
  let f pf = pf.dsk_person_of_person in wrap_per f f f p

let get_consang a =
  let f pf = pf.get_consang in
  match a with
    Person2 (db2, i, _) ->
      begin match db2.consang_array with
        Some tab -> tab.(i)
      | None -> wrap_asc f f f a
      end
  | _ -> wrap_asc f f f a
let get_parents a =
  let f pf = pf.get_parents in
  match a with
    Person2 (db2, i, _) ->
      begin match db2.parents_array with
        Some tab -> tab.(i)
      | None -> wrap_asc f f f a
      end
  | _ -> wrap_asc f f f a

let get_family u = let f pf = pf.get_family in wrap_uni f f f u

(* Families - common definitions *)

type family =
    Family of dsk_base * int * family1_dat
  | Family2 of db2 * int * family2_dat
and family1_dat =
  { mutable fam1 : dsk_family option;
    mutable cpl1 : dsk_couple option;
    mutable des1 : dsk_descend option }
and family2_dat =
  { mutable fam2 : (iper, string) gen_family option option;
    mutable cpl2 : iper gen_couple option option;
    mutable des2 : iper gen_descend option option }

type ('f, 'c, 'd) family_fun =
  { get_comment : 'f -> istr;
    get_divorce : 'f -> Def.divorce;
    get_fsources : 'f -> istr;
    get_fevents : 'f -> fam_event list;
    get_marriage : 'f -> cdate;
    get_marriage_place : 'f -> istr;
    get_marriage_note : 'f -> istr;
    get_marriage_src : 'f -> istr;
    get_origin_file : 'f -> istr;
    get_relation : 'f -> Def.relation_kind;
    get_witnesses : 'f -> iper array;
    gen_family_of_family : 'f -> (iper, istr) Def.gen_family;
    is_deleted_family : 'f -> bool;
    get_father : 'c -> iper;
    get_mother : 'c -> iper;
    get_parent_array : 'c -> iper array;
    gen_couple_of_couple : 'c -> iper Def.gen_couple;
    get_children : 'd -> iper array;
    gen_descend_of_descend : 'd -> iper Def.gen_descend }

(* Families - implementation database 1 *)

let family1_fun =
  {get_comment = (fun f -> Istr f.Def.comment);
   get_divorce = (fun f -> f.Def.divorce);
   get_fsources = (fun f -> Istr f.Def.fsources);
   get_fevents =
     (fun f ->
        List.map (fun t -> Futil.map_fam_event (fun x -> x) (fun i -> Istr i) t)
          f.Def.fevents);
   get_marriage = (fun f -> f.Def.marriage);
   get_marriage_place = (fun f -> Istr f.Def.marriage_place);
   get_marriage_note = (fun f -> Istr f.Def.marriage_note);
   get_marriage_src = (fun f -> Istr f.Def.marriage_src);
   get_origin_file = (fun f -> Istr f.Def.origin_file);
   get_relation = (fun f -> f.Def.relation);
   get_witnesses = (fun f -> f.Def.witnesses);
   gen_family_of_family =
     (fun f -> Futil.map_family_ps (fun p -> p) (fun s -> Istr s) f);
   is_deleted_family = (fun f -> f.Def.fam_index = Adef.ifam_of_int (-1));
   get_father = (fun c -> Adef.father c);
   get_mother = (fun c -> Adef.mother c);
   get_parent_array = (fun c -> Adef.parent_array c);
   gen_couple_of_couple = (fun c -> c);
   get_children = (fun d -> d.Def.children);
   gen_descend_of_descend = fun d -> d}

(* Families - implementation database 2 *)

let family2_fun =
  let rec self =
    {get_comment = (fun (db2, i) -> make_istr2 db2 ("family", "comment") i);
     get_divorce = (fun (db2, i) -> get_field db2 i ("family", "divorce"));
     get_fsources = (fun (db2, i) -> make_istr2 db2 ("family", "fsources") i);
     get_fevents =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("family", "fevents") in
          List.map
            (Futil.map_fam_event (fun x -> x)
               (fun pos -> Istr2 (db2, ("family", "fevents"), pos)))
            list);
     get_marriage = (fun (db2, i) -> get_field db2 i ("family", "marriage"));
     get_marriage_place =
       (fun (db2, i) -> make_istr2 db2 ("family", "marriage_place") i);
     get_marriage_note =
       (fun (db2, i) -> make_istr2 db2 ("family", "marriage_note") i);
     get_marriage_src =
       (fun (db2, i) -> make_istr2 db2 ("family", "marriage_src") i);
     get_origin_file =
       (fun (db2, i) -> make_istr2 db2 ("family", "origin_file") i);
     get_relation = (fun (db2, i) -> get_field db2 i ("family", "relation"));
     get_witnesses =
       (fun (db2, i) -> get_field db2 i ("family", "witnesses"));
     gen_family_of_family =
       (fun ((_db2, i) as f) ->
          {marriage = self.get_marriage f;
           marriage_place = self.get_marriage_place f;
           marriage_note = self.get_marriage_note f;
           marriage_src = self.get_marriage_src f;
           witnesses = self.get_witnesses f; relation = self.get_relation f;
           divorce = self.get_divorce f; fevents = self.get_fevents f;
           comment = self.get_comment f; origin_file = self.get_origin_file f;
           fsources = self.get_fsources f; fam_index = Adef.ifam_of_int i});
     is_deleted_family =
       (fun (db2, i) ->
          let fath =
            match db2.father_array with
              Some tab -> tab.(i)
            | None -> get_field db2 i ("family", "father")
          in
          Adef.int_of_iper fath < 0);
     get_father =
       (fun (db2, i) ->
          match db2.father_array with
            Some tab -> tab.(i)
          | None -> get_field db2 i ("family", "father"));
     get_mother =
       (fun (db2, i) ->
          match db2.mother_array with
            Some tab -> tab.(i)
          | None -> get_field db2 i ("family", "mother"));
     get_parent_array =
       (fun (db2, i) ->
          let p1 = get_field db2 i ("family", "father") in
          let p2 = get_field db2 i ("family", "mother") in [| p1; p2 |]);
     gen_couple_of_couple =
       (fun c -> Adef.couple (self.get_father c) (self.get_mother c));
     get_children =
       (fun (db2, i) ->
          match db2.children_array with
            Some tab -> tab.(i)
          | None -> get_field db2 i ("family", "children"));
     gen_descend_of_descend = fun d -> {children = self.get_children d}}
  in
  self

let family2gen_fun =
  {get_comment = (fun (db2, f) -> Istr2New (db2, f.Def.comment));
   get_divorce = (fun (_db2, f) -> f.Def.divorce);
   get_fsources = (fun (db2, f) -> Istr2New (db2, f.Def.fsources));
   get_fevents =
     (fun (db2, f) ->
        List.map
          (fun t -> Futil.map_fam_event (fun x -> x) (fun s -> Istr2New (db2, s)) t)
          f.Def.fevents);
   get_marriage = (fun (_db2, f) -> f.Def.marriage);
   get_marriage_place =
     (fun (db2, f) -> Istr2New (db2, f.Def.marriage_place));
   get_marriage_note = (fun (db2, f) -> Istr2New (db2, f.Def.marriage_note));
   get_marriage_src = (fun (db2, f) -> Istr2New (db2, f.Def.marriage_src));
   get_origin_file = (fun (db2, f) -> Istr2New (db2, f.Def.origin_file));
   get_relation = (fun (_db2, f) -> f.Def.relation);
   get_witnesses = (fun (_db2, f) -> f.Def.witnesses);
   gen_family_of_family =
     (fun (db2, f) ->
        Futil.map_family_ps (fun p -> p) (fun s -> Istr2New (db2, s)) f);
   is_deleted_family =
     (fun (_db2, f) -> f.Def.fam_index = Adef.ifam_of_int (-1));
   get_father = (fun (_db2, c) -> Adef.father c);
   get_mother = (fun (_db2, c) -> Adef.mother c);
   get_parent_array = (fun (_db2, c) -> Adef.parent_array c);
   gen_couple_of_couple = (fun (_db2, c) -> c);
   get_children = (fun (_db2, d) -> d.Def.children);
   gen_descend_of_descend = fun (_db2, d) -> d}

(* Families - user functions *)

let get_set_fam1 = (fun p -> p.fam1), (fun p v -> p.fam1 <- v)
let get_set_cpl1 = (fun p -> p.cpl1), (fun p v -> p.cpl1 <- v)
let get_set_des1 = (fun p -> p.des1), (fun p v -> p.des1 <- v)

let get_set_fam2 = (fun p -> p.fam2), (fun p v -> p.fam2 <- v)
let get_set_cpl2 = (fun p -> p.cpl2), (fun p v -> p.cpl2 <- v)
let get_set_des2 = (fun p -> p.des2), (fun p v -> p.des2 <- v)

let wrap_fam f g h =
  function
    Family (base, i, d) ->
      let fam = milazy_force base.data.families.get i get_set_fam1 d in
      f family1_fun fam
  | Family2 (db2, i, d) ->
      let fam =
        milazy_force (ht_find db2.patches.h_family) (Adef.ifam_of_int i)
          get_set_fam2 d
      in
      match fam with
        Some fam -> h family2gen_fun (db2, fam)
      | None -> g family2_fun (db2, i)

let wrap_cpl f g h =
  function
    Family (base, i, d) ->
      let cpl = milazy_force base.data.couples.get i get_set_cpl1 d in
      f family1_fun cpl
  | Family2 (db2, i, d) ->
      let cpl =
        milazy_force (ht_find db2.patches.h_couple) (Adef.ifam_of_int i)
          get_set_cpl2 d
      in
      match cpl with
        Some cpl -> h family2gen_fun (db2, cpl)
      | None -> g family2_fun (db2, i)

let wrap_des f g h =
  function
    Family (base, i, d) ->
      let des = milazy_force base.data.descends.get i get_set_des1 d in
      f family1_fun des
  | Family2 (db2, i, d) ->
      let des =
        milazy_force (ht_find db2.patches.h_descend) (Adef.ifam_of_int i)
          get_set_des2 d
      in
      match des with
        Some des -> h family2gen_fun (db2, des)
      | None -> g family2_fun (db2, i)

let get_comment fam = let f pf = pf.get_comment in wrap_fam f f f fam
let get_divorce fam = let f pf = pf.get_divorce in wrap_fam f f f fam
let get_fsources fam = let f pf = pf.get_fsources in wrap_fam f f f fam
let get_fevents fam = let f pf = pf.get_fevents in wrap_fam f f f fam
let get_marriage fam = let f pf = pf.get_marriage in wrap_fam f f f fam
let get_marriage_place fam =
  let f pf = pf.get_marriage_place in wrap_fam f f f fam
let get_marriage_note fam =
  let f pf = pf.get_marriage_note in wrap_fam f f f fam
let get_marriage_src fam =
  let f pf = pf.get_marriage_src in wrap_fam f f f fam
let get_origin_file fam = let f pf = pf.get_origin_file in wrap_fam f f f fam
let get_relation fam = let f pf = pf.get_relation in wrap_fam f f f fam
let get_witnesses fam = let f pf = pf.get_witnesses in wrap_fam f f f fam
let gen_family_of_family fam =
  let f pf = pf.gen_family_of_family in wrap_fam f f f fam
let is_deleted_family fam =
  let f pf = pf.is_deleted_family in wrap_fam f f f fam

let get_father cpl =
  let f pf = pf.get_father in
  match cpl with
    Family2 (db2, i, _) ->
      begin match db2.father_array with
        Some tab -> tab.(i)
      | None -> wrap_cpl f f f cpl
      end
  | _ -> wrap_cpl f f f cpl
let get_mother cpl =
  let f pf = pf.get_mother in
  match cpl with
    Family2 (db2, i, _) ->
      begin match db2.mother_array with
        Some tab -> tab.(i)
      | None -> wrap_cpl f f f cpl
      end
  | _ -> wrap_cpl f f f cpl
let get_parent_array cpl =
  let f pf = pf.get_parent_array in wrap_cpl f f f cpl
let gen_couple_of_couple cpl =
  let f pf = pf.gen_couple_of_couple in wrap_cpl f f f cpl

let get_children des = let f pf = pf.get_children in wrap_des f f f des
let gen_descend_of_descend des =
  let f pf = pf.gen_descend_of_descend in wrap_des f f f des

(* Databases - common definitions *)

type base =
  { bname : string
  ; close_base : unit -> unit;
    empty_person : iper -> person;
    person_of_gen_person :
      (iper, istr) gen_person * ifam gen_ascend * ifam gen_union -> person;
    family_of_gen_family :
      (iper, istr) gen_family * iper gen_couple * iper gen_descend -> family;
    poi : iper -> person;
    foi : ifam -> family;
    sou : istr -> string;
    nb_of_persons : unit -> int;
    nb_of_families : unit -> int;
    patch_person : iper -> (iper, istr) Def.gen_person -> unit;
    patch_ascend : iper -> ifam Def.gen_ascend -> unit;
    patch_union : iper -> ifam Def.gen_union -> unit;
    patch_family : ifam -> (iper, istr) Def.gen_family -> unit;
    patch_descend : ifam -> iper Def.gen_descend -> unit;
    patch_couple : ifam -> iper Def.gen_couple -> unit;
    patch_name : string -> iper -> unit;
    patch_key : iper -> string -> string -> int -> unit;
    delete_key : string -> string -> int -> unit;
    insert_string : string -> istr;
    commit_patches : unit -> unit;
    commit_notes : string -> string -> unit;
    is_patched_person : iper -> bool;
    patched_ascends : unit -> iper list;
    delete_family : ifam -> unit;
    person_of_key : string -> string -> int -> iper option;
    persons_of_name : string -> iper list;
    persons_of_first_name : unit -> string_person_index;
    persons_of_surname : unit -> string_person_index;
    base_visible_get : (person -> bool) -> int -> bool;
    base_visible_write : unit -> unit;
    base_particles : unit -> string list;
    base_strings_of_first_name : string -> istr list;
    base_strings_of_surname : string -> istr list;
    load_ascends_array : unit -> unit;
    load_unions_array : unit -> unit;
    load_couples_array : unit -> unit;
    load_descends_array : unit -> unit;
    load_strings_array : unit -> unit;
    load_persons_array : unit -> unit;
    load_families_array : unit -> unit;
    clear_ascends_array : unit -> unit;
    clear_unions_array : unit -> unit;
    clear_couples_array : unit -> unit;
    clear_descends_array : unit -> unit;
    clear_strings_array : unit -> unit;
    clear_persons_array : unit -> unit;
    clear_families_array : unit -> unit;
    persons_array :
      unit ->
        (int -> (iper, istr) gen_person) *
          (int -> (iper, istr) gen_person -> unit);
    ascends_array :
      unit ->
        (int -> ifam option) * (int -> Adef.fix) * (int -> Adef.fix -> unit) *
          Adef.fix array option;
    base_notes_read : string -> string;
    base_notes_read_first_line : string -> string;
    base_notes_are_empty : string -> bool;
    base_notes_origin_file : unit -> string;
    base_notes_dir : unit -> string;
    base_wiznotes_dir : unit -> string;
    nobtit : string list Lazy.t -> string list Lazy.t -> person -> title list;
    p_first_name : person -> string;
    p_surname : person -> string;
    date_of_last_change : unit -> float;
    apply_base1 : (Dbdisk.dsk_base -> unit) -> unit;
    apply_base2 : (Db2disk.db2 -> unit) -> unit }

module C_base :
  sig
    val delete_family : base -> ifam -> unit
    val nobtit :
      base -> string list Lazy.t -> string list Lazy.t -> person -> title list
    val p_first_name : base -> person -> string
    val p_surname : base -> person -> string
  end =
  struct
    let delete_family (self : base) ifam =
      let cpl = Adef.couple (Adef.iper_of_int (-1)) (Adef.iper_of_int (-1)) in
      let fam =
        let empty = self.insert_string "" in
        {marriage = Adef.cdate_None; marriage_place = empty;
         marriage_note = empty; marriage_src = empty; relation = Married;
         divorce = NotDivorced; fevents = []; witnesses = [| |];
         comment = empty; origin_file = empty; fsources = empty;
         fam_index = Adef.ifam_of_int (-1)}
      in
      let des = {children = [| |]} in
      self.patch_family ifam fam;
      self.patch_couple ifam cpl;
      self.patch_descend ifam des
    let nobtit self allowed_titles denied_titles p =
      let list = get_titles p in
      match Lazy.force allowed_titles with
        [] -> list
      | allowed_titles ->
          let list =
            List.fold_right
              (fun t l ->
                 let id = Name.lower (self.sou t.t_ident) in
                 let pl = Name.lower (self.sou t.t_place) in
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
                   let id = Name.lower (self.sou t.t_ident) in
                   let pl = Name.lower (self.sou t.t_place) in
                   if List.mem (id ^ "/" ^ pl) denied_titles ||
                      List.mem ("*/" ^ pl) denied_titles
                   then
                     false
                   else true)
                list
    let p_first_name self p = Mutil.nominative (self.sou (get_first_name p))
    let p_surname self p = Mutil.nominative (self.sou (get_surname p))
  end

(* Database - implementation 1 *)

let base1 bname base =
  let base_strings_of_first_name_or_surname s =
    List.map (fun s -> Istr s) (base.func.strings_of_fsname s)
  in
  let rec self =
    { bname
    ; close_base = base.func.cleanup;
     empty_person =
       (fun ip ->
          Person
            (base, Adef.int_of_iper ip,
             {per1 = Some (no_person (Adef.istr_of_int 0) ip);
              asc1 = Some no_ascend; uni1 = Some no_union}));
     person_of_gen_person =
       (fun (p, a, u) ->
          Person
            (base, 0,
             {per1 = Some (Futil.map_person_ps (fun p -> p) un_istr p);
              asc1 = Some a; uni1 = Some u}));
     family_of_gen_family =
       (fun (f, c, d) ->
          Family
            (base, 0,
             {fam1 = Some (Futil.map_family_ps (fun p -> p) un_istr f);
              cpl1 = Some c; des1 = Some d}));
     poi =
       (fun i ->
          Person
            (base, Adef.int_of_iper i,
             {per1 = None; asc1 = None; uni1 = None}));
     foi =
       (fun i ->
          Family
            (base, Adef.int_of_ifam i,
             {fam1 = None; cpl1 = None; des1 = None}));
     sou =
       (fun i ->
          match i with
            Istr i -> base.data.strings.get (Adef.int_of_istr i)
          | _ -> assert false);
     nb_of_persons = (fun () -> base.data.persons.len);
     nb_of_families = (fun () -> base.data.families.len);
     patch_person =
       (fun ip p ->
          let p = Futil.map_person_ps (fun p -> p) un_istr p in
          base.func.Dbdisk.patch_person ip p);
     patch_ascend = (fun ip a -> base.func.Dbdisk.patch_ascend ip a);
     patch_union = (fun ip u -> base.func.Dbdisk.patch_union ip u);
     patch_family =
       (fun ifam f ->
          let f = Futil.map_family_ps (fun p -> p) un_istr f in
          base.func.Dbdisk.patch_family ifam f);
     patch_descend = (fun ifam d -> base.func.Dbdisk.patch_descend ifam d);
     patch_couple = (fun ifam c -> base.func.Dbdisk.patch_couple ifam c);
     patch_name = (fun s ip -> base.func.Dbdisk.patch_name s ip);
     patch_key = (fun _ip _fn _sn _occ -> ());
     delete_key = (fun _fn _sn _occ -> ());
     insert_string = (fun s -> Istr (base.func.Dbdisk.insert_string s));
     commit_patches = base.func.Dbdisk.commit_patches;
     commit_notes = base.func.Dbdisk.commit_notes;
     is_patched_person = (fun ip -> base.func.Dbdisk.is_patched_person ip);
     patched_ascends = base.func.Dbdisk.patched_ascends;
     delete_family = (fun ifam -> C_base.delete_family self ifam);
     person_of_key = base.func.Dbdisk.person_of_key;
     persons_of_name = base.func.Dbdisk.persons_of_name;
     persons_of_first_name =
       (fun () -> Spi base.func.Dbdisk.persons_of_first_name);
     persons_of_surname = (fun () -> Spi base.func.Dbdisk.persons_of_surname);
     base_visible_get =
       (fun f ->
          base.data.visible.v_get
            (fun p ->
               f
                 (Person
                    (base, 0, {per1 = Some p; asc1 = None; uni1 = None}))));
     base_visible_write = base.data.visible.v_write;
     base_particles = (fun () -> base.data.particles);
     base_strings_of_first_name = base_strings_of_first_name_or_surname;
     base_strings_of_surname = base_strings_of_first_name_or_surname;
     load_ascends_array = base.data.ascends.load_array;
     load_unions_array = base.data.unions.load_array;
     load_couples_array = base.data.couples.load_array;
     load_descends_array = base.data.descends.load_array;
     load_strings_array = base.data.strings.load_array;
     load_persons_array = base.data.persons.load_array;
     load_families_array = base.data.families.load_array;
     clear_ascends_array = base.data.ascends.clear_array;
     clear_unions_array = base.data.unions.clear_array;
     clear_couples_array = base.data.couples.clear_array;
     clear_descends_array = base.data.descends.clear_array;
     clear_strings_array = base.data.strings.clear_array;
     clear_persons_array = base.data.persons.clear_array;
     clear_families_array = base.data.families.clear_array;
     persons_array =
       (fun () ->
          let get i =
            let p = base.data.persons.get i in
            Futil.map_person_ps (fun p -> p) (fun i -> Istr i) p
          in
          let set i p =
            let p = Futil.map_person_ps (fun p -> p) un_istr p in
            base.data.persons.set i p
          in
          get, set);
     ascends_array =
       (fun () ->
          let fget i = (base.data.ascends.get i).parents in
          let cget i = (base.data.ascends.get i).consang in
          let cset i v =
            base.data.ascends.set i
              {(base.data.ascends.get i) with consang = v}
          in
          fget, cget, cset, None);
     base_notes_read = (fun fnotes -> base.data.bnotes.nread fnotes RnAll);
     base_notes_read_first_line =
       (fun fnotes -> base.data.bnotes.nread fnotes Rn1Ln);
     base_notes_are_empty =
       (fun fnotes -> base.data.bnotes.nread fnotes RnDeg = "");
     base_notes_origin_file = (fun () -> base.data.bnotes.norigin_file);
     base_notes_dir = (fun () -> "notes_d");
     base_wiznotes_dir = (fun () -> "wiznotes");
     nobtit = (fun conf p -> C_base.nobtit self conf p);
     p_first_name = (fun p -> C_base.p_first_name self p);
     p_surname = (fun p -> C_base.p_surname self p);
     date_of_last_change =
       (fun () ->
          let s =
            let bdir = base.data.bdir in
            try Unix.stat (Filename.concat bdir "patches") with
              Unix.Unix_error (_, _, _) ->
                Unix.stat (Filename.concat bdir "base")
          in
          s.Unix.st_mtime);
     apply_base1 = (fun f -> f base);
     apply_base2 = fun _f -> invalid_arg "apply_base2"}
  in
  self

(* Database - implementation 2 *)

let base2 db2 =
  let base_strings_of_first_name_or_surname field proj s =
    let posl = strings2_of_fsname db2 field s in
    let istrl =
      List.map (fun pos -> Istr2 (db2, ("person", field), pos)) posl
    in
    let s = Name.crush_lower s in
    let sl =
      Hashtbl.fold
        (fun _ p sl ->
           if Name.crush_lower (proj p) = s then proj p :: sl else sl)
        db2.patches.h_person []
    in
    let sl = List.sort_uniq compare sl in
    List.fold_left (fun istrl s -> Istr2New (db2, s) :: istrl) istrl sl
  in
  let rec self =
    {bname="";close_base =
      (fun () ->
         Hashtbl.iter (fun (_f1, _f2, _f) ic -> close_in ic) db2.cache_chan);
     empty_person =
       (fun ip ->
          Person2
            (db2, Adef.int_of_iper ip,
             {per2 = Some (Some (no_person "" ip));
              asc2 = Some (Some no_ascend); uni2 = Some (Some no_union)}));
     person_of_gen_person =
       (fun (p, a, u) ->
          Person2
            (db2, Adef.int_of_iper p.key_index,
             {per2 = Some (Some (Futil.map_person_ps (fun p -> p) un_istr2 p));
              asc2 = Some (Some a); uni2 = Some (Some u)}));
     family_of_gen_family =
       (fun (f, c, d) ->
          Family2
            (db2, Adef.int_of_ifam f.fam_index,
             {fam2 = Some (Some (Futil.map_family_ps (fun p -> p) un_istr2 f));
              cpl2 = Some (Some c); des2 = Some (Some d)}));
     poi =
       (fun i ->
          Person2
            (db2, Adef.int_of_iper i,
             {per2 = None; asc2 = None; uni2 = None}));
     foi =
       (fun i ->
          Family2
            (db2, Adef.int_of_ifam i,
             {fam2 = None; cpl2 = None; des2 = None}));
     sou =
       (fun i ->
          match i with
            Istr2 (db2, f, pos) -> string_of_istr2 db2 f pos
          | Istr2New (_db2, s) -> s
          | _ -> assert false);
     nb_of_persons = (fun () -> db2.patches.nb_per);
     nb_of_families = (fun () -> db2.patches.nb_fam);
     patch_person =
       (fun ip p ->
          let p = Futil.map_person_ps (fun p -> p) un_istr2 p in
          Hashtbl.replace db2.patches.h_person ip p;
          db2.patches.nb_per <-
            max (Adef.int_of_iper ip + 1) db2.patches.nb_per);
     patch_ascend =
       (fun ip a ->
          Hashtbl.replace db2.patches.h_ascend ip a;
          db2.patches.nb_per <-
            max (Adef.int_of_iper ip + 1) db2.patches.nb_per);
     patch_union =
       (fun ip u ->
          Hashtbl.replace db2.patches.h_union ip u;
          db2.patches.nb_per <-
            max (Adef.int_of_iper ip + 1) db2.patches.nb_per);
     patch_family =
       (fun ifam f ->
          let f = Futil.map_family_ps (fun p -> p) un_istr2 f in
          Hashtbl.replace db2.patches.h_family ifam f;
          db2.patches.nb_fam <-
            max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam);
     patch_descend =
       (fun ifam d ->
          Hashtbl.replace db2.patches.h_descend ifam d;
          db2.patches.nb_fam <-
            max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam);
     patch_couple =
       (fun ifam c ->
          Hashtbl.replace db2.patches.h_couple ifam c;
          db2.patches.nb_fam <-
            max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam);
     patch_name =
       (fun s ip ->
          let s = Name.crush_lower s in
          let ht = db2.patches.h_name in
          try
            let ipl = Hashtbl.find ht s in
            if List.mem ip ipl then () else Hashtbl.replace ht s (ip :: ipl)
          with Not_found -> Hashtbl.add ht s [ip]);
     patch_key =
       (fun ip fn sn occ ->
          let fn = Name.lower (Mutil.nominative fn) in
          let sn = Name.lower (Mutil.nominative sn) in
          Hashtbl.replace db2.patches.h_key (fn, sn, occ) (Some ip));
     delete_key =
       (fun fn sn occ ->
          let fn = Name.lower (Mutil.nominative fn) in
          let sn = Name.lower (Mutil.nominative sn) in
          match disk_person2_of_key db2 fn sn occ with
            Some _ -> Hashtbl.replace db2.patches.h_key (fn, sn, occ) None
          | None -> Hashtbl.remove db2.patches.h_key (fn, sn, occ));
     insert_string = (fun s -> Istr2New (db2, s));
     commit_patches = (fun () -> commit_patches2 db2);
     commit_notes = (fun fnotes s -> commit_notes2 db2 fnotes s);
     is_patched_person = (fun ip -> Hashtbl.mem db2.patches.h_person ip);
     patched_ascends =
       (fun () ->
          let r = ref [] in
          Hashtbl.iter (fun ip _ -> r := ip :: !r) db2.patches.h_ascend; !r);
     delete_family = (fun ifam -> C_base.delete_family self ifam);
     person_of_key = (fun fn sn oc -> person2_of_key db2 fn sn oc);
     persons_of_name = (fun s -> persons2_of_name db2 s);
     persons_of_first_name =
       (fun () -> Spi2 (db2, persons_of_first_name_or_surname2 db2 true));
     persons_of_surname =
       (fun () -> Spi2 (db2, persons_of_first_name_or_surname2 db2 false));
     base_visible_get = (fun _f -> failwith "not impl visible_get");
     base_visible_write = (fun () -> failwith "not impl visible_write");
     base_particles =
       (fun () ->
          Mutil.input_particles (Filename.concat db2.bdir2 "particles.txt"));
     base_strings_of_first_name =
       (fun s ->
          base_strings_of_first_name_or_surname "first_name"
            (fun p -> p.first_name) s);
     base_strings_of_surname =
       (fun s ->
          base_strings_of_first_name_or_surname "surname" (fun p -> p.surname)
            s);
     load_ascends_array =
       (fun () ->
          Printf.eprintf "*** loading ascends array\n";
          flush stderr;
          let nb = db2.patches.nb_per in
          let nb_ini = db2.patches.nb_per_ini in
          begin match db2.parents_array with
            Some _ -> ()
          | None -> db2.parents_array <- Some (parents_array2 db2 nb_ini nb)
          end;
          match db2.consang_array with
            Some _ -> ()
          | None -> db2.consang_array <- Some (consang_array2 db2 nb));
     load_unions_array =
       (fun () ->
          match db2.family_array with
            Some _ -> ()
          | None ->
              Printf.eprintf "*** loading unions array\n";
              flush stderr;
              db2.family_array <- Some (family_array2 db2));
     load_couples_array = (fun () -> load_couples_array2 db2);
     load_descends_array =
       (fun () ->
          match db2.children_array with
            Some _ -> ()
          | None ->
              Printf.eprintf "*** loading descends array\n";
              flush stderr;
              db2.children_array <- Some (children_array2 db2));
     load_strings_array = (fun () -> ());
     load_persons_array = (fun () -> ());
     load_families_array = (fun () -> ());
     clear_ascends_array =
       (fun () -> db2.parents_array <- None);
     clear_unions_array =
       (fun () -> db2.family_array <- None);
     clear_couples_array = (fun () ->
         db2.father_array <- None ;
         db2.mother_array <- None );
     clear_descends_array =
       (fun () -> db2.children_array <- None);
     clear_strings_array = (fun () -> ());
     clear_persons_array = (fun () -> ());
     clear_families_array = (fun () -> ());
     persons_array = (fun () -> failwith "not impl persons_array");
     ascends_array =
       (fun () ->
          let nb = db2.patches.nb_per in
          let nb_ini = db2.patches.nb_per_ini in
          let ptab =
            match db2.parents_array with
              Some tab -> tab
            | None -> parents_array2 db2 nb_ini nb
          in
          let cg_tab =
            match db2.consang_array with
              Some tab -> tab
            | None -> consang_array2 db2 nb
          in
          let fget i = ptab.(i) in
          let cget i = cg_tab.(i) in
          let cset i v = cg_tab.(i) <- v in fget, cget, cset, Some cg_tab);
     base_notes_read = (fun fnotes -> read_notes db2 fnotes RnAll);
     base_notes_read_first_line = (fun fnotes -> read_notes db2 fnotes Rn1Ln);
     base_notes_are_empty = (fun fnotes -> read_notes db2 fnotes RnDeg = "");
     base_notes_origin_file =
       (fun () ->
          let fname = Filename.concat db2.bdir2 "notes_of.txt" in
          try
            let ic = Secure.open_in fname in
            let r = input_line ic in
            close_in ic;
            r
          with Sys_error _ -> "");
     base_notes_dir = (fun () -> Filename.concat "base_d" "notes_d");
     base_wiznotes_dir = (fun () -> Filename.concat "base_d" "wiznotes_d");
     nobtit = (fun conf p -> C_base.nobtit self conf p);
     p_first_name = (fun p -> C_base.p_first_name self p);
     p_surname = (fun p -> C_base.p_surname self p);
     date_of_last_change =
       (fun () ->
          let s =
            let bdir = db2.bdir2 in
            try Unix.stat (Filename.concat bdir "patches") with
              Unix.Unix_error (_, _, _) -> Unix.stat bdir
          in
          s.Unix.st_mtime);
     apply_base1 = (fun _f -> invalid_arg "apply_base1");
     apply_base2 = fun f -> f db2}
  in
  self

(* Database - user functions *)

let open_base bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  if Sys.file_exists (Filename.concat bname "base_d") then
    base2 (base_of_base2 bname)
  else base1 bname (Database.opendb bname)

let close_base (b : base) = b.close_base ()
let empty_person (b : base) = b.empty_person
let person_of_gen_person (b : base) = b.person_of_gen_person
let family_of_gen_family (b : base) = b.family_of_gen_family
let poi (b : base) = b.poi
let foi (b : base) = b.foi
let sou (b : base) = b.sou
let nb_of_persons (b : base) = b.nb_of_persons ()
let nb_of_families (b : base) = b.nb_of_families ()
let patch_person (b : base) ip p =
  if p.access = Public then begin
    let op = poi b ip in
    if get_access op <> Public then
      let tm = Unix.localtime (Unix.time ()) in
      Log.with_file ~file:(Sys.getenv "LOG_PUBLIC") (fun oc ->
          Printf.fprintf oc
            "%02d/%02d/%4d %02d:%02d:%02d %s %s %s %d\n"
            tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year)
            tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
            b.bname (sou b p.surname) (sou b p.first_name) (p.occ))
  end ;
  b.patch_person ip p
let patch_ascend (b : base) = b.patch_ascend
let patch_union (b : base) = b.patch_union
let patch_family (b : base) = b.patch_family
let patch_descend (b : base) = b.patch_descend
let patch_couple (b : base) = b.patch_couple
let patch_name (b : base) = b.patch_name
let patch_key (b : base) = b.patch_key
let delete_key (b : base) = b.delete_key
let insert_string (b : base) = b.insert_string
let commit_patches (b : base) = b.commit_patches ()
let commit_notes (b : base) = b.commit_notes
let is_patched_person (b : base) = b.is_patched_person
let patched_ascends (b : base) = b.patched_ascends ()
let delete_family (b : base) = b.delete_family
let person_of_key (b : base) = b.person_of_key
let persons_of_name (b : base) = b.persons_of_name
let persons_of_first_name (b : base) = b.persons_of_first_name ()
let persons_of_surname (b : base) = b.persons_of_surname ()
let base_visible_get (b : base) = b.base_visible_get
let base_visible_write (b : base) = b.base_visible_write ()
let base_particles (b : base) = b.base_particles ()
let base_strings_of_first_name (b : base) = b.base_strings_of_first_name
let base_strings_of_surname (b : base) = b.base_strings_of_surname
let load_ascends_array (b : base) = b.load_ascends_array ()
let load_unions_array (b : base) = b.load_unions_array ()
let load_couples_array (b : base) = b.load_couples_array ()
let load_descends_array (b : base) = b.load_descends_array ()
let load_strings_array (b : base) = b.load_strings_array ()
let load_persons_array (b : base) = b.load_persons_array ()
let load_families_array (b : base) = b.load_families_array ()
let clear_ascends_array (b : base) = b.clear_ascends_array ()
let clear_unions_array (b : base) = b.clear_unions_array ()
let clear_couples_array (b : base) = b.clear_couples_array ()
let clear_descends_array (b : base) = b.clear_descends_array ()
let clear_strings_array (b : base) = b.clear_strings_array ()
let clear_persons_array (b : base) = b.clear_persons_array ()
let clear_families_array (b : base) = b.clear_families_array ()
let persons_array (b : base) = b.persons_array ()
let ascends_array (b : base) = b.ascends_array ()
let base_notes_read (b : base) = b.base_notes_read
let base_notes_read_first_line (b : base) = b.base_notes_read_first_line
let base_notes_are_empty (b : base) = b.base_notes_are_empty
let base_notes_origin_file (b : base) = b.base_notes_origin_file ()
let base_notes_dir (b : base) = b.base_notes_dir ()
let base_wiznotes_dir (b : base) = b.base_wiznotes_dir ()
let nobtit (b : base) = b.nobtit
let p_first_name (b : base) = b.p_first_name
let p_surname (b : base) = b.p_surname
let date_of_last_change (b : base) = b.date_of_last_change ()
let base_of_base1 = base1 ""
let apply_base1 (b : base) = b.apply_base1
let apply_base2 (b : base) = b.apply_base2

let husbands base gp =
  let p = poi base gp.key_index in
  List.map
    (fun ifam ->
       let fam = foi base ifam in
       let husband = poi base (get_father fam) in
       let husband_surname = p_surname base husband in
       let husband_surnames_aliases =
         List.map (sou base) (get_surnames_aliases husband)
       in
       husband_surname, husband_surnames_aliases)
    (Array.to_list (get_family p))

let father_titles_places base p nobtit =
  match get_parents (poi base p.key_index) with
    Some ifam ->
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
    (if p.sex = Female then husbands base p else [])
    (father_titles_places base p nobtit_fun)

let gen_person_misc_names base p nobtit =
  gen_gen_person_misc_names base p (nobtit p)
    (fun p -> nobtit (gen_person_of_person p))

let person_misc_names base p nobtit =
  gen_gen_person_misc_names base (gen_person_of_person p) (nobtit p) nobtit
