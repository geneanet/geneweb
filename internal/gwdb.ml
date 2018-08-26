(* $Id: gwdb.ml,v 5.244 2012-01-18 20:49:57 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk
open Def

(* Strings - common definitions *)

let eq_istr i1 i2 =  i1 = i2

[@@@ocaml.warning "-40"]
[@@@ocaml.warning "-42"]

module Make (D : Gwdb_driver.S) = struct

  include D

  let milazy_force f a get set p =
    match get p with
      Some v -> v
    | None -> let v = f a in set p (Some v); v

  let spi_first = D.spi_first
  let spi_next = D.spi_next
  let spi_find = D.spi_find
  let is_quest_string = D.is_quest_string
  let is_empty_string = D.is_empty_string

  module P = D

  let wrap_per f = fun (base, i, p) ->
    let per = milazy_force base.data.persons.get i P.get_per P.set_per p in
    f per

  let wrap_asc f = fun (base, i, p) ->
    let asc = milazy_force base.data.ascends.get i P.get_asc P.set_asc p in
    f asc

  let wrap_uni f = fun (base, i, p) ->
    let uni = milazy_force base.data.unions.get i P.get_uni P.set_uni p in
    f uni

  let get_access = wrap_per P.get_access
  let get_aliases = wrap_per P.get_aliases
  let get_baptism = wrap_per P.get_baptism
  let get_baptism_place = wrap_per P.get_baptism_place
  let get_baptism_note = wrap_per P.get_baptism_note
  let get_baptism_src = wrap_per P.get_baptism_src
  let get_birth = wrap_per P.get_birth
  let get_birth_place = wrap_per P.get_birth_place
  let get_birth_note = wrap_per P.get_birth_note
  let get_birth_src = wrap_per P.get_birth_src
  let get_burial = wrap_per P.get_burial
  let get_burial_place = wrap_per P.get_burial_place
  let get_burial_note = wrap_per P.get_burial_note
  let get_burial_src = wrap_per P.get_burial_src
  let get_death = wrap_per P.get_death
  let get_death_place = wrap_per P.get_death_place
  let get_death_note = wrap_per P.get_death_note
  let get_death_src = wrap_per P.get_death_src
  let get_pevents = wrap_per P.get_pevents
  let get_first_name = wrap_per P.get_first_name
  let get_first_names_aliases =
    wrap_per P.get_first_names_aliases
  let get_image = wrap_per P.get_image
  let get_key_index = wrap_per P.get_key_index
  let get_notes = wrap_per P.get_notes
  let get_occ = wrap_per P.get_occ
  let get_occupation = wrap_per P.get_occupation
  let get_psources = wrap_per P.get_psources
  let get_public_name = wrap_per P.get_public_name
  let get_qualifiers = wrap_per P.get_qualifiers
  let get_related = wrap_per P.get_related
  let get_rparents = wrap_per P.get_rparents
  let get_sex = wrap_per P.get_sex
  let get_surname = wrap_per P.get_surname
  let get_surnames_aliases =
    wrap_per P.get_surnames_aliases
  let get_titles = wrap_per P.get_titles
  let gen_person_of_person =
    wrap_per P.gen_person_of_person
  let dsk_person_of_person =
    wrap_per P.dsk_person_of_person
  let get_consang = wrap_asc P.get_consang
  let get_parents = wrap_asc P.get_parents
  let get_family = wrap_uni P.get_family

(* Families - common definitions *)

  module F = D

  let wrap_fam f = fun (base, i, d) ->
    let fam = milazy_force base.data.families.get i F.get_fam1 F.set_fam1 d in
    f fam

  let wrap_cpl f = fun (base, i, d) ->
    let cpl = milazy_force base.data.couples.get i F.get_cpl1 F.set_cpl1 d in
    f cpl

  let wrap_des f = fun (base, i, d) ->
    let des = milazy_force base.data.descends.get i F.get_des1 F.set_des1 d in
    f des

  let get_comment = wrap_fam F.get_comment
  let get_divorce = wrap_fam F.get_divorce
  let get_fsources = wrap_fam F.get_fsources
  let get_fevents = wrap_fam F.get_fevents
  let get_marriage = wrap_fam F.get_marriage
  let get_marriage_place = wrap_fam F.get_marriage_place
  let get_marriage_note = wrap_fam F.get_marriage_note
  let get_marriage_src = wrap_fam F.get_marriage_src
  let get_origin_file = wrap_fam F.get_origin_file
  let get_relation = wrap_fam F.get_relation
  let get_witnesses = wrap_fam F.get_witnesses
  let gen_family_of_family = wrap_fam F.gen_family_of_family
  let is_deleted_family = wrap_fam F.is_deleted_family

  let get_father = wrap_cpl F.get_father
  let get_mother = wrap_cpl F.get_mother
  let get_parent_array = wrap_cpl F.get_parent_array

  let gen_couple_of_couple = wrap_cpl F.gen_couple_of_couple

  let get_children = wrap_des F.get_children
  let gen_descend_of_descend = wrap_des F.gen_descend_of_descend

  (* Base funcion *)

  module B = D

  let delete_family base ifam =
    let cpl = Adef.couple (Adef.iper_of_int (-1)) (Adef.iper_of_int (-1)) in
    let fam =
      let empty = B.insert_string base "" in
      {marriage = Adef.cdate_None; marriage_place = empty;
       marriage_note = empty; marriage_src = empty; relation = Married;
       divorce = NotDivorced; fevents = []; witnesses = [| |];
       comment = empty; origin_file = empty; fsources = empty;
       fam_index = Adef.ifam_of_int (-1)}
    in
    let des = {children = [| |]} in
    B.patch_family base ifam fam;
    B.patch_couple base ifam cpl;
    B.patch_descend base ifam des

    let nobtit base allowed_titles denied_titles p =
      let list = get_titles p in
      match Lazy.force allowed_titles with
      | [] -> list
      | allowed_titles ->
          let list =
            List.fold_right
              (fun t l ->
                 let id = Name.lower (B.sou base t.t_ident) in
                 let pl = Name.lower (B.sou base t.t_place) in
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
          | [] -> list
          | denied_titles ->
              List.filter
                (fun t ->
                   let id = Name.lower (B.sou base t.t_ident) in
                   let pl = Name.lower (B.sou base t.t_place) in
                   not (List.mem (id ^ "/" ^ pl) denied_titles)
                   && not (List.mem ("*/" ^ pl) denied_titles) )
                list

    let p_first_name base p = Mutil.nominative (B.sou base (get_first_name p))
    let p_surname base p = Mutil.nominative (B.sou base (get_surname p))

end

include Make (Gwdb_1 : Gwdb_driver.S)

(* Database - user functions *)

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
