#ifdef API

module M = Api_piqi
module Mext = Api_piqi_ext

open Def
open Gwdb
open Api_util

let set_list l v = l := v :: !l

(* Listes des erreurs *)
let already_defined = ref []
let own_ancestor = ref []
let bad_sex_of_married_person = ref []

(* Listes des warnings *)
let big_age_between_spouses = ref []
let birth_after_death = ref []
let incoherent_sex = ref []
let changed_order_of_children = ref []
let changed_order_of_marriages = ref []
let changed_order_of_family_events = ref []
let changed_order_of_person_events = ref []
let children_not_in_order = ref []
let close_children = ref []
let dead_old = ref []
let dead_too_early_to_be_father = ref []
let fevent_order = ref []
let fevent_witness_after_death = ref []
let fevent_witness_before_birth = ref []
let incoherent_ancestor_date = ref []
let marriage_date_after_death = ref []
let marriage_date_before_birth = ref []
(* Renommage : MotherDeadAfterChildBirth => mother_dead_before_child_birth *)
let mother_dead_before_child_birth = ref []
let parent_born_after_child = ref []
let parent_too_old = ref []
let parent_too_young = ref []
let possible_duplicate_fam = ref []
let pevent_order = ref []
let pevent_witness_after_death = ref []
let pevent_witness_before_birth = ref []
let title_dates_error = ref []
let undefined_sex = ref []
let witness_date_after_death = ref []
let witness_date_before_birth = ref []
let young_for_marriage = ref []
let old_for_marriage = ref []
let old_individual = ref []
let distant_children = ref []

(** [add_error_to_piqi_warning_list base error]
    Convert [error] and add it to corresponding error list
*)
let add_error_to_piqi_warning_list base = function
  | AlreadyDefined p ->
    set_list
      already_defined
      M.Warning_already_defined.{person = person_to_warning_person base p }
  | OwnAncestor p ->
    set_list
      own_ancestor
      M.Warning_own_ancestor.{person = person_to_warning_person base p }
  | BadSexOfMarriedPerson p ->
    set_list
      bad_sex_of_married_person
      M.Warning_bad_sex_of_married_person.{person = person_to_warning_person base p }

let add_warning_to_piqi_warning_list conf base =
  let p2wp = person_to_warning_person in
  let pn2s = Util.string_of_pevent_name conf base in
  function
  | BigAgeBetweenSpouses (fath, moth, dmy) ->
    set_list big_age_between_spouses
      M.Warning_big_age_between_spouses.{
        father = p2wp base fath
      ; mother = p2wp base moth
      ; date = string_of_prec_dmy dmy
      }
  | BirthAfterDeath p ->
    set_list birth_after_death
      M.Warning_birth_after_death.{ person = p2wp base p }
  | IncoherentSex (p, _, _) ->
    set_list incoherent_sex
      M.Warning_incoherent_sex.{ person = p2wp base p }
  | ChangedOrderOfChildren (ifam, _, _, _) ->
      let cpl = foi base ifam in
      set_list changed_order_of_children
        M.Warning_changed_order_of_children.{
          father = p2wp base @@ poi base @@ get_father cpl
        ; mother = p2wp base @@ poi base @@ get_mother cpl
        }
  | ChangedOrderOfMarriages (p, _, _) ->
    set_list changed_order_of_marriages
      M.Warning_changed_order_of_marriages.{ person = p2wp base p }
  | ChangedOrderOfFamilyEvents (ifam, _, _) ->
      let cpl = foi base ifam in
      set_list changed_order_of_family_events
        M.Warning_changed_order_of_family_events.{
          father = p2wp base @@ poi base @@ get_father cpl
        ; mother = p2wp base @@ poi base @@ get_mother cpl
        }
  | ChangedOrderOfPersonEvents (p, _, _) ->
    set_list changed_order_of_person_events
      M.Warning_changed_order_of_person_events.{ person = p2wp base p }
  | ChildrenNotInOrder (ifam, _, _, _) ->
      let cpl = foi base ifam in
      set_list children_not_in_order
        M.Warning_children_not_in_order.{
          father = p2wp base @@ poi base @@ get_father cpl
        ; mother = p2wp base @@ poi base @@ get_mother cpl
        }
  | CloseChildren (ifam, _, c1, c2) ->
      let cpl = foi base ifam in
      set_list close_children
        M.Warning_close_children.{
          father = p2wp base @@ poi base @@ get_father cpl
        ; mother = p2wp base @@ poi base @@ get_mother cpl
        ; child1 = p2wp base c1
        ; child2 = p2wp base c2
        }
  | DeadOld (p, dmy) ->
    set_list dead_old
      M.Warning_dead_old.{
        person = p2wp base p
        ; date = string_of_prec_dmy dmy ;
      }
  | DeadTooEarlyToBeFather (f, s) ->
    set_list dead_too_early_to_be_father
      M.Warning_dead_too_early_to_be_father.{
        father = p2wp base f
        ; son = p2wp base s;
      }
  | DistantChildren (ifam, c1, c2) ->
      let cpl = foi base ifam in
      set_list distant_children
        M.Warning_distant_children.{
          father = p2wp base @@ poi base @@ get_father cpl
        ; mother = p2wp base @@ poi base @@ get_mother cpl
        ; child1 = p2wp base c1
        ; child2 = p2wp base c2
        }
  | FEventOrder (p, e1, e2) ->
    set_list fevent_order
      M.Warning_fevent_order.{
        person = p2wp base p
        ; event1 = Util.string_of_fevent_name conf base e1.efam_name
        ; event2 = Util.string_of_fevent_name conf base e2.efam_name ;
      }
  | FWitnessEventAfterDeath (p, e) ->
    set_list fevent_witness_after_death
      M.Warning_fwitness_event_after_death.{
        person = p2wp base p
        ; event = Util.string_of_fevent_name conf base e.efam_name ;
      }
  | FWitnessEventBeforeBirth (p, e) ->
    set_list fevent_witness_before_birth
      M.Warning_fwitness_event_before_birth.{
        person = p2wp base p
      ; event = Util.string_of_fevent_name conf base e.efam_name
    }
  | IncoherentAncestorDate (a, p) ->
      let ancestor =
        p2wp base a
      in
      let person =
        p2wp base p
      in
      let w =
        M.Warning_incoherent_ancestor_date.{
          person = person
        ; ancestor = ancestor
        }
      in
      set_list incoherent_ancestor_date w
  | MarriageDateAfterDeath p ->
      let p =
        p2wp base p
      in
      let w = M.Warning_marriage_date_after_death.{person = p} in
      set_list marriage_date_after_death w
  | MarriageDateBeforeBirth p ->
      let p =
        p2wp base p
      in
      let w = M.Warning_marriage_date_before_birth.{person = p} in
      set_list marriage_date_before_birth w
  | MotherDeadAfterChildBirth (m, c) ->
    (* Étrangement c'est le seul message qui se lit de droite à gauche *)
    (* Naissance de l'enfant après la mort de sa mère => on le renomme *)
    set_list mother_dead_before_child_birth
      M.Warning_mother_dead_before_child_birth.{
        mother = p2wp base m
      ; child = p2wp base c
      }
  | ParentBornAfterChild (p, c) ->
    set_list parent_born_after_child
      M.Warning_parent_born_after_child.{
        parent = p2wp base p
      ; child = p2wp base c
      }
  | ParentTooOld (p, dmy) ->
    set_list parent_too_old
      M.Warning_parent_too_old.{
        parent = p2wp base p
      ; date = string_of_prec_dmy dmy
      }
  | ParentTooYoung (p, dmy) ->
    set_list parent_too_young
      M.Warning_parent_too_young.{
        parent = p2wp base p
      ; date = string_of_prec_dmy dmy
      }
  | PossibleDuplicateFam (f1, f2) ->
    let f1 = fam_to_piqi_family conf base f1 in
    let f2 = fam_to_piqi_family conf base f2 in
      let w =
        M.Warning_possible_duplicate_fam.{
          family1 = f1
        ; family2 = f2
        }
      in
      set_list possible_duplicate_fam w
  | PEventOrder (p, e1, e2) ->
    set_list pevent_order
      M.Warning_pevent_order.{
        person = p2wp base p
      ; event1 = pn2s e1.epers_name
      ; event2 = pn2s e2.epers_name
      }
  | PWitnessEventAfterDeath (p, e) ->
    set_list pevent_witness_after_death
      M.Warning_pwitness_event_after_death.{
        person = p2wp base p
      ; event = pn2s e.epers_name
      }
  | PWitnessEventBeforeBirth (p, e) ->
    set_list pevent_witness_before_birth
      M.Warning_pwitness_event_before_birth.{
        person = p2wp base p
      ; event = pn2s e.epers_name
      }
  | TitleDatesError (p, _) ->
    set_list title_dates_error
      M.Warning_title_dates_error.{ person = p2wp base p }
  | UndefinedSex p ->
    set_list undefined_sex
      M.Warning_undefined_sex.{ person = p2wp base p }
  | WitnessDateAfterDeath p ->
    set_list witness_date_after_death
      M.Warning_witness_date_after_death.{ person = p2wp base p }
  | WitnessDateBeforeBirth p ->
    set_list witness_date_before_birth
      M.Warning_witness_date_before_birth.{ person = p2wp base p }
  | YoungForMarriage (p, dmy) ->
    set_list young_for_marriage
      M.Warning_young_for_marriage.{
        person = p2wp base p
      ; date = string_of_prec_dmy dmy
      }
  | OldForMarriage (p, dmy) ->
    set_list old_for_marriage
      M.Warning_old_for_marriage.{
        person = p2wp base p
      ; date = string_of_prec_dmy dmy
      }

let create_piqi_warnings () =
  (* Ajouter une limite. Pour pierfit, on peut
     exploser la taille des données à envoyer. *)
  M.Base_warnings.{
    warning_already_defined = !already_defined;
    warning_own_ancestor = !own_ancestor;
    warning_bad_sex_of_married_person = !bad_sex_of_married_person;
    warning_birth_after_death = !birth_after_death;
    warning_incoherent_sex = !incoherent_sex;
    warning_changed_order_of_children = !changed_order_of_children;
    warning_children_not_in_order = !children_not_in_order;
    warning_dead_too_early_to_be_father = !dead_too_early_to_be_father;
    warning_incoherent_ancestor_date = !incoherent_ancestor_date;
    warning_marriage_date_after_death = !marriage_date_after_death;
    warning_marriage_date_before_birth = !marriage_date_before_birth;
    warning_mother_dead_before_child_birth = !mother_dead_before_child_birth;
    warning_parent_born_after_child = !parent_born_after_child;
    warning_parent_too_young = !parent_too_young;
    warning_possible_duplicate_fam = !possible_duplicate_fam;
    warning_title_dates_error = !title_dates_error;
    warning_undefined_sex = !undefined_sex;
    warning_young_for_marriage = !young_for_marriage;
    warning_old_for_marriage = !old_for_marriage;
    warning_close_children = !close_children;
    warning_parent_too_old = !parent_too_old;
    warning_changed_order_of_marriages = !changed_order_of_marriages;
    warning_big_age_between_spouses = !big_age_between_spouses;
    warning_dead_old = !dead_old;
    warning_old_individual = !old_individual;
    warning_witness_date_after_death = !witness_date_after_death;
    warning_witness_date_before_birth = !witness_date_before_birth;
    warning_distant_children = !distant_children;
  }

#endif
