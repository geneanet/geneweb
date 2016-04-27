module rec Api_piqi:
  sig
    type protobuf_int64 = int64
    type protobuf_int32 = int32
    type sex =
      [
        | `male
        | `female
        | `unknown
      ]
    type death_type =
      [
        | `not_dead
        | `dead
        | `dead_young
        | `dead_dont_know_when
        | `dont_know_if_dead
        | `of_course_dead
      ]
    type marriage_type =
      [
        | `married
        | `not_married
        | `engaged
        | `no_sexes_check_not_married
        | `no_mention
        | `no_sexes_check_married
      ]
    type divorce_type =
      [
        | `not_divorced
        | `divorced
        | `separated
      ]
    type relation_parent_type =
      [
        | `rpt_adoption
        | `rpt_recognition
        | `rpt_candidate_parent
        | `rpt_god_parent
        | `rpt_foster_parent
      ]
    type title_type =
      [
        | `title_main
        | `title_name
        | `title_none
      ]
    type search_type =
      [
        | `starting_with
        | `approximative
        | `lastname_or_firstname
      ]
    type relation_type =
      [
        | `self
        | `spouse
        | `sibling
        | `step_brother
        | `parent
        | `step_parent
        | `grand_parent
        | `uncle
        | `uncle_spouse
        | `cousin
        | `cousin_spouse
        | `child
        | `step_child
        | `grand_child
        | `grand_child_spouse
        | `great_grand_child
        | `great_grand_child_spouse
        | `child_cousin
        | `child_cousin_spouse
        | `grand_child_cousin
        | `grand_child_cousin_spouse
        | `great_grand_child_cousin
        | `great_grand_child_cousin_spouse
        | `nephew
        | `nephew_spouse
        | `nephew_spouse_spouse
        | `grand_nephew
        | `grand_nephew_spouse
        | `grand_nephew_spouse_spouse
        | `great_grand_nephew
        | `great_grand_nephew_spouse
        | `great_grand_nephew_spouse_spouse
        | `adoptive_parent
        | `adoptive_child
        | `recognized_parent
        | `recognized_child
        | `candidate_parent
        | `candidate_child
        | `god_parent
        | `god_child
        | `foster_parent
        | `foster_child
        | `witness
        | `no_relation
      ]
    type notif_birthday_params =
      [
        | `close_person
        | `descend_grand_parent
        | `descend_great_grand_parent
      ]
    type infos_base = Infos_base.t
    type reference_person = Reference_person.t
    type list_reference_persons = List_reference_persons.t
    type relation_parent = Relation_parent.t
    type title = Title.t
    type spouse = Spouse.t
    type person = Person.t
    type full_person = Full_person.t
    type full_family = Full_family.t
    type internal_int32 = Internal_int32.t
    type list_persons = List_persons.t
    type list_full_persons = List_full_persons.t
    type list_full_families = List_full_families.t
    type search_params = Search_params.t
    type image = Image.t
    type full_image = Full_image.t
    type list_images = List_images.t
    type list_full_images = List_full_images.t
    type pers_img = Pers_img.t
    type list_pers_img = List_pers_img.t
    type index = Index.t
    type image_address = Image_address.t
    type close_persons_params = Close_persons_params.t
    type person_relation = Person_relation.t
    type full_person_relation = Full_person_relation.t
    type list_person_relation = List_person_relation.t
    type list_full_person_relation = List_full_person_relation.t
    type anniversary_params = Anniversary_params.t
    type graph_params = Graph_params.t
    type graph_rel_params = Graph_rel_params.t
    type cpl_rel_params = Cpl_rel_params.t
    type node = Node.t
    type full_node = Full_node.t
    type edge = Edge.t
    type graph = Graph.t
    type full_graph = Full_graph.t
    type all_persons_params = All_persons_params.t
    type all_families_params = All_families_params.t
    type warning_already_defined = Warning_already_defined.t
    type warning_own_ancestor = Warning_own_ancestor.t
    type warning_bad_sex_of_married_person = Warning_bad_sex_of_married_person.t
    type warning_birth_after_death = Warning_birth_after_death.t
    type warning_incoherent_sex = Warning_incoherent_sex.t
    type warning_changed_order_of_children = Warning_changed_order_of_children.t
    type warning_changed_order_of_marriages = Warning_changed_order_of_marriages.t
    type warning_children_not_in_order = Warning_children_not_in_order.t
    type warning_dead_too_early_to_be_father = Warning_dead_too_early_to_be_father.t
    type warning_incoherent_ancestor_date = Warning_incoherent_ancestor_date.t
    type warning_marriage_date_after_death = Warning_marriage_date_after_death.t
    type warning_marriage_date_before_birth = Warning_marriage_date_before_birth.t
    type warning_mother_dead_before_child_birth = Warning_mother_dead_before_child_birth.t
    type warning_parent_born_after_child = Warning_parent_born_after_child.t
    type warning_parent_too_young = Warning_parent_too_young.t
    type warning_title_dates_error = Warning_title_dates_error.t
    type warning_undefined_sex = Warning_undefined_sex.t
    type warning_young_for_marriage = Warning_young_for_marriage.t
    type warning_parent_too_old = Warning_parent_too_old.t
    type warning_close_children = Warning_close_children.t
    type warning_big_age_between_spouses = Warning_big_age_between_spouses.t
    type warning_dead_old = Warning_dead_old.t
    type warning_old_individual = Warning_old_individual.t
    type warning_witness_date_after_death = Warning_witness_date_after_death.t
    type warning_witness_date_before_birth = Warning_witness_date_before_birth.t
    type warning_changed_order_of_family_events = Warning_changed_order_of_family_events.t
    type warning_changed_order_of_person_events = Warning_changed_order_of_person_events.t
    type warning_fevent_order = Warning_fevent_order.t
    type warning_fwitness_event_after_death = Warning_fwitness_event_after_death.t
    type warning_fwitness_event_before_birth = Warning_fwitness_event_before_birth.t
    type warning_pevent_order = Warning_pevent_order.t
    type warning_pwitness_event_after_death = Warning_pwitness_event_after_death.t
    type warning_pwitness_event_before_birth = Warning_pwitness_event_before_birth.t
    type base_warnings = Base_warnings.t
    type filter_date = Filter_date.t
    type filter_date_range = Filter_date_range.t
    type filters = Filters.t
    type modification_status = Modification_status.t
    type notification_birthday_params = Notification_birthday_params.t
    type notification_birthday = Notification_birthday.t
    type person_start = Person_start.t
    type synchro_params = Synchro_params.t
    type last_modifications = Last_modifications.t
    type last_visits = Last_visits.t
    type correspondance_family = Correspondance_family.t
    type correspondance = Correspondance.t
    type correspondance_list = Correspondance_list.t
  end = Api_piqi
and Infos_base:
  sig
    type t = {
      mutable nb_persons: Api_piqi.protobuf_int64;
      mutable nb_families: Api_piqi.protobuf_int64;
      mutable sosa: Api_piqi.reference_person option;
      mutable last_modified_person: Api_piqi.protobuf_int64 option;
      mutable real_nb_persons: Api_piqi.protobuf_int64 option;
    }
  end = Infos_base
and Reference_person:
  sig
    type t = {
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
    }
  end = Reference_person
and List_reference_persons:
  sig
    type t = {
      mutable list_ref_persons: Api_piqi.reference_person list;
    }
  end = List_reference_persons
and Relation_parent:
  sig
    type t = {
      mutable father: Api_piqi.protobuf_int32 option;
      mutable mother: Api_piqi.protobuf_int32 option;
      mutable source: string option;
      mutable rpt_type: Api_piqi.relation_parent_type;
    }
  end = Relation_parent
and Title:
  sig
    type t = {
      mutable title_type: Api_piqi.title_type;
      mutable name: string option;
      mutable title: string option;
      mutable fief: string option;
      mutable date_begin: string option;
      mutable date_end: string option;
      mutable nth: Api_piqi.protobuf_int32 option;
    }
  end = Title
and Spouse:
  sig
    type t = {
      mutable sosa: string;
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
      mutable sex: Api_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable public_name: string option;
      mutable image: string;
      mutable birth_date: string;
      mutable birth_place: string;
      mutable baptism_date: string;
      mutable baptism_place: string;
      mutable death_date: string;
      mutable death_place: string;
      mutable death_type: Api_piqi.death_type;
      mutable burial_date: string;
      mutable burial_place: string;
      mutable marriage_date: string;
      mutable marriage_place: string;
      mutable divorce_type: Api_piqi.divorce_type;
      mutable visible_for_visitors: bool;
    }
  end = Spouse
and Person:
  sig
    type t = {
      mutable sosa: string;
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
      mutable sex: Api_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable public_name: string option;
      mutable image: string;
      mutable birth_date: string;
      mutable birth_place: string;
      mutable baptism_date: string;
      mutable baptism_place: string;
      mutable death_date: string;
      mutable death_place: string;
      mutable death_type: Api_piqi.death_type;
      mutable burial_date: string;
      mutable burial_place: string;
      mutable spouses: Api_piqi.spouse list;
      mutable ascend: bool;
      mutable descend: bool;
      mutable visible_for_visitors: bool;
      mutable baseprefix: string;
    }
  end = Person
and Full_person:
  sig
    type t = {
      mutable sosa: string;
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
      mutable index: Api_piqi.protobuf_int32;
      mutable sex: Api_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable public_name: string option;
      mutable aliases: string list;
      mutable qualifiers: string list;
      mutable firstname_aliases: string list;
      mutable surname_aliases: string list;
      mutable image: string option;
      mutable birth_date: string option;
      mutable birth_place: string option;
      mutable birth_src: string option;
      mutable baptism_date: string option;
      mutable baptism_place: string option;
      mutable baptism_src: string option;
      mutable death_date: string option;
      mutable death_place: string option;
      mutable death_src: string option;
      mutable death_type: Api_piqi.death_type;
      mutable burial_date: string option;
      mutable burial_place: string option;
      mutable burial_src: string option;
      mutable occupation: string option;
      mutable psources: string option;
      mutable titles: Api_piqi.title list;
      mutable related: Api_piqi.internal_int32 list;
      mutable rparents: Api_piqi.relation_parent list;
      mutable visible_for_visitors: bool;
      mutable parents: Api_piqi.protobuf_int32 option;
      mutable families: Api_piqi.internal_int32 list;
      mutable baseprefix: string;
    }
  end = Full_person
and Full_family:
  sig
    type t = {
      mutable fsources: string option;
      mutable marriage_date: string option;
      mutable marriage_place: string option;
      mutable marriage_src: string option;
      mutable marriage_type: Api_piqi.marriage_type;
      mutable divorce_type: Api_piqi.divorce_type;
      mutable divorce_date: string option;
      mutable witnesses: Api_piqi.internal_int32 list;
      mutable father: Api_piqi.protobuf_int32;
      mutable mother: Api_piqi.protobuf_int32;
      mutable children: Api_piqi.internal_int32 list;
      mutable index: Api_piqi.protobuf_int32;
    }
  end = Full_family
and Internal_int32:
  sig
    type t = {
      mutable value: Api_piqi.protobuf_int32;
    }
  end = Internal_int32
and List_persons:
  sig
    type t = {
      mutable list_persons: Api_piqi.person list;
    }
  end = List_persons
and List_full_persons:
  sig
    type t = {
      mutable persons: Api_piqi.full_person list;
    }
  end = List_full_persons
and List_full_families:
  sig
    type t = {
      mutable families: Api_piqi.full_family list;
    }
  end = List_full_families
and Search_params:
  sig
    type t = {
      mutable search_type: Api_piqi.search_type;
      mutable lastname: string option;
      mutable firstname: string option;
      mutable only_sosa: bool;
      mutable only_recent: bool;
      mutable maiden_name: bool;
    }
  end = Search_params
and Image:
  sig
    type t = {
      mutable person: Api_piqi.person;
      mutable img: string;
    }
  end = Image
and Full_image:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable img: string;
    }
  end = Full_image
and List_images:
  sig
    type t = {
      mutable list_images: Api_piqi.image list;
    }
  end = List_images
and List_full_images:
  sig
    type t = {
      mutable images: Api_piqi.full_image list;
    }
  end = List_full_images
and Pers_img:
  sig
    type t = {
      mutable person: Api_piqi.reference_person;
      mutable img: string;
    }
  end = Pers_img
and List_pers_img:
  sig
    type t = {
      mutable list_pers_img: Api_piqi.pers_img list;
    }
  end = List_pers_img
and Index:
  sig
    type t = {
      mutable index: Api_piqi.protobuf_int32;
    }
  end = Index
and Image_address:
  sig
    type t = {
      mutable img: string;
    }
  end = Image_address
and Close_persons_params:
  sig
    type t = {
      mutable person: Api_piqi.reference_person;
      mutable nb_gen_asc: Api_piqi.protobuf_int32 option;
      mutable nb_gen_desc: Api_piqi.protobuf_int32 option;
      mutable spouse_ascend: bool;
      mutable only_recent: bool;
    }
  end = Close_persons_params
and Person_relation:
  sig
    type t = {
      mutable person: Api_piqi.person;
      mutable relation: Api_piqi.relation_type;
    }
  end = Person_relation
and Full_person_relation:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable relation: Api_piqi.relation_type;
    }
  end = Full_person_relation
and List_person_relation:
  sig
    type t = {
      mutable person_relations: Api_piqi.person_relation list;
    }
  end = List_person_relation
and List_full_person_relation:
  sig
    type t = {
      mutable person_relations: Api_piqi.full_person_relation list;
    }
  end = List_full_person_relation
and Anniversary_params:
  sig
    type t = {
      mutable month: Api_piqi.protobuf_int32 option;
    }
  end = Anniversary_params
and Graph_params:
  sig
    type t = {
      mutable generation: Api_piqi.protobuf_int32 option;
      mutable person: Api_piqi.reference_person;
    }
  end = Graph_params
and Graph_rel_params:
  sig
    type t = {
      mutable person1: Api_piqi.reference_person;
      mutable person2: Api_piqi.reference_person;
    }
  end = Graph_rel_params
and Cpl_rel_params:
  sig
    type t = {
      mutable person1: Api_piqi.reference_person;
      mutable person2: Api_piqi.reference_person;
    }
  end = Cpl_rel_params
and Node:
  sig
    type t = {
      mutable id: Api_piqi.protobuf_int64;
      mutable person: Api_piqi.person;
    }
  end = Node
and Full_node:
  sig
    type t = {
      mutable id: Api_piqi.protobuf_int64;
      mutable person: Api_piqi.full_person;
    }
  end = Full_node
and Edge:
  sig
    type t = {
      mutable from_node: Api_piqi.protobuf_int64;
      mutable to_node: Api_piqi.protobuf_int64;
    }
  end = Edge
and Graph:
  sig
    type t = {
      mutable nodes: Api_piqi.node list;
      mutable edges: Api_piqi.edge list;
    }
  end = Graph
and Full_graph:
  sig
    type t = {
      mutable nodes: Api_piqi.full_node list;
      mutable edges: Api_piqi.edge list;
      mutable families: Api_piqi.full_family list;
    }
  end = Full_graph
and All_persons_params:
  sig
    type t = {
      mutable from: Api_piqi.protobuf_int32 option;
      mutable limit: Api_piqi.protobuf_int32 option;
    }
  end = All_persons_params
and All_families_params:
  sig
    type t = {
      mutable from: Api_piqi.protobuf_int32 option;
      mutable limit: Api_piqi.protobuf_int32 option;
    }
  end = All_families_params
and Warning_already_defined:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_already_defined
and Warning_own_ancestor:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_own_ancestor
and Warning_bad_sex_of_married_person:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_bad_sex_of_married_person
and Warning_birth_after_death:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_birth_after_death
and Warning_incoherent_sex:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_incoherent_sex
and Warning_changed_order_of_children:
  sig
    type t = {
      mutable father: Api_piqi.full_person;
      mutable mother: Api_piqi.full_person;
    }
  end = Warning_changed_order_of_children
and Warning_changed_order_of_marriages:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_changed_order_of_marriages
and Warning_children_not_in_order:
  sig
    type t = {
      mutable father: Api_piqi.full_person;
      mutable mother: Api_piqi.full_person;
    }
  end = Warning_children_not_in_order
and Warning_dead_too_early_to_be_father:
  sig
    type t = {
      mutable son: Api_piqi.full_person;
      mutable father: Api_piqi.full_person;
    }
  end = Warning_dead_too_early_to_be_father
and Warning_incoherent_ancestor_date:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable ancestor: Api_piqi.full_person;
    }
  end = Warning_incoherent_ancestor_date
and Warning_marriage_date_after_death:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_marriage_date_after_death
and Warning_marriage_date_before_birth:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_marriage_date_before_birth
and Warning_mother_dead_before_child_birth:
  sig
    type t = {
      mutable mother: Api_piqi.full_person;
      mutable child: Api_piqi.full_person;
    }
  end = Warning_mother_dead_before_child_birth
and Warning_parent_born_after_child:
  sig
    type t = {
      mutable parent: Api_piqi.full_person;
      mutable child: Api_piqi.full_person;
    }
  end = Warning_parent_born_after_child
and Warning_parent_too_young:
  sig
    type t = {
      mutable parent: Api_piqi.full_person;
      mutable date: string;
    }
  end = Warning_parent_too_young
and Warning_title_dates_error:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_title_dates_error
and Warning_undefined_sex:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_undefined_sex
and Warning_young_for_marriage:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable date: string;
    }
  end = Warning_young_for_marriage
and Warning_parent_too_old:
  sig
    type t = {
      mutable parent: Api_piqi.full_person;
      mutable date: string;
    }
  end = Warning_parent_too_old
and Warning_close_children:
  sig
    type t = {
      mutable father: Api_piqi.full_person;
      mutable mother: Api_piqi.full_person;
      mutable child1: Api_piqi.full_person;
      mutable child2: Api_piqi.full_person;
    }
  end = Warning_close_children
and Warning_big_age_between_spouses:
  sig
    type t = {
      mutable father: Api_piqi.full_person;
      mutable mother: Api_piqi.full_person;
      mutable date: string;
    }
  end = Warning_big_age_between_spouses
and Warning_dead_old:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable date: string;
    }
  end = Warning_dead_old
and Warning_old_individual:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable date: string;
    }
  end = Warning_old_individual
and Warning_witness_date_after_death:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_witness_date_after_death
and Warning_witness_date_before_birth:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_witness_date_before_birth
and Warning_changed_order_of_family_events:
  sig
    type t = {
      mutable father: Api_piqi.full_person;
      mutable mother: Api_piqi.full_person;
    }
  end = Warning_changed_order_of_family_events
and Warning_changed_order_of_person_events:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
    }
  end = Warning_changed_order_of_person_events
and Warning_fevent_order:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable event1: string;
      mutable event2: string;
    }
  end = Warning_fevent_order
and Warning_fwitness_event_after_death:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable event: string;
    }
  end = Warning_fwitness_event_after_death
and Warning_fwitness_event_before_birth:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable event: string;
    }
  end = Warning_fwitness_event_before_birth
and Warning_pevent_order:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable event1: string;
      mutable event2: string;
    }
  end = Warning_pevent_order
and Warning_pwitness_event_after_death:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable event: string;
    }
  end = Warning_pwitness_event_after_death
and Warning_pwitness_event_before_birth:
  sig
    type t = {
      mutable person: Api_piqi.full_person;
      mutable event: string;
    }
  end = Warning_pwitness_event_before_birth
and Base_warnings:
  sig
    type t = {
      mutable warning_already_defined: Api_piqi.warning_already_defined list;
      mutable warning_own_ancestor: Api_piqi.warning_own_ancestor list;
      mutable warning_bad_sex_of_married_person: Api_piqi.warning_bad_sex_of_married_person list;
      mutable warning_birth_after_death: Api_piqi.warning_birth_after_death list;
      mutable warning_incoherent_sex: Api_piqi.warning_incoherent_sex list;
      mutable warning_changed_order_of_children: Api_piqi.warning_changed_order_of_children list;
      mutable warning_children_not_in_order: Api_piqi.warning_children_not_in_order list;
      mutable warning_dead_too_early_to_be_father: Api_piqi.warning_dead_too_early_to_be_father list;
      mutable warning_incoherent_ancestor_date: Api_piqi.warning_incoherent_ancestor_date list;
      mutable warning_marriage_date_after_death: Api_piqi.warning_marriage_date_after_death list;
      mutable warning_marriage_date_before_birth: Api_piqi.warning_marriage_date_before_birth list;
      mutable warning_mother_dead_before_child_birth: Api_piqi.warning_mother_dead_before_child_birth list;
      mutable warning_parent_born_after_child: Api_piqi.warning_parent_born_after_child list;
      mutable warning_parent_too_young: Api_piqi.warning_parent_too_young list;
      mutable warning_title_dates_error: Api_piqi.warning_title_dates_error list;
      mutable warning_undefined_sex: Api_piqi.warning_undefined_sex list;
      mutable warning_young_for_marriage: Api_piqi.warning_young_for_marriage list;
      mutable warning_close_children: Api_piqi.warning_close_children list;
      mutable warning_parent_too_old: Api_piqi.warning_parent_too_old list;
      mutable warning_changed_order_of_marriages: Api_piqi.warning_changed_order_of_marriages list;
      mutable warning_big_age_between_spouses: Api_piqi.warning_big_age_between_spouses list;
      mutable warning_dead_old: Api_piqi.warning_dead_old list;
      mutable warning_old_individual: Api_piqi.warning_old_individual list;
      mutable warning_witness_date_after_death: Api_piqi.warning_witness_date_after_death list;
      mutable warning_witness_date_before_birth: Api_piqi.warning_witness_date_before_birth list;
    }
  end = Base_warnings
and Filter_date:
  sig
    type t = {
      mutable day: Api_piqi.protobuf_int32;
      mutable month: Api_piqi.protobuf_int32;
      mutable year: Api_piqi.protobuf_int32;
    }
  end = Filter_date
and Filter_date_range:
  sig
    type t = {
      mutable date_begin: Api_piqi.filter_date;
      mutable date_end: Api_piqi.filter_date;
      mutable only_exact: bool;
    }
  end = Filter_date_range
and Filters:
  sig
    type t = {
      mutable only_sosa: bool;
      mutable only_recent: bool;
      mutable sex: Api_piqi.sex option;
      mutable nb_results: bool;
      mutable date_birth: Api_piqi.filter_date_range option;
      mutable date_death: Api_piqi.filter_date_range option;
    }
  end = Filters
and Modification_status:
  sig
    type t = {
      mutable status: bool;
      mutable base_warnings: Api_piqi.base_warnings;
      mutable index: Api_piqi.protobuf_int32 option;
    }
  end = Modification_status
and Notification_birthday_params:
  sig
    type t = {
      mutable person: Api_piqi.reference_person;
      mutable params: Api_piqi.notif_birthday_params;
      mutable month: Api_piqi.protobuf_int32 option;
      mutable day: Api_piqi.protobuf_int32 option;
    }
  end = Notification_birthday_params
and Notification_birthday:
  sig
    type t = {
      mutable number: Api_piqi.protobuf_int32;
      mutable has_proprio_birthday: bool;
      mutable firstname1: string option;
      mutable firstname2: string option;
      mutable firstname3: string option;
    }
  end = Notification_birthday
and Person_start:
  sig
    type t = {
      mutable lastname: string;
      mutable firstname: string;
      mutable sex: Api_piqi.sex;
      mutable birth_date_day: Api_piqi.protobuf_int32 option;
      mutable birth_date_month: Api_piqi.protobuf_int32 option;
      mutable birth_date_year: Api_piqi.protobuf_int32 option;
    }
  end = Person_start
and Synchro_params:
  sig
    type t = {
      mutable export_directory: string;
      mutable timestamp: string;
    }
  end = Synchro_params
and Last_modifications:
  sig
    type t = {
      mutable wizard: string option;
      mutable max_res: Api_piqi.protobuf_int32 option;
      mutable range: Api_piqi.filter_date_range option;
    }
  end = Last_modifications
and Last_visits:
  sig
    type t = {
      mutable user: string;
    }
  end = Last_visits
and Correspondance_family:
  sig
    type t = {
      mutable index: Api_piqi.protobuf_int32;
      mutable spouse: Api_piqi.person;
      mutable children: Api_piqi.person list;
    }
  end = Correspondance_family
and Correspondance:
  sig
    type t = {
      mutable base: string;
      mutable person: Api_piqi.person;
      mutable father: Api_piqi.person option;
      mutable mother: Api_piqi.person option;
      mutable families: Api_piqi.correspondance_family list;
    }
  end = Correspondance
and Correspondance_list:
  sig
    type t = {
      mutable correspondances: Api_piqi.correspondance list;
    }
  end = Correspondance_list


let rec parse_int64 x = Piqirun.int64_of_zigzag_varint x
and packed_parse_int64 x = Piqirun.int64_of_packed_zigzag_varint x

and parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x

and parse_protobuf_int64 x = Piqirun.int64_of_signed_varint x
and packed_parse_protobuf_int64 x = Piqirun.int64_of_packed_signed_varint x

and parse_string x = Piqirun.string_of_block x

and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_infos_base x =
  let x = Piqirun.parse_record x in
  let _nb_persons, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _nb_families, x = Piqirun.parse_required_field 2 parse_protobuf_int64 x in
  let _sosa, x = Piqirun.parse_optional_field 3 parse_reference_person x in
  let _last_modified_person, x = Piqirun.parse_optional_field 4 parse_protobuf_int64 x in
  let _real_nb_persons, x = Piqirun.parse_optional_field 5 parse_protobuf_int64 x in
  Piqirun.check_unparsed_fields x;
  {
    Infos_base.nb_persons = _nb_persons;
    Infos_base.nb_families = _nb_families;
    Infos_base.sosa = _sosa;
    Infos_base.last_modified_person = _last_modified_person;
    Infos_base.real_nb_persons = _real_nb_persons;
  }

and parse_reference_person x =
  let x = Piqirun.parse_record x in
  let _n, x = Piqirun.parse_required_field 1 parse_string x in
  let _p, x = Piqirun.parse_required_field 2 parse_string x in
  let _oc, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Reference_person.n = _n;
    Reference_person.p = _p;
    Reference_person.oc = _oc;
  }

and parse_list_reference_persons x =
  let x = Piqirun.parse_record x in
  let _list_ref_persons, x = Piqirun.parse_repeated_field 1 parse_reference_person x in
  Piqirun.check_unparsed_fields x;
  {
    List_reference_persons.list_ref_persons = _list_ref_persons;
  }

and parse_relation_parent x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _mother, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _source, x = Piqirun.parse_optional_field 3 parse_string x in
  let _rpt_type, x = Piqirun.parse_required_field 4 parse_relation_parent_type x in
  Piqirun.check_unparsed_fields x;
  {
    Relation_parent.father = _father;
    Relation_parent.mother = _mother;
    Relation_parent.source = _source;
    Relation_parent.rpt_type = _rpt_type;
  }

and parse_title x =
  let x = Piqirun.parse_record x in
  let _title_type, x = Piqirun.parse_required_field 1 parse_title_type x in
  let _name, x = Piqirun.parse_optional_field 2 parse_string x in
  let _title, x = Piqirun.parse_optional_field 3 parse_string x in
  let _fief, x = Piqirun.parse_optional_field 4 parse_string x in
  let _date_begin, x = Piqirun.parse_optional_field 5 parse_string x in
  let _date_end, x = Piqirun.parse_optional_field 6 parse_string x in
  let _nth, x = Piqirun.parse_optional_field 7 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Title.title_type = _title_type;
    Title.name = _name;
    Title.title = _title;
    Title.fief = _fief;
    Title.date_begin = _date_begin;
    Title.date_end = _date_end;
    Title.nth = _nth;
  }

and parse_spouse x =
  let x = Piqirun.parse_record x in
  let _sosa, x = Piqirun.parse_required_field 1 parse_string x in
  let _n, x = Piqirun.parse_required_field 2 parse_string x in
  let _p, x = Piqirun.parse_required_field 3 parse_string x in
  let _oc, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 5 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 6 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 7 parse_string x in
  let _public_name, x = Piqirun.parse_optional_field 8 parse_string x in
  let _image, x = Piqirun.parse_required_field 9 parse_string x in
  let _birth_date, x = Piqirun.parse_required_field 10 parse_string x in
  let _birth_place, x = Piqirun.parse_required_field 11 parse_string x in
  let _baptism_date, x = Piqirun.parse_required_field 12 parse_string x in
  let _baptism_place, x = Piqirun.parse_required_field 13 parse_string x in
  let _death_date, x = Piqirun.parse_required_field 14 parse_string x in
  let _death_place, x = Piqirun.parse_required_field 15 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 16 parse_death_type x in
  let _burial_date, x = Piqirun.parse_required_field 17 parse_string x in
  let _burial_place, x = Piqirun.parse_required_field 18 parse_string x in
  let _marriage_date, x = Piqirun.parse_required_field 19 parse_string x in
  let _marriage_place, x = Piqirun.parse_required_field 20 parse_string x in
  let _divorce_type, x = Piqirun.parse_required_field 21 parse_divorce_type x in
  let _visible_for_visitors, x = Piqirun.parse_required_field 22 parse_bool x in
  Piqirun.check_unparsed_fields x;
  {
    Spouse.sosa = _sosa;
    Spouse.n = _n;
    Spouse.p = _p;
    Spouse.oc = _oc;
    Spouse.sex = _sex;
    Spouse.lastname = _lastname;
    Spouse.firstname = _firstname;
    Spouse.public_name = _public_name;
    Spouse.image = _image;
    Spouse.birth_date = _birth_date;
    Spouse.birth_place = _birth_place;
    Spouse.baptism_date = _baptism_date;
    Spouse.baptism_place = _baptism_place;
    Spouse.death_date = _death_date;
    Spouse.death_place = _death_place;
    Spouse.death_type = _death_type;
    Spouse.burial_date = _burial_date;
    Spouse.burial_place = _burial_place;
    Spouse.marriage_date = _marriage_date;
    Spouse.marriage_place = _marriage_place;
    Spouse.divorce_type = _divorce_type;
    Spouse.visible_for_visitors = _visible_for_visitors;
  }

and parse_person x =
  let x = Piqirun.parse_record x in
  let _sosa, x = Piqirun.parse_required_field 1 parse_string x in
  let _n, x = Piqirun.parse_required_field 2 parse_string x in
  let _p, x = Piqirun.parse_required_field 3 parse_string x in
  let _oc, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 5 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 6 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 7 parse_string x in
  let _public_name, x = Piqirun.parse_optional_field 8 parse_string x in
  let _image, x = Piqirun.parse_required_field 9 parse_string x in
  let _birth_date, x = Piqirun.parse_required_field 10 parse_string x in
  let _birth_place, x = Piqirun.parse_required_field 11 parse_string x in
  let _baptism_date, x = Piqirun.parse_required_field 12 parse_string x in
  let _baptism_place, x = Piqirun.parse_required_field 13 parse_string x in
  let _death_date, x = Piqirun.parse_required_field 14 parse_string x in
  let _death_place, x = Piqirun.parse_required_field 15 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 16 parse_death_type x in
  let _burial_date, x = Piqirun.parse_required_field 17 parse_string x in
  let _burial_place, x = Piqirun.parse_required_field 18 parse_string x in
  let _spouses, x = Piqirun.parse_repeated_field 19 parse_spouse x in
  let _ascend, x = Piqirun.parse_required_field 20 parse_bool x in
  let _descend, x = Piqirun.parse_required_field 21 parse_bool x in
  let _visible_for_visitors, x = Piqirun.parse_required_field 22 parse_bool x in
  let _baseprefix, x = Piqirun.parse_required_field 23 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Person.sosa = _sosa;
    Person.n = _n;
    Person.p = _p;
    Person.oc = _oc;
    Person.sex = _sex;
    Person.lastname = _lastname;
    Person.firstname = _firstname;
    Person.public_name = _public_name;
    Person.image = _image;
    Person.birth_date = _birth_date;
    Person.birth_place = _birth_place;
    Person.baptism_date = _baptism_date;
    Person.baptism_place = _baptism_place;
    Person.death_date = _death_date;
    Person.death_place = _death_place;
    Person.death_type = _death_type;
    Person.burial_date = _burial_date;
    Person.burial_place = _burial_place;
    Person.spouses = _spouses;
    Person.ascend = _ascend;
    Person.descend = _descend;
    Person.visible_for_visitors = _visible_for_visitors;
    Person.baseprefix = _baseprefix;
  }

and parse_full_person x =
  let x = Piqirun.parse_record x in
  let _sosa, x = Piqirun.parse_required_field 1 parse_string x in
  let _n, x = Piqirun.parse_required_field 2 parse_string x in
  let _p, x = Piqirun.parse_required_field 3 parse_string x in
  let _oc, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  let _index, x = Piqirun.parse_required_field 5 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 6 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 7 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 8 parse_string x in
  let _public_name, x = Piqirun.parse_optional_field 9 parse_string x in
  let _aliases, x = Piqirun.parse_repeated_field 10 parse_string x in
  let _qualifiers, x = Piqirun.parse_repeated_field 11 parse_string x in
  let _firstname_aliases, x = Piqirun.parse_repeated_field 12 parse_string x in
  let _surname_aliases, x = Piqirun.parse_repeated_field 13 parse_string x in
  let _image, x = Piqirun.parse_optional_field 15 parse_string x in
  let _birth_date, x = Piqirun.parse_optional_field 16 parse_string x in
  let _birth_place, x = Piqirun.parse_optional_field 17 parse_string x in
  let _birth_src, x = Piqirun.parse_optional_field 18 parse_string x in
  let _baptism_date, x = Piqirun.parse_optional_field 19 parse_string x in
  let _baptism_place, x = Piqirun.parse_optional_field 20 parse_string x in
  let _baptism_src, x = Piqirun.parse_optional_field 21 parse_string x in
  let _death_date, x = Piqirun.parse_optional_field 22 parse_string x in
  let _death_place, x = Piqirun.parse_optional_field 23 parse_string x in
  let _death_src, x = Piqirun.parse_optional_field 24 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 25 parse_death_type x in
  let _burial_date, x = Piqirun.parse_optional_field 26 parse_string x in
  let _burial_place, x = Piqirun.parse_optional_field 27 parse_string x in
  let _burial_src, x = Piqirun.parse_optional_field 28 parse_string x in
  let _occupation, x = Piqirun.parse_optional_field 30 parse_string x in
  let _psources, x = Piqirun.parse_optional_field 31 parse_string x in
  let _titles, x = Piqirun.parse_repeated_field 32 parse_title x in
  let _related, x = Piqirun.parse_repeated_field 33 parse_internal_int32 x in
  let _rparents, x = Piqirun.parse_repeated_field 34 parse_relation_parent x in
  let _visible_for_visitors, x = Piqirun.parse_required_field 35 parse_bool x in
  let _parents, x = Piqirun.parse_optional_field 36 parse_protobuf_int32 x in
  let _families, x = Piqirun.parse_repeated_field 37 parse_internal_int32 x in
  let _baseprefix, x = Piqirun.parse_required_field 38 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Full_person.sosa = _sosa;
    Full_person.n = _n;
    Full_person.p = _p;
    Full_person.oc = _oc;
    Full_person.index = _index;
    Full_person.sex = _sex;
    Full_person.lastname = _lastname;
    Full_person.firstname = _firstname;
    Full_person.public_name = _public_name;
    Full_person.aliases = _aliases;
    Full_person.qualifiers = _qualifiers;
    Full_person.firstname_aliases = _firstname_aliases;
    Full_person.surname_aliases = _surname_aliases;
    Full_person.image = _image;
    Full_person.birth_date = _birth_date;
    Full_person.birth_place = _birth_place;
    Full_person.birth_src = _birth_src;
    Full_person.baptism_date = _baptism_date;
    Full_person.baptism_place = _baptism_place;
    Full_person.baptism_src = _baptism_src;
    Full_person.death_date = _death_date;
    Full_person.death_place = _death_place;
    Full_person.death_src = _death_src;
    Full_person.death_type = _death_type;
    Full_person.burial_date = _burial_date;
    Full_person.burial_place = _burial_place;
    Full_person.burial_src = _burial_src;
    Full_person.occupation = _occupation;
    Full_person.psources = _psources;
    Full_person.titles = _titles;
    Full_person.related = _related;
    Full_person.rparents = _rparents;
    Full_person.visible_for_visitors = _visible_for_visitors;
    Full_person.parents = _parents;
    Full_person.families = _families;
    Full_person.baseprefix = _baseprefix;
  }

and parse_full_family x =
  let x = Piqirun.parse_record x in
  let _fsources, x = Piqirun.parse_optional_field 1 parse_string x in
  let _marriage_date, x = Piqirun.parse_optional_field 2 parse_string x in
  let _marriage_place, x = Piqirun.parse_optional_field 3 parse_string x in
  let _marriage_src, x = Piqirun.parse_optional_field 4 parse_string x in
  let _marriage_type, x = Piqirun.parse_required_field 5 parse_marriage_type x in
  let _divorce_type, x = Piqirun.parse_required_field 6 parse_divorce_type x in
  let _divorce_date, x = Piqirun.parse_optional_field 7 parse_string x in
  let _witnesses, x = Piqirun.parse_repeated_field 8 parse_internal_int32 x in
  let _father, x = Piqirun.parse_required_field 9 parse_protobuf_int32 x in
  let _mother, x = Piqirun.parse_required_field 10 parse_protobuf_int32 x in
  let _children, x = Piqirun.parse_repeated_field 11 parse_internal_int32 x in
  let _index, x = Piqirun.parse_required_field 12 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Full_family.fsources = _fsources;
    Full_family.marriage_date = _marriage_date;
    Full_family.marriage_place = _marriage_place;
    Full_family.marriage_src = _marriage_src;
    Full_family.marriage_type = _marriage_type;
    Full_family.divorce_type = _divorce_type;
    Full_family.divorce_date = _divorce_date;
    Full_family.witnesses = _witnesses;
    Full_family.father = _father;
    Full_family.mother = _mother;
    Full_family.children = _children;
    Full_family.index = _index;
  }

and parse_internal_int32 x =
  let x = Piqirun.parse_record x in
  let _value, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Internal_int32.value = _value;
  }

and parse_list_persons x =
  let x = Piqirun.parse_record x in
  let _list_persons, x = Piqirun.parse_repeated_field 1 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    List_persons.list_persons = _list_persons;
  }

and parse_list_full_persons x =
  let x = Piqirun.parse_record x in
  let _persons, x = Piqirun.parse_repeated_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    List_full_persons.persons = _persons;
  }

and parse_list_full_families x =
  let x = Piqirun.parse_record x in
  let _families, x = Piqirun.parse_repeated_field 1 parse_full_family x in
  Piqirun.check_unparsed_fields x;
  {
    List_full_families.families = _families;
  }

and parse_search_params x =
  let x = Piqirun.parse_record x in
  let _search_type, x = Piqirun.parse_required_field 1 parse_search_type x ~default:"\b\000" in
  let _lastname, x = Piqirun.parse_optional_field 2 parse_string x in
  let _firstname, x = Piqirun.parse_optional_field 3 parse_string x in
  let _only_sosa, x = Piqirun.parse_required_field 4 parse_bool x ~default:"\b\000" in
  let _only_recent, x = Piqirun.parse_required_field 5 parse_bool x ~default:"\b\000" in
  let _maiden_name, x = Piqirun.parse_required_field 6 parse_bool x ~default:"\b\000" in
  Piqirun.check_unparsed_fields x;
  {
    Search_params.search_type = _search_type;
    Search_params.lastname = _lastname;
    Search_params.firstname = _firstname;
    Search_params.only_sosa = _only_sosa;
    Search_params.only_recent = _only_recent;
    Search_params.maiden_name = _maiden_name;
  }

and parse_image x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_person x in
  let _img, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Image.person = _person;
    Image.img = _img;
  }

and parse_full_image x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _img, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Full_image.person = _person;
    Full_image.img = _img;
  }

and parse_list_images x =
  let x = Piqirun.parse_record x in
  let _list_images, x = Piqirun.parse_repeated_field 1 parse_image x in
  Piqirun.check_unparsed_fields x;
  {
    List_images.list_images = _list_images;
  }

and parse_list_full_images x =
  let x = Piqirun.parse_record x in
  let _images, x = Piqirun.parse_repeated_field 1 parse_full_image x in
  Piqirun.check_unparsed_fields x;
  {
    List_full_images.images = _images;
  }

and parse_pers_img x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _img, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Pers_img.person = _person;
    Pers_img.img = _img;
  }

and parse_list_pers_img x =
  let x = Piqirun.parse_record x in
  let _list_pers_img, x = Piqirun.parse_repeated_field 1 parse_pers_img x in
  Piqirun.check_unparsed_fields x;
  {
    List_pers_img.list_pers_img = _list_pers_img;
  }

and parse_index x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Index.index = _index;
  }

and parse_image_address x =
  let x = Piqirun.parse_record x in
  let _img, x = Piqirun.parse_required_field 1 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Image_address.img = _img;
  }

and parse_close_persons_params x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _nb_gen_asc, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _nb_gen_desc, x = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  let _spouse_ascend, x = Piqirun.parse_required_field 4 parse_bool x ~default:"\b\000" in
  let _only_recent, x = Piqirun.parse_required_field 5 parse_bool x ~default:"\b\000" in
  Piqirun.check_unparsed_fields x;
  {
    Close_persons_params.person = _person;
    Close_persons_params.nb_gen_asc = _nb_gen_asc;
    Close_persons_params.nb_gen_desc = _nb_gen_desc;
    Close_persons_params.spouse_ascend = _spouse_ascend;
    Close_persons_params.only_recent = _only_recent;
  }

and parse_person_relation x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_person x in
  let _relation, x = Piqirun.parse_required_field 2 parse_relation_type x in
  Piqirun.check_unparsed_fields x;
  {
    Person_relation.person = _person;
    Person_relation.relation = _relation;
  }

and parse_full_person_relation x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _relation, x = Piqirun.parse_required_field 2 parse_relation_type x in
  Piqirun.check_unparsed_fields x;
  {
    Full_person_relation.person = _person;
    Full_person_relation.relation = _relation;
  }

and parse_list_person_relation x =
  let x = Piqirun.parse_record x in
  let _person_relations, x = Piqirun.parse_repeated_field 1 parse_person_relation x in
  Piqirun.check_unparsed_fields x;
  {
    List_person_relation.person_relations = _person_relations;
  }

and parse_list_full_person_relation x =
  let x = Piqirun.parse_record x in
  let _person_relations, x = Piqirun.parse_repeated_field 1 parse_full_person_relation x in
  Piqirun.check_unparsed_fields x;
  {
    List_full_person_relation.person_relations = _person_relations;
  }

and parse_anniversary_params x =
  let x = Piqirun.parse_record x in
  let _month, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Anniversary_params.month = _month;
  }

and parse_graph_params x =
  let x = Piqirun.parse_record x in
  let _generation, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _person, x = Piqirun.parse_required_field 2 parse_reference_person x in
  Piqirun.check_unparsed_fields x;
  {
    Graph_params.generation = _generation;
    Graph_params.person = _person;
  }

and parse_graph_rel_params x =
  let x = Piqirun.parse_record x in
  let _person1, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _person2, x = Piqirun.parse_required_field 2 parse_reference_person x in
  Piqirun.check_unparsed_fields x;
  {
    Graph_rel_params.person1 = _person1;
    Graph_rel_params.person2 = _person2;
  }

and parse_cpl_rel_params x =
  let x = Piqirun.parse_record x in
  let _person1, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _person2, x = Piqirun.parse_required_field 2 parse_reference_person x in
  Piqirun.check_unparsed_fields x;
  {
    Cpl_rel_params.person1 = _person1;
    Cpl_rel_params.person2 = _person2;
  }

and parse_node x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _person, x = Piqirun.parse_required_field 2 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Node.id = _id;
    Node.person = _person;
  }

and parse_full_node x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _person, x = Piqirun.parse_required_field 2 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Full_node.id = _id;
    Full_node.person = _person;
  }

and parse_edge x =
  let x = Piqirun.parse_record x in
  let _from_node, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _to_node, x = Piqirun.parse_required_field 2 parse_protobuf_int64 x in
  Piqirun.check_unparsed_fields x;
  {
    Edge.from_node = _from_node;
    Edge.to_node = _to_node;
  }

and parse_graph x =
  let x = Piqirun.parse_record x in
  let _nodes, x = Piqirun.parse_repeated_field 1 parse_node x in
  let _edges, x = Piqirun.parse_repeated_field 2 parse_edge x in
  Piqirun.check_unparsed_fields x;
  {
    Graph.nodes = _nodes;
    Graph.edges = _edges;
  }

and parse_full_graph x =
  let x = Piqirun.parse_record x in
  let _nodes, x = Piqirun.parse_repeated_field 1 parse_full_node x in
  let _edges, x = Piqirun.parse_repeated_field 2 parse_edge x in
  let _families, x = Piqirun.parse_repeated_field 3 parse_full_family x in
  Piqirun.check_unparsed_fields x;
  {
    Full_graph.nodes = _nodes;
    Full_graph.edges = _edges;
    Full_graph.families = _families;
  }

and parse_all_persons_params x =
  let x = Piqirun.parse_record x in
  let _from, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _limit, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    All_persons_params.from = _from;
    All_persons_params.limit = _limit;
  }

and parse_all_families_params x =
  let x = Piqirun.parse_record x in
  let _from, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _limit, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    All_families_params.from = _from;
    All_families_params.limit = _limit;
  }

and parse_warning_already_defined x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_already_defined.person = _person;
  }

and parse_warning_own_ancestor x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_own_ancestor.person = _person;
  }

and parse_warning_bad_sex_of_married_person x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_bad_sex_of_married_person.person = _person;
  }

and parse_warning_birth_after_death x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_birth_after_death.person = _person;
  }

and parse_warning_incoherent_sex x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_incoherent_sex.person = _person;
  }

and parse_warning_changed_order_of_children x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_changed_order_of_children.father = _father;
    Warning_changed_order_of_children.mother = _mother;
  }

and parse_warning_changed_order_of_marriages x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_changed_order_of_marriages.person = _person;
  }

and parse_warning_children_not_in_order x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_children_not_in_order.father = _father;
    Warning_children_not_in_order.mother = _mother;
  }

and parse_warning_dead_too_early_to_be_father x =
  let x = Piqirun.parse_record x in
  let _son, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _father, x = Piqirun.parse_required_field 2 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_dead_too_early_to_be_father.son = _son;
    Warning_dead_too_early_to_be_father.father = _father;
  }

and parse_warning_incoherent_ancestor_date x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _ancestor, x = Piqirun.parse_required_field 2 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_incoherent_ancestor_date.person = _person;
    Warning_incoherent_ancestor_date.ancestor = _ancestor;
  }

and parse_warning_marriage_date_after_death x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_marriage_date_after_death.person = _person;
  }

and parse_warning_marriage_date_before_birth x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_marriage_date_before_birth.person = _person;
  }

and parse_warning_mother_dead_before_child_birth x =
  let x = Piqirun.parse_record x in
  let _mother, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _child, x = Piqirun.parse_required_field 2 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_mother_dead_before_child_birth.mother = _mother;
    Warning_mother_dead_before_child_birth.child = _child;
  }

and parse_warning_parent_born_after_child x =
  let x = Piqirun.parse_record x in
  let _parent, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _child, x = Piqirun.parse_required_field 2 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_parent_born_after_child.parent = _parent;
    Warning_parent_born_after_child.child = _child;
  }

and parse_warning_parent_too_young x =
  let x = Piqirun.parse_record x in
  let _parent, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _date, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_parent_too_young.parent = _parent;
    Warning_parent_too_young.date = _date;
  }

and parse_warning_title_dates_error x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_title_dates_error.person = _person;
  }

and parse_warning_undefined_sex x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_undefined_sex.person = _person;
  }

and parse_warning_young_for_marriage x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _date, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_young_for_marriage.person = _person;
    Warning_young_for_marriage.date = _date;
  }

and parse_warning_parent_too_old x =
  let x = Piqirun.parse_record x in
  let _parent, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _date, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_parent_too_old.parent = _parent;
    Warning_parent_too_old.date = _date;
  }

and parse_warning_close_children x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_full_person x in
  let _child1, x = Piqirun.parse_required_field 3 parse_full_person x in
  let _child2, x = Piqirun.parse_required_field 4 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_close_children.father = _father;
    Warning_close_children.mother = _mother;
    Warning_close_children.child1 = _child1;
    Warning_close_children.child2 = _child2;
  }

and parse_warning_big_age_between_spouses x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_full_person x in
  let _date, x = Piqirun.parse_required_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_big_age_between_spouses.father = _father;
    Warning_big_age_between_spouses.mother = _mother;
    Warning_big_age_between_spouses.date = _date;
  }

and parse_warning_dead_old x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _date, x = Piqirun.parse_required_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_dead_old.person = _person;
    Warning_dead_old.date = _date;
  }

and parse_warning_old_individual x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _date, x = Piqirun.parse_required_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_old_individual.person = _person;
    Warning_old_individual.date = _date;
  }

and parse_warning_witness_date_after_death x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_witness_date_after_death.person = _person;
  }

and parse_warning_witness_date_before_birth x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_witness_date_before_birth.person = _person;
  }

and parse_warning_changed_order_of_family_events x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_changed_order_of_family_events.father = _father;
    Warning_changed_order_of_family_events.mother = _mother;
  }

and parse_warning_changed_order_of_person_events x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_changed_order_of_person_events.person = _person;
  }

and parse_warning_fevent_order x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _event1, x = Piqirun.parse_required_field 2 parse_string x in
  let _event2, x = Piqirun.parse_required_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_fevent_order.person = _person;
    Warning_fevent_order.event1 = _event1;
    Warning_fevent_order.event2 = _event2;
  }

and parse_warning_fwitness_event_after_death x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _event, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_fwitness_event_after_death.person = _person;
    Warning_fwitness_event_after_death.event = _event;
  }

and parse_warning_fwitness_event_before_birth x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _event, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_fwitness_event_before_birth.person = _person;
    Warning_fwitness_event_before_birth.event = _event;
  }

and parse_warning_pevent_order x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _event1, x = Piqirun.parse_required_field 2 parse_string x in
  let _event2, x = Piqirun.parse_required_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_pevent_order.person = _person;
    Warning_pevent_order.event1 = _event1;
    Warning_pevent_order.event2 = _event2;
  }

and parse_warning_pwitness_event_after_death x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _event, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_pwitness_event_after_death.person = _person;
    Warning_pwitness_event_after_death.event = _event;
  }

and parse_warning_pwitness_event_before_birth x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_full_person x in
  let _event, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_pwitness_event_before_birth.person = _person;
    Warning_pwitness_event_before_birth.event = _event;
  }

and parse_base_warnings x =
  let x = Piqirun.parse_record x in
  let _warning_already_defined, x = Piqirun.parse_repeated_field 1 parse_warning_already_defined x in
  let _warning_own_ancestor, x = Piqirun.parse_repeated_field 2 parse_warning_own_ancestor x in
  let _warning_bad_sex_of_married_person, x = Piqirun.parse_repeated_field 3 parse_warning_bad_sex_of_married_person x in
  let _warning_birth_after_death, x = Piqirun.parse_repeated_field 4 parse_warning_birth_after_death x in
  let _warning_incoherent_sex, x = Piqirun.parse_repeated_field 5 parse_warning_incoherent_sex x in
  let _warning_changed_order_of_children, x = Piqirun.parse_repeated_field 6 parse_warning_changed_order_of_children x in
  let _warning_children_not_in_order, x = Piqirun.parse_repeated_field 7 parse_warning_children_not_in_order x in
  let _warning_dead_too_early_to_be_father, x = Piqirun.parse_repeated_field 8 parse_warning_dead_too_early_to_be_father x in
  let _warning_incoherent_ancestor_date, x = Piqirun.parse_repeated_field 9 parse_warning_incoherent_ancestor_date x in
  let _warning_marriage_date_after_death, x = Piqirun.parse_repeated_field 10 parse_warning_marriage_date_after_death x in
  let _warning_marriage_date_before_birth, x = Piqirun.parse_repeated_field 11 parse_warning_marriage_date_before_birth x in
  let _warning_mother_dead_before_child_birth, x = Piqirun.parse_repeated_field 12 parse_warning_mother_dead_before_child_birth x in
  let _warning_parent_born_after_child, x = Piqirun.parse_repeated_field 13 parse_warning_parent_born_after_child x in
  let _warning_parent_too_young, x = Piqirun.parse_repeated_field 14 parse_warning_parent_too_young x in
  let _warning_title_dates_error, x = Piqirun.parse_repeated_field 15 parse_warning_title_dates_error x in
  let _warning_undefined_sex, x = Piqirun.parse_repeated_field 16 parse_warning_undefined_sex x in
  let _warning_young_for_marriage, x = Piqirun.parse_repeated_field 17 parse_warning_young_for_marriage x in
  let _warning_close_children, x = Piqirun.parse_repeated_field 18 parse_warning_close_children x in
  let _warning_parent_too_old, x = Piqirun.parse_repeated_field 19 parse_warning_parent_too_old x in
  let _warning_changed_order_of_marriages, x = Piqirun.parse_repeated_field 20 parse_warning_changed_order_of_marriages x in
  let _warning_big_age_between_spouses, x = Piqirun.parse_repeated_field 21 parse_warning_big_age_between_spouses x in
  let _warning_dead_old, x = Piqirun.parse_repeated_field 22 parse_warning_dead_old x in
  let _warning_old_individual, x = Piqirun.parse_repeated_field 23 parse_warning_old_individual x in
  let _warning_witness_date_after_death, x = Piqirun.parse_repeated_field 24 parse_warning_witness_date_after_death x in
  let _warning_witness_date_before_birth, x = Piqirun.parse_repeated_field 25 parse_warning_witness_date_before_birth x in
  Piqirun.check_unparsed_fields x;
  {
    Base_warnings.warning_already_defined = _warning_already_defined;
    Base_warnings.warning_own_ancestor = _warning_own_ancestor;
    Base_warnings.warning_bad_sex_of_married_person = _warning_bad_sex_of_married_person;
    Base_warnings.warning_birth_after_death = _warning_birth_after_death;
    Base_warnings.warning_incoherent_sex = _warning_incoherent_sex;
    Base_warnings.warning_changed_order_of_children = _warning_changed_order_of_children;
    Base_warnings.warning_children_not_in_order = _warning_children_not_in_order;
    Base_warnings.warning_dead_too_early_to_be_father = _warning_dead_too_early_to_be_father;
    Base_warnings.warning_incoherent_ancestor_date = _warning_incoherent_ancestor_date;
    Base_warnings.warning_marriage_date_after_death = _warning_marriage_date_after_death;
    Base_warnings.warning_marriage_date_before_birth = _warning_marriage_date_before_birth;
    Base_warnings.warning_mother_dead_before_child_birth = _warning_mother_dead_before_child_birth;
    Base_warnings.warning_parent_born_after_child = _warning_parent_born_after_child;
    Base_warnings.warning_parent_too_young = _warning_parent_too_young;
    Base_warnings.warning_title_dates_error = _warning_title_dates_error;
    Base_warnings.warning_undefined_sex = _warning_undefined_sex;
    Base_warnings.warning_young_for_marriage = _warning_young_for_marriage;
    Base_warnings.warning_close_children = _warning_close_children;
    Base_warnings.warning_parent_too_old = _warning_parent_too_old;
    Base_warnings.warning_changed_order_of_marriages = _warning_changed_order_of_marriages;
    Base_warnings.warning_big_age_between_spouses = _warning_big_age_between_spouses;
    Base_warnings.warning_dead_old = _warning_dead_old;
    Base_warnings.warning_old_individual = _warning_old_individual;
    Base_warnings.warning_witness_date_after_death = _warning_witness_date_after_death;
    Base_warnings.warning_witness_date_before_birth = _warning_witness_date_before_birth;
  }

and parse_filter_date x =
  let x = Piqirun.parse_record x in
  let _day, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _month, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _year, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Filter_date.day = _day;
    Filter_date.month = _month;
    Filter_date.year = _year;
  }

and parse_filter_date_range x =
  let x = Piqirun.parse_record x in
  let _date_begin, x = Piqirun.parse_required_field 1 parse_filter_date x in
  let _date_end, x = Piqirun.parse_required_field 2 parse_filter_date x in
  let _only_exact, x = Piqirun.parse_required_field 3 parse_bool x ~default:"\b\000" in
  Piqirun.check_unparsed_fields x;
  {
    Filter_date_range.date_begin = _date_begin;
    Filter_date_range.date_end = _date_end;
    Filter_date_range.only_exact = _only_exact;
  }

and parse_filters x =
  let x = Piqirun.parse_record x in
  let _only_sosa, x = Piqirun.parse_required_field 1 parse_bool x ~default:"\b\000" in
  let _only_recent, x = Piqirun.parse_required_field 2 parse_bool x ~default:"\b\000" in
  let _sex, x = Piqirun.parse_optional_field 3 parse_sex x in
  let _nb_results, x = Piqirun.parse_required_field 4 parse_bool x ~default:"\b\000" in
  let _date_birth, x = Piqirun.parse_optional_field 5 parse_filter_date_range x in
  let _date_death, x = Piqirun.parse_optional_field 6 parse_filter_date_range x in
  Piqirun.check_unparsed_fields x;
  {
    Filters.only_sosa = _only_sosa;
    Filters.only_recent = _only_recent;
    Filters.sex = _sex;
    Filters.nb_results = _nb_results;
    Filters.date_birth = _date_birth;
    Filters.date_death = _date_death;
  }

and parse_modification_status x =
  let x = Piqirun.parse_record x in
  let _status, x = Piqirun.parse_required_field 1 parse_bool x in
  let _base_warnings, x = Piqirun.parse_required_field 2 parse_base_warnings x in
  let _index, x = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Modification_status.status = _status;
    Modification_status.base_warnings = _base_warnings;
    Modification_status.index = _index;
  }

and parse_notification_birthday_params x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _params, x = Piqirun.parse_required_field 2 parse_notif_birthday_params x in
  let _month, x = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  let _day, x = Piqirun.parse_optional_field 4 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Notification_birthday_params.person = _person;
    Notification_birthday_params.params = _params;
    Notification_birthday_params.month = _month;
    Notification_birthday_params.day = _day;
  }

and parse_notification_birthday x =
  let x = Piqirun.parse_record x in
  let _number, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _has_proprio_birthday, x = Piqirun.parse_required_field 2 parse_bool x in
  let _firstname1, x = Piqirun.parse_optional_field 3 parse_string x in
  let _firstname2, x = Piqirun.parse_optional_field 4 parse_string x in
  let _firstname3, x = Piqirun.parse_optional_field 5 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Notification_birthday.number = _number;
    Notification_birthday.has_proprio_birthday = _has_proprio_birthday;
    Notification_birthday.firstname1 = _firstname1;
    Notification_birthday.firstname2 = _firstname2;
    Notification_birthday.firstname3 = _firstname3;
  }

and parse_person_start x =
  let x = Piqirun.parse_record x in
  let _lastname, x = Piqirun.parse_required_field 1 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 2 parse_string x in
  let _sex, x = Piqirun.parse_required_field 3 parse_sex x in
  let _birth_date_day, x = Piqirun.parse_optional_field 4 parse_protobuf_int32 x in
  let _birth_date_month, x = Piqirun.parse_optional_field 5 parse_protobuf_int32 x in
  let _birth_date_year, x = Piqirun.parse_optional_field 6 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Person_start.lastname = _lastname;
    Person_start.firstname = _firstname;
    Person_start.sex = _sex;
    Person_start.birth_date_day = _birth_date_day;
    Person_start.birth_date_month = _birth_date_month;
    Person_start.birth_date_year = _birth_date_year;
  }

and parse_synchro_params x =
  let x = Piqirun.parse_record x in
  let _export_directory, x = Piqirun.parse_required_field 1 parse_string x in
  let _timestamp, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Synchro_params.export_directory = _export_directory;
    Synchro_params.timestamp = _timestamp;
  }

and parse_last_modifications x =
  let x = Piqirun.parse_record x in
  let _wizard, x = Piqirun.parse_optional_field 1 parse_string x in
  let _max_res, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _range, x = Piqirun.parse_optional_field 3 parse_filter_date_range x in
  Piqirun.check_unparsed_fields x;
  {
    Last_modifications.wizard = _wizard;
    Last_modifications.max_res = _max_res;
    Last_modifications.range = _range;
  }

and parse_last_visits x =
  let x = Piqirun.parse_record x in
  let _user, x = Piqirun.parse_required_field 1 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Last_visits.user = _user;
  }

and parse_correspondance_family x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _spouse, x = Piqirun.parse_required_field 2 parse_person x in
  let _children, x = Piqirun.parse_repeated_field 3 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Correspondance_family.index = _index;
    Correspondance_family.spouse = _spouse;
    Correspondance_family.children = _children;
  }

and parse_correspondance x =
  let x = Piqirun.parse_record x in
  let _base, x = Piqirun.parse_required_field 1 parse_string x in
  let _person, x = Piqirun.parse_required_field 2 parse_person x in
  let _father, x = Piqirun.parse_optional_field 3 parse_person x in
  let _mother, x = Piqirun.parse_optional_field 4 parse_person x in
  let _families, x = Piqirun.parse_repeated_field 5 parse_correspondance_family x in
  Piqirun.check_unparsed_fields x;
  {
    Correspondance.base = _base;
    Correspondance.person = _person;
    Correspondance.father = _father;
    Correspondance.mother = _mother;
    Correspondance.families = _families;
  }

and parse_correspondance_list x =
  let x = Piqirun.parse_record x in
  let _correspondances, x = Piqirun.parse_repeated_field 1 parse_correspondance x in
  Piqirun.check_unparsed_fields x;
  {
    Correspondance_list.correspondances = _correspondances;
  }

and parse_sex x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `male
    | 1l -> `female
    | 2l -> `unknown
    | x -> Piqirun.error_enum_const x
and packed_parse_sex x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `male
    | 1l -> `female
    | 2l -> `unknown
    | x -> Piqirun.error_enum_const x

and parse_death_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 4l -> `dont_know_if_dead
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x
and packed_parse_death_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 4l -> `dont_know_if_dead
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x

and parse_marriage_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
    | 4l -> `no_mention
    | 5l -> `no_sexes_check_married
    | x -> Piqirun.error_enum_const x
and packed_parse_marriage_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
    | 4l -> `no_mention
    | 5l -> `no_sexes_check_married
    | x -> Piqirun.error_enum_const x

and parse_divorce_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `not_divorced
    | 1l -> `divorced
    | 2l -> `separated
    | x -> Piqirun.error_enum_const x
and packed_parse_divorce_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `not_divorced
    | 1l -> `divorced
    | 2l -> `separated
    | x -> Piqirun.error_enum_const x

and parse_relation_parent_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `rpt_adoption
    | 1l -> `rpt_recognition
    | 2l -> `rpt_candidate_parent
    | 3l -> `rpt_god_parent
    | 4l -> `rpt_foster_parent
    | x -> Piqirun.error_enum_const x
and packed_parse_relation_parent_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `rpt_adoption
    | 1l -> `rpt_recognition
    | 2l -> `rpt_candidate_parent
    | 3l -> `rpt_god_parent
    | 4l -> `rpt_foster_parent
    | x -> Piqirun.error_enum_const x

and parse_title_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `title_main
    | 1l -> `title_name
    | 2l -> `title_none
    | x -> Piqirun.error_enum_const x
and packed_parse_title_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `title_main
    | 1l -> `title_name
    | 2l -> `title_none
    | x -> Piqirun.error_enum_const x

and parse_search_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `starting_with
    | 1l -> `approximative
    | 2l -> `lastname_or_firstname
    | x -> Piqirun.error_enum_const x
and packed_parse_search_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `starting_with
    | 1l -> `approximative
    | 2l -> `lastname_or_firstname
    | x -> Piqirun.error_enum_const x

and parse_relation_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `self
    | 1l -> `spouse
    | 2l -> `sibling
    | 3l -> `step_brother
    | 4l -> `parent
    | 5l -> `step_parent
    | 6l -> `grand_parent
    | 7l -> `uncle
    | 8l -> `uncle_spouse
    | 9l -> `cousin
    | 10l -> `cousin_spouse
    | 11l -> `child
    | 12l -> `step_child
    | 13l -> `grand_child
    | 14l -> `grand_child_spouse
    | 15l -> `great_grand_child
    | 16l -> `great_grand_child_spouse
    | 17l -> `child_cousin
    | 18l -> `child_cousin_spouse
    | 19l -> `grand_child_cousin
    | 20l -> `grand_child_cousin_spouse
    | 21l -> `great_grand_child_cousin
    | 22l -> `great_grand_child_cousin_spouse
    | 23l -> `nephew
    | 24l -> `nephew_spouse
    | 25l -> `nephew_spouse_spouse
    | 26l -> `grand_nephew
    | 27l -> `grand_nephew_spouse
    | 28l -> `grand_nephew_spouse_spouse
    | 29l -> `great_grand_nephew
    | 30l -> `great_grand_nephew_spouse
    | 31l -> `great_grand_nephew_spouse_spouse
    | 32l -> `adoptive_parent
    | 33l -> `adoptive_child
    | 34l -> `recognized_parent
    | 35l -> `recognized_child
    | 36l -> `candidate_parent
    | 37l -> `candidate_child
    | 38l -> `god_parent
    | 39l -> `god_child
    | 40l -> `foster_parent
    | 41l -> `foster_child
    | 42l -> `witness
    | 43l -> `no_relation
    | x -> Piqirun.error_enum_const x
and packed_parse_relation_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `self
    | 1l -> `spouse
    | 2l -> `sibling
    | 3l -> `step_brother
    | 4l -> `parent
    | 5l -> `step_parent
    | 6l -> `grand_parent
    | 7l -> `uncle
    | 8l -> `uncle_spouse
    | 9l -> `cousin
    | 10l -> `cousin_spouse
    | 11l -> `child
    | 12l -> `step_child
    | 13l -> `grand_child
    | 14l -> `grand_child_spouse
    | 15l -> `great_grand_child
    | 16l -> `great_grand_child_spouse
    | 17l -> `child_cousin
    | 18l -> `child_cousin_spouse
    | 19l -> `grand_child_cousin
    | 20l -> `grand_child_cousin_spouse
    | 21l -> `great_grand_child_cousin
    | 22l -> `great_grand_child_cousin_spouse
    | 23l -> `nephew
    | 24l -> `nephew_spouse
    | 25l -> `nephew_spouse_spouse
    | 26l -> `grand_nephew
    | 27l -> `grand_nephew_spouse
    | 28l -> `grand_nephew_spouse_spouse
    | 29l -> `great_grand_nephew
    | 30l -> `great_grand_nephew_spouse
    | 31l -> `great_grand_nephew_spouse_spouse
    | 32l -> `adoptive_parent
    | 33l -> `adoptive_child
    | 34l -> `recognized_parent
    | 35l -> `recognized_child
    | 36l -> `candidate_parent
    | 37l -> `candidate_child
    | 38l -> `god_parent
    | 39l -> `god_child
    | 40l -> `foster_parent
    | 41l -> `foster_child
    | 42l -> `witness
    | 43l -> `no_relation
    | x -> Piqirun.error_enum_const x

and parse_notif_birthday_params x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `close_person
    | 1l -> `descend_grand_parent
    | 2l -> `descend_great_grand_parent
    | x -> Piqirun.error_enum_const x
and packed_parse_notif_birthday_params x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `close_person
    | 1l -> `descend_grand_parent
    | 2l -> `descend_great_grand_parent
    | x -> Piqirun.error_enum_const x


let rec gen__int64 code x = Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = Piqirun.int64_to_packed_zigzag_varint x

and gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x

and gen__protobuf_int64 code x = Piqirun.int64_to_signed_varint code x
and packed_gen__protobuf_int64 x = Piqirun.int64_to_packed_signed_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__infos_base code x =
  let _nb_persons = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Infos_base.nb_persons in
  let _nb_families = Piqirun.gen_required_field 2 gen__protobuf_int64 x.Infos_base.nb_families in
  let _sosa = Piqirun.gen_optional_field 3 gen__reference_person x.Infos_base.sosa in
  let _last_modified_person = Piqirun.gen_optional_field 4 gen__protobuf_int64 x.Infos_base.last_modified_person in
  let _real_nb_persons = Piqirun.gen_optional_field 5 gen__protobuf_int64 x.Infos_base.real_nb_persons in
  Piqirun.gen_record code (_nb_persons :: _nb_families :: _sosa :: _last_modified_person :: _real_nb_persons :: [])

and gen__reference_person code x =
  let _n = Piqirun.gen_required_field 1 gen__string x.Reference_person.n in
  let _p = Piqirun.gen_required_field 2 gen__string x.Reference_person.p in
  let _oc = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Reference_person.oc in
  Piqirun.gen_record code (_n :: _p :: _oc :: [])

and gen__list_reference_persons code x =
  let _list_ref_persons = Piqirun.gen_repeated_field 1 gen__reference_person x.List_reference_persons.list_ref_persons in
  Piqirun.gen_record code (_list_ref_persons :: [])

and gen__relation_parent code x =
  let _father = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Relation_parent.father in
  let _mother = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Relation_parent.mother in
  let _source = Piqirun.gen_optional_field 3 gen__string x.Relation_parent.source in
  let _rpt_type = Piqirun.gen_required_field 4 gen__relation_parent_type x.Relation_parent.rpt_type in
  Piqirun.gen_record code (_father :: _mother :: _source :: _rpt_type :: [])

and gen__title code x =
  let _title_type = Piqirun.gen_required_field 1 gen__title_type x.Title.title_type in
  let _name = Piqirun.gen_optional_field 2 gen__string x.Title.name in
  let _title = Piqirun.gen_optional_field 3 gen__string x.Title.title in
  let _fief = Piqirun.gen_optional_field 4 gen__string x.Title.fief in
  let _date_begin = Piqirun.gen_optional_field 5 gen__string x.Title.date_begin in
  let _date_end = Piqirun.gen_optional_field 6 gen__string x.Title.date_end in
  let _nth = Piqirun.gen_optional_field 7 gen__protobuf_int32 x.Title.nth in
  Piqirun.gen_record code (_title_type :: _name :: _title :: _fief :: _date_begin :: _date_end :: _nth :: [])

and gen__spouse code x =
  let _sosa = Piqirun.gen_required_field 1 gen__string x.Spouse.sosa in
  let _n = Piqirun.gen_required_field 2 gen__string x.Spouse.n in
  let _p = Piqirun.gen_required_field 3 gen__string x.Spouse.p in
  let _oc = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Spouse.oc in
  let _sex = Piqirun.gen_required_field 5 gen__sex x.Spouse.sex in
  let _lastname = Piqirun.gen_required_field 6 gen__string x.Spouse.lastname in
  let _firstname = Piqirun.gen_required_field 7 gen__string x.Spouse.firstname in
  let _public_name = Piqirun.gen_optional_field 8 gen__string x.Spouse.public_name in
  let _image = Piqirun.gen_required_field 9 gen__string x.Spouse.image in
  let _birth_date = Piqirun.gen_required_field 10 gen__string x.Spouse.birth_date in
  let _birth_place = Piqirun.gen_required_field 11 gen__string x.Spouse.birth_place in
  let _baptism_date = Piqirun.gen_required_field 12 gen__string x.Spouse.baptism_date in
  let _baptism_place = Piqirun.gen_required_field 13 gen__string x.Spouse.baptism_place in
  let _death_date = Piqirun.gen_required_field 14 gen__string x.Spouse.death_date in
  let _death_place = Piqirun.gen_required_field 15 gen__string x.Spouse.death_place in
  let _death_type = Piqirun.gen_required_field 16 gen__death_type x.Spouse.death_type in
  let _burial_date = Piqirun.gen_required_field 17 gen__string x.Spouse.burial_date in
  let _burial_place = Piqirun.gen_required_field 18 gen__string x.Spouse.burial_place in
  let _marriage_date = Piqirun.gen_required_field 19 gen__string x.Spouse.marriage_date in
  let _marriage_place = Piqirun.gen_required_field 20 gen__string x.Spouse.marriage_place in
  let _divorce_type = Piqirun.gen_required_field 21 gen__divorce_type x.Spouse.divorce_type in
  let _visible_for_visitors = Piqirun.gen_required_field 22 gen__bool x.Spouse.visible_for_visitors in
  Piqirun.gen_record code (_sosa :: _n :: _p :: _oc :: _sex :: _lastname :: _firstname :: _public_name :: _image :: _birth_date :: _birth_place :: _baptism_date :: _baptism_place :: _death_date :: _death_place :: _death_type :: _burial_date :: _burial_place :: _marriage_date :: _marriage_place :: _divorce_type :: _visible_for_visitors :: [])

and gen__person code x =
  let _sosa = Piqirun.gen_required_field 1 gen__string x.Person.sosa in
  let _n = Piqirun.gen_required_field 2 gen__string x.Person.n in
  let _p = Piqirun.gen_required_field 3 gen__string x.Person.p in
  let _oc = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Person.oc in
  let _sex = Piqirun.gen_required_field 5 gen__sex x.Person.sex in
  let _lastname = Piqirun.gen_required_field 6 gen__string x.Person.lastname in
  let _firstname = Piqirun.gen_required_field 7 gen__string x.Person.firstname in
  let _public_name = Piqirun.gen_optional_field 8 gen__string x.Person.public_name in
  let _image = Piqirun.gen_required_field 9 gen__string x.Person.image in
  let _birth_date = Piqirun.gen_required_field 10 gen__string x.Person.birth_date in
  let _birth_place = Piqirun.gen_required_field 11 gen__string x.Person.birth_place in
  let _baptism_date = Piqirun.gen_required_field 12 gen__string x.Person.baptism_date in
  let _baptism_place = Piqirun.gen_required_field 13 gen__string x.Person.baptism_place in
  let _death_date = Piqirun.gen_required_field 14 gen__string x.Person.death_date in
  let _death_place = Piqirun.gen_required_field 15 gen__string x.Person.death_place in
  let _death_type = Piqirun.gen_required_field 16 gen__death_type x.Person.death_type in
  let _burial_date = Piqirun.gen_required_field 17 gen__string x.Person.burial_date in
  let _burial_place = Piqirun.gen_required_field 18 gen__string x.Person.burial_place in
  let _spouses = Piqirun.gen_repeated_field 19 gen__spouse x.Person.spouses in
  let _ascend = Piqirun.gen_required_field 20 gen__bool x.Person.ascend in
  let _descend = Piqirun.gen_required_field 21 gen__bool x.Person.descend in
  let _visible_for_visitors = Piqirun.gen_required_field 22 gen__bool x.Person.visible_for_visitors in
  let _baseprefix = Piqirun.gen_required_field 23 gen__string x.Person.baseprefix in
  Piqirun.gen_record code (_sosa :: _n :: _p :: _oc :: _sex :: _lastname :: _firstname :: _public_name :: _image :: _birth_date :: _birth_place :: _baptism_date :: _baptism_place :: _death_date :: _death_place :: _death_type :: _burial_date :: _burial_place :: _spouses :: _ascend :: _descend :: _visible_for_visitors :: _baseprefix :: [])

and gen__full_person code x =
  let _sosa = Piqirun.gen_required_field 1 gen__string x.Full_person.sosa in
  let _n = Piqirun.gen_required_field 2 gen__string x.Full_person.n in
  let _p = Piqirun.gen_required_field 3 gen__string x.Full_person.p in
  let _oc = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Full_person.oc in
  let _index = Piqirun.gen_required_field 5 gen__protobuf_int32 x.Full_person.index in
  let _sex = Piqirun.gen_required_field 6 gen__sex x.Full_person.sex in
  let _lastname = Piqirun.gen_required_field 7 gen__string x.Full_person.lastname in
  let _firstname = Piqirun.gen_required_field 8 gen__string x.Full_person.firstname in
  let _public_name = Piqirun.gen_optional_field 9 gen__string x.Full_person.public_name in
  let _aliases = Piqirun.gen_repeated_field 10 gen__string x.Full_person.aliases in
  let _qualifiers = Piqirun.gen_repeated_field 11 gen__string x.Full_person.qualifiers in
  let _firstname_aliases = Piqirun.gen_repeated_field 12 gen__string x.Full_person.firstname_aliases in
  let _surname_aliases = Piqirun.gen_repeated_field 13 gen__string x.Full_person.surname_aliases in
  let _image = Piqirun.gen_optional_field 15 gen__string x.Full_person.image in
  let _birth_date = Piqirun.gen_optional_field 16 gen__string x.Full_person.birth_date in
  let _birth_place = Piqirun.gen_optional_field 17 gen__string x.Full_person.birth_place in
  let _birth_src = Piqirun.gen_optional_field 18 gen__string x.Full_person.birth_src in
  let _baptism_date = Piqirun.gen_optional_field 19 gen__string x.Full_person.baptism_date in
  let _baptism_place = Piqirun.gen_optional_field 20 gen__string x.Full_person.baptism_place in
  let _baptism_src = Piqirun.gen_optional_field 21 gen__string x.Full_person.baptism_src in
  let _death_date = Piqirun.gen_optional_field 22 gen__string x.Full_person.death_date in
  let _death_place = Piqirun.gen_optional_field 23 gen__string x.Full_person.death_place in
  let _death_src = Piqirun.gen_optional_field 24 gen__string x.Full_person.death_src in
  let _death_type = Piqirun.gen_required_field 25 gen__death_type x.Full_person.death_type in
  let _burial_date = Piqirun.gen_optional_field 26 gen__string x.Full_person.burial_date in
  let _burial_place = Piqirun.gen_optional_field 27 gen__string x.Full_person.burial_place in
  let _burial_src = Piqirun.gen_optional_field 28 gen__string x.Full_person.burial_src in
  let _occupation = Piqirun.gen_optional_field 30 gen__string x.Full_person.occupation in
  let _psources = Piqirun.gen_optional_field 31 gen__string x.Full_person.psources in
  let _titles = Piqirun.gen_repeated_field 32 gen__title x.Full_person.titles in
  let _related = Piqirun.gen_repeated_field 33 gen__internal_int32 x.Full_person.related in
  let _rparents = Piqirun.gen_repeated_field 34 gen__relation_parent x.Full_person.rparents in
  let _visible_for_visitors = Piqirun.gen_required_field 35 gen__bool x.Full_person.visible_for_visitors in
  let _parents = Piqirun.gen_optional_field 36 gen__protobuf_int32 x.Full_person.parents in
  let _families = Piqirun.gen_repeated_field 37 gen__internal_int32 x.Full_person.families in
  let _baseprefix = Piqirun.gen_required_field 38 gen__string x.Full_person.baseprefix in
  Piqirun.gen_record code (_sosa :: _n :: _p :: _oc :: _index :: _sex :: _lastname :: _firstname :: _public_name :: _aliases :: _qualifiers :: _firstname_aliases :: _surname_aliases :: _image :: _birth_date :: _birth_place :: _birth_src :: _baptism_date :: _baptism_place :: _baptism_src :: _death_date :: _death_place :: _death_src :: _death_type :: _burial_date :: _burial_place :: _burial_src :: _occupation :: _psources :: _titles :: _related :: _rparents :: _visible_for_visitors :: _parents :: _families :: _baseprefix :: [])

and gen__full_family code x =
  let _fsources = Piqirun.gen_optional_field 1 gen__string x.Full_family.fsources in
  let _marriage_date = Piqirun.gen_optional_field 2 gen__string x.Full_family.marriage_date in
  let _marriage_place = Piqirun.gen_optional_field 3 gen__string x.Full_family.marriage_place in
  let _marriage_src = Piqirun.gen_optional_field 4 gen__string x.Full_family.marriage_src in
  let _marriage_type = Piqirun.gen_required_field 5 gen__marriage_type x.Full_family.marriage_type in
  let _divorce_type = Piqirun.gen_required_field 6 gen__divorce_type x.Full_family.divorce_type in
  let _divorce_date = Piqirun.gen_optional_field 7 gen__string x.Full_family.divorce_date in
  let _witnesses = Piqirun.gen_repeated_field 8 gen__internal_int32 x.Full_family.witnesses in
  let _father = Piqirun.gen_required_field 9 gen__protobuf_int32 x.Full_family.father in
  let _mother = Piqirun.gen_required_field 10 gen__protobuf_int32 x.Full_family.mother in
  let _children = Piqirun.gen_repeated_field 11 gen__internal_int32 x.Full_family.children in
  let _index = Piqirun.gen_required_field 12 gen__protobuf_int32 x.Full_family.index in
  Piqirun.gen_record code (_fsources :: _marriage_date :: _marriage_place :: _marriage_src :: _marriage_type :: _divorce_type :: _divorce_date :: _witnesses :: _father :: _mother :: _children :: _index :: [])

and gen__internal_int32 code x =
  let _value = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Internal_int32.value in
  Piqirun.gen_record code (_value :: [])

and gen__list_persons code x =
  let _list_persons = Piqirun.gen_repeated_field 1 gen__person x.List_persons.list_persons in
  Piqirun.gen_record code (_list_persons :: [])

and gen__list_full_persons code x =
  let _persons = Piqirun.gen_repeated_field 1 gen__full_person x.List_full_persons.persons in
  Piqirun.gen_record code (_persons :: [])

and gen__list_full_families code x =
  let _families = Piqirun.gen_repeated_field 1 gen__full_family x.List_full_families.families in
  Piqirun.gen_record code (_families :: [])

and gen__search_params code x =
  let _search_type = Piqirun.gen_required_field 1 gen__search_type x.Search_params.search_type in
  let _lastname = Piqirun.gen_optional_field 2 gen__string x.Search_params.lastname in
  let _firstname = Piqirun.gen_optional_field 3 gen__string x.Search_params.firstname in
  let _only_sosa = Piqirun.gen_required_field 4 gen__bool x.Search_params.only_sosa in
  let _only_recent = Piqirun.gen_required_field 5 gen__bool x.Search_params.only_recent in
  let _maiden_name = Piqirun.gen_required_field 6 gen__bool x.Search_params.maiden_name in
  Piqirun.gen_record code (_search_type :: _lastname :: _firstname :: _only_sosa :: _only_recent :: _maiden_name :: [])

and gen__image code x =
  let _person = Piqirun.gen_required_field 1 gen__person x.Image.person in
  let _img = Piqirun.gen_required_field 2 gen__string x.Image.img in
  Piqirun.gen_record code (_person :: _img :: [])

and gen__full_image code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Full_image.person in
  let _img = Piqirun.gen_required_field 2 gen__string x.Full_image.img in
  Piqirun.gen_record code (_person :: _img :: [])

and gen__list_images code x =
  let _list_images = Piqirun.gen_repeated_field 1 gen__image x.List_images.list_images in
  Piqirun.gen_record code (_list_images :: [])

and gen__list_full_images code x =
  let _images = Piqirun.gen_repeated_field 1 gen__full_image x.List_full_images.images in
  Piqirun.gen_record code (_images :: [])

and gen__pers_img code x =
  let _person = Piqirun.gen_required_field 1 gen__reference_person x.Pers_img.person in
  let _img = Piqirun.gen_required_field 2 gen__string x.Pers_img.img in
  Piqirun.gen_record code (_person :: _img :: [])

and gen__list_pers_img code x =
  let _list_pers_img = Piqirun.gen_repeated_field 1 gen__pers_img x.List_pers_img.list_pers_img in
  Piqirun.gen_record code (_list_pers_img :: [])

and gen__index code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Index.index in
  Piqirun.gen_record code (_index :: [])

and gen__image_address code x =
  let _img = Piqirun.gen_required_field 1 gen__string x.Image_address.img in
  Piqirun.gen_record code (_img :: [])

and gen__close_persons_params code x =
  let _person = Piqirun.gen_required_field 1 gen__reference_person x.Close_persons_params.person in
  let _nb_gen_asc = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Close_persons_params.nb_gen_asc in
  let _nb_gen_desc = Piqirun.gen_optional_field 3 gen__protobuf_int32 x.Close_persons_params.nb_gen_desc in
  let _spouse_ascend = Piqirun.gen_required_field 4 gen__bool x.Close_persons_params.spouse_ascend in
  let _only_recent = Piqirun.gen_required_field 5 gen__bool x.Close_persons_params.only_recent in
  Piqirun.gen_record code (_person :: _nb_gen_asc :: _nb_gen_desc :: _spouse_ascend :: _only_recent :: [])

and gen__person_relation code x =
  let _person = Piqirun.gen_required_field 1 gen__person x.Person_relation.person in
  let _relation = Piqirun.gen_required_field 2 gen__relation_type x.Person_relation.relation in
  Piqirun.gen_record code (_person :: _relation :: [])

and gen__full_person_relation code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Full_person_relation.person in
  let _relation = Piqirun.gen_required_field 2 gen__relation_type x.Full_person_relation.relation in
  Piqirun.gen_record code (_person :: _relation :: [])

and gen__list_person_relation code x =
  let _person_relations = Piqirun.gen_repeated_field 1 gen__person_relation x.List_person_relation.person_relations in
  Piqirun.gen_record code (_person_relations :: [])

and gen__list_full_person_relation code x =
  let _person_relations = Piqirun.gen_repeated_field 1 gen__full_person_relation x.List_full_person_relation.person_relations in
  Piqirun.gen_record code (_person_relations :: [])

and gen__anniversary_params code x =
  let _month = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Anniversary_params.month in
  Piqirun.gen_record code (_month :: [])

and gen__graph_params code x =
  let _generation = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Graph_params.generation in
  let _person = Piqirun.gen_required_field 2 gen__reference_person x.Graph_params.person in
  Piqirun.gen_record code (_generation :: _person :: [])

and gen__graph_rel_params code x =
  let _person1 = Piqirun.gen_required_field 1 gen__reference_person x.Graph_rel_params.person1 in
  let _person2 = Piqirun.gen_required_field 2 gen__reference_person x.Graph_rel_params.person2 in
  Piqirun.gen_record code (_person1 :: _person2 :: [])

and gen__cpl_rel_params code x =
  let _person1 = Piqirun.gen_required_field 1 gen__reference_person x.Cpl_rel_params.person1 in
  let _person2 = Piqirun.gen_required_field 2 gen__reference_person x.Cpl_rel_params.person2 in
  Piqirun.gen_record code (_person1 :: _person2 :: [])

and gen__node code x =
  let _id = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Node.id in
  let _person = Piqirun.gen_required_field 2 gen__person x.Node.person in
  Piqirun.gen_record code (_id :: _person :: [])

and gen__full_node code x =
  let _id = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Full_node.id in
  let _person = Piqirun.gen_required_field 2 gen__full_person x.Full_node.person in
  Piqirun.gen_record code (_id :: _person :: [])

and gen__edge code x =
  let _from_node = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Edge.from_node in
  let _to_node = Piqirun.gen_required_field 2 gen__protobuf_int64 x.Edge.to_node in
  Piqirun.gen_record code (_from_node :: _to_node :: [])

and gen__graph code x =
  let _nodes = Piqirun.gen_repeated_field 1 gen__node x.Graph.nodes in
  let _edges = Piqirun.gen_repeated_field 2 gen__edge x.Graph.edges in
  Piqirun.gen_record code (_nodes :: _edges :: [])

and gen__full_graph code x =
  let _nodes = Piqirun.gen_repeated_field 1 gen__full_node x.Full_graph.nodes in
  let _edges = Piqirun.gen_repeated_field 2 gen__edge x.Full_graph.edges in
  let _families = Piqirun.gen_repeated_field 3 gen__full_family x.Full_graph.families in
  Piqirun.gen_record code (_nodes :: _edges :: _families :: [])

and gen__all_persons_params code x =
  let _from = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.All_persons_params.from in
  let _limit = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.All_persons_params.limit in
  Piqirun.gen_record code (_from :: _limit :: [])

and gen__all_families_params code x =
  let _from = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.All_families_params.from in
  let _limit = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.All_families_params.limit in
  Piqirun.gen_record code (_from :: _limit :: [])

and gen__warning_already_defined code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_already_defined.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_own_ancestor code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_own_ancestor.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_bad_sex_of_married_person code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_bad_sex_of_married_person.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_birth_after_death code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_birth_after_death.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_incoherent_sex code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_incoherent_sex.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_changed_order_of_children code x =
  let _father = Piqirun.gen_required_field 1 gen__full_person x.Warning_changed_order_of_children.father in
  let _mother = Piqirun.gen_required_field 2 gen__full_person x.Warning_changed_order_of_children.mother in
  Piqirun.gen_record code (_father :: _mother :: [])

and gen__warning_changed_order_of_marriages code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_changed_order_of_marriages.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_children_not_in_order code x =
  let _father = Piqirun.gen_required_field 1 gen__full_person x.Warning_children_not_in_order.father in
  let _mother = Piqirun.gen_required_field 2 gen__full_person x.Warning_children_not_in_order.mother in
  Piqirun.gen_record code (_father :: _mother :: [])

and gen__warning_dead_too_early_to_be_father code x =
  let _son = Piqirun.gen_required_field 1 gen__full_person x.Warning_dead_too_early_to_be_father.son in
  let _father = Piqirun.gen_required_field 2 gen__full_person x.Warning_dead_too_early_to_be_father.father in
  Piqirun.gen_record code (_son :: _father :: [])

and gen__warning_incoherent_ancestor_date code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_incoherent_ancestor_date.person in
  let _ancestor = Piqirun.gen_required_field 2 gen__full_person x.Warning_incoherent_ancestor_date.ancestor in
  Piqirun.gen_record code (_person :: _ancestor :: [])

and gen__warning_marriage_date_after_death code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_marriage_date_after_death.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_marriage_date_before_birth code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_marriage_date_before_birth.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_mother_dead_before_child_birth code x =
  let _mother = Piqirun.gen_required_field 1 gen__full_person x.Warning_mother_dead_before_child_birth.mother in
  let _child = Piqirun.gen_required_field 2 gen__full_person x.Warning_mother_dead_before_child_birth.child in
  Piqirun.gen_record code (_mother :: _child :: [])

and gen__warning_parent_born_after_child code x =
  let _parent = Piqirun.gen_required_field 1 gen__full_person x.Warning_parent_born_after_child.parent in
  let _child = Piqirun.gen_required_field 2 gen__full_person x.Warning_parent_born_after_child.child in
  Piqirun.gen_record code (_parent :: _child :: [])

and gen__warning_parent_too_young code x =
  let _parent = Piqirun.gen_required_field 1 gen__full_person x.Warning_parent_too_young.parent in
  let _date = Piqirun.gen_required_field 2 gen__string x.Warning_parent_too_young.date in
  Piqirun.gen_record code (_parent :: _date :: [])

and gen__warning_title_dates_error code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_title_dates_error.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_undefined_sex code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_undefined_sex.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_young_for_marriage code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_young_for_marriage.person in
  let _date = Piqirun.gen_required_field 2 gen__string x.Warning_young_for_marriage.date in
  Piqirun.gen_record code (_person :: _date :: [])

and gen__warning_parent_too_old code x =
  let _parent = Piqirun.gen_required_field 1 gen__full_person x.Warning_parent_too_old.parent in
  let _date = Piqirun.gen_required_field 2 gen__string x.Warning_parent_too_old.date in
  Piqirun.gen_record code (_parent :: _date :: [])

and gen__warning_close_children code x =
  let _father = Piqirun.gen_required_field 1 gen__full_person x.Warning_close_children.father in
  let _mother = Piqirun.gen_required_field 2 gen__full_person x.Warning_close_children.mother in
  let _child1 = Piqirun.gen_required_field 3 gen__full_person x.Warning_close_children.child1 in
  let _child2 = Piqirun.gen_required_field 4 gen__full_person x.Warning_close_children.child2 in
  Piqirun.gen_record code (_father :: _mother :: _child1 :: _child2 :: [])

and gen__warning_big_age_between_spouses code x =
  let _father = Piqirun.gen_required_field 1 gen__full_person x.Warning_big_age_between_spouses.father in
  let _mother = Piqirun.gen_required_field 2 gen__full_person x.Warning_big_age_between_spouses.mother in
  let _date = Piqirun.gen_required_field 3 gen__string x.Warning_big_age_between_spouses.date in
  Piqirun.gen_record code (_father :: _mother :: _date :: [])

and gen__warning_dead_old code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_dead_old.person in
  let _date = Piqirun.gen_required_field 3 gen__string x.Warning_dead_old.date in
  Piqirun.gen_record code (_person :: _date :: [])

and gen__warning_old_individual code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_old_individual.person in
  let _date = Piqirun.gen_required_field 3 gen__string x.Warning_old_individual.date in
  Piqirun.gen_record code (_person :: _date :: [])

and gen__warning_witness_date_after_death code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_witness_date_after_death.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_witness_date_before_birth code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_witness_date_before_birth.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_changed_order_of_family_events code x =
  let _father = Piqirun.gen_required_field 1 gen__full_person x.Warning_changed_order_of_family_events.father in
  let _mother = Piqirun.gen_required_field 2 gen__full_person x.Warning_changed_order_of_family_events.mother in
  Piqirun.gen_record code (_father :: _mother :: [])

and gen__warning_changed_order_of_person_events code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_changed_order_of_person_events.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_fevent_order code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_fevent_order.person in
  let _event1 = Piqirun.gen_required_field 2 gen__string x.Warning_fevent_order.event1 in
  let _event2 = Piqirun.gen_required_field 3 gen__string x.Warning_fevent_order.event2 in
  Piqirun.gen_record code (_person :: _event1 :: _event2 :: [])

and gen__warning_fwitness_event_after_death code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_fwitness_event_after_death.person in
  let _event = Piqirun.gen_required_field 2 gen__string x.Warning_fwitness_event_after_death.event in
  Piqirun.gen_record code (_person :: _event :: [])

and gen__warning_fwitness_event_before_birth code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_fwitness_event_before_birth.person in
  let _event = Piqirun.gen_required_field 2 gen__string x.Warning_fwitness_event_before_birth.event in
  Piqirun.gen_record code (_person :: _event :: [])

and gen__warning_pevent_order code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_pevent_order.person in
  let _event1 = Piqirun.gen_required_field 2 gen__string x.Warning_pevent_order.event1 in
  let _event2 = Piqirun.gen_required_field 3 gen__string x.Warning_pevent_order.event2 in
  Piqirun.gen_record code (_person :: _event1 :: _event2 :: [])

and gen__warning_pwitness_event_after_death code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_pwitness_event_after_death.person in
  let _event = Piqirun.gen_required_field 2 gen__string x.Warning_pwitness_event_after_death.event in
  Piqirun.gen_record code (_person :: _event :: [])

and gen__warning_pwitness_event_before_birth code x =
  let _person = Piqirun.gen_required_field 1 gen__full_person x.Warning_pwitness_event_before_birth.person in
  let _event = Piqirun.gen_required_field 2 gen__string x.Warning_pwitness_event_before_birth.event in
  Piqirun.gen_record code (_person :: _event :: [])

and gen__base_warnings code x =
  let _warning_already_defined = Piqirun.gen_repeated_field 1 gen__warning_already_defined x.Base_warnings.warning_already_defined in
  let _warning_own_ancestor = Piqirun.gen_repeated_field 2 gen__warning_own_ancestor x.Base_warnings.warning_own_ancestor in
  let _warning_bad_sex_of_married_person = Piqirun.gen_repeated_field 3 gen__warning_bad_sex_of_married_person x.Base_warnings.warning_bad_sex_of_married_person in
  let _warning_birth_after_death = Piqirun.gen_repeated_field 4 gen__warning_birth_after_death x.Base_warnings.warning_birth_after_death in
  let _warning_incoherent_sex = Piqirun.gen_repeated_field 5 gen__warning_incoherent_sex x.Base_warnings.warning_incoherent_sex in
  let _warning_changed_order_of_children = Piqirun.gen_repeated_field 6 gen__warning_changed_order_of_children x.Base_warnings.warning_changed_order_of_children in
  let _warning_children_not_in_order = Piqirun.gen_repeated_field 7 gen__warning_children_not_in_order x.Base_warnings.warning_children_not_in_order in
  let _warning_dead_too_early_to_be_father = Piqirun.gen_repeated_field 8 gen__warning_dead_too_early_to_be_father x.Base_warnings.warning_dead_too_early_to_be_father in
  let _warning_incoherent_ancestor_date = Piqirun.gen_repeated_field 9 gen__warning_incoherent_ancestor_date x.Base_warnings.warning_incoherent_ancestor_date in
  let _warning_marriage_date_after_death = Piqirun.gen_repeated_field 10 gen__warning_marriage_date_after_death x.Base_warnings.warning_marriage_date_after_death in
  let _warning_marriage_date_before_birth = Piqirun.gen_repeated_field 11 gen__warning_marriage_date_before_birth x.Base_warnings.warning_marriage_date_before_birth in
  let _warning_mother_dead_before_child_birth = Piqirun.gen_repeated_field 12 gen__warning_mother_dead_before_child_birth x.Base_warnings.warning_mother_dead_before_child_birth in
  let _warning_parent_born_after_child = Piqirun.gen_repeated_field 13 gen__warning_parent_born_after_child x.Base_warnings.warning_parent_born_after_child in
  let _warning_parent_too_young = Piqirun.gen_repeated_field 14 gen__warning_parent_too_young x.Base_warnings.warning_parent_too_young in
  let _warning_title_dates_error = Piqirun.gen_repeated_field 15 gen__warning_title_dates_error x.Base_warnings.warning_title_dates_error in
  let _warning_undefined_sex = Piqirun.gen_repeated_field 16 gen__warning_undefined_sex x.Base_warnings.warning_undefined_sex in
  let _warning_young_for_marriage = Piqirun.gen_repeated_field 17 gen__warning_young_for_marriage x.Base_warnings.warning_young_for_marriage in
  let _warning_close_children = Piqirun.gen_repeated_field 18 gen__warning_close_children x.Base_warnings.warning_close_children in
  let _warning_parent_too_old = Piqirun.gen_repeated_field 19 gen__warning_parent_too_old x.Base_warnings.warning_parent_too_old in
  let _warning_changed_order_of_marriages = Piqirun.gen_repeated_field 20 gen__warning_changed_order_of_marriages x.Base_warnings.warning_changed_order_of_marriages in
  let _warning_big_age_between_spouses = Piqirun.gen_repeated_field 21 gen__warning_big_age_between_spouses x.Base_warnings.warning_big_age_between_spouses in
  let _warning_dead_old = Piqirun.gen_repeated_field 22 gen__warning_dead_old x.Base_warnings.warning_dead_old in
  let _warning_old_individual = Piqirun.gen_repeated_field 23 gen__warning_old_individual x.Base_warnings.warning_old_individual in
  let _warning_witness_date_after_death = Piqirun.gen_repeated_field 24 gen__warning_witness_date_after_death x.Base_warnings.warning_witness_date_after_death in
  let _warning_witness_date_before_birth = Piqirun.gen_repeated_field 25 gen__warning_witness_date_before_birth x.Base_warnings.warning_witness_date_before_birth in
  Piqirun.gen_record code (_warning_already_defined :: _warning_own_ancestor :: _warning_bad_sex_of_married_person :: _warning_birth_after_death :: _warning_incoherent_sex :: _warning_changed_order_of_children :: _warning_children_not_in_order :: _warning_dead_too_early_to_be_father :: _warning_incoherent_ancestor_date :: _warning_marriage_date_after_death :: _warning_marriage_date_before_birth :: _warning_mother_dead_before_child_birth :: _warning_parent_born_after_child :: _warning_parent_too_young :: _warning_title_dates_error :: _warning_undefined_sex :: _warning_young_for_marriage :: _warning_close_children :: _warning_parent_too_old :: _warning_changed_order_of_marriages :: _warning_big_age_between_spouses :: _warning_dead_old :: _warning_old_individual :: _warning_witness_date_after_death :: _warning_witness_date_before_birth :: [])

and gen__filter_date code x =
  let _day = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Filter_date.day in
  let _month = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Filter_date.month in
  let _year = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Filter_date.year in
  Piqirun.gen_record code (_day :: _month :: _year :: [])

and gen__filter_date_range code x =
  let _date_begin = Piqirun.gen_required_field 1 gen__filter_date x.Filter_date_range.date_begin in
  let _date_end = Piqirun.gen_required_field 2 gen__filter_date x.Filter_date_range.date_end in
  let _only_exact = Piqirun.gen_required_field 3 gen__bool x.Filter_date_range.only_exact in
  Piqirun.gen_record code (_date_begin :: _date_end :: _only_exact :: [])

and gen__filters code x =
  let _only_sosa = Piqirun.gen_required_field 1 gen__bool x.Filters.only_sosa in
  let _only_recent = Piqirun.gen_required_field 2 gen__bool x.Filters.only_recent in
  let _sex = Piqirun.gen_optional_field 3 gen__sex x.Filters.sex in
  let _nb_results = Piqirun.gen_required_field 4 gen__bool x.Filters.nb_results in
  let _date_birth = Piqirun.gen_optional_field 5 gen__filter_date_range x.Filters.date_birth in
  let _date_death = Piqirun.gen_optional_field 6 gen__filter_date_range x.Filters.date_death in
  Piqirun.gen_record code (_only_sosa :: _only_recent :: _sex :: _nb_results :: _date_birth :: _date_death :: [])

and gen__modification_status code x =
  let _status = Piqirun.gen_required_field 1 gen__bool x.Modification_status.status in
  let _base_warnings = Piqirun.gen_required_field 2 gen__base_warnings x.Modification_status.base_warnings in
  let _index = Piqirun.gen_optional_field 3 gen__protobuf_int32 x.Modification_status.index in
  Piqirun.gen_record code (_status :: _base_warnings :: _index :: [])

and gen__notification_birthday_params code x =
  let _person = Piqirun.gen_required_field 1 gen__reference_person x.Notification_birthday_params.person in
  let _params = Piqirun.gen_required_field 2 gen__notif_birthday_params x.Notification_birthday_params.params in
  let _month = Piqirun.gen_optional_field 3 gen__protobuf_int32 x.Notification_birthday_params.month in
  let _day = Piqirun.gen_optional_field 4 gen__protobuf_int32 x.Notification_birthday_params.day in
  Piqirun.gen_record code (_person :: _params :: _month :: _day :: [])

and gen__notification_birthday code x =
  let _number = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Notification_birthday.number in
  let _has_proprio_birthday = Piqirun.gen_required_field 2 gen__bool x.Notification_birthday.has_proprio_birthday in
  let _firstname1 = Piqirun.gen_optional_field 3 gen__string x.Notification_birthday.firstname1 in
  let _firstname2 = Piqirun.gen_optional_field 4 gen__string x.Notification_birthday.firstname2 in
  let _firstname3 = Piqirun.gen_optional_field 5 gen__string x.Notification_birthday.firstname3 in
  Piqirun.gen_record code (_number :: _has_proprio_birthday :: _firstname1 :: _firstname2 :: _firstname3 :: [])

and gen__person_start code x =
  let _lastname = Piqirun.gen_required_field 1 gen__string x.Person_start.lastname in
  let _firstname = Piqirun.gen_required_field 2 gen__string x.Person_start.firstname in
  let _sex = Piqirun.gen_required_field 3 gen__sex x.Person_start.sex in
  let _birth_date_day = Piqirun.gen_optional_field 4 gen__protobuf_int32 x.Person_start.birth_date_day in
  let _birth_date_month = Piqirun.gen_optional_field 5 gen__protobuf_int32 x.Person_start.birth_date_month in
  let _birth_date_year = Piqirun.gen_optional_field 6 gen__protobuf_int32 x.Person_start.birth_date_year in
  Piqirun.gen_record code (_lastname :: _firstname :: _sex :: _birth_date_day :: _birth_date_month :: _birth_date_year :: [])

and gen__synchro_params code x =
  let _export_directory = Piqirun.gen_required_field 1 gen__string x.Synchro_params.export_directory in
  let _timestamp = Piqirun.gen_required_field 2 gen__string x.Synchro_params.timestamp in
  Piqirun.gen_record code (_export_directory :: _timestamp :: [])

and gen__last_modifications code x =
  let _wizard = Piqirun.gen_optional_field 1 gen__string x.Last_modifications.wizard in
  let _max_res = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Last_modifications.max_res in
  let _range = Piqirun.gen_optional_field 3 gen__filter_date_range x.Last_modifications.range in
  Piqirun.gen_record code (_wizard :: _max_res :: _range :: [])

and gen__last_visits code x =
  let _user = Piqirun.gen_required_field 1 gen__string x.Last_visits.user in
  Piqirun.gen_record code (_user :: [])

and gen__correspondance_family code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Correspondance_family.index in
  let _spouse = Piqirun.gen_required_field 2 gen__person x.Correspondance_family.spouse in
  let _children = Piqirun.gen_repeated_field 3 gen__person x.Correspondance_family.children in
  Piqirun.gen_record code (_index :: _spouse :: _children :: [])

and gen__correspondance code x =
  let _base = Piqirun.gen_required_field 1 gen__string x.Correspondance.base in
  let _person = Piqirun.gen_required_field 2 gen__person x.Correspondance.person in
  let _father = Piqirun.gen_optional_field 3 gen__person x.Correspondance.father in
  let _mother = Piqirun.gen_optional_field 4 gen__person x.Correspondance.mother in
  let _families = Piqirun.gen_repeated_field 5 gen__correspondance_family x.Correspondance.families in
  Piqirun.gen_record code (_base :: _person :: _father :: _mother :: _families :: [])

and gen__correspondance_list code x =
  let _correspondances = Piqirun.gen_repeated_field 1 gen__correspondance x.Correspondance_list.correspondances in
  Piqirun.gen_record code (_correspondances :: [])

and gen__sex code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `male -> 0l
    | `female -> 1l
    | `unknown -> 2l
  )
and packed_gen__sex x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `male -> 0l
    | `female -> 1l
    | `unknown -> 2l
  )

and gen__death_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `dont_know_if_dead -> 4l
    | `of_course_dead -> 5l
  )
and packed_gen__death_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `dont_know_if_dead -> 4l
    | `of_course_dead -> 5l
  )

and gen__marriage_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
    | `no_mention -> 4l
    | `no_sexes_check_married -> 5l
  )
and packed_gen__marriage_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
    | `no_mention -> 4l
    | `no_sexes_check_married -> 5l
  )

and gen__divorce_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `not_divorced -> 0l
    | `divorced -> 1l
    | `separated -> 2l
  )
and packed_gen__divorce_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `not_divorced -> 0l
    | `divorced -> 1l
    | `separated -> 2l
  )

and gen__relation_parent_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `rpt_adoption -> 0l
    | `rpt_recognition -> 1l
    | `rpt_candidate_parent -> 2l
    | `rpt_god_parent -> 3l
    | `rpt_foster_parent -> 4l
  )
and packed_gen__relation_parent_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `rpt_adoption -> 0l
    | `rpt_recognition -> 1l
    | `rpt_candidate_parent -> 2l
    | `rpt_god_parent -> 3l
    | `rpt_foster_parent -> 4l
  )

and gen__title_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `title_main -> 0l
    | `title_name -> 1l
    | `title_none -> 2l
  )
and packed_gen__title_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `title_main -> 0l
    | `title_name -> 1l
    | `title_none -> 2l
  )

and gen__search_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `starting_with -> 0l
    | `approximative -> 1l
    | `lastname_or_firstname -> 2l
  )
and packed_gen__search_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `starting_with -> 0l
    | `approximative -> 1l
    | `lastname_or_firstname -> 2l
  )

and gen__relation_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `self -> 0l
    | `spouse -> 1l
    | `sibling -> 2l
    | `step_brother -> 3l
    | `parent -> 4l
    | `step_parent -> 5l
    | `grand_parent -> 6l
    | `uncle -> 7l
    | `uncle_spouse -> 8l
    | `cousin -> 9l
    | `cousin_spouse -> 10l
    | `child -> 11l
    | `step_child -> 12l
    | `grand_child -> 13l
    | `grand_child_spouse -> 14l
    | `great_grand_child -> 15l
    | `great_grand_child_spouse -> 16l
    | `child_cousin -> 17l
    | `child_cousin_spouse -> 18l
    | `grand_child_cousin -> 19l
    | `grand_child_cousin_spouse -> 20l
    | `great_grand_child_cousin -> 21l
    | `great_grand_child_cousin_spouse -> 22l
    | `nephew -> 23l
    | `nephew_spouse -> 24l
    | `nephew_spouse_spouse -> 25l
    | `grand_nephew -> 26l
    | `grand_nephew_spouse -> 27l
    | `grand_nephew_spouse_spouse -> 28l
    | `great_grand_nephew -> 29l
    | `great_grand_nephew_spouse -> 30l
    | `great_grand_nephew_spouse_spouse -> 31l
    | `adoptive_parent -> 32l
    | `adoptive_child -> 33l
    | `recognized_parent -> 34l
    | `recognized_child -> 35l
    | `candidate_parent -> 36l
    | `candidate_child -> 37l
    | `god_parent -> 38l
    | `god_child -> 39l
    | `foster_parent -> 40l
    | `foster_child -> 41l
    | `witness -> 42l
    | `no_relation -> 43l
  )
and packed_gen__relation_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `self -> 0l
    | `spouse -> 1l
    | `sibling -> 2l
    | `step_brother -> 3l
    | `parent -> 4l
    | `step_parent -> 5l
    | `grand_parent -> 6l
    | `uncle -> 7l
    | `uncle_spouse -> 8l
    | `cousin -> 9l
    | `cousin_spouse -> 10l
    | `child -> 11l
    | `step_child -> 12l
    | `grand_child -> 13l
    | `grand_child_spouse -> 14l
    | `great_grand_child -> 15l
    | `great_grand_child_spouse -> 16l
    | `child_cousin -> 17l
    | `child_cousin_spouse -> 18l
    | `grand_child_cousin -> 19l
    | `grand_child_cousin_spouse -> 20l
    | `great_grand_child_cousin -> 21l
    | `great_grand_child_cousin_spouse -> 22l
    | `nephew -> 23l
    | `nephew_spouse -> 24l
    | `nephew_spouse_spouse -> 25l
    | `grand_nephew -> 26l
    | `grand_nephew_spouse -> 27l
    | `grand_nephew_spouse_spouse -> 28l
    | `great_grand_nephew -> 29l
    | `great_grand_nephew_spouse -> 30l
    | `great_grand_nephew_spouse_spouse -> 31l
    | `adoptive_parent -> 32l
    | `adoptive_child -> 33l
    | `recognized_parent -> 34l
    | `recognized_child -> 35l
    | `candidate_parent -> 36l
    | `candidate_child -> 37l
    | `god_parent -> 38l
    | `god_child -> 39l
    | `foster_parent -> 40l
    | `foster_child -> 41l
    | `witness -> 42l
    | `no_relation -> 43l
  )

and gen__notif_birthday_params code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `close_person -> 0l
    | `descend_grand_parent -> 1l
    | `descend_great_grand_parent -> 2l
  )
and packed_gen__notif_birthday_params x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `close_person -> 0l
    | `descend_grand_parent -> 1l
    | `descend_great_grand_parent -> 2l
  )


let gen_int64 x = gen__int64 (-1) x
let gen_int32 x = gen__int32 (-1) x
let gen_protobuf_int64 x = gen__protobuf_int64 (-1) x
let gen_string x = gen__string (-1) x
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
let gen_bool x = gen__bool (-1) x
let gen_infos_base x = gen__infos_base (-1) x
let gen_reference_person x = gen__reference_person (-1) x
let gen_list_reference_persons x = gen__list_reference_persons (-1) x
let gen_relation_parent x = gen__relation_parent (-1) x
let gen_title x = gen__title (-1) x
let gen_spouse x = gen__spouse (-1) x
let gen_person x = gen__person (-1) x
let gen_full_person x = gen__full_person (-1) x
let gen_full_family x = gen__full_family (-1) x
let gen_internal_int32 x = gen__internal_int32 (-1) x
let gen_list_persons x = gen__list_persons (-1) x
let gen_list_full_persons x = gen__list_full_persons (-1) x
let gen_list_full_families x = gen__list_full_families (-1) x
let gen_search_params x = gen__search_params (-1) x
let gen_image x = gen__image (-1) x
let gen_full_image x = gen__full_image (-1) x
let gen_list_images x = gen__list_images (-1) x
let gen_list_full_images x = gen__list_full_images (-1) x
let gen_pers_img x = gen__pers_img (-1) x
let gen_list_pers_img x = gen__list_pers_img (-1) x
let gen_index x = gen__index (-1) x
let gen_image_address x = gen__image_address (-1) x
let gen_close_persons_params x = gen__close_persons_params (-1) x
let gen_person_relation x = gen__person_relation (-1) x
let gen_full_person_relation x = gen__full_person_relation (-1) x
let gen_list_person_relation x = gen__list_person_relation (-1) x
let gen_list_full_person_relation x = gen__list_full_person_relation (-1) x
let gen_anniversary_params x = gen__anniversary_params (-1) x
let gen_graph_params x = gen__graph_params (-1) x
let gen_graph_rel_params x = gen__graph_rel_params (-1) x
let gen_cpl_rel_params x = gen__cpl_rel_params (-1) x
let gen_node x = gen__node (-1) x
let gen_full_node x = gen__full_node (-1) x
let gen_edge x = gen__edge (-1) x
let gen_graph x = gen__graph (-1) x
let gen_full_graph x = gen__full_graph (-1) x
let gen_all_persons_params x = gen__all_persons_params (-1) x
let gen_all_families_params x = gen__all_families_params (-1) x
let gen_warning_already_defined x = gen__warning_already_defined (-1) x
let gen_warning_own_ancestor x = gen__warning_own_ancestor (-1) x
let gen_warning_bad_sex_of_married_person x = gen__warning_bad_sex_of_married_person (-1) x
let gen_warning_birth_after_death x = gen__warning_birth_after_death (-1) x
let gen_warning_incoherent_sex x = gen__warning_incoherent_sex (-1) x
let gen_warning_changed_order_of_children x = gen__warning_changed_order_of_children (-1) x
let gen_warning_changed_order_of_marriages x = gen__warning_changed_order_of_marriages (-1) x
let gen_warning_children_not_in_order x = gen__warning_children_not_in_order (-1) x
let gen_warning_dead_too_early_to_be_father x = gen__warning_dead_too_early_to_be_father (-1) x
let gen_warning_incoherent_ancestor_date x = gen__warning_incoherent_ancestor_date (-1) x
let gen_warning_marriage_date_after_death x = gen__warning_marriage_date_after_death (-1) x
let gen_warning_marriage_date_before_birth x = gen__warning_marriage_date_before_birth (-1) x
let gen_warning_mother_dead_before_child_birth x = gen__warning_mother_dead_before_child_birth (-1) x
let gen_warning_parent_born_after_child x = gen__warning_parent_born_after_child (-1) x
let gen_warning_parent_too_young x = gen__warning_parent_too_young (-1) x
let gen_warning_title_dates_error x = gen__warning_title_dates_error (-1) x
let gen_warning_undefined_sex x = gen__warning_undefined_sex (-1) x
let gen_warning_young_for_marriage x = gen__warning_young_for_marriage (-1) x
let gen_warning_parent_too_old x = gen__warning_parent_too_old (-1) x
let gen_warning_close_children x = gen__warning_close_children (-1) x
let gen_warning_big_age_between_spouses x = gen__warning_big_age_between_spouses (-1) x
let gen_warning_dead_old x = gen__warning_dead_old (-1) x
let gen_warning_old_individual x = gen__warning_old_individual (-1) x
let gen_warning_witness_date_after_death x = gen__warning_witness_date_after_death (-1) x
let gen_warning_witness_date_before_birth x = gen__warning_witness_date_before_birth (-1) x
let gen_warning_changed_order_of_family_events x = gen__warning_changed_order_of_family_events (-1) x
let gen_warning_changed_order_of_person_events x = gen__warning_changed_order_of_person_events (-1) x
let gen_warning_fevent_order x = gen__warning_fevent_order (-1) x
let gen_warning_fwitness_event_after_death x = gen__warning_fwitness_event_after_death (-1) x
let gen_warning_fwitness_event_before_birth x = gen__warning_fwitness_event_before_birth (-1) x
let gen_warning_pevent_order x = gen__warning_pevent_order (-1) x
let gen_warning_pwitness_event_after_death x = gen__warning_pwitness_event_after_death (-1) x
let gen_warning_pwitness_event_before_birth x = gen__warning_pwitness_event_before_birth (-1) x
let gen_base_warnings x = gen__base_warnings (-1) x
let gen_filter_date x = gen__filter_date (-1) x
let gen_filter_date_range x = gen__filter_date_range (-1) x
let gen_filters x = gen__filters (-1) x
let gen_modification_status x = gen__modification_status (-1) x
let gen_notification_birthday_params x = gen__notification_birthday_params (-1) x
let gen_notification_birthday x = gen__notification_birthday (-1) x
let gen_person_start x = gen__person_start (-1) x
let gen_synchro_params x = gen__synchro_params (-1) x
let gen_last_modifications x = gen__last_modifications (-1) x
let gen_last_visits x = gen__last_visits (-1) x
let gen_correspondance_family x = gen__correspondance_family (-1) x
let gen_correspondance x = gen__correspondance (-1) x
let gen_correspondance_list x = gen__correspondance_list (-1) x
let gen_sex x = gen__sex (-1) x
let gen_death_type x = gen__death_type (-1) x
let gen_marriage_type x = gen__marriage_type (-1) x
let gen_divorce_type x = gen__divorce_type (-1) x
let gen_relation_parent_type x = gen__relation_parent_type (-1) x
let gen_title_type x = gen__title_type (-1) x
let gen_search_type x = gen__search_type (-1) x
let gen_relation_type x = gen__relation_type (-1) x
let gen_notif_birthday_params x = gen__notif_birthday_params (-1) x


let rec default_int64 () = 0L
and default_int32 () = 0l
and default_protobuf_int64 () = default_int64 ()
and default_string () = ""
and default_protobuf_int32 () = default_int32 ()
and default_bool () = false
and default_infos_base () =
  {
    Infos_base.nb_persons = default_protobuf_int64 ();
    Infos_base.nb_families = default_protobuf_int64 ();
    Infos_base.sosa = None;
    Infos_base.last_modified_person = None;
    Infos_base.real_nb_persons = None;
  }
and default_reference_person () =
  {
    Reference_person.n = default_string ();
    Reference_person.p = default_string ();
    Reference_person.oc = default_protobuf_int32 ();
  }
and default_list_reference_persons () =
  {
    List_reference_persons.list_ref_persons = [];
  }
and default_relation_parent () =
  {
    Relation_parent.father = None;
    Relation_parent.mother = None;
    Relation_parent.source = None;
    Relation_parent.rpt_type = default_relation_parent_type ();
  }
and default_title () =
  {
    Title.title_type = default_title_type ();
    Title.name = None;
    Title.title = None;
    Title.fief = None;
    Title.date_begin = None;
    Title.date_end = None;
    Title.nth = None;
  }
and default_spouse () =
  {
    Spouse.sosa = default_string ();
    Spouse.n = default_string ();
    Spouse.p = default_string ();
    Spouse.oc = default_protobuf_int32 ();
    Spouse.sex = default_sex ();
    Spouse.lastname = default_string ();
    Spouse.firstname = default_string ();
    Spouse.public_name = None;
    Spouse.image = default_string ();
    Spouse.birth_date = default_string ();
    Spouse.birth_place = default_string ();
    Spouse.baptism_date = default_string ();
    Spouse.baptism_place = default_string ();
    Spouse.death_date = default_string ();
    Spouse.death_place = default_string ();
    Spouse.death_type = default_death_type ();
    Spouse.burial_date = default_string ();
    Spouse.burial_place = default_string ();
    Spouse.marriage_date = default_string ();
    Spouse.marriage_place = default_string ();
    Spouse.divorce_type = default_divorce_type ();
    Spouse.visible_for_visitors = default_bool ();
  }
and default_person () =
  {
    Person.sosa = default_string ();
    Person.n = default_string ();
    Person.p = default_string ();
    Person.oc = default_protobuf_int32 ();
    Person.sex = default_sex ();
    Person.lastname = default_string ();
    Person.firstname = default_string ();
    Person.public_name = None;
    Person.image = default_string ();
    Person.birth_date = default_string ();
    Person.birth_place = default_string ();
    Person.baptism_date = default_string ();
    Person.baptism_place = default_string ();
    Person.death_date = default_string ();
    Person.death_place = default_string ();
    Person.death_type = default_death_type ();
    Person.burial_date = default_string ();
    Person.burial_place = default_string ();
    Person.spouses = [];
    Person.ascend = default_bool ();
    Person.descend = default_bool ();
    Person.visible_for_visitors = default_bool ();
    Person.baseprefix = default_string ();
  }
and default_full_person () =
  {
    Full_person.sosa = default_string ();
    Full_person.n = default_string ();
    Full_person.p = default_string ();
    Full_person.oc = default_protobuf_int32 ();
    Full_person.index = default_protobuf_int32 ();
    Full_person.sex = default_sex ();
    Full_person.lastname = default_string ();
    Full_person.firstname = default_string ();
    Full_person.public_name = None;
    Full_person.aliases = [];
    Full_person.qualifiers = [];
    Full_person.firstname_aliases = [];
    Full_person.surname_aliases = [];
    Full_person.image = None;
    Full_person.birth_date = None;
    Full_person.birth_place = None;
    Full_person.birth_src = None;
    Full_person.baptism_date = None;
    Full_person.baptism_place = None;
    Full_person.baptism_src = None;
    Full_person.death_date = None;
    Full_person.death_place = None;
    Full_person.death_src = None;
    Full_person.death_type = default_death_type ();
    Full_person.burial_date = None;
    Full_person.burial_place = None;
    Full_person.burial_src = None;
    Full_person.occupation = None;
    Full_person.psources = None;
    Full_person.titles = [];
    Full_person.related = [];
    Full_person.rparents = [];
    Full_person.visible_for_visitors = default_bool ();
    Full_person.parents = None;
    Full_person.families = [];
    Full_person.baseprefix = default_string ();
  }
and default_full_family () =
  {
    Full_family.fsources = None;
    Full_family.marriage_date = None;
    Full_family.marriage_place = None;
    Full_family.marriage_src = None;
    Full_family.marriage_type = default_marriage_type ();
    Full_family.divorce_type = default_divorce_type ();
    Full_family.divorce_date = None;
    Full_family.witnesses = [];
    Full_family.father = default_protobuf_int32 ();
    Full_family.mother = default_protobuf_int32 ();
    Full_family.children = [];
    Full_family.index = default_protobuf_int32 ();
  }
and default_internal_int32 () =
  {
    Internal_int32.value = default_protobuf_int32 ();
  }
and default_list_persons () =
  {
    List_persons.list_persons = [];
  }
and default_list_full_persons () =
  {
    List_full_persons.persons = [];
  }
and default_list_full_families () =
  {
    List_full_families.families = [];
  }
and default_search_params () =
  {
    Search_params.search_type = parse_search_type (Piqirun.parse_default "\b\000");
    Search_params.lastname = None;
    Search_params.firstname = None;
    Search_params.only_sosa = parse_bool (Piqirun.parse_default "\b\000");
    Search_params.only_recent = parse_bool (Piqirun.parse_default "\b\000");
    Search_params.maiden_name = parse_bool (Piqirun.parse_default "\b\000");
  }
and default_image () =
  {
    Image.person = default_person ();
    Image.img = default_string ();
  }
and default_full_image () =
  {
    Full_image.person = default_full_person ();
    Full_image.img = default_string ();
  }
and default_list_images () =
  {
    List_images.list_images = [];
  }
and default_list_full_images () =
  {
    List_full_images.images = [];
  }
and default_pers_img () =
  {
    Pers_img.person = default_reference_person ();
    Pers_img.img = default_string ();
  }
and default_list_pers_img () =
  {
    List_pers_img.list_pers_img = [];
  }
and default_index () =
  {
    Index.index = default_protobuf_int32 ();
  }
and default_image_address () =
  {
    Image_address.img = default_string ();
  }
and default_close_persons_params () =
  {
    Close_persons_params.person = default_reference_person ();
    Close_persons_params.nb_gen_asc = None;
    Close_persons_params.nb_gen_desc = None;
    Close_persons_params.spouse_ascend = parse_bool (Piqirun.parse_default "\b\000");
    Close_persons_params.only_recent = parse_bool (Piqirun.parse_default "\b\000");
  }
and default_person_relation () =
  {
    Person_relation.person = default_person ();
    Person_relation.relation = default_relation_type ();
  }
and default_full_person_relation () =
  {
    Full_person_relation.person = default_full_person ();
    Full_person_relation.relation = default_relation_type ();
  }
and default_list_person_relation () =
  {
    List_person_relation.person_relations = [];
  }
and default_list_full_person_relation () =
  {
    List_full_person_relation.person_relations = [];
  }
and default_anniversary_params () =
  {
    Anniversary_params.month = None;
  }
and default_graph_params () =
  {
    Graph_params.generation = None;
    Graph_params.person = default_reference_person ();
  }
and default_graph_rel_params () =
  {
    Graph_rel_params.person1 = default_reference_person ();
    Graph_rel_params.person2 = default_reference_person ();
  }
and default_cpl_rel_params () =
  {
    Cpl_rel_params.person1 = default_reference_person ();
    Cpl_rel_params.person2 = default_reference_person ();
  }
and default_node () =
  {
    Node.id = default_protobuf_int64 ();
    Node.person = default_person ();
  }
and default_full_node () =
  {
    Full_node.id = default_protobuf_int64 ();
    Full_node.person = default_full_person ();
  }
and default_edge () =
  {
    Edge.from_node = default_protobuf_int64 ();
    Edge.to_node = default_protobuf_int64 ();
  }
and default_graph () =
  {
    Graph.nodes = [];
    Graph.edges = [];
  }
and default_full_graph () =
  {
    Full_graph.nodes = [];
    Full_graph.edges = [];
    Full_graph.families = [];
  }
and default_all_persons_params () =
  {
    All_persons_params.from = None;
    All_persons_params.limit = None;
  }
and default_all_families_params () =
  {
    All_families_params.from = None;
    All_families_params.limit = None;
  }
and default_warning_already_defined () =
  {
    Warning_already_defined.person = default_full_person ();
  }
and default_warning_own_ancestor () =
  {
    Warning_own_ancestor.person = default_full_person ();
  }
and default_warning_bad_sex_of_married_person () =
  {
    Warning_bad_sex_of_married_person.person = default_full_person ();
  }
and default_warning_birth_after_death () =
  {
    Warning_birth_after_death.person = default_full_person ();
  }
and default_warning_incoherent_sex () =
  {
    Warning_incoherent_sex.person = default_full_person ();
  }
and default_warning_changed_order_of_children () =
  {
    Warning_changed_order_of_children.father = default_full_person ();
    Warning_changed_order_of_children.mother = default_full_person ();
  }
and default_warning_changed_order_of_marriages () =
  {
    Warning_changed_order_of_marriages.person = default_full_person ();
  }
and default_warning_children_not_in_order () =
  {
    Warning_children_not_in_order.father = default_full_person ();
    Warning_children_not_in_order.mother = default_full_person ();
  }
and default_warning_dead_too_early_to_be_father () =
  {
    Warning_dead_too_early_to_be_father.son = default_full_person ();
    Warning_dead_too_early_to_be_father.father = default_full_person ();
  }
and default_warning_incoherent_ancestor_date () =
  {
    Warning_incoherent_ancestor_date.person = default_full_person ();
    Warning_incoherent_ancestor_date.ancestor = default_full_person ();
  }
and default_warning_marriage_date_after_death () =
  {
    Warning_marriage_date_after_death.person = default_full_person ();
  }
and default_warning_marriage_date_before_birth () =
  {
    Warning_marriage_date_before_birth.person = default_full_person ();
  }
and default_warning_mother_dead_before_child_birth () =
  {
    Warning_mother_dead_before_child_birth.mother = default_full_person ();
    Warning_mother_dead_before_child_birth.child = default_full_person ();
  }
and default_warning_parent_born_after_child () =
  {
    Warning_parent_born_after_child.parent = default_full_person ();
    Warning_parent_born_after_child.child = default_full_person ();
  }
and default_warning_parent_too_young () =
  {
    Warning_parent_too_young.parent = default_full_person ();
    Warning_parent_too_young.date = default_string ();
  }
and default_warning_title_dates_error () =
  {
    Warning_title_dates_error.person = default_full_person ();
  }
and default_warning_undefined_sex () =
  {
    Warning_undefined_sex.person = default_full_person ();
  }
and default_warning_young_for_marriage () =
  {
    Warning_young_for_marriage.person = default_full_person ();
    Warning_young_for_marriage.date = default_string ();
  }
and default_warning_parent_too_old () =
  {
    Warning_parent_too_old.parent = default_full_person ();
    Warning_parent_too_old.date = default_string ();
  }
and default_warning_close_children () =
  {
    Warning_close_children.father = default_full_person ();
    Warning_close_children.mother = default_full_person ();
    Warning_close_children.child1 = default_full_person ();
    Warning_close_children.child2 = default_full_person ();
  }
and default_warning_big_age_between_spouses () =
  {
    Warning_big_age_between_spouses.father = default_full_person ();
    Warning_big_age_between_spouses.mother = default_full_person ();
    Warning_big_age_between_spouses.date = default_string ();
  }
and default_warning_dead_old () =
  {
    Warning_dead_old.person = default_full_person ();
    Warning_dead_old.date = default_string ();
  }
and default_warning_old_individual () =
  {
    Warning_old_individual.person = default_full_person ();
    Warning_old_individual.date = default_string ();
  }
and default_warning_witness_date_after_death () =
  {
    Warning_witness_date_after_death.person = default_full_person ();
  }
and default_warning_witness_date_before_birth () =
  {
    Warning_witness_date_before_birth.person = default_full_person ();
  }
and default_warning_changed_order_of_family_events () =
  {
    Warning_changed_order_of_family_events.father = default_full_person ();
    Warning_changed_order_of_family_events.mother = default_full_person ();
  }
and default_warning_changed_order_of_person_events () =
  {
    Warning_changed_order_of_person_events.person = default_full_person ();
  }
and default_warning_fevent_order () =
  {
    Warning_fevent_order.person = default_full_person ();
    Warning_fevent_order.event1 = default_string ();
    Warning_fevent_order.event2 = default_string ();
  }
and default_warning_fwitness_event_after_death () =
  {
    Warning_fwitness_event_after_death.person = default_full_person ();
    Warning_fwitness_event_after_death.event = default_string ();
  }
and default_warning_fwitness_event_before_birth () =
  {
    Warning_fwitness_event_before_birth.person = default_full_person ();
    Warning_fwitness_event_before_birth.event = default_string ();
  }
and default_warning_pevent_order () =
  {
    Warning_pevent_order.person = default_full_person ();
    Warning_pevent_order.event1 = default_string ();
    Warning_pevent_order.event2 = default_string ();
  }
and default_warning_pwitness_event_after_death () =
  {
    Warning_pwitness_event_after_death.person = default_full_person ();
    Warning_pwitness_event_after_death.event = default_string ();
  }
and default_warning_pwitness_event_before_birth () =
  {
    Warning_pwitness_event_before_birth.person = default_full_person ();
    Warning_pwitness_event_before_birth.event = default_string ();
  }
and default_base_warnings () =
  {
    Base_warnings.warning_already_defined = [];
    Base_warnings.warning_own_ancestor = [];
    Base_warnings.warning_bad_sex_of_married_person = [];
    Base_warnings.warning_birth_after_death = [];
    Base_warnings.warning_incoherent_sex = [];
    Base_warnings.warning_changed_order_of_children = [];
    Base_warnings.warning_children_not_in_order = [];
    Base_warnings.warning_dead_too_early_to_be_father = [];
    Base_warnings.warning_incoherent_ancestor_date = [];
    Base_warnings.warning_marriage_date_after_death = [];
    Base_warnings.warning_marriage_date_before_birth = [];
    Base_warnings.warning_mother_dead_before_child_birth = [];
    Base_warnings.warning_parent_born_after_child = [];
    Base_warnings.warning_parent_too_young = [];
    Base_warnings.warning_title_dates_error = [];
    Base_warnings.warning_undefined_sex = [];
    Base_warnings.warning_young_for_marriage = [];
    Base_warnings.warning_close_children = [];
    Base_warnings.warning_parent_too_old = [];
    Base_warnings.warning_changed_order_of_marriages = [];
    Base_warnings.warning_big_age_between_spouses = [];
    Base_warnings.warning_dead_old = [];
    Base_warnings.warning_old_individual = [];
    Base_warnings.warning_witness_date_after_death = [];
    Base_warnings.warning_witness_date_before_birth = [];
  }
and default_filter_date () =
  {
    Filter_date.day = default_protobuf_int32 ();
    Filter_date.month = default_protobuf_int32 ();
    Filter_date.year = default_protobuf_int32 ();
  }
and default_filter_date_range () =
  {
    Filter_date_range.date_begin = default_filter_date ();
    Filter_date_range.date_end = default_filter_date ();
    Filter_date_range.only_exact = parse_bool (Piqirun.parse_default "\b\000");
  }
and default_filters () =
  {
    Filters.only_sosa = parse_bool (Piqirun.parse_default "\b\000");
    Filters.only_recent = parse_bool (Piqirun.parse_default "\b\000");
    Filters.sex = None;
    Filters.nb_results = parse_bool (Piqirun.parse_default "\b\000");
    Filters.date_birth = None;
    Filters.date_death = None;
  }
and default_modification_status () =
  {
    Modification_status.status = default_bool ();
    Modification_status.base_warnings = default_base_warnings ();
    Modification_status.index = None;
  }
and default_notification_birthday_params () =
  {
    Notification_birthday_params.person = default_reference_person ();
    Notification_birthday_params.params = default_notif_birthday_params ();
    Notification_birthday_params.month = None;
    Notification_birthday_params.day = None;
  }
and default_notification_birthday () =
  {
    Notification_birthday.number = default_protobuf_int32 ();
    Notification_birthday.has_proprio_birthday = default_bool ();
    Notification_birthday.firstname1 = None;
    Notification_birthday.firstname2 = None;
    Notification_birthday.firstname3 = None;
  }
and default_person_start () =
  {
    Person_start.lastname = default_string ();
    Person_start.firstname = default_string ();
    Person_start.sex = default_sex ();
    Person_start.birth_date_day = None;
    Person_start.birth_date_month = None;
    Person_start.birth_date_year = None;
  }
and default_synchro_params () =
  {
    Synchro_params.export_directory = default_string ();
    Synchro_params.timestamp = default_string ();
  }
and default_last_modifications () =
  {
    Last_modifications.wizard = None;
    Last_modifications.max_res = None;
    Last_modifications.range = None;
  }
and default_last_visits () =
  {
    Last_visits.user = default_string ();
  }
and default_correspondance_family () =
  {
    Correspondance_family.index = default_protobuf_int32 ();
    Correspondance_family.spouse = default_person ();
    Correspondance_family.children = [];
  }
and default_correspondance () =
  {
    Correspondance.base = default_string ();
    Correspondance.person = default_person ();
    Correspondance.father = None;
    Correspondance.mother = None;
    Correspondance.families = [];
  }
and default_correspondance_list () =
  {
    Correspondance_list.correspondances = [];
  }
and default_sex () = `male
and default_death_type () = `not_dead
and default_marriage_type () = `married
and default_divorce_type () = `not_divorced
and default_relation_parent_type () = `rpt_adoption
and default_title_type () = `title_main
and default_search_type () = `starting_with
and default_relation_type () = `self
and default_notif_birthday_params () = `close_person


let piqi = "\226\202\2304\003api\226\231\249\238\001\014api.proto.piqi\162\244\146\155\011\018geneweb.api.object\218\244\134\182\012\187\002\138\233\142\251\014\180\002\210\203\242$3\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nnb-persons\210\171\158\194\006\014protobuf-int64\210\203\242$4\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011nb-families\210\171\158\194\006\014protobuf-int64\210\203\242$/\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004sosa\210\171\158\194\006\016reference-person\210\203\242$=\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\020last-modified-person\210\171\158\194\006\014protobuf-int64\210\203\242$8\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015real-nb-persons\210\171\158\194\006\014protobuf-int64\218\164\238\191\004\ninfos-base\218\244\134\182\012\155\001\138\233\142\251\014\148\001\210\203\242$\"\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$+\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002oc\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\016reference-person\218\244\134\182\012b\138\233\142\251\014\\\210\203\242$;\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\016list-ref-persons\210\171\158\194\006\016reference-person\218\164\238\191\004\022list-reference-persons\218\244\134\182\012\236\001\138\233\142\251\014\229\001\210\203\242$/\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006father\210\171\158\194\006\014protobuf-int32\210\203\242$/\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006mother\210\171\158\194\006\014protobuf-int32\210\203\242$'\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006source\210\171\158\194\006\006string\210\203\242$7\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\brpt-type\210\171\158\194\006\020relation-parent-type\218\164\238\191\004\015relation-parent\218\244\134\182\012\212\002\138\233\142\251\014\205\002\210\203\242$/\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ntitle-type\210\171\158\194\006\ntitle-type\210\203\242$%\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004name\210\171\158\194\006\006string\210\203\242$&\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005title\210\171\158\194\006\006string\210\203\242$%\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004fief\210\171\158\194\006\006string\210\203\242$+\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndate-begin\210\171\158\194\006\006string\210\203\242$)\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bdate-end\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003nth\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\005title\218\244\134\182\012\175\b\138\233\142\251\014\168\b\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$+\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002oc\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$,\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$&\232\146\150q\018\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$+\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbirth-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q\022\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$-\232\146\150q\024\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012baptism-date\210\171\158\194\006\006string\210\203\242$.\232\146\150q\026\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rbaptism-place\210\171\158\194\006\006string\210\203\242$+\232\146\150q\028\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q\030\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$/\232\146\150q \152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-type\210\171\158\194\006\ndeath-type\210\203\242$,\232\146\150q\"\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011burial-date\210\171\158\194\006\006string\210\203\242$-\232\146\150q$\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012burial-place\210\171\158\194\006\006string\210\203\242$.\232\146\150q&\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rmarriage-date\210\171\158\194\006\006string\210\203\242$/\232\146\150q(\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\014marriage-place\210\171\158\194\006\006string\210\203\242$3\232\146\150q*\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012divorce-type\210\171\158\194\006\012divorce-type\210\203\242$3\232\146\150q,\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\020visible-for-visitors\210\171\158\194\006\004bool\218\164\238\191\004\006spouse\218\244\134\182\012\194\b\138\233\142\251\014\187\b\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$+\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002oc\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$,\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$&\232\146\150q\018\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$+\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbirth-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q\022\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$-\232\146\150q\024\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012baptism-date\210\171\158\194\006\006string\210\203\242$.\232\146\150q\026\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rbaptism-place\210\171\158\194\006\006string\210\203\242$+\232\146\150q\028\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q\030\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$/\232\146\150q \152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-type\210\171\158\194\006\ndeath-type\210\203\242$,\232\146\150q\"\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011burial-date\210\171\158\194\006\006string\210\203\242$-\232\146\150q$\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012burial-place\210\171\158\194\006\006string\210\203\242$(\232\146\150q&\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007spouses\210\171\158\194\006\006spouse\210\203\242$%\232\146\150q(\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006ascend\210\171\158\194\006\004bool\210\203\242$&\232\146\150q*\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007descend\210\171\158\194\006\004bool\210\203\242$3\232\146\150q,\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\020visible-for-visitors\210\171\158\194\006\004bool\210\203\242$+\232\146\150q.\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\218\164\238\191\004\006person\218\244\134\182\012\224\r\138\233\142\251\014\217\r\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$+\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002oc\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$,\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$(\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007aliases\210\171\158\194\006\006string\210\203\242$+\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nqualifiers\210\171\158\194\006\006string\210\203\242$2\232\146\150q\024\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\017firstname-aliases\210\171\158\194\006\006string\210\203\242$0\232\146\150q\026\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015surname-aliases\210\171\158\194\006\006string\210\203\242$&\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$+\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nbirth-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q$\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tbirth-src\210\171\158\194\006\006string\210\203\242$-\232\146\150q&\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012baptism-date\210\171\158\194\006\006string\210\203\242$.\232\146\150q(\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rbaptism-place\210\171\158\194\006\006string\210\203\242$,\232\146\150q*\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011baptism-src\210\171\158\194\006\006string\210\203\242$+\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeath-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q0\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tdeath-src\210\171\158\194\006\006string\210\203\242$/\232\146\150q2\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-type\210\171\158\194\006\ndeath-type\210\203\242$,\232\146\150q4\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011burial-date\210\171\158\194\006\006string\210\203\242$-\232\146\150q6\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012burial-place\210\171\158\194\006\006string\210\203\242$+\232\146\150q8\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nburial-src\210\171\158\194\006\006string\210\203\242$+\232\146\150q<\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\noccupation\210\171\158\194\006\006string\210\203\242$)\232\146\150q>\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bpsources\210\171\158\194\006\006string\210\203\242$&\232\146\150q@\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006titles\210\171\158\194\006\005title\210\203\242$0\232\146\150qB\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007related\210\171\158\194\006\014internal-int32\210\203\242$2\232\146\150qD\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\brparents\210\171\158\194\006\015relation-parent\210\203\242$3\232\146\150qF\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\020visible-for-visitors\210\171\158\194\006\004bool\210\203\242$0\232\146\150qH\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007parents\210\171\158\194\006\014protobuf-int32\210\203\242$1\232\146\150qJ\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\014internal-int32\210\203\242$+\232\146\150qL\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\218\164\238\191\004\011full-person\218\244\134\182\012\139\005\138\233\142\251\014\132\005\210\203\242$)\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bfsources\210\171\158\194\006\006string\210\203\242$.\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rmarriage-date\210\171\158\194\006\006string\210\203\242$/\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014marriage-place\210\171\158\194\006\006string\210\203\242$-\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012marriage-src\210\171\158\194\006\006string\210\203\242$5\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rmarriage-type\210\171\158\194\006\rmarriage-type\210\203\242$3\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012divorce-type\210\171\158\194\006\012divorce-type\210\203\242$-\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012divorce-date\210\171\158\194\006\006string\210\203\242$2\232\146\150q\016\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\014internal-int32\210\203\242$/\232\146\150q\018\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\014protobuf-int32\210\203\242$/\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\014protobuf-int32\210\203\242$1\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bchildren\210\171\158\194\006\014internal-int32\210\203\242$.\232\146\150q\024\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\011full-family\218\244\134\182\012M\138\233\142\251\014G\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005value\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\014internal-int32\218\244\134\182\012J\138\233\142\251\014D\210\203\242$-\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012list-persons\210\171\158\194\006\006person\218\164\238\191\004\012list-persons\218\244\134\182\012O\138\233\142\251\014I\210\203\242$-\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007persons\210\171\158\194\006\011full-person\218\164\238\191\004\017list-full-persons\218\244\134\182\012Q\138\233\142\251\014K\210\203\242$.\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\011full-family\218\164\238\191\004\018list-full-families\218\244\134\182\012\159\003\138\233\142\251\014\152\003\210\203\242$S\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011search-type\210\171\158\194\006\011search-type\138\140\251\240\r\028\218\148\211\024\002\b\000\210\171\158\194\006\015api/search-type\210\203\242$)\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$?\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tonly-sosa\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$A\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011only-recent\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$A\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011maiden-name\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\218\164\238\191\004\rsearch-params\218\244\134\182\012f\138\233\142\251\014`\210\203\242$'\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\006person\210\203\242$$\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003img\210\171\158\194\006\006string\218\164\238\191\004\005image\218\244\134\182\012p\138\233\142\251\014j\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$$\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003img\210\171\158\194\006\006string\218\164\238\191\004\nfull-image\218\244\134\182\012G\138\233\142\251\014A\210\203\242$+\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\011list-images\210\171\158\194\006\005image\218\164\238\191\004\011list-images\218\244\134\182\012L\138\233\142\251\014F\210\203\242$+\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006images\210\171\158\194\006\nfull-image\218\164\238\191\004\016list-full-images\218\244\134\182\012s\138\233\142\251\014m\210\203\242$1\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\016reference-person\210\203\242$$\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003img\210\171\158\194\006\006string\218\164\238\191\004\bpers-img\218\244\134\182\012N\138\233\142\251\014H\210\203\242$0\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\rlist-pers-img\210\171\158\194\006\bpers-img\218\164\238\191\004\rlist-pers-img\218\244\134\182\012D\138\233\142\251\014>\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\005index\218\244\134\182\012B\138\233\142\251\014<\210\203\242$$\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003img\210\171\158\194\006\006string\218\164\238\191\004\rimage-address\218\244\134\182\012\214\002\138\233\142\251\014\207\002\210\203\242$1\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\016reference-person\210\203\242$3\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nnb-gen-asc\210\171\158\194\006\014protobuf-int32\210\203\242$4\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011nb-gen-desc\210\171\158\194\006\014protobuf-int32\210\203\242$C\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rspouse-ascend\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$A\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011only-recent\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\218\164\238\191\004\020close-persons-params\218\244\134\182\012|\138\233\142\251\014v\210\203\242$'\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\006person\210\203\242$0\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\brelation\210\171\158\194\006\rrelation-type\218\164\238\191\004\015person-relation\218\244\134\182\012\135\001\138\233\142\251\014\128\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$0\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\brelation\210\171\158\194\006\rrelation-type\218\164\238\191\004\020full-person-relation\218\244\134\182\012_\138\233\142\251\014Y\210\203\242$:\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\016person-relations\210\171\158\194\006\015person-relation\218\164\238\191\004\020list-person-relation\218\244\134\182\012i\138\233\142\251\014c\210\203\242$?\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\016person-relations\210\171\158\194\006\020full-person-relation\218\164\238\191\004\025list-full-person-relation\218\244\134\182\012Q\138\233\142\251\014K\210\203\242$.\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005month\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\018anniversary-params\218\244\134\182\012\135\001\138\233\142\251\014\128\001\210\203\242$3\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ngeneration\210\171\158\194\006\014protobuf-int32\210\203\242$1\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\016reference-person\218\164\238\191\004\012graph-params\218\244\134\182\012\139\001\138\233\142\251\014\132\001\210\203\242$2\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007person1\210\171\158\194\006\016reference-person\210\203\242$2\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007person2\210\171\158\194\006\016reference-person\218\164\238\191\004\016graph-rel-params\218\244\134\182\012\137\001\138\233\142\251\014\130\001\210\203\242$2\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007person1\210\171\158\194\006\016reference-person\210\203\242$2\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007person2\210\171\158\194\006\016reference-person\218\164\238\191\004\014cpl-rel-params\218\244\134\182\012l\138\233\142\251\014f\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002id\210\171\158\194\006\014protobuf-int64\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\006person\218\164\238\191\004\004node\218\244\134\182\012v\138\233\142\251\014p\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002id\210\171\158\194\006\014protobuf-int64\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004\tfull-node\218\244\134\182\012|\138\233\142\251\014v\210\203\242$2\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfrom-node\210\171\158\194\006\014protobuf-int64\210\203\242$0\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007to-node\210\171\158\194\006\014protobuf-int64\218\164\238\191\004\004edge\218\244\134\182\012c\138\233\142\251\014]\210\203\242$$\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\005nodes\210\171\158\194\006\004node\210\203\242$$\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\005edges\210\171\158\194\006\004edge\218\164\238\191\004\005graph\218\244\134\182\012\161\001\138\233\142\251\014\154\001\210\203\242$)\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\005nodes\210\171\158\194\006\tfull-node\210\203\242$$\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\005edges\210\171\158\194\006\004edge\210\203\242$.\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\011full-family\218\164\238\191\004\nfull-graph\218\244\134\182\012\131\001\138\233\142\251\014}\210\203\242$-\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004from\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005limit\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\018all-persons-params\218\244\134\182\012\132\001\138\233\142\251\014~\210\203\242$-\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004from\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005limit\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\019all-families-params\218\244\134\182\012T\138\233\142\251\014N\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004\023warning-already-defined\218\244\134\182\012Q\138\233\142\251\014K\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004\020warning-own-ancestor\218\244\134\182\012^\138\233\142\251\014X\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004!warning-bad-sex-of-married-person\218\244\134\182\012V\138\233\142\251\014P\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004\025warning-birth-after-death\218\244\134\182\012S\138\233\142\251\014M\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004\022warning-incoherent-sex\218\244\134\182\012\144\001\138\233\142\251\014\137\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\011full-person\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\011full-person\218\164\238\191\004!warning-changed-order-of-children\218\244\134\182\012_\138\233\142\251\014Y\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004\"warning-changed-order-of-marriages\218\244\134\182\012\140\001\138\233\142\251\014\133\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\011full-person\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\011full-person\218\164\238\191\004\029warning-children-not-in-order\218\244\134\182\012\143\001\138\233\142\251\014\136\001\210\203\242$)\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003son\210\171\158\194\006\011full-person\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\011full-person\218\164\238\191\004#warning-dead-too-early-to-be-father\218\244\134\182\012\145\001\138\233\142\251\014\138\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bancestor\210\171\158\194\006\011full-person\218\164\238\191\004 warning-incoherent-ancestor-date\218\244\134\182\012^\138\233\142\251\014X\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004!warning-marriage-date-after-death\218\244\134\182\012_\138\233\142\251\014Y\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004\"warning-marriage-date-before-birth\218\244\134\182\012\148\001\138\233\142\251\014\141\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\011full-person\210\203\242$+\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005child\210\171\158\194\006\011full-person\218\164\238\191\004&warning-mother-dead-before-child-birth\218\244\134\182\012\141\001\138\233\142\251\014\134\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006parent\210\171\158\194\006\011full-person\210\203\242$+\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005child\210\171\158\194\006\011full-person\218\164\238\191\004\031warning-parent-born-after-child\218\244\134\182\012\127\138\233\142\251\014y\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006parent\210\171\158\194\006\011full-person\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004date\210\171\158\194\006\006string\218\164\238\191\004\024warning-parent-too-young\218\244\134\182\012V\138\233\142\251\014P\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004\025warning-title-dates-error\218\244\134\182\012R\138\233\142\251\014L\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004\021warning-undefined-sex\218\244\134\182\012\129\001\138\233\142\251\014{\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004date\210\171\158\194\006\006string\218\164\238\191\004\026warning-young-for-marriage\218\244\134\182\012}\138\233\142\251\014w\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006parent\210\171\158\194\006\011full-person\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004date\210\171\158\194\006\006string\218\164\238\191\004\022warning-parent-too-old\218\244\134\182\012\231\001\138\233\142\251\014\224\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\011full-person\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\011full-person\210\203\242$,\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006child1\210\171\158\194\006\011full-person\210\203\242$,\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006child2\210\171\158\194\006\011full-person\218\164\238\191\004\022warning-close-children\218\244\134\182\012\184\001\138\233\142\251\014\177\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\011full-person\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\011full-person\210\203\242$%\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004date\210\171\158\194\006\006string\218\164\238\191\004\031warning-big-age-between-spouses\218\244\134\182\012w\138\233\142\251\014q\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$%\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004date\210\171\158\194\006\006string\218\164\238\191\004\016warning-dead-old\218\244\134\182\012}\138\233\142\251\014w\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$%\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004date\210\171\158\194\006\006string\218\164\238\191\004\022warning-old-individual\218\244\134\182\012]\138\233\142\251\014W\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004 warning-witness-date-after-death\218\244\134\182\012^\138\233\142\251\014X\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004!warning-witness-date-before-birth\218\244\134\182\012\149\001\138\233\142\251\014\142\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\011full-person\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\011full-person\218\164\238\191\004&warning-changed-order-of-family-events\218\244\134\182\012c\138\233\142\251\014]\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\218\164\238\191\004&warning-changed-order-of-person-events\218\244\134\182\012\170\001\138\233\142\251\014\163\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006event1\210\171\158\194\006\006string\210\203\242$'\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006event2\210\171\158\194\006\006string\218\164\238\191\004\020warning-fevent-order\218\244\134\182\012\139\001\138\233\142\251\014\132\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$&\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005event\210\171\158\194\006\006string\218\164\238\191\004\"warning-fwitness-event-after-death\218\244\134\182\012\140\001\138\233\142\251\014\133\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$&\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005event\210\171\158\194\006\006string\218\164\238\191\004#warning-fwitness-event-before-birth\218\244\134\182\012\170\001\138\233\142\251\014\163\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006event1\210\171\158\194\006\006string\210\203\242$'\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006event2\210\171\158\194\006\006string\218\164\238\191\004\020warning-pevent-order\218\244\134\182\012\139\001\138\233\142\251\014\132\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$&\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005event\210\171\158\194\006\006string\218\164\238\191\004\"warning-pwitness-event-after-death\218\244\134\182\012\140\001\138\233\142\251\014\133\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011full-person\210\203\242$&\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005event\210\171\158\194\006\006string\218\164\238\191\004#warning-pwitness-event-before-birth\218\244\134\182\012\170\017\138\233\142\251\014\163\017\210\203\242$I\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\023warning-already-defined\210\171\158\194\006\023warning-already-defined\210\203\242$C\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\020warning-own-ancestor\210\171\158\194\006\020warning-own-ancestor\210\203\242$]\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004!warning-bad-sex-of-married-person\210\171\158\194\006!warning-bad-sex-of-married-person\210\203\242$M\232\146\150q\b\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\025warning-birth-after-death\210\171\158\194\006\025warning-birth-after-death\210\203\242$G\232\146\150q\n\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\022warning-incoherent-sex\210\171\158\194\006\022warning-incoherent-sex\210\203\242$]\232\146\150q\012\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004!warning-changed-order-of-children\210\171\158\194\006!warning-changed-order-of-children\210\203\242$U\232\146\150q\014\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\029warning-children-not-in-order\210\171\158\194\006\029warning-children-not-in-order\210\203\242$a\232\146\150q\016\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004#warning-dead-too-early-to-be-father\210\171\158\194\006#warning-dead-too-early-to-be-father\210\203\242$[\232\146\150q\018\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004 warning-incoherent-ancestor-date\210\171\158\194\006 warning-incoherent-ancestor-date\210\203\242$]\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004!warning-marriage-date-after-death\210\171\158\194\006!warning-marriage-date-after-death\210\203\242$_\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\"warning-marriage-date-before-birth\210\171\158\194\006\"warning-marriage-date-before-birth\210\203\242$g\232\146\150q\024\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004&warning-mother-dead-before-child-birth\210\171\158\194\006&warning-mother-dead-before-child-birth\210\203\242$Y\232\146\150q\026\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\031warning-parent-born-after-child\210\171\158\194\006\031warning-parent-born-after-child\210\203\242$K\232\146\150q\028\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\024warning-parent-too-young\210\171\158\194\006\024warning-parent-too-young\210\203\242$M\232\146\150q\030\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\025warning-title-dates-error\210\171\158\194\006\025warning-title-dates-error\210\203\242$E\232\146\150q \152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\021warning-undefined-sex\210\171\158\194\006\021warning-undefined-sex\210\203\242$O\232\146\150q\"\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\026warning-young-for-marriage\210\171\158\194\006\026warning-young-for-marriage\210\203\242$G\232\146\150q$\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\022warning-close-children\210\171\158\194\006\022warning-close-children\210\203\242$G\232\146\150q&\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\022warning-parent-too-old\210\171\158\194\006\022warning-parent-too-old\210\203\242$_\232\146\150q(\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\"warning-changed-order-of-marriages\210\171\158\194\006\"warning-changed-order-of-marriages\210\203\242$Y\232\146\150q*\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\031warning-big-age-between-spouses\210\171\158\194\006\031warning-big-age-between-spouses\210\203\242$;\232\146\150q,\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\016warning-dead-old\210\171\158\194\006\016warning-dead-old\210\203\242$G\232\146\150q.\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\022warning-old-individual\210\171\158\194\006\022warning-old-individual\210\203\242$[\232\146\150q0\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004 warning-witness-date-after-death\210\171\158\194\006 warning-witness-date-after-death\210\203\242$]\232\146\150q2\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004!warning-witness-date-before-birth\210\171\158\194\006!warning-witness-date-before-birth\218\164\238\191\004\rbase-warnings\218\244\134\182\012\174\001\138\233\142\251\014\167\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003day\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005month\210\171\158\194\006\014protobuf-int32\210\203\242$-\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004year\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\011filter-date\218\244\134\182\012\203\001\138\233\142\251\014\196\001\210\203\242$0\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndate-begin\210\171\158\194\006\011filter-date\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bdate-end\210\171\158\194\006\011filter-date\210\203\242$@\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nonly-exact\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\218\164\238\191\004\017filter-date-range\218\244\134\182\012\255\002\138\233\142\251\014\248\002\210\203\242$?\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tonly-sosa\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$A\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011only-recent\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$!\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$@\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nnb-results\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$6\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndate-birth\210\171\158\194\006\017filter-date-range\210\203\242$6\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndate-death\210\171\158\194\006\017filter-date-range\218\164\238\191\004\007filters\218\244\134\182\012\183\001\138\233\142\251\014\176\001\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006status\210\171\158\194\006\004bool\210\203\242$5\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rbase-warnings\210\171\158\194\006\rbase-warnings\210\203\242$.\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\019modification-status\218\244\134\182\012\254\001\138\233\142\251\014\247\001\210\203\242$1\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\016reference-person\210\203\242$6\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006params\210\171\158\194\006\021notif-birthday-params\210\203\242$.\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005month\210\171\158\194\006\014protobuf-int32\210\203\242$,\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003day\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\028notification-birthday-params\218\244\134\182\012\158\002\138\233\142\251\014\151\002\210\203\242$/\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006number\210\171\158\194\006\014protobuf-int32\210\203\242$3\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\020has-proprio-birthday\210\171\158\194\006\004bool\210\203\242$+\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nfirstname1\210\171\158\194\006\006string\210\203\242$+\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nfirstname2\210\171\158\194\006\006string\210\203\242$+\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nfirstname3\210\171\158\194\006\006string\218\164\238\191\004\021notification-birthday\218\244\134\182\012\211\002\138\233\142\251\014\204\002\210\203\242$)\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$!\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$7\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014birth-date-day\210\171\158\194\006\014protobuf-int32\210\203\242$9\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016birth-date-month\210\171\158\194\006\014protobuf-int32\210\203\242$8\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015birth-date-year\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\012person-start\218\244\134\182\012\127\138\233\142\251\014y\210\203\242$1\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016export-directory\210\171\158\194\006\006string\210\203\242$*\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ttimestamp\210\171\158\194\006\006string\218\164\238\191\004\014synchro-params\218\244\134\182\012\182\001\138\233\142\251\014\175\001\210\203\242$'\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006wizard\210\171\158\194\006\006string\210\203\242$0\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007max-res\210\171\158\194\006\014protobuf-int32\210\203\242$1\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005range\210\171\158\194\006\017filter-date-range\218\164\238\191\004\018last-modifications\218\244\134\182\012A\138\233\142\251\014;\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004user\210\171\158\194\006\006string\218\164\238\191\004\011last-visits\218\244\134\182\012\175\001\138\233\142\251\014\168\001\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006spouse\210\171\158\194\006\006person\210\203\242$)\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bchildren\210\171\158\194\006\006person\218\164\238\191\004\021correspondance-family\218\244\134\182\012\134\002\138\233\142\251\014\255\001\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004base\210\171\158\194\006\006string\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\006person\210\203\242$'\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006father\210\171\158\194\006\006person\210\203\242$'\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006mother\210\171\158\194\006\006person\210\203\242$8\232\146\150q\n\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\021correspondance-family\218\164\238\191\004\014correspondance\218\244\134\182\012\\\138\233\142\251\014V\210\203\242$8\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015correspondances\210\171\158\194\006\014correspondance\218\164\238\191\004\019correspondance-list\218\244\134\182\012S\138\176\205\197\001M\218\164\238\191\004\003sex\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004male\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006female\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007unknown\218\244\134\182\012\197\001\138\176\205\197\001\190\001\218\164\238\191\004\ndeath-type\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\bnot-dead\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004dead\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ndead-young\170\183\218\222\005\030\232\146\150q\006\218\164\238\191\004\019dead-dont-know-when\170\183\218\222\005\028\232\146\150q\b\218\164\238\191\004\017dont-know-if-dead\170\183\218\222\005\025\232\146\150q\n\218\164\238\191\004\014of-course-dead\218\244\134\182\012\211\001\138\176\205\197\001\204\001\218\164\238\191\004\rmarriage-type\170\183\218\222\005\018\232\146\150q\000\218\164\238\191\004\007married\170\183\218\222\005\022\232\146\150q\002\218\164\238\191\004\011not-married\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007engaged\170\183\218\222\005%\232\146\150q\006\218\164\238\191\004\026no-sexes-check-not-married\170\183\218\222\005\021\232\146\150q\b\218\164\238\191\004\nno-mention\170\183\218\222\005!\232\146\150q\n\218\164\238\191\004\022no-sexes-check-married\218\244\134\182\012h\138\176\205\197\001b\218\164\238\191\004\012divorce-type\170\183\218\222\005\023\232\146\150q\000\218\164\238\191\004\012not-divorced\170\183\218\222\005\019\232\146\150q\002\218\164\238\191\004\bdivorced\170\183\218\222\005\020\232\146\150q\004\218\164\238\191\004\tseparated\218\244\134\182\012\196\001\138\176\205\197\001\189\001\218\164\238\191\004\020relation-parent-type\170\183\218\222\005\023\232\146\150q\000\218\164\238\191\004\012rpt-adoption\170\183\218\222\005\026\232\146\150q\002\218\164\238\191\004\015rpt-recognition\170\183\218\222\005\031\232\146\150q\004\218\164\238\191\004\020rpt-candidate-parent\170\183\218\222\005\025\232\146\150q\006\218\164\238\191\004\014rpt-god-parent\170\183\218\222\005\028\232\146\150q\b\218\164\238\191\004\017rpt-foster-parent\218\244\134\182\012g\138\176\205\197\001a\218\164\238\191\004\ntitle-type\170\183\218\222\005\021\232\146\150q\000\218\164\238\191\004\ntitle-main\170\183\218\222\005\021\232\146\150q\002\218\164\238\191\004\ntitle-name\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ntitle-none\218\244\134\182\012y\138\176\205\197\001s\218\164\238\191\004\011search-type\170\183\218\222\005\024\232\146\150q\000\218\164\238\191\004\rstarting-with\170\183\218\222\005\024\232\146\150q\002\218\164\238\191\004\rapproximative\170\183\218\222\005 \232\146\150q\004\218\164\238\191\004\021lastname-or-firstname\218\244\134\182\012\128\011\138\176\205\197\001\249\n\218\164\238\191\004\rrelation-type\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004self\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006spouse\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007sibling\170\183\218\222\005\023\232\146\150q\006\218\164\238\191\004\012step-brother\170\183\218\222\005\017\232\146\150q\b\218\164\238\191\004\006parent\170\183\218\222\005\022\232\146\150q\n\218\164\238\191\004\011step-parent\170\183\218\222\005\023\232\146\150q\012\218\164\238\191\004\012grand-parent\170\183\218\222\005\016\232\146\150q\014\218\164\238\191\004\005uncle\170\183\218\222\005\023\232\146\150q\016\218\164\238\191\004\012uncle-spouse\170\183\218\222\005\017\232\146\150q\018\218\164\238\191\004\006cousin\170\183\218\222\005\024\232\146\150q\020\218\164\238\191\004\rcousin-spouse\170\183\218\222\005\016\232\146\150q\022\218\164\238\191\004\005child\170\183\218\222\005\021\232\146\150q\024\218\164\238\191\004\nstep-child\170\183\218\222\005\022\232\146\150q\026\218\164\238\191\004\011grand-child\170\183\218\222\005\029\232\146\150q\028\218\164\238\191\004\018grand-child-spouse\170\183\218\222\005\028\232\146\150q\030\218\164\238\191\004\017great-grand-child\170\183\218\222\005#\232\146\150q \218\164\238\191\004\024great-grand-child-spouse\170\183\218\222\005\023\232\146\150q\"\218\164\238\191\004\012child-cousin\170\183\218\222\005\030\232\146\150q$\218\164\238\191\004\019child-cousin-spouse\170\183\218\222\005\029\232\146\150q&\218\164\238\191\004\018grand-child-cousin\170\183\218\222\005$\232\146\150q(\218\164\238\191\004\025grand-child-cousin-spouse\170\183\218\222\005#\232\146\150q*\218\164\238\191\004\024great-grand-child-cousin\170\183\218\222\005*\232\146\150q,\218\164\238\191\004\031great-grand-child-cousin-spouse\170\183\218\222\005\017\232\146\150q.\218\164\238\191\004\006nephew\170\183\218\222\005\024\232\146\150q0\218\164\238\191\004\rnephew-spouse\170\183\218\222\005\031\232\146\150q2\218\164\238\191\004\020nephew-spouse-spouse\170\183\218\222\005\023\232\146\150q4\218\164\238\191\004\012grand-nephew\170\183\218\222\005\030\232\146\150q6\218\164\238\191\004\019grand-nephew-spouse\170\183\218\222\005%\232\146\150q8\218\164\238\191\004\026grand-nephew-spouse-spouse\170\183\218\222\005\029\232\146\150q:\218\164\238\191\004\018great-grand-nephew\170\183\218\222\005$\232\146\150q<\218\164\238\191\004\025great-grand-nephew-spouse\170\183\218\222\005+\232\146\150q>\218\164\238\191\004 great-grand-nephew-spouse-spouse\170\183\218\222\005\026\232\146\150q@\218\164\238\191\004\015adoptive-parent\170\183\218\222\005\025\232\146\150qB\218\164\238\191\004\014adoptive-child\170\183\218\222\005\028\232\146\150qD\218\164\238\191\004\017recognized-parent\170\183\218\222\005\027\232\146\150qF\218\164\238\191\004\016recognized-child\170\183\218\222\005\027\232\146\150qH\218\164\238\191\004\016candidate-parent\170\183\218\222\005\026\232\146\150qJ\218\164\238\191\004\015candidate-child\170\183\218\222\005\021\232\146\150qL\218\164\238\191\004\ngod-parent\170\183\218\222\005\020\232\146\150qN\218\164\238\191\004\tgod-child\170\183\218\222\005\024\232\146\150qP\218\164\238\191\004\rfoster-parent\170\183\218\222\005\023\232\146\150qR\218\164\238\191\004\012foster-child\170\183\218\222\005\018\232\146\150qT\218\164\238\191\004\007witness\170\183\218\222\005\022\232\146\150qV\218\164\238\191\004\011no-relation\218\244\134\182\012\143\001\138\176\205\197\001\136\001\218\164\238\191\004\021notif-birthday-params\170\183\218\222\005\023\232\146\150q\000\218\164\238\191\004\012close-person\170\183\218\222\005\031\232\146\150q\002\218\164\238\191\004\020descend-grand-parent\170\183\218\222\005%\232\146\150q\004\218\164\238\191\004\026descend-great-grand-parent"
include Api_piqi
