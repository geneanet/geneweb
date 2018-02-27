module rec Api_saisie_write_piqi:
  sig
    type protobuf_int32 = int32
    type sosa =
      [
        | `sosa_ref
        | `sosa
        | `no_sosa
      ]
    type calendar =
      [
        | `gregorian
        | `julian
        | `french
        | `hebrew
      ]
    type witness_type =
      [
        | `witness
        | `witness_godparent
      ]
    type precision =
      [
        | `sure
        | `about
        | `maybe
        | `before
        | `after
        | `oryear
        | `yearint
      ]
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
    type relation_type =
      [
        | `rparent_adoption
        | `rparent_recognition
        | `rparent_candidate_parent
        | `rparent_god_parent
        | `rparent_foster_parent
        | `rchild_adoption
        | `rchild_recognition
        | `rchild_candidate_parent
        | `rchild_god_parent
        | `rchild_foster_parent
      ]
    type create_or_link =
      [
        | `create
        | `link
        | `create_default_occ
      ]
    type fevent_name =
      [
        | `efam_marriage
        | `efam_no_marriage
        | `efam_no_mention
        | `efam_engage
        | `efam_divorce
        | `efam_separated
        | `efam_annulation
        | `efam_marriage_bann
        | `efam_marriage_contract
        | `efam_marriage_license
        | `efam_pacs
        | `efam_residence
      ]
    type relation_parent_type =
      [
        | `rpt_adoption_father
        | `rpt_adoption_mother
        | `rpt_recognition_father
        | `rpt_recognition_mother
        | `rpt_candidate_parent_father
        | `rpt_candidate_parent_mother
        | `rpt_god_parent_father
        | `rpt_god_parent_mother
        | `rpt_foster_parent_father
        | `rpt_foster_parent_mother
      ]
    type pevent_name =
      [
        | `epers_birth
        | `epers_baptism
        | `epers_death
        | `epers_burial
        | `epers_cremation
        | `epers_accomplishment
        | `epers_acquisition
        | `epers_adhesion
        | `epers_baptismlds
        | `epers_barmitzvah
        | `epers_batmitzvah
        | `epers_benediction
        | `epers_changename
        | `epers_circumcision
        | `epers_confirmation
        | `epers_confirmationlds
        | `epers_decoration
        | `epers_demobilisationmilitaire
        | `epers_diploma
        | `epers_distinction
        | `epers_dotation
        | `epers_dotationlds
        | `epers_education
        | `epers_election
        | `epers_emigration
        | `epers_excommunication
        | `epers_familylinklds
        | `epers_firstcommunion
        | `epers_funeral
        | `epers_graduate
        | `epers_hospitalisation
        | `epers_illness
        | `epers_immigration
        | `epers_listepassenger
        | `epers_militarydistinction
        | `epers_militarypromotion
        | `epers_militaryservice
        | `epers_mobilisationmilitaire
        | `epers_naturalisation
        | `epers_occupation
        | `epers_ordination
        | `epers_property
        | `epers_recensement
        | `epers_residence
        | `epers_retired
        | `epers_scellentchildlds
        | `epers_scellentparentlds
        | `epers_scellentspouselds
        | `epers_ventebien
        | `epers_will
      ]
    type access =
      [
        | `access_iftitles
        | `access_public
        | `access_private
      ]
    type update_warning_js =
      [
        | `empty_index
        | `empty_surname
        | `empty_first_name
        | `empty_sex
        | `required_field
        | `birth_date_after_event
        | `death_date_before_event
      ]
    type person_or_family =
      [
        | `person_form1
        | `person_form2
        | `family_form
      ]
    type auto_complete_place_field =
      [
        | `subdivision
        | `town
        | `area_code
        | `county
        | `region
        | `country
      ]
    type auto_complete_field =
      [
        | `lastname
        | `firstname
        | `place
        | `source
      ]
    type short_greg_month =
      [
        | `janv
        | `fevr
        | `mars
        | `avr
        | `mai
        | `juin
        | `juil
        | `aout
        | `sept
        | `oct
        | `nov
        | `dec
      ]
    type french_month =
      [
        | `vendemiaire
        | `brumaire
        | `frimaire
        | `nivose
        | `pluviose
        | `ventose
        | `germinal
        | `floreal
        | `prairial
        | `messidor
        | `thermidor
        | `fructidor
        | `complementaire
      ]
    type hebrew_month =
      [
        | `tichri
        | `marhechvan
        | `kislev
        | `tevet
        | `chevat
        | `adar_1
        | `adar_2
        | `nissan
        | `iyar
        | `sivan
        | `tamouz
        | `av
        | `eloul
      ]
    type dmy = Dmy.t
    type date = Date.t
    type person_search = Person_search.t
    type simple_person = Simple_person.t
    type witness_event = Witness_event.t
    type event = Event.t
    type relation_person = Relation_person.t
    type was_witness = Was_witness.t
    type person_search_info = Person_search_info.t
    type person_link = Person_link.t
    type witness = Witness.t
    type fevent = Fevent.t
    type relation_parent = Relation_parent.t
    type title = Title.t
    type pevent = Pevent.t
    type person = Person.t
    type family = Family.t
    type create_conflict = Create_conflict.t
    type modification_status = Modification_status.t
    type index_person = Index_person.t
    type index_family = Index_family.t
    type index_person_and_family = Index_person_and_family.t
    type family_spouse = Family_spouse.t
    type add_child_request = Add_child_request.t
    type add_child = Add_child.t
    type add_child_ok = Add_child_ok.t
    type add_parents = Add_parents.t
    type add_parents_ok = Add_parents_ok.t
    type add_family = Add_family.t
    type add_family_ok = Add_family_ok.t
    type edit_family_request = Edit_family_request.t
    type edit_family = Edit_family.t
    type edit_family_ok = Edit_family_ok.t
    type add_sibling_request = Add_sibling_request.t
    type add_sibling = Add_sibling.t
    type add_sibling_ok = Add_sibling_ok.t
    type add_first_fam = Add_first_fam.t
    type auto_complete = Auto_complete.t
    type auto_complete_result = Auto_complete_result.t
    type person_search_list_params = Person_search_list_params.t
    type person_search_list = Person_search_list.t
    type transl_calendar = Transl_calendar.t
    type config_transl_calendar = Config_transl_calendar.t
    type transl_witness_type = Transl_witness_type.t
    type config_transl_witness_type = Config_transl_witness_type.t
    type transl_precision = Transl_precision.t
    type config_transl_precision = Config_transl_precision.t
    type transl_death_type = Transl_death_type.t
    type config_transl_death_type = Config_transl_death_type.t
    type transl_relation_parent_type = Transl_relation_parent_type.t
    type config_transl_relation_parent_type = Config_transl_relation_parent_type.t
    type transl_fevent_name = Transl_fevent_name.t
    type config_transl_fevent_name = Config_transl_fevent_name.t
    type transl_pevent_name = Transl_pevent_name.t
    type config_transl_pevent_name = Config_transl_pevent_name.t
    type transl_access = Transl_access.t
    type config_transl_access = Config_transl_access.t
    type transl_update_warning_js = Transl_update_warning_js.t
    type config_transl_update_warning_js = Config_transl_update_warning_js.t
    type transl_short_greg_month = Transl_short_greg_month.t
    type config_transl_short_greg_month = Config_transl_short_greg_month.t
    type transl_french_month = Transl_french_month.t
    type config_transl_french_month = Config_transl_french_month.t
    type transl_hebrew_month = Transl_hebrew_month.t
    type config_transl_hebrew_month = Config_transl_hebrew_month.t
    type config = Config.t
  end = Api_saisie_write_piqi
and Dmy:
  sig
    type t = {
      mutable day: Api_saisie_write_piqi.protobuf_int32 option;
      mutable month: Api_saisie_write_piqi.protobuf_int32 option;
      mutable year: int32 option;
      mutable delta: Api_saisie_write_piqi.protobuf_int32 option;
    }
  end = Dmy
and Date:
  sig
    type t = {
      mutable cal: Api_saisie_write_piqi.calendar option;
      mutable prec: Api_saisie_write_piqi.precision option;
      mutable dmy: Api_saisie_write_piqi.dmy option;
      mutable dmy2: Api_saisie_write_piqi.dmy option;
      mutable text: string option;
    }
  end = Date
and Person_search:
  sig
    type t = {
      mutable index: Api_saisie_write_piqi.protobuf_int32;
      mutable sex: Api_saisie_write_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable dates: string option;
      mutable image: string option;
      mutable sosa: Api_saisie_write_piqi.sosa;
      mutable family: string;
      mutable n: string;
      mutable p: string;
      mutable oc: Api_saisie_write_piqi.protobuf_int32;
    }
  end = Person_search
and Simple_person:
  sig
    type t = {
      mutable index: Api_saisie_write_piqi.protobuf_int32;
      mutable sex: Api_saisie_write_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable birth_short_date: string option;
      mutable birth_place: string option;
      mutable death_short_date: string option;
      mutable death_place: string option;
      mutable image: string option;
      mutable sosa: Api_saisie_write_piqi.sosa;
    }
  end = Simple_person
and Witness_event:
  sig
    type t = {
      mutable witness_type: Api_saisie_write_piqi.witness_type;
      mutable witness: Api_saisie_write_piqi.simple_person;
    }
  end = Witness_event
and Event:
  sig
    type t = {
      mutable name: string;
      mutable date: string option;
      mutable date_conv: string option;
      mutable date_cal: Api_saisie_write_piqi.calendar option;
      mutable place: string option;
      mutable reason: string option;
      mutable note: string option;
      mutable src: string option;
      mutable spouse: Api_saisie_write_piqi.simple_person option;
      mutable witnesses: Api_saisie_write_piqi.witness_event list;
    }
  end = Event
and Relation_person:
  sig
    type t = {
      mutable r_type: Api_saisie_write_piqi.relation_type;
      mutable person: Api_saisie_write_piqi.simple_person;
    }
  end = Relation_person
and Was_witness:
  sig
    type t = {
      mutable husband: string;
      mutable wife: string;
    }
  end = Was_witness
and Person_search_info:
  sig
    type t = {
      mutable index: Api_saisie_write_piqi.protobuf_int32;
      mutable sex: Api_saisie_write_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable public_name: string option;
      mutable aliases: string list;
      mutable qualifiers: string list;
      mutable firstname_aliases: string list;
      mutable surname_aliases: string list;
      mutable image: string option;
      mutable events: Api_saisie_write_piqi.event list;
      mutable occupation: string option;
      mutable notes: string option;
      mutable psources: string option;
      mutable has_sources: bool;
      mutable titles: string list;
      mutable related: Api_saisie_write_piqi.relation_person list;
      mutable rparents: Api_saisie_write_piqi.relation_person list;
      mutable was_witness: Api_saisie_write_piqi.was_witness list;
      mutable sosa: Api_saisie_write_piqi.sosa;
    }
  end = Person_search_info
and Person_link:
  sig
    type t = {
      mutable create_link: Api_saisie_write_piqi.create_or_link;
      mutable index: Api_saisie_write_piqi.protobuf_int32;
      mutable sex: Api_saisie_write_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable occ: Api_saisie_write_piqi.protobuf_int32 option;
      mutable dates: string option;
    }
  end = Person_link
and Witness:
  sig
    type t = {
      mutable witness_type: Api_saisie_write_piqi.witness_type;
      mutable person: Api_saisie_write_piqi.person_link option;
    }
  end = Witness
and Fevent:
  sig
    type t = {
      mutable fevent_type: Api_saisie_write_piqi.fevent_name option;
      mutable date: Api_saisie_write_piqi.date option;
      mutable place: string option;
      mutable reason: string option;
      mutable note: string option;
      mutable src: string option;
      mutable witnesses: Api_saisie_write_piqi.witness list;
      mutable event_perso: string option;
    }
  end = Fevent
and Relation_parent:
  sig
    type t = {
      mutable rpt_type: Api_saisie_write_piqi.relation_parent_type;
      mutable person: Api_saisie_write_piqi.person_link option;
      mutable source: string option;
    }
  end = Relation_parent
and Title:
  sig
    type t = {
      mutable name: string option;
      mutable title: string option;
      mutable fief: string option;
      mutable date_begin: Api_saisie_write_piqi.date option;
      mutable date_end: Api_saisie_write_piqi.date option;
      mutable nth: Api_saisie_write_piqi.protobuf_int32 option;
    }
  end = Title
and Pevent:
  sig
    type t = {
      mutable pevent_type: Api_saisie_write_piqi.pevent_name option;
      mutable date: Api_saisie_write_piqi.date option;
      mutable place: string option;
      mutable reason: string option;
      mutable note: string option;
      mutable src: string option;
      mutable witnesses: Api_saisie_write_piqi.witness list;
      mutable event_perso: string option;
    }
  end = Pevent
and Person:
  sig
    type t = {
      mutable digest: string;
      mutable create_link: Api_saisie_write_piqi.create_or_link;
      mutable index: Api_saisie_write_piqi.protobuf_int32;
      mutable sex: Api_saisie_write_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable occ: Api_saisie_write_piqi.protobuf_int32 option;
      mutable public_name: string option;
      mutable aliases: string list;
      mutable qualifiers: string list;
      mutable firstname_aliases: string list;
      mutable surname_aliases: string list;
      mutable image: string option;
      mutable death_type: Api_saisie_write_piqi.death_type;
      mutable occupation: string option;
      mutable psources: string option;
      mutable notes: string option;
      mutable titles: Api_saisie_write_piqi.title list;
      mutable pevents: Api_saisie_write_piqi.pevent list;
      mutable related: Api_saisie_write_piqi.protobuf_int32 list;
      mutable rparents: Api_saisie_write_piqi.relation_parent list;
      mutable access: Api_saisie_write_piqi.access;
      mutable parents: Api_saisie_write_piqi.protobuf_int32 option;
      mutable families: Api_saisie_write_piqi.protobuf_int32 list;
    }
  end = Person
and Family:
  sig
    type t = {
      mutable digest: string;
      mutable index: Api_saisie_write_piqi.protobuf_int32;
      mutable fevents: Api_saisie_write_piqi.fevent list;
      mutable fsources: string option;
      mutable origin_file: string option;
      mutable comment: string option;
      mutable father: Api_saisie_write_piqi.person;
      mutable mother: Api_saisie_write_piqi.person;
      mutable children: Api_saisie_write_piqi.person_link list;
      mutable old_witnesses: Api_saisie_write_piqi.protobuf_int32 list;
    }
  end = Family
and Create_conflict:
  sig
    type t = {
      mutable form: Api_saisie_write_piqi.person_or_family option;
      mutable witness: bool;
      mutable rparents: bool;
      mutable event: bool;
      mutable pos: Api_saisie_write_piqi.protobuf_int32 option;
      mutable pos_witness: Api_saisie_write_piqi.protobuf_int32 option;
      mutable lastname: string;
      mutable firstname: string;
    }
  end = Create_conflict
and Modification_status:
  sig
    type t = {
      mutable is_base_updated: bool;
      mutable base_warnings: string list;
      mutable base_miscs: string list;
      mutable index_person: Api_saisie_write_piqi.protobuf_int32 option;
      mutable lastname: string;
      mutable firstname: string;
      mutable occ: Api_saisie_write_piqi.protobuf_int32 option;
      mutable index_family: Api_saisie_write_piqi.protobuf_int32 option;
      mutable conflict: Api_saisie_write_piqi.create_conflict option;
      mutable lastname_str: string option;
      mutable firstname_str: string option;
      mutable n: string option;
      mutable p: string option;
    }
  end = Modification_status
and Index_person:
  sig
    type t = {
      mutable index: Api_saisie_write_piqi.protobuf_int32;
    }
  end = Index_person
and Index_family:
  sig
    type t = {
      mutable index: Api_saisie_write_piqi.protobuf_int32;
    }
  end = Index_family
and Index_person_and_family:
  sig
    type t = {
      mutable index_person: Api_saisie_write_piqi.protobuf_int32;
      mutable index_family: Api_saisie_write_piqi.protobuf_int32;
    }
  end = Index_person_and_family
and Family_spouse:
  sig
    type t = {
      mutable index_family: Api_saisie_write_piqi.protobuf_int32;
      mutable index_person: Api_saisie_write_piqi.protobuf_int32;
      mutable sex: Api_saisie_write_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable dates: string option;
      mutable image: string option;
      mutable sosa: Api_saisie_write_piqi.sosa;
    }
  end = Family_spouse
and Add_child_request:
  sig
    type t = {
      mutable index: Api_saisie_write_piqi.protobuf_int32;
      mutable index_family: Api_saisie_write_piqi.protobuf_int32 option;
      mutable sex: Api_saisie_write_piqi.sex option;
    }
  end = Add_child_request
and Add_child:
  sig
    type t = {
      mutable person_lastname: string;
      mutable person_firstname: string;
      mutable family_spouse: Api_saisie_write_piqi.family_spouse list;
      mutable child: Api_saisie_write_piqi.person;
    }
  end = Add_child
and Add_child_ok:
  sig
    type t = {
      mutable index_person: Api_saisie_write_piqi.protobuf_int32;
      mutable index_family: Api_saisie_write_piqi.protobuf_int32;
      mutable new_family: bool;
      mutable child: Api_saisie_write_piqi.person;
    }
  end = Add_child_ok
and Add_parents:
  sig
    type t = {
      mutable person_lastname: string;
      mutable person_firstname: string;
      mutable family: Api_saisie_write_piqi.family;
    }
  end = Add_parents
and Add_parents_ok:
  sig
    type t = {
      mutable index_person: Api_saisie_write_piqi.protobuf_int32;
      mutable family: Api_saisie_write_piqi.family;
    }
  end = Add_parents_ok
and Add_family:
  sig
    type t = {
      mutable person_lastname: string;
      mutable person_firstname: string;
      mutable family: Api_saisie_write_piqi.family;
    }
  end = Add_family
and Add_family_ok:
  sig
    type t = {
      mutable index_person: Api_saisie_write_piqi.protobuf_int32;
      mutable family: Api_saisie_write_piqi.family;
    }
  end = Add_family_ok
and Edit_family_request:
  sig
    type t = {
      mutable spouses: Api_saisie_write_piqi.family_spouse list;
      mutable first_family: Api_saisie_write_piqi.edit_family option;
    }
  end = Edit_family_request
and Edit_family:
  sig
    type t = {
      mutable person_lastname: string;
      mutable person_firstname: string;
      mutable family: Api_saisie_write_piqi.family;
    }
  end = Edit_family
and Edit_family_ok:
  sig
    type t = {
      mutable index_person: Api_saisie_write_piqi.protobuf_int32;
      mutable family: Api_saisie_write_piqi.family;
    }
  end = Edit_family_ok
and Add_sibling_request:
  sig
    type t = {
      mutable index: Api_saisie_write_piqi.protobuf_int32;
      mutable sex: Api_saisie_write_piqi.sex option;
    }
  end = Add_sibling_request
and Add_sibling:
  sig
    type t = {
      mutable person_lastname: string;
      mutable person_firstname: string;
      mutable sibling: Api_saisie_write_piqi.person;
    }
  end = Add_sibling
and Add_sibling_ok:
  sig
    type t = {
      mutable index_person: Api_saisie_write_piqi.protobuf_int32;
      mutable sibling: Api_saisie_write_piqi.person;
    }
  end = Add_sibling_ok
and Add_first_fam:
  sig
    type t = {
      mutable sosa: Api_saisie_write_piqi.person;
      mutable father: Api_saisie_write_piqi.person;
      mutable mother: Api_saisie_write_piqi.person;
      mutable spouse: Api_saisie_write_piqi.person;
      mutable children: Api_saisie_write_piqi.person list;
    }
  end = Add_first_fam
and Auto_complete:
  sig
    type t = {
      mutable field: Api_saisie_write_piqi.auto_complete_field;
      mutable place_field: Api_saisie_write_piqi.auto_complete_place_field option;
      mutable input: string;
      mutable limit: Api_saisie_write_piqi.protobuf_int32;
    }
  end = Auto_complete
and Auto_complete_result:
  sig
    type t = {
      mutable result: string list;
    }
  end = Auto_complete_result
and Person_search_list_params:
  sig
    type t = {
      mutable lastname: string option;
      mutable firstname: string option;
      mutable limit: Api_saisie_write_piqi.protobuf_int32;
    }
  end = Person_search_list_params
and Person_search_list:
  sig
    type t = {
      mutable persons: Api_saisie_write_piqi.person_search list;
    }
  end = Person_search_list
and Transl_calendar:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.calendar;
      mutable sval: string;
    }
  end = Transl_calendar
and Config_transl_calendar:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_calendar list;
    }
  end = Config_transl_calendar
and Transl_witness_type:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.witness_type;
      mutable sval: string;
    }
  end = Transl_witness_type
and Config_transl_witness_type:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_witness_type list;
    }
  end = Config_transl_witness_type
and Transl_precision:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.precision;
      mutable sval: string;
    }
  end = Transl_precision
and Config_transl_precision:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_precision list;
    }
  end = Config_transl_precision
and Transl_death_type:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.death_type;
      mutable sval: string;
    }
  end = Transl_death_type
and Config_transl_death_type:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_death_type list;
    }
  end = Config_transl_death_type
and Transl_relation_parent_type:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.relation_parent_type;
      mutable sval: string;
    }
  end = Transl_relation_parent_type
and Config_transl_relation_parent_type:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_relation_parent_type list;
    }
  end = Config_transl_relation_parent_type
and Transl_fevent_name:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.fevent_name;
      mutable sval: string;
    }
  end = Transl_fevent_name
and Config_transl_fevent_name:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_fevent_name list;
    }
  end = Config_transl_fevent_name
and Transl_pevent_name:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.pevent_name;
      mutable sval: string;
    }
  end = Transl_pevent_name
and Config_transl_pevent_name:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_pevent_name list;
    }
  end = Config_transl_pevent_name
and Transl_access:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.access;
      mutable sval: string;
    }
  end = Transl_access
and Config_transl_access:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_access list;
    }
  end = Config_transl_access
and Transl_update_warning_js:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.update_warning_js;
      mutable sval: string;
    }
  end = Transl_update_warning_js
and Config_transl_update_warning_js:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_update_warning_js list;
    }
  end = Config_transl_update_warning_js
and Transl_short_greg_month:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.short_greg_month;
      mutable sval: string;
    }
  end = Transl_short_greg_month
and Config_transl_short_greg_month:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_short_greg_month list;
    }
  end = Config_transl_short_greg_month
and Transl_french_month:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.french_month;
      mutable sval: string;
    }
  end = Transl_french_month
and Config_transl_french_month:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_french_month list;
    }
  end = Config_transl_french_month
and Transl_hebrew_month:
  sig
    type t = {
      mutable pos: Api_saisie_write_piqi.hebrew_month;
      mutable sval: string;
    }
  end = Transl_hebrew_month
and Config_transl_hebrew_month:
  sig
    type t = {
      mutable msg: Api_saisie_write_piqi.transl_hebrew_month list;
    }
  end = Config_transl_hebrew_month
and Config:
  sig
    type t = {
      mutable transl_cal: Api_saisie_write_piqi.config_transl_calendar;
      mutable transl_wit: Api_saisie_write_piqi.config_transl_witness_type;
      mutable transl_prec: Api_saisie_write_piqi.config_transl_precision;
      mutable transl_death: Api_saisie_write_piqi.config_transl_death_type;
      mutable transl_rel: Api_saisie_write_piqi.config_transl_relation_parent_type;
      mutable transl_fevents: Api_saisie_write_piqi.config_transl_fevent_name;
      mutable transl_pevents: Api_saisie_write_piqi.config_transl_pevent_name;
      mutable transl_access: Api_saisie_write_piqi.config_transl_access;
      mutable transl_warning: Api_saisie_write_piqi.config_transl_update_warning_js;
      mutable transl_short_greg_month: Api_saisie_write_piqi.config_transl_short_greg_month;
      mutable transl_french_month: Api_saisie_write_piqi.config_transl_french_month;
      mutable transl_hebrew_month: Api_saisie_write_piqi.config_transl_hebrew_month;
      mutable gwf_place_format: string;
      mutable gwf_place_format_placeholder: string;
    }
  end = Config


let rec parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x

and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x

and parse_string x = Piqirun.string_of_block x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_dmy x =
  let x = Piqirun.parse_record x in
  let _day, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _month, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _year, x = Piqirun.parse_optional_field 3 parse_int32 x in
  let _delta, x = Piqirun.parse_optional_field 4 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Dmy.day = _day;
    Dmy.month = _month;
    Dmy.year = _year;
    Dmy.delta = _delta;
  }

and parse_date x =
  let x = Piqirun.parse_record x in
  let _cal, x = Piqirun.parse_optional_field 1 parse_calendar x in
  let _prec, x = Piqirun.parse_optional_field 2 parse_precision x in
  let _dmy, x = Piqirun.parse_optional_field 3 parse_dmy x in
  let _dmy2, x = Piqirun.parse_optional_field 4 parse_dmy x in
  let _text, x = Piqirun.parse_optional_field 5 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Date.cal = _cal;
    Date.prec = _prec;
    Date.dmy = _dmy;
    Date.dmy2 = _dmy2;
    Date.text = _text;
  }

and parse_person_search x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 2 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 3 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 4 parse_string x in
  let _dates, x = Piqirun.parse_optional_field 5 parse_string x in
  let _image, x = Piqirun.parse_optional_field 6 parse_string x in
  let _sosa, x = Piqirun.parse_required_field 7 parse_sosa x in
  let _family, x = Piqirun.parse_required_field 8 parse_string x in
  let _n, x = Piqirun.parse_required_field 9 parse_string x in
  let _p, x = Piqirun.parse_required_field 10 parse_string x in
  let _oc, x = Piqirun.parse_required_field 11 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Person_search.index = _index;
    Person_search.sex = _sex;
    Person_search.lastname = _lastname;
    Person_search.firstname = _firstname;
    Person_search.dates = _dates;
    Person_search.image = _image;
    Person_search.sosa = _sosa;
    Person_search.family = _family;
    Person_search.n = _n;
    Person_search.p = _p;
    Person_search.oc = _oc;
  }

and parse_simple_person x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 2 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 3 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 4 parse_string x in
  let _birth_short_date, x = Piqirun.parse_optional_field 5 parse_string x in
  let _birth_place, x = Piqirun.parse_optional_field 6 parse_string x in
  let _death_short_date, x = Piqirun.parse_optional_field 7 parse_string x in
  let _death_place, x = Piqirun.parse_optional_field 8 parse_string x in
  let _image, x = Piqirun.parse_optional_field 9 parse_string x in
  let _sosa, x = Piqirun.parse_required_field 10 parse_sosa x in
  Piqirun.check_unparsed_fields x;
  {
    Simple_person.index = _index;
    Simple_person.sex = _sex;
    Simple_person.lastname = _lastname;
    Simple_person.firstname = _firstname;
    Simple_person.birth_short_date = _birth_short_date;
    Simple_person.birth_place = _birth_place;
    Simple_person.death_short_date = _death_short_date;
    Simple_person.death_place = _death_place;
    Simple_person.image = _image;
    Simple_person.sosa = _sosa;
  }

and parse_witness_event x =
  let x = Piqirun.parse_record x in
  let _witness_type, x = Piqirun.parse_required_field 1 parse_witness_type x in
  let _witness, x = Piqirun.parse_required_field 2 parse_simple_person x in
  Piqirun.check_unparsed_fields x;
  {
    Witness_event.witness_type = _witness_type;
    Witness_event.witness = _witness;
  }

and parse_event x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_required_field 1 parse_string x in
  let _date, x = Piqirun.parse_optional_field 2 parse_string x in
  let _date_conv, x = Piqirun.parse_optional_field 3 parse_string x in
  let _date_cal, x = Piqirun.parse_optional_field 4 parse_calendar x in
  let _place, x = Piqirun.parse_optional_field 5 parse_string x in
  let _reason, x = Piqirun.parse_optional_field 6 parse_string x in
  let _note, x = Piqirun.parse_optional_field 7 parse_string x in
  let _src, x = Piqirun.parse_optional_field 8 parse_string x in
  let _spouse, x = Piqirun.parse_optional_field 9 parse_simple_person x in
  let _witnesses, x = Piqirun.parse_repeated_field 10 parse_witness_event x in
  Piqirun.check_unparsed_fields x;
  {
    Event.name = _name;
    Event.date = _date;
    Event.date_conv = _date_conv;
    Event.date_cal = _date_cal;
    Event.place = _place;
    Event.reason = _reason;
    Event.note = _note;
    Event.src = _src;
    Event.spouse = _spouse;
    Event.witnesses = _witnesses;
  }

and parse_relation_person x =
  let x = Piqirun.parse_record x in
  let _r_type, x = Piqirun.parse_required_field 1 parse_relation_type x in
  let _person, x = Piqirun.parse_required_field 2 parse_simple_person x in
  Piqirun.check_unparsed_fields x;
  {
    Relation_person.r_type = _r_type;
    Relation_person.person = _person;
  }

and parse_was_witness x =
  let x = Piqirun.parse_record x in
  let _husband, x = Piqirun.parse_required_field 1 parse_string x in
  let _wife, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Was_witness.husband = _husband;
    Was_witness.wife = _wife;
  }

and parse_person_search_info x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 2 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 3 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 4 parse_string x in
  let _public_name, x = Piqirun.parse_optional_field 5 parse_string x in
  let _aliases, x = Piqirun.parse_repeated_field 6 parse_string x in
  let _qualifiers, x = Piqirun.parse_repeated_field 7 parse_string x in
  let _firstname_aliases, x = Piqirun.parse_repeated_field 8 parse_string x in
  let _surname_aliases, x = Piqirun.parse_repeated_field 9 parse_string x in
  let _image, x = Piqirun.parse_optional_field 10 parse_string x in
  let _events, x = Piqirun.parse_repeated_field 11 parse_event x in
  let _occupation, x = Piqirun.parse_optional_field 12 parse_string x in
  let _notes, x = Piqirun.parse_optional_field 13 parse_string x in
  let _psources, x = Piqirun.parse_optional_field 14 parse_string x in
  let _has_sources, x = Piqirun.parse_required_field 15 parse_bool x in
  let _titles, x = Piqirun.parse_repeated_field 16 parse_string x in
  let _related, x = Piqirun.parse_repeated_field 17 parse_relation_person x in
  let _rparents, x = Piqirun.parse_repeated_field 18 parse_relation_person x in
  let _was_witness, x = Piqirun.parse_repeated_field 19 parse_was_witness x in
  let _sosa, x = Piqirun.parse_required_field 20 parse_sosa x in
  Piqirun.check_unparsed_fields x;
  {
    Person_search_info.index = _index;
    Person_search_info.sex = _sex;
    Person_search_info.lastname = _lastname;
    Person_search_info.firstname = _firstname;
    Person_search_info.public_name = _public_name;
    Person_search_info.aliases = _aliases;
    Person_search_info.qualifiers = _qualifiers;
    Person_search_info.firstname_aliases = _firstname_aliases;
    Person_search_info.surname_aliases = _surname_aliases;
    Person_search_info.image = _image;
    Person_search_info.events = _events;
    Person_search_info.occupation = _occupation;
    Person_search_info.notes = _notes;
    Person_search_info.psources = _psources;
    Person_search_info.has_sources = _has_sources;
    Person_search_info.titles = _titles;
    Person_search_info.related = _related;
    Person_search_info.rparents = _rparents;
    Person_search_info.was_witness = _was_witness;
    Person_search_info.sosa = _sosa;
  }

and parse_person_link x =
  let x = Piqirun.parse_record x in
  let _create_link, x = Piqirun.parse_required_field 1 parse_create_or_link x in
  let _index, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 3 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 4 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 5 parse_string x in
  let _occ, x = Piqirun.parse_optional_field 6 parse_protobuf_int32 x in
  let _dates, x = Piqirun.parse_optional_field 7 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Person_link.create_link = _create_link;
    Person_link.index = _index;
    Person_link.sex = _sex;
    Person_link.lastname = _lastname;
    Person_link.firstname = _firstname;
    Person_link.occ = _occ;
    Person_link.dates = _dates;
  }

and parse_witness x =
  let x = Piqirun.parse_record x in
  let _witness_type, x = Piqirun.parse_required_field 1 parse_witness_type x in
  let _person, x = Piqirun.parse_optional_field 2 parse_person_link x in
  Piqirun.check_unparsed_fields x;
  {
    Witness.witness_type = _witness_type;
    Witness.person = _person;
  }

and parse_fevent x =
  let x = Piqirun.parse_record x in
  let _fevent_type, x = Piqirun.parse_optional_field 1 parse_fevent_name x in
  let _date, x = Piqirun.parse_optional_field 2 parse_date x in
  let _place, x = Piqirun.parse_optional_field 3 parse_string x in
  let _reason, x = Piqirun.parse_optional_field 4 parse_string x in
  let _note, x = Piqirun.parse_optional_field 5 parse_string x in
  let _src, x = Piqirun.parse_optional_field 6 parse_string x in
  let _witnesses, x = Piqirun.parse_repeated_field 7 parse_witness x in
  let _event_perso, x = Piqirun.parse_optional_field 8 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Fevent.fevent_type = _fevent_type;
    Fevent.date = _date;
    Fevent.place = _place;
    Fevent.reason = _reason;
    Fevent.note = _note;
    Fevent.src = _src;
    Fevent.witnesses = _witnesses;
    Fevent.event_perso = _event_perso;
  }

and parse_relation_parent x =
  let x = Piqirun.parse_record x in
  let _rpt_type, x = Piqirun.parse_required_field 1 parse_relation_parent_type x in
  let _person, x = Piqirun.parse_optional_field 2 parse_person_link x in
  let _source, x = Piqirun.parse_optional_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Relation_parent.rpt_type = _rpt_type;
    Relation_parent.person = _person;
    Relation_parent.source = _source;
  }

and parse_title x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_optional_field 1 parse_string x in
  let _title, x = Piqirun.parse_optional_field 2 parse_string x in
  let _fief, x = Piqirun.parse_optional_field 3 parse_string x in
  let _date_begin, x = Piqirun.parse_optional_field 4 parse_date x in
  let _date_end, x = Piqirun.parse_optional_field 5 parse_date x in
  let _nth, x = Piqirun.parse_optional_field 6 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Title.name = _name;
    Title.title = _title;
    Title.fief = _fief;
    Title.date_begin = _date_begin;
    Title.date_end = _date_end;
    Title.nth = _nth;
  }

and parse_pevent x =
  let x = Piqirun.parse_record x in
  let _pevent_type, x = Piqirun.parse_optional_field 1 parse_pevent_name x in
  let _date, x = Piqirun.parse_optional_field 2 parse_date x in
  let _place, x = Piqirun.parse_optional_field 3 parse_string x in
  let _reason, x = Piqirun.parse_optional_field 4 parse_string x in
  let _note, x = Piqirun.parse_optional_field 5 parse_string x in
  let _src, x = Piqirun.parse_optional_field 6 parse_string x in
  let _witnesses, x = Piqirun.parse_repeated_field 7 parse_witness x in
  let _event_perso, x = Piqirun.parse_optional_field 8 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Pevent.pevent_type = _pevent_type;
    Pevent.date = _date;
    Pevent.place = _place;
    Pevent.reason = _reason;
    Pevent.note = _note;
    Pevent.src = _src;
    Pevent.witnesses = _witnesses;
    Pevent.event_perso = _event_perso;
  }

and parse_person x =
  let x = Piqirun.parse_record x in
  let _digest, x = Piqirun.parse_required_field 1 parse_string x in
  let _create_link, x = Piqirun.parse_required_field 2 parse_create_or_link x in
  let _index, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 4 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 5 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 6 parse_string x in
  let _occ, x = Piqirun.parse_optional_field 7 parse_protobuf_int32 x in
  let _public_name, x = Piqirun.parse_optional_field 8 parse_string x in
  let _aliases, x = Piqirun.parse_repeated_field 9 parse_string x in
  let _qualifiers, x = Piqirun.parse_repeated_field 10 parse_string x in
  let _firstname_aliases, x = Piqirun.parse_repeated_field 11 parse_string x in
  let _surname_aliases, x = Piqirun.parse_repeated_field 12 parse_string x in
  let _image, x = Piqirun.parse_optional_field 13 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 14 parse_death_type x in
  let _occupation, x = Piqirun.parse_optional_field 15 parse_string x in
  let _psources, x = Piqirun.parse_optional_field 16 parse_string x in
  let _notes, x = Piqirun.parse_optional_field 17 parse_string x in
  let _titles, x = Piqirun.parse_repeated_field 18 parse_title x in
  let _pevents, x = Piqirun.parse_repeated_field 19 parse_pevent x in
  let _related, x = Piqirun.parse_repeated_field 20 parse_protobuf_int32 x in
  let _rparents, x = Piqirun.parse_repeated_field 21 parse_relation_parent x in
  let _access, x = Piqirun.parse_required_field 22 parse_access x ~default:"\b\000" in
  let _parents, x = Piqirun.parse_optional_field 23 parse_protobuf_int32 x in
  let _families, x = Piqirun.parse_repeated_field 24 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Person.digest = _digest;
    Person.create_link = _create_link;
    Person.index = _index;
    Person.sex = _sex;
    Person.lastname = _lastname;
    Person.firstname = _firstname;
    Person.occ = _occ;
    Person.public_name = _public_name;
    Person.aliases = _aliases;
    Person.qualifiers = _qualifiers;
    Person.firstname_aliases = _firstname_aliases;
    Person.surname_aliases = _surname_aliases;
    Person.image = _image;
    Person.death_type = _death_type;
    Person.occupation = _occupation;
    Person.psources = _psources;
    Person.notes = _notes;
    Person.titles = _titles;
    Person.pevents = _pevents;
    Person.related = _related;
    Person.rparents = _rparents;
    Person.access = _access;
    Person.parents = _parents;
    Person.families = _families;
  }

and parse_family x =
  let x = Piqirun.parse_record x in
  let _digest, x = Piqirun.parse_required_field 1 parse_string x in
  let _index, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _fevents, x = Piqirun.parse_repeated_field 3 parse_fevent x in
  let _fsources, x = Piqirun.parse_optional_field 4 parse_string x in
  let _origin_file, x = Piqirun.parse_optional_field 5 parse_string x in
  let _comment, x = Piqirun.parse_optional_field 6 parse_string x in
  let _father, x = Piqirun.parse_required_field 7 parse_person x in
  let _mother, x = Piqirun.parse_required_field 8 parse_person x in
  let _children, x = Piqirun.parse_repeated_field 9 parse_person_link x in
  let _old_witnesses, x = Piqirun.parse_repeated_field 10 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Family.digest = _digest;
    Family.index = _index;
    Family.fevents = _fevents;
    Family.fsources = _fsources;
    Family.origin_file = _origin_file;
    Family.comment = _comment;
    Family.father = _father;
    Family.mother = _mother;
    Family.children = _children;
    Family.old_witnesses = _old_witnesses;
  }

and parse_create_conflict x =
  let x = Piqirun.parse_record x in
  let _form, x = Piqirun.parse_optional_field 1 parse_person_or_family x in
  let _witness, x = Piqirun.parse_required_field 2 parse_bool x in
  let _rparents, x = Piqirun.parse_required_field 3 parse_bool x in
  let _event, x = Piqirun.parse_required_field 4 parse_bool x in
  let _pos, x = Piqirun.parse_optional_field 5 parse_protobuf_int32 x in
  let _pos_witness, x = Piqirun.parse_optional_field 6 parse_protobuf_int32 x in
  let _lastname, x = Piqirun.parse_required_field 7 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 8 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Create_conflict.form = _form;
    Create_conflict.witness = _witness;
    Create_conflict.rparents = _rparents;
    Create_conflict.event = _event;
    Create_conflict.pos = _pos;
    Create_conflict.pos_witness = _pos_witness;
    Create_conflict.lastname = _lastname;
    Create_conflict.firstname = _firstname;
  }

and parse_modification_status x =
  let x = Piqirun.parse_record x in
  let _is_base_updated, x = Piqirun.parse_required_field 1 parse_bool x in
  let _base_warnings, x = Piqirun.parse_repeated_field 2 parse_string x in
  let _index_person, x = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  let _lastname, x = Piqirun.parse_required_field 4 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 5 parse_string x in
  let _occ, x = Piqirun.parse_optional_field 6 parse_protobuf_int32 x in
  let _index_family, x = Piqirun.parse_optional_field 7 parse_protobuf_int32 x in
  let _conflict, x = Piqirun.parse_optional_field 8 parse_create_conflict x in
  let _lastname_str, x = Piqirun.parse_optional_field 9 parse_string x in
  let _firstname_str, x = Piqirun.parse_optional_field 10 parse_string x in
  let _n, x = Piqirun.parse_optional_field 11 parse_string x in
  let _p, x = Piqirun.parse_optional_field 12 parse_string x in
  let _base_miscs, x = Piqirun.parse_repeated_field 13 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Modification_status.is_base_updated = _is_base_updated;
    Modification_status.base_warnings = _base_warnings;
    Modification_status.index_person = _index_person;
    Modification_status.lastname = _lastname;
    Modification_status.firstname = _firstname;
    Modification_status.occ = _occ;
    Modification_status.index_family = _index_family;
    Modification_status.conflict = _conflict;
    Modification_status.lastname_str = _lastname_str;
    Modification_status.firstname_str = _firstname_str;
    Modification_status.n = _n;
    Modification_status.p = _p;
    Modification_status.base_miscs = _base_miscs;
  }

and parse_index_person x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Index_person.index = _index;
  }

and parse_index_family x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Index_family.index = _index;
  }

and parse_index_person_and_family x =
  let x = Piqirun.parse_record x in
  let _index_person, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _index_family, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Index_person_and_family.index_person = _index_person;
    Index_person_and_family.index_family = _index_family;
  }

and parse_family_spouse x =
  let x = Piqirun.parse_record x in
  let _index_family, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _index_person, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 3 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 4 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 5 parse_string x in
  let _dates, x = Piqirun.parse_optional_field 6 parse_string x in
  let _image, x = Piqirun.parse_optional_field 7 parse_string x in
  let _sosa, x = Piqirun.parse_required_field 8 parse_sosa x in
  Piqirun.check_unparsed_fields x;
  {
    Family_spouse.index_family = _index_family;
    Family_spouse.index_person = _index_person;
    Family_spouse.sex = _sex;
    Family_spouse.lastname = _lastname;
    Family_spouse.firstname = _firstname;
    Family_spouse.dates = _dates;
    Family_spouse.image = _image;
    Family_spouse.sosa = _sosa;
  }

and parse_add_child_request x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _index_family, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_optional_field 3 parse_sex x in
  Piqirun.check_unparsed_fields x;
  {
    Add_child_request.index = _index;
    Add_child_request.index_family = _index_family;
    Add_child_request.sex = _sex;
  }

and parse_add_child x =
  let x = Piqirun.parse_record x in
  let _person_lastname, x = Piqirun.parse_required_field 1 parse_string x in
  let _person_firstname, x = Piqirun.parse_required_field 2 parse_string x in
  let _family_spouse, x = Piqirun.parse_repeated_field 3 parse_family_spouse x in
  let _child, x = Piqirun.parse_required_field 4 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Add_child.person_lastname = _person_lastname;
    Add_child.person_firstname = _person_firstname;
    Add_child.family_spouse = _family_spouse;
    Add_child.child = _child;
  }

and parse_add_child_ok x =
  let x = Piqirun.parse_record x in
  let _index_person, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _index_family, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _new_family, x = Piqirun.parse_required_field 3 parse_bool x in
  let _child, x = Piqirun.parse_required_field 4 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Add_child_ok.index_person = _index_person;
    Add_child_ok.index_family = _index_family;
    Add_child_ok.new_family = _new_family;
    Add_child_ok.child = _child;
  }

and parse_add_parents x =
  let x = Piqirun.parse_record x in
  let _person_lastname, x = Piqirun.parse_required_field 1 parse_string x in
  let _person_firstname, x = Piqirun.parse_required_field 2 parse_string x in
  let _family, x = Piqirun.parse_required_field 3 parse_family x in
  Piqirun.check_unparsed_fields x;
  {
    Add_parents.person_lastname = _person_lastname;
    Add_parents.person_firstname = _person_firstname;
    Add_parents.family = _family;
  }

and parse_add_parents_ok x =
  let x = Piqirun.parse_record x in
  let _index_person, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _family, x = Piqirun.parse_required_field 2 parse_family x in
  Piqirun.check_unparsed_fields x;
  {
    Add_parents_ok.index_person = _index_person;
    Add_parents_ok.family = _family;
  }

and parse_add_family x =
  let x = Piqirun.parse_record x in
  let _person_lastname, x = Piqirun.parse_required_field 1 parse_string x in
  let _person_firstname, x = Piqirun.parse_required_field 2 parse_string x in
  let _family, x = Piqirun.parse_required_field 3 parse_family x in
  Piqirun.check_unparsed_fields x;
  {
    Add_family.person_lastname = _person_lastname;
    Add_family.person_firstname = _person_firstname;
    Add_family.family = _family;
  }

and parse_add_family_ok x =
  let x = Piqirun.parse_record x in
  let _index_person, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _family, x = Piqirun.parse_required_field 2 parse_family x in
  Piqirun.check_unparsed_fields x;
  {
    Add_family_ok.index_person = _index_person;
    Add_family_ok.family = _family;
  }

and parse_edit_family_request x =
  let x = Piqirun.parse_record x in
  let _spouses, x = Piqirun.parse_repeated_field 1 parse_family_spouse x in
  let _first_family, x = Piqirun.parse_optional_field 2 parse_edit_family x in
  Piqirun.check_unparsed_fields x;
  {
    Edit_family_request.spouses = _spouses;
    Edit_family_request.first_family = _first_family;
  }

and parse_edit_family x =
  let x = Piqirun.parse_record x in
  let _person_lastname, x = Piqirun.parse_required_field 1 parse_string x in
  let _person_firstname, x = Piqirun.parse_required_field 2 parse_string x in
  let _family, x = Piqirun.parse_required_field 3 parse_family x in
  Piqirun.check_unparsed_fields x;
  {
    Edit_family.person_lastname = _person_lastname;
    Edit_family.person_firstname = _person_firstname;
    Edit_family.family = _family;
  }

and parse_edit_family_ok x =
  let x = Piqirun.parse_record x in
  let _index_person, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _family, x = Piqirun.parse_required_field 2 parse_family x in
  Piqirun.check_unparsed_fields x;
  {
    Edit_family_ok.index_person = _index_person;
    Edit_family_ok.family = _family;
  }

and parse_add_sibling_request x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_optional_field 2 parse_sex x in
  Piqirun.check_unparsed_fields x;
  {
    Add_sibling_request.index = _index;
    Add_sibling_request.sex = _sex;
  }

and parse_add_sibling x =
  let x = Piqirun.parse_record x in
  let _person_lastname, x = Piqirun.parse_required_field 1 parse_string x in
  let _person_firstname, x = Piqirun.parse_required_field 2 parse_string x in
  let _sibling, x = Piqirun.parse_required_field 3 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Add_sibling.person_lastname = _person_lastname;
    Add_sibling.person_firstname = _person_firstname;
    Add_sibling.sibling = _sibling;
  }

and parse_add_sibling_ok x =
  let x = Piqirun.parse_record x in
  let _index_person, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _sibling, x = Piqirun.parse_required_field 2 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Add_sibling_ok.index_person = _index_person;
    Add_sibling_ok.sibling = _sibling;
  }

and parse_add_first_fam x =
  let x = Piqirun.parse_record x in
  let _sosa, x = Piqirun.parse_required_field 1 parse_person x in
  let _father, x = Piqirun.parse_required_field 2 parse_person x in
  let _mother, x = Piqirun.parse_required_field 3 parse_person x in
  let _spouse, x = Piqirun.parse_required_field 4 parse_person x in
  let _children, x = Piqirun.parse_repeated_field 5 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Add_first_fam.sosa = _sosa;
    Add_first_fam.father = _father;
    Add_first_fam.mother = _mother;
    Add_first_fam.spouse = _spouse;
    Add_first_fam.children = _children;
  }

and parse_auto_complete x =
  let x = Piqirun.parse_record x in
  let _field, x = Piqirun.parse_required_field 1 parse_auto_complete_field x in
  let _place_field, x = Piqirun.parse_optional_field 2 parse_auto_complete_place_field x in
  let _input, x = Piqirun.parse_required_field 3 parse_string x in
  let _limit, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Auto_complete.field = _field;
    Auto_complete.place_field = _place_field;
    Auto_complete.input = _input;
    Auto_complete.limit = _limit;
  }

and parse_auto_complete_result x =
  let x = Piqirun.parse_record x in
  let _result, x = Piqirun.parse_repeated_field 1 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Auto_complete_result.result = _result;
  }

and parse_person_search_list_params x =
  let x = Piqirun.parse_record x in
  let _lastname, x = Piqirun.parse_optional_field 1 parse_string x in
  let _firstname, x = Piqirun.parse_optional_field 2 parse_string x in
  let _limit, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Person_search_list_params.lastname = _lastname;
    Person_search_list_params.firstname = _firstname;
    Person_search_list_params.limit = _limit;
  }

and parse_person_search_list x =
  let x = Piqirun.parse_record x in
  let _persons, x = Piqirun.parse_repeated_field 1 parse_person_search x in
  Piqirun.check_unparsed_fields x;
  {
    Person_search_list.persons = _persons;
  }

and parse_transl_calendar x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_calendar x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_calendar.pos = _pos;
    Transl_calendar.sval = _sval;
  }

and parse_config_transl_calendar x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_calendar x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_calendar.msg = _msg;
  }

and parse_transl_witness_type x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_witness_type x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_witness_type.pos = _pos;
    Transl_witness_type.sval = _sval;
  }

and parse_config_transl_witness_type x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_witness_type x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_witness_type.msg = _msg;
  }

and parse_transl_precision x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_precision x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_precision.pos = _pos;
    Transl_precision.sval = _sval;
  }

and parse_config_transl_precision x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_precision x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_precision.msg = _msg;
  }

and parse_transl_death_type x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_death_type x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_death_type.pos = _pos;
    Transl_death_type.sval = _sval;
  }

and parse_config_transl_death_type x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_death_type x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_death_type.msg = _msg;
  }

and parse_transl_relation_parent_type x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_relation_parent_type x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_relation_parent_type.pos = _pos;
    Transl_relation_parent_type.sval = _sval;
  }

and parse_config_transl_relation_parent_type x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_relation_parent_type x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_relation_parent_type.msg = _msg;
  }

and parse_transl_fevent_name x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_fevent_name x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_fevent_name.pos = _pos;
    Transl_fevent_name.sval = _sval;
  }

and parse_config_transl_fevent_name x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_fevent_name x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_fevent_name.msg = _msg;
  }

and parse_transl_pevent_name x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_pevent_name x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_pevent_name.pos = _pos;
    Transl_pevent_name.sval = _sval;
  }

and parse_config_transl_pevent_name x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_pevent_name x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_pevent_name.msg = _msg;
  }

and parse_transl_access x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_access x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_access.pos = _pos;
    Transl_access.sval = _sval;
  }

and parse_config_transl_access x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_access x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_access.msg = _msg;
  }

and parse_transl_update_warning_js x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_update_warning_js x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_update_warning_js.pos = _pos;
    Transl_update_warning_js.sval = _sval;
  }

and parse_config_transl_update_warning_js x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_update_warning_js x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_update_warning_js.msg = _msg;
  }

and parse_transl_short_greg_month x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_short_greg_month x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_short_greg_month.pos = _pos;
    Transl_short_greg_month.sval = _sval;
  }

and parse_config_transl_short_greg_month x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_short_greg_month x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_short_greg_month.msg = _msg;
  }

and parse_transl_french_month x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_french_month x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_french_month.pos = _pos;
    Transl_french_month.sval = _sval;
  }

and parse_config_transl_french_month x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_french_month x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_french_month.msg = _msg;
  }

and parse_transl_hebrew_month x =
  let x = Piqirun.parse_record x in
  let _pos, x = Piqirun.parse_required_field 1 parse_hebrew_month x in
  let _sval, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Transl_hebrew_month.pos = _pos;
    Transl_hebrew_month.sval = _sval;
  }

and parse_config_transl_hebrew_month x =
  let x = Piqirun.parse_record x in
  let _msg, x = Piqirun.parse_repeated_field 1 parse_transl_hebrew_month x in
  Piqirun.check_unparsed_fields x;
  {
    Config_transl_hebrew_month.msg = _msg;
  }

and parse_config x =
  let x = Piqirun.parse_record x in
  let _transl_cal, x = Piqirun.parse_required_field 1 parse_config_transl_calendar x in
  let _transl_wit, x = Piqirun.parse_required_field 2 parse_config_transl_witness_type x in
  let _transl_prec, x = Piqirun.parse_required_field 3 parse_config_transl_precision x in
  let _transl_death, x = Piqirun.parse_required_field 4 parse_config_transl_death_type x in
  let _transl_rel, x = Piqirun.parse_required_field 5 parse_config_transl_relation_parent_type x in
  let _transl_fevents, x = Piqirun.parse_required_field 6 parse_config_transl_fevent_name x in
  let _transl_pevents, x = Piqirun.parse_required_field 7 parse_config_transl_pevent_name x in
  let _transl_access, x = Piqirun.parse_required_field 8 parse_config_transl_access x in
  let _transl_warning, x = Piqirun.parse_required_field 9 parse_config_transl_update_warning_js x in
  let _transl_short_greg_month, x = Piqirun.parse_required_field 10 parse_config_transl_short_greg_month x in
  let _transl_french_month, x = Piqirun.parse_required_field 11 parse_config_transl_french_month x in
  let _transl_hebrew_month, x = Piqirun.parse_required_field 12 parse_config_transl_hebrew_month x in
  let _gwf_place_format, x = Piqirun.parse_required_field 13 parse_string x in
  let _gwf_place_format_placeholder, x = Piqirun.parse_required_field 14 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Config.transl_cal = _transl_cal;
    Config.transl_wit = _transl_wit;
    Config.transl_prec = _transl_prec;
    Config.transl_death = _transl_death;
    Config.transl_rel = _transl_rel;
    Config.transl_fevents = _transl_fevents;
    Config.transl_pevents = _transl_pevents;
    Config.transl_access = _transl_access;
    Config.transl_warning = _transl_warning;
    Config.transl_short_greg_month = _transl_short_greg_month;
    Config.transl_french_month = _transl_french_month;
    Config.transl_hebrew_month = _transl_hebrew_month;
    Config.gwf_place_format = _gwf_place_format;
    Config.gwf_place_format_placeholder = _gwf_place_format_placeholder;
  }

and parse_sosa x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `sosa_ref
    | 1l -> `sosa
    | 2l -> `no_sosa
    | x -> Piqirun.error_enum_const x
and packed_parse_sosa x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `sosa_ref
    | 1l -> `sosa
    | 2l -> `no_sosa
    | x -> Piqirun.error_enum_const x

and parse_calendar x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `gregorian
    | 1l -> `julian
    | 2l -> `french
    | 3l -> `hebrew
    | x -> Piqirun.error_enum_const x
and packed_parse_calendar x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `gregorian
    | 1l -> `julian
    | 2l -> `french
    | 3l -> `hebrew
    | x -> Piqirun.error_enum_const x

and parse_witness_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `witness
    | 1l -> `witness_godparent
    | x -> Piqirun.error_enum_const x
and packed_parse_witness_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `witness
    | 1l -> `witness_godparent
    | x -> Piqirun.error_enum_const x

and parse_precision x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `sure
    | 1l -> `about
    | 2l -> `maybe
    | 3l -> `before
    | 4l -> `after
    | 5l -> `oryear
    | 6l -> `yearint
    | x -> Piqirun.error_enum_const x
and packed_parse_precision x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `sure
    | 1l -> `about
    | 2l -> `maybe
    | 3l -> `before
    | 4l -> `after
    | 5l -> `oryear
    | 6l -> `yearint
    | x -> Piqirun.error_enum_const x

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

and parse_relation_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `rparent_adoption
    | 1l -> `rparent_recognition
    | 2l -> `rparent_candidate_parent
    | 3l -> `rparent_god_parent
    | 4l -> `rparent_foster_parent
    | 5l -> `rchild_adoption
    | 6l -> `rchild_recognition
    | 7l -> `rchild_candidate_parent
    | 8l -> `rchild_god_parent
    | 9l -> `rchild_foster_parent
    | x -> Piqirun.error_enum_const x
and packed_parse_relation_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `rparent_adoption
    | 1l -> `rparent_recognition
    | 2l -> `rparent_candidate_parent
    | 3l -> `rparent_god_parent
    | 4l -> `rparent_foster_parent
    | 5l -> `rchild_adoption
    | 6l -> `rchild_recognition
    | 7l -> `rchild_candidate_parent
    | 8l -> `rchild_god_parent
    | 9l -> `rchild_foster_parent
    | x -> Piqirun.error_enum_const x

and parse_create_or_link x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `create
    | 1l -> `link
    | 2l -> `create_default_occ
    | x -> Piqirun.error_enum_const x
and packed_parse_create_or_link x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `create
    | 1l -> `link
    | 2l -> `create_default_occ
    | x -> Piqirun.error_enum_const x

and parse_fevent_name x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `efam_marriage
    | 1l -> `efam_no_marriage
    | 2l -> `efam_no_mention
    | 3l -> `efam_engage
    | 4l -> `efam_divorce
    | 5l -> `efam_separated
    | 6l -> `efam_annulation
    | 7l -> `efam_marriage_bann
    | 8l -> `efam_marriage_contract
    | 9l -> `efam_marriage_license
    | 10l -> `efam_pacs
    | 11l -> `efam_residence
    | x -> Piqirun.error_enum_const x
and packed_parse_fevent_name x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `efam_marriage
    | 1l -> `efam_no_marriage
    | 2l -> `efam_no_mention
    | 3l -> `efam_engage
    | 4l -> `efam_divorce
    | 5l -> `efam_separated
    | 6l -> `efam_annulation
    | 7l -> `efam_marriage_bann
    | 8l -> `efam_marriage_contract
    | 9l -> `efam_marriage_license
    | 10l -> `efam_pacs
    | 11l -> `efam_residence
    | x -> Piqirun.error_enum_const x

and parse_relation_parent_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `rpt_adoption_father
    | 1l -> `rpt_adoption_mother
    | 2l -> `rpt_recognition_father
    | 3l -> `rpt_recognition_mother
    | 4l -> `rpt_candidate_parent_father
    | 5l -> `rpt_candidate_parent_mother
    | 6l -> `rpt_god_parent_father
    | 7l -> `rpt_god_parent_mother
    | 8l -> `rpt_foster_parent_father
    | 9l -> `rpt_foster_parent_mother
    | x -> Piqirun.error_enum_const x
and packed_parse_relation_parent_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `rpt_adoption_father
    | 1l -> `rpt_adoption_mother
    | 2l -> `rpt_recognition_father
    | 3l -> `rpt_recognition_mother
    | 4l -> `rpt_candidate_parent_father
    | 5l -> `rpt_candidate_parent_mother
    | 6l -> `rpt_god_parent_father
    | 7l -> `rpt_god_parent_mother
    | 8l -> `rpt_foster_parent_father
    | 9l -> `rpt_foster_parent_mother
    | x -> Piqirun.error_enum_const x

and parse_pevent_name x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `epers_birth
    | 1l -> `epers_baptism
    | 2l -> `epers_death
    | 3l -> `epers_burial
    | 4l -> `epers_cremation
    | 5l -> `epers_accomplishment
    | 6l -> `epers_acquisition
    | 7l -> `epers_adhesion
    | 8l -> `epers_baptismlds
    | 9l -> `epers_barmitzvah
    | 10l -> `epers_batmitzvah
    | 11l -> `epers_benediction
    | 12l -> `epers_changename
    | 13l -> `epers_circumcision
    | 14l -> `epers_confirmation
    | 15l -> `epers_confirmationlds
    | 16l -> `epers_decoration
    | 17l -> `epers_demobilisationmilitaire
    | 18l -> `epers_diploma
    | 19l -> `epers_distinction
    | 20l -> `epers_dotation
    | 21l -> `epers_dotationlds
    | 22l -> `epers_education
    | 23l -> `epers_election
    | 24l -> `epers_emigration
    | 25l -> `epers_excommunication
    | 26l -> `epers_familylinklds
    | 27l -> `epers_firstcommunion
    | 28l -> `epers_funeral
    | 29l -> `epers_graduate
    | 30l -> `epers_hospitalisation
    | 31l -> `epers_illness
    | 32l -> `epers_immigration
    | 33l -> `epers_listepassenger
    | 34l -> `epers_militarydistinction
    | 35l -> `epers_militarypromotion
    | 36l -> `epers_militaryservice
    | 37l -> `epers_mobilisationmilitaire
    | 38l -> `epers_naturalisation
    | 39l -> `epers_occupation
    | 40l -> `epers_ordination
    | 41l -> `epers_property
    | 42l -> `epers_recensement
    | 43l -> `epers_residence
    | 44l -> `epers_retired
    | 45l -> `epers_scellentchildlds
    | 46l -> `epers_scellentparentlds
    | 47l -> `epers_scellentspouselds
    | 48l -> `epers_ventebien
    | 49l -> `epers_will
    | x -> Piqirun.error_enum_const x
and packed_parse_pevent_name x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `epers_birth
    | 1l -> `epers_baptism
    | 2l -> `epers_death
    | 3l -> `epers_burial
    | 4l -> `epers_cremation
    | 5l -> `epers_accomplishment
    | 6l -> `epers_acquisition
    | 7l -> `epers_adhesion
    | 8l -> `epers_baptismlds
    | 9l -> `epers_barmitzvah
    | 10l -> `epers_batmitzvah
    | 11l -> `epers_benediction
    | 12l -> `epers_changename
    | 13l -> `epers_circumcision
    | 14l -> `epers_confirmation
    | 15l -> `epers_confirmationlds
    | 16l -> `epers_decoration
    | 17l -> `epers_demobilisationmilitaire
    | 18l -> `epers_diploma
    | 19l -> `epers_distinction
    | 20l -> `epers_dotation
    | 21l -> `epers_dotationlds
    | 22l -> `epers_education
    | 23l -> `epers_election
    | 24l -> `epers_emigration
    | 25l -> `epers_excommunication
    | 26l -> `epers_familylinklds
    | 27l -> `epers_firstcommunion
    | 28l -> `epers_funeral
    | 29l -> `epers_graduate
    | 30l -> `epers_hospitalisation
    | 31l -> `epers_illness
    | 32l -> `epers_immigration
    | 33l -> `epers_listepassenger
    | 34l -> `epers_militarydistinction
    | 35l -> `epers_militarypromotion
    | 36l -> `epers_militaryservice
    | 37l -> `epers_mobilisationmilitaire
    | 38l -> `epers_naturalisation
    | 39l -> `epers_occupation
    | 40l -> `epers_ordination
    | 41l -> `epers_property
    | 42l -> `epers_recensement
    | 43l -> `epers_residence
    | 44l -> `epers_retired
    | 45l -> `epers_scellentchildlds
    | 46l -> `epers_scellentparentlds
    | 47l -> `epers_scellentspouselds
    | 48l -> `epers_ventebien
    | 49l -> `epers_will
    | x -> Piqirun.error_enum_const x

and parse_access x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `access_iftitles
    | 1l -> `access_public
    | 2l -> `access_private
    | x -> Piqirun.error_enum_const x
and packed_parse_access x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `access_iftitles
    | 1l -> `access_public
    | 2l -> `access_private
    | x -> Piqirun.error_enum_const x

and parse_update_warning_js x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `empty_index
    | 1l -> `empty_surname
    | 2l -> `empty_first_name
    | 3l -> `empty_sex
    | 4l -> `required_field
    | 5l -> `birth_date_after_event
    | 6l -> `death_date_before_event
    | x -> Piqirun.error_enum_const x
and packed_parse_update_warning_js x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `empty_index
    | 1l -> `empty_surname
    | 2l -> `empty_first_name
    | 3l -> `empty_sex
    | 4l -> `required_field
    | 5l -> `birth_date_after_event
    | 6l -> `death_date_before_event
    | x -> Piqirun.error_enum_const x

and parse_person_or_family x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `person_form1
    | 1l -> `person_form2
    | 2l -> `family_form
    | x -> Piqirun.error_enum_const x
and packed_parse_person_or_family x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `person_form1
    | 1l -> `person_form2
    | 2l -> `family_form
    | x -> Piqirun.error_enum_const x

and parse_auto_complete_place_field x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `subdivision
    | 1l -> `town
    | 2l -> `area_code
    | 3l -> `county
    | 4l -> `region
    | 5l -> `country
    | x -> Piqirun.error_enum_const x
and packed_parse_auto_complete_place_field x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `subdivision
    | 1l -> `town
    | 2l -> `area_code
    | 3l -> `county
    | 4l -> `region
    | 5l -> `country
    | x -> Piqirun.error_enum_const x

and parse_auto_complete_field x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `lastname
    | 1l -> `firstname
    | 2l -> `place
    | 3l -> `source
    | x -> Piqirun.error_enum_const x
and packed_parse_auto_complete_field x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `lastname
    | 1l -> `firstname
    | 2l -> `place
    | 3l -> `source
    | x -> Piqirun.error_enum_const x

and parse_short_greg_month x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `janv
    | 1l -> `fevr
    | 2l -> `mars
    | 3l -> `avr
    | 4l -> `mai
    | 5l -> `juin
    | 6l -> `juil
    | 7l -> `aout
    | 8l -> `sept
    | 9l -> `oct
    | 10l -> `nov
    | 11l -> `dec
    | x -> Piqirun.error_enum_const x
and packed_parse_short_greg_month x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `janv
    | 1l -> `fevr
    | 2l -> `mars
    | 3l -> `avr
    | 4l -> `mai
    | 5l -> `juin
    | 6l -> `juil
    | 7l -> `aout
    | 8l -> `sept
    | 9l -> `oct
    | 10l -> `nov
    | 11l -> `dec
    | x -> Piqirun.error_enum_const x

and parse_french_month x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `vendemiaire
    | 1l -> `brumaire
    | 2l -> `frimaire
    | 3l -> `nivose
    | 4l -> `pluviose
    | 5l -> `ventose
    | 6l -> `germinal
    | 7l -> `floreal
    | 8l -> `prairial
    | 9l -> `messidor
    | 10l -> `thermidor
    | 11l -> `fructidor
    | 12l -> `complementaire
    | x -> Piqirun.error_enum_const x
and packed_parse_french_month x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `vendemiaire
    | 1l -> `brumaire
    | 2l -> `frimaire
    | 3l -> `nivose
    | 4l -> `pluviose
    | 5l -> `ventose
    | 6l -> `germinal
    | 7l -> `floreal
    | 8l -> `prairial
    | 9l -> `messidor
    | 10l -> `thermidor
    | 11l -> `fructidor
    | 12l -> `complementaire
    | x -> Piqirun.error_enum_const x

and parse_hebrew_month x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `tichri
    | 1l -> `marhechvan
    | 2l -> `kislev
    | 3l -> `tevet
    | 4l -> `chevat
    | 5l -> `adar_1
    | 6l -> `adar_2
    | 7l -> `nissan
    | 8l -> `iyar
    | 9l -> `sivan
    | 10l -> `tamouz
    | 11l -> `av
    | 12l -> `eloul
    | x -> Piqirun.error_enum_const x
and packed_parse_hebrew_month x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `tichri
    | 1l -> `marhechvan
    | 2l -> `kislev
    | 3l -> `tevet
    | 4l -> `chevat
    | 5l -> `adar_1
    | 6l -> `adar_2
    | 7l -> `nissan
    | 8l -> `iyar
    | 9l -> `sivan
    | 10l -> `tamouz
    | 11l -> `av
    | 12l -> `eloul
    | x -> Piqirun.error_enum_const x


let rec gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x

and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__dmy code x =
  let _day = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Dmy.day in
  let _month = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Dmy.month in
  let _year = Piqirun.gen_optional_field 3 gen__int32 x.Dmy.year in
  let _delta = Piqirun.gen_optional_field 4 gen__protobuf_int32 x.Dmy.delta in
  Piqirun.gen_record code (_day :: _month :: _year :: _delta :: [])

and gen__date code x =
  let _cal = Piqirun.gen_optional_field 1 gen__calendar x.Date.cal in
  let _prec = Piqirun.gen_optional_field 2 gen__precision x.Date.prec in
  let _dmy = Piqirun.gen_optional_field 3 gen__dmy x.Date.dmy in
  let _dmy2 = Piqirun.gen_optional_field 4 gen__dmy x.Date.dmy2 in
  let _text = Piqirun.gen_optional_field 5 gen__string x.Date.text in
  Piqirun.gen_record code (_cal :: _prec :: _dmy :: _dmy2 :: _text :: [])

and gen__person_search code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Person_search.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Person_search.sex in
  let _lastname = Piqirun.gen_required_field 3 gen__string x.Person_search.lastname in
  let _firstname = Piqirun.gen_required_field 4 gen__string x.Person_search.firstname in
  let _dates = Piqirun.gen_optional_field 5 gen__string x.Person_search.dates in
  let _image = Piqirun.gen_optional_field 6 gen__string x.Person_search.image in
  let _sosa = Piqirun.gen_required_field 7 gen__sosa x.Person_search.sosa in
  let _family = Piqirun.gen_required_field 8 gen__string x.Person_search.family in
  let _n = Piqirun.gen_required_field 9 gen__string x.Person_search.n in
  let _p = Piqirun.gen_required_field 10 gen__string x.Person_search.p in
  let _oc = Piqirun.gen_required_field 11 gen__protobuf_int32 x.Person_search.oc in
  Piqirun.gen_record code (_index :: _sex :: _lastname :: _firstname :: _dates :: _image :: _sosa :: _family :: _n :: _p :: _oc :: [])

and gen__simple_person code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Simple_person.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Simple_person.sex in
  let _lastname = Piqirun.gen_required_field 3 gen__string x.Simple_person.lastname in
  let _firstname = Piqirun.gen_required_field 4 gen__string x.Simple_person.firstname in
  let _birth_short_date = Piqirun.gen_optional_field 5 gen__string x.Simple_person.birth_short_date in
  let _birth_place = Piqirun.gen_optional_field 6 gen__string x.Simple_person.birth_place in
  let _death_short_date = Piqirun.gen_optional_field 7 gen__string x.Simple_person.death_short_date in
  let _death_place = Piqirun.gen_optional_field 8 gen__string x.Simple_person.death_place in
  let _image = Piqirun.gen_optional_field 9 gen__string x.Simple_person.image in
  let _sosa = Piqirun.gen_required_field 10 gen__sosa x.Simple_person.sosa in
  Piqirun.gen_record code (_index :: _sex :: _lastname :: _firstname :: _birth_short_date :: _birth_place :: _death_short_date :: _death_place :: _image :: _sosa :: [])

and gen__witness_event code x =
  let _witness_type = Piqirun.gen_required_field 1 gen__witness_type x.Witness_event.witness_type in
  let _witness = Piqirun.gen_required_field 2 gen__simple_person x.Witness_event.witness in
  Piqirun.gen_record code (_witness_type :: _witness :: [])

and gen__event code x =
  let _name = Piqirun.gen_required_field 1 gen__string x.Event.name in
  let _date = Piqirun.gen_optional_field 2 gen__string x.Event.date in
  let _date_conv = Piqirun.gen_optional_field 3 gen__string x.Event.date_conv in
  let _date_cal = Piqirun.gen_optional_field 4 gen__calendar x.Event.date_cal in
  let _place = Piqirun.gen_optional_field 5 gen__string x.Event.place in
  let _reason = Piqirun.gen_optional_field 6 gen__string x.Event.reason in
  let _note = Piqirun.gen_optional_field 7 gen__string x.Event.note in
  let _src = Piqirun.gen_optional_field 8 gen__string x.Event.src in
  let _spouse = Piqirun.gen_optional_field 9 gen__simple_person x.Event.spouse in
  let _witnesses = Piqirun.gen_repeated_field 10 gen__witness_event x.Event.witnesses in
  Piqirun.gen_record code (_name :: _date :: _date_conv :: _date_cal :: _place :: _reason :: _note :: _src :: _spouse :: _witnesses :: [])

and gen__relation_person code x =
  let _r_type = Piqirun.gen_required_field 1 gen__relation_type x.Relation_person.r_type in
  let _person = Piqirun.gen_required_field 2 gen__simple_person x.Relation_person.person in
  Piqirun.gen_record code (_r_type :: _person :: [])

and gen__was_witness code x =
  let _husband = Piqirun.gen_required_field 1 gen__string x.Was_witness.husband in
  let _wife = Piqirun.gen_required_field 2 gen__string x.Was_witness.wife in
  Piqirun.gen_record code (_husband :: _wife :: [])

and gen__person_search_info code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Person_search_info.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Person_search_info.sex in
  let _lastname = Piqirun.gen_required_field 3 gen__string x.Person_search_info.lastname in
  let _firstname = Piqirun.gen_required_field 4 gen__string x.Person_search_info.firstname in
  let _public_name = Piqirun.gen_optional_field 5 gen__string x.Person_search_info.public_name in
  let _aliases = Piqirun.gen_repeated_field 6 gen__string x.Person_search_info.aliases in
  let _qualifiers = Piqirun.gen_repeated_field 7 gen__string x.Person_search_info.qualifiers in
  let _firstname_aliases = Piqirun.gen_repeated_field 8 gen__string x.Person_search_info.firstname_aliases in
  let _surname_aliases = Piqirun.gen_repeated_field 9 gen__string x.Person_search_info.surname_aliases in
  let _image = Piqirun.gen_optional_field 10 gen__string x.Person_search_info.image in
  let _events = Piqirun.gen_repeated_field 11 gen__event x.Person_search_info.events in
  let _occupation = Piqirun.gen_optional_field 12 gen__string x.Person_search_info.occupation in
  let _notes = Piqirun.gen_optional_field 13 gen__string x.Person_search_info.notes in
  let _psources = Piqirun.gen_optional_field 14 gen__string x.Person_search_info.psources in
  let _has_sources = Piqirun.gen_required_field 15 gen__bool x.Person_search_info.has_sources in
  let _titles = Piqirun.gen_repeated_field 16 gen__string x.Person_search_info.titles in
  let _related = Piqirun.gen_repeated_field 17 gen__relation_person x.Person_search_info.related in
  let _rparents = Piqirun.gen_repeated_field 18 gen__relation_person x.Person_search_info.rparents in
  let _was_witness = Piqirun.gen_repeated_field 19 gen__was_witness x.Person_search_info.was_witness in
  let _sosa = Piqirun.gen_required_field 20 gen__sosa x.Person_search_info.sosa in
  Piqirun.gen_record code (_index :: _sex :: _lastname :: _firstname :: _public_name :: _aliases :: _qualifiers :: _firstname_aliases :: _surname_aliases :: _image :: _events :: _occupation :: _notes :: _psources :: _has_sources :: _titles :: _related :: _rparents :: _was_witness :: _sosa :: [])

and gen__person_link code x =
  let _create_link = Piqirun.gen_required_field 1 gen__create_or_link x.Person_link.create_link in
  let _index = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Person_link.index in
  let _sex = Piqirun.gen_required_field 3 gen__sex x.Person_link.sex in
  let _lastname = Piqirun.gen_required_field 4 gen__string x.Person_link.lastname in
  let _firstname = Piqirun.gen_required_field 5 gen__string x.Person_link.firstname in
  let _occ = Piqirun.gen_optional_field 6 gen__protobuf_int32 x.Person_link.occ in
  let _dates = Piqirun.gen_optional_field 7 gen__string x.Person_link.dates in
  Piqirun.gen_record code (_create_link :: _index :: _sex :: _lastname :: _firstname :: _occ :: _dates :: [])

and gen__witness code x =
  let _witness_type = Piqirun.gen_required_field 1 gen__witness_type x.Witness.witness_type in
  let _person = Piqirun.gen_optional_field 2 gen__person_link x.Witness.person in
  Piqirun.gen_record code (_witness_type :: _person :: [])

and gen__fevent code x =
  let _fevent_type = Piqirun.gen_optional_field 1 gen__fevent_name x.Fevent.fevent_type in
  let _date = Piqirun.gen_optional_field 2 gen__date x.Fevent.date in
  let _place = Piqirun.gen_optional_field 3 gen__string x.Fevent.place in
  let _reason = Piqirun.gen_optional_field 4 gen__string x.Fevent.reason in
  let _note = Piqirun.gen_optional_field 5 gen__string x.Fevent.note in
  let _src = Piqirun.gen_optional_field 6 gen__string x.Fevent.src in
  let _witnesses = Piqirun.gen_repeated_field 7 gen__witness x.Fevent.witnesses in
  let _event_perso = Piqirun.gen_optional_field 8 gen__string x.Fevent.event_perso in
  Piqirun.gen_record code (_fevent_type :: _date :: _place :: _reason :: _note :: _src :: _witnesses :: _event_perso :: [])

and gen__relation_parent code x =
  let _rpt_type = Piqirun.gen_required_field 1 gen__relation_parent_type x.Relation_parent.rpt_type in
  let _person = Piqirun.gen_optional_field 2 gen__person_link x.Relation_parent.person in
  let _source = Piqirun.gen_optional_field 3 gen__string x.Relation_parent.source in
  Piqirun.gen_record code (_rpt_type :: _person :: _source :: [])

and gen__title code x =
  let _name = Piqirun.gen_optional_field 1 gen__string x.Title.name in
  let _title = Piqirun.gen_optional_field 2 gen__string x.Title.title in
  let _fief = Piqirun.gen_optional_field 3 gen__string x.Title.fief in
  let _date_begin = Piqirun.gen_optional_field 4 gen__date x.Title.date_begin in
  let _date_end = Piqirun.gen_optional_field 5 gen__date x.Title.date_end in
  let _nth = Piqirun.gen_optional_field 6 gen__protobuf_int32 x.Title.nth in
  Piqirun.gen_record code (_name :: _title :: _fief :: _date_begin :: _date_end :: _nth :: [])

and gen__pevent code x =
  let _pevent_type = Piqirun.gen_optional_field 1 gen__pevent_name x.Pevent.pevent_type in
  let _date = Piqirun.gen_optional_field 2 gen__date x.Pevent.date in
  let _place = Piqirun.gen_optional_field 3 gen__string x.Pevent.place in
  let _reason = Piqirun.gen_optional_field 4 gen__string x.Pevent.reason in
  let _note = Piqirun.gen_optional_field 5 gen__string x.Pevent.note in
  let _src = Piqirun.gen_optional_field 6 gen__string x.Pevent.src in
  let _witnesses = Piqirun.gen_repeated_field 7 gen__witness x.Pevent.witnesses in
  let _event_perso = Piqirun.gen_optional_field 8 gen__string x.Pevent.event_perso in
  Piqirun.gen_record code (_pevent_type :: _date :: _place :: _reason :: _note :: _src :: _witnesses :: _event_perso :: [])

and gen__person code x =
  let _digest = Piqirun.gen_required_field 1 gen__string x.Person.digest in
  let _create_link = Piqirun.gen_required_field 2 gen__create_or_link x.Person.create_link in
  let _index = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Person.index in
  let _sex = Piqirun.gen_required_field 4 gen__sex x.Person.sex in
  let _lastname = Piqirun.gen_required_field 5 gen__string x.Person.lastname in
  let _firstname = Piqirun.gen_required_field 6 gen__string x.Person.firstname in
  let _occ = Piqirun.gen_optional_field 7 gen__protobuf_int32 x.Person.occ in
  let _public_name = Piqirun.gen_optional_field 8 gen__string x.Person.public_name in
  let _aliases = Piqirun.gen_repeated_field 9 gen__string x.Person.aliases in
  let _qualifiers = Piqirun.gen_repeated_field 10 gen__string x.Person.qualifiers in
  let _firstname_aliases = Piqirun.gen_repeated_field 11 gen__string x.Person.firstname_aliases in
  let _surname_aliases = Piqirun.gen_repeated_field 12 gen__string x.Person.surname_aliases in
  let _image = Piqirun.gen_optional_field 13 gen__string x.Person.image in
  let _death_type = Piqirun.gen_required_field 14 gen__death_type x.Person.death_type in
  let _occupation = Piqirun.gen_optional_field 15 gen__string x.Person.occupation in
  let _psources = Piqirun.gen_optional_field 16 gen__string x.Person.psources in
  let _notes = Piqirun.gen_optional_field 17 gen__string x.Person.notes in
  let _titles = Piqirun.gen_repeated_field 18 gen__title x.Person.titles in
  let _pevents = Piqirun.gen_repeated_field 19 gen__pevent x.Person.pevents in
  let _related = Piqirun.gen_repeated_field 20 gen__protobuf_int32 x.Person.related in
  let _rparents = Piqirun.gen_repeated_field 21 gen__relation_parent x.Person.rparents in
  let _access = Piqirun.gen_required_field 22 gen__access x.Person.access in
  let _parents = Piqirun.gen_optional_field 23 gen__protobuf_int32 x.Person.parents in
  let _families = Piqirun.gen_repeated_field 24 gen__protobuf_int32 x.Person.families in
  Piqirun.gen_record code (_digest :: _create_link :: _index :: _sex :: _lastname :: _firstname :: _occ :: _public_name :: _aliases :: _qualifiers :: _firstname_aliases :: _surname_aliases :: _image :: _death_type :: _occupation :: _psources :: _notes :: _titles :: _pevents :: _related :: _rparents :: _access :: _parents :: _families :: [])

and gen__family code x =
  let _digest = Piqirun.gen_required_field 1 gen__string x.Family.digest in
  let _index = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Family.index in
  let _fevents = Piqirun.gen_repeated_field 3 gen__fevent x.Family.fevents in
  let _fsources = Piqirun.gen_optional_field 4 gen__string x.Family.fsources in
  let _origin_file = Piqirun.gen_optional_field 5 gen__string x.Family.origin_file in
  let _comment = Piqirun.gen_optional_field 6 gen__string x.Family.comment in
  let _father = Piqirun.gen_required_field 7 gen__person x.Family.father in
  let _mother = Piqirun.gen_required_field 8 gen__person x.Family.mother in
  let _children = Piqirun.gen_repeated_field 9 gen__person_link x.Family.children in
  let _old_witnesses = Piqirun.gen_repeated_field 10 gen__protobuf_int32 x.Family.old_witnesses in
  Piqirun.gen_record code (_digest :: _index :: _fevents :: _fsources :: _origin_file :: _comment :: _father :: _mother :: _children :: _old_witnesses :: [])

and gen__create_conflict code x =
  let _form = Piqirun.gen_optional_field 1 gen__person_or_family x.Create_conflict.form in
  let _witness = Piqirun.gen_required_field 2 gen__bool x.Create_conflict.witness in
  let _rparents = Piqirun.gen_required_field 3 gen__bool x.Create_conflict.rparents in
  let _event = Piqirun.gen_required_field 4 gen__bool x.Create_conflict.event in
  let _pos = Piqirun.gen_optional_field 5 gen__protobuf_int32 x.Create_conflict.pos in
  let _pos_witness = Piqirun.gen_optional_field 6 gen__protobuf_int32 x.Create_conflict.pos_witness in
  let _lastname = Piqirun.gen_required_field 7 gen__string x.Create_conflict.lastname in
  let _firstname = Piqirun.gen_required_field 8 gen__string x.Create_conflict.firstname in
  Piqirun.gen_record code (_form :: _witness :: _rparents :: _event :: _pos :: _pos_witness :: _lastname :: _firstname :: [])

and gen__modification_status code x =
  let _is_base_updated = Piqirun.gen_required_field 1 gen__bool x.Modification_status.is_base_updated in
  let _base_warnings = Piqirun.gen_repeated_field 2 gen__string x.Modification_status.base_warnings in
  let _index_person = Piqirun.gen_optional_field 3 gen__protobuf_int32 x.Modification_status.index_person in
  let _lastname = Piqirun.gen_required_field 4 gen__string x.Modification_status.lastname in
  let _firstname = Piqirun.gen_required_field 5 gen__string x.Modification_status.firstname in
  let _occ = Piqirun.gen_optional_field 6 gen__protobuf_int32 x.Modification_status.occ in
  let _index_family = Piqirun.gen_optional_field 7 gen__protobuf_int32 x.Modification_status.index_family in
  let _conflict = Piqirun.gen_optional_field 8 gen__create_conflict x.Modification_status.conflict in
  let _lastname_str = Piqirun.gen_optional_field 9 gen__string x.Modification_status.lastname_str in
  let _firstname_str = Piqirun.gen_optional_field 10 gen__string x.Modification_status.firstname_str in
  let _n = Piqirun.gen_optional_field 11 gen__string x.Modification_status.n in
  let _p = Piqirun.gen_optional_field 12 gen__string x.Modification_status.p in
  let _base_miscs = Piqirun.gen_repeated_field 13 gen__string x.Modification_status.base_miscs in
  Piqirun.gen_record code (_is_base_updated :: _base_warnings :: _index_person :: _lastname :: _firstname :: _occ :: _index_family :: _conflict :: _lastname_str :: _firstname_str :: _n :: _p :: _base_miscs :: [])

and gen__index_person code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Index_person.index in
  Piqirun.gen_record code (_index :: [])

and gen__index_family code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Index_family.index in
  Piqirun.gen_record code (_index :: [])

and gen__index_person_and_family code x =
  let _index_person = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Index_person_and_family.index_person in
  let _index_family = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Index_person_and_family.index_family in
  Piqirun.gen_record code (_index_person :: _index_family :: [])

and gen__family_spouse code x =
  let _index_family = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Family_spouse.index_family in
  let _index_person = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Family_spouse.index_person in
  let _sex = Piqirun.gen_required_field 3 gen__sex x.Family_spouse.sex in
  let _lastname = Piqirun.gen_required_field 4 gen__string x.Family_spouse.lastname in
  let _firstname = Piqirun.gen_required_field 5 gen__string x.Family_spouse.firstname in
  let _dates = Piqirun.gen_optional_field 6 gen__string x.Family_spouse.dates in
  let _image = Piqirun.gen_optional_field 7 gen__string x.Family_spouse.image in
  let _sosa = Piqirun.gen_required_field 8 gen__sosa x.Family_spouse.sosa in
  Piqirun.gen_record code (_index_family :: _index_person :: _sex :: _lastname :: _firstname :: _dates :: _image :: _sosa :: [])

and gen__add_child_request code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Add_child_request.index in
  let _index_family = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Add_child_request.index_family in
  let _sex = Piqirun.gen_optional_field 3 gen__sex x.Add_child_request.sex in
  Piqirun.gen_record code (_index :: _index_family :: _sex :: [])

and gen__add_child code x =
  let _person_lastname = Piqirun.gen_required_field 1 gen__string x.Add_child.person_lastname in
  let _person_firstname = Piqirun.gen_required_field 2 gen__string x.Add_child.person_firstname in
  let _family_spouse = Piqirun.gen_repeated_field 3 gen__family_spouse x.Add_child.family_spouse in
  let _child = Piqirun.gen_required_field 4 gen__person x.Add_child.child in
  Piqirun.gen_record code (_person_lastname :: _person_firstname :: _family_spouse :: _child :: [])

and gen__add_child_ok code x =
  let _index_person = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Add_child_ok.index_person in
  let _index_family = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Add_child_ok.index_family in
  let _new_family = Piqirun.gen_required_field 3 gen__bool x.Add_child_ok.new_family in
  let _child = Piqirun.gen_required_field 4 gen__person x.Add_child_ok.child in
  Piqirun.gen_record code (_index_person :: _index_family :: _new_family :: _child :: [])

and gen__add_parents code x =
  let _person_lastname = Piqirun.gen_required_field 1 gen__string x.Add_parents.person_lastname in
  let _person_firstname = Piqirun.gen_required_field 2 gen__string x.Add_parents.person_firstname in
  let _family = Piqirun.gen_required_field 3 gen__family x.Add_parents.family in
  Piqirun.gen_record code (_person_lastname :: _person_firstname :: _family :: [])

and gen__add_parents_ok code x =
  let _index_person = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Add_parents_ok.index_person in
  let _family = Piqirun.gen_required_field 2 gen__family x.Add_parents_ok.family in
  Piqirun.gen_record code (_index_person :: _family :: [])

and gen__add_family code x =
  let _person_lastname = Piqirun.gen_required_field 1 gen__string x.Add_family.person_lastname in
  let _person_firstname = Piqirun.gen_required_field 2 gen__string x.Add_family.person_firstname in
  let _family = Piqirun.gen_required_field 3 gen__family x.Add_family.family in
  Piqirun.gen_record code (_person_lastname :: _person_firstname :: _family :: [])

and gen__add_family_ok code x =
  let _index_person = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Add_family_ok.index_person in
  let _family = Piqirun.gen_required_field 2 gen__family x.Add_family_ok.family in
  Piqirun.gen_record code (_index_person :: _family :: [])

and gen__edit_family_request code x =
  let _spouses = Piqirun.gen_repeated_field 1 gen__family_spouse x.Edit_family_request.spouses in
  let _first_family = Piqirun.gen_optional_field 2 gen__edit_family x.Edit_family_request.first_family in
  Piqirun.gen_record code (_spouses :: _first_family :: [])

and gen__edit_family code x =
  let _person_lastname = Piqirun.gen_required_field 1 gen__string x.Edit_family.person_lastname in
  let _person_firstname = Piqirun.gen_required_field 2 gen__string x.Edit_family.person_firstname in
  let _family = Piqirun.gen_required_field 3 gen__family x.Edit_family.family in
  Piqirun.gen_record code (_person_lastname :: _person_firstname :: _family :: [])

and gen__edit_family_ok code x =
  let _index_person = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Edit_family_ok.index_person in
  let _family = Piqirun.gen_required_field 2 gen__family x.Edit_family_ok.family in
  Piqirun.gen_record code (_index_person :: _family :: [])

and gen__add_sibling_request code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Add_sibling_request.index in
  let _sex = Piqirun.gen_optional_field 2 gen__sex x.Add_sibling_request.sex in
  Piqirun.gen_record code (_index :: _sex :: [])

and gen__add_sibling code x =
  let _person_lastname = Piqirun.gen_required_field 1 gen__string x.Add_sibling.person_lastname in
  let _person_firstname = Piqirun.gen_required_field 2 gen__string x.Add_sibling.person_firstname in
  let _sibling = Piqirun.gen_required_field 3 gen__person x.Add_sibling.sibling in
  Piqirun.gen_record code (_person_lastname :: _person_firstname :: _sibling :: [])

and gen__add_sibling_ok code x =
  let _index_person = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Add_sibling_ok.index_person in
  let _sibling = Piqirun.gen_required_field 2 gen__person x.Add_sibling_ok.sibling in
  Piqirun.gen_record code (_index_person :: _sibling :: [])

and gen__add_first_fam code x =
  let _sosa = Piqirun.gen_required_field 1 gen__person x.Add_first_fam.sosa in
  let _father = Piqirun.gen_required_field 2 gen__person x.Add_first_fam.father in
  let _mother = Piqirun.gen_required_field 3 gen__person x.Add_first_fam.mother in
  let _spouse = Piqirun.gen_required_field 4 gen__person x.Add_first_fam.spouse in
  let _children = Piqirun.gen_repeated_field 5 gen__person x.Add_first_fam.children in
  Piqirun.gen_record code (_sosa :: _father :: _mother :: _spouse :: _children :: [])

and gen__auto_complete code x =
  let _field = Piqirun.gen_required_field 1 gen__auto_complete_field x.Auto_complete.field in
  let _place_field = Piqirun.gen_optional_field 2 gen__auto_complete_place_field x.Auto_complete.place_field in
  let _input = Piqirun.gen_required_field 3 gen__string x.Auto_complete.input in
  let _limit = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Auto_complete.limit in
  Piqirun.gen_record code (_field :: _place_field :: _input :: _limit :: [])

and gen__auto_complete_result code x =
  let _result = Piqirun.gen_repeated_field 1 gen__string x.Auto_complete_result.result in
  Piqirun.gen_record code (_result :: [])

and gen__person_search_list_params code x =
  let _lastname = Piqirun.gen_optional_field 1 gen__string x.Person_search_list_params.lastname in
  let _firstname = Piqirun.gen_optional_field 2 gen__string x.Person_search_list_params.firstname in
  let _limit = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Person_search_list_params.limit in
  Piqirun.gen_record code (_lastname :: _firstname :: _limit :: [])

and gen__person_search_list code x =
  let _persons = Piqirun.gen_repeated_field 1 gen__person_search x.Person_search_list.persons in
  Piqirun.gen_record code (_persons :: [])

and gen__transl_calendar code x =
  let _pos = Piqirun.gen_required_field 1 gen__calendar x.Transl_calendar.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_calendar.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_calendar code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_calendar x.Config_transl_calendar.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_witness_type code x =
  let _pos = Piqirun.gen_required_field 1 gen__witness_type x.Transl_witness_type.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_witness_type.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_witness_type code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_witness_type x.Config_transl_witness_type.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_precision code x =
  let _pos = Piqirun.gen_required_field 1 gen__precision x.Transl_precision.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_precision.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_precision code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_precision x.Config_transl_precision.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_death_type code x =
  let _pos = Piqirun.gen_required_field 1 gen__death_type x.Transl_death_type.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_death_type.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_death_type code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_death_type x.Config_transl_death_type.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_relation_parent_type code x =
  let _pos = Piqirun.gen_required_field 1 gen__relation_parent_type x.Transl_relation_parent_type.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_relation_parent_type.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_relation_parent_type code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_relation_parent_type x.Config_transl_relation_parent_type.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_fevent_name code x =
  let _pos = Piqirun.gen_required_field 1 gen__fevent_name x.Transl_fevent_name.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_fevent_name.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_fevent_name code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_fevent_name x.Config_transl_fevent_name.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_pevent_name code x =
  let _pos = Piqirun.gen_required_field 1 gen__pevent_name x.Transl_pevent_name.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_pevent_name.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_pevent_name code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_pevent_name x.Config_transl_pevent_name.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_access code x =
  let _pos = Piqirun.gen_required_field 1 gen__access x.Transl_access.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_access.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_access code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_access x.Config_transl_access.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_update_warning_js code x =
  let _pos = Piqirun.gen_required_field 1 gen__update_warning_js x.Transl_update_warning_js.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_update_warning_js.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_update_warning_js code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_update_warning_js x.Config_transl_update_warning_js.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_short_greg_month code x =
  let _pos = Piqirun.gen_required_field 1 gen__short_greg_month x.Transl_short_greg_month.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_short_greg_month.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_short_greg_month code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_short_greg_month x.Config_transl_short_greg_month.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_french_month code x =
  let _pos = Piqirun.gen_required_field 1 gen__french_month x.Transl_french_month.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_french_month.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_french_month code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_french_month x.Config_transl_french_month.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__transl_hebrew_month code x =
  let _pos = Piqirun.gen_required_field 1 gen__hebrew_month x.Transl_hebrew_month.pos in
  let _sval = Piqirun.gen_required_field 2 gen__string x.Transl_hebrew_month.sval in
  Piqirun.gen_record code (_pos :: _sval :: [])

and gen__config_transl_hebrew_month code x =
  let _msg = Piqirun.gen_repeated_field 1 gen__transl_hebrew_month x.Config_transl_hebrew_month.msg in
  Piqirun.gen_record code (_msg :: [])

and gen__config code x =
  let _transl_cal = Piqirun.gen_required_field 1 gen__config_transl_calendar x.Config.transl_cal in
  let _transl_wit = Piqirun.gen_required_field 2 gen__config_transl_witness_type x.Config.transl_wit in
  let _transl_prec = Piqirun.gen_required_field 3 gen__config_transl_precision x.Config.transl_prec in
  let _transl_death = Piqirun.gen_required_field 4 gen__config_transl_death_type x.Config.transl_death in
  let _transl_rel = Piqirun.gen_required_field 5 gen__config_transl_relation_parent_type x.Config.transl_rel in
  let _transl_fevents = Piqirun.gen_required_field 6 gen__config_transl_fevent_name x.Config.transl_fevents in
  let _transl_pevents = Piqirun.gen_required_field 7 gen__config_transl_pevent_name x.Config.transl_pevents in
  let _transl_access = Piqirun.gen_required_field 8 gen__config_transl_access x.Config.transl_access in
  let _transl_warning = Piqirun.gen_required_field 9 gen__config_transl_update_warning_js x.Config.transl_warning in
  let _transl_short_greg_month = Piqirun.gen_required_field 10 gen__config_transl_short_greg_month x.Config.transl_short_greg_month in
  let _transl_french_month = Piqirun.gen_required_field 11 gen__config_transl_french_month x.Config.transl_french_month in
  let _transl_hebrew_month = Piqirun.gen_required_field 12 gen__config_transl_hebrew_month x.Config.transl_hebrew_month in
  let _gwf_place_format = Piqirun.gen_required_field 13 gen__string x.Config.gwf_place_format in
  let _gwf_place_format_placeholder = Piqirun.gen_required_field 14 gen__string x.Config.gwf_place_format_placeholder in
  Piqirun.gen_record code (_transl_cal :: _transl_wit :: _transl_prec :: _transl_death :: _transl_rel :: _transl_fevents :: _transl_pevents :: _transl_access :: _transl_warning :: _transl_short_greg_month :: _transl_french_month :: _transl_hebrew_month :: _gwf_place_format :: _gwf_place_format_placeholder :: [])

and gen__sosa code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `sosa_ref -> 0l
    | `sosa -> 1l
    | `no_sosa -> 2l
  )
and packed_gen__sosa x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `sosa_ref -> 0l
    | `sosa -> 1l
    | `no_sosa -> 2l
  )

and gen__calendar code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `gregorian -> 0l
    | `julian -> 1l
    | `french -> 2l
    | `hebrew -> 3l
  )
and packed_gen__calendar x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `gregorian -> 0l
    | `julian -> 1l
    | `french -> 2l
    | `hebrew -> 3l
  )

and gen__witness_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `witness -> 0l
    | `witness_godparent -> 1l
  )
and packed_gen__witness_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `witness -> 0l
    | `witness_godparent -> 1l
  )

and gen__precision code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `sure -> 0l
    | `about -> 1l
    | `maybe -> 2l
    | `before -> 3l
    | `after -> 4l
    | `oryear -> 5l
    | `yearint -> 6l
  )
and packed_gen__precision x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `sure -> 0l
    | `about -> 1l
    | `maybe -> 2l
    | `before -> 3l
    | `after -> 4l
    | `oryear -> 5l
    | `yearint -> 6l
  )

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

and gen__relation_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `rparent_adoption -> 0l
    | `rparent_recognition -> 1l
    | `rparent_candidate_parent -> 2l
    | `rparent_god_parent -> 3l
    | `rparent_foster_parent -> 4l
    | `rchild_adoption -> 5l
    | `rchild_recognition -> 6l
    | `rchild_candidate_parent -> 7l
    | `rchild_god_parent -> 8l
    | `rchild_foster_parent -> 9l
  )
and packed_gen__relation_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `rparent_adoption -> 0l
    | `rparent_recognition -> 1l
    | `rparent_candidate_parent -> 2l
    | `rparent_god_parent -> 3l
    | `rparent_foster_parent -> 4l
    | `rchild_adoption -> 5l
    | `rchild_recognition -> 6l
    | `rchild_candidate_parent -> 7l
    | `rchild_god_parent -> 8l
    | `rchild_foster_parent -> 9l
  )

and gen__create_or_link code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `create -> 0l
    | `link -> 1l
    | `create_default_occ -> 2l
  )
and packed_gen__create_or_link x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `create -> 0l
    | `link -> 1l
    | `create_default_occ -> 2l
  )

and gen__fevent_name code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `efam_marriage -> 0l
    | `efam_no_marriage -> 1l
    | `efam_no_mention -> 2l
    | `efam_engage -> 3l
    | `efam_divorce -> 4l
    | `efam_separated -> 5l
    | `efam_annulation -> 6l
    | `efam_marriage_bann -> 7l
    | `efam_marriage_contract -> 8l
    | `efam_marriage_license -> 9l
    | `efam_pacs -> 10l
    | `efam_residence -> 11l
  )
and packed_gen__fevent_name x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `efam_marriage -> 0l
    | `efam_no_marriage -> 1l
    | `efam_no_mention -> 2l
    | `efam_engage -> 3l
    | `efam_divorce -> 4l
    | `efam_separated -> 5l
    | `efam_annulation -> 6l
    | `efam_marriage_bann -> 7l
    | `efam_marriage_contract -> 8l
    | `efam_marriage_license -> 9l
    | `efam_pacs -> 10l
    | `efam_residence -> 11l
  )

and gen__relation_parent_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `rpt_adoption_father -> 0l
    | `rpt_adoption_mother -> 1l
    | `rpt_recognition_father -> 2l
    | `rpt_recognition_mother -> 3l
    | `rpt_candidate_parent_father -> 4l
    | `rpt_candidate_parent_mother -> 5l
    | `rpt_god_parent_father -> 6l
    | `rpt_god_parent_mother -> 7l
    | `rpt_foster_parent_father -> 8l
    | `rpt_foster_parent_mother -> 9l
  )
and packed_gen__relation_parent_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `rpt_adoption_father -> 0l
    | `rpt_adoption_mother -> 1l
    | `rpt_recognition_father -> 2l
    | `rpt_recognition_mother -> 3l
    | `rpt_candidate_parent_father -> 4l
    | `rpt_candidate_parent_mother -> 5l
    | `rpt_god_parent_father -> 6l
    | `rpt_god_parent_mother -> 7l
    | `rpt_foster_parent_father -> 8l
    | `rpt_foster_parent_mother -> 9l
  )

and gen__pevent_name code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `epers_birth -> 0l
    | `epers_baptism -> 1l
    | `epers_death -> 2l
    | `epers_burial -> 3l
    | `epers_cremation -> 4l
    | `epers_accomplishment -> 5l
    | `epers_acquisition -> 6l
    | `epers_adhesion -> 7l
    | `epers_baptismlds -> 8l
    | `epers_barmitzvah -> 9l
    | `epers_batmitzvah -> 10l
    | `epers_benediction -> 11l
    | `epers_changename -> 12l
    | `epers_circumcision -> 13l
    | `epers_confirmation -> 14l
    | `epers_confirmationlds -> 15l
    | `epers_decoration -> 16l
    | `epers_demobilisationmilitaire -> 17l
    | `epers_diploma -> 18l
    | `epers_distinction -> 19l
    | `epers_dotation -> 20l
    | `epers_dotationlds -> 21l
    | `epers_education -> 22l
    | `epers_election -> 23l
    | `epers_emigration -> 24l
    | `epers_excommunication -> 25l
    | `epers_familylinklds -> 26l
    | `epers_firstcommunion -> 27l
    | `epers_funeral -> 28l
    | `epers_graduate -> 29l
    | `epers_hospitalisation -> 30l
    | `epers_illness -> 31l
    | `epers_immigration -> 32l
    | `epers_listepassenger -> 33l
    | `epers_militarydistinction -> 34l
    | `epers_militarypromotion -> 35l
    | `epers_militaryservice -> 36l
    | `epers_mobilisationmilitaire -> 37l
    | `epers_naturalisation -> 38l
    | `epers_occupation -> 39l
    | `epers_ordination -> 40l
    | `epers_property -> 41l
    | `epers_recensement -> 42l
    | `epers_residence -> 43l
    | `epers_retired -> 44l
    | `epers_scellentchildlds -> 45l
    | `epers_scellentparentlds -> 46l
    | `epers_scellentspouselds -> 47l
    | `epers_ventebien -> 48l
    | `epers_will -> 49l
  )
and packed_gen__pevent_name x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `epers_birth -> 0l
    | `epers_baptism -> 1l
    | `epers_death -> 2l
    | `epers_burial -> 3l
    | `epers_cremation -> 4l
    | `epers_accomplishment -> 5l
    | `epers_acquisition -> 6l
    | `epers_adhesion -> 7l
    | `epers_baptismlds -> 8l
    | `epers_barmitzvah -> 9l
    | `epers_batmitzvah -> 10l
    | `epers_benediction -> 11l
    | `epers_changename -> 12l
    | `epers_circumcision -> 13l
    | `epers_confirmation -> 14l
    | `epers_confirmationlds -> 15l
    | `epers_decoration -> 16l
    | `epers_demobilisationmilitaire -> 17l
    | `epers_diploma -> 18l
    | `epers_distinction -> 19l
    | `epers_dotation -> 20l
    | `epers_dotationlds -> 21l
    | `epers_education -> 22l
    | `epers_election -> 23l
    | `epers_emigration -> 24l
    | `epers_excommunication -> 25l
    | `epers_familylinklds -> 26l
    | `epers_firstcommunion -> 27l
    | `epers_funeral -> 28l
    | `epers_graduate -> 29l
    | `epers_hospitalisation -> 30l
    | `epers_illness -> 31l
    | `epers_immigration -> 32l
    | `epers_listepassenger -> 33l
    | `epers_militarydistinction -> 34l
    | `epers_militarypromotion -> 35l
    | `epers_militaryservice -> 36l
    | `epers_mobilisationmilitaire -> 37l
    | `epers_naturalisation -> 38l
    | `epers_occupation -> 39l
    | `epers_ordination -> 40l
    | `epers_property -> 41l
    | `epers_recensement -> 42l
    | `epers_residence -> 43l
    | `epers_retired -> 44l
    | `epers_scellentchildlds -> 45l
    | `epers_scellentparentlds -> 46l
    | `epers_scellentspouselds -> 47l
    | `epers_ventebien -> 48l
    | `epers_will -> 49l
  )

and gen__access code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `access_iftitles -> 0l
    | `access_public -> 1l
    | `access_private -> 2l
  )
and packed_gen__access x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `access_iftitles -> 0l
    | `access_public -> 1l
    | `access_private -> 2l
  )

and gen__update_warning_js code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `empty_index -> 0l
    | `empty_surname -> 1l
    | `empty_first_name -> 2l
    | `empty_sex -> 3l
    | `required_field -> 4l
    | `birth_date_after_event -> 5l
    | `death_date_before_event -> 6l
  )
and packed_gen__update_warning_js x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `empty_index -> 0l
    | `empty_surname -> 1l
    | `empty_first_name -> 2l
    | `empty_sex -> 3l
    | `required_field -> 4l
    | `birth_date_after_event -> 5l
    | `death_date_before_event -> 6l
  )

and gen__person_or_family code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `person_form1 -> 0l
    | `person_form2 -> 1l
    | `family_form -> 2l
  )
and packed_gen__person_or_family x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `person_form1 -> 0l
    | `person_form2 -> 1l
    | `family_form -> 2l
  )

and gen__auto_complete_place_field code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `subdivision -> 0l
    | `town -> 1l
    | `area_code -> 2l
    | `county -> 3l
    | `region -> 4l
    | `country -> 5l
  )
and packed_gen__auto_complete_place_field x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `subdivision -> 0l
    | `town -> 1l
    | `area_code -> 2l
    | `county -> 3l
    | `region -> 4l
    | `country -> 5l
  )

and gen__auto_complete_field code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `lastname -> 0l
    | `firstname -> 1l
    | `place -> 2l
    | `source -> 3l
  )
and packed_gen__auto_complete_field x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `lastname -> 0l
    | `firstname -> 1l
    | `place -> 2l
    | `source -> 3l
  )

and gen__short_greg_month code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `janv -> 0l
    | `fevr -> 1l
    | `mars -> 2l
    | `avr -> 3l
    | `mai -> 4l
    | `juin -> 5l
    | `juil -> 6l
    | `aout -> 7l
    | `sept -> 8l
    | `oct -> 9l
    | `nov -> 10l
    | `dec -> 11l
  )
and packed_gen__short_greg_month x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `janv -> 0l
    | `fevr -> 1l
    | `mars -> 2l
    | `avr -> 3l
    | `mai -> 4l
    | `juin -> 5l
    | `juil -> 6l
    | `aout -> 7l
    | `sept -> 8l
    | `oct -> 9l
    | `nov -> 10l
    | `dec -> 11l
  )

and gen__french_month code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `vendemiaire -> 0l
    | `brumaire -> 1l
    | `frimaire -> 2l
    | `nivose -> 3l
    | `pluviose -> 4l
    | `ventose -> 5l
    | `germinal -> 6l
    | `floreal -> 7l
    | `prairial -> 8l
    | `messidor -> 9l
    | `thermidor -> 10l
    | `fructidor -> 11l
    | `complementaire -> 12l
  )
and packed_gen__french_month x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `vendemiaire -> 0l
    | `brumaire -> 1l
    | `frimaire -> 2l
    | `nivose -> 3l
    | `pluviose -> 4l
    | `ventose -> 5l
    | `germinal -> 6l
    | `floreal -> 7l
    | `prairial -> 8l
    | `messidor -> 9l
    | `thermidor -> 10l
    | `fructidor -> 11l
    | `complementaire -> 12l
  )

and gen__hebrew_month code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `tichri -> 0l
    | `marhechvan -> 1l
    | `kislev -> 2l
    | `tevet -> 3l
    | `chevat -> 4l
    | `adar_1 -> 5l
    | `adar_2 -> 6l
    | `nissan -> 7l
    | `iyar -> 8l
    | `sivan -> 9l
    | `tamouz -> 10l
    | `av -> 11l
    | `eloul -> 12l
  )
and packed_gen__hebrew_month x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `tichri -> 0l
    | `marhechvan -> 1l
    | `kislev -> 2l
    | `tevet -> 3l
    | `chevat -> 4l
    | `adar_1 -> 5l
    | `adar_2 -> 6l
    | `nissan -> 7l
    | `iyar -> 8l
    | `sivan -> 9l
    | `tamouz -> 10l
    | `av -> 11l
    | `eloul -> 12l
  )


let gen_int32 x = gen__int32 (-1) x
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
let gen_string x = gen__string (-1) x
let gen_bool x = gen__bool (-1) x
let gen_dmy x = gen__dmy (-1) x
let gen_date x = gen__date (-1) x
let gen_person_search x = gen__person_search (-1) x
let gen_simple_person x = gen__simple_person (-1) x
let gen_witness_event x = gen__witness_event (-1) x
let gen_event x = gen__event (-1) x
let gen_relation_person x = gen__relation_person (-1) x
let gen_was_witness x = gen__was_witness (-1) x
let gen_person_search_info x = gen__person_search_info (-1) x
let gen_person_link x = gen__person_link (-1) x
let gen_witness x = gen__witness (-1) x
let gen_fevent x = gen__fevent (-1) x
let gen_relation_parent x = gen__relation_parent (-1) x
let gen_title x = gen__title (-1) x
let gen_pevent x = gen__pevent (-1) x
let gen_person x = gen__person (-1) x
let gen_family x = gen__family (-1) x
let gen_create_conflict x = gen__create_conflict (-1) x
let gen_modification_status x = gen__modification_status (-1) x
let gen_index_person x = gen__index_person (-1) x
let gen_index_family x = gen__index_family (-1) x
let gen_index_person_and_family x = gen__index_person_and_family (-1) x
let gen_family_spouse x = gen__family_spouse (-1) x
let gen_add_child_request x = gen__add_child_request (-1) x
let gen_add_child x = gen__add_child (-1) x
let gen_add_child_ok x = gen__add_child_ok (-1) x
let gen_add_parents x = gen__add_parents (-1) x
let gen_add_parents_ok x = gen__add_parents_ok (-1) x
let gen_add_family x = gen__add_family (-1) x
let gen_add_family_ok x = gen__add_family_ok (-1) x
let gen_edit_family_request x = gen__edit_family_request (-1) x
let gen_edit_family x = gen__edit_family (-1) x
let gen_edit_family_ok x = gen__edit_family_ok (-1) x
let gen_add_sibling_request x = gen__add_sibling_request (-1) x
let gen_add_sibling x = gen__add_sibling (-1) x
let gen_add_sibling_ok x = gen__add_sibling_ok (-1) x
let gen_add_first_fam x = gen__add_first_fam (-1) x
let gen_auto_complete x = gen__auto_complete (-1) x
let gen_auto_complete_result x = gen__auto_complete_result (-1) x
let gen_person_search_list_params x = gen__person_search_list_params (-1) x
let gen_person_search_list x = gen__person_search_list (-1) x
let gen_transl_calendar x = gen__transl_calendar (-1) x
let gen_config_transl_calendar x = gen__config_transl_calendar (-1) x
let gen_transl_witness_type x = gen__transl_witness_type (-1) x
let gen_config_transl_witness_type x = gen__config_transl_witness_type (-1) x
let gen_transl_precision x = gen__transl_precision (-1) x
let gen_config_transl_precision x = gen__config_transl_precision (-1) x
let gen_transl_death_type x = gen__transl_death_type (-1) x
let gen_config_transl_death_type x = gen__config_transl_death_type (-1) x
let gen_transl_relation_parent_type x = gen__transl_relation_parent_type (-1) x
let gen_config_transl_relation_parent_type x = gen__config_transl_relation_parent_type (-1) x
let gen_transl_fevent_name x = gen__transl_fevent_name (-1) x
let gen_config_transl_fevent_name x = gen__config_transl_fevent_name (-1) x
let gen_transl_pevent_name x = gen__transl_pevent_name (-1) x
let gen_config_transl_pevent_name x = gen__config_transl_pevent_name (-1) x
let gen_transl_access x = gen__transl_access (-1) x
let gen_config_transl_access x = gen__config_transl_access (-1) x
let gen_transl_update_warning_js x = gen__transl_update_warning_js (-1) x
let gen_config_transl_update_warning_js x = gen__config_transl_update_warning_js (-1) x
let gen_transl_short_greg_month x = gen__transl_short_greg_month (-1) x
let gen_config_transl_short_greg_month x = gen__config_transl_short_greg_month (-1) x
let gen_transl_french_month x = gen__transl_french_month (-1) x
let gen_config_transl_french_month x = gen__config_transl_french_month (-1) x
let gen_transl_hebrew_month x = gen__transl_hebrew_month (-1) x
let gen_config_transl_hebrew_month x = gen__config_transl_hebrew_month (-1) x
let gen_config x = gen__config (-1) x
let gen_sosa x = gen__sosa (-1) x
let gen_calendar x = gen__calendar (-1) x
let gen_witness_type x = gen__witness_type (-1) x
let gen_precision x = gen__precision (-1) x
let gen_sex x = gen__sex (-1) x
let gen_death_type x = gen__death_type (-1) x
let gen_relation_type x = gen__relation_type (-1) x
let gen_create_or_link x = gen__create_or_link (-1) x
let gen_fevent_name x = gen__fevent_name (-1) x
let gen_relation_parent_type x = gen__relation_parent_type (-1) x
let gen_pevent_name x = gen__pevent_name (-1) x
let gen_access x = gen__access (-1) x
let gen_update_warning_js x = gen__update_warning_js (-1) x
let gen_person_or_family x = gen__person_or_family (-1) x
let gen_auto_complete_place_field x = gen__auto_complete_place_field (-1) x
let gen_auto_complete_field x = gen__auto_complete_field (-1) x
let gen_short_greg_month x = gen__short_greg_month (-1) x
let gen_french_month x = gen__french_month (-1) x
let gen_hebrew_month x = gen__hebrew_month (-1) x


let rec default_int32 () = 0l
and default_protobuf_int32 () = default_int32 ()
and default_string () = ""
and default_bool () = false
and default_dmy () =
  {
    Dmy.day = None;
    Dmy.month = None;
    Dmy.year = None;
    Dmy.delta = None;
  }
and default_date () =
  {
    Date.cal = None;
    Date.prec = None;
    Date.dmy = None;
    Date.dmy2 = None;
    Date.text = None;
  }
and default_person_search () =
  {
    Person_search.index = default_protobuf_int32 ();
    Person_search.sex = default_sex ();
    Person_search.lastname = default_string ();
    Person_search.firstname = default_string ();
    Person_search.dates = None;
    Person_search.image = None;
    Person_search.sosa = default_sosa ();
    Person_search.family = default_string ();
    Person_search.n = default_string ();
    Person_search.p = default_string ();
    Person_search.oc = default_protobuf_int32 ();
  }
and default_simple_person () =
  {
    Simple_person.index = default_protobuf_int32 ();
    Simple_person.sex = default_sex ();
    Simple_person.lastname = default_string ();
    Simple_person.firstname = default_string ();
    Simple_person.birth_short_date = None;
    Simple_person.birth_place = None;
    Simple_person.death_short_date = None;
    Simple_person.death_place = None;
    Simple_person.image = None;
    Simple_person.sosa = default_sosa ();
  }
and default_witness_event () =
  {
    Witness_event.witness_type = default_witness_type ();
    Witness_event.witness = default_simple_person ();
  }
and default_event () =
  {
    Event.name = default_string ();
    Event.date = None;
    Event.date_conv = None;
    Event.date_cal = None;
    Event.place = None;
    Event.reason = None;
    Event.note = None;
    Event.src = None;
    Event.spouse = None;
    Event.witnesses = [];
  }
and default_relation_person () =
  {
    Relation_person.r_type = default_relation_type ();
    Relation_person.person = default_simple_person ();
  }
and default_was_witness () =
  {
    Was_witness.husband = default_string ();
    Was_witness.wife = default_string ();
  }
and default_person_search_info () =
  {
    Person_search_info.index = default_protobuf_int32 ();
    Person_search_info.sex = default_sex ();
    Person_search_info.lastname = default_string ();
    Person_search_info.firstname = default_string ();
    Person_search_info.public_name = None;
    Person_search_info.aliases = [];
    Person_search_info.qualifiers = [];
    Person_search_info.firstname_aliases = [];
    Person_search_info.surname_aliases = [];
    Person_search_info.image = None;
    Person_search_info.events = [];
    Person_search_info.occupation = None;
    Person_search_info.notes = None;
    Person_search_info.psources = None;
    Person_search_info.has_sources = default_bool ();
    Person_search_info.titles = [];
    Person_search_info.related = [];
    Person_search_info.rparents = [];
    Person_search_info.was_witness = [];
    Person_search_info.sosa = default_sosa ();
  }
and default_person_link () =
  {
    Person_link.create_link = default_create_or_link ();
    Person_link.index = default_protobuf_int32 ();
    Person_link.sex = default_sex ();
    Person_link.lastname = default_string ();
    Person_link.firstname = default_string ();
    Person_link.occ = None;
    Person_link.dates = None;
  }
and default_witness () =
  {
    Witness.witness_type = default_witness_type ();
    Witness.person = None;
  }
and default_fevent () =
  {
    Fevent.fevent_type = None;
    Fevent.date = None;
    Fevent.place = None;
    Fevent.reason = None;
    Fevent.note = None;
    Fevent.src = None;
    Fevent.witnesses = [];
    Fevent.event_perso = None;
  }
and default_relation_parent () =
  {
    Relation_parent.rpt_type = default_relation_parent_type ();
    Relation_parent.person = None;
    Relation_parent.source = None;
  }
and default_title () =
  {
    Title.name = None;
    Title.title = None;
    Title.fief = None;
    Title.date_begin = None;
    Title.date_end = None;
    Title.nth = None;
  }
and default_pevent () =
  {
    Pevent.pevent_type = None;
    Pevent.date = None;
    Pevent.place = None;
    Pevent.reason = None;
    Pevent.note = None;
    Pevent.src = None;
    Pevent.witnesses = [];
    Pevent.event_perso = None;
  }
and default_person () =
  {
    Person.digest = default_string ();
    Person.create_link = default_create_or_link ();
    Person.index = default_protobuf_int32 ();
    Person.sex = default_sex ();
    Person.lastname = default_string ();
    Person.firstname = default_string ();
    Person.occ = None;
    Person.public_name = None;
    Person.aliases = [];
    Person.qualifiers = [];
    Person.firstname_aliases = [];
    Person.surname_aliases = [];
    Person.image = None;
    Person.death_type = default_death_type ();
    Person.occupation = None;
    Person.psources = None;
    Person.notes = None;
    Person.titles = [];
    Person.pevents = [];
    Person.related = [];
    Person.rparents = [];
    Person.access = parse_access (Piqirun.parse_default "\b\000");
    Person.parents = None;
    Person.families = [];
  }
and default_family () =
  {
    Family.digest = default_string ();
    Family.index = default_protobuf_int32 ();
    Family.fevents = [];
    Family.fsources = None;
    Family.origin_file = None;
    Family.comment = None;
    Family.father = default_person ();
    Family.mother = default_person ();
    Family.children = [];
    Family.old_witnesses = [];
  }
and default_create_conflict () =
  {
    Create_conflict.form = None;
    Create_conflict.witness = default_bool ();
    Create_conflict.rparents = default_bool ();
    Create_conflict.event = default_bool ();
    Create_conflict.pos = None;
    Create_conflict.pos_witness = None;
    Create_conflict.lastname = default_string ();
    Create_conflict.firstname = default_string ();
  }
and default_modification_status () =
  {
    Modification_status.is_base_updated = default_bool ();
    Modification_status.base_warnings = [];
    Modification_status.index_person = None;
    Modification_status.lastname = default_string ();
    Modification_status.firstname = default_string ();
    Modification_status.occ = None;
    Modification_status.index_family = None;
    Modification_status.conflict = None;
    Modification_status.lastname_str = None;
    Modification_status.firstname_str = None;
    Modification_status.n = None;
    Modification_status.p = None;
    Modification_status.base_miscs = [];
  }
and default_index_person () =
  {
    Index_person.index = default_protobuf_int32 ();
  }
and default_index_family () =
  {
    Index_family.index = default_protobuf_int32 ();
  }
and default_index_person_and_family () =
  {
    Index_person_and_family.index_person = default_protobuf_int32 ();
    Index_person_and_family.index_family = default_protobuf_int32 ();
  }
and default_family_spouse () =
  {
    Family_spouse.index_family = default_protobuf_int32 ();
    Family_spouse.index_person = default_protobuf_int32 ();
    Family_spouse.sex = default_sex ();
    Family_spouse.lastname = default_string ();
    Family_spouse.firstname = default_string ();
    Family_spouse.dates = None;
    Family_spouse.image = None;
    Family_spouse.sosa = default_sosa ();
  }
and default_add_child_request () =
  {
    Add_child_request.index = default_protobuf_int32 ();
    Add_child_request.index_family = None;
    Add_child_request.sex = None;
  }
and default_add_child () =
  {
    Add_child.person_lastname = default_string ();
    Add_child.person_firstname = default_string ();
    Add_child.family_spouse = [];
    Add_child.child = default_person ();
  }
and default_add_child_ok () =
  {
    Add_child_ok.index_person = default_protobuf_int32 ();
    Add_child_ok.index_family = default_protobuf_int32 ();
    Add_child_ok.new_family = default_bool ();
    Add_child_ok.child = default_person ();
  }
and default_add_parents () =
  {
    Add_parents.person_lastname = default_string ();
    Add_parents.person_firstname = default_string ();
    Add_parents.family = default_family ();
  }
and default_add_parents_ok () =
  {
    Add_parents_ok.index_person = default_protobuf_int32 ();
    Add_parents_ok.family = default_family ();
  }
and default_add_family () =
  {
    Add_family.person_lastname = default_string ();
    Add_family.person_firstname = default_string ();
    Add_family.family = default_family ();
  }
and default_add_family_ok () =
  {
    Add_family_ok.index_person = default_protobuf_int32 ();
    Add_family_ok.family = default_family ();
  }
and default_edit_family_request () =
  {
    Edit_family_request.spouses = [];
    Edit_family_request.first_family = None;
  }
and default_edit_family () =
  {
    Edit_family.person_lastname = default_string ();
    Edit_family.person_firstname = default_string ();
    Edit_family.family = default_family ();
  }
and default_edit_family_ok () =
  {
    Edit_family_ok.index_person = default_protobuf_int32 ();
    Edit_family_ok.family = default_family ();
  }
and default_add_sibling_request () =
  {
    Add_sibling_request.index = default_protobuf_int32 ();
    Add_sibling_request.sex = None;
  }
and default_add_sibling () =
  {
    Add_sibling.person_lastname = default_string ();
    Add_sibling.person_firstname = default_string ();
    Add_sibling.sibling = default_person ();
  }
and default_add_sibling_ok () =
  {
    Add_sibling_ok.index_person = default_protobuf_int32 ();
    Add_sibling_ok.sibling = default_person ();
  }
and default_add_first_fam () =
  {
    Add_first_fam.sosa = default_person ();
    Add_first_fam.father = default_person ();
    Add_first_fam.mother = default_person ();
    Add_first_fam.spouse = default_person ();
    Add_first_fam.children = [];
  }
and default_auto_complete () =
  {
    Auto_complete.field = default_auto_complete_field ();
    Auto_complete.place_field = None;
    Auto_complete.input = default_string ();
    Auto_complete.limit = default_protobuf_int32 ();
  }
and default_auto_complete_result () =
  {
    Auto_complete_result.result = [];
  }
and default_person_search_list_params () =
  {
    Person_search_list_params.lastname = None;
    Person_search_list_params.firstname = None;
    Person_search_list_params.limit = default_protobuf_int32 ();
  }
and default_person_search_list () =
  {
    Person_search_list.persons = [];
  }
and default_transl_calendar () =
  {
    Transl_calendar.pos = default_calendar ();
    Transl_calendar.sval = default_string ();
  }
and default_config_transl_calendar () =
  {
    Config_transl_calendar.msg = [];
  }
and default_transl_witness_type () =
  {
    Transl_witness_type.pos = default_witness_type ();
    Transl_witness_type.sval = default_string ();
  }
and default_config_transl_witness_type () =
  {
    Config_transl_witness_type.msg = [];
  }
and default_transl_precision () =
  {
    Transl_precision.pos = default_precision ();
    Transl_precision.sval = default_string ();
  }
and default_config_transl_precision () =
  {
    Config_transl_precision.msg = [];
  }
and default_transl_death_type () =
  {
    Transl_death_type.pos = default_death_type ();
    Transl_death_type.sval = default_string ();
  }
and default_config_transl_death_type () =
  {
    Config_transl_death_type.msg = [];
  }
and default_transl_relation_parent_type () =
  {
    Transl_relation_parent_type.pos = default_relation_parent_type ();
    Transl_relation_parent_type.sval = default_string ();
  }
and default_config_transl_relation_parent_type () =
  {
    Config_transl_relation_parent_type.msg = [];
  }
and default_transl_fevent_name () =
  {
    Transl_fevent_name.pos = default_fevent_name ();
    Transl_fevent_name.sval = default_string ();
  }
and default_config_transl_fevent_name () =
  {
    Config_transl_fevent_name.msg = [];
  }
and default_transl_pevent_name () =
  {
    Transl_pevent_name.pos = default_pevent_name ();
    Transl_pevent_name.sval = default_string ();
  }
and default_config_transl_pevent_name () =
  {
    Config_transl_pevent_name.msg = [];
  }
and default_transl_access () =
  {
    Transl_access.pos = default_access ();
    Transl_access.sval = default_string ();
  }
and default_config_transl_access () =
  {
    Config_transl_access.msg = [];
  }
and default_transl_update_warning_js () =
  {
    Transl_update_warning_js.pos = default_update_warning_js ();
    Transl_update_warning_js.sval = default_string ();
  }
and default_config_transl_update_warning_js () =
  {
    Config_transl_update_warning_js.msg = [];
  }
and default_transl_short_greg_month () =
  {
    Transl_short_greg_month.pos = default_short_greg_month ();
    Transl_short_greg_month.sval = default_string ();
  }
and default_config_transl_short_greg_month () =
  {
    Config_transl_short_greg_month.msg = [];
  }
and default_transl_french_month () =
  {
    Transl_french_month.pos = default_french_month ();
    Transl_french_month.sval = default_string ();
  }
and default_config_transl_french_month () =
  {
    Config_transl_french_month.msg = [];
  }
and default_transl_hebrew_month () =
  {
    Transl_hebrew_month.pos = default_hebrew_month ();
    Transl_hebrew_month.sval = default_string ();
  }
and default_config_transl_hebrew_month () =
  {
    Config_transl_hebrew_month.msg = [];
  }
and default_config () =
  {
    Config.transl_cal = default_config_transl_calendar ();
    Config.transl_wit = default_config_transl_witness_type ();
    Config.transl_prec = default_config_transl_precision ();
    Config.transl_death = default_config_transl_death_type ();
    Config.transl_rel = default_config_transl_relation_parent_type ();
    Config.transl_fevents = default_config_transl_fevent_name ();
    Config.transl_pevents = default_config_transl_pevent_name ();
    Config.transl_access = default_config_transl_access ();
    Config.transl_warning = default_config_transl_update_warning_js ();
    Config.transl_short_greg_month = default_config_transl_short_greg_month ();
    Config.transl_french_month = default_config_transl_french_month ();
    Config.transl_hebrew_month = default_config_transl_hebrew_month ();
    Config.gwf_place_format = default_string ();
    Config.gwf_place_format_placeholder = default_string ();
  }
and default_sosa () = `sosa_ref
and default_calendar () = `gregorian
and default_witness_type () = `witness
and default_precision () = `sure
and default_sex () = `male
and default_death_type () = `not_dead
and default_relation_type () = `rparent_adoption
and default_create_or_link () = `create
and default_fevent_name () = `efam_marriage
and default_relation_parent_type () = `rpt_adoption_father
and default_pevent_name () = `epers_birth
and default_access () = `access_iftitles
and default_update_warning_js () = `empty_index
and default_person_or_family () = `person_form1
and default_auto_complete_place_field () = `subdivision
and default_auto_complete_field () = `lastname
and default_short_greg_month () = `janv
and default_french_month () = `vendemiaire
and default_hebrew_month () = `tichri


let piqi = "\226\202\2304\016api_saisie_write\226\231\249\238\001\027api_saisie_write.proto.piqi\162\244\146\155\011\031geneweb.api.saisie_write.object\218\244\134\182\012\208\001\138\233\142\251\014\201\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003day\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005month\210\171\158\194\006\014protobuf-int32\210\203\242$$\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004year\210\171\158\194\006\005int32\210\203\242$.\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005delta\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\003dmy\218\244\134\182\012\224\001\138\233\142\251\014\217\001\210\203\242$&\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003cal\210\171\158\194\006\bcalendar\210\203\242$(\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004prec\210\171\158\194\006\tprecision\210\203\242$!\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003dmy\210\171\158\194\006\003dmy\210\203\242$\"\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004dmy2\210\171\158\194\006\003dmy\210\203\242$%\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004text\210\171\158\194\006\006string\218\164\238\191\004\004date\218\244\134\182\012\248\003\138\233\142\251\014\241\003\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$&\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005dates\210\171\158\194\006\006string\210\203\242$&\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$#\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\210\203\242$'\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006family\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\018\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$+\232\146\150q\022\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002oc\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\rperson-search\218\244\134\182\012\241\003\138\233\142\251\014\234\003\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$1\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016birth-short-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$1\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016death-short-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$&\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$#\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\218\164\238\191\004\rsimple-person\218\244\134\182\012\133\001\138\233\142\251\014\127\210\203\242$3\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012witness-type\210\171\158\194\006\012witness-type\210\203\242$/\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007witness\210\171\158\194\006\rsimple-person\218\164\238\191\004\rwitness-event\218\244\134\182\012\216\003\138\233\142\251\014\209\003\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004name\210\171\158\194\006\006string\210\203\242$%\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004date\210\171\158\194\006\006string\210\203\242$*\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tdate-conv\210\171\158\194\006\006string\210\203\242$+\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bdate-cal\210\171\158\194\006\bcalendar\210\203\242$&\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005place\210\171\158\194\006\006string\210\203\242$'\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006reason\210\171\158\194\006\006string\210\203\242$%\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004note\210\171\158\194\006\006string\210\203\242$$\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003src\210\171\158\194\006\006string\210\203\242$.\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006spouse\210\171\158\194\006\rsimple-person\210\203\242$1\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\rwitness-event\218\164\238\191\004\005event\218\244\134\182\012\129\001\138\233\142\251\014{\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006r-type\210\171\158\194\006\rrelation-type\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\rsimple-person\218\164\238\191\004\015relation-person\218\244\134\182\012n\138\233\142\251\014h\210\203\242$(\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007husband\210\171\158\194\006\006string\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004wife\210\171\158\194\006\006string\218\164\238\191\004\011was-witness\218\244\134\182\012\212\007\138\233\142\251\014\205\007\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$,\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$(\232\146\150q\012\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007aliases\210\171\158\194\006\006string\210\203\242$+\232\146\150q\014\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nqualifiers\210\171\158\194\006\006string\210\203\242$2\232\146\150q\016\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\017firstname-aliases\210\171\158\194\006\006string\210\203\242$0\232\146\150q\018\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015surname-aliases\210\171\158\194\006\006string\210\203\242$&\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$&\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006events\210\171\158\194\006\005event\210\203\242$+\232\146\150q\024\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\noccupation\210\171\158\194\006\006string\210\203\242$&\232\146\150q\026\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005notes\210\171\158\194\006\006string\210\203\242$)\232\146\150q\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bpsources\210\171\158\194\006\006string\210\203\242$*\232\146\150q\030\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011has-sources\210\171\158\194\006\004bool\210\203\242$'\232\146\150q \152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006titles\210\171\158\194\006\006string\210\203\242$1\232\146\150q\"\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007related\210\171\158\194\006\015relation-person\210\203\242$2\232\146\150q$\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\brparents\210\171\158\194\006\015relation-person\210\203\242$1\232\146\150q&\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\011was-witness\210\171\158\194\006\011was-witness\210\203\242$#\232\146\150q(\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\218\164\238\191\004\018person-search-info\218\244\134\182\012\227\002\138\233\142\251\014\220\002\210\203\242$4\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011create-link\210\171\158\194\006\014create-or-link\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$,\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$&\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005dates\210\171\158\194\006\006string\218\164\238\191\004\011person-link\218\244\134\182\012|\138\233\142\251\014v\210\203\242$3\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012witness-type\210\171\158\194\006\012witness-type\210\203\242$,\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006person\210\171\158\194\006\011person-link\218\164\238\191\004\007witness\218\244\134\182\012\252\002\138\233\142\251\014\245\002\210\203\242$1\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011fevent-type\210\171\158\194\006\011fevent-name\210\203\242$#\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004date\210\171\158\194\006\004date\210\203\242$&\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005place\210\171\158\194\006\006string\210\203\242$'\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006reason\210\171\158\194\006\006string\210\203\242$%\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004note\210\171\158\194\006\006string\210\203\242$$\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003src\210\171\158\194\006\006string\210\203\242$+\232\146\150q\014\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\007witness\210\203\242$,\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011event-perso\210\171\158\194\006\006string\218\164\238\191\004\006fevent\218\244\134\182\012\181\001\138\233\142\251\014\174\001\210\203\242$7\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\brpt-type\210\171\158\194\006\020relation-parent-type\210\203\242$,\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006person\210\171\158\194\006\011person-link\210\203\242$'\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006source\210\171\158\194\006\006string\218\164\238\191\004\015relation-parent\218\244\134\182\012\156\002\138\233\142\251\014\149\002\210\203\242$%\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004name\210\171\158\194\006\006string\210\203\242$&\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005title\210\171\158\194\006\006string\210\203\242$%\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004fief\210\171\158\194\006\006string\210\203\242$)\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndate-begin\210\171\158\194\006\004date\210\203\242$'\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bdate-end\210\171\158\194\006\004date\210\203\242$,\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003nth\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\005title\218\244\134\182\012\252\002\138\233\142\251\014\245\002\210\203\242$1\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011pevent-type\210\171\158\194\006\011pevent-name\210\203\242$#\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004date\210\171\158\194\006\004date\210\203\242$&\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005place\210\171\158\194\006\006string\210\203\242$'\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006reason\210\171\158\194\006\006string\210\203\242$%\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004note\210\171\158\194\006\006string\210\203\242$$\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003src\210\171\158\194\006\006string\210\203\242$+\232\146\150q\014\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\007witness\210\203\242$,\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011event-perso\210\171\158\194\006\006string\218\164\238\191\004\006pevent\218\244\134\182\012\198\t\138\233\142\251\014\191\t\210\203\242$'\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006digest\210\171\158\194\006\006string\210\203\242$4\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011create-link\210\171\158\194\006\014create-or-link\210\203\242$.\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$,\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$(\232\146\150q\018\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007aliases\210\171\158\194\006\006string\210\203\242$+\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nqualifiers\210\171\158\194\006\006string\210\203\242$2\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\017firstname-aliases\210\171\158\194\006\006string\210\203\242$0\232\146\150q\024\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015surname-aliases\210\171\158\194\006\006string\210\203\242$&\232\146\150q\026\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$/\232\146\150q\028\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-type\210\171\158\194\006\ndeath-type\210\203\242$+\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\noccupation\210\171\158\194\006\006string\210\203\242$)\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bpsources\210\171\158\194\006\006string\210\203\242$&\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005notes\210\171\158\194\006\006string\210\203\242$&\232\146\150q$\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006titles\210\171\158\194\006\005title\210\203\242$(\232\146\150q&\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007pevents\210\171\158\194\006\006pevent\210\203\242$0\232\146\150q(\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007related\210\171\158\194\006\014protobuf-int32\210\203\242$2\232\146\150q*\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\brparents\210\171\158\194\006\015relation-parent\210\203\242$Q\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006access\210\171\158\194\006\006access\138\140\251\240\r$\218\148\211\024\002\b\000\210\171\158\194\006\023api_saisie_write/access\210\203\242$0\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007parents\210\171\158\194\006\014protobuf-int32\210\203\242$1\232\146\150q0\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\006person\218\244\134\182\012\241\003\138\233\142\251\014\234\003\210\203\242$'\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006digest\210\171\158\194\006\006string\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$(\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007fevents\210\171\158\194\006\006fevent\210\203\242$)\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bfsources\210\171\158\194\006\006string\210\203\242$,\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011origin-file\210\171\158\194\006\006string\210\203\242$(\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007comment\210\171\158\194\006\006string\210\203\242$'\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\006person\210\203\242$'\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\006person\210\203\242$.\232\146\150q\018\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bchildren\210\171\158\194\006\011person-link\210\203\242$6\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\rold-witnesses\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\006family\218\244\134\182\012\151\003\138\233\142\251\014\144\003\210\203\242$/\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004form\210\171\158\194\006\016person-or-family\210\203\242$&\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007witness\210\171\158\194\006\004bool\210\203\242$'\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\brparents\210\171\158\194\006\004bool\210\203\242$$\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005event\210\171\158\194\006\004bool\210\203\242$,\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003pos\210\171\158\194\006\014protobuf-int32\210\203\242$4\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011pos-witness\210\171\158\194\006\014protobuf-int32\210\203\242$)\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\218\164\238\191\004\015create-conflict\218\244\134\182\012\162\005\138\233\142\251\014\155\005\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\015is-base-updated\210\171\158\194\006\004bool\210\203\242$.\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\rbase-warnings\210\171\158\194\006\006string\210\203\242$+\232\146\150q\026\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nbase-miscs\210\171\158\194\006\006string\210\203\242$5\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012index-person\210\171\158\194\006\014protobuf-int32\210\203\242$)\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$,\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$5\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012index-family\210\171\158\194\006\014protobuf-int32\210\203\242$2\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bconflict\210\171\158\194\006\015create-conflict\210\203\242$-\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012lastname-str\210\171\158\194\006\006string\210\203\242$.\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rfirstname-str\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\022\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\024\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\001p\210\171\158\194\006\006string\218\164\238\191\004\019modification-status\218\244\134\182\012K\138\233\142\251\014E\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\012index-person\218\244\134\182\012K\138\233\142\251\014E\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\012index-family\218\244\134\182\012\152\001\138\233\142\251\014\145\001\210\203\242$5\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-person\210\171\158\194\006\014protobuf-int32\210\203\242$5\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-family\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\023index-person-and-family\218\244\134\182\012\143\003\138\233\142\251\014\136\003\210\203\242$5\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-family\210\171\158\194\006\014protobuf-int32\210\203\242$5\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-person\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$&\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005dates\210\171\158\194\006\006string\210\203\242$&\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$#\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\218\164\238\191\004\rfamily-spouse\218\244\134\182\012\177\001\138\233\142\251\014\170\001\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$5\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012index-family\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\218\164\238\191\004\017add-child-request\218\244\134\182\012\230\001\138\233\142\251\014\223\001\210\203\242$0\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\015person-lastname\210\171\158\194\006\006string\210\203\242$1\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016person-firstname\210\171\158\194\006\006string\210\203\242$5\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\rfamily-spouse\210\171\158\194\006\rfamily-spouse\210\203\242$&\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005child\210\171\158\194\006\006person\218\164\238\191\004\tadd-child\218\244\134\182\012\230\001\138\233\142\251\014\223\001\210\203\242$5\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-person\210\171\158\194\006\014protobuf-int32\210\203\242$5\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-family\210\171\158\194\006\014protobuf-int32\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nnew-family\210\171\158\194\006\004bool\210\203\242$&\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005child\210\171\158\194\006\006person\218\164\238\191\004\012add-child-ok\218\244\134\182\012\175\001\138\233\142\251\014\168\001\210\203\242$0\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\015person-lastname\210\171\158\194\006\006string\210\203\242$1\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016person-firstname\210\171\158\194\006\006string\210\203\242$'\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006family\210\171\158\194\006\006family\218\164\238\191\004\011add-parents\218\244\134\182\012\128\001\138\233\142\251\014z\210\203\242$5\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-person\210\171\158\194\006\014protobuf-int32\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006family\210\171\158\194\006\006family\218\164\238\191\004\014add-parents-ok\218\244\134\182\012\174\001\138\233\142\251\014\167\001\210\203\242$0\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\015person-lastname\210\171\158\194\006\006string\210\203\242$1\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016person-firstname\210\171\158\194\006\006string\210\203\242$'\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006family\210\171\158\194\006\006family\218\164\238\191\004\nadd-family\218\244\134\182\012\127\138\233\142\251\014y\210\203\242$5\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-person\210\171\158\194\006\014protobuf-int32\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006family\210\171\158\194\006\006family\218\164\238\191\004\radd-family-ok\218\244\134\182\012\139\001\138\233\142\251\014\132\001\210\203\242$/\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007spouses\210\171\158\194\006\rfamily-spouse\210\203\242$2\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012first-family\210\171\158\194\006\011edit-family\218\164\238\191\004\019edit-family-request\218\244\134\182\012\175\001\138\233\142\251\014\168\001\210\203\242$0\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\015person-lastname\210\171\158\194\006\006string\210\203\242$1\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016person-firstname\210\171\158\194\006\006string\210\203\242$'\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006family\210\171\158\194\006\006family\218\164\238\191\004\011edit-family\218\244\134\182\012\128\001\138\233\142\251\014z\210\203\242$5\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-person\210\171\158\194\006\014protobuf-int32\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006family\210\171\158\194\006\006family\218\164\238\191\004\014edit-family-ok\218\244\134\182\012x\138\233\142\251\014r\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\218\164\238\191\004\019add-sibling-request\218\244\134\182\012\176\001\138\233\142\251\014\169\001\210\203\242$0\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\015person-lastname\210\171\158\194\006\006string\210\203\242$1\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016person-firstname\210\171\158\194\006\006string\210\203\242$(\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007sibling\210\171\158\194\006\006person\218\164\238\191\004\011add-sibling\218\244\134\182\012\129\001\138\233\142\251\014{\210\203\242$5\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012index-person\210\171\158\194\006\014protobuf-int32\210\203\242$(\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007sibling\210\171\158\194\006\006person\218\164\238\191\004\014add-sibling-ok\218\244\134\182\012\246\001\138\233\142\251\014\239\001\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\006person\210\203\242$'\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\006person\210\203\242$'\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\006person\210\203\242$'\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006spouse\210\171\158\194\006\006person\210\203\242$)\232\146\150q\n\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bchildren\210\171\158\194\006\006person\218\164\238\191\004\radd-first-fam\218\244\134\182\012\244\001\138\233\142\251\014\237\001\210\203\242$3\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005field\210\171\158\194\006\019auto-complete-field\210\203\242$?\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011place-field\210\171\158\194\006\025auto-complete-place-field\210\203\242$&\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005input\210\171\158\194\006\006string\210\203\242$.\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005limit\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\rauto-complete\218\244\134\182\012L\138\233\142\251\014F\210\203\242$'\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006result\210\171\158\194\006\006string\218\164\238\191\004\020auto-complete-result\218\244\134\182\012\182\001\138\233\142\251\014\175\001\210\203\242$)\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$.\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005limit\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\025person-search-list-params\218\244\134\182\012R\138\233\142\251\014L\210\203\242$/\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007persons\210\171\158\194\006\rperson-search\218\164\238\191\004\018person-search-list\218\244\134\182\012p\138\233\142\251\014j\210\203\242$&\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\bcalendar\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\015transl-calendar\218\244\134\182\012T\138\233\142\251\014N\210\203\242$-\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\015transl-calendar\218\164\238\191\004\022config-transl-calendar\218\244\134\182\012x\138\233\142\251\014r\210\203\242$*\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\012witness-type\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\019transl-witness-type\218\244\134\182\012\\\138\233\142\251\014V\210\203\242$1\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\019transl-witness-type\218\164\238\191\004\026config-transl-witness-type\218\244\134\182\012r\138\233\142\251\014l\210\203\242$'\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\tprecision\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\016transl-precision\218\244\134\182\012V\138\233\142\251\014P\210\203\242$.\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\016transl-precision\218\164\238\191\004\023config-transl-precision\218\244\134\182\012t\138\233\142\251\014n\210\203\242$(\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\ndeath-type\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\017transl-death-type\218\244\134\182\012X\138\233\142\251\014R\210\203\242$/\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\017transl-death-type\218\164\238\191\004\024config-transl-death-type\218\244\134\182\012\137\001\138\233\142\251\014\130\001\210\203\242$2\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\020relation-parent-type\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\027transl-relation-parent-type\218\244\134\182\012l\138\233\142\251\014f\210\203\242$9\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\027transl-relation-parent-type\218\164\238\191\004\"config-transl-relation-parent-type\218\244\134\182\012v\138\233\142\251\014p\210\203\242$)\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\011fevent-name\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\018transl-fevent-name\218\244\134\182\012Z\138\233\142\251\014T\210\203\242$0\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\018transl-fevent-name\218\164\238\191\004\025config-transl-fevent-name\218\244\134\182\012v\138\233\142\251\014p\210\203\242$)\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\011pevent-name\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\018transl-pevent-name\218\244\134\182\012Z\138\233\142\251\014T\210\203\242$0\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\018transl-pevent-name\218\164\238\191\004\025config-transl-pevent-name\218\244\134\182\012l\138\233\142\251\014f\210\203\242$$\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\006access\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\rtransl-access\218\244\134\182\012P\138\233\142\251\014J\210\203\242$+\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\rtransl-access\218\164\238\191\004\020config-transl-access\218\244\134\182\012\130\001\138\233\142\251\014|\210\203\242$/\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\017update-warning-js\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\024transl-update-warning-js\218\244\134\182\012f\138\233\142\251\014`\210\203\242$6\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\024transl-update-warning-js\218\164\238\191\004\031config-transl-update-warning-js\218\244\134\182\012\128\001\138\233\142\251\014z\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\016short-greg-month\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\023transl-short-greg-month\218\244\134\182\012d\138\233\142\251\014^\210\203\242$5\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\023transl-short-greg-month\218\164\238\191\004\030config-transl-short-greg-month\218\244\134\182\012x\138\233\142\251\014r\210\203\242$*\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\012french-month\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\019transl-french-month\218\244\134\182\012\\\138\233\142\251\014V\210\203\242$1\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\019transl-french-month\218\164\238\191\004\026config-transl-french-month\218\244\134\182\012x\138\233\142\251\014r\210\203\242$*\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003pos\210\171\158\194\006\012hebrew-month\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sval\210\171\158\194\006\006string\218\164\238\191\004\019transl-hebrew-month\218\244\134\182\012\\\138\233\142\251\014V\210\203\242$1\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\003msg\210\171\158\194\006\019transl-hebrew-month\218\164\238\191\004\026config-transl-hebrew-month\218\244\134\182\012\236\007\138\233\142\251\014\229\007\210\203\242$;\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ntransl-cal\210\171\158\194\006\022config-transl-calendar\210\203\242$?\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ntransl-wit\210\171\158\194\006\026config-transl-witness-type\210\203\242$=\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011transl-prec\210\171\158\194\006\023config-transl-precision\210\203\242$?\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012transl-death\210\171\158\194\006\024config-transl-death-type\210\203\242$G\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ntransl-rel\210\171\158\194\006\"config-transl-relation-parent-type\210\203\242$B\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\014transl-fevents\210\171\158\194\006\025config-transl-fevent-name\210\203\242$B\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\014transl-pevents\210\171\158\194\006\025config-transl-pevent-name\210\203\242$<\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rtransl-access\210\171\158\194\006\020config-transl-access\210\203\242$H\232\146\150q\018\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\014transl-warning\210\171\158\194\006\031config-transl-update-warning-js\210\203\242$P\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\023transl-short-greg-month\210\171\158\194\006\030config-transl-short-greg-month\210\203\242$H\232\146\150q\022\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\019transl-french-month\210\171\158\194\006\026config-transl-french-month\210\203\242$H\232\146\150q\024\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\019transl-hebrew-month\210\171\158\194\006\026config-transl-hebrew-month\210\203\242$1\232\146\150q\026\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016gwf-place-format\210\171\158\194\006\006string\210\203\242$=\232\146\150q\028\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\028gwf-place-format-placeholder\210\171\158\194\006\006string\218\164\238\191\004\006config\218\244\134\182\012V\138\176\205\197\001P\218\164\238\191\004\004sosa\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\bsosa-ref\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004sosa\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007no-sosa\218\244\134\182\012s\138\176\205\197\001m\218\164\238\191\004\bcalendar\170\183\218\222\005\020\232\146\150q\000\218\164\238\191\004\tgregorian\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006julian\170\183\218\222\005\017\232\146\150q\004\218\164\238\191\004\006french\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006hebrew\218\244\134\182\012R\138\176\205\197\001L\218\164\238\191\004\012witness-type\170\183\218\222\005\018\232\146\150q\000\218\164\238\191\004\007witness\170\183\218\222\005\028\232\146\150q\002\218\164\238\191\004\017witness-godparent\218\244\134\182\012\179\001\138\176\205\197\001\172\001\218\164\238\191\004\tprecision\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004sure\170\183\218\222\005\016\232\146\150q\002\218\164\238\191\004\005about\170\183\218\222\005\016\232\146\150q\004\218\164\238\191\004\005maybe\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006before\170\183\218\222\005\016\232\146\150q\b\218\164\238\191\004\005after\170\183\218\222\005\017\232\146\150q\n\218\164\238\191\004\006oryear\170\183\218\222\005\018\232\146\150q\012\218\164\238\191\004\007yearint\218\244\134\182\012S\138\176\205\197\001M\218\164\238\191\004\003sex\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004male\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006female\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007unknown\218\244\134\182\012\197\001\138\176\205\197\001\190\001\218\164\238\191\004\ndeath-type\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\bnot-dead\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004dead\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ndead-young\170\183\218\222\005\030\232\146\150q\006\218\164\238\191\004\019dead-dont-know-when\170\183\218\222\005\028\232\146\150q\b\218\164\238\191\004\017dont-know-if-dead\170\183\218\222\005\025\232\146\150q\n\218\164\238\191\004\014of-course-dead\218\244\134\182\012\131\003\138\176\205\197\001\252\002\218\164\238\191\004\rrelation-type\170\183\218\222\005\027\232\146\150q\000\218\164\238\191\004\016rparent-adoption\170\183\218\222\005\030\232\146\150q\002\218\164\238\191\004\019rparent-recognition\170\183\218\222\005#\232\146\150q\004\218\164\238\191\004\024rparent-candidate-parent\170\183\218\222\005\029\232\146\150q\006\218\164\238\191\004\018rparent-god-parent\170\183\218\222\005 \232\146\150q\b\218\164\238\191\004\021rparent-foster-parent\170\183\218\222\005\026\232\146\150q\n\218\164\238\191\004\015rchild-adoption\170\183\218\222\005\029\232\146\150q\012\218\164\238\191\004\018rchild-recognition\170\183\218\222\005\"\232\146\150q\014\218\164\238\191\004\023rchild-candidate-parent\170\183\218\222\005\028\232\146\150q\016\218\164\238\191\004\017rchild-god-parent\170\183\218\222\005\031\232\146\150q\018\218\164\238\191\004\020rchild-foster-parent\218\244\134\182\012i\138\176\205\197\001c\218\164\238\191\004\014create-or-link\170\183\218\222\005\017\232\146\150q\000\218\164\238\191\004\006create\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004link\170\183\218\222\005\029\232\146\150q\004\218\164\238\191\004\018create-default-occ\218\244\134\182\012\152\003\138\176\205\197\001\145\003\218\164\238\191\004\011fevent-name\170\183\218\222\005\024\232\146\150q\000\218\164\238\191\004\refam-marriage\170\183\218\222\005\027\232\146\150q\002\218\164\238\191\004\016efam-no-marriage\170\183\218\222\005\026\232\146\150q\004\218\164\238\191\004\015efam-no-mention\170\183\218\222\005\022\232\146\150q\006\218\164\238\191\004\011efam-engage\170\183\218\222\005\023\232\146\150q\b\218\164\238\191\004\012efam-divorce\170\183\218\222\005\025\232\146\150q\n\218\164\238\191\004\014efam-separated\170\183\218\222\005\026\232\146\150q\012\218\164\238\191\004\015efam-annulation\170\183\218\222\005\029\232\146\150q\014\218\164\238\191\004\018efam-marriage-bann\170\183\218\222\005!\232\146\150q\016\218\164\238\191\004\022efam-marriage-contract\170\183\218\222\005 \232\146\150q\018\218\164\238\191\004\021efam-marriage-license\170\183\218\222\005\020\232\146\150q\020\218\164\238\191\004\tefam-pacs\170\183\218\222\005\025\232\146\150q\022\218\164\238\191\004\014efam-residence\218\244\134\182\012\173\003\138\176\205\197\001\166\003\218\164\238\191\004\020relation-parent-type\170\183\218\222\005\030\232\146\150q\000\218\164\238\191\004\019rpt-adoption-father\170\183\218\222\005\030\232\146\150q\002\218\164\238\191\004\019rpt-adoption-mother\170\183\218\222\005!\232\146\150q\004\218\164\238\191\004\022rpt-recognition-father\170\183\218\222\005!\232\146\150q\006\218\164\238\191\004\022rpt-recognition-mother\170\183\218\222\005&\232\146\150q\b\218\164\238\191\004\027rpt-candidate-parent-father\170\183\218\222\005&\232\146\150q\n\218\164\238\191\004\027rpt-candidate-parent-mother\170\183\218\222\005 \232\146\150q\012\218\164\238\191\004\021rpt-god-parent-father\170\183\218\222\005 \232\146\150q\014\218\164\238\191\004\021rpt-god-parent-mother\170\183\218\222\005#\232\146\150q\016\218\164\238\191\004\024rpt-foster-parent-father\170\183\218\222\005#\232\146\150q\018\218\164\238\191\004\024rpt-foster-parent-mother\218\244\134\182\012\198\r\138\176\205\197\001\191\r\218\164\238\191\004\011pevent-name\170\183\218\222\005\022\232\146\150q\000\218\164\238\191\004\011epers-birth\170\183\218\222\005\024\232\146\150q\002\218\164\238\191\004\repers-baptism\170\183\218\222\005\022\232\146\150q\004\218\164\238\191\004\011epers-death\170\183\218\222\005\023\232\146\150q\006\218\164\238\191\004\012epers-burial\170\183\218\222\005\026\232\146\150q\b\218\164\238\191\004\015epers-cremation\170\183\218\222\005\031\232\146\150q\n\218\164\238\191\004\020epers-accomplishment\170\183\218\222\005\028\232\146\150q\012\218\164\238\191\004\017epers-acquisition\170\183\218\222\005\025\232\146\150q\014\218\164\238\191\004\014epers-adhesion\170\183\218\222\005\027\232\146\150q\016\218\164\238\191\004\016epers-baptismlds\170\183\218\222\005\027\232\146\150q\018\218\164\238\191\004\016epers-barmitzvah\170\183\218\222\005\027\232\146\150q\020\218\164\238\191\004\016epers-batmitzvah\170\183\218\222\005\028\232\146\150q\022\218\164\238\191\004\017epers-benediction\170\183\218\222\005\027\232\146\150q\024\218\164\238\191\004\016epers-changename\170\183\218\222\005\029\232\146\150q\026\218\164\238\191\004\018epers-circumcision\170\183\218\222\005\029\232\146\150q\028\218\164\238\191\004\018epers-confirmation\170\183\218\222\005 \232\146\150q\030\218\164\238\191\004\021epers-confirmationlds\170\183\218\222\005\027\232\146\150q \218\164\238\191\004\016epers-decoration\170\183\218\222\005(\232\146\150q\"\218\164\238\191\004\029epers-demobilisationmilitaire\170\183\218\222\005\024\232\146\150q$\218\164\238\191\004\repers-diploma\170\183\218\222\005\028\232\146\150q&\218\164\238\191\004\017epers-distinction\170\183\218\222\005\025\232\146\150q(\218\164\238\191\004\014epers-dotation\170\183\218\222\005\028\232\146\150q*\218\164\238\191\004\017epers-dotationlds\170\183\218\222\005\026\232\146\150q,\218\164\238\191\004\015epers-education\170\183\218\222\005\025\232\146\150q.\218\164\238\191\004\014epers-election\170\183\218\222\005\027\232\146\150q0\218\164\238\191\004\016epers-emigration\170\183\218\222\005 \232\146\150q2\218\164\238\191\004\021epers-excommunication\170\183\218\222\005\030\232\146\150q4\218\164\238\191\004\019epers-familylinklds\170\183\218\222\005\031\232\146\150q6\218\164\238\191\004\020epers-firstcommunion\170\183\218\222\005\024\232\146\150q8\218\164\238\191\004\repers-funeral\170\183\218\222\005\025\232\146\150q:\218\164\238\191\004\014epers-graduate\170\183\218\222\005 \232\146\150q<\218\164\238\191\004\021epers-hospitalisation\170\183\218\222\005\024\232\146\150q>\218\164\238\191\004\repers-illness\170\183\218\222\005\028\232\146\150q@\218\164\238\191\004\017epers-immigration\170\183\218\222\005\031\232\146\150qB\218\164\238\191\004\020epers-listepassenger\170\183\218\222\005$\232\146\150qD\218\164\238\191\004\025epers-militarydistinction\170\183\218\222\005\"\232\146\150qF\218\164\238\191\004\023epers-militarypromotion\170\183\218\222\005 \232\146\150qH\218\164\238\191\004\021epers-militaryservice\170\183\218\222\005&\232\146\150qJ\218\164\238\191\004\027epers-mobilisationmilitaire\170\183\218\222\005\031\232\146\150qL\218\164\238\191\004\020epers-naturalisation\170\183\218\222\005\027\232\146\150qN\218\164\238\191\004\016epers-occupation\170\183\218\222\005\027\232\146\150qP\218\164\238\191\004\016epers-ordination\170\183\218\222\005\025\232\146\150qR\218\164\238\191\004\014epers-property\170\183\218\222\005\028\232\146\150qT\218\164\238\191\004\017epers-recensement\170\183\218\222\005\026\232\146\150qV\218\164\238\191\004\015epers-residence\170\183\218\222\005\024\232\146\150qX\218\164\238\191\004\repers-retired\170\183\218\222\005!\232\146\150qZ\218\164\238\191\004\022epers-scellentchildlds\170\183\218\222\005\"\232\146\150q\\\218\164\238\191\004\023epers-scellentparentlds\170\183\218\222\005\"\232\146\150q^\218\164\238\191\004\023epers-scellentspouselds\170\183\218\222\005\026\232\146\150q`\218\164\238\191\004\015epers-ventebien\170\183\218\222\005\021\232\146\150qb\218\164\238\191\004\nepers-will\218\244\134\182\012o\138\176\205\197\001i\218\164\238\191\004\006access\170\183\218\222\005\026\232\146\150q\000\218\164\238\191\004\015access-iftitles\170\183\218\222\005\024\232\146\150q\002\218\164\238\191\004\raccess-public\170\183\218\222\005\025\232\146\150q\004\218\164\238\191\004\014access-private\218\244\134\182\012\129\002\138\176\205\197\001\250\001\218\164\238\191\004\017update-warning-js\170\183\218\222\005\022\232\146\150q\000\218\164\238\191\004\011empty-index\170\183\218\222\005\024\232\146\150q\002\218\164\238\191\004\rempty-surname\170\183\218\222\005\027\232\146\150q\004\218\164\238\191\004\016empty-first-name\170\183\218\222\005\020\232\146\150q\006\218\164\238\191\004\tempty-sex\170\183\218\222\005\025\232\146\150q\b\218\164\238\191\004\014required-field\170\183\218\222\005!\232\146\150q\n\218\164\238\191\004\022birth-date-after-event\170\183\218\222\005\"\232\146\150q\012\218\164\238\191\004\023death-date-before-event\218\244\134\182\012r\138\176\205\197\001l\218\164\238\191\004\016person-or-family\170\183\218\222\005\023\232\146\150q\000\218\164\238\191\004\012person-form1\170\183\218\222\005\023\232\146\150q\002\218\164\238\191\004\012person-form2\170\183\218\222\005\022\232\146\150q\004\218\164\238\191\004\011family-form\218\244\134\182\012\183\001\138\176\205\197\001\176\001\218\164\238\191\004\025auto-complete-place-field\170\183\218\222\005\022\232\146\150q\000\218\164\238\191\004\011subdivision\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004town\170\183\218\222\005\020\232\146\150q\004\218\164\238\191\004\tarea-code\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006county\170\183\218\222\005\017\232\146\150q\b\218\164\238\191\004\006region\170\183\218\222\005\018\232\146\150q\n\218\164\238\191\004\007country\218\244\134\182\012\127\138\176\205\197\001y\218\164\238\191\004\019auto-complete-field\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\blastname\170\183\218\222\005\020\232\146\150q\002\218\164\238\191\004\tfirstname\170\183\218\222\005\016\232\146\150q\004\218\164\238\191\004\005place\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006source\218\244\134\182\012\148\002\138\176\205\197\001\141\002\218\164\238\191\004\016short-greg-month\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004janv\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004fevr\170\183\218\222\005\015\232\146\150q\004\218\164\238\191\004\004mars\170\183\218\222\005\014\232\146\150q\006\218\164\238\191\004\003avr\170\183\218\222\005\014\232\146\150q\b\218\164\238\191\004\003mai\170\183\218\222\005\015\232\146\150q\n\218\164\238\191\004\004juin\170\183\218\222\005\015\232\146\150q\012\218\164\238\191\004\004juil\170\183\218\222\005\015\232\146\150q\014\218\164\238\191\004\004aout\170\183\218\222\005\015\232\146\150q\016\218\164\238\191\004\004sept\170\183\218\222\005\014\232\146\150q\018\218\164\238\191\004\003oct\170\183\218\222\005\014\232\146\150q\020\218\164\238\191\004\003nov\170\183\218\222\005\014\232\146\150q\022\218\164\238\191\004\003dec\218\244\134\182\012\229\002\138\176\205\197\001\222\002\218\164\238\191\004\012french-month\170\183\218\222\005\022\232\146\150q\000\218\164\238\191\004\011vendemiaire\170\183\218\222\005\019\232\146\150q\002\218\164\238\191\004\bbrumaire\170\183\218\222\005\019\232\146\150q\004\218\164\238\191\004\bfrimaire\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006nivose\170\183\218\222\005\019\232\146\150q\b\218\164\238\191\004\bpluviose\170\183\218\222\005\018\232\146\150q\n\218\164\238\191\004\007ventose\170\183\218\222\005\019\232\146\150q\012\218\164\238\191\004\bgerminal\170\183\218\222\005\018\232\146\150q\014\218\164\238\191\004\007floreal\170\183\218\222\005\019\232\146\150q\016\218\164\238\191\004\bprairial\170\183\218\222\005\019\232\146\150q\018\218\164\238\191\004\bmessidor\170\183\218\222\005\020\232\146\150q\020\218\164\238\191\004\tthermidor\170\183\218\222\005\020\232\146\150q\022\218\164\238\191\004\tfructidor\170\183\218\222\005\025\232\146\150q\024\218\164\238\191\004\014complementaire\218\244\134\182\012\191\002\138\176\205\197\001\184\002\218\164\238\191\004\012hebrew-month\170\183\218\222\005\017\232\146\150q\000\218\164\238\191\004\006tichri\170\183\218\222\005\021\232\146\150q\002\218\164\238\191\004\nmarhechvan\170\183\218\222\005\017\232\146\150q\004\218\164\238\191\004\006kislev\170\183\218\222\005\016\232\146\150q\006\218\164\238\191\004\005tevet\170\183\218\222\005\017\232\146\150q\b\218\164\238\191\004\006chevat\170\183\218\222\005\017\232\146\150q\n\218\164\238\191\004\006adar-1\170\183\218\222\005\017\232\146\150q\012\218\164\238\191\004\006adar-2\170\183\218\222\005\017\232\146\150q\014\218\164\238\191\004\006nissan\170\183\218\222\005\015\232\146\150q\016\218\164\238\191\004\004iyar\170\183\218\222\005\016\232\146\150q\018\218\164\238\191\004\005sivan\170\183\218\222\005\017\232\146\150q\020\218\164\238\191\004\006tamouz\170\183\218\222\005\r\232\146\150q\022\218\164\238\191\004\002av\170\183\218\222\005\016\232\146\150q\024\218\164\238\191\004\005eloul"
include Api_saisie_write_piqi
