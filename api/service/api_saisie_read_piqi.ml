module rec Api_saisie_read_piqi:
  sig
    type protobuf_int32 = int32
    type protobuf_int64 = int64
    type person_type =
      [
        | `simple
        | `full
        | `fiche
      ]
    type error_code =
      [
        | `bad_request
        | `unauthorized
        | `forbidden
        | `not_found
      ]
    type sosa =
      [
        | `no_sosa
        | `sosa_ref
        | `sosa
      ]
    type calendar =
      [
        | `gregorian
        | `julian
        | `french
        | `hebrew
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
        | `unknown
        | `male
        | `female
      ]
    type death_type =
      [
        | `dont_know_if_dead
        | `not_dead
        | `dead
        | `dead_young
        | `dead_dont_know_when
        | `of_course_dead
      ]
    type burial_type =
      [
        | `dont_know
        | `buried
        | `cremated
      ]
    type marriage_type =
      [
        | `no_mention
        | `married
        | `not_married
        | `engaged
        | `no_sexes_check_not_married
        | `no_sexes_check_married
      ]
    type divorce_type =
      [
        | `not_divorced
        | `divorced
        | `separated
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
    type witness_type =
      [
        | `witness
        | `witness_godparent
      ]
    type event_type =
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
        | `epers_custom
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
        | `efam_custom
      ]
    type title_type =
      [
        | `title_main
        | `title_name
        | `title_none
      ]
    type dmy = Dmy.t
    type date = Date.t
    type witness_event = Witness_event.t
    type event = Event.t
    type person_tree = Person_tree.t
    type simple_person = Simple_person.t
    type relation_person = Relation_person.t
    type event_witness = Event_witness.t
    type person = Person.t
    type fiche_person = Fiche_person.t
    type family = Family.t
    type index_person = Index_person.t
    type node = Node.t
    type edge = Edge.t
    type graph_tree = Graph_tree.t
    type graph_tree_new = Graph_tree_new.t
    type graph_tree_params = Graph_tree_params.t
    type title = Title.t
    type person_tree_full = Person_tree_full.t
    type family_tree_full = Family_tree_full.t
    type node_full = Node_full.t
    type graph_tree_full = Graph_tree_full.t
    type identifier_person = Identifier_person.t
    type error = Error.t
    type nb_ancestors = Nb_ancestors.t
    type conf_line = Conf_line.t
    type conf_base = Conf_base.t
  end = Api_saisie_read_piqi
and Dmy:
  sig
    type t = {
      mutable day: Api_saisie_read_piqi.protobuf_int32;
      mutable month: Api_saisie_read_piqi.protobuf_int32;
      mutable year: Api_saisie_read_piqi.protobuf_int32;
      mutable delta: Api_saisie_read_piqi.protobuf_int32;
    }
  end = Dmy
and Date:
  sig
    type t = {
      mutable cal: Api_saisie_read_piqi.calendar option;
      mutable prec: Api_saisie_read_piqi.precision option;
      mutable dmy: Api_saisie_read_piqi.dmy option;
      mutable dmy2: Api_saisie_read_piqi.dmy option;
      mutable text: string option;
    }
  end = Date
and Witness_event:
  sig
    type t = {
      mutable witness_type: Api_saisie_read_piqi.witness_type;
      mutable witness: Api_saisie_read_piqi.simple_person;
    }
  end = Witness_event
and Event:
  sig
    type t = {
      mutable name: string;
      mutable type_: Api_saisie_read_piqi.event_type;
      mutable date: string option;
      mutable date_long: string option;
      mutable date_raw: string option;
      mutable date_conv: string option;
      mutable date_conv_long: string option;
      mutable date_cal: Api_saisie_read_piqi.calendar option;
      mutable place: string option;
      mutable reason: string option;
      mutable note: string option;
      mutable src: string option;
      mutable spouse: Api_saisie_read_piqi.simple_person option;
      mutable witnesses: Api_saisie_read_piqi.witness_event list;
    }
  end = Event
and Person_tree:
  sig
    type t = {
      mutable index: Api_saisie_read_piqi.protobuf_int32;
      mutable sex: Api_saisie_read_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable n: string;
      mutable p: string;
      mutable occ: Api_saisie_read_piqi.protobuf_int32;
      mutable dates: string option;
      mutable image: string option;
      mutable sosa: Api_saisie_read_piqi.sosa;
      mutable has_more_infos: bool;
      mutable baseprefix: string;
    }
  end = Person_tree
and Simple_person:
  sig
    type t = {
      mutable index: Api_saisie_read_piqi.protobuf_int32;
      mutable sex: Api_saisie_read_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable n: string;
      mutable p: string;
      mutable occ: Api_saisie_read_piqi.protobuf_int32;
      mutable birth_short_date: string option;
      mutable birth_date_raw: string option;
      mutable birth_place: string option;
      mutable death_short_date: string option;
      mutable death_date_raw: string option;
      mutable death_place: string option;
      mutable image: string option;
      mutable sosa: Api_saisie_read_piqi.sosa;
      mutable baseprefix: string;
      mutable sosa_nb: string option;
      mutable visible_for_visitors: bool;
      mutable has_parent: bool;
      mutable has_spouse: bool;
      mutable has_child: bool;
    }
  end = Simple_person
and Relation_person:
  sig
    type t = {
      mutable r_type: Api_saisie_read_piqi.relation_type;
      mutable person: Api_saisie_read_piqi.simple_person;
    }
  end = Relation_person
and Event_witness:
  sig
    type t = {
      mutable event_witness_type: string;
      mutable husband: Api_saisie_read_piqi.simple_person;
      mutable wife: Api_saisie_read_piqi.simple_person option;
    }
  end = Event_witness
and Person:
  sig
    type t = {
      mutable type_: Api_saisie_read_piqi.person_type;
      mutable index: Api_saisie_read_piqi.protobuf_int32;
      mutable sex: Api_saisie_read_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable n: string;
      mutable p: string;
      mutable occ: Api_saisie_read_piqi.protobuf_int32;
      mutable public_name: string option;
      mutable aliases: string list;
      mutable qualifiers: string list;
      mutable firstname_aliases: string list;
      mutable surname_aliases: string list;
      mutable image: string option;
      mutable birth_date: string option;
      mutable birth_date_conv: string option;
      mutable birth_date_cal: Api_saisie_read_piqi.calendar option;
      mutable birth_place: string option;
      mutable birth_src: string option;
      mutable baptism_date: string option;
      mutable baptism_date_conv: string option;
      mutable baptism_date_cal: Api_saisie_read_piqi.calendar option;
      mutable baptism_place: string option;
      mutable baptism_src: string option;
      mutable death_date: string option;
      mutable death_date_conv: string option;
      mutable death_date_cal: Api_saisie_read_piqi.calendar option;
      mutable death_place: string option;
      mutable death_src: string option;
      mutable death_type: Api_saisie_read_piqi.death_type;
      mutable burial_date: string option;
      mutable burial_date_conv: string option;
      mutable burial_date_cal: Api_saisie_read_piqi.calendar option;
      mutable burial_place: string option;
      mutable burial_src: string option;
      mutable occupation: string option;
      mutable notes: string option;
      mutable psources: string option;
      mutable has_sources: bool;
      mutable titles: string list;
      mutable related: Api_saisie_read_piqi.relation_person list;
      mutable rparents: Api_saisie_read_piqi.relation_person list;
      mutable father: Api_saisie_read_piqi.simple_person option;
      mutable mother: Api_saisie_read_piqi.simple_person option;
      mutable families: Api_saisie_read_piqi.family list;
      mutable sosa: Api_saisie_read_piqi.sosa;
      mutable events: Api_saisie_read_piqi.event list;
      mutable events_witnesses: Api_saisie_read_piqi.event_witness list;
      mutable baseprefix: string;
      mutable fiche_person_person: Api_saisie_read_piqi.fiche_person option;
    }
  end = Person
and Fiche_person:
  sig
    type t = {
      mutable birth_date_raw: string option;
      mutable birth_text: string option;
      mutable baptism_date_raw: string option;
      mutable baptism_text: string option;
      mutable death_date_raw: string option;
      mutable death_text: string option;
      mutable burial_date_raw: string option;
      mutable burial_text: string option;
      mutable cremation_text: string option;
      mutable burial_type: Api_saisie_read_piqi.burial_type;
      mutable titles_links: string list;
      mutable sosa_nb: string option;
      mutable has_history: bool;
      mutable has_possible_duplications: bool;
      mutable ref_index: Api_saisie_read_piqi.protobuf_int32 option;
      mutable linked_page_biblio: string;
      mutable linked_page_bnote: string;
      mutable linked_page_death: string;
      mutable linked_page_head: string;
      mutable linked_page_occu: string;
      mutable visible_for_visitors: bool;
      mutable father: Api_saisie_read_piqi.person option;
      mutable mother: Api_saisie_read_piqi.person option;
    }
  end = Fiche_person
and Family:
  sig
    type t = {
      mutable index: Api_saisie_read_piqi.protobuf_int32;
      mutable spouse: Api_saisie_read_piqi.simple_person;
      mutable marriage_date: string option;
      mutable marriage_date_long: string option;
      mutable marriage_date_raw: string option;
      mutable marriage_date_conv: string option;
      mutable marriage_date_conv_long: string option;
      mutable marriage_date_text: string option;
      mutable marriage_date_cal: Api_saisie_read_piqi.calendar option;
      mutable marriage_place: string option;
      mutable marriage_src: string option;
      mutable marriage_type: Api_saisie_read_piqi.marriage_type;
      mutable divorce_type: Api_saisie_read_piqi.divorce_type;
      mutable divorce_date: string option;
      mutable divorce_date_long: string option;
      mutable divorce_date_raw: string option;
      mutable divorce_date_conv: string option;
      mutable divorce_date_conv_long: string option;
      mutable divorce_date_cal: Api_saisie_read_piqi.calendar option;
      mutable witnesses: Api_saisie_read_piqi.simple_person list;
      mutable notes: string option;
      mutable fsources: string option;
      mutable children: Api_saisie_read_piqi.simple_person list;
    }
  end = Family
and Index_person:
  sig
    type t = {
      mutable index: Api_saisie_read_piqi.protobuf_int32;
      mutable indexz: Api_saisie_read_piqi.protobuf_int32 option;
    }
  end = Index_person
and Node:
  sig
    type t = {
      mutable id: Api_saisie_read_piqi.protobuf_int64;
      mutable person: Api_saisie_read_piqi.person_tree;
      mutable ifam: Api_saisie_read_piqi.protobuf_int64 option;
    }
  end = Node
and Edge:
  sig
    type t = {
      mutable from_node: Api_saisie_read_piqi.protobuf_int64;
      mutable to_node: Api_saisie_read_piqi.protobuf_int64;
    }
  end = Edge
and Graph_tree:
  sig
    type t = {
      mutable nodes_asc: Api_saisie_read_piqi.node list;
      mutable edges_asc: Api_saisie_read_piqi.edge list;
      mutable nodes_desc: Api_saisie_read_piqi.node list;
      mutable edges_desc: Api_saisie_read_piqi.edge list;
      mutable nodes_siblings: Api_saisie_read_piqi.node list;
    }
  end = Graph_tree
and Graph_tree_new:
  sig
    type t = {
      mutable nodes_asc: Api_saisie_read_piqi.node list;
      mutable edges_asc: Api_saisie_read_piqi.edge list;
      mutable nodes_desc: Api_saisie_read_piqi.node list;
      mutable edges_desc: Api_saisie_read_piqi.edge list;
      mutable nodes_siblings: Api_saisie_read_piqi.node list;
      mutable nodes_siblings_before: Api_saisie_read_piqi.node list;
      mutable nodes_siblings_after: Api_saisie_read_piqi.node list;
    }
  end = Graph_tree_new
and Graph_tree_params:
  sig
    type t = {
      mutable identifier_person: Api_saisie_read_piqi.identifier_person;
      mutable nb_asc: Api_saisie_read_piqi.protobuf_int32 option;
      mutable nb_desc: Api_saisie_read_piqi.protobuf_int32 option;
      mutable indexz: Api_saisie_read_piqi.protobuf_int32 option;
    }
  end = Graph_tree_params
and Title:
  sig
    type t = {
      mutable title_type: Api_saisie_read_piqi.title_type;
      mutable name: string option;
      mutable title: string option;
      mutable fief: string option;
      mutable date_begin: string option;
      mutable date_end: string option;
      mutable nth: Api_saisie_read_piqi.protobuf_int32 option;
    }
  end = Title
and Person_tree_full:
  sig
    type t = {
      mutable index: Api_saisie_read_piqi.protobuf_int32;
      mutable sex: Api_saisie_read_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable n: string;
      mutable p: string;
      mutable occ: Api_saisie_read_piqi.protobuf_int32;
      mutable image: string option;
      mutable sosa: Api_saisie_read_piqi.sosa;
      mutable public_name: string option;
      mutable aliases: string list;
      mutable qualifiers: string list;
      mutable firstname_aliases: string list;
      mutable surname_aliases: string list;
      mutable birth_date: string option;
      mutable birth_place: string option;
      mutable birth_src: string option;
      mutable baptism_date: string option;
      mutable baptism_place: string option;
      mutable baptism_src: string option;
      mutable death_date: string option;
      mutable death_place: string option;
      mutable death_src: string option;
      mutable death_type: Api_saisie_read_piqi.death_type;
      mutable burial_date: string option;
      mutable burial_place: string option;
      mutable burial_src: string option;
      mutable occupation: string option;
      mutable psources: string option;
      mutable titles: Api_saisie_read_piqi.title list;
      mutable visible_for_visitors: bool;
      mutable has_more_infos: bool;
      mutable baseprefix: string;
    }
  end = Person_tree_full
and Family_tree_full:
  sig
    type t = {
      mutable fsources: string option;
      mutable marriage_date: string option;
      mutable marriage_place: string option;
      mutable marriage_src: string option;
      mutable marriage_type: Api_saisie_read_piqi.marriage_type;
      mutable divorce_type: Api_saisie_read_piqi.divorce_type;
      mutable divorce_date: string option;
      mutable index: Api_saisie_read_piqi.protobuf_int32;
    }
  end = Family_tree_full
and Node_full:
  sig
    type t = {
      mutable id: Api_saisie_read_piqi.protobuf_int64;
      mutable person: Api_saisie_read_piqi.person_tree_full;
      mutable ifam: Api_saisie_read_piqi.protobuf_int64 option;
    }
  end = Node_full
and Graph_tree_full:
  sig
    type t = {
      mutable nodes_asc: Api_saisie_read_piqi.node_full list;
      mutable edges_asc: Api_saisie_read_piqi.edge list;
      mutable families_asc: Api_saisie_read_piqi.family_tree_full list;
      mutable nodes_desc: Api_saisie_read_piqi.node_full list;
      mutable edges_desc: Api_saisie_read_piqi.edge list;
      mutable families_desc: Api_saisie_read_piqi.family_tree_full list;
      mutable nodes_siblings: Api_saisie_read_piqi.node_full list;
      mutable nodes_siblings_before: Api_saisie_read_piqi.node_full list;
      mutable nodes_siblings_after: Api_saisie_read_piqi.node_full list;
    }
  end = Graph_tree_full
and Identifier_person:
  sig
    type t = {
      mutable index: Api_saisie_read_piqi.protobuf_int32 option;
      mutable n: string option;
      mutable p: string option;
      mutable oc: Api_saisie_read_piqi.protobuf_int32 option;
      mutable track_visit: bool option;
    }
  end = Identifier_person
and Error:
  sig
    type t = {
      mutable code: Api_saisie_read_piqi.error_code;
      mutable message: string option;
    }
  end = Error
and Nb_ancestors:
  sig
    type t = {
      mutable nb: Api_saisie_read_piqi.protobuf_int32;
    }
  end = Nb_ancestors
and Conf_line:
  sig
    type t = {
      mutable key: string;
      mutable value: string;
    }
  end = Conf_line
and Conf_base:
  sig
    type t = {
      mutable line: Api_saisie_read_piqi.conf_line list;
      mutable wizards: string list;
      mutable friends: string list;
    }
  end = Conf_base


let rec parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x

and parse_int64 x = Piqirun.int64_of_zigzag_varint x
and packed_parse_int64 x = Piqirun.int64_of_packed_zigzag_varint x

and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x

and parse_string x = Piqirun.string_of_block x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_protobuf_int64 x = Piqirun.int64_of_signed_varint x
and packed_parse_protobuf_int64 x = Piqirun.int64_of_packed_signed_varint x

and parse_dmy x =
  let x = Piqirun.parse_record x in
  let _day, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _month, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _year, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  let _delta, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Dmy.day = _day;
    Dmy.month = _month;
    Dmy.year = _year;
    Dmy.delta = _delta;
  }

and parse_date x =
  let x = Piqirun.parse_record x in
  let _cal, x = Piqirun.parse_optional_field 2 parse_calendar x in
  let _prec, x = Piqirun.parse_optional_field 3 parse_precision x in
  let _dmy, x = Piqirun.parse_optional_field 4 parse_dmy x in
  let _dmy2, x = Piqirun.parse_optional_field 5 parse_dmy x in
  let _text, x = Piqirun.parse_optional_field 6 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Date.cal = _cal;
    Date.prec = _prec;
    Date.dmy = _dmy;
    Date.dmy2 = _dmy2;
    Date.text = _text;
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
  let _type_, x = Piqirun.parse_required_field 2 parse_event_type x in
  let _date, x = Piqirun.parse_optional_field 3 parse_string x in
  let _date_long, x = Piqirun.parse_optional_field 4 parse_string x in
  let _date_conv, x = Piqirun.parse_optional_field 5 parse_string x in
  let _date_cal, x = Piqirun.parse_optional_field 6 parse_calendar x in
  let _place, x = Piqirun.parse_optional_field 7 parse_string x in
  let _reason, x = Piqirun.parse_optional_field 8 parse_string x in
  let _note, x = Piqirun.parse_optional_field 9 parse_string x in
  let _src, x = Piqirun.parse_optional_field 10 parse_string x in
  let _spouse, x = Piqirun.parse_optional_field 11 parse_simple_person x in
  let _witnesses, x = Piqirun.parse_repeated_field 12 parse_witness_event x in
  let _date_raw, x = Piqirun.parse_optional_field 13 parse_string x in
  let _date_conv_long, x = Piqirun.parse_optional_field 14 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Event.name = _name;
    Event.type_ = _type_;
    Event.date = _date;
    Event.date_long = _date_long;
    Event.date_conv = _date_conv;
    Event.date_cal = _date_cal;
    Event.place = _place;
    Event.reason = _reason;
    Event.note = _note;
    Event.src = _src;
    Event.spouse = _spouse;
    Event.witnesses = _witnesses;
    Event.date_raw = _date_raw;
    Event.date_conv_long = _date_conv_long;
  }

and parse_person_tree x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 2 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 3 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 4 parse_string x in
  let _n, x = Piqirun.parse_required_field 5 parse_string x in
  let _p, x = Piqirun.parse_required_field 6 parse_string x in
  let _occ, x = Piqirun.parse_required_field 7 parse_protobuf_int32 x in
  let _dates, x = Piqirun.parse_optional_field 8 parse_string x in
  let _image, x = Piqirun.parse_optional_field 9 parse_string x in
  let _sosa, x = Piqirun.parse_required_field 10 parse_sosa x in
  let _has_more_infos, x = Piqirun.parse_required_field 11 parse_bool x in
  let _baseprefix, x = Piqirun.parse_required_field 12 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Person_tree.index = _index;
    Person_tree.sex = _sex;
    Person_tree.lastname = _lastname;
    Person_tree.firstname = _firstname;
    Person_tree.n = _n;
    Person_tree.p = _p;
    Person_tree.occ = _occ;
    Person_tree.dates = _dates;
    Person_tree.image = _image;
    Person_tree.sosa = _sosa;
    Person_tree.has_more_infos = _has_more_infos;
    Person_tree.baseprefix = _baseprefix;
  }

and parse_simple_person x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 2 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 3 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 4 parse_string x in
  let _n, x = Piqirun.parse_required_field 5 parse_string x in
  let _p, x = Piqirun.parse_required_field 6 parse_string x in
  let _occ, x = Piqirun.parse_required_field 7 parse_protobuf_int32 x in
  let _birth_short_date, x = Piqirun.parse_optional_field 8 parse_string x in
  let _birth_place, x = Piqirun.parse_optional_field 9 parse_string x in
  let _death_short_date, x = Piqirun.parse_optional_field 10 parse_string x in
  let _death_place, x = Piqirun.parse_optional_field 11 parse_string x in
  let _image, x = Piqirun.parse_optional_field 12 parse_string x in
  let _sosa, x = Piqirun.parse_required_field 13 parse_sosa x in
  let _baseprefix, x = Piqirun.parse_required_field 14 parse_string x in
  let _birth_date_raw, x = Piqirun.parse_optional_field 15 parse_string x in
  let _death_date_raw, x = Piqirun.parse_optional_field 16 parse_string x in
  let _sosa_nb, x = Piqirun.parse_optional_field 17 parse_string x in
  let _visible_for_visitors, x = Piqirun.parse_required_field 18 parse_bool x in
  let _has_parent, x = Piqirun.parse_required_field 19 parse_bool x in
  let _has_spouse, x = Piqirun.parse_required_field 20 parse_bool x in
  let _has_child, x = Piqirun.parse_required_field 21 parse_bool x in
  Piqirun.check_unparsed_fields x;
  {
    Simple_person.index = _index;
    Simple_person.sex = _sex;
    Simple_person.lastname = _lastname;
    Simple_person.firstname = _firstname;
    Simple_person.n = _n;
    Simple_person.p = _p;
    Simple_person.occ = _occ;
    Simple_person.birth_short_date = _birth_short_date;
    Simple_person.birth_place = _birth_place;
    Simple_person.death_short_date = _death_short_date;
    Simple_person.death_place = _death_place;
    Simple_person.image = _image;
    Simple_person.sosa = _sosa;
    Simple_person.baseprefix = _baseprefix;
    Simple_person.birth_date_raw = _birth_date_raw;
    Simple_person.death_date_raw = _death_date_raw;
    Simple_person.sosa_nb = _sosa_nb;
    Simple_person.visible_for_visitors = _visible_for_visitors;
    Simple_person.has_parent = _has_parent;
    Simple_person.has_spouse = _has_spouse;
    Simple_person.has_child = _has_child;
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

and parse_event_witness x =
  let x = Piqirun.parse_record x in
  let _event_witness_type, x = Piqirun.parse_required_field 1 parse_string x in
  let _husband, x = Piqirun.parse_required_field 2 parse_simple_person x in
  let _wife, x = Piqirun.parse_optional_field 3 parse_simple_person x in
  Piqirun.check_unparsed_fields x;
  {
    Event_witness.event_witness_type = _event_witness_type;
    Event_witness.husband = _husband;
    Event_witness.wife = _wife;
  }

and parse_person x =
  let x = Piqirun.parse_record x in
  let _type_, x = Piqirun.parse_required_field 1 parse_person_type x in
  let _index, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 3 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 4 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 5 parse_string x in
  let _n, x = Piqirun.parse_required_field 6 parse_string x in
  let _p, x = Piqirun.parse_required_field 7 parse_string x in
  let _occ, x = Piqirun.parse_required_field 8 parse_protobuf_int32 x in
  let _public_name, x = Piqirun.parse_optional_field 9 parse_string x in
  let _aliases, x = Piqirun.parse_repeated_field 10 parse_string x in
  let _qualifiers, x = Piqirun.parse_repeated_field 11 parse_string x in
  let _firstname_aliases, x = Piqirun.parse_repeated_field 12 parse_string x in
  let _surname_aliases, x = Piqirun.parse_repeated_field 13 parse_string x in
  let _image, x = Piqirun.parse_optional_field 14 parse_string x in
  let _birth_date, x = Piqirun.parse_optional_field 15 parse_string x in
  let _birth_date_conv, x = Piqirun.parse_optional_field 16 parse_string x in
  let _birth_date_cal, x = Piqirun.parse_optional_field 17 parse_calendar x in
  let _birth_place, x = Piqirun.parse_optional_field 18 parse_string x in
  let _birth_src, x = Piqirun.parse_optional_field 19 parse_string x in
  let _baptism_date, x = Piqirun.parse_optional_field 20 parse_string x in
  let _baptism_date_conv, x = Piqirun.parse_optional_field 21 parse_string x in
  let _baptism_date_cal, x = Piqirun.parse_optional_field 22 parse_calendar x in
  let _baptism_place, x = Piqirun.parse_optional_field 23 parse_string x in
  let _baptism_src, x = Piqirun.parse_optional_field 24 parse_string x in
  let _death_date, x = Piqirun.parse_optional_field 25 parse_string x in
  let _death_date_conv, x = Piqirun.parse_optional_field 26 parse_string x in
  let _death_date_cal, x = Piqirun.parse_optional_field 27 parse_calendar x in
  let _death_place, x = Piqirun.parse_optional_field 28 parse_string x in
  let _death_src, x = Piqirun.parse_optional_field 29 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 30 parse_death_type x in
  let _burial_date, x = Piqirun.parse_optional_field 31 parse_string x in
  let _burial_date_conv, x = Piqirun.parse_optional_field 32 parse_string x in
  let _burial_date_cal, x = Piqirun.parse_optional_field 33 parse_calendar x in
  let _burial_place, x = Piqirun.parse_optional_field 34 parse_string x in
  let _burial_src, x = Piqirun.parse_optional_field 35 parse_string x in
  let _occupation, x = Piqirun.parse_optional_field 36 parse_string x in
  let _notes, x = Piqirun.parse_optional_field 37 parse_string x in
  let _psources, x = Piqirun.parse_optional_field 38 parse_string x in
  let _has_sources, x = Piqirun.parse_required_field 39 parse_bool x in
  let _titles, x = Piqirun.parse_repeated_field 40 parse_string x in
  let _related, x = Piqirun.parse_repeated_field 41 parse_relation_person x in
  let _rparents, x = Piqirun.parse_repeated_field 42 parse_relation_person x in
  let _father, x = Piqirun.parse_optional_field 43 parse_simple_person x in
  let _mother, x = Piqirun.parse_optional_field 44 parse_simple_person x in
  let _families, x = Piqirun.parse_repeated_field 45 parse_family x in
  let _sosa, x = Piqirun.parse_required_field 46 parse_sosa x in
  let _events, x = Piqirun.parse_repeated_field 47 parse_event x in
  let _events_witnesses, x = Piqirun.parse_repeated_field 48 parse_event_witness x in
  let _baseprefix, x = Piqirun.parse_required_field 49 parse_string x in
  let _fiche_person_person, x = Piqirun.parse_optional_field 103 parse_fiche_person x in
  Piqirun.check_unparsed_fields x;
  {
    Person.type_ = _type_;
    Person.index = _index;
    Person.sex = _sex;
    Person.lastname = _lastname;
    Person.firstname = _firstname;
    Person.n = _n;
    Person.p = _p;
    Person.occ = _occ;
    Person.public_name = _public_name;
    Person.aliases = _aliases;
    Person.qualifiers = _qualifiers;
    Person.firstname_aliases = _firstname_aliases;
    Person.surname_aliases = _surname_aliases;
    Person.image = _image;
    Person.birth_date = _birth_date;
    Person.birth_date_conv = _birth_date_conv;
    Person.birth_date_cal = _birth_date_cal;
    Person.birth_place = _birth_place;
    Person.birth_src = _birth_src;
    Person.baptism_date = _baptism_date;
    Person.baptism_date_conv = _baptism_date_conv;
    Person.baptism_date_cal = _baptism_date_cal;
    Person.baptism_place = _baptism_place;
    Person.baptism_src = _baptism_src;
    Person.death_date = _death_date;
    Person.death_date_conv = _death_date_conv;
    Person.death_date_cal = _death_date_cal;
    Person.death_place = _death_place;
    Person.death_src = _death_src;
    Person.death_type = _death_type;
    Person.burial_date = _burial_date;
    Person.burial_date_conv = _burial_date_conv;
    Person.burial_date_cal = _burial_date_cal;
    Person.burial_place = _burial_place;
    Person.burial_src = _burial_src;
    Person.occupation = _occupation;
    Person.notes = _notes;
    Person.psources = _psources;
    Person.has_sources = _has_sources;
    Person.titles = _titles;
    Person.related = _related;
    Person.rparents = _rparents;
    Person.father = _father;
    Person.mother = _mother;
    Person.families = _families;
    Person.sosa = _sosa;
    Person.events = _events;
    Person.events_witnesses = _events_witnesses;
    Person.baseprefix = _baseprefix;
    Person.fiche_person_person = _fiche_person_person;
  }

and parse_person_type x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `simple
    | 2l -> `full
    | 3l -> `fiche
    | x -> Piqirun.error_enum_const x
and packed_parse_person_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `simple
    | 2l -> `full
    | 3l -> `fiche
    | x -> Piqirun.error_enum_const x

and parse_fiche_person x =
  let x = Piqirun.parse_record x in
  let _birth_date_raw, x = Piqirun.parse_optional_field 1 parse_string x in
  let _birth_text, x = Piqirun.parse_optional_field 2 parse_string x in
  let _baptism_date_raw, x = Piqirun.parse_optional_field 3 parse_string x in
  let _baptism_text, x = Piqirun.parse_optional_field 4 parse_string x in
  let _death_date_raw, x = Piqirun.parse_optional_field 5 parse_string x in
  let _death_text, x = Piqirun.parse_optional_field 6 parse_string x in
  let _burial_date_raw, x = Piqirun.parse_optional_field 7 parse_string x in
  let _burial_text, x = Piqirun.parse_optional_field 8 parse_string x in
  let _cremation_text, x = Piqirun.parse_optional_field 9 parse_string x in
  let _burial_type, x = Piqirun.parse_required_field 10 parse_burial_type x in
  let _titles_links, x = Piqirun.parse_repeated_field 11 parse_string x in
  let _sosa_nb, x = Piqirun.parse_optional_field 12 parse_string x in
  let _has_history, x = Piqirun.parse_required_field 13 parse_bool x in
  let _has_possible_duplications, x = Piqirun.parse_required_field 14 parse_bool x in
  let _ref_index, x = Piqirun.parse_optional_field 15 parse_protobuf_int32 x in
  let _linked_page_biblio, x = Piqirun.parse_required_field 16 parse_string x in
  let _linked_page_bnote, x = Piqirun.parse_required_field 17 parse_string x in
  let _linked_page_death, x = Piqirun.parse_required_field 18 parse_string x in
  let _linked_page_head, x = Piqirun.parse_required_field 19 parse_string x in
  let _linked_page_occu, x = Piqirun.parse_required_field 20 parse_string x in
  let _visible_for_visitors, x = Piqirun.parse_required_field 21 parse_bool x in
  let _father, x = Piqirun.parse_optional_field 22 parse_person x in
  let _mother, x = Piqirun.parse_optional_field 23 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Fiche_person.birth_date_raw = _birth_date_raw;
    Fiche_person.birth_text = _birth_text;
    Fiche_person.baptism_date_raw = _baptism_date_raw;
    Fiche_person.baptism_text = _baptism_text;
    Fiche_person.death_date_raw = _death_date_raw;
    Fiche_person.death_text = _death_text;
    Fiche_person.burial_date_raw = _burial_date_raw;
    Fiche_person.burial_text = _burial_text;
    Fiche_person.cremation_text = _cremation_text;
    Fiche_person.burial_type = _burial_type;
    Fiche_person.titles_links = _titles_links;
    Fiche_person.sosa_nb = _sosa_nb;
    Fiche_person.has_history = _has_history;
    Fiche_person.has_possible_duplications = _has_possible_duplications;
    Fiche_person.ref_index = _ref_index;
    Fiche_person.linked_page_biblio = _linked_page_biblio;
    Fiche_person.linked_page_bnote = _linked_page_bnote;
    Fiche_person.linked_page_death = _linked_page_death;
    Fiche_person.linked_page_head = _linked_page_head;
    Fiche_person.linked_page_occu = _linked_page_occu;
    Fiche_person.visible_for_visitors = _visible_for_visitors;
    Fiche_person.father = _father;
    Fiche_person.mother = _mother;
  }

and parse_family x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _spouse, x = Piqirun.parse_required_field 2 parse_simple_person x in
  let _marriage_date, x = Piqirun.parse_optional_field 3 parse_string x in
  let _marriage_date_long, x = Piqirun.parse_optional_field 4 parse_string x in
  let _marriage_date_conv, x = Piqirun.parse_optional_field 5 parse_string x in
  let _marriage_date_cal, x = Piqirun.parse_optional_field 6 parse_calendar x in
  let _marriage_place, x = Piqirun.parse_optional_field 7 parse_string x in
  let _marriage_src, x = Piqirun.parse_optional_field 8 parse_string x in
  let _marriage_type, x = Piqirun.parse_required_field 9 parse_marriage_type x in
  let _divorce_type, x = Piqirun.parse_required_field 10 parse_divorce_type x in
  let _divorce_date, x = Piqirun.parse_optional_field 11 parse_string x in
  let _divorce_date_long, x = Piqirun.parse_optional_field 12 parse_string x in
  let _divorce_date_conv, x = Piqirun.parse_optional_field 13 parse_string x in
  let _divorce_date_cal, x = Piqirun.parse_optional_field 14 parse_calendar x in
  let _witnesses, x = Piqirun.parse_repeated_field 15 parse_simple_person x in
  let _notes, x = Piqirun.parse_optional_field 16 parse_string x in
  let _fsources, x = Piqirun.parse_optional_field 17 parse_string x in
  let _children, x = Piqirun.parse_repeated_field 18 parse_simple_person x in
  let _marriage_date_raw, x = Piqirun.parse_optional_field 19 parse_string x in
  let _divorce_date_raw, x = Piqirun.parse_optional_field 20 parse_string x in
  let _marriage_date_conv_long, x = Piqirun.parse_optional_field 21 parse_string x in
  let _divorce_date_conv_long, x = Piqirun.parse_optional_field 22 parse_string x in
  let _marriage_date_text, x = Piqirun.parse_optional_field 23 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Family.index = _index;
    Family.spouse = _spouse;
    Family.marriage_date = _marriage_date;
    Family.marriage_date_long = _marriage_date_long;
    Family.marriage_date_conv = _marriage_date_conv;
    Family.marriage_date_cal = _marriage_date_cal;
    Family.marriage_place = _marriage_place;
    Family.marriage_src = _marriage_src;
    Family.marriage_type = _marriage_type;
    Family.divorce_type = _divorce_type;
    Family.divorce_date = _divorce_date;
    Family.divorce_date_long = _divorce_date_long;
    Family.divorce_date_conv = _divorce_date_conv;
    Family.divorce_date_cal = _divorce_date_cal;
    Family.witnesses = _witnesses;
    Family.notes = _notes;
    Family.fsources = _fsources;
    Family.children = _children;
    Family.marriage_date_raw = _marriage_date_raw;
    Family.divorce_date_raw = _divorce_date_raw;
    Family.marriage_date_conv_long = _marriage_date_conv_long;
    Family.divorce_date_conv_long = _divorce_date_conv_long;
    Family.marriage_date_text = _marriage_date_text;
  }

and parse_index_person x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _indexz, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Index_person.index = _index;
    Index_person.indexz = _indexz;
  }

and parse_node x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _person, x = Piqirun.parse_required_field 2 parse_person_tree x in
  let _ifam, x = Piqirun.parse_optional_field 3 parse_protobuf_int64 x in
  Piqirun.check_unparsed_fields x;
  {
    Node.id = _id;
    Node.person = _person;
    Node.ifam = _ifam;
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

and parse_graph_tree x =
  let x = Piqirun.parse_record x in
  let _nodes_asc, x = Piqirun.parse_repeated_field 1 parse_node x in
  let _edges_asc, x = Piqirun.parse_repeated_field 2 parse_edge x in
  let _nodes_desc, x = Piqirun.parse_repeated_field 3 parse_node x in
  let _edges_desc, x = Piqirun.parse_repeated_field 4 parse_edge x in
  let _nodes_siblings, x = Piqirun.parse_repeated_field 5 parse_node x in
  Piqirun.check_unparsed_fields x;
  {
    Graph_tree.nodes_asc = _nodes_asc;
    Graph_tree.edges_asc = _edges_asc;
    Graph_tree.nodes_desc = _nodes_desc;
    Graph_tree.edges_desc = _edges_desc;
    Graph_tree.nodes_siblings = _nodes_siblings;
  }

and parse_graph_tree_new x =
  let x = Piqirun.parse_record x in
  let _nodes_asc, x = Piqirun.parse_repeated_field 1 parse_node x in
  let _edges_asc, x = Piqirun.parse_repeated_field 2 parse_edge x in
  let _nodes_desc, x = Piqirun.parse_repeated_field 3 parse_node x in
  let _edges_desc, x = Piqirun.parse_repeated_field 4 parse_edge x in
  let _nodes_siblings, x = Piqirun.parse_repeated_field 5 parse_node x in
  let _nodes_siblings_before, x = Piqirun.parse_repeated_field 6 parse_node x in
  let _nodes_siblings_after, x = Piqirun.parse_repeated_field 7 parse_node x in
  Piqirun.check_unparsed_fields x;
  {
    Graph_tree_new.nodes_asc = _nodes_asc;
    Graph_tree_new.edges_asc = _edges_asc;
    Graph_tree_new.nodes_desc = _nodes_desc;
    Graph_tree_new.edges_desc = _edges_desc;
    Graph_tree_new.nodes_siblings = _nodes_siblings;
    Graph_tree_new.nodes_siblings_before = _nodes_siblings_before;
    Graph_tree_new.nodes_siblings_after = _nodes_siblings_after;
  }

and parse_graph_tree_params x =
  let x = Piqirun.parse_record x in
  let _identifier_person, x = Piqirun.parse_required_field 1 parse_identifier_person x in
  let _nb_asc, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _nb_desc, x = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  let _indexz, x = Piqirun.parse_optional_field 4 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Graph_tree_params.identifier_person = _identifier_person;
    Graph_tree_params.nb_asc = _nb_asc;
    Graph_tree_params.nb_desc = _nb_desc;
    Graph_tree_params.indexz = _indexz;
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

and parse_person_tree_full x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 2 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 3 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 4 parse_string x in
  let _n, x = Piqirun.parse_required_field 5 parse_string x in
  let _p, x = Piqirun.parse_required_field 6 parse_string x in
  let _occ, x = Piqirun.parse_required_field 7 parse_protobuf_int32 x in
  let _image, x = Piqirun.parse_optional_field 8 parse_string x in
  let _sosa, x = Piqirun.parse_required_field 9 parse_sosa x in
  let _public_name, x = Piqirun.parse_optional_field 10 parse_string x in
  let _aliases, x = Piqirun.parse_repeated_field 11 parse_string x in
  let _qualifiers, x = Piqirun.parse_repeated_field 12 parse_string x in
  let _firstname_aliases, x = Piqirun.parse_repeated_field 13 parse_string x in
  let _surname_aliases, x = Piqirun.parse_repeated_field 14 parse_string x in
  let _birth_date, x = Piqirun.parse_optional_field 15 parse_string x in
  let _birth_place, x = Piqirun.parse_optional_field 16 parse_string x in
  let _birth_src, x = Piqirun.parse_optional_field 17 parse_string x in
  let _baptism_date, x = Piqirun.parse_optional_field 18 parse_string x in
  let _baptism_place, x = Piqirun.parse_optional_field 19 parse_string x in
  let _baptism_src, x = Piqirun.parse_optional_field 20 parse_string x in
  let _death_date, x = Piqirun.parse_optional_field 21 parse_string x in
  let _death_place, x = Piqirun.parse_optional_field 22 parse_string x in
  let _death_src, x = Piqirun.parse_optional_field 23 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 24 parse_death_type x in
  let _burial_date, x = Piqirun.parse_optional_field 25 parse_string x in
  let _burial_place, x = Piqirun.parse_optional_field 26 parse_string x in
  let _burial_src, x = Piqirun.parse_optional_field 27 parse_string x in
  let _occupation, x = Piqirun.parse_optional_field 28 parse_string x in
  let _psources, x = Piqirun.parse_optional_field 29 parse_string x in
  let _titles, x = Piqirun.parse_repeated_field 30 parse_title x in
  let _visible_for_visitors, x = Piqirun.parse_required_field 31 parse_bool x in
  let _has_more_infos, x = Piqirun.parse_required_field 32 parse_bool x in
  let _baseprefix, x = Piqirun.parse_required_field 33 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Person_tree_full.index = _index;
    Person_tree_full.sex = _sex;
    Person_tree_full.lastname = _lastname;
    Person_tree_full.firstname = _firstname;
    Person_tree_full.n = _n;
    Person_tree_full.p = _p;
    Person_tree_full.occ = _occ;
    Person_tree_full.image = _image;
    Person_tree_full.sosa = _sosa;
    Person_tree_full.public_name = _public_name;
    Person_tree_full.aliases = _aliases;
    Person_tree_full.qualifiers = _qualifiers;
    Person_tree_full.firstname_aliases = _firstname_aliases;
    Person_tree_full.surname_aliases = _surname_aliases;
    Person_tree_full.birth_date = _birth_date;
    Person_tree_full.birth_place = _birth_place;
    Person_tree_full.birth_src = _birth_src;
    Person_tree_full.baptism_date = _baptism_date;
    Person_tree_full.baptism_place = _baptism_place;
    Person_tree_full.baptism_src = _baptism_src;
    Person_tree_full.death_date = _death_date;
    Person_tree_full.death_place = _death_place;
    Person_tree_full.death_src = _death_src;
    Person_tree_full.death_type = _death_type;
    Person_tree_full.burial_date = _burial_date;
    Person_tree_full.burial_place = _burial_place;
    Person_tree_full.burial_src = _burial_src;
    Person_tree_full.occupation = _occupation;
    Person_tree_full.psources = _psources;
    Person_tree_full.titles = _titles;
    Person_tree_full.visible_for_visitors = _visible_for_visitors;
    Person_tree_full.has_more_infos = _has_more_infos;
    Person_tree_full.baseprefix = _baseprefix;
  }

and parse_family_tree_full x =
  let x = Piqirun.parse_record x in
  let _fsources, x = Piqirun.parse_optional_field 1 parse_string x in
  let _marriage_date, x = Piqirun.parse_optional_field 2 parse_string x in
  let _marriage_place, x = Piqirun.parse_optional_field 3 parse_string x in
  let _marriage_src, x = Piqirun.parse_optional_field 4 parse_string x in
  let _marriage_type, x = Piqirun.parse_required_field 5 parse_marriage_type x in
  let _divorce_type, x = Piqirun.parse_required_field 6 parse_divorce_type x in
  let _divorce_date, x = Piqirun.parse_optional_field 7 parse_string x in
  let _index, x = Piqirun.parse_required_field 8 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Family_tree_full.fsources = _fsources;
    Family_tree_full.marriage_date = _marriage_date;
    Family_tree_full.marriage_place = _marriage_place;
    Family_tree_full.marriage_src = _marriage_src;
    Family_tree_full.marriage_type = _marriage_type;
    Family_tree_full.divorce_type = _divorce_type;
    Family_tree_full.divorce_date = _divorce_date;
    Family_tree_full.index = _index;
  }

and parse_node_full x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _person, x = Piqirun.parse_required_field 2 parse_person_tree_full x in
  let _ifam, x = Piqirun.parse_optional_field 3 parse_protobuf_int64 x in
  Piqirun.check_unparsed_fields x;
  {
    Node_full.id = _id;
    Node_full.person = _person;
    Node_full.ifam = _ifam;
  }

and parse_graph_tree_full x =
  let x = Piqirun.parse_record x in
  let _nodes_asc, x = Piqirun.parse_repeated_field 1 parse_node_full x in
  let _edges_asc, x = Piqirun.parse_repeated_field 2 parse_edge x in
  let _families_asc, x = Piqirun.parse_repeated_field 3 parse_family_tree_full x in
  let _nodes_desc, x = Piqirun.parse_repeated_field 4 parse_node_full x in
  let _edges_desc, x = Piqirun.parse_repeated_field 5 parse_edge x in
  let _families_desc, x = Piqirun.parse_repeated_field 6 parse_family_tree_full x in
  let _nodes_siblings, x = Piqirun.parse_repeated_field 7 parse_node_full x in
  let _nodes_siblings_before, x = Piqirun.parse_repeated_field 8 parse_node_full x in
  let _nodes_siblings_after, x = Piqirun.parse_repeated_field 9 parse_node_full x in
  Piqirun.check_unparsed_fields x;
  {
    Graph_tree_full.nodes_asc = _nodes_asc;
    Graph_tree_full.edges_asc = _edges_asc;
    Graph_tree_full.families_asc = _families_asc;
    Graph_tree_full.nodes_desc = _nodes_desc;
    Graph_tree_full.edges_desc = _edges_desc;
    Graph_tree_full.families_desc = _families_desc;
    Graph_tree_full.nodes_siblings = _nodes_siblings;
    Graph_tree_full.nodes_siblings_before = _nodes_siblings_before;
    Graph_tree_full.nodes_siblings_after = _nodes_siblings_after;
  }

and parse_identifier_person x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _n, x = Piqirun.parse_optional_field 2 parse_string x in
  let _p, x = Piqirun.parse_optional_field 3 parse_string x in
  let _oc, x = Piqirun.parse_optional_field 4 parse_protobuf_int32 x in
  let _track_visit, x = Piqirun.parse_optional_field 5 parse_bool x in
  Piqirun.check_unparsed_fields x;
  {
    Identifier_person.index = _index;
    Identifier_person.n = _n;
    Identifier_person.p = _p;
    Identifier_person.oc = _oc;
    Identifier_person.track_visit = _track_visit;
  }

and parse_error x =
  let x = Piqirun.parse_record x in
  let _code, x = Piqirun.parse_required_field 998 parse_error_code x in
  let _message, x = Piqirun.parse_optional_field 999 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Error.code = _code;
    Error.message = _message;
  }

and parse_error_code x =
  match Piqirun.int32_of_signed_varint x with
    | 400l -> `bad_request
    | 401l -> `unauthorized
    | 403l -> `forbidden
    | 404l -> `not_found
    | x -> Piqirun.error_enum_const x
and packed_parse_error_code x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 400l -> `bad_request
    | 401l -> `unauthorized
    | 403l -> `forbidden
    | 404l -> `not_found
    | x -> Piqirun.error_enum_const x

and parse_nb_ancestors x =
  let x = Piqirun.parse_record x in
  let _nb, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Nb_ancestors.nb = _nb;
  }

and parse_conf_line x =
  let x = Piqirun.parse_record x in
  let _key, x = Piqirun.parse_required_field 1 parse_string x in
  let _value, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Conf_line.key = _key;
    Conf_line.value = _value;
  }

and parse_conf_base x =
  let x = Piqirun.parse_record x in
  let _line, x = Piqirun.parse_repeated_field 1 parse_conf_line x in
  let _wizards, x = Piqirun.parse_repeated_field 2 parse_string x in
  let _friends, x = Piqirun.parse_repeated_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Conf_base.line = _line;
    Conf_base.wizards = _wizards;
    Conf_base.friends = _friends;
  }

and parse_sosa x =
  match Piqirun.int32_of_signed_varint x with
    | 2l -> `no_sosa
    | 0l -> `sosa_ref
    | 1l -> `sosa
    | x -> Piqirun.error_enum_const x
and packed_parse_sosa x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 2l -> `no_sosa
    | 0l -> `sosa_ref
    | 1l -> `sosa
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
    | 2l -> `unknown
    | 0l -> `male
    | 1l -> `female
    | x -> Piqirun.error_enum_const x
and packed_parse_sex x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 2l -> `unknown
    | 0l -> `male
    | 1l -> `female
    | x -> Piqirun.error_enum_const x

and parse_death_type x =
  match Piqirun.int32_of_signed_varint x with
    | 4l -> `dont_know_if_dead
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x
and packed_parse_death_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 4l -> `dont_know_if_dead
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x

and parse_burial_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `dont_know
    | 1l -> `buried
    | 2l -> `cremated
    | x -> Piqirun.error_enum_const x
and packed_parse_burial_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `dont_know
    | 1l -> `buried
    | 2l -> `cremated
    | x -> Piqirun.error_enum_const x

and parse_marriage_type x =
  match Piqirun.int32_of_signed_varint x with
    | 4l -> `no_mention
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
    | 5l -> `no_sexes_check_married
    | x -> Piqirun.error_enum_const x
and packed_parse_marriage_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 4l -> `no_mention
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
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

and parse_event_type x =
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
    | 50l -> `epers_custom
    | 100l -> `efam_marriage
    | 101l -> `efam_no_marriage
    | 102l -> `efam_no_mention
    | 103l -> `efam_engage
    | 104l -> `efam_divorce
    | 105l -> `efam_separated
    | 106l -> `efam_annulation
    | 107l -> `efam_marriage_bann
    | 108l -> `efam_marriage_contract
    | 109l -> `efam_marriage_license
    | 110l -> `efam_pacs
    | 111l -> `efam_residence
    | 112l -> `efam_custom
    | x -> Piqirun.error_enum_const x
and packed_parse_event_type x =
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
    | 50l -> `epers_custom
    | 100l -> `efam_marriage
    | 101l -> `efam_no_marriage
    | 102l -> `efam_no_mention
    | 103l -> `efam_engage
    | 104l -> `efam_divorce
    | 105l -> `efam_separated
    | 106l -> `efam_annulation
    | 107l -> `efam_marriage_bann
    | 108l -> `efam_marriage_contract
    | 109l -> `efam_marriage_license
    | 110l -> `efam_pacs
    | 111l -> `efam_residence
    | 112l -> `efam_custom
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


let rec gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x

and gen__int64 code x = Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = Piqirun.int64_to_packed_zigzag_varint x

and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__protobuf_int64 code x = Piqirun.int64_to_signed_varint code x
and packed_gen__protobuf_int64 x = Piqirun.int64_to_packed_signed_varint x

and gen__dmy code x =
  let _day = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Dmy.day in
  let _month = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Dmy.month in
  let _year = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Dmy.year in
  let _delta = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Dmy.delta in
  Piqirun.gen_record code (_day :: _month :: _year :: _delta :: [])

and gen__date code x =
  let _cal = Piqirun.gen_optional_field 2 gen__calendar x.Date.cal in
  let _prec = Piqirun.gen_optional_field 3 gen__precision x.Date.prec in
  let _dmy = Piqirun.gen_optional_field 4 gen__dmy x.Date.dmy in
  let _dmy2 = Piqirun.gen_optional_field 5 gen__dmy x.Date.dmy2 in
  let _text = Piqirun.gen_optional_field 6 gen__string x.Date.text in
  Piqirun.gen_record code (_cal :: _prec :: _dmy :: _dmy2 :: _text :: [])

and gen__witness_event code x =
  let _witness_type = Piqirun.gen_required_field 1 gen__witness_type x.Witness_event.witness_type in
  let _witness = Piqirun.gen_required_field 2 gen__simple_person x.Witness_event.witness in
  Piqirun.gen_record code (_witness_type :: _witness :: [])

and gen__event code x =
  let _name = Piqirun.gen_required_field 1 gen__string x.Event.name in
  let _type_ = Piqirun.gen_required_field 2 gen__event_type x.Event.type_ in
  let _date = Piqirun.gen_optional_field 3 gen__string x.Event.date in
  let _date_long = Piqirun.gen_optional_field 4 gen__string x.Event.date_long in
  let _date_conv = Piqirun.gen_optional_field 5 gen__string x.Event.date_conv in
  let _date_cal = Piqirun.gen_optional_field 6 gen__calendar x.Event.date_cal in
  let _place = Piqirun.gen_optional_field 7 gen__string x.Event.place in
  let _reason = Piqirun.gen_optional_field 8 gen__string x.Event.reason in
  let _note = Piqirun.gen_optional_field 9 gen__string x.Event.note in
  let _src = Piqirun.gen_optional_field 10 gen__string x.Event.src in
  let _spouse = Piqirun.gen_optional_field 11 gen__simple_person x.Event.spouse in
  let _witnesses = Piqirun.gen_repeated_field 12 gen__witness_event x.Event.witnesses in
  let _date_raw = Piqirun.gen_optional_field 13 gen__string x.Event.date_raw in
  let _date_conv_long = Piqirun.gen_optional_field 14 gen__string x.Event.date_conv_long in
  Piqirun.gen_record code (_name :: _type_ :: _date :: _date_long :: _date_conv :: _date_cal :: _place :: _reason :: _note :: _src :: _spouse :: _witnesses :: _date_raw :: _date_conv_long :: [])

and gen__person_tree code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Person_tree.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Person_tree.sex in
  let _lastname = Piqirun.gen_required_field 3 gen__string x.Person_tree.lastname in
  let _firstname = Piqirun.gen_required_field 4 gen__string x.Person_tree.firstname in
  let _n = Piqirun.gen_required_field 5 gen__string x.Person_tree.n in
  let _p = Piqirun.gen_required_field 6 gen__string x.Person_tree.p in
  let _occ = Piqirun.gen_required_field 7 gen__protobuf_int32 x.Person_tree.occ in
  let _dates = Piqirun.gen_optional_field 8 gen__string x.Person_tree.dates in
  let _image = Piqirun.gen_optional_field 9 gen__string x.Person_tree.image in
  let _sosa = Piqirun.gen_required_field 10 gen__sosa x.Person_tree.sosa in
  let _has_more_infos = Piqirun.gen_required_field 11 gen__bool x.Person_tree.has_more_infos in
  let _baseprefix = Piqirun.gen_required_field 12 gen__string x.Person_tree.baseprefix in
  Piqirun.gen_record code (_index :: _sex :: _lastname :: _firstname :: _n :: _p :: _occ :: _dates :: _image :: _sosa :: _has_more_infos :: _baseprefix :: [])

and gen__simple_person code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Simple_person.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Simple_person.sex in
  let _lastname = Piqirun.gen_required_field 3 gen__string x.Simple_person.lastname in
  let _firstname = Piqirun.gen_required_field 4 gen__string x.Simple_person.firstname in
  let _n = Piqirun.gen_required_field 5 gen__string x.Simple_person.n in
  let _p = Piqirun.gen_required_field 6 gen__string x.Simple_person.p in
  let _occ = Piqirun.gen_required_field 7 gen__protobuf_int32 x.Simple_person.occ in
  let _birth_short_date = Piqirun.gen_optional_field 8 gen__string x.Simple_person.birth_short_date in
  let _birth_place = Piqirun.gen_optional_field 9 gen__string x.Simple_person.birth_place in
  let _death_short_date = Piqirun.gen_optional_field 10 gen__string x.Simple_person.death_short_date in
  let _death_place = Piqirun.gen_optional_field 11 gen__string x.Simple_person.death_place in
  let _image = Piqirun.gen_optional_field 12 gen__string x.Simple_person.image in
  let _sosa = Piqirun.gen_required_field 13 gen__sosa x.Simple_person.sosa in
  let _baseprefix = Piqirun.gen_required_field 14 gen__string x.Simple_person.baseprefix in
  let _birth_date_raw = Piqirun.gen_optional_field 15 gen__string x.Simple_person.birth_date_raw in
  let _death_date_raw = Piqirun.gen_optional_field 16 gen__string x.Simple_person.death_date_raw in
  let _sosa_nb = Piqirun.gen_optional_field 17 gen__string x.Simple_person.sosa_nb in
  let _visible_for_visitors = Piqirun.gen_required_field 18 gen__bool x.Simple_person.visible_for_visitors in
  let _has_parent = Piqirun.gen_required_field 19 gen__bool x.Simple_person.has_parent in
  let _has_spouse = Piqirun.gen_required_field 20 gen__bool x.Simple_person.has_spouse in
  let _has_child = Piqirun.gen_required_field 21 gen__bool x.Simple_person.has_child in
  Piqirun.gen_record code (_index :: _sex :: _lastname :: _firstname :: _n :: _p :: _occ :: _birth_short_date :: _birth_place :: _death_short_date :: _death_place :: _image :: _sosa :: _baseprefix :: _birth_date_raw :: _death_date_raw :: _sosa_nb :: _visible_for_visitors :: _has_parent :: _has_spouse :: _has_child :: [])

and gen__relation_person code x =
  let _r_type = Piqirun.gen_required_field 1 gen__relation_type x.Relation_person.r_type in
  let _person = Piqirun.gen_required_field 2 gen__simple_person x.Relation_person.person in
  Piqirun.gen_record code (_r_type :: _person :: [])

and gen__event_witness code x =
  let _event_witness_type = Piqirun.gen_required_field 1 gen__string x.Event_witness.event_witness_type in
  let _husband = Piqirun.gen_required_field 2 gen__simple_person x.Event_witness.husband in
  let _wife = Piqirun.gen_optional_field 3 gen__simple_person x.Event_witness.wife in
  Piqirun.gen_record code (_event_witness_type :: _husband :: _wife :: [])

and gen__person code x =
  let _type_ = Piqirun.gen_required_field 1 gen__person_type x.Person.type_ in
  let _index = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Person.index in
  let _sex = Piqirun.gen_required_field 3 gen__sex x.Person.sex in
  let _lastname = Piqirun.gen_required_field 4 gen__string x.Person.lastname in
  let _firstname = Piqirun.gen_required_field 5 gen__string x.Person.firstname in
  let _n = Piqirun.gen_required_field 6 gen__string x.Person.n in
  let _p = Piqirun.gen_required_field 7 gen__string x.Person.p in
  let _occ = Piqirun.gen_required_field 8 gen__protobuf_int32 x.Person.occ in
  let _public_name = Piqirun.gen_optional_field 9 gen__string x.Person.public_name in
  let _aliases = Piqirun.gen_repeated_field 10 gen__string x.Person.aliases in
  let _qualifiers = Piqirun.gen_repeated_field 11 gen__string x.Person.qualifiers in
  let _firstname_aliases = Piqirun.gen_repeated_field 12 gen__string x.Person.firstname_aliases in
  let _surname_aliases = Piqirun.gen_repeated_field 13 gen__string x.Person.surname_aliases in
  let _image = Piqirun.gen_optional_field 14 gen__string x.Person.image in
  let _birth_date = Piqirun.gen_optional_field 15 gen__string x.Person.birth_date in
  let _birth_date_conv = Piqirun.gen_optional_field 16 gen__string x.Person.birth_date_conv in
  let _birth_date_cal = Piqirun.gen_optional_field 17 gen__calendar x.Person.birth_date_cal in
  let _birth_place = Piqirun.gen_optional_field 18 gen__string x.Person.birth_place in
  let _birth_src = Piqirun.gen_optional_field 19 gen__string x.Person.birth_src in
  let _baptism_date = Piqirun.gen_optional_field 20 gen__string x.Person.baptism_date in
  let _baptism_date_conv = Piqirun.gen_optional_field 21 gen__string x.Person.baptism_date_conv in
  let _baptism_date_cal = Piqirun.gen_optional_field 22 gen__calendar x.Person.baptism_date_cal in
  let _baptism_place = Piqirun.gen_optional_field 23 gen__string x.Person.baptism_place in
  let _baptism_src = Piqirun.gen_optional_field 24 gen__string x.Person.baptism_src in
  let _death_date = Piqirun.gen_optional_field 25 gen__string x.Person.death_date in
  let _death_date_conv = Piqirun.gen_optional_field 26 gen__string x.Person.death_date_conv in
  let _death_date_cal = Piqirun.gen_optional_field 27 gen__calendar x.Person.death_date_cal in
  let _death_place = Piqirun.gen_optional_field 28 gen__string x.Person.death_place in
  let _death_src = Piqirun.gen_optional_field 29 gen__string x.Person.death_src in
  let _death_type = Piqirun.gen_required_field 30 gen__death_type x.Person.death_type in
  let _burial_date = Piqirun.gen_optional_field 31 gen__string x.Person.burial_date in
  let _burial_date_conv = Piqirun.gen_optional_field 32 gen__string x.Person.burial_date_conv in
  let _burial_date_cal = Piqirun.gen_optional_field 33 gen__calendar x.Person.burial_date_cal in
  let _burial_place = Piqirun.gen_optional_field 34 gen__string x.Person.burial_place in
  let _burial_src = Piqirun.gen_optional_field 35 gen__string x.Person.burial_src in
  let _occupation = Piqirun.gen_optional_field 36 gen__string x.Person.occupation in
  let _notes = Piqirun.gen_optional_field 37 gen__string x.Person.notes in
  let _psources = Piqirun.gen_optional_field 38 gen__string x.Person.psources in
  let _has_sources = Piqirun.gen_required_field 39 gen__bool x.Person.has_sources in
  let _titles = Piqirun.gen_repeated_field 40 gen__string x.Person.titles in
  let _related = Piqirun.gen_repeated_field 41 gen__relation_person x.Person.related in
  let _rparents = Piqirun.gen_repeated_field 42 gen__relation_person x.Person.rparents in
  let _father = Piqirun.gen_optional_field 43 gen__simple_person x.Person.father in
  let _mother = Piqirun.gen_optional_field 44 gen__simple_person x.Person.mother in
  let _families = Piqirun.gen_repeated_field 45 gen__family x.Person.families in
  let _sosa = Piqirun.gen_required_field 46 gen__sosa x.Person.sosa in
  let _events = Piqirun.gen_repeated_field 47 gen__event x.Person.events in
  let _events_witnesses = Piqirun.gen_repeated_field 48 gen__event_witness x.Person.events_witnesses in
  let _baseprefix = Piqirun.gen_required_field 49 gen__string x.Person.baseprefix in
  let _fiche_person_person = Piqirun.gen_optional_field 103 gen__fiche_person x.Person.fiche_person_person in
  Piqirun.gen_record code (_type_ :: _index :: _sex :: _lastname :: _firstname :: _n :: _p :: _occ :: _public_name :: _aliases :: _qualifiers :: _firstname_aliases :: _surname_aliases :: _image :: _birth_date :: _birth_date_conv :: _birth_date_cal :: _birth_place :: _birth_src :: _baptism_date :: _baptism_date_conv :: _baptism_date_cal :: _baptism_place :: _baptism_src :: _death_date :: _death_date_conv :: _death_date_cal :: _death_place :: _death_src :: _death_type :: _burial_date :: _burial_date_conv :: _burial_date_cal :: _burial_place :: _burial_src :: _occupation :: _notes :: _psources :: _has_sources :: _titles :: _related :: _rparents :: _father :: _mother :: _families :: _sosa :: _events :: _events_witnesses :: _baseprefix :: _fiche_person_person :: [])

and gen__person_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `simple -> 1l
    | `full -> 2l
    | `fiche -> 3l
  )
and packed_gen__person_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `simple -> 1l
    | `full -> 2l
    | `fiche -> 3l
  )

and gen__fiche_person code x =
  let _birth_date_raw = Piqirun.gen_optional_field 1 gen__string x.Fiche_person.birth_date_raw in
  let _birth_text = Piqirun.gen_optional_field 2 gen__string x.Fiche_person.birth_text in
  let _baptism_date_raw = Piqirun.gen_optional_field 3 gen__string x.Fiche_person.baptism_date_raw in
  let _baptism_text = Piqirun.gen_optional_field 4 gen__string x.Fiche_person.baptism_text in
  let _death_date_raw = Piqirun.gen_optional_field 5 gen__string x.Fiche_person.death_date_raw in
  let _death_text = Piqirun.gen_optional_field 6 gen__string x.Fiche_person.death_text in
  let _burial_date_raw = Piqirun.gen_optional_field 7 gen__string x.Fiche_person.burial_date_raw in
  let _burial_text = Piqirun.gen_optional_field 8 gen__string x.Fiche_person.burial_text in
  let _cremation_text = Piqirun.gen_optional_field 9 gen__string x.Fiche_person.cremation_text in
  let _burial_type = Piqirun.gen_required_field 10 gen__burial_type x.Fiche_person.burial_type in
  let _titles_links = Piqirun.gen_repeated_field 11 gen__string x.Fiche_person.titles_links in
  let _sosa_nb = Piqirun.gen_optional_field 12 gen__string x.Fiche_person.sosa_nb in
  let _has_history = Piqirun.gen_required_field 13 gen__bool x.Fiche_person.has_history in
  let _has_possible_duplications = Piqirun.gen_required_field 14 gen__bool x.Fiche_person.has_possible_duplications in
  let _ref_index = Piqirun.gen_optional_field 15 gen__protobuf_int32 x.Fiche_person.ref_index in
  let _linked_page_biblio = Piqirun.gen_required_field 16 gen__string x.Fiche_person.linked_page_biblio in
  let _linked_page_bnote = Piqirun.gen_required_field 17 gen__string x.Fiche_person.linked_page_bnote in
  let _linked_page_death = Piqirun.gen_required_field 18 gen__string x.Fiche_person.linked_page_death in
  let _linked_page_head = Piqirun.gen_required_field 19 gen__string x.Fiche_person.linked_page_head in
  let _linked_page_occu = Piqirun.gen_required_field 20 gen__string x.Fiche_person.linked_page_occu in
  let _visible_for_visitors = Piqirun.gen_required_field 21 gen__bool x.Fiche_person.visible_for_visitors in
  let _father = Piqirun.gen_optional_field 22 gen__person x.Fiche_person.father in
  let _mother = Piqirun.gen_optional_field 23 gen__person x.Fiche_person.mother in
  Piqirun.gen_record code (_birth_date_raw :: _birth_text :: _baptism_date_raw :: _baptism_text :: _death_date_raw :: _death_text :: _burial_date_raw :: _burial_text :: _cremation_text :: _burial_type :: _titles_links :: _sosa_nb :: _has_history :: _has_possible_duplications :: _ref_index :: _linked_page_biblio :: _linked_page_bnote :: _linked_page_death :: _linked_page_head :: _linked_page_occu :: _visible_for_visitors :: _father :: _mother :: [])

and gen__family code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Family.index in
  let _spouse = Piqirun.gen_required_field 2 gen__simple_person x.Family.spouse in
  let _marriage_date = Piqirun.gen_optional_field 3 gen__string x.Family.marriage_date in
  let _marriage_date_long = Piqirun.gen_optional_field 4 gen__string x.Family.marriage_date_long in
  let _marriage_date_conv = Piqirun.gen_optional_field 5 gen__string x.Family.marriage_date_conv in
  let _marriage_date_cal = Piqirun.gen_optional_field 6 gen__calendar x.Family.marriage_date_cal in
  let _marriage_place = Piqirun.gen_optional_field 7 gen__string x.Family.marriage_place in
  let _marriage_src = Piqirun.gen_optional_field 8 gen__string x.Family.marriage_src in
  let _marriage_type = Piqirun.gen_required_field 9 gen__marriage_type x.Family.marriage_type in
  let _divorce_type = Piqirun.gen_required_field 10 gen__divorce_type x.Family.divorce_type in
  let _divorce_date = Piqirun.gen_optional_field 11 gen__string x.Family.divorce_date in
  let _divorce_date_long = Piqirun.gen_optional_field 12 gen__string x.Family.divorce_date_long in
  let _divorce_date_conv = Piqirun.gen_optional_field 13 gen__string x.Family.divorce_date_conv in
  let _divorce_date_cal = Piqirun.gen_optional_field 14 gen__calendar x.Family.divorce_date_cal in
  let _witnesses = Piqirun.gen_repeated_field 15 gen__simple_person x.Family.witnesses in
  let _notes = Piqirun.gen_optional_field 16 gen__string x.Family.notes in
  let _fsources = Piqirun.gen_optional_field 17 gen__string x.Family.fsources in
  let _children = Piqirun.gen_repeated_field 18 gen__simple_person x.Family.children in
  let _marriage_date_raw = Piqirun.gen_optional_field 19 gen__string x.Family.marriage_date_raw in
  let _divorce_date_raw = Piqirun.gen_optional_field 20 gen__string x.Family.divorce_date_raw in
  let _marriage_date_conv_long = Piqirun.gen_optional_field 21 gen__string x.Family.marriage_date_conv_long in
  let _divorce_date_conv_long = Piqirun.gen_optional_field 22 gen__string x.Family.divorce_date_conv_long in
  let _marriage_date_text = Piqirun.gen_optional_field 23 gen__string x.Family.marriage_date_text in
  Piqirun.gen_record code (_index :: _spouse :: _marriage_date :: _marriage_date_long :: _marriage_date_conv :: _marriage_date_cal :: _marriage_place :: _marriage_src :: _marriage_type :: _divorce_type :: _divorce_date :: _divorce_date_long :: _divorce_date_conv :: _divorce_date_cal :: _witnesses :: _notes :: _fsources :: _children :: _marriage_date_raw :: _divorce_date_raw :: _marriage_date_conv_long :: _divorce_date_conv_long :: _marriage_date_text :: [])

and gen__index_person code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Index_person.index in
  let _indexz = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Index_person.indexz in
  Piqirun.gen_record code (_index :: _indexz :: [])

and gen__node code x =
  let _id = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Node.id in
  let _person = Piqirun.gen_required_field 2 gen__person_tree x.Node.person in
  let _ifam = Piqirun.gen_optional_field 3 gen__protobuf_int64 x.Node.ifam in
  Piqirun.gen_record code (_id :: _person :: _ifam :: [])

and gen__edge code x =
  let _from_node = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Edge.from_node in
  let _to_node = Piqirun.gen_required_field 2 gen__protobuf_int64 x.Edge.to_node in
  Piqirun.gen_record code (_from_node :: _to_node :: [])

and gen__graph_tree code x =
  let _nodes_asc = Piqirun.gen_repeated_field 1 gen__node x.Graph_tree.nodes_asc in
  let _edges_asc = Piqirun.gen_repeated_field 2 gen__edge x.Graph_tree.edges_asc in
  let _nodes_desc = Piqirun.gen_repeated_field 3 gen__node x.Graph_tree.nodes_desc in
  let _edges_desc = Piqirun.gen_repeated_field 4 gen__edge x.Graph_tree.edges_desc in
  let _nodes_siblings = Piqirun.gen_repeated_field 5 gen__node x.Graph_tree.nodes_siblings in
  Piqirun.gen_record code (_nodes_asc :: _edges_asc :: _nodes_desc :: _edges_desc :: _nodes_siblings :: [])

and gen__graph_tree_new code x =
  let _nodes_asc = Piqirun.gen_repeated_field 1 gen__node x.Graph_tree_new.nodes_asc in
  let _edges_asc = Piqirun.gen_repeated_field 2 gen__edge x.Graph_tree_new.edges_asc in
  let _nodes_desc = Piqirun.gen_repeated_field 3 gen__node x.Graph_tree_new.nodes_desc in
  let _edges_desc = Piqirun.gen_repeated_field 4 gen__edge x.Graph_tree_new.edges_desc in
  let _nodes_siblings = Piqirun.gen_repeated_field 5 gen__node x.Graph_tree_new.nodes_siblings in
  let _nodes_siblings_before = Piqirun.gen_repeated_field 6 gen__node x.Graph_tree_new.nodes_siblings_before in
  let _nodes_siblings_after = Piqirun.gen_repeated_field 7 gen__node x.Graph_tree_new.nodes_siblings_after in
  Piqirun.gen_record code (_nodes_asc :: _edges_asc :: _nodes_desc :: _edges_desc :: _nodes_siblings :: _nodes_siblings_before :: _nodes_siblings_after :: [])

and gen__graph_tree_params code x =
  let _identifier_person = Piqirun.gen_required_field 1 gen__identifier_person x.Graph_tree_params.identifier_person in
  let _nb_asc = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Graph_tree_params.nb_asc in
  let _nb_desc = Piqirun.gen_optional_field 3 gen__protobuf_int32 x.Graph_tree_params.nb_desc in
  let _indexz = Piqirun.gen_optional_field 4 gen__protobuf_int32 x.Graph_tree_params.indexz in
  Piqirun.gen_record code (_identifier_person :: _nb_asc :: _nb_desc :: _indexz :: [])

and gen__title code x =
  let _title_type = Piqirun.gen_required_field 1 gen__title_type x.Title.title_type in
  let _name = Piqirun.gen_optional_field 2 gen__string x.Title.name in
  let _title = Piqirun.gen_optional_field 3 gen__string x.Title.title in
  let _fief = Piqirun.gen_optional_field 4 gen__string x.Title.fief in
  let _date_begin = Piqirun.gen_optional_field 5 gen__string x.Title.date_begin in
  let _date_end = Piqirun.gen_optional_field 6 gen__string x.Title.date_end in
  let _nth = Piqirun.gen_optional_field 7 gen__protobuf_int32 x.Title.nth in
  Piqirun.gen_record code (_title_type :: _name :: _title :: _fief :: _date_begin :: _date_end :: _nth :: [])

and gen__person_tree_full code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Person_tree_full.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Person_tree_full.sex in
  let _lastname = Piqirun.gen_required_field 3 gen__string x.Person_tree_full.lastname in
  let _firstname = Piqirun.gen_required_field 4 gen__string x.Person_tree_full.firstname in
  let _n = Piqirun.gen_required_field 5 gen__string x.Person_tree_full.n in
  let _p = Piqirun.gen_required_field 6 gen__string x.Person_tree_full.p in
  let _occ = Piqirun.gen_required_field 7 gen__protobuf_int32 x.Person_tree_full.occ in
  let _image = Piqirun.gen_optional_field 8 gen__string x.Person_tree_full.image in
  let _sosa = Piqirun.gen_required_field 9 gen__sosa x.Person_tree_full.sosa in
  let _public_name = Piqirun.gen_optional_field 10 gen__string x.Person_tree_full.public_name in
  let _aliases = Piqirun.gen_repeated_field 11 gen__string x.Person_tree_full.aliases in
  let _qualifiers = Piqirun.gen_repeated_field 12 gen__string x.Person_tree_full.qualifiers in
  let _firstname_aliases = Piqirun.gen_repeated_field 13 gen__string x.Person_tree_full.firstname_aliases in
  let _surname_aliases = Piqirun.gen_repeated_field 14 gen__string x.Person_tree_full.surname_aliases in
  let _birth_date = Piqirun.gen_optional_field 15 gen__string x.Person_tree_full.birth_date in
  let _birth_place = Piqirun.gen_optional_field 16 gen__string x.Person_tree_full.birth_place in
  let _birth_src = Piqirun.gen_optional_field 17 gen__string x.Person_tree_full.birth_src in
  let _baptism_date = Piqirun.gen_optional_field 18 gen__string x.Person_tree_full.baptism_date in
  let _baptism_place = Piqirun.gen_optional_field 19 gen__string x.Person_tree_full.baptism_place in
  let _baptism_src = Piqirun.gen_optional_field 20 gen__string x.Person_tree_full.baptism_src in
  let _death_date = Piqirun.gen_optional_field 21 gen__string x.Person_tree_full.death_date in
  let _death_place = Piqirun.gen_optional_field 22 gen__string x.Person_tree_full.death_place in
  let _death_src = Piqirun.gen_optional_field 23 gen__string x.Person_tree_full.death_src in
  let _death_type = Piqirun.gen_required_field 24 gen__death_type x.Person_tree_full.death_type in
  let _burial_date = Piqirun.gen_optional_field 25 gen__string x.Person_tree_full.burial_date in
  let _burial_place = Piqirun.gen_optional_field 26 gen__string x.Person_tree_full.burial_place in
  let _burial_src = Piqirun.gen_optional_field 27 gen__string x.Person_tree_full.burial_src in
  let _occupation = Piqirun.gen_optional_field 28 gen__string x.Person_tree_full.occupation in
  let _psources = Piqirun.gen_optional_field 29 gen__string x.Person_tree_full.psources in
  let _titles = Piqirun.gen_repeated_field 30 gen__title x.Person_tree_full.titles in
  let _visible_for_visitors = Piqirun.gen_required_field 31 gen__bool x.Person_tree_full.visible_for_visitors in
  let _has_more_infos = Piqirun.gen_required_field 32 gen__bool x.Person_tree_full.has_more_infos in
  let _baseprefix = Piqirun.gen_required_field 33 gen__string x.Person_tree_full.baseprefix in
  Piqirun.gen_record code (_index :: _sex :: _lastname :: _firstname :: _n :: _p :: _occ :: _image :: _sosa :: _public_name :: _aliases :: _qualifiers :: _firstname_aliases :: _surname_aliases :: _birth_date :: _birth_place :: _birth_src :: _baptism_date :: _baptism_place :: _baptism_src :: _death_date :: _death_place :: _death_src :: _death_type :: _burial_date :: _burial_place :: _burial_src :: _occupation :: _psources :: _titles :: _visible_for_visitors :: _has_more_infos :: _baseprefix :: [])

and gen__family_tree_full code x =
  let _fsources = Piqirun.gen_optional_field 1 gen__string x.Family_tree_full.fsources in
  let _marriage_date = Piqirun.gen_optional_field 2 gen__string x.Family_tree_full.marriage_date in
  let _marriage_place = Piqirun.gen_optional_field 3 gen__string x.Family_tree_full.marriage_place in
  let _marriage_src = Piqirun.gen_optional_field 4 gen__string x.Family_tree_full.marriage_src in
  let _marriage_type = Piqirun.gen_required_field 5 gen__marriage_type x.Family_tree_full.marriage_type in
  let _divorce_type = Piqirun.gen_required_field 6 gen__divorce_type x.Family_tree_full.divorce_type in
  let _divorce_date = Piqirun.gen_optional_field 7 gen__string x.Family_tree_full.divorce_date in
  let _index = Piqirun.gen_required_field 8 gen__protobuf_int32 x.Family_tree_full.index in
  Piqirun.gen_record code (_fsources :: _marriage_date :: _marriage_place :: _marriage_src :: _marriage_type :: _divorce_type :: _divorce_date :: _index :: [])

and gen__node_full code x =
  let _id = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Node_full.id in
  let _person = Piqirun.gen_required_field 2 gen__person_tree_full x.Node_full.person in
  let _ifam = Piqirun.gen_optional_field 3 gen__protobuf_int64 x.Node_full.ifam in
  Piqirun.gen_record code (_id :: _person :: _ifam :: [])

and gen__graph_tree_full code x =
  let _nodes_asc = Piqirun.gen_repeated_field 1 gen__node_full x.Graph_tree_full.nodes_asc in
  let _edges_asc = Piqirun.gen_repeated_field 2 gen__edge x.Graph_tree_full.edges_asc in
  let _families_asc = Piqirun.gen_repeated_field 3 gen__family_tree_full x.Graph_tree_full.families_asc in
  let _nodes_desc = Piqirun.gen_repeated_field 4 gen__node_full x.Graph_tree_full.nodes_desc in
  let _edges_desc = Piqirun.gen_repeated_field 5 gen__edge x.Graph_tree_full.edges_desc in
  let _families_desc = Piqirun.gen_repeated_field 6 gen__family_tree_full x.Graph_tree_full.families_desc in
  let _nodes_siblings = Piqirun.gen_repeated_field 7 gen__node_full x.Graph_tree_full.nodes_siblings in
  let _nodes_siblings_before = Piqirun.gen_repeated_field 8 gen__node_full x.Graph_tree_full.nodes_siblings_before in
  let _nodes_siblings_after = Piqirun.gen_repeated_field 9 gen__node_full x.Graph_tree_full.nodes_siblings_after in
  Piqirun.gen_record code (_nodes_asc :: _edges_asc :: _families_asc :: _nodes_desc :: _edges_desc :: _families_desc :: _nodes_siblings :: _nodes_siblings_before :: _nodes_siblings_after :: [])

and gen__identifier_person code x =
  let _index = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Identifier_person.index in
  let _n = Piqirun.gen_optional_field 2 gen__string x.Identifier_person.n in
  let _p = Piqirun.gen_optional_field 3 gen__string x.Identifier_person.p in
  let _oc = Piqirun.gen_optional_field 4 gen__protobuf_int32 x.Identifier_person.oc in
  let _track_visit = Piqirun.gen_optional_field 5 gen__bool x.Identifier_person.track_visit in
  Piqirun.gen_record code (_index :: _n :: _p :: _oc :: _track_visit :: [])

and gen__error code x =
  let _code = Piqirun.gen_required_field 998 gen__error_code x.Error.code in
  let _message = Piqirun.gen_optional_field 999 gen__string x.Error.message in
  Piqirun.gen_record code (_code :: _message :: [])

and gen__error_code code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `bad_request -> 400l
    | `unauthorized -> 401l
    | `forbidden -> 403l
    | `not_found -> 404l
  )
and packed_gen__error_code x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `bad_request -> 400l
    | `unauthorized -> 401l
    | `forbidden -> 403l
    | `not_found -> 404l
  )

and gen__nb_ancestors code x =
  let _nb = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Nb_ancestors.nb in
  Piqirun.gen_record code (_nb :: [])

and gen__conf_line code x =
  let _key = Piqirun.gen_required_field 1 gen__string x.Conf_line.key in
  let _value = Piqirun.gen_required_field 2 gen__string x.Conf_line.value in
  Piqirun.gen_record code (_key :: _value :: [])

and gen__conf_base code x =
  let _line = Piqirun.gen_repeated_field 1 gen__conf_line x.Conf_base.line in
  let _wizards = Piqirun.gen_repeated_field 2 gen__string x.Conf_base.wizards in
  let _friends = Piqirun.gen_repeated_field 3 gen__string x.Conf_base.friends in
  Piqirun.gen_record code (_line :: _wizards :: _friends :: [])

and gen__sosa code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `no_sosa -> 2l
    | `sosa_ref -> 0l
    | `sosa -> 1l
  )
and packed_gen__sosa x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `no_sosa -> 2l
    | `sosa_ref -> 0l
    | `sosa -> 1l
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
    | `unknown -> 2l
    | `male -> 0l
    | `female -> 1l
  )
and packed_gen__sex x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `unknown -> 2l
    | `male -> 0l
    | `female -> 1l
  )

and gen__death_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `dont_know_if_dead -> 4l
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `of_course_dead -> 5l
  )
and packed_gen__death_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `dont_know_if_dead -> 4l
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `of_course_dead -> 5l
  )

and gen__burial_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `dont_know -> 0l
    | `buried -> 1l
    | `cremated -> 2l
  )
and packed_gen__burial_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `dont_know -> 0l
    | `buried -> 1l
    | `cremated -> 2l
  )

and gen__marriage_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `no_mention -> 4l
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
    | `no_sexes_check_married -> 5l
  )
and packed_gen__marriage_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `no_mention -> 4l
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
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

and gen__event_type code x =
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
    | `epers_custom -> 50l
    | `efam_marriage -> 100l
    | `efam_no_marriage -> 101l
    | `efam_no_mention -> 102l
    | `efam_engage -> 103l
    | `efam_divorce -> 104l
    | `efam_separated -> 105l
    | `efam_annulation -> 106l
    | `efam_marriage_bann -> 107l
    | `efam_marriage_contract -> 108l
    | `efam_marriage_license -> 109l
    | `efam_pacs -> 110l
    | `efam_residence -> 111l
    | `efam_custom -> 112l
  )
and packed_gen__event_type x =
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
    | `epers_custom -> 50l
    | `efam_marriage -> 100l
    | `efam_no_marriage -> 101l
    | `efam_no_mention -> 102l
    | `efam_engage -> 103l
    | `efam_divorce -> 104l
    | `efam_separated -> 105l
    | `efam_annulation -> 106l
    | `efam_marriage_bann -> 107l
    | `efam_marriage_contract -> 108l
    | `efam_marriage_license -> 109l
    | `efam_pacs -> 110l
    | `efam_residence -> 111l
    | `efam_custom -> 112l
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


let gen_int32 x = gen__int32 (-1) x
let gen_int64 x = gen__int64 (-1) x
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
let gen_string x = gen__string (-1) x
let gen_bool x = gen__bool (-1) x
let gen_protobuf_int64 x = gen__protobuf_int64 (-1) x
let gen_dmy x = gen__dmy (-1) x
let gen_date x = gen__date (-1) x
let gen_witness_event x = gen__witness_event (-1) x
let gen_event x = gen__event (-1) x
let gen_person_tree x = gen__person_tree (-1) x
let gen_simple_person x = gen__simple_person (-1) x
let gen_relation_person x = gen__relation_person (-1) x
let gen_event_witness x = gen__event_witness (-1) x
let gen_person x = gen__person (-1) x
let gen_person_type x = gen__person_type (-1) x
let gen_fiche_person x = gen__fiche_person (-1) x
let gen_family x = gen__family (-1) x
let gen_index_person x = gen__index_person (-1) x
let gen_node x = gen__node (-1) x
let gen_edge x = gen__edge (-1) x
let gen_graph_tree x = gen__graph_tree (-1) x
let gen_graph_tree_new x = gen__graph_tree_new (-1) x
let gen_graph_tree_params x = gen__graph_tree_params (-1) x
let gen_title x = gen__title (-1) x
let gen_person_tree_full x = gen__person_tree_full (-1) x
let gen_family_tree_full x = gen__family_tree_full (-1) x
let gen_node_full x = gen__node_full (-1) x
let gen_graph_tree_full x = gen__graph_tree_full (-1) x
let gen_identifier_person x = gen__identifier_person (-1) x
let gen_error x = gen__error (-1) x
let gen_error_code x = gen__error_code (-1) x
let gen_nb_ancestors x = gen__nb_ancestors (-1) x
let gen_conf_line x = gen__conf_line (-1) x
let gen_conf_base x = gen__conf_base (-1) x
let gen_sosa x = gen__sosa (-1) x
let gen_calendar x = gen__calendar (-1) x
let gen_precision x = gen__precision (-1) x
let gen_sex x = gen__sex (-1) x
let gen_death_type x = gen__death_type (-1) x
let gen_burial_type x = gen__burial_type (-1) x
let gen_marriage_type x = gen__marriage_type (-1) x
let gen_divorce_type x = gen__divorce_type (-1) x
let gen_relation_type x = gen__relation_type (-1) x
let gen_witness_type x = gen__witness_type (-1) x
let gen_event_type x = gen__event_type (-1) x
let gen_title_type x = gen__title_type (-1) x


let rec default_int32 () = 0l
and default_int64 () = 0L
and default_protobuf_int32 () = default_int32 ()
and default_string () = ""
and default_bool () = false
and default_protobuf_int64 () = default_int64 ()
and default_dmy () =
  {
    Dmy.day = default_protobuf_int32 ();
    Dmy.month = default_protobuf_int32 ();
    Dmy.year = default_protobuf_int32 ();
    Dmy.delta = default_protobuf_int32 ();
  }
and default_date () =
  {
    Date.cal = None;
    Date.prec = None;
    Date.dmy = None;
    Date.dmy2 = None;
    Date.text = None;
  }
and default_witness_event () =
  {
    Witness_event.witness_type = default_witness_type ();
    Witness_event.witness = default_simple_person ();
  }
and default_event () =
  {
    Event.name = default_string ();
    Event.type_ = default_event_type ();
    Event.date = None;
    Event.date_long = None;
    Event.date_conv = None;
    Event.date_cal = None;
    Event.place = None;
    Event.reason = None;
    Event.note = None;
    Event.src = None;
    Event.spouse = None;
    Event.witnesses = [];
    Event.date_raw = None;
    Event.date_conv_long = None;
  }
and default_person_tree () =
  {
    Person_tree.index = default_protobuf_int32 ();
    Person_tree.sex = default_sex ();
    Person_tree.lastname = default_string ();
    Person_tree.firstname = default_string ();
    Person_tree.n = default_string ();
    Person_tree.p = default_string ();
    Person_tree.occ = default_protobuf_int32 ();
    Person_tree.dates = None;
    Person_tree.image = None;
    Person_tree.sosa = default_sosa ();
    Person_tree.has_more_infos = default_bool ();
    Person_tree.baseprefix = default_string ();
  }
and default_simple_person () =
  {
    Simple_person.index = default_protobuf_int32 ();
    Simple_person.sex = default_sex ();
    Simple_person.lastname = default_string ();
    Simple_person.firstname = default_string ();
    Simple_person.n = default_string ();
    Simple_person.p = default_string ();
    Simple_person.occ = default_protobuf_int32 ();
    Simple_person.birth_short_date = None;
    Simple_person.birth_place = None;
    Simple_person.death_short_date = None;
    Simple_person.death_place = None;
    Simple_person.image = None;
    Simple_person.sosa = default_sosa ();
    Simple_person.baseprefix = default_string ();
    Simple_person.birth_date_raw = None;
    Simple_person.death_date_raw = None;
    Simple_person.sosa_nb = None;
    Simple_person.visible_for_visitors = default_bool ();
    Simple_person.has_parent = default_bool ();
    Simple_person.has_spouse = default_bool ();
    Simple_person.has_child = default_bool ();
  }
and default_relation_person () =
  {
    Relation_person.r_type = default_relation_type ();
    Relation_person.person = default_simple_person ();
  }
and default_event_witness () =
  {
    Event_witness.event_witness_type = default_string ();
    Event_witness.husband = default_simple_person ();
    Event_witness.wife = None;
  }
and default_person () =
  {
    Person.type_ = default_person_type ();
    Person.index = default_protobuf_int32 ();
    Person.sex = default_sex ();
    Person.lastname = default_string ();
    Person.firstname = default_string ();
    Person.n = default_string ();
    Person.p = default_string ();
    Person.occ = default_protobuf_int32 ();
    Person.public_name = None;
    Person.aliases = [];
    Person.qualifiers = [];
    Person.firstname_aliases = [];
    Person.surname_aliases = [];
    Person.image = None;
    Person.birth_date = None;
    Person.birth_date_conv = None;
    Person.birth_date_cal = None;
    Person.birth_place = None;
    Person.birth_src = None;
    Person.baptism_date = None;
    Person.baptism_date_conv = None;
    Person.baptism_date_cal = None;
    Person.baptism_place = None;
    Person.baptism_src = None;
    Person.death_date = None;
    Person.death_date_conv = None;
    Person.death_date_cal = None;
    Person.death_place = None;
    Person.death_src = None;
    Person.death_type = default_death_type ();
    Person.burial_date = None;
    Person.burial_date_conv = None;
    Person.burial_date_cal = None;
    Person.burial_place = None;
    Person.burial_src = None;
    Person.occupation = None;
    Person.notes = None;
    Person.psources = None;
    Person.has_sources = default_bool ();
    Person.titles = [];
    Person.related = [];
    Person.rparents = [];
    Person.father = None;
    Person.mother = None;
    Person.families = [];
    Person.sosa = default_sosa ();
    Person.events = [];
    Person.events_witnesses = [];
    Person.baseprefix = default_string ();
    Person.fiche_person_person = None;
  }
and default_person_type () = `simple
and default_fiche_person () =
  {
    Fiche_person.birth_date_raw = None;
    Fiche_person.birth_text = None;
    Fiche_person.baptism_date_raw = None;
    Fiche_person.baptism_text = None;
    Fiche_person.death_date_raw = None;
    Fiche_person.death_text = None;
    Fiche_person.burial_date_raw = None;
    Fiche_person.burial_text = None;
    Fiche_person.cremation_text = None;
    Fiche_person.burial_type = default_burial_type ();
    Fiche_person.titles_links = [];
    Fiche_person.sosa_nb = None;
    Fiche_person.has_history = default_bool ();
    Fiche_person.has_possible_duplications = default_bool ();
    Fiche_person.ref_index = None;
    Fiche_person.linked_page_biblio = default_string ();
    Fiche_person.linked_page_bnote = default_string ();
    Fiche_person.linked_page_death = default_string ();
    Fiche_person.linked_page_head = default_string ();
    Fiche_person.linked_page_occu = default_string ();
    Fiche_person.visible_for_visitors = default_bool ();
    Fiche_person.father = None;
    Fiche_person.mother = None;
  }
and default_family () =
  {
    Family.index = default_protobuf_int32 ();
    Family.spouse = default_simple_person ();
    Family.marriage_date = None;
    Family.marriage_date_long = None;
    Family.marriage_date_conv = None;
    Family.marriage_date_cal = None;
    Family.marriage_place = None;
    Family.marriage_src = None;
    Family.marriage_type = default_marriage_type ();
    Family.divorce_type = default_divorce_type ();
    Family.divorce_date = None;
    Family.divorce_date_long = None;
    Family.divorce_date_conv = None;
    Family.divorce_date_cal = None;
    Family.witnesses = [];
    Family.notes = None;
    Family.fsources = None;
    Family.children = [];
    Family.marriage_date_raw = None;
    Family.divorce_date_raw = None;
    Family.marriage_date_conv_long = None;
    Family.divorce_date_conv_long = None;
    Family.marriage_date_text = None;
  }
and default_index_person () =
  {
    Index_person.index = default_protobuf_int32 ();
    Index_person.indexz = None;
  }
and default_node () =
  {
    Node.id = default_protobuf_int64 ();
    Node.person = default_person_tree ();
    Node.ifam = None;
  }
and default_edge () =
  {
    Edge.from_node = default_protobuf_int64 ();
    Edge.to_node = default_protobuf_int64 ();
  }
and default_graph_tree () =
  {
    Graph_tree.nodes_asc = [];
    Graph_tree.edges_asc = [];
    Graph_tree.nodes_desc = [];
    Graph_tree.edges_desc = [];
    Graph_tree.nodes_siblings = [];
  }
and default_graph_tree_new () =
  {
    Graph_tree_new.nodes_asc = [];
    Graph_tree_new.edges_asc = [];
    Graph_tree_new.nodes_desc = [];
    Graph_tree_new.edges_desc = [];
    Graph_tree_new.nodes_siblings = [];
    Graph_tree_new.nodes_siblings_before = [];
    Graph_tree_new.nodes_siblings_after = [];
  }
and default_graph_tree_params () =
  {
    Graph_tree_params.identifier_person = default_identifier_person ();
    Graph_tree_params.nb_asc = None;
    Graph_tree_params.nb_desc = None;
    Graph_tree_params.indexz = None;
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
and default_person_tree_full () =
  {
    Person_tree_full.index = default_protobuf_int32 ();
    Person_tree_full.sex = default_sex ();
    Person_tree_full.lastname = default_string ();
    Person_tree_full.firstname = default_string ();
    Person_tree_full.n = default_string ();
    Person_tree_full.p = default_string ();
    Person_tree_full.occ = default_protobuf_int32 ();
    Person_tree_full.image = None;
    Person_tree_full.sosa = default_sosa ();
    Person_tree_full.public_name = None;
    Person_tree_full.aliases = [];
    Person_tree_full.qualifiers = [];
    Person_tree_full.firstname_aliases = [];
    Person_tree_full.surname_aliases = [];
    Person_tree_full.birth_date = None;
    Person_tree_full.birth_place = None;
    Person_tree_full.birth_src = None;
    Person_tree_full.baptism_date = None;
    Person_tree_full.baptism_place = None;
    Person_tree_full.baptism_src = None;
    Person_tree_full.death_date = None;
    Person_tree_full.death_place = None;
    Person_tree_full.death_src = None;
    Person_tree_full.death_type = default_death_type ();
    Person_tree_full.burial_date = None;
    Person_tree_full.burial_place = None;
    Person_tree_full.burial_src = None;
    Person_tree_full.occupation = None;
    Person_tree_full.psources = None;
    Person_tree_full.titles = [];
    Person_tree_full.visible_for_visitors = default_bool ();
    Person_tree_full.has_more_infos = default_bool ();
    Person_tree_full.baseprefix = default_string ();
  }
and default_family_tree_full () =
  {
    Family_tree_full.fsources = None;
    Family_tree_full.marriage_date = None;
    Family_tree_full.marriage_place = None;
    Family_tree_full.marriage_src = None;
    Family_tree_full.marriage_type = default_marriage_type ();
    Family_tree_full.divorce_type = default_divorce_type ();
    Family_tree_full.divorce_date = None;
    Family_tree_full.index = default_protobuf_int32 ();
  }
and default_node_full () =
  {
    Node_full.id = default_protobuf_int64 ();
    Node_full.person = default_person_tree_full ();
    Node_full.ifam = None;
  }
and default_graph_tree_full () =
  {
    Graph_tree_full.nodes_asc = [];
    Graph_tree_full.edges_asc = [];
    Graph_tree_full.families_asc = [];
    Graph_tree_full.nodes_desc = [];
    Graph_tree_full.edges_desc = [];
    Graph_tree_full.families_desc = [];
    Graph_tree_full.nodes_siblings = [];
    Graph_tree_full.nodes_siblings_before = [];
    Graph_tree_full.nodes_siblings_after = [];
  }
and default_identifier_person () =
  {
    Identifier_person.index = None;
    Identifier_person.n = None;
    Identifier_person.p = None;
    Identifier_person.oc = None;
    Identifier_person.track_visit = None;
  }
and default_error () =
  {
    Error.code = default_error_code ();
    Error.message = None;
  }
and default_error_code () = `bad_request
and default_nb_ancestors () =
  {
    Nb_ancestors.nb = default_protobuf_int32 ();
  }
and default_conf_line () =
  {
    Conf_line.key = default_string ();
    Conf_line.value = default_string ();
  }
and default_conf_base () =
  {
    Conf_base.line = [];
    Conf_base.wizards = [];
    Conf_base.friends = [];
  }
and default_sosa () = `no_sosa
and default_calendar () = `gregorian
and default_precision () = `sure
and default_sex () = `unknown
and default_death_type () = `dont_know_if_dead
and default_burial_type () = `dont_know
and default_marriage_type () = `no_mention
and default_divorce_type () = `not_divorced
and default_relation_type () = `rparent_adoption
and default_witness_type () = `witness
and default_event_type () = `epers_birth
and default_title_type () = `title_main


let piqi = "\226\202\2304\015api_saisie_read\226\231\249\238\001\026api_saisie_read.proto.piqi\162\244\146\155\011\030geneweb.api.saisie_read.object\218\244\134\182\012\217\001\138\233\142\251\014\210\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003day\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005month\210\171\158\194\006\014protobuf-int32\210\203\242$-\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004year\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005delta\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\003dmy\218\244\134\182\012\224\001\138\233\142\251\014\217\001\210\203\242$&\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003cal\210\171\158\194\006\bcalendar\210\203\242$(\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004prec\210\171\158\194\006\tprecision\210\203\242$!\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003dmy\210\171\158\194\006\003dmy\210\203\242$\"\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004dmy2\210\171\158\194\006\003dmy\210\203\242$%\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004text\210\171\158\194\006\006string\218\164\238\191\004\004date\218\244\134\182\012\133\001\138\233\142\251\014\127\210\203\242$3\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012witness-type\210\171\158\194\006\012witness-type\210\203\242$/\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007witness\210\171\158\194\006\rsimple-person\218\164\238\191\004\rwitness-event\218\244\134\182\012\151\005\138\233\142\251\014\144\005\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004name\210\171\158\194\006\006string\210\203\242$)\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004type\210\171\158\194\006\nevent-type\210\203\242$%\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004date\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tdate-long\210\171\158\194\006\006string\210\203\242$)\232\146\150q\026\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bdate-raw\210\171\158\194\006\006string\210\203\242$*\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tdate-conv\210\171\158\194\006\006string\210\203\242$/\232\146\150q\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014date-conv-long\210\171\158\194\006\006string\210\203\242$+\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bdate-cal\210\171\158\194\006\bcalendar\210\203\242$&\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005place\210\171\158\194\006\006string\210\203\242$'\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006reason\210\171\158\194\006\006string\210\203\242$%\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004note\210\171\158\194\006\006string\210\203\242$$\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003src\210\171\158\194\006\006string\210\203\242$.\232\146\150q\022\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006spouse\210\171\158\194\006\rsimple-person\210\203\242$1\232\146\150q\024\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\rwitness-event\218\164\238\191\004\005event\218\244\134\182\012\173\004\138\233\142\251\014\166\004\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$&\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005dates\210\171\158\194\006\006string\210\203\242$&\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$#\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\210\203\242$-\232\146\150q\022\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\014has-more-infos\210\171\158\194\006\004bool\210\203\242$+\232\146\150q\024\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\218\164\238\191\004\011person-tree\218\244\134\182\012\246\007\138\233\142\251\014\239\007\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$1\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016birth-short-date\210\171\158\194\006\006string\210\203\242$/\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014birth-date-raw\210\171\158\194\006\006string\210\203\242$,\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$1\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016death-short-date\210\171\158\194\006\006string\210\203\242$/\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014death-date-raw\210\171\158\194\006\006string\210\203\242$,\232\146\150q\022\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$&\232\146\150q\024\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$#\232\146\150q\026\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\210\203\242$+\232\146\150q\028\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\210\203\242$(\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007sosa-nb\210\171\158\194\006\006string\210\203\242$3\232\146\150q$\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\020visible-for-visitors\210\171\158\194\006\004bool\210\203\242$)\232\146\150q&\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nhas-parent\210\171\158\194\006\004bool\210\203\242$)\232\146\150q(\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nhas-spouse\210\171\158\194\006\004bool\210\203\242$(\232\146\150q*\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\thas-child\210\171\158\194\006\004bool\218\164\238\191\004\rsimple-person\218\244\134\182\012\129\001\138\233\142\251\014{\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006r-type\210\171\158\194\006\rrelation-type\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\rsimple-person\218\164\238\191\004\015relation-person\218\244\134\182\012\183\001\138\233\142\251\014\176\001\210\203\242$3\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\018event-witness-type\210\171\158\194\006\006string\210\203\242$/\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007husband\210\171\158\194\006\rsimple-person\210\203\242$,\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004wife\210\171\158\194\006\rsimple-person\218\164\238\191\004\revent-witness\218\244\134\182\012\176\019\138\233\142\251\014\169\019\210\203\242$*\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004type\210\171\158\194\006\011person-type\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$,\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$,\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$(\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007aliases\210\171\158\194\006\006string\210\203\242$+\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nqualifiers\210\171\158\194\006\006string\210\203\242$2\232\146\150q\024\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\017firstname-aliases\210\171\158\194\006\006string\210\203\242$0\232\146\150q\026\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015surname-aliases\210\171\158\194\006\006string\210\203\242$&\232\146\150q\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$+\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nbirth-date\210\171\158\194\006\006string\210\203\242$0\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015birth-date-conv\210\171\158\194\006\006string\210\203\242$1\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014birth-date-cal\210\171\158\194\006\bcalendar\210\203\242$,\232\146\150q$\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q&\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tbirth-src\210\171\158\194\006\006string\210\203\242$-\232\146\150q(\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012baptism-date\210\171\158\194\006\006string\210\203\242$2\232\146\150q*\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017baptism-date-conv\210\171\158\194\006\006string\210\203\242$3\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016baptism-date-cal\210\171\158\194\006\bcalendar\210\203\242$.\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rbaptism-place\210\171\158\194\006\006string\210\203\242$,\232\146\150q0\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011baptism-src\210\171\158\194\006\006string\210\203\242$+\232\146\150q2\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeath-date\210\171\158\194\006\006string\210\203\242$0\232\146\150q4\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015death-date-conv\210\171\158\194\006\006string\210\203\242$1\232\146\150q6\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014death-date-cal\210\171\158\194\006\bcalendar\210\203\242$,\232\146\150q8\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q:\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tdeath-src\210\171\158\194\006\006string\210\203\242$/\232\146\150q<\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-type\210\171\158\194\006\ndeath-type\210\203\242$,\232\146\150q>\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011burial-date\210\171\158\194\006\006string\210\203\242$1\232\146\150q@\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016burial-date-conv\210\171\158\194\006\006string\210\203\242$2\232\146\150qB\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015burial-date-cal\210\171\158\194\006\bcalendar\210\203\242$-\232\146\150qD\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012burial-place\210\171\158\194\006\006string\210\203\242$+\232\146\150qF\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nburial-src\210\171\158\194\006\006string\210\203\242$+\232\146\150qH\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\noccupation\210\171\158\194\006\006string\210\203\242$&\232\146\150qJ\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005notes\210\171\158\194\006\006string\210\203\242$)\232\146\150qL\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bpsources\210\171\158\194\006\006string\210\203\242$*\232\146\150qN\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011has-sources\210\171\158\194\006\004bool\210\203\242$'\232\146\150qP\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006titles\210\171\158\194\006\006string\210\203\242$1\232\146\150qR\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007related\210\171\158\194\006\015relation-person\210\203\242$2\232\146\150qT\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\brparents\210\171\158\194\006\015relation-person\210\203\242$.\232\146\150qV\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006father\210\171\158\194\006\rsimple-person\210\203\242$.\232\146\150qX\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006mother\210\171\158\194\006\rsimple-person\210\203\242$)\232\146\150qZ\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\006family\210\203\242$#\232\146\150q\\\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\210\203\242$&\232\146\150q^\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006events\210\171\158\194\006\005event\210\203\242$8\232\146\150q`\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\016events-witnesses\210\171\158\194\006\revent-witness\210\203\242$+\232\146\150qb\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\210\203\242$;\232\146\150q\206\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\019fiche-person-person\210\171\158\194\006\012fiche-person\218\164\238\191\004\006person\218\244\134\182\012\144\001\138\176\205\197\001\137\001\218\164\238\191\004\011person-type\170\183\218\222\005$\232\146\150q\002\234\188\204\215\002\rperson_simple\218\164\238\191\004\006simple\170\183\218\222\005 \232\146\150q\004\234\188\204\215\002\011person_full\218\164\238\191\004\004full\170\183\218\222\005\"\232\146\150q\006\234\188\204\215\002\012person_fiche\218\164\238\191\004\005fiche\218\244\134\182\012\189\t\138\233\142\251\014\182\t\210\203\242$/\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014birth-date-raw\210\171\158\194\006\006string\210\203\242$+\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nbirth-text\210\171\158\194\006\006string\210\203\242$1\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016baptism-date-raw\210\171\158\194\006\006string\210\203\242$-\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012baptism-text\210\171\158\194\006\006string\210\203\242$/\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014death-date-raw\210\171\158\194\006\006string\210\203\242$+\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeath-text\210\171\158\194\006\006string\210\203\242$0\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015burial-date-raw\210\171\158\194\006\006string\210\203\242$,\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011burial-text\210\171\158\194\006\006string\210\203\242$/\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014cremation-text\210\171\158\194\006\006string\210\203\242$1\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011burial-type\210\171\158\194\006\011burial-type\210\203\242$-\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012titles-links\210\171\158\194\006\006string\210\203\242$(\232\146\150q\024\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007sosa-nb\210\171\158\194\006\006string\210\203\242$*\232\146\150q\026\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011has-history\210\171\158\194\006\004bool\210\203\242$8\232\146\150q\028\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\025has-possible-duplications\210\171\158\194\006\004bool\210\203\242$2\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tref-index\210\171\158\194\006\014protobuf-int32\210\203\242$3\232\146\150q \152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\018linked-page-biblio\210\171\158\194\006\006string\210\203\242$2\232\146\150q\"\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\017linked-page-bnote\210\171\158\194\006\006string\210\203\242$2\232\146\150q$\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\017linked-page-death\210\171\158\194\006\006string\210\203\242$1\232\146\150q&\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016linked-page-head\210\171\158\194\006\006string\210\203\242$1\232\146\150q(\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016linked-page-occu\210\171\158\194\006\006string\210\203\242$3\232\146\150q*\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\020visible-for-visitors\210\171\158\194\006\004bool\210\203\242$'\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006father\210\171\158\194\006\006person\210\203\242$'\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006mother\210\171\158\194\006\006person\218\164\238\191\004\012fiche-person\218\244\134\182\012\231\t\138\233\142\251\014\224\t\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006spouse\210\171\158\194\006\rsimple-person\210\203\242$.\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rmarriage-date\210\171\158\194\006\006string\210\203\242$3\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\018marriage-date-long\210\171\158\194\006\006string\210\203\242$2\232\146\150q&\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017marriage-date-raw\210\171\158\194\006\006string\210\203\242$3\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\018marriage-date-conv\210\171\158\194\006\006string\210\203\242$8\232\146\150q*\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\023marriage-date-conv-long\210\171\158\194\006\006string\210\203\242$3\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\018marriage-date-text\210\171\158\194\006\006string\210\203\242$4\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017marriage-date-cal\210\171\158\194\006\bcalendar\210\203\242$/\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014marriage-place\210\171\158\194\006\006string\210\203\242$-\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012marriage-src\210\171\158\194\006\006string\210\203\242$5\232\146\150q\018\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rmarriage-type\210\171\158\194\006\rmarriage-type\210\203\242$3\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012divorce-type\210\171\158\194\006\012divorce-type\210\203\242$-\232\146\150q\022\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012divorce-date\210\171\158\194\006\006string\210\203\242$2\232\146\150q\024\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017divorce-date-long\210\171\158\194\006\006string\210\203\242$1\232\146\150q(\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016divorce-date-raw\210\171\158\194\006\006string\210\203\242$2\232\146\150q\026\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017divorce-date-conv\210\171\158\194\006\006string\210\203\242$7\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\022divorce-date-conv-long\210\171\158\194\006\006string\210\203\242$3\232\146\150q\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016divorce-date-cal\210\171\158\194\006\bcalendar\210\203\242$1\232\146\150q\030\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\rsimple-person\210\203\242$&\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005notes\210\171\158\194\006\006string\210\203\242$)\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bfsources\210\171\158\194\006\006string\210\203\242$0\232\146\150q$\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bchildren\210\171\158\194\006\rsimple-person\218\164\238\191\004\006family\218\244\134\182\012\127\138\233\142\251\014y\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$/\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006indexz\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\012index-person\218\244\134\182\012\164\001\138\233\142\251\014\157\001\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002id\210\171\158\194\006\014protobuf-int64\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011person-tree\210\203\242$-\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004ifam\210\171\158\194\006\014protobuf-int64\218\164\238\191\004\004node\218\244\134\182\012|\138\233\142\251\014v\210\203\242$2\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfrom-node\210\171\158\194\006\014protobuf-int64\210\203\242$0\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007to-node\210\171\158\194\006\014protobuf-int64\218\164\238\191\004\004edge\218\244\134\182\012\255\001\138\233\142\251\014\248\001\210\203\242$(\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\tnodes-asc\210\171\158\194\006\004node\210\203\242$(\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\tedges-asc\210\171\158\194\006\004edge\210\203\242$)\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nnodes-desc\210\171\158\194\006\004node\210\203\242$)\232\146\150q\b\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nedges-desc\210\171\158\194\006\004edge\210\203\242$-\232\146\150q\n\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\014nodes-siblings\210\171\158\194\006\004node\218\164\238\191\004\ngraph-tree\218\244\134\182\012\244\002\138\233\142\251\014\237\002\210\203\242$(\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\tnodes-asc\210\171\158\194\006\004node\210\203\242$(\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\tedges-asc\210\171\158\194\006\004edge\210\203\242$)\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nnodes-desc\210\171\158\194\006\004node\210\203\242$)\232\146\150q\b\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nedges-desc\210\171\158\194\006\004edge\210\203\242$-\232\146\150q\n\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\014nodes-siblings\210\171\158\194\006\004node\210\203\242$4\232\146\150q\012\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\021nodes-siblings-before\210\171\158\194\006\004node\210\203\242$3\232\146\150q\014\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\020nodes-siblings-after\210\171\158\194\006\004node\218\164\238\191\004\014graph-tree-new\218\244\134\182\012\253\001\138\233\142\251\014\246\001\210\203\242$=\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\017identifier-person\210\171\158\194\006\017identifier-person\210\203\242$/\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006nb-asc\210\171\158\194\006\014protobuf-int32\210\203\242$0\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007nb-desc\210\171\158\194\006\014protobuf-int32\210\203\242$/\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006indexz\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\017graph-tree-params\218\244\134\182\012\212\002\138\233\142\251\014\205\002\210\203\242$/\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ntitle-type\210\171\158\194\006\ntitle-type\210\203\242$%\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004name\210\171\158\194\006\006string\210\203\242$&\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005title\210\171\158\194\006\006string\210\203\242$%\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004fief\210\171\158\194\006\006string\210\203\242$+\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndate-begin\210\171\158\194\006\006string\210\203\242$)\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bdate-end\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003nth\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\005title\218\244\134\182\012\191\012\138\233\142\251\014\184\012\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$&\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$#\232\146\150q\018\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\210\203\242$,\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$(\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007aliases\210\171\158\194\006\006string\210\203\242$+\232\146\150q\024\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nqualifiers\210\171\158\194\006\006string\210\203\242$2\232\146\150q\026\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\017firstname-aliases\210\171\158\194\006\006string\210\203\242$0\232\146\150q\028\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015surname-aliases\210\171\158\194\006\006string\210\203\242$+\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nbirth-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tbirth-src\210\171\158\194\006\006string\210\203\242$-\232\146\150q$\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012baptism-date\210\171\158\194\006\006string\210\203\242$.\232\146\150q&\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rbaptism-place\210\171\158\194\006\006string\210\203\242$,\232\146\150q(\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011baptism-src\210\171\158\194\006\006string\210\203\242$+\232\146\150q*\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeath-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tdeath-src\210\171\158\194\006\006string\210\203\242$/\232\146\150q0\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-type\210\171\158\194\006\ndeath-type\210\203\242$,\232\146\150q2\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011burial-date\210\171\158\194\006\006string\210\203\242$-\232\146\150q4\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012burial-place\210\171\158\194\006\006string\210\203\242$+\232\146\150q6\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nburial-src\210\171\158\194\006\006string\210\203\242$+\232\146\150q8\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\noccupation\210\171\158\194\006\006string\210\203\242$)\232\146\150q:\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bpsources\210\171\158\194\006\006string\210\203\242$&\232\146\150q<\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006titles\210\171\158\194\006\005title\210\203\242$3\232\146\150q>\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\020visible-for-visitors\210\171\158\194\006\004bool\210\203\242$-\232\146\150q@\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\014has-more-infos\210\171\158\194\006\004bool\210\203\242$+\232\146\150qB\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\218\164\238\191\004\016person-tree-full\218\244\134\182\012\187\003\138\233\142\251\014\180\003\210\203\242$)\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bfsources\210\171\158\194\006\006string\210\203\242$.\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rmarriage-date\210\171\158\194\006\006string\210\203\242$/\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014marriage-place\210\171\158\194\006\006string\210\203\242$-\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012marriage-src\210\171\158\194\006\006string\210\203\242$5\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rmarriage-type\210\171\158\194\006\rmarriage-type\210\203\242$3\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012divorce-type\210\171\158\194\006\012divorce-type\210\203\242$-\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012divorce-date\210\171\158\194\006\006string\210\203\242$.\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\016family-tree-full\218\244\134\182\012\174\001\138\233\142\251\014\167\001\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002id\210\171\158\194\006\014protobuf-int64\210\203\242$1\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\016person-tree-full\210\203\242$-\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004ifam\210\171\158\194\006\014protobuf-int64\218\164\238\191\004\tnode-full\218\244\134\182\012\135\004\138\233\142\251\014\128\004\210\203\242$-\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\tnodes-asc\210\171\158\194\006\tnode-full\210\203\242$(\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\tedges-asc\210\171\158\194\006\004edge\210\203\242$7\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012families-asc\210\171\158\194\006\016family-tree-full\210\203\242$.\232\146\150q\b\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nnodes-desc\210\171\158\194\006\tnode-full\210\203\242$)\232\146\150q\n\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nedges-desc\210\171\158\194\006\004edge\210\203\242$8\232\146\150q\012\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\rfamilies-desc\210\171\158\194\006\016family-tree-full\210\203\242$2\232\146\150q\014\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\014nodes-siblings\210\171\158\194\006\tnode-full\210\203\242$9\232\146\150q\016\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\021nodes-siblings-before\210\171\158\194\006\tnode-full\210\203\242$8\232\146\150q\018\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\020nodes-siblings-after\210\171\158\194\006\tnode-full\218\164\238\191\004\015graph-tree-full\218\244\134\182\012\254\001\138\233\142\251\014\247\001\210\203\242$.\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$\"\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$+\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\002oc\210\171\158\194\006\014protobuf-int32\210\203\242$*\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011track-visit\210\171\158\194\006\004bool\218\164\238\191\004\017identifier-person\218\244\134\182\012n\138\233\142\251\014h\210\203\242$*\232\146\150q\204\015\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004code\210\171\158\194\006\nerror-code\210\203\242$)\232\146\150q\206\015\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007message\210\171\158\194\006\006string\218\164\238\191\004\005error\218\244\134\182\012\225\001\138\176\205\197\001\218\001\218\164\238\191\004\nerror-code\170\183\218\222\005.\232\146\150q\160\006\234\188\204\215\002\017error_bad_request\218\164\238\191\004\011bad-request\170\183\218\222\0050\232\146\150q\162\006\234\188\204\215\002\018error_unauthorized\218\164\238\191\004\012unauthorized\170\183\218\222\005*\232\146\150q\166\006\234\188\204\215\002\015error_forbidden\218\164\238\191\004\tforbidden\170\183\218\222\005*\232\146\150q\168\006\234\188\204\215\002\015error_not_found\218\164\238\191\004\tnot-found\218\244\134\182\012H\138\233\142\251\014B\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002nb\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\012nb-ancestors\218\244\134\182\012i\138\233\142\251\014c\210\203\242$$\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003key\210\171\158\194\006\006string\210\203\242$&\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005value\210\171\158\194\006\006string\218\164\238\191\004\tconf-line\218\244\134\182\012\157\001\138\233\142\251\014\150\001\210\203\242$(\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\004line\210\171\158\194\006\tconf-line\210\203\242$(\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007wizards\210\171\158\194\006\006string\210\203\242$(\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007friends\210\171\158\194\006\006string\218\164\238\191\004\tconf-base\218\244\134\182\012V\138\176\205\197\001P\218\164\238\191\004\004sosa\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007no-sosa\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\bsosa-ref\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004sosa\218\244\134\182\012s\138\176\205\197\001m\218\164\238\191\004\bcalendar\170\183\218\222\005\020\232\146\150q\000\218\164\238\191\004\tgregorian\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006julian\170\183\218\222\005\017\232\146\150q\004\218\164\238\191\004\006french\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006hebrew\218\244\134\182\012\179\001\138\176\205\197\001\172\001\218\164\238\191\004\tprecision\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004sure\170\183\218\222\005\016\232\146\150q\002\218\164\238\191\004\005about\170\183\218\222\005\016\232\146\150q\004\218\164\238\191\004\005maybe\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006before\170\183\218\222\005\016\232\146\150q\b\218\164\238\191\004\005after\170\183\218\222\005\017\232\146\150q\n\218\164\238\191\004\006oryear\170\183\218\222\005\018\232\146\150q\012\218\164\238\191\004\007yearint\218\244\134\182\012S\138\176\205\197\001M\218\164\238\191\004\003sex\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007unknown\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004male\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006female\218\244\134\182\012\197\001\138\176\205\197\001\190\001\218\164\238\191\004\ndeath-type\170\183\218\222\005\028\232\146\150q\b\218\164\238\191\004\017dont-know-if-dead\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\bnot-dead\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004dead\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ndead-young\170\183\218\222\005\030\232\146\150q\006\218\164\238\191\004\019dead-dont-know-when\170\183\218\222\005\025\232\146\150q\n\218\164\238\191\004\014of-course-dead\218\244\134\182\012a\138\176\205\197\001[\218\164\238\191\004\011burial-type\170\183\218\222\005\020\232\146\150q\000\218\164\238\191\004\tdont-know\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006buried\170\183\218\222\005\019\232\146\150q\004\218\164\238\191\004\bcremated\218\244\134\182\012\211\001\138\176\205\197\001\204\001\218\164\238\191\004\rmarriage-type\170\183\218\222\005\021\232\146\150q\b\218\164\238\191\004\nno-mention\170\183\218\222\005\018\232\146\150q\000\218\164\238\191\004\007married\170\183\218\222\005\022\232\146\150q\002\218\164\238\191\004\011not-married\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007engaged\170\183\218\222\005%\232\146\150q\006\218\164\238\191\004\026no-sexes-check-not-married\170\183\218\222\005!\232\146\150q\n\218\164\238\191\004\022no-sexes-check-married\218\244\134\182\012h\138\176\205\197\001b\218\164\238\191\004\012divorce-type\170\183\218\222\005\023\232\146\150q\000\218\164\238\191\004\012not-divorced\170\183\218\222\005\019\232\146\150q\002\218\164\238\191\004\bdivorced\170\183\218\222\005\020\232\146\150q\004\218\164\238\191\004\tseparated\218\244\134\182\012\131\003\138\176\205\197\001\252\002\218\164\238\191\004\rrelation-type\170\183\218\222\005\027\232\146\150q\000\218\164\238\191\004\016rparent-adoption\170\183\218\222\005\030\232\146\150q\002\218\164\238\191\004\019rparent-recognition\170\183\218\222\005#\232\146\150q\004\218\164\238\191\004\024rparent-candidate-parent\170\183\218\222\005\029\232\146\150q\006\218\164\238\191\004\018rparent-god-parent\170\183\218\222\005 \232\146\150q\b\218\164\238\191\004\021rparent-foster-parent\170\183\218\222\005\026\232\146\150q\n\218\164\238\191\004\015rchild-adoption\170\183\218\222\005\029\232\146\150q\012\218\164\238\191\004\018rchild-recognition\170\183\218\222\005\"\232\146\150q\014\218\164\238\191\004\023rchild-candidate-parent\170\183\218\222\005\028\232\146\150q\016\218\164\238\191\004\017rchild-god-parent\170\183\218\222\005\031\232\146\150q\018\218\164\238\191\004\020rchild-foster-parent\218\244\134\182\012R\138\176\205\197\001L\218\164\238\191\004\012witness-type\170\183\218\222\005\018\232\146\150q\000\218\164\238\191\004\007witness\170\183\218\222\005\028\232\146\150q\002\218\164\238\191\004\017witness-godparent\218\244\134\182\012\139\017\138\176\205\197\001\132\017\218\164\238\191\004\nevent-type\170\183\218\222\005\022\232\146\150q\000\218\164\238\191\004\011epers-birth\170\183\218\222\005\024\232\146\150q\002\218\164\238\191\004\repers-baptism\170\183\218\222\005\022\232\146\150q\004\218\164\238\191\004\011epers-death\170\183\218\222\005\023\232\146\150q\006\218\164\238\191\004\012epers-burial\170\183\218\222\005\026\232\146\150q\b\218\164\238\191\004\015epers-cremation\170\183\218\222\005\031\232\146\150q\n\218\164\238\191\004\020epers-accomplishment\170\183\218\222\005\028\232\146\150q\012\218\164\238\191\004\017epers-acquisition\170\183\218\222\005\025\232\146\150q\014\218\164\238\191\004\014epers-adhesion\170\183\218\222\005\027\232\146\150q\016\218\164\238\191\004\016epers-baptismlds\170\183\218\222\005\027\232\146\150q\018\218\164\238\191\004\016epers-barmitzvah\170\183\218\222\005\027\232\146\150q\020\218\164\238\191\004\016epers-batmitzvah\170\183\218\222\005\028\232\146\150q\022\218\164\238\191\004\017epers-benediction\170\183\218\222\005\027\232\146\150q\024\218\164\238\191\004\016epers-changename\170\183\218\222\005\029\232\146\150q\026\218\164\238\191\004\018epers-circumcision\170\183\218\222\005\029\232\146\150q\028\218\164\238\191\004\018epers-confirmation\170\183\218\222\005 \232\146\150q\030\218\164\238\191\004\021epers-confirmationlds\170\183\218\222\005\027\232\146\150q \218\164\238\191\004\016epers-decoration\170\183\218\222\005(\232\146\150q\"\218\164\238\191\004\029epers-demobilisationmilitaire\170\183\218\222\005\024\232\146\150q$\218\164\238\191\004\repers-diploma\170\183\218\222\005\028\232\146\150q&\218\164\238\191\004\017epers-distinction\170\183\218\222\005\025\232\146\150q(\218\164\238\191\004\014epers-dotation\170\183\218\222\005\028\232\146\150q*\218\164\238\191\004\017epers-dotationlds\170\183\218\222\005\026\232\146\150q,\218\164\238\191\004\015epers-education\170\183\218\222\005\025\232\146\150q.\218\164\238\191\004\014epers-election\170\183\218\222\005\027\232\146\150q0\218\164\238\191\004\016epers-emigration\170\183\218\222\005 \232\146\150q2\218\164\238\191\004\021epers-excommunication\170\183\218\222\005\030\232\146\150q4\218\164\238\191\004\019epers-familylinklds\170\183\218\222\005\031\232\146\150q6\218\164\238\191\004\020epers-firstcommunion\170\183\218\222\005\024\232\146\150q8\218\164\238\191\004\repers-funeral\170\183\218\222\005\025\232\146\150q:\218\164\238\191\004\014epers-graduate\170\183\218\222\005 \232\146\150q<\218\164\238\191\004\021epers-hospitalisation\170\183\218\222\005\024\232\146\150q>\218\164\238\191\004\repers-illness\170\183\218\222\005\028\232\146\150q@\218\164\238\191\004\017epers-immigration\170\183\218\222\005\031\232\146\150qB\218\164\238\191\004\020epers-listepassenger\170\183\218\222\005$\232\146\150qD\218\164\238\191\004\025epers-militarydistinction\170\183\218\222\005\"\232\146\150qF\218\164\238\191\004\023epers-militarypromotion\170\183\218\222\005 \232\146\150qH\218\164\238\191\004\021epers-militaryservice\170\183\218\222\005&\232\146\150qJ\218\164\238\191\004\027epers-mobilisationmilitaire\170\183\218\222\005\031\232\146\150qL\218\164\238\191\004\020epers-naturalisation\170\183\218\222\005\027\232\146\150qN\218\164\238\191\004\016epers-occupation\170\183\218\222\005\027\232\146\150qP\218\164\238\191\004\016epers-ordination\170\183\218\222\005\025\232\146\150qR\218\164\238\191\004\014epers-property\170\183\218\222\005\028\232\146\150qT\218\164\238\191\004\017epers-recensement\170\183\218\222\005\026\232\146\150qV\218\164\238\191\004\015epers-residence\170\183\218\222\005\024\232\146\150qX\218\164\238\191\004\repers-retired\170\183\218\222\005!\232\146\150qZ\218\164\238\191\004\022epers-scellentchildlds\170\183\218\222\005\"\232\146\150q\\\218\164\238\191\004\023epers-scellentparentlds\170\183\218\222\005\"\232\146\150q^\218\164\238\191\004\023epers-scellentspouselds\170\183\218\222\005\026\232\146\150q`\218\164\238\191\004\015epers-ventebien\170\183\218\222\005\021\232\146\150qb\218\164\238\191\004\nepers-will\170\183\218\222\005\023\232\146\150qd\218\164\238\191\004\012epers-custom\170\183\218\222\005\025\232\146\150q\200\001\218\164\238\191\004\refam-marriage\170\183\218\222\005\028\232\146\150q\202\001\218\164\238\191\004\016efam-no-marriage\170\183\218\222\005\027\232\146\150q\204\001\218\164\238\191\004\015efam-no-mention\170\183\218\222\005\023\232\146\150q\206\001\218\164\238\191\004\011efam-engage\170\183\218\222\005\024\232\146\150q\208\001\218\164\238\191\004\012efam-divorce\170\183\218\222\005\026\232\146\150q\210\001\218\164\238\191\004\014efam-separated\170\183\218\222\005\027\232\146\150q\212\001\218\164\238\191\004\015efam-annulation\170\183\218\222\005\030\232\146\150q\214\001\218\164\238\191\004\018efam-marriage-bann\170\183\218\222\005\"\232\146\150q\216\001\218\164\238\191\004\022efam-marriage-contract\170\183\218\222\005!\232\146\150q\218\001\218\164\238\191\004\021efam-marriage-license\170\183\218\222\005\021\232\146\150q\220\001\218\164\238\191\004\tefam-pacs\170\183\218\222\005\026\232\146\150q\222\001\218\164\238\191\004\014efam-residence\170\183\218\222\005\023\232\146\150q\224\001\218\164\238\191\004\011efam-custom\218\244\134\182\012g\138\176\205\197\001a\218\164\238\191\004\ntitle-type\170\183\218\222\005\021\232\146\150q\000\218\164\238\191\004\ntitle-main\170\183\218\222\005\021\232\146\150q\002\218\164\238\191\004\ntitle-name\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ntitle-none"
include Api_saisie_read_piqi
