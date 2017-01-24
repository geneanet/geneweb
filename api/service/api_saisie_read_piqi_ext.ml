let piqi = Api_saisie_read_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _dmy_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/dmy"
let _date_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/date"
let _witness_event_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/witness-event"
let _witness_fiche_event_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/witness-fiche-event"
let _event_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/event"
let _fiche_event_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/fiche-event"
let _person_tree_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/person-tree"
let _simple_person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/simple-person"
let _relation_person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/relation-person"
let _relation_fiche_person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/relation-fiche-person"
let _event_witness_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/event-witness"
let _event_fiche_witness_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/event-fiche-witness"
let _person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/person"
let _person_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/person-type"
let _fiche_person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/fiche-person"
let _family_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/family"
let _fiche_family_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/fiche-family"
let _index_person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/index-person"
let _node_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/node"
let _edge_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/edge"
let _graph_tree_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/graph-tree"
let _graph_tree_new_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/graph-tree-new"
let _graph_tree_params_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/graph-tree-params"
let _title_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/title"
let _person_tree_full_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/person-tree-full"
let _family_tree_full_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/family-tree-full"
let _node_full_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/node-full"
let _graph_tree_full_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/graph-tree-full"
let _identifier_person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/identifier-person"
let _error_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/error"
let _error_code_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/error-code"
let _nb_ancestors_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/nb-ancestors"
let _sosa_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/sosa"
let _calendar_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/calendar"
let _precision_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/precision"
let _sex_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/sex"
let _death_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/death-type"
let _burial_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/burial-type"
let _marriage_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/marriage-type"
let _divorce_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/divorce-type"
let _relation_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/relation-type"
let _witness_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/witness-type"
let _event_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/event-type"
let _title_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_read/title-type"


let parse_dmy ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _dmy_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_dmy buf

let parse_date ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _date_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_date buf

let parse_witness_event ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_event_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_witness_event buf

let parse_witness_fiche_event ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_fiche_event_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_witness_fiche_event buf

let parse_event ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_event buf

let parse_fiche_event ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _fiche_event_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_fiche_event buf

let parse_person_tree ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_tree_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_person_tree buf

let parse_simple_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _simple_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_simple_person buf

let parse_relation_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_relation_person buf

let parse_relation_fiche_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_fiche_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_relation_fiche_person buf

let parse_event_witness ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_witness_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_event_witness buf

let parse_event_fiche_witness ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_fiche_witness_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_event_fiche_witness buf

let parse_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_person buf

let parse_person_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_person_type buf

let parse_fiche_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _fiche_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_fiche_person buf

let parse_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_family buf

let parse_fiche_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _fiche_family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_fiche_family buf

let parse_index_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_index_person buf

let parse_node ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _node_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_node buf

let parse_edge ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edge_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_edge buf

let parse_graph_tree ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_tree_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_graph_tree buf

let parse_graph_tree_new ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_tree_new_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_graph_tree_new buf

let parse_graph_tree_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_tree_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_graph_tree_params buf

let parse_title ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_title buf

let parse_person_tree_full ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_tree_full_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_person_tree_full buf

let parse_family_tree_full ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_tree_full_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_family_tree_full buf

let parse_node_full ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _node_full_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_node_full buf

let parse_graph_tree_full ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_tree_full_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_graph_tree_full buf

let parse_identifier_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _identifier_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_identifier_person buf

let parse_error ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _error_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_error buf

let parse_error_code ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _error_code_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_error_code buf

let parse_nb_ancestors ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _nb_ancestors_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_nb_ancestors buf

let parse_sosa ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sosa_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_sosa buf

let parse_calendar ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _calendar_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_calendar buf

let parse_precision ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _precision_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_precision buf

let parse_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_sex buf

let parse_death_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_death_type buf

let parse_burial_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _burial_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_burial_type buf

let parse_marriage_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _marriage_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_marriage_type buf

let parse_divorce_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _divorce_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_divorce_type buf

let parse_relation_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_relation_type buf

let parse_witness_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_witness_type buf

let parse_event_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_event_type buf

let parse_title_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_read_piqi.parse_title_type buf


let gen_dmy ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_dmy x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _dmy_piqi_type `pb format x_pb ?opts

let gen_date ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_date x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _date_piqi_type `pb format x_pb ?opts

let gen_witness_event ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_witness_event x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _witness_event_piqi_type `pb format x_pb ?opts

let gen_witness_fiche_event ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_witness_fiche_event x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _witness_fiche_event_piqi_type `pb format x_pb ?opts

let gen_event ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_event x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _event_piqi_type `pb format x_pb ?opts

let gen_fiche_event ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_fiche_event x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _fiche_event_piqi_type `pb format x_pb ?opts

let gen_person_tree ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_person_tree x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_tree_piqi_type `pb format x_pb ?opts

let gen_simple_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_simple_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _simple_person_piqi_type `pb format x_pb ?opts

let gen_relation_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_relation_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_person_piqi_type `pb format x_pb ?opts

let gen_relation_fiche_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_relation_fiche_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_fiche_person_piqi_type `pb format x_pb ?opts

let gen_event_witness ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_event_witness x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _event_witness_piqi_type `pb format x_pb ?opts

let gen_event_fiche_witness ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_event_fiche_witness x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _event_fiche_witness_piqi_type `pb format x_pb ?opts

let gen_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_piqi_type `pb format x_pb ?opts

let gen_person_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_person_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_type_piqi_type `pb format x_pb ?opts

let gen_fiche_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_fiche_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _fiche_person_piqi_type `pb format x_pb ?opts

let gen_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _family_piqi_type `pb format x_pb ?opts

let gen_fiche_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_fiche_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _fiche_family_piqi_type `pb format x_pb ?opts

let gen_index_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_index_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _index_person_piqi_type `pb format x_pb ?opts

let gen_node ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_node x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _node_piqi_type `pb format x_pb ?opts

let gen_edge ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_edge x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _edge_piqi_type `pb format x_pb ?opts

let gen_graph_tree ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_graph_tree x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_tree_piqi_type `pb format x_pb ?opts

let gen_graph_tree_new ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_graph_tree_new x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_tree_new_piqi_type `pb format x_pb ?opts

let gen_graph_tree_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_graph_tree_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_tree_params_piqi_type `pb format x_pb ?opts

let gen_title ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_piqi_type `pb format x_pb ?opts

let gen_person_tree_full ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_person_tree_full x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_tree_full_piqi_type `pb format x_pb ?opts

let gen_family_tree_full ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_family_tree_full x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _family_tree_full_piqi_type `pb format x_pb ?opts

let gen_node_full ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_node_full x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _node_full_piqi_type `pb format x_pb ?opts

let gen_graph_tree_full ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_graph_tree_full x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_tree_full_piqi_type `pb format x_pb ?opts

let gen_identifier_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_identifier_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _identifier_person_piqi_type `pb format x_pb ?opts

let gen_error ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_error x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _error_piqi_type `pb format x_pb ?opts

let gen_error_code ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_error_code x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _error_code_piqi_type `pb format x_pb ?opts

let gen_nb_ancestors ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_nb_ancestors x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _nb_ancestors_piqi_type `pb format x_pb ?opts

let gen_sosa ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_sosa x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _sosa_piqi_type `pb format x_pb ?opts

let gen_calendar ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_calendar x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _calendar_piqi_type `pb format x_pb ?opts

let gen_precision ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_precision x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _precision_piqi_type `pb format x_pb ?opts

let gen_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _sex_piqi_type `pb format x_pb ?opts

let gen_death_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _death_type_piqi_type `pb format x_pb ?opts

let gen_burial_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_burial_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _burial_type_piqi_type `pb format x_pb ?opts

let gen_marriage_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_marriage_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _marriage_type_piqi_type `pb format x_pb ?opts

let gen_divorce_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_divorce_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _divorce_type_piqi_type `pb format x_pb ?opts

let gen_relation_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_relation_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_type_piqi_type `pb format x_pb ?opts

let gen_witness_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_witness_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _witness_type_piqi_type `pb format x_pb ?opts

let gen_event_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_event_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _event_type_piqi_type `pb format x_pb ?opts

let gen_title_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_title_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_type_piqi_type `pb format x_pb ?opts


let print_dmy ?opts x =
  Pervasives.print_endline (gen_dmy x `piq ?opts)
let prerr_dmy ?opts x =
  Pervasives.prerr_endline (gen_dmy x `piq ?opts)

let print_date ?opts x =
  Pervasives.print_endline (gen_date x `piq ?opts)
let prerr_date ?opts x =
  Pervasives.prerr_endline (gen_date x `piq ?opts)

let print_witness_event ?opts x =
  Pervasives.print_endline (gen_witness_event x `piq ?opts)
let prerr_witness_event ?opts x =
  Pervasives.prerr_endline (gen_witness_event x `piq ?opts)

let print_witness_fiche_event ?opts x =
  Pervasives.print_endline (gen_witness_fiche_event x `piq ?opts)
let prerr_witness_fiche_event ?opts x =
  Pervasives.prerr_endline (gen_witness_fiche_event x `piq ?opts)

let print_event ?opts x =
  Pervasives.print_endline (gen_event x `piq ?opts)
let prerr_event ?opts x =
  Pervasives.prerr_endline (gen_event x `piq ?opts)

let print_fiche_event ?opts x =
  Pervasives.print_endline (gen_fiche_event x `piq ?opts)
let prerr_fiche_event ?opts x =
  Pervasives.prerr_endline (gen_fiche_event x `piq ?opts)

let print_person_tree ?opts x =
  Pervasives.print_endline (gen_person_tree x `piq ?opts)
let prerr_person_tree ?opts x =
  Pervasives.prerr_endline (gen_person_tree x `piq ?opts)

let print_simple_person ?opts x =
  Pervasives.print_endline (gen_simple_person x `piq ?opts)
let prerr_simple_person ?opts x =
  Pervasives.prerr_endline (gen_simple_person x `piq ?opts)

let print_relation_person ?opts x =
  Pervasives.print_endline (gen_relation_person x `piq ?opts)
let prerr_relation_person ?opts x =
  Pervasives.prerr_endline (gen_relation_person x `piq ?opts)

let print_relation_fiche_person ?opts x =
  Pervasives.print_endline (gen_relation_fiche_person x `piq ?opts)
let prerr_relation_fiche_person ?opts x =
  Pervasives.prerr_endline (gen_relation_fiche_person x `piq ?opts)

let print_event_witness ?opts x =
  Pervasives.print_endline (gen_event_witness x `piq ?opts)
let prerr_event_witness ?opts x =
  Pervasives.prerr_endline (gen_event_witness x `piq ?opts)

let print_event_fiche_witness ?opts x =
  Pervasives.print_endline (gen_event_fiche_witness x `piq ?opts)
let prerr_event_fiche_witness ?opts x =
  Pervasives.prerr_endline (gen_event_fiche_witness x `piq ?opts)

let print_person ?opts x =
  Pervasives.print_endline (gen_person x `piq ?opts)
let prerr_person ?opts x =
  Pervasives.prerr_endline (gen_person x `piq ?opts)

let print_person_type ?opts x =
  Pervasives.print_endline (gen_person_type x `piq ?opts)
let prerr_person_type ?opts x =
  Pervasives.prerr_endline (gen_person_type x `piq ?opts)

let print_fiche_person ?opts x =
  Pervasives.print_endline (gen_fiche_person x `piq ?opts)
let prerr_fiche_person ?opts x =
  Pervasives.prerr_endline (gen_fiche_person x `piq ?opts)

let print_family ?opts x =
  Pervasives.print_endline (gen_family x `piq ?opts)
let prerr_family ?opts x =
  Pervasives.prerr_endline (gen_family x `piq ?opts)

let print_fiche_family ?opts x =
  Pervasives.print_endline (gen_fiche_family x `piq ?opts)
let prerr_fiche_family ?opts x =
  Pervasives.prerr_endline (gen_fiche_family x `piq ?opts)

let print_index_person ?opts x =
  Pervasives.print_endline (gen_index_person x `piq ?opts)
let prerr_index_person ?opts x =
  Pervasives.prerr_endline (gen_index_person x `piq ?opts)

let print_node ?opts x =
  Pervasives.print_endline (gen_node x `piq ?opts)
let prerr_node ?opts x =
  Pervasives.prerr_endline (gen_node x `piq ?opts)

let print_edge ?opts x =
  Pervasives.print_endline (gen_edge x `piq ?opts)
let prerr_edge ?opts x =
  Pervasives.prerr_endline (gen_edge x `piq ?opts)

let print_graph_tree ?opts x =
  Pervasives.print_endline (gen_graph_tree x `piq ?opts)
let prerr_graph_tree ?opts x =
  Pervasives.prerr_endline (gen_graph_tree x `piq ?opts)

let print_graph_tree_new ?opts x =
  Pervasives.print_endline (gen_graph_tree_new x `piq ?opts)
let prerr_graph_tree_new ?opts x =
  Pervasives.prerr_endline (gen_graph_tree_new x `piq ?opts)

let print_graph_tree_params ?opts x =
  Pervasives.print_endline (gen_graph_tree_params x `piq ?opts)
let prerr_graph_tree_params ?opts x =
  Pervasives.prerr_endline (gen_graph_tree_params x `piq ?opts)

let print_title ?opts x =
  Pervasives.print_endline (gen_title x `piq ?opts)
let prerr_title ?opts x =
  Pervasives.prerr_endline (gen_title x `piq ?opts)

let print_person_tree_full ?opts x =
  Pervasives.print_endline (gen_person_tree_full x `piq ?opts)
let prerr_person_tree_full ?opts x =
  Pervasives.prerr_endline (gen_person_tree_full x `piq ?opts)

let print_family_tree_full ?opts x =
  Pervasives.print_endline (gen_family_tree_full x `piq ?opts)
let prerr_family_tree_full ?opts x =
  Pervasives.prerr_endline (gen_family_tree_full x `piq ?opts)

let print_node_full ?opts x =
  Pervasives.print_endline (gen_node_full x `piq ?opts)
let prerr_node_full ?opts x =
  Pervasives.prerr_endline (gen_node_full x `piq ?opts)

let print_graph_tree_full ?opts x =
  Pervasives.print_endline (gen_graph_tree_full x `piq ?opts)
let prerr_graph_tree_full ?opts x =
  Pervasives.prerr_endline (gen_graph_tree_full x `piq ?opts)

let print_identifier_person ?opts x =
  Pervasives.print_endline (gen_identifier_person x `piq ?opts)
let prerr_identifier_person ?opts x =
  Pervasives.prerr_endline (gen_identifier_person x `piq ?opts)

let print_error ?opts x =
  Pervasives.print_endline (gen_error x `piq ?opts)
let prerr_error ?opts x =
  Pervasives.prerr_endline (gen_error x `piq ?opts)

let print_error_code ?opts x =
  Pervasives.print_endline (gen_error_code x `piq ?opts)
let prerr_error_code ?opts x =
  Pervasives.prerr_endline (gen_error_code x `piq ?opts)

let print_nb_ancestors ?opts x =
  Pervasives.print_endline (gen_nb_ancestors x `piq ?opts)
let prerr_nb_ancestors ?opts x =
  Pervasives.prerr_endline (gen_nb_ancestors x `piq ?opts)

let print_sosa ?opts x =
  Pervasives.print_endline (gen_sosa x `piq ?opts)
let prerr_sosa ?opts x =
  Pervasives.prerr_endline (gen_sosa x `piq ?opts)

let print_calendar ?opts x =
  Pervasives.print_endline (gen_calendar x `piq ?opts)
let prerr_calendar ?opts x =
  Pervasives.prerr_endline (gen_calendar x `piq ?opts)

let print_precision ?opts x =
  Pervasives.print_endline (gen_precision x `piq ?opts)
let prerr_precision ?opts x =
  Pervasives.prerr_endline (gen_precision x `piq ?opts)

let print_sex ?opts x =
  Pervasives.print_endline (gen_sex x `piq ?opts)
let prerr_sex ?opts x =
  Pervasives.prerr_endline (gen_sex x `piq ?opts)

let print_death_type ?opts x =
  Pervasives.print_endline (gen_death_type x `piq ?opts)
let prerr_death_type ?opts x =
  Pervasives.prerr_endline (gen_death_type x `piq ?opts)

let print_burial_type ?opts x =
  Pervasives.print_endline (gen_burial_type x `piq ?opts)
let prerr_burial_type ?opts x =
  Pervasives.prerr_endline (gen_burial_type x `piq ?opts)

let print_marriage_type ?opts x =
  Pervasives.print_endline (gen_marriage_type x `piq ?opts)
let prerr_marriage_type ?opts x =
  Pervasives.prerr_endline (gen_marriage_type x `piq ?opts)

let print_divorce_type ?opts x =
  Pervasives.print_endline (gen_divorce_type x `piq ?opts)
let prerr_divorce_type ?opts x =
  Pervasives.prerr_endline (gen_divorce_type x `piq ?opts)

let print_relation_type ?opts x =
  Pervasives.print_endline (gen_relation_type x `piq ?opts)
let prerr_relation_type ?opts x =
  Pervasives.prerr_endline (gen_relation_type x `piq ?opts)

let print_witness_type ?opts x =
  Pervasives.print_endline (gen_witness_type x `piq ?opts)
let prerr_witness_type ?opts x =
  Pervasives.prerr_endline (gen_witness_type x `piq ?opts)

let print_event_type ?opts x =
  Pervasives.print_endline (gen_event_type x `piq ?opts)
let prerr_event_type ?opts x =
  Pervasives.prerr_endline (gen_event_type x `piq ?opts)

let print_title_type ?opts x =
  Pervasives.print_endline (gen_title_type x `piq ?opts)
let prerr_title_type ?opts x =
  Pervasives.prerr_endline (gen_title_type x `piq ?opts)


