(* $Id: util.mli,v 5.36 2007-07-26 01:57:42 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;

value add_lang_path : string -> unit;
value add_doc_path : string -> unit;
value set_base_dir : string -> unit;
value cnt_dir : ref string;
value images_url : ref string;
value image_prefix : config -> string;
value base_path : list string -> string -> string;

value find_misc_file : string -> string;

value search_in_lang_path : string -> string;
value search_in_doc_path : string -> string;

value etc_file_name : config -> string -> string;

value escache_value : base -> string;
value commit_patches : config -> base -> unit;
value update_wf_trace : config -> string -> unit;

value get_referer : config -> string;

value no_html_tags : string -> string;
value clean_html_tags : string -> list string -> string;
value sanitize_html : string -> string;

value nl : unit -> unit;
value html : config -> unit;
value html_br : config -> unit;
value html_p : config -> unit;
value html_li : config -> unit;
value unauthorized : config -> string -> unit;
value string_of_ctime : config -> string;

value redirect_HTML : config -> string -> string -> unit;

value commd : config -> string;
value code_varenv : string -> string;
value decode_varenv : string -> string;
value hidden_env : config -> unit;

value nobtit : config -> base -> person -> list title;

value strictly_after_private_years : config -> dmy -> bool;
value authorized_age : config -> base -> person -> bool;
value is_old_person : config -> gen_person iper istr -> bool;
value fast_auth_age : config -> person -> bool;

value start_with_vowel : string -> bool;
value know : base -> person -> bool;
value acces_n : config -> base -> string -> person -> string;
value acces : config -> base -> person -> string;
value wprint_hidden_person : config -> base -> string -> person -> unit;
value accessible_by_key : config -> base -> person -> string -> string -> bool;

value geneweb_link : config -> string -> string -> string;
value wprint_geneweb_link : config -> string -> string -> unit;

value is_restricted : config -> base -> iper -> bool;
value is_hidden : person -> bool;

value pget : config -> base -> iper -> person;
value string_gen_person :
  base -> gen_person iper istr -> gen_person iper string
;
value string_gen_family :
  base -> gen_family iper istr -> gen_family iper string
;

type p_access = (base -> person -> string * base -> person -> string);
value std_access : p_access;
value raw_access : p_access;

(* Fonctions d'écriture du nom et prénom d'un individu en fonction de : *)
(*   - son/ses titre de noblesse                                        *)
(*   - son/ses nom public                                               *)
(*   - son/ses sobriquets ...                                           *)
value gen_person_text : p_access -> config -> base -> person -> string;
value gen_person_text_no_html : p_access -> config -> base -> person -> string;
value gen_person_text_without_title :
  p_access -> config -> base -> person -> string
;
value gen_person_title_text :
  (config -> base -> person -> string -> string) ->
    p_access -> config -> base -> person -> string
;
value person_text : config -> base -> person -> string;
value person_text_no_html : config -> base -> person -> string;
value person_text_without_surname : config -> base -> person -> string;
value person_text_no_surn_no_acc_chk : config -> base -> person -> string;
value person_text_without_title : config -> base -> person -> string;
value main_title : config -> base -> person -> option title;
value titled_person_text : config -> base -> person -> title -> string;
value one_title_text : config -> base -> person -> title -> string;
value person_title_text : config -> base -> person -> string;
value person_title : config -> base -> person -> string;

value reference : config -> base -> person -> string -> string;
value no_reference : config -> base -> person -> string -> string;
value referenced_person_title_text : config -> base -> person -> string;
value referenced_person_text : config -> base -> person -> string;
value referenced_person_text_without_surname :
  config -> base -> person -> string;

value update_family_loop : config -> base -> person -> string -> string;

value p_getenv : list (string * string) -> string -> option string;
value p_getint : list (string * string) -> string -> option int;
value create_env : string -> list (string * string);
value capitale : string -> string;
value index_of_next_char : string -> int -> int;

value open_etc_file : string -> option in_channel;
value open_hed_trl : config -> string -> option in_channel;
value open_templ : config -> string -> option in_channel;
value copy_from_etc :
  list (char * unit -> string) -> string -> string -> in_channel -> unit;
value string_with_macros :
  config -> list (char * unit -> string) -> string -> string;
value string_of_place : config -> string -> string;
value place_of_string : config -> string -> option place;
value filter_html_tags : string -> string;
value allowed_tags_file : ref string;
value body_prop : config -> string;
value url_no_index : config -> base -> string;
value message_to_wizard : config -> unit;
value check_xhtml : string -> string;

value print_alphab_list :
  config -> ('a -> string) -> ('a -> unit) -> list 'a -> unit;
value of_course_died : config -> person -> bool;
value hexa_string : string -> string;

value surname_begin : base -> string -> string;
value surname_end : base -> string -> string;
value get_particle : base -> string -> string;
value old_surname_begin : string -> string;
value old_surname_end : string -> string;

value specify_homonymous : config -> base -> person -> bool -> unit;

type format2 'a 'b = format4 'a unit string 'b;

value check_format : format2 'a 'b -> string -> option (format2 'a 'b);
value valid_format : format2 'a 'b -> string -> format2 'a 'b;

value transl : config -> string -> string;
value transl_nth : config -> string -> int -> string;
value transl_decline : config -> string -> string -> string;
value transl_a_of_b : config -> string -> string -> string;
value transl_a_of_gr_eq_gen_lev : config -> string -> string -> string;
value ftransl : config -> format2 'a 'b -> format2 'a 'b;
value ftransl_nth : config -> format2 'a 'b -> int -> format2 'a 'b;
value fdecline : config -> format2 'a 'b -> string -> format2 'a 'b;
value fcapitale : format2 'a 'b -> format2 'a 'b;
value nth_field : string -> int -> string;

value cftransl : config -> string -> list string -> string;
value translate_eval : string -> string;

value std_color : config -> string -> string;

value index_of_sex : sex -> int;

value string_of_pevent_name :
  config -> base -> gen_pers_event_name istr -> string;
value string_of_fevent_name :
  config -> base -> gen_fam_event_name istr -> string;
value string_of_witness_kind : config -> person -> witness_kind -> string;

value relation_txt :
  config -> sex -> family -> format (('a -> 'b) -> 'b) 'a 'b;

value string_of_decimal_num : config -> float -> string;

value person_exists : config -> base -> (string * string * int) -> bool;

value find_person_in_env : config -> base -> string -> option person;
(* Recherche le sosa uniquement dans le fichier gwf *)
value default_sosa_ref : config -> base -> option person;
value find_sosa_ref : config -> base -> option person;
value update_gwf_sosa :
  config -> base -> (iper * (string * string * int)) -> unit;

value quote_escaped : string -> string;

value get_server_string : config -> string;
value get_request_string : config -> string;

value get_server_string_aux : bool -> list string -> string;
value get_request_string_aux : bool -> list string -> string;

value create_topological_sort : config -> base -> array int;

value branch_of_sosa :
  config -> base -> iper -> Num.t -> option (list (iper * sex));
value sosa_of_branch : list (iper * sex) -> Num.t;

value has_image : config -> base -> person -> bool;
value image_file_name : string -> string;
value source_image_file_name : string -> string -> string;

value image_size : string -> option (int * int);
value limited_image_size : int -> int -> string -> option (int * int)
  -> option (int * int);
value image_and_size :
  config -> base -> person ->
  (string -> option (int * int) -> option (int * int)) ->
    option (bool * string * option (int * int));

value default_image_name_of_key : string -> string -> int -> string;
value default_image_name : base -> person -> string;
value auto_image_file : config -> base -> person -> option string;

value only_printable : string -> string;
value only_printable_or_nl : string -> string;

value relation_type_text : config -> relation_type -> int -> string;
value rchild_type_text : config -> relation_type -> int -> string;

value has_nephews_or_nieces : config -> base -> person -> bool;

value browser_doesnt_have_tables : config -> bool;

value start_with : string -> int -> string -> bool;

value doctype : config -> string;

value begin_centered : config -> unit;
value end_centered : config -> unit;

(* Printing for browsers without tables *)

value pre_text_size : string -> int;
value print_pre_center : int -> string -> unit;
value print_pre_left : int -> string -> unit;
value print_pre_right : int -> string -> unit;

value short_f_month : int -> string;

value compilation_time_hook : ref (config -> string);
value compilation_time : config -> string;

(* Reading password file *)

type auth_user = {au_user : string; au_passwd : string; au_info : string};

value read_gen_auth_file : string -> list auth_user;

value is_that_user_and_password :
  auth_scheme_kind -> string -> string -> bool;

(* Searching *)

value in_text : bool -> string -> string -> bool;
value html_highlight : bool -> string -> string -> string;

(* Pretty print XHTML wrapper for Wserver.wrap_string *)

value xml_pretty_print : string -> string;

(* Print list in columns with alphabetic order *)

value wprint_in_columns :
  config -> ('a -> string) -> ('a -> unit) -> list 'a -> unit;

(* Variable that use also private flag of person *)
value is_hide_names : config -> person -> bool;

value reduce_list : int -> list 'a -> list 'a;

value print_reference : config -> string -> int -> string -> unit;

value gen_print_tips : config -> string -> unit;
value print_tips_relationship : config -> unit;

value print_image_sex : config -> person -> int -> unit;

value display_options : config -> string;

type cache_visited_t = Hashtbl.t string (list (iper * string));
value cache_visited : config -> string;
value read_visited : config -> cache_visited_t;
value record_visited : config -> iper -> unit;

type cache_info_t = Hashtbl.t string string;

(* valeur dans le cache. *)
value cache_nb_base_persons : string;

value cache_info : config -> string;
value read_cache_info : config -> cache_info_t;
value patch_cache_info : config -> string -> (string -> string) -> unit;

value real_nb_of_persons : config -> base -> int;
value read_file : string -> string;