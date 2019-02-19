(* $Id: util.mli,v 5.36 2007-07-26 01:57:42 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

val add_lang_path : string -> unit
val set_base_dir : string -> unit
val cnt_dir : string ref
val image_prefix : config -> string
val base_path : string list -> string -> string

val find_misc_file : string -> string

val search_in_lang_path : string -> string

val etc_file_name : config -> string -> string

val escache_value : base -> string
val commit_patches : config -> base -> unit
val update_wf_trace : config -> string -> unit

val get_referer : config -> string

val no_html_tags : string -> string
val clean_html_tags : string -> string list -> string
val sanitize_html : string -> string

val nl : unit -> unit
val html : ?content_type:string -> config -> unit
val html_br : config -> unit
val html_p : config -> unit
val html_li : config -> unit
val unauthorized : config -> string -> unit
val string_of_ctime : config -> string

val commd : config -> string
val commd_2 : config -> string
val prefix_base : config -> string
val prefix_base_password : config -> string
val prefix_base_2 : config -> string
val prefix_base_password_2 : config -> string
val code_varenv : string -> string
val decode_varenv : string -> string
val hidden_env : config -> unit

val nobtit : config -> base -> person -> title list

val strictly_after_private_years : config -> dmy -> bool
val authorized_age : config -> base -> person -> bool
val is_old_person : config -> (iper, istr) gen_person -> bool
val fast_auth_age : config -> person -> bool

val start_with_vowel : string -> bool
val know : base -> person -> bool
val acces_n : config -> base -> string -> person -> string
val acces : config -> base -> person -> string
val wprint_hidden_person : config -> base -> string -> person -> unit
val accessible_by_key : config -> base -> person -> string -> string -> bool

val geneweb_link : config -> string -> string -> string
val wprint_geneweb_link : config -> string -> string -> unit

val is_restricted : config -> base -> iper -> bool
val is_hidden : person -> bool

val pget : config -> base -> iper -> person
val string_gen_person :
  base -> (iper, istr) gen_person -> (iper, string) gen_person
val string_gen_family :
  base -> (iper, istr) gen_family -> (iper, string) gen_family

type p_access = (base -> person -> string) * (base -> person -> string)
val std_access : p_access
val raw_access : p_access

(* Fonctions d'écriture du nom et prénom d'un individu en fonction de : *)
(*   - son/ses titre de noblesse                                        *)
(*   - son/ses nom public                                               *)
(*   - son/ses sobriquets ...                                           *)
val gen_person_text : p_access -> config -> base -> person -> string
val gen_person_text_no_html : p_access -> config -> base -> person -> string
val gen_person_text_without_title :
  p_access -> config -> base -> person -> string
val gen_person_title_text :
  (config -> base -> person -> string -> string) -> p_access -> config ->
    base -> person -> string
val person_text : config -> base -> person -> string
val person_text_no_html : config -> base -> person -> string
val person_text_without_surname : config -> base -> person -> string
val person_text_no_surn_no_acc_chk : config -> base -> person -> string
val person_text_without_title : config -> base -> person -> string
val main_title : config -> base -> person -> title option
val titled_person_text : config -> base -> person -> title -> string
val one_title_text : base -> title -> string
val person_title_text : config -> base -> person -> string
val person_title : config -> base -> person -> string

val child_of_parent : config -> base -> person -> string

val reference : config -> base -> person -> string -> string
val no_reference : config -> base -> person -> string -> string
val referenced_person_title_text : config -> base -> person -> string
val referenced_person_text : config -> base -> person -> string
val referenced_person_text_without_surname :
  config -> base -> person -> string

val update_family_loop : config -> base -> person -> string -> string

val p_getenv : (string * string) list -> string -> string option
val p_getint : (string * string) list -> string -> int option
val create_env : string -> (string * string) list
val capitale : string -> string
val index_of_next_char : string -> int -> int

val open_etc_file : string -> in_channel option
val open_hed_trl : config -> string -> in_channel option
val open_templ : config -> string -> in_channel option
val string_with_macros :
  config -> (char * (unit -> string)) list -> string -> string
val string_of_place : config -> string -> string
val place_of_string : config -> string -> place option
val filter_html_tags : string -> string
val allowed_tags_file : string ref
val body_prop : config -> string
val url_no_index : config -> base -> string
val message_to_wizard : config -> unit
val check_xhtml : string -> string

val print_alphab_list :
  ('a -> string) -> ('a -> unit) -> 'a list -> unit
val of_course_died : config -> person -> bool
val hexa_string : string -> string

(** [surname_particle base sn]
    Extract the particle of [sn] if there is one.
    The list of particles to use is defined in [base]. *)
val surname_particle : base -> string -> string

(** [surname_without_particle base sn]
    Remove the particle of [sn] if there is one.
    The list of particles to use is defined in [base]. *)
val surname_without_particle : base -> string -> string

val specify_homonymous : config -> base -> person -> bool -> unit

val get_approx_birth_date_place :
  config -> base -> person -> date option * string
val get_approx_death_date_place :
  config -> base -> person -> date option * string

type ('a, 'b) format2 = ('a, unit, string, 'b) format4

val check_format : ('a, 'b) format2 -> string -> ('a, 'b) format2 option
val valid_format : ('a, 'b) format2 -> string -> ('a, 'b) format2

val transl : config -> string -> string
val transl_nth : config -> string -> int -> string
val transl_decline : config -> string -> string -> string
val ftransl : config -> ('a, 'b) format2 -> ('a, 'b) format2
val ftransl_nth : config -> ('a, 'b) format2 -> int -> ('a, 'b) format2
val fdecline : ('a, 'b) format2 -> string -> ('a, 'b) format2
val fcapitale : ('a, 'b) format2 -> ('a, 'b) format2
val nth_field : string -> int -> string
val cftransl : config -> string -> string list -> string
val translate_eval : string -> string

(** [transl_a_of_b conf a b b_raw]
    Translate "a of b" using [b_raw] for declension.
    i.e. if [b] is wrapped in html, [b_raw] should be that texte with no html,
    and [b_raw] should be [b] otherwise.
*)
val transl_a_of_b : config -> string -> string -> string -> string
val transl_a_of_gr_eq_gen_lev : config -> string -> string -> string -> string

val std_color : config -> string -> string

val index_of_sex : sex -> int

val string_of_pevent_name :
  config -> base -> istr gen_pers_event_name -> string
val string_of_fevent_name :
  config -> base -> istr gen_fam_event_name -> string

(** [string_of_witness_kind conf sex wk]
    Return the string corresponding to wk according to [sex] and [conf].
*)
val string_of_witness_kind : config -> sex -> witness_kind -> string

val relation_txt :
  config -> sex -> family -> (('a -> 'b) -> 'b, 'a, 'b) format

val string_of_decimal_num : config -> float -> string

val person_exists : config -> base -> string * string * int -> bool
val husband_wife : config -> base -> person -> bool -> string

val find_person_in_env : config -> base -> string -> person option
(* Recherche le sosa uniquement dans le fichier gwf *)
val default_sosa_ref : config -> base -> person option
val find_sosa_ref : config -> base -> person option
val update_gwf_sosa : config -> base -> iper * (string * string * int) -> unit

val quote_escaped : string -> string

val get_server_string : string list -> string
val get_request_string : string list -> string

val create_topological_sort : config -> base -> int array

val branch_of_sosa :
  config -> base -> iper -> Sosa.t -> (iper * sex) list option
val sosa_of_branch : (iper * sex) list -> Sosa.t

val has_image : config -> base -> person -> bool
val image_file_name : string -> string
val source_image_file_name : string -> string -> string

val image_size : string -> (int * int) option
val limited_image_size :
  int -> int -> string -> (int * int) option -> (int * int) option
val image_and_size :
  config -> base -> person ->
    (string -> (int * int) option -> (int * int) option) ->
    (bool * string * (int * int) option) option

val default_image_name_of_key : string -> string -> int -> string
val default_image_name : base -> person -> string
val auto_image_file : config -> base -> person -> string option

val only_printable : string -> string
val only_printable_or_nl : string -> string

val relation_type_text : config -> relation_type -> int -> string
val rchild_type_text : config -> relation_type -> int -> string

val has_nephews_or_nieces : config -> base -> person -> bool

val browser_doesnt_have_tables : config -> bool

val doctype : config -> string

val begin_centered : config -> unit
val end_centered : config -> unit

(* Printing for browsers without tables *)

val pre_text_size : string -> int
val print_pre_center : int -> string -> unit
val print_pre_left : int -> string -> unit
val print_pre_right : int -> string -> unit

val short_f_month : int -> string

(* Reading password file *)

type auth_user = { au_user : string; au_passwd : string; au_info : string }

val read_gen_auth_file : string -> auth_user list

val is_that_user_and_password : auth_scheme_kind -> string -> string -> bool

(* Searching *)

val in_text : bool -> string -> string -> bool
val html_highlight : bool -> string -> string -> string

(* Pretty print XHTML wrapper for Wserver.wrap_string *)

val xml_pretty_print : string -> string

(* Print list in columns with Gutil.alphabetic order *)

val wprint_in_columns :
  config -> ('a -> string) -> ('a -> unit) -> 'a list -> unit

(* Variable that use also private flag of person *)
val is_hide_names : config -> person -> bool

val reduce_list : int -> 'a list -> 'a list

val print_reference : config -> string -> int -> string -> unit

val gen_print_tips : config -> string -> unit
val print_tips_relationship : config -> unit

val print_image_sex : config -> person -> int -> unit

val display_options : config -> string

type cache_visited_t = (string, (iper * string) list) Hashtbl.t
val cache_visited : config -> string
val read_visited : config -> cache_visited_t
val record_visited : config -> iper -> unit

type cache_info_t = (string, string) Hashtbl.t

(* valeur dans le cache. *)
val cache_nb_base_persons : string

val cache_info : config -> string
val read_cache_info : config -> cache_info_t
val patch_cache_info : config -> string -> (string -> string) -> unit

val real_nb_of_persons : config -> base -> int

(** [array_mem_witn conf base ip array] checks if [ip] is in [array]
    and returns corresponding [string_of_witness_kind] if so.
*)
val array_mem_witn
 : Config.config
 -> Gwdb.base
 -> Def.iper
 -> (Def.iper * Def.witness_kind) array
 -> bool * string

(** Print a date using "%4d-%02d-%02d %02d:%02d:%02d" format. *)
val fprintf_date : out_channel -> Unix.tm -> unit

(** [name_key base name] is [name],
    with particles put at the end of the string instead of the beginning.
*)
val name_key : Gwdb.base -> string -> string

(** [nb_char_occ c s] return the number of times [c] appears in [s]. *)
val nb_char_occ : char -> string -> int

(** [filter_map fn list] is a combination of map and filter.
    Not tail-recursive.
*)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

(** [rev_iter fn list] is like [List.iter fn (List.rev list)].
    Not tail-recursive.
*)
val rev_iter : ('a -> unit) -> 'a list -> unit

(** [group_by ~key ~value list]
    Group the elements returning the same key together.
    Ordering of elements is unspecified.
 *)
val groupby : key:('a -> 'k) -> value:('a -> 'v) -> 'a list -> ('k * 'v list) list

(** [str_replace ?unsafe c ~by str]
    Return a new string which is the same as [str] with all occurences of [c]
    replaced by [by].
    If [str] does not contain [c]. [str] is returned intouched.

    If [unsafe] is set to true, [str] is modified instead of a copy of [str].
 *)
val str_replace : ?unsafe:bool -> char -> by:char -> string -> string

(** [str_nth_pos str n]
    Return a position of the byte starting the [n]-th UTF8 character.
 *)
val str_nth_pos : string -> int -> int

(** [str_sub ?pad s start len]
    Return a fresh UTF8-friendly substring of [len] characters, padded if needed.
    Be careful [start] is the index of the byte where to start in [s],
    not the [start-th] UTF8-character.
*)
val str_sub : ?pad:char -> string -> int -> int -> string

(** [ls_rs dirs]
    List directories (and subdirectories) contents of [dirs], including [dirs] themselves.
*)
val ls_r : string list -> string list

(** [rm_rf dir]
    Remove directory [dir] and everything inside [dir].
*)
val rm_rf : string -> unit
