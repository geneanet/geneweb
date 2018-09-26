(* $Id: perso.mli,v 5.7 2007-03-30 18:57:19 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Gwdb
open Config

type generation_person =
    GP_person of Sosa.t * Adef.iper * Adef.ifam option
  | GP_same of Sosa.t * Sosa.t * Adef.iper
  | GP_interv of (Sosa.t * Sosa.t * (Sosa.t * Sosa.t) option) option
  | GP_missing of Sosa.t * Adef.iper

type sosa_t =
  { tstab : int array;
    mark : bool array;
    mutable last_zil : (Def.iper * Sosa.t) list;
    sosa_ht : (Def.iper, (Sosa.t * Gwdb.person) option) Hashtbl.t }

val string_of_marriage_text : config -> base -> family -> string
val interp_templ : string -> config -> base -> person -> unit
val interp_templ_with_menu :
  (bool -> unit) -> string -> config -> base -> person -> unit
val interp_notempl_with_menu :
  (bool -> unit) -> string -> config -> base -> person -> unit

val print : config -> base -> person -> unit
val print_ascend : config -> base -> person -> unit
val print_what_links : config -> base -> person -> unit

val links_to_ind :
  config -> base -> (NotesLinks.page * ('a * ('b * 'c) list)) list -> 'b ->
    NotesLinks.page list

val build_sosa_tree_ht : config -> base -> person -> unit
val build_sosa_ht : config -> base -> unit
val get_sosa_person : person -> Sosa.t
val get_single_sosa : config -> base -> person -> Sosa.t
val print_sosa : config -> base -> person -> bool -> unit

val string_of_num : string -> Sosa.t -> string
val get_linked_page : config -> base -> person -> string -> string
val get_birth_text : config -> person -> bool -> string
val get_baptism_text : config -> person -> bool -> string
val get_death_text : config -> person -> bool -> string
val get_burial_text : config -> person -> bool -> string
val get_cremation_text : config -> person -> bool -> string
val get_marriage_date_text : config -> family -> bool -> string

val linked_page_text
  : Config.config
  -> Gwdb.base
  -> Gwdb.person
  -> string
  -> 'a
  -> string
  -> NotesLinks.page * ('b * ('a * NotesLinks.ind_link) list)
  -> string

module IperSet : sig include Set.S with type elt = Adef.iper end

val max_ancestor_level : config -> base -> Adef.iper -> int -> int

val string_of_died : config -> person -> bool -> string

val string_of_parent_age : config -> base -> person * bool -> (family -> Adef.iper) -> string

val string_of_image_url : config -> base -> person * bool -> bool -> string

val round_2_dec : float -> float

val has_children : base -> person -> bool

val string_of_image_size : config -> base -> person * bool -> string
val string_of_image_medium_size : config -> base -> person * bool -> string
val string_of_image_small_size : config -> base -> person * bool -> string

val get_link : generation_person list -> IperSet.elt -> generation_person option

val find_sosa
  : config -> base -> person -> person option Lazy.t -> sosa_t -> (Sosa.t * Gwdb.person) option

(**)

val infinite : int
val limit_desc : config -> int
val make_desc_level_table :
  config -> base -> int -> person -> int array * int array
val default_max_cousin_lev : int

type dup =
    DupFam of Adef.ifam * Adef.ifam
  | DupInd of Adef.iper * Adef.iper
  | NoDup

type excl_dup = (Adef.iper * Adef.iper) list * (Adef.ifam * Adef.ifam) list

val excluded_possible_duplications : config -> excl_dup
val first_possible_duplication : base -> Adef.iper -> excl_dup -> dup


(* Ajout pour l'API *)
val nobility_titles_list :
  config -> base -> person ->
    (int * istr Def.gen_title_name * istr * istr list *
       (Adef.date option * Adef.date option) list)
      list

val has_history : config -> base -> person -> bool -> bool
val has_possible_duplications : config -> base -> person -> bool

val string_of_title :
  config -> base -> string -> person ->
    int * istr Def.gen_title_name * istr * istr list *
      (Def.date option * Def.date option) list ->
    string

type event_name =
    Pevent of istr Def.gen_pers_event_name
  | Fevent of istr Def.gen_fam_event_name

val events_list :
  config -> base -> person ->
    (event_name * Def.cdate * istr * istr * istr *
       (Def.iper * Def.witness_kind) array * Def.iper option)
      list
