(* $Id: futil.mli,v 5.5 2007-03-05 05:18:23 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Def

val map_title_strings : ('a -> 'b) -> 'a gen_title -> 'b gen_title
val map_pers_event :
  ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) gen_pers_event ->
    ('c, 'd) gen_pers_event
val map_fam_event :
  ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) gen_fam_event -> ('c, 'd) gen_fam_event
val map_relation_ps :
  ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) gen_relation -> ('c, 'd) gen_relation

val map_person_ps :
  ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) gen_person -> ('c, 'd) gen_person
val map_union_f : ('a -> 'b) -> 'a gen_union -> 'b gen_union

val map_family_ps :
  ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) gen_family -> ('c, 'd) gen_family
val map_couple_p : bool -> ('a -> 'b) -> 'a gen_couple -> 'b gen_couple
val map_descend_p : ('a -> 'b) -> 'a gen_descend -> 'b gen_descend

val eq_lists : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val eq_titles : ('a -> 'b -> bool) -> 'a gen_title -> 'b gen_title -> bool
val eq_title_names :
  ('a -> 'b -> bool) -> 'a gen_title_name -> 'b gen_title_name -> bool

val parent : bool -> 'a array -> 'a gen_couple

val gen_person_misc_names :
  string -> string -> string -> string list -> string list -> string list ->
    string list -> string Def.gen_title list -> (string * string list) list ->
    string list -> string list
