(* Copyright (c) 2006-2007 INRIA *)

(* checking database ; independent from its implementation on disk *)

open Gwdb

val print_base_error : out_channel -> base -> CheckItem.base_error -> unit
val print_base_warning : out_channel -> base -> CheckItem.base_warning -> unit

val check_base
  : ?verbose:bool
  -> ?mem:bool
  -> base
  -> (CheckItem.base_error -> unit)
  -> (CheckItem.base_warning -> unit)
  -> (iper * person * Def.sex option * relation list option -> unit)
  -> unit
