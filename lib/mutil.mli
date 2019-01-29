(* Copyright (c) 2006-2007 INRIA *)

(** Misc utilities independants from any other Geneweb module. *)

val int_size : int
val verbose : bool ref

val list_iter_first : (bool -> 'a -> unit) -> 'a list -> unit
val tr : char -> char -> string -> string
val strip_all_trailing_spaces : string -> string

val decline : char -> string -> string
val nominative : string -> string

val lock_file : string -> string

val output_value_no_sharing : out_channel -> _ -> unit
val output_array_no_sharing : out_channel -> (int -> _) -> int -> unit

val name_key : string -> string
val initial : string -> int
val input_particles : string -> string list
val surnames_pieces : string -> string list

val utf_8_of_iso_8859_1 : string -> string
val iso_8859_1_of_utf_8 : string -> string

val roman_of_arabian : int -> string
val arabian_of_roman : string -> int

val compare_after_particle : string list -> string -> string -> int

val input_lexicon :
  string -> (string, string) Hashtbl.t -> (unit -> in_channel) -> unit

module StrSet : Set.S with type elt = string

(** [array_to_list_map fn a] is almost like [Array.to_list a |> List.map fn]
    but it does not allocate an intermediate list.

    The list is constructed backward,
    so if [fn] have side effects it may not behave as excepted.
 *)
val array_to_list_map : ('a -> 'b) -> 'a array -> 'b list

(** [filter_map fn list] is a combination of map and filter.
    Not tail-recursive.
*)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

(** [rev_iter fn list] is like [List.iter fn (List.rev list)].
    Not tail-recursive.
*)
val rev_iter : ('a -> unit) -> 'a list -> unit

(** [start_with ?wildcard prefix off str]
    Test if [str] starts with [prefix] (at offset [off]).
    If [wildcard] is set to [true], occurences of ['_'] in [prefix]
    will match both ['_'] and [' '] in [str] and trailing ['_'] of [prefix]
    is treated as an optional ['_'] [' '].

    Raise [Invalid_argument] if [off] is not a valid index in [str].
*)
val start_with : ?wildcard:bool -> string -> int -> string -> bool

(** [contains ?wildcard str sub] Test [sub] is contained in [str].
    See {!val:start_with} for details about [wildcard]
    (with [sub] in the role of [prefix]).
*)
val contains : ?wildcard:bool -> string -> string -> bool

(** [get_particle particles name]
    Return [p] where [p] is in [particles] and is prefix of [name].
    If no such [p] exists, empty string [""] is returned. *)
val get_particle : string list -> string -> string

(** [ls_rs dirs]
    List directories (and subdirectories) contents of [dirs], including [dirs] themselves.
*)
val ls_r : string list -> string list

(** [rm_rf dir]
    Remove directory [dir] and everything inside [dir].
*)
val rm_rf : string -> unit

(** [rm fname]
    Remove [fname]. If [fname] does not exists, do nothing.
*)
val rm : string -> unit

(** [mkdir_p d]
    Create [d] directory.
    No error if existing, make parent directories as needed
*)
val mkdir_p : string -> unit
