open Def

type ('a, 'b) pb_person =
  | PLight of 'a
  | PFull of 'b
;;

type filters =
  { only_sosa : bool;
    only_recent : bool;
    filter_sex : Def.sex option;
    nb_results : bool;
    date_birth : (dmy * dmy * bool) option;
    date_death : (dmy * dmy * bool) option;
  }
;;


module StringSetAutoComplete =
  Set.Make
    (struct
      type t = string ;;
      let compare = compare ;;
     end)
;;

module PlaceSetAutoComplete =
  Set.Make
    (struct
      type t = (string,string,string,string,string,string,string) Def.gen_place ;;
      let compare _p1 _p2 = 0 ;;  (* FIXME! *)
     end)
;;

type cache_type =
  | Cache_string of (string list)
  | Cache_place of ((string,string,string,string,string,string,string) Def.gen_place list)
;;
