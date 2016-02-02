(* $Id: date.mli,v 5.4 2007-03-14 00:39:57 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;

value dmy_of_dmy2 : dmy2 -> dmy;
value code_dmy : config -> dmy -> string;
value string_of_ondate : config -> date -> string;
value string_of_ondate_aux : config -> date -> string;
value string_of_date : config -> date -> string;
value string_of_date_sep : config -> string -> date -> string;
value string_slash_of_date : config -> date -> string;
value string_of_age : config -> dmy -> string;
value prec_year_text : config -> dmy -> string;
value prec_text : config -> dmy -> string;
value day_text : dmy -> string;
value month_text : dmy -> string;
value year_text : dmy -> string;
value short_dates_text : config -> base -> person -> string;
value short_marriage_date_text :
  config -> base -> family -> person -> person -> string;
value print_dates : config -> base -> person -> unit;
value print_calendar : config -> base -> unit;
value get_birth_death_date : person -> (option date * option date * bool);

value before_date : dmy -> dmy -> bool;
  (* [before_date d1 d2] = True if d2 before d1; I know, it is not logical *)

(* return the day of the week given the date as parameter *)
value get_wday : config -> date -> string;

value compare_date : date -> date -> int;


(* Ajout pour l'API *)
value death_symbol : config -> string;
value string_of_dmy : config -> dmy -> string;
value string_of_on_french_dmy : config -> dmy -> string;
value string_of_on_hebrew_dmy : config -> dmy -> string;
value string_of_prec_dmy : config -> string -> string -> dmy -> string;
value gregorian_precision : config -> dmy -> string;
value french_month : config -> int -> string;
value code_french_year : config -> int -> string;
value code_hebrew_date : config -> int -> int -> int -> string;

