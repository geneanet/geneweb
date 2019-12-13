(* $Id: date.ml,v 5.17 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

let dmy_of_dmy2 dmy2 =
  {day = dmy2.day2; month = dmy2.month2; year = dmy2.year2; prec = Sure;
   delta = dmy2.delta2}

let common_prec p1 p2 =
  if p1 = p2 then p1
  else
    match p1, p2 with
      Sure, _ -> p2
    | _, Sure -> p1
    | _ -> Maybe

let leap_year a = if a mod 100 = 0 then a / 100 mod 4 = 0 else a mod 4 = 0

let nb_days_in_month m a =
  if m = 2 && leap_year a
  then 29
  else if m >= 1 && m <= 12
  then [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |].(m-1)
  else 0

let time_elapsed d1 d2 =
  let prec = common_prec d1.prec d2.prec in
  match d1 with
  | {day = 0; month = 0; year = a1} ->
    {day = 0; month = 0; year = d2.year - a1; prec = prec; delta = 0}
  | {day = 0; month = m1; year = a1} ->
    begin match d2 with
        {day = 0; month = 0; year = a2} ->
        {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
        let (month, r) =
          if m1 <= m2 then m2 - m1, 0 else m2 - m1 + 12, 1
        in
        let year = a2 - a1 - r in
        {day = 0; month = month; year = year; prec = prec; delta = 0}
      | {month = m2; year = a2} ->
        let (month, r) =
          if m1 <= m2 then m2 - m1, 0 else m2 - m1 + 12, 1
        in
        let year = a2 - a1 - r in
        {day = 0; month = month; year = year; prec = prec; delta = 0}
    end
  | {day = j1; month = m1; year = a1} ->
    match d2 with
      {day = 0; month = 0; year = a2} ->
      {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
    | {day = 0; month = m2; year = a2} ->
      let (month, r) =
        if m1 <= m2 then m2 - m1, 0 else m2 - m1 + 12, 1
      in
      let year = a2 - a1 - r in
      {day = 0; month = month; year = year; prec = prec; delta = 0}
    | {day = j2; month = m2; year = a2} ->
      let (day, r) =
        if j1 <= j2 then j2 - j1, 0
        else j2 - j1 + nb_days_in_month m1 a1, 1
      in
      let (month, r) =
        if m1 + r <= m2 then m2 - m1 - r, 0 else m2 - m1 - r + 12, 1
      in
      let year = a2 - a1 - r in
      {day = day; month = month; year = year; prec = prec; delta = 0}

let date_of_death =
  function
    Death (_, cd) -> Some (Adef.date_of_cdate cd)
  | _ -> None

let get_birth_death_date p =
  let (birth_date, approx) =
    match Adef.od_of_cdate (get_birth p) with
      None -> Adef.od_of_cdate (get_baptism p), true
    | x -> x, false
  in
  let (death_date, approx) =
    match date_of_death (get_death p) with
      Some d -> Some d, approx
    | _ ->
        match get_burial p with
          Buried cd -> Adef.od_of_cdate cd, true
        | Cremated cd -> Adef.od_of_cdate cd, true
        | _ -> None, approx
  in
  birth_date, death_date, approx

exception Not_comparable

let rec compare_dmy_aux failure success ?(strict=false) dmy1 dmy2 =
  match compare dmy1.year dmy2.year with
  | 0 -> compare_month strict failure success dmy1 dmy2
  | x ->
    if strict then match x with
      | -1 when dmy1.prec = After || dmy2.prec = Before -> failure ()
      | 1 when dmy1.prec = Before || dmy2.prec = After -> failure ()
      | x -> success x
    else success x
and compare_month strict failure success dmy1 dmy2 = match dmy1.month, dmy2.month with
  | 0, 0 -> cmp_prec strict failure success dmy1 dmy2
  | 0, _ ->
    if dmy1.prec = After then success 1
    else if strict then failure ()
    else cmp_prec false failure success dmy1 dmy2
  | _, 0 ->
    if dmy2.prec = After then success (-1)
    else if strict then failure ()
    else cmp_prec false failure success dmy1 dmy2
  | m1, m2 -> match compare m1 m2 with
    | 0 -> compare_day strict failure success dmy1 dmy2
    | x -> success x
and compare_day strict failure success dmy1 dmy2 = match dmy1.day, dmy2.day with
  | 0, 0 -> cmp_prec strict failure success dmy1 dmy2
  | 0, _ ->
    if dmy1.prec = After then success 1
    else if strict then failure ()
    else cmp_prec false failure success dmy1 dmy2
  | _, 0 ->
    if dmy2.prec = After then success (-1)
    else if strict then failure ()
    else cmp_prec false failure success dmy1 dmy2
  | d1, d2 -> match compare d1 d2 with
    | 0 -> cmp_prec strict failure success dmy1 dmy2
    | x -> success x
and cmp_prec strict failure success dmy1 dmy2 =
  match dmy1.prec, dmy2.prec with
  | (Sure|About|Maybe), (Sure|About|Maybe) -> success 0
  | After, After | Before, Before -> success 0
  | OrYear dmy1, OrYear dmy2 | YearInt dmy1, YearInt dmy2 ->
    compare_dmy_aux ~strict failure success (dmy_of_dmy2 dmy1) (dmy_of_dmy2 dmy2)
  | _, After | Before, _ -> success (-1)
  | After, _ | _, Before -> success 1
  | _ -> success 0

let compare_dmy = compare_dmy_aux (fun () -> raise Not_comparable) (fun x -> x)
let compare_dmy_opt = compare_dmy_aux (fun () -> None) (fun x -> Some x)

let compare_date ?(strict=false) d1 d2 =
  match d1, d2 with
  | Dgreg (dmy1, _), Dgreg (dmy2, _) -> compare_dmy ~strict dmy1 dmy2
  | Dgreg (_, _), Dtext _ -> if strict then raise Not_comparable else 1
  | Dtext _, Dgreg (_, _) -> if strict then raise Not_comparable else -1
  | Dtext _, Dtext _ -> if strict then raise Not_comparable else 0
