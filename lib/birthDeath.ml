(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

let get_k conf =
  match p_getint conf.env "k" with
    Some x -> x
  | _ ->
      try int_of_string (List.assoc "latest_event" conf.base_env) with
        Not_found | Failure _ -> 20

let select conf base get_date find_oldest =
  let module Q =
    Pqueue.Make
      (struct
         type t = Gwdb.person * Def.dmy * Def.calendar
         let leq (_, x, _) (_, y, _) =
           if find_oldest
           then Date.compare_dmy y x <= 0
           else Date.compare_dmy x y <= 0
       end)
  in
  let n = min (max 0 (get_k conf)) (nb_of_persons base) in
  let ref_date =
    match p_getint conf.env "by" with
      Some by ->
        let bm =
          match p_getint conf.env "bm" with
            Some x -> x
          | None -> -1
        in
        let bd =
          match p_getint conf.env "bd" with
            Some x -> x
          | None -> -1
        in
        Some {day = bd; month = bm; year = by; prec = Sure; delta = 0}
    | None -> None
  in
  let (q, len) =
    Gwdb.Collection.fold (fun (q, len) i ->
        let p = pget conf base i in
        match get_date p with
        | Some (Dgreg (d, cal)) ->
          let aft =
            match ref_date with
            | Some ref_date -> Date.compare_dmy ref_date d <= 0
            | None -> false
          in
          if aft then (q, len)
          else
            let e = p, d, cal in
            if len < n then ((Q.add e q), (len + 1))
            else ((snd (Q.take (Q.add e q))), len)
        | _ -> (q, len)
      ) (Q.empty, 0) (Gwdb.ipers base)
  in
  let rec loop list q =
    if Q.is_empty q then list, len
    else let (e, q) = Q.take q in loop (e :: list) q
  in
  loop [] q

(* TODO? Factorize with select_person? *)
let select_family conf base get_date find_oldest =
  let module QF =
    Pqueue.Make
      (struct
         type t = Gwdb.ifam * Gwdb.family * Def.dmy * Def.calendar
         let leq (_, _, x, _) (_, _, y, _) =
           if find_oldest
           then Date.compare_dmy y x <= 0
           else Date.compare_dmy x y <= 0
       end)
  in
  let n = min (max 0 (get_k conf)) (nb_of_families base) in
  let ref_date =
    match p_getint conf.env "by" with
      Some by ->
        let bm =
          match p_getint conf.env "bm" with
            Some x -> x
          | None -> -1
        in
        let bd =
          match p_getint conf.env "bd" with
            Some x -> x
          | None -> -1
        in
        Some {day = bd; month = bm; year = by; prec = Sure; delta = 0}
    | None -> None
  in
  let (q, len) =
    Gwdb.Collection.fold (fun (q, len) i ->
        let fam = foi base i in
        match get_date i fam with
          | Some (Dgreg (d, cal)) ->
            let aft =
              match ref_date with
              | Some ref_date -> Date.compare_dmy ref_date d <= 0
              | None -> false
            in
            if aft then (q, len)
            else
              let e = i, fam, d, cal in
              if len < n then (QF.add e q, len + 1)
              else (snd (QF.take (QF.add e q)), len)
          | _ -> (q, len)
      ) (QF.empty, 0) (Gwdb.ifams base)
  in
  let rec loop list q =
    if QF.is_empty q then list, len
    else let (e, q) = QF.take q in loop (e :: list) q
  in
  loop [] q

let death_date p =
  match get_death p with
    Death (_, cd) -> Some (Adef.date_of_cdate cd)
  | _ -> None

let make_population_pyramid ~nb_intervals ~interval ~limit ~at_date conf base =
  let men = Array.make (nb_intervals + 1) 0 in
  let wom = Array.make (nb_intervals + 1) 0 in
  (* TODO? Load person array *)
  Gwdb.Collection.iter (fun i ->
      let p = pget conf base i in
      let sex = get_sex p in
      let dea = get_death p in
      if sex <> Neuter then
        match Adef.od_of_cdate (get_birth p) with
        | Some (Dgreg (dmy, _)) ->
          if Date.compare_dmy dmy at_date <= 0 then
            let a = Date.time_elapsed dmy at_date in
            let j = min nb_intervals (a.year / interval) in
            if (dea = NotDead || dea = DontKnowIfDead && a.year < limit)
            || match dea with
            | Death (_, cd) ->
              begin match Adef.date_of_cdate cd with
                | Dgreg (d, _) -> Date.compare_dmy d at_date > 0
                | _ -> false
              end
            | _ -> false
            then
              if sex = Male then men.(j) <- men.(j) + 1
              else wom.(j) <- wom.(j) + 1
        | Some (Dtext _) | None -> ()
    ) (Gwdb.ipers base) ;
  (men, wom)
