#ifdef API

module Mstats = Api_stats_piqi
module Mext_stats = Api_stats_piqi_ext

open Config
open Def
open Gwdb
open Api_util

(**/**) (* API_STATS *)

let list_uniq l =
  let ht = Hashtbl.create (List.length l) in
  let l =
    List.fold_left
      (fun accu x ->
        let hash = Hashtbl.hash x in
        if Hashtbl.mem ht hash then accu
        else begin Hashtbl.add ht hash (); x :: accu end)
      [] l
  in
  List.rev l

let nsplit s c =
  if s = "" then []
  else
    let rec loop list i j =
      if i = 0 then
        if s.[i] = c then ("" :: list)
        else
          let ss = String.sub s 0 j in
          (ss :: list)
      else
        if s.[i] = c then
          let ss = String.sub s (i + 1) (j - i - 1) in
          loop (ss :: list) (i - 1) i
        else loop list (i - 1) j
    in
    let len = String.length s in
    loop [] (len - 1) len

let get_wday conf d =
  let jd =
    match d with
    | Dgreg (d, _)  ->
        begin
          match d.prec with
          | Sure ->
              if (d.day <> 0 && d.month <> 0) then Calendar.sdn_of_gregorian d
              else -1
          | _ -> -1
        end
    | _ -> -1
  in
  let wday =
    let jd_today = Calendar.sdn_of_gregorian conf.today in
    let x = conf.today_wd - jd_today + jd in
    if x < 0 then 6 + (x + 1) mod 7 else x mod 7
  in
  if jd <> -1 then wday else -1

let get_month d =
  match d with
  | Dgreg (d, _)  ->
      begin
        match d.prec with
        | Sure -> if d.month <> 0 then d.month else -1
        | _ -> -1
      end
  | _ -> -1


type record_stats =
  { s_year : int;
    s_sex : Def.sex;
    s_value : int; }

let format_res ht =
  let res = ref [] in
  Hashtbl.iter
    (fun s_year l ->
      let ((nb_male, v_male), (nb_female, v_female)) =
        List.fold_left
          (fun ((nb_male, v_male), (nb_female, v_female)) r ->
            if r.s_sex = Male then
              ((nb_male + 1, v_male + r.s_value), (nb_female, v_female))
            else
              ((nb_male, v_male), (nb_female + 1 , v_female + r.s_value)))
          ((0, 0), (0, 0)) l
      in
      res := (s_year, ((nb_male, v_male), (nb_female, v_female))) :: !res)
    ht;
  List.sort (fun (k1, _) (k2, _) -> compare k1 k2) !res

let format_res2 ht =
  let res = ref [] in
  Hashtbl.iter
    (fun (s_year, wd) l ->
      let (nb_male, nb_female) =
        List.fold_left
          (fun (nb_male, nb_female) r ->
            if r.s_sex = Male then (nb_male + 1, nb_female)
            else (nb_male, nb_female + 1))
          (0, 0) l
      in
      res := (s_year, (wd, nb_male, nb_female)) :: !res)
    ht;
  List.sort
    (fun (k1, (w1, _, _)) (k2, (w2, _, _)) ->
      let v = compare k1 k2 in
      if v = 0 then compare w1 w2
      else v)
    !res

let format_res3 ht =
  let res = ref [] in
  Hashtbl.iter
    (fun s_year l ->
      let (nb_male, nb_female) =
        List.fold_left
          (fun (nb_male, nb_female) r ->
            if r.s_sex = Male then (nb_male + 1, nb_female)
            else (nb_male, nb_female + 1))
          (0, 0) l
      in
      res := (s_year, (nb_male, nb_female)) :: !res)
    ht;
  List.sort (fun (k1, _) (k2, _) -> compare k1 k2) !res

let filter_gte_1600 : 'a . (int * 'a) list -> (int * 'a) list =
  fun l -> List.filter (fun (k, _) -> k >= 1600) l

let format_stats_all l title series =
  let l = filter_gte_1600 l in
  let (labels, datas_all) =
    List.fold_right
      (fun (k, (_, (nb_f, af))) (labels, datas_all) ->
        let data =
          Mstats.Data.({
            nb = Int32.of_int nb_f;
            value = Int32.of_int af;
          })
        in
        (Int32.of_int k :: labels, (data :: datas_all)))
      l ([], [])
  in
  let datas_all = Mstats.Data_l.({data = datas_all;}) in
  let datas = [datas_all] in
  Mstats.Stat.({
    title = title;
    labels = labels;
    series = series;
    series_string = [];
    datas = datas;
  })

let format_stats_m_f l title series =
  let l = filter_gte_1600 l in
  let (labels, (datas_male, datas_female)) =
    List.fold_right
      (fun (k, ((nb_m, am), (nb_f, af))) (labels, (datas_male, datas_female)) ->
        let data_male =
          Mstats.Data.({
            nb = Int32.of_int nb_m;
            value = Int32.of_int am;
          })
        in
        let data_female =
          Mstats.Data.({
            nb = Int32.of_int nb_f;
            value = Int32.of_int af;
          })
        in
        (Int32.of_int k :: labels,
         (data_male :: datas_male, data_female :: datas_female)))
      l ([], ([], []))
  in
  let datas_male = Mstats.Data_l.({data = datas_male;}) in
  let datas_female = Mstats.Data_l.({data = datas_female;}) in
  let datas = [datas_male; datas_female] in
  Mstats.Stat.({
    title = title;
    labels = labels;
    series = series;
    series_string = [];
    datas = datas;
  })

let format_stats_m_f2 l1 l2 title series1 series2 =
  let l1 = filter_gte_1600 l1 in
  let l2 = filter_gte_1600 l2 in
  let labels =
    List.sort_uniq compare @@
    List.rev_map fst @@
    List.rev_append l1 l2
  in
  let ht_labels = Hashtbl.create (List.length labels) in
  let () =
    let rec loop i l =
      match l with
      | [] -> ()
      | y :: l -> Hashtbl.add ht_labels y i; loop (i + 1) l
    in
    loop 0 labels
  in
  let data = Array.make (List.length series1 + List.length series2) ([| |]) in
  let data =
    Array.map
      (fun _ ->
        Array.make (List.length labels)
          (Mstats.Data.({
            nb = Int32.zero;
            value = Int32.zero;
          })))
      data
  in
  let () =
    List.iter
      (fun (k, ((nb_m, am), (nb_f, af))) ->
        try
          let pos = Hashtbl.find ht_labels k in
          let data_male =
            Mstats.Data.({
              nb = Int32.of_int nb_m;
              value = Int32.of_int am;
            })
          in
          let data_female =
            Mstats.Data.({
              nb = Int32.of_int nb_f;
              value = Int32.of_int af;
            })
          in
          data.(0).(pos) <- data_male;
          data.(1).(pos) <- data_female;
        with Not_found -> ())
      l1
  in
  let () =
    List.iter
      (fun (k, ((nb_m, am), (nb_f, af))) ->
        try
          let pos = Hashtbl.find ht_labels k in
          let data_male =
            Mstats.Data.({
              nb = Int32.of_int nb_m;
              value = Int32.of_int am;
            })
          in
          let data_female =
            Mstats.Data.({
              nb = Int32.of_int nb_f;
              value = Int32.of_int af;
            })
          in
          data.(2).(pos) <- data_male;
          data.(3).(pos) <- data_female;
        with Not_found -> ())
      l2
  in
  let datas =
    Mutil.array_to_list_map
      (fun s -> Mstats.Data_l.({data = Array.to_list s;}))
      data
  in
  let labels = List.map Int32.of_int labels in
  Mstats.Stat.({
    title = title;
    labels = labels;
    series = series1 @ series2;
    series_string = [];
    datas = datas;
  })

let format_stats_dmy l title series =
  let l = filter_gte_1600 l in
  let ht = Hashtbl.create (List.length l) in
  let () =
    List.iter
      (fun (k, (md, nb_m, nb_f)) -> Hashtbl.add ht k (md, nb_m, nb_f))
      l
  in
  let labels = list_uniq (List.map (fun (k, _) -> k) l) in
  let data = Array.make (List.length series) ([| |]) in
  let data =
    Array.map
      (fun _ ->
        Array.make (List.length labels)
          (Mstats.Data.({
            nb = Int32.zero;
            value = Int32.zero;
          })))
      data
  in
  let rec loop i l =
    match l with
    | [] -> ()
    | year :: l ->
        List.iter
          (fun (md, nb_m, nb_f) ->
             let v =
               Mstats.Data.({
                 nb = Int32.of_int (nb_m + nb_f);
                 value = Int32.zero;
               })
             in
             data.(md).(i) <- v)
          (Hashtbl.find_all ht year);
        loop (i + 1) l
  in
  let () = loop 0 labels in
  let datas =
    Mutil.array_to_list_map
      (fun s -> Mstats.Data_l.({data = Array.to_list s;}))
      data
  in
  let labels = List.map Int32.of_int labels in
  Mstats.Stat.({
      title = title;
      labels = labels;
      series = series;
      series_string = [];
      datas = datas;
    })

let filter_gte_1600_and_create_ht l =
  let l = filter_gte_1600 l in
  let ht = Hashtbl.create (List.length l) in
  let () = List.iter (fun (k, v) -> Hashtbl.add ht k v) l in
  (l, ht)

let format_stats_day l title =
  let l, ht = filter_gte_1600_and_create_ht l in
  let labels = list_uniq (List.map (fun (k, _) -> k) l) in
  let (data_mon, data_tue, data_wed, data_thu, data_fri, data_sat, data_sun) =
    List.fold_right
      (fun year data ->
        List.fold_right
          (fun (md, nb_m, nb_f)
               (data_mon, data_tue, data_wed, data_thu,
                data_fri, data_sat, data_sun) ->
            let data =
              Mstats.Data.({
                nb = Int32.of_int (nb_m + nb_f);
                value = Int32.zero;
              })
            in
            let zero_data =
              Mstats.Data.({
                nb = Int32.zero;
                value = Int32.zero;
              })
            in
            let data_sun =
              if md = 0 then data :: data_sun else zero_data :: data_sun
            in
            let data_mon =
              if md = 1 then data :: data_mon else zero_data :: data_mon
            in
            let data_tue =
              if md = 2 then data :: data_tue else zero_data :: data_tue
            in
            let data_wed =
              if md = 3 then data :: data_wed else zero_data :: data_wed
            in
            let data_thu =
              if md = 4 then data :: data_thu else zero_data :: data_thu
            in
            let data_fri =
              if md = 5 then data :: data_fri else zero_data :: data_fri
            in
            let data_sat =
              if md = 6 then data :: data_sat else zero_data :: data_sat
            in
            (data_mon, data_tue, data_wed, data_thu,
             data_fri, data_sat, data_sun))
          (Hashtbl.find_all ht year) data)
      labels ([], [], [], [], [], [], [])
  in
  let datas_mon = Mstats.Data_l.({data = data_mon;}) in
  let datas_tue = Mstats.Data_l.({data = data_tue;}) in
  let datas_wed = Mstats.Data_l.({data = data_wed;}) in
  let datas_thu = Mstats.Data_l.({data = data_thu;}) in
  let datas_fri = Mstats.Data_l.({data = data_fri;}) in
  let datas_sat = Mstats.Data_l.({data = data_sat;}) in
  let datas_sun = Mstats.Data_l.({data = data_sun;}) in
  let datas =
    [datas_mon; datas_tue; datas_wed; datas_thu;
     datas_fri; datas_sat; datas_sun]
  in
  let labels = List.map Int32.of_int labels in
  let series =
    [`serie_day_1; `serie_day_2; `serie_day_3; `serie_day_4;
     `serie_day_5; `serie_day_6; `serie_day_7;]
  in
  Mstats.Stat.({
      title = title;
      labels = labels;
      series = series;
      series_string = [];
      datas = datas;
    })

let format_stats_month l title =
  let l, ht = filter_gte_1600_and_create_ht l in
  let labels = list_uniq (List.map (fun (k, _) -> k) l) in
  let (data_jan, data_feb, data_mar, data_apr,
       data_may, data_jun, data_jul, data_aug,
       data_sep, data_oct, data_nov, data_dec) =
    List.fold_right
      (fun k data ->
        List.fold_right
          (fun (md, nb_m, nb_f)
               (data_jan, data_feb, data_mar, data_apr,
                data_may, data_jun, data_jul, data_aug,
                data_sep, data_oct, data_nov, data_dec) ->
            let data =
              Mstats.Data.({
                nb = Int32.of_int (nb_m + nb_f);
                value = Int32.zero;
              })
            in
            let zero_data =
              Mstats.Data.({
                nb = Int32.zero;
                value = Int32.zero;
              })
            in
            let data_jan =
              if md = 1 then data :: data_jan else zero_data :: data_jan
            in
            let data_feb =
              if md = 2 then data :: data_feb else zero_data :: data_feb
            in
            let data_mar =
              if md = 3 then data :: data_mar else zero_data :: data_mar
            in
            let data_apr =
              if md = 4 then data :: data_apr else zero_data :: data_apr
            in
            let data_may =
              if md = 5 then data :: data_may else zero_data :: data_may
            in
            let data_jun =
              if md = 6 then data :: data_jun else zero_data :: data_jun
            in
            let data_jul =
              if md = 7 then data :: data_jul else zero_data :: data_jul
            in
            let data_aug =
              if md = 8 then data :: data_aug else zero_data :: data_aug
            in
            let data_sep =
              if md = 9 then data :: data_sep else zero_data :: data_sep
            in
            let data_oct =
              if md = 10 then data :: data_oct else zero_data :: data_oct
            in
            let data_nov =
              if md = 11 then data :: data_nov else zero_data :: data_nov
            in
            let data_dec =
              if md = 12 then data :: data_dec else zero_data :: data_dec
            in
            (data_jan, data_feb, data_mar, data_apr,
             data_may, data_jun, data_jul, data_aug,
             data_sep, data_oct, data_nov, data_dec))
          (Hashtbl.find_all ht k) data)
      labels ([], [], [], [], [], [], [], [], [], [], [], [])
  in
  let datas_jan = Mstats.Data_l.({data = data_jan;}) in
  let datas_feb = Mstats.Data_l.({data = data_feb;}) in
  let datas_mar = Mstats.Data_l.({data = data_mar;}) in
  let datas_apr = Mstats.Data_l.({data = data_apr;}) in
  let datas_may = Mstats.Data_l.({data = data_may;}) in
  let datas_jun = Mstats.Data_l.({data = data_jun;}) in
  let datas_jul = Mstats.Data_l.({data = data_jul;}) in
  let datas_aug = Mstats.Data_l.({data = data_aug;}) in
  let datas_sep = Mstats.Data_l.({data = data_sep;}) in
  let datas_oct = Mstats.Data_l.({data = data_oct;}) in
  let datas_nov = Mstats.Data_l.({data = data_nov;}) in
  let datas_dec = Mstats.Data_l.({data = data_dec;}) in
  let datas =
    [datas_jan; datas_feb; datas_mar; datas_apr;
     datas_may; datas_jun; datas_jul; datas_aug;
     datas_sep; datas_oct; datas_nov; datas_dec]
  in
  let labels = List.map Int32.of_int labels in
  let series =
    [`serie_month_1; `serie_month_2; `serie_month_3; `serie_month_4;
     `serie_month_5; `serie_month_6; `serie_month_7; `serie_month_8;
     `serie_month_9; `serie_month_10; `serie_month_11; `serie_month_12]
  in
  Mstats.Stat.({
    title = title;
    labels = labels;
    series = series;
    series_string = [];
    datas = datas;
  })

let format_top_stats ht title =
  let res = ref [] in
  Hashtbl.iter (fun _ (s, n) -> res := (s, n) :: !res) ht;
  let l =
    List.sort (fun (_, n1) (_, n2) -> if n1 > n2 then -1 else 1) !res
  in
  let l = Util.reduce_list 10 l in
  let (series_string, datas) =
    List.fold_right
      (fun (s, n) (series, datas) ->
         let serie = s in
         let data =
           Mstats.Data.({
             nb = Int32.of_int n;
             value = Int32.one;
           })
         in
         (serie :: series, data :: datas))
      l ([], [])
  in
  let datas =
    List.map
      (fun datas -> Mstats.Data_l.({data = [datas];}))
      datas
  in
  Mstats.Stat.({
    title = title;
    labels = [Int32.zero];
    series =
      [`serie_top_10_1; `serie_top_10_2; `serie_top_10_3; `serie_top_10_4;
       `serie_top_10_5; `serie_top_10_6; `serie_top_10_7; `serie_top_10_8;
       `serie_top_10_9; `serie_top_10_10];
    series_string = series_string;
    datas = datas;
  })

let format_stats_astro l title =
  let l, ht = filter_gte_1600_and_create_ht l in
  let labels = list_uniq (List.map (fun (k, _) -> k) l) in
  let (data_aries, data_taurus, data_gemini, data_cancer,
       data_leo, data_virgo, data_libra, data_scorpio,
       data_sagittarius, data_capricorn, data_aquarius, data_pisces) =
    List.fold_right
      (fun k data ->
        List.fold_right
          (fun (md, nb_m, nb_f)
               (data_aries, data_taurus, data_gemini, data_cancer,
                data_leo, data_virgo, data_libra, data_scorpio,
                data_sagittarius, data_capricorn, data_aquarius, data_pisces) ->
            let md = md + 1 in
            let data = nb_m + nb_f in
            let data_aries =
              if md = 1 then data :: data_aries else data_aries
            in
            let data_taurus =
              if md = 2 then data :: data_taurus else data_taurus
            in
            let data_gemini =
              if md = 3 then data :: data_gemini else data_gemini
            in
            let data_cancer =
              if md = 4 then data :: data_cancer else data_cancer
            in
            let data_leo =
              if md = 5 then data :: data_leo else data_leo
            in
            let data_virgo =
              if md = 6 then data :: data_virgo else data_virgo
            in
            let data_libra =
              if md = 7 then data :: data_libra else data_libra
            in
            let data_scorpio =
              if md = 8 then data :: data_scorpio else data_scorpio
            in
            let data_sagittarius =
              if md = 9 then data :: data_sagittarius else data_sagittarius
            in
            let data_capricorn =
              if md = 10 then data :: data_capricorn else data_capricorn
            in
            let data_aquarius =
              if md = 11 then data :: data_aquarius else data_aquarius
            in
            let data_pisces =
              if md = 12 then data :: data_pisces else data_pisces
            in
            (data_aries, data_taurus, data_gemini, data_cancer,
             data_leo, data_virgo, data_libra, data_scorpio,
             data_sagittarius, data_capricorn, data_aquarius, data_pisces))
          (Hashtbl.find_all ht k) data)
      labels ([], [], [], [], [], [], [], [], [], [], [], [])
  in
  let data_aries =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_aries);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_taurus =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_taurus);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_gemini =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_gemini);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_cancer =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_cancer);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_leo =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_leo);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_virgo =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_virgo);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_libra =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_libra);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_scorpio =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_scorpio);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_sagittarius =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_sagittarius);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_capricorn =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_capricorn);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_aquarius =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_aquarius);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_pisces =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_pisces);
        value = Int32.zero;
      })
    in
    [data]
  in
  let datas_aries = Mstats.Data_l.({data = data_aries;}) in
  let datas_taurus = Mstats.Data_l.({data = data_taurus;}) in
  let datas_gemini = Mstats.Data_l.({data = data_gemini;}) in
  let datas_cancer = Mstats.Data_l.({data = data_cancer;}) in
  let datas_leo = Mstats.Data_l.({data = data_leo;}) in
  let datas_virgo = Mstats.Data_l.({data = data_virgo;}) in
  let datas_libra = Mstats.Data_l.({data = data_libra;}) in
  let datas_scorpio = Mstats.Data_l.({data = data_scorpio;}) in
  let datas_sagittarius = Mstats.Data_l.({data = data_sagittarius;}) in
  let datas_capricorn = Mstats.Data_l.({data = data_capricorn;}) in
  let datas_aquarius = Mstats.Data_l.({data = data_aquarius;}) in
  let datas_pisces = Mstats.Data_l.({data = data_pisces;}) in
  let datas =
    [datas_aries; datas_taurus; datas_gemini; datas_cancer;
     datas_leo; datas_virgo; datas_libra; datas_scorpio;
     datas_sagittarius; datas_capricorn; datas_aquarius; datas_pisces]
  in
  let series =
    [`serie_aries; `serie_taurus; `serie_gemini; `serie_cancer;
     `serie_leo; `serie_virgo; `serie_libra; `serie_scorpio;
     `serie_sagittarius; `serie_capricorn; `serie_aquarius;
     `serie_pisces]
  in
  Mstats.Stat.({
    title = title;
    labels = [Int32.zero];
    series = series;
    series_string = [];
    datas = datas;
  })

let format_stats_moon l title =
  let l, ht = filter_gte_1600_and_create_ht l in
  let labels = list_uniq (List.map (fun (k, _) -> k) l) in
  let (data_new, data_first, data_full, data_last) =
    List.fold_right
      (fun year data ->
        List.fold_right
          (fun (md, nb_m, nb_f)
               (data_new, data_first, data_full, data_last) ->
            let data = nb_m + nb_f in
            let data_new =
              if md = 1 then data :: data_new else data_new
            in
            let data_first =
              if md = 2 then data :: data_first else data_first
            in
            let data_full =
              if md = 3 then data :: data_full else data_full
            in
            let data_last =
              if md = 4 then data :: data_last else data_last
            in
            (data_new, data_first, data_full, data_last))
          (Hashtbl.find_all ht year) data)
      labels ([], [], [], [])
  in
  let data_new =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_new);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_first =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_first);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_full =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_full);
        value = Int32.zero;
      })
    in
    [data]
  in
  let data_last =
    let data =
      Mstats.Data.({
        nb = Int32.of_int (List.fold_left (+) 0 data_last);
        value = Int32.zero;
      })
    in
    [data]
  in
  let datas_new = Mstats.Data_l.({data = data_new;}) in
  let datas_first = Mstats.Data_l.({data = data_first;}) in
  let datas_full = Mstats.Data_l.({data = data_full;}) in
  let datas_last = Mstats.Data_l.({data = data_last;}) in
  let datas =
    [datas_new; datas_first; datas_full; datas_last]
  in
  let series =
    [`serie_moon_new; `serie_moon_first_quarter; `serie_moon_full;
     `serie_moon_last_quarter ]
  in
  Mstats.Stat.({
      title = title;
      labels = [Int32.zero];
      series = series;
      series_string = [];
      datas = datas;
    })

let print_ind_stats conf base =
  let params =
    get_params conf (fun a b -> Mext_stats.parse_stats_params a b)
  in

  (* nombre d'ascendants *)
  let (ancestors, stats_ancestors) =
    let rec loop gen ipl accu =
      match ipl with
      | [] -> List.rev accu
      | ipl ->
          if gen >= 15 then List.rev accu
          else
            let next_gen =
              List.fold_left
                (fun accu ip ->
                  let p = poi base ip in
                  match get_parents p with
                  | Some ifam ->
                      let cpl = foi base ifam in
                      get_father cpl :: get_mother cpl :: accu
                  | None -> accu)
                [] ipl
            in
            loop (gen + 1) next_gen (ipl :: accu)
    in
    match params.Mstats.Stats_params.i with
    | Some i ->
        let ip = Gwdb.iper_of_string i in
        let ancestors = loop 0 [ip] [] in
        let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
        let (datas_found, datas_diss) =
          List.fold_left
            (fun (datas_found, datas_diss) l ->
              let found = List.length l in
              let diss =
                List.fold_left
                  (fun accu ip ->
                    if Gwdb.Marker.get mark ip then accu
                    else begin Gwdb.Marker.set mark ip true; succ accu end)
                  0 l
              in
              let data_found =
                Mstats.Data.({
                  nb = Int32.of_int found;
                  value = Int32.one;
                })
              in
              let data_diss =
                Mstats.Data.({
                  nb = Int32.of_int diss;
                  value = Int32.one;
                })
              in
              (data_found :: datas_found, data_diss :: datas_diss))
            ([], []) ancestors
        in
        let (datas_found, datas_diss) =
          (List.rev datas_found, List.rev datas_diss)
        in
        let labels =
          let rec loop i l accu =
            match l with
            | [] -> accu
            | _ :: l -> loop (i + 1) l (Int32.of_int i :: accu)
          in
          List.rev (loop 0 ancestors [])
        in
        let datas =
          [Mstats.Data_l.({data = datas_found;});
           Mstats.Data_l.({data = datas_diss;})]
        in
        let stats =
          Mstats.Stat.({
            title = `st_asc;
            labels = labels;
            series = [`serie_asc_found; `serie_asc_uniq];
            series_string = [];
            datas = datas;
          })
        in
        (ancestors, stats)
    | None ->
        let stats =
          Mstats.Stat.({
            title = `st_asc;
            labels = [];
            series = [];
            series_string = [];
            datas = [];
          })
        in
        ([], stats)
  in

  (* nombre de descendants *)
  let (descendants, stats_descendants) =
    let rec loop gen ipl accu =
      match ipl with
      | [] -> List.rev accu
      | ipl ->
          if gen >= 15 then List.rev accu
          else
            let next_gen =
              List.fold_left
                (fun accu ip ->
                  let p = poi base ip in
                  Array.fold_left
                    (fun accu ifam ->
                       Array.fold_right List.cons (get_children @@ foi base ifam) accu)
                    accu (get_family p))
                [] ipl
            in
            loop (gen + 1) next_gen (ipl :: accu)
    in
    match params.Mstats.Stats_params.i with
    | Some i ->
        let ip = Gwdb.iper_of_string i in
        let descendants = loop 0 [ip] [] in
        let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
        let (datas_found, datas_diss) =
          List.fold_left
            (fun (datas_found, datas_diss) l ->
              let found = List.length l in
              let diss =
                List.fold_left
                  (fun accu ip ->
                    if Gwdb.Marker.get mark ip then accu
                    else begin Gwdb.Marker.set mark ip true; succ accu end)
                  0 l
              in
              let data_found =
                Mstats.Data.({
                  nb = Int32.of_int found;
                  value = Int32.one;
                })
              in
              let data_diss =
                Mstats.Data.({
                  nb = Int32.of_int diss;
                  value = Int32.one;
                })
              in
              (data_found :: datas_found, data_diss :: datas_diss))
            ([], []) descendants
        in
        let (datas_found, datas_diss) = (List.rev datas_found, List.rev datas_diss) in
        let labels =
          let rec loop i l accu =
            match l with
            | [] -> accu
            | _ :: l -> loop (i + 1) l (Int32.of_int i :: accu)
          in
          List.rev (loop 0 descendants [])
        in
        let datas = [Mstats.Data_l.({data = datas_found;}); Mstats.Data_l.({data = datas_diss;})] in
        let stats =
          Mstats.Stat.({
            title = `st_desc;
            labels = labels;
            series = [`serie_desc_found; `serie_desc_uniq];
            series_string = [];
            datas = datas;
          })
        in
        (descendants, stats)
    | None ->
        let stats =
          Mstats.Stat.({
            title = `st_desc;
            labels = [];
            series = [];
            series_string = [];
            datas = [];
          })
        in
        ([], stats)
  in

  (* répartition homme femme sur la descendance *)
  let stats_descendants_m_f =
    let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
    let (nb_male, nb_female) =
      List.fold_left
        (fun (nb_male, nb_female) l ->
          List.fold_left
            (fun (nb_male, nb_female) ip ->
              if Gwdb.Marker.get mark ip then (nb_male, nb_female)
              else
                begin
                  Gwdb.Marker.set mark ip true;
                  let p = poi base ip in
                  match get_sex p with
                  | Male -> (nb_male + 1, nb_female)
                  | Female -> (nb_male, nb_female + 1)
                  | Neuter -> (nb_male, nb_female)
                end)
            (nb_male, nb_female) l)
        (0, 0) descendants
    in
    let data_male =
      Mstats.Data.({
        nb = Int32.of_int nb_male;
        value = Int32.one;
      })
    in
    let data_female =
      Mstats.Data.({
        nb = Int32.of_int nb_female;
        value = Int32.one;
      })
    in
    let datas_male = Mstats.Data_l.({data = [data_male];}) in
    let datas_female = Mstats.Data_l.({data = [data_female];}) in
    Mstats.Stat.({
      title = `st_desc_man_woman;
      labels = [Int32.zero];
      series = [`serie_male; `serie_female];
      series_string = [];
      datas = [datas_male; datas_female];
    })
  in

  (* calcul de fréquence de nom/prénom *)
  let compute_stats_name title proj split l =
    let ht = Hashtbl.create 1001 in
    List.iter
      (fun l ->
        List.iter
          (fun ip ->
            let p = poi base ip in
            let p_auth = Util.authorized_age conf base p in
            if p_auth then
              let s = sou base (proj p) in
              let sl =
                if split then nsplit s ' '
                else [s]
              in
              List.iter
                (fun s ->
                  let s = if split then Name.strip_c s '"' else s in
                  let k = Name.lower s in
                  if k = "?" || k = "" then ()
                  else
                    try
                      let (s, n) = Hashtbl.find ht k in
                      Hashtbl.replace ht k (s, (n + 1))
                    with Not_found -> Hashtbl.add ht k (s, 1))
                sl
            else ())
          l)
      l;
    let res = ref [] in
    Hashtbl.iter (fun _ (s, n) -> res := (s, n) :: !res) ht;
    let l =
      List.sort
        (fun (_, n1) (_, n2) -> if n1 > n2 then -1 else 1)
        !res
    in
    let l = Util.reduce_list 10 l in
    let (series_string, datas) =
      List.fold_right
        (fun (s, n) (series, datas) ->
           let serie = s in
           let data =
             Mstats.Data.({
               nb = Int32.of_int n;
               value = Int32.one;
             })
           in
           (serie :: series, data :: datas))
        l ([], [])
    in
    let datas =
      List.map
        (fun datas -> Mstats.Data_l.({data = [datas];}))
        datas
    in
    Mstats.Stat.({
      title = title;
      labels = [Int32.zero];
      series =
        [`serie_top_10_1; `serie_top_10_2; `serie_top_10_3; `serie_top_10_4;
         `serie_top_10_5; `serie_top_10_6; `serie_top_10_7; `serie_top_10_8;
         `serie_top_10_9; `serie_top_10_10];
      series_string = series_string;
      datas = datas;
    })
  in

  (* calcul de fréquence des occupation *)
  let compute_stats_occupation title proj split l =
    let ht = Hashtbl.create 1001 in
    List.iter
      (fun l ->
        List.iter
          (fun ip ->
            let p = poi base ip in
            let p_auth = Util.authorized_age conf base p in
            if p_auth then
              begin
                let s = sou base (proj p) in
                let sl =
                  if split then nsplit s ','
                  else [s]
                in
                List.iter
                  (fun s ->
                    let k = Name.lower s in
                    if k = "?" || k = "" then ()
                    else
                      try
                        let (s, n) = Hashtbl.find ht k in
                        Hashtbl.replace ht k (s, (n + 1))
                      with Not_found -> Hashtbl.add ht k (s, 1))
                  sl;
                  List.iter
                    (fun e ->
                      let s = sou base e.epers_note in
                      let k = Name.lower s in
                      if k = "" then ()
                      else
                        try
                          let (s, n) = Hashtbl.find ht k in
                          Hashtbl.replace ht k (s, (n + 1))
                        with Not_found -> Hashtbl.add ht k (s, 1))
                    (List.filter (fun e -> e.epers_name = Epers_Occupation) (get_pevents p));
              end
            else ())
          l)
      l;
    let res = ref [] in
    Hashtbl.iter (fun _ (s, n) -> res := (s, n) :: !res) ht;
    let l =
      List.sort
        (fun (_, n1) (_, n2) -> if n1 > n2 then -1 else 1)
        !res
    in
    let l = Util.reduce_list 10 l in
    let (series_string, datas) =
      List.fold_right
        (fun (s, n) (series, datas) ->
           let serie = s in
           let data =
             Mstats.Data.({
               nb = Int32.of_int n;
               value = Int32.one;
             })
           in
           (serie :: series, data :: datas))
        l ([], [])
    in
    let datas =
      List.map
        (fun datas -> Mstats.Data_l.({data = [datas];}))
        datas
    in
    Mstats.Stat.({
      title = title;
      labels = [Int32.zero];
      series =
        [`serie_top_10_1; `serie_top_10_2; `serie_top_10_3; `serie_top_10_4;
         `serie_top_10_5; `serie_top_10_6; `serie_top_10_7; `serie_top_10_8;
         `serie_top_10_9; `serie_top_10_10];
      series_string = series_string;
      datas = datas;
    })
  in

  (* fréquence nom sur l'ascendance *)
  let stats_asc_surname =
    compute_stats_name `st_asc_lastname get_surname false ancestors
  in

  (* fréquence prénom sur l'ascendance *)
  let stats_asc_first_name =
    compute_stats_name `st_asc_firstname get_first_name true ancestors
  in

  (* fréquence occupation sur l'ascendance *)
  let stats_asc_occupation =
    compute_stats_occupation `st_asc_occupation get_occupation true ancestors
  in

  (* fréquence nom sur la descendance *)
  let stats_desc_surname =
    compute_stats_name `st_desc_lastname get_surname false descendants
  in

  (* fréquence prénom sur la descendance *)
  let stats_desc_first_name =
    compute_stats_name `st_desc_firstname get_first_name true descendants
  in

  (* fréquence occupation sur la descendance *)
  let stats_desc_occupation =
    compute_stats_occupation `st_desc_occupation get_occupation true descendants
  in

  let all_stats =
    [stats_ancestors; stats_descendants; stats_descendants_m_f;
     stats_asc_surname; stats_asc_first_name; stats_asc_occupation;
     stats_desc_surname; stats_desc_first_name; stats_desc_occupation]
  in

  let stats =
    Mstats.Stats.({
      stats = all_stats;
    })
  in

  let data = Mext_stats.gen_stats stats in
  print_result conf data


let print_all_stats conf base =
  let periode = 5 in
  let nb_pers = nb_of_persons base in
  let nb_fam = nb_of_families base in

  let ht_longuest = Hashtbl.create nb_pers in
  let ht_day_birth = Hashtbl.create 7 in
  let ht_month_birth = Hashtbl.create 12 in
  let ht_month_death = Hashtbl.create 12 in
  let ht_male_female = Hashtbl.create nb_pers in
  let ht_moon = Hashtbl.create nb_pers in
  let ht_astro = Hashtbl.create nb_pers in

  let ht_day_marr = Hashtbl.create 7 in
  let ht_month_marr = Hashtbl.create 12 in
  let ht_marr_age = Hashtbl.create nb_pers in
  let ht_marr_diff_age_cpl = Hashtbl.create nb_pers in
  let ht_moy_marr = Hashtbl.create nb_pers in
  let ht_marr_time = Hashtbl.create nb_pers in
  let ht_fecondite = Hashtbl.create nb_pers in
  let ht_moy_age_birth = Hashtbl.create nb_pers in
  let ht_moy_age_first_birth = Hashtbl.create nb_pers in
  let ht_moy_age_last_birth = Hashtbl.create nb_pers in
  let ht_diff_age_child = Hashtbl.create nb_pers in
  let ht_diff_age_extr_child = Hashtbl.create nb_pers in

  (* TOP 10 *)
  let ht_occupation = Hashtbl.create nb_pers in
  let ht_surname = Hashtbl.create nb_pers in
  let ht_first_name = Hashtbl.create nb_pers in
  let ht_max_child = Hashtbl.create nb_fam in
  let ht_young_dad = Hashtbl.create nb_pers in
  let ht_young_mom = Hashtbl.create nb_pers in

  Gwdb.Collection.iter begin fun p ->
    let p_auth = Util.authorized_age conf base p in
    let faml = Array.to_list (get_family p) in
    let ip = get_iper p in

    if get_sex p = Neuter then ()
    else
      begin
        match Date.get_birth_death_date p with
        | (Some (Dgreg (({prec = Sure} as dmy1), _) as d1),
           Some (Dgreg (({prec = Sure} as dmy2), _) as d2), _) ->
            begin
              (* plus vieux *)
              if p_auth && d1 <> d2 then
                let a = Date.time_elapsed dmy1 dmy2 in
                let r =
                  { s_year = dmy2.year - (dmy2.year mod periode);
                    s_sex = get_sex p;
                    s_value = a.year; }
                in
                begin
                  try
                    let l = Hashtbl.find ht_longuest r.s_year in
                    Hashtbl.replace ht_longuest r.s_year (r :: l)
                  with Not_found -> Hashtbl.add ht_longuest r.s_year [r]
                end
              else ()
            end;

            (* jour de naissance *)
            begin
              if p_auth && dmy1.day > 0 then
                let wd = get_wday conf d1 in
                if wd <> -1 then
                  let wd = (wd + 6) mod 7 in
                  let r =
                    { s_year = dmy1.year - (dmy1.year mod periode);
                      s_sex = get_sex p;
                      s_value = wd; }
                  in
                  begin
                    try
                      let l = Hashtbl.find ht_day_birth (r.s_year, r.s_value) in
                      Hashtbl.replace ht_day_birth (r.s_year, r.s_value) (r :: l)
                    with Not_found -> Hashtbl.add ht_day_birth (r.s_year, r.s_value) [r]
                  end
                else ()
              else ()
            end;

            (* mois de naissance *)
            begin
              if p_auth && dmy1.month > 0 then
                let md = get_month d1 in
                if md <> -1 then
                  let md = md - 1 in
                  let r =
                    { s_year = dmy1.year - (dmy1.year mod periode);
                      s_sex = get_sex p;
                      s_value = md; }
                  in
                  begin
                    try
                      let l = Hashtbl.find ht_month_birth (r.s_year, r.s_value) in
                      Hashtbl.replace ht_month_birth (r.s_year, r.s_value) (r :: l)
                    with Not_found -> Hashtbl.add ht_month_birth (r.s_year, r.s_value) [r]
                  end
                else ()
              else ()
            end;

            (* mois de décès *)
            begin
              if p_auth && dmy2.month > 0 then
                let md = get_month d2 in
                if md <> -1 then
                  let md = md - 1 in
                  let r =
                    { s_year = dmy2.year - (dmy2.year mod periode);
                      s_sex = get_sex p;
                      s_value = md; }
                  in
                  begin
                    try
                      let l = Hashtbl.find ht_month_death (r.s_year, r.s_value) in
                      Hashtbl.replace ht_month_death (r.s_year, r.s_value) (r :: l)
                    with Not_found -> Hashtbl.add ht_month_death (r.s_year, r.s_value) [r]
                  end
                else ()
              else ()
            end;

            (* répartition homme femme *)
            begin
              if p_auth then
                let r =
                  { s_year = dmy1.year - (dmy1.year mod periode);
                    s_sex = get_sex p;
                    s_value = 1; }
                in
                begin
                  try
                    let l = Hashtbl.find ht_male_female r.s_year in
                    Hashtbl.replace ht_male_female r.s_year (r :: l)
                  with Not_found -> Hashtbl.add ht_male_female r.s_year [r]
                end
              else ()
            end;

            (* signe astrologique *)
            begin
              if p_auth && dmy1.day > 0 then
                begin
                  let v =
                    if dmy1.day >= 15 && dmy1.month = 4 || dmy1.day <= 15 && dmy1.month = 5 then 0
                    else if dmy1.day >= 16 && dmy1.month = 5 || dmy1.day <= 15 && dmy1.month = 6 then 1
                    else if dmy1.day >= 16 && dmy1.month = 6 || dmy1.day <= 15 && dmy1.month = 7 then 2
                    else if dmy1.day >= 16 && dmy1.month = 7 || dmy1.day <= 15 && dmy1.month = 8 then 3
                    else if dmy1.day >= 16 && dmy1.month = 8 || dmy1.day <= 15 && dmy1.month = 9 then 4
                    else if dmy1.day >= 16 && dmy1.month = 9 || dmy1.day <= 15 && dmy1.month = 10 then 5
                    else if dmy1.day >= 16 && dmy1.month = 10 || dmy1.day <= 15 && dmy1.month = 11 then 6
                    else if dmy1.day >= 16 && dmy1.month = 11 || dmy1.day <= 15 && dmy1.month = 12 then 7
                    else if dmy1.day >= 16 && dmy1.month = 12 || dmy1.day <= 14 && dmy1.month = 1 then 8
                    else if dmy1.day >= 15 && dmy1.month = 1 || dmy1.day <= 14 && dmy1.month = 2 then 9
                    else if dmy1.day >= 15 && dmy1.month = 2 || dmy1.day <= 14 && dmy1.month = 3 then 10
                    else if dmy1.day >= 15 && dmy1.month = 3 || dmy1.day <= 14 && dmy1.month = 4 then 11
                    else -1
                  in
                  if v <> -1 then
                    let r =
                      { s_year = dmy1.year - (dmy1.year mod periode);
                        s_sex = get_sex p;
                        s_value = v; }
                    in
                    try
                      let l = Hashtbl.find ht_astro (r.s_year, r.s_value) in
                      Hashtbl.replace ht_astro (r.s_year, r.s_value) (r :: l)
                    with Not_found -> Hashtbl.add ht_astro (r.s_year, r.s_value) [r]
                  else ()
                end
              else ()
            end;

            (* influence de la lune *)
            begin
              if p_auth && dmy1.delta = 0 && dmy1.day > 0 then
                begin
                  try
                    begin
                      let jd = Calendar.sdn_of_gregorian dmy1 in
                      let (mp, md) = Calendar.moon_phase_of_sdn jd in
                      let i =
                        match mp with
                        | None ->
                            let md = float_of_int md in
                            if md <= 3.69 then 1
                            else if md <= 11.07 then 2
                            else if md <= 18.45 then 3
                            else if md <= 25.83 then 4
                            else 1
                        | Some (Calendar.NewMoon, _, _) -> 1
                        | Some (Calendar.FirstQuarter, _, _) -> 2
                        | Some (Calendar.FullMoon, _, _) -> 3
                        | Some (Calendar.LastQuarter, _, _) -> 4
                      in
                      let r =
                        { s_year = dmy1.year - (dmy1.year mod periode);
                          s_sex = get_sex p;
                          s_value = i; }
                      in
                      try
                        let l = Hashtbl.find ht_moon (r.s_year, r.s_value) in
                        Hashtbl.replace ht_moon (r.s_year, r.s_value) (r :: l)
                      with Not_found -> Hashtbl.add ht_moon (r.s_year, r.s_value) [r]
                    end
                  with Failure _ -> ()
                end
              else ()
            end;

            (* nombre moyen d'union *)
            begin
              if p_auth then
                let v = Array.length (get_family p) in
                let r =
                  { s_year = dmy1.year - (dmy1.year mod periode);
                    s_sex = get_sex p;
                    s_value = v; }
                in
                try
                  let l = Hashtbl.find ht_moy_marr r.s_year in
                  Hashtbl.replace ht_moy_marr r.s_year (r :: l)
                with Not_found -> Hashtbl.add ht_moy_marr r.s_year [r]
              else ()
            end;

            (* durée moyenne union *)
            begin
              let rec loop l =
                match l with
                | [] -> ()
                | ifam :: l ->
                    begin
                      let fam = foi base ifam in
                      let m_auth =
                        p_auth &&
                          Util.authorized_age conf base
                          (poi base (Gutil.spouse (get_iper p) fam))
                      in
                      if not m_auth then ()
                      else
                        match Adef.od_of_cdate (get_marriage fam) with
                        | Some (Dgreg (({prec = Sure}) as dmy, _)) ->
                            begin
                              match get_divorce fam with
                              | Divorced co ->
                                  begin
                                    match Adef.od_of_cdate co with
                                    | Some (Dgreg (({prec = Sure}) as dmy2, _)) ->
                                        if dmy2.year < dmy.year then ()
                                        else
                                          begin
                                            let a = Date.time_elapsed dmy dmy2 in
                                            let v = a.year in
                                            let r =
                                              { s_year = dmy.year - (dmy.year mod periode);
                                                s_sex = Neuter;
                                                s_value = v; }
                                            in
                                            try
                                              let l = Hashtbl.find ht_marr_time r.s_year in
                                              Hashtbl.replace ht_marr_time r.s_year (r :: l)
                                            with Not_found -> Hashtbl.add ht_marr_time r.s_year [r]
                                          end;
                                        loop l
                                    | _ -> loop l
                                  end
                              | _ ->
                                  begin
                                    match l with
                                    | [] ->
                                        begin
                                          let father = poi base (get_father fam) in
                                          let mother = poi base (get_mother fam) in
                                          match
                                            (Date.get_birth_death_date father,
                                             Date.get_birth_death_date mother)
                                          with
                                          | ((_, Some (Dgreg (dmyf, _)), _) ,
                                             (_, Some (Dgreg (dmym, _)), _)) ->
                                              if dmyf.year < dmy.year || dmym.year < dmy.year then ()
                                              else
                                                begin
                                                  if dmyf.year < dmym.year then
                                                    let a = Date.time_elapsed dmy dmyf in
                                                    begin
                                                      let v = a.year in
                                                      let r =
                                                        { s_year = dmy.year - (dmy.year mod periode);
                                                          s_sex = Neuter;
                                                          s_value = v; }
                                                      in
                                                      try
                                                        let l = Hashtbl.find ht_marr_time r.s_year in
                                                        Hashtbl.replace ht_marr_time r.s_year (r :: l)
                                                      with Not_found -> Hashtbl.add ht_marr_time r.s_year [r]
                                                    end
                                                  else
                                                    let a = Date.time_elapsed dmy dmym in
                                                    begin
                                                      let v = a.year in
                                                      let r =
                                                        { s_year = dmy.year - (dmy.year mod periode);
                                                          s_sex = Neuter;
                                                          s_value = v; }
                                                      in
                                                      try
                                                        let l = Hashtbl.find ht_marr_time r.s_year in
                                                        Hashtbl.replace ht_marr_time r.s_year (r :: l)
                                                      with Not_found -> Hashtbl.add ht_marr_time r.s_year [r]
                                                    end
                                                end
                                          | ((_, Some (Dgreg (dmyf, _)), _) , _) ->
                                                if dmyf.year < dmy.year then ()
                                                else
                                                  begin
                                                    let a = Date.time_elapsed dmy dmyf in
                                                    let v = a.year in
                                                    let r =
                                                      { s_year = dmy.year - (dmy.year mod periode);
                                                        s_sex = Neuter;
                                                        s_value = v; }
                                                    in
                                                    try
                                                      let l = Hashtbl.find ht_marr_time r.s_year in
                                                      Hashtbl.replace ht_marr_time r.s_year (r :: l)
                                                    with Not_found -> Hashtbl.add ht_marr_time r.s_year [r]
                                                  end
                                          | (_, (_, Some (Dgreg (dmym, _)), _)) ->
                                                if dmym.year > dmy.year then ()
                                                else
                                                  begin
                                                    let a = Date.time_elapsed dmy dmym in
                                                    let v = a.year in
                                                    let r =
                                                      { s_year = dmy.year - (dmy.year mod periode);
                                                        s_sex = Neuter;
                                                        s_value = v; }
                                                    in
                                                    try
                                                      let l = Hashtbl.find ht_marr_time r.s_year in
                                                      Hashtbl.replace ht_marr_time r.s_year (r :: l)
                                                    with Not_found -> Hashtbl.add ht_marr_time r.s_year [r]
                                                  end
                                          | _ -> ()
                                        end;
                                        loop l
                                    | ifam2 :: l2 ->
                                        begin
                                          let fam2 = foi base ifam2 in
                                          match Adef.od_of_cdate (get_marriage fam2) with
                                          | Some (Dgreg (({prec = Sure}) as dmy2, _)) ->
                                              if dmy.year > dmy2.year then ()
                                              else
                                                begin
                                                  let a = Date.time_elapsed dmy dmy2 in
                                                  let v = a.year in
                                                  let r =
                                                    { s_year = dmy.year - (dmy.year mod periode);
                                                      s_sex = Neuter;
                                                      s_value = v; }
                                                  in
                                                  try
                                                    let l = Hashtbl.find ht_marr_time r.s_year in
                                                    Hashtbl.replace ht_marr_time r.s_year (r :: l)
                                                  with Not_found -> Hashtbl.add ht_marr_time r.s_year [r]
                                                end;
                                              loop l
                                          | _ -> loop l2
                                        end
                                  end
                            end
                        | _ -> loop l
                    end
              in
              loop faml
            end;

            (* profession les plus frequente *)
            begin
              if p_auth then
                begin
                  List.iter
                    (fun s ->
                      let k = Name.lower s in
                      if k = "" then ()
                      else
                        try
                          let (s, n) = Hashtbl.find ht_occupation k in
                          Hashtbl.replace ht_occupation k (s, (n + 1))
                        with Not_found -> Hashtbl.add ht_occupation k (s, 1))
                    (nsplit (sou base (get_occupation p)) ',');
                  List.iter
                    (fun e ->
                      let s = sou base e.epers_note in
                      let k = Name.lower s in
                      if k = "" then ()
                      else
                        try
                          let (s, n) = Hashtbl.find ht_occupation k in
                          Hashtbl.replace ht_occupation k (s, (n + 1))
                        with Not_found -> Hashtbl.add ht_occupation k (s, 1))
                    (List.filter (fun e -> e.epers_name = Epers_Occupation) (get_pevents p));
                end
              else ()
            end;

            (* jeune père et jeune mère *)
            begin
              let ht = if get_sex p = Male then ht_young_dad else ht_young_mom in
              let rec loop l =
                match l with
                | [] -> ()
                | ifam :: l ->
                    begin
                      let fam = foi base ifam in
                      let rec loopc lc =
                        match lc with
                        | [] -> loop l
                        | ic :: lc ->
                            begin
                              let c = poi base ic in
                              let c_auth = Util.authorized_age conf base c in
                              if c_auth then
                                match Date.get_birth_death_date c with
                                | (Some (Dgreg (({prec = Sure | About | Maybe} as dmy2), _)), _, _) ->
                                    begin
                                      let a = Date.time_elapsed dmy1 dmy2 in
                                      try
                                        let n = Hashtbl.find ht ip in
                                        if n > a.year then Hashtbl.replace ht ip a.year
                                        else ()
                                      with Not_found -> Hashtbl.add ht ip a.year
                                    end
                                | _ -> loopc lc
                              else ()
                            end
                      in
                      loopc (Array.to_list (get_children fam))
                    end
              in
              loop faml
            end;
        | _ -> ()
      end;
  end
    (Gwdb.persons base) ;

  Gwdb.Collection.iter begin fun fam ->
    let ifam = get_ifam fam in
        let m_auth =
          let father = poi base (get_father fam) in
          let mother = poi base (get_mother fam) in
          Util.authorized_age conf base father &&
            Util.authorized_age conf base mother
        in
        let childrenl = Array.to_list (get_children fam) in

        match Adef.od_of_cdate (get_marriage fam) with
        | Some (Dgreg (({prec = Sure}) as dmy, _) as d) ->
            begin
              let p = poi base (get_father fam) in
              let sp = poi base (get_mother fam) in

              (* age au mariage *)
              begin
                if m_auth then
                  match
                    (Date.get_birth_death_date p, Date.get_birth_death_date sp)
                  with
                  | ((Some (Dgreg (({prec = Sure} as dmy1), _)), _, _),
                     (Some (Dgreg (({prec = Sure} as dmy2), _)), _, _)) ->
                      begin
                        if dmy1.day = 0 then ()
                        else
                        let a1 = Date.time_elapsed dmy1 dmy in
                        let r1 =
                          { s_year = dmy1.year - (dmy1.year mod periode);
                            s_sex = get_sex p;
                            s_value = a1.year; }
                        in
                        try
                          let l = Hashtbl.find ht_marr_age r1.s_year in
                          Hashtbl.replace ht_marr_age r1.s_year (r1 :: l)
                        with Not_found -> Hashtbl.add ht_marr_age r1.s_year [r1]
                      end;
                      begin
                        if dmy2.day = 0 then ()
                        else
                        let a2 = Date.time_elapsed dmy2 dmy in
                        let r2 =
                          { s_year = dmy2.year - (dmy2.year mod periode);
                            s_sex = get_sex sp;
                            s_value = a2.year; }
                        in
                        try
                          let l = Hashtbl.find ht_marr_age r2.s_year in
                          Hashtbl.replace ht_marr_age r2.s_year (r2 :: l)
                        with Not_found -> Hashtbl.add ht_marr_age r2.s_year [r2]
                      end;
                  | _ -> ()
                else ()
              end;

              (* jour de mariage *)
              begin
                if m_auth && dmy.day > 0 then
                  let wd = get_wday conf d in
                  if wd <> -1 then
                    let wd = (wd + 6) mod 7 in
                    let r1 =
                      { s_year = dmy.year - (dmy.year mod periode);
                        s_sex = get_sex p;
                        s_value = wd; }
                    in
                    let r2 =
                      { s_year = dmy.year - (dmy.year mod periode);
                        s_sex = get_sex sp;
                        s_value = wd; }
                    in
                    begin
                      try
                        let l = Hashtbl.find ht_day_marr (r1.s_year, r1.s_value) in
                        Hashtbl.replace ht_day_marr (r1.s_year, r1.s_value) (r1 :: l)
                      with Not_found -> Hashtbl.add ht_day_marr (r1.s_year, r1.s_value) [r1]
                    end;
                    begin
                      try
                        let l = Hashtbl.find ht_day_marr (r2.s_year, r2.s_value) in
                        Hashtbl.replace ht_day_marr (r2.s_year, r2.s_value) (r2 :: l)
                      with Not_found -> Hashtbl.add ht_day_marr (r2.s_year, r2.s_value) [r2]
                    end;
                  else ()
                else ()
              end;

              (* mois de mariage *)
              begin
                if m_auth && dmy.month > 0 then
                  let md = get_month d in
                  if md <> -1 then
                    let md = md - 1 in
                    let r1 =
                      { s_year = dmy.year - (dmy.year mod periode);
                        s_sex = get_sex p;
                        s_value = md; }
                    in
                    let r2 =
                      { s_year = dmy.year - (dmy.year mod periode);
                        s_sex = get_sex sp;
                        s_value = md; }
                    in
                    begin
                      try
                        let l = Hashtbl.find ht_month_marr (r1.s_year, r1.s_value) in
                        Hashtbl.replace ht_month_marr (r1.s_year, r1.s_value) (r1 :: l)
                      with Not_found -> Hashtbl.add ht_month_marr (r1.s_year, r1.s_value) [r1]
                    end;
                    begin
                      try
                        let l = Hashtbl.find ht_month_marr (r2.s_year, r2.s_value) in
                        Hashtbl.replace ht_month_marr (r2.s_year, r2.s_value) (r2 :: l)
                      with Not_found -> Hashtbl.add ht_month_marr (r2.s_year, r2.s_value) [r2]
                    end;
                  else ()
                else ()
              end;

              (* différence age couple *)
              begin
                if m_auth then
                  match
                    (Date.get_birth_death_date p, Date.get_birth_death_date sp)
                  with
                  | ((Some (Dgreg (({prec = Sure} as dmy1), _)), _, _),
                     (Some (Dgreg (({prec = Sure} as dmy2), _)), _, _)) ->
                      if dmy1.day > 0 && dmy2.day > 0 then
                        let a =
                          if dmy1.year < dmy2.year then Date.time_elapsed dmy1 dmy2
                          else Date.time_elapsed dmy2 dmy2
                        in
                        let v = a.month + 12 * a.year in
                        begin
                          let r1 =
                            { s_year = dmy.year - (dmy.year mod periode);
                              s_sex = get_sex p;
                              s_value = v; }
                          in
                          try
                            let l = Hashtbl.find ht_marr_diff_age_cpl r1.s_year in
                            Hashtbl.replace ht_marr_diff_age_cpl r1.s_year (r1 :: l)
                          with Not_found -> Hashtbl.add ht_marr_diff_age_cpl r1.s_year [r1]
                        end;
                        begin
                          let r2 =
                            { s_year = dmy.year - (dmy.year mod periode);
                              s_sex = get_sex sp;
                              s_value = v; }
                          in
                          try
                            let l = Hashtbl.find ht_marr_diff_age_cpl r2.s_year in
                            Hashtbl.replace ht_marr_diff_age_cpl r2.s_year (r2 :: l)
                          with Not_found -> Hashtbl.add ht_marr_diff_age_cpl r2.s_year [r2]
                        end;
                      else ()
                  | _ -> ()
                else ()
              end;

              (* taux de fecondite *)
              begin
                let v = Array.length (get_children fam) in
                begin
                  let r1 =
                    { s_year = dmy.year - (dmy.year mod periode);
                      s_sex = Neuter;
                      s_value = v; }
                  in
                  try
                    let l = Hashtbl.find ht_fecondite r1.s_year in
                    Hashtbl.replace ht_fecondite r1.s_year (r1 :: l)
                  with Not_found -> Hashtbl.add ht_fecondite r1.s_year [r1]
                end;
                begin
                  let r2 =
                    { s_year = dmy.year - (dmy.year mod periode);
                      s_sex = Neuter;
                      s_value = v; }
                  in
                  try
                    let l = Hashtbl.find ht_fecondite r2.s_year in
                    Hashtbl.replace ht_fecondite r2.s_year (r2 :: l)
                  with Not_found -> Hashtbl.add ht_fecondite r2.s_year [r2]
                end;
              end;

              (* interval entre naissance *)
              begin
                (* On prend le parti que les enfants sont triés ! *)
                let rec loop l =
                  match l with
                  | [] | [_] -> ()
                  | ic1 :: ic2 :: l ->
                      let c1 = poi base ic1 in
                      let c2 = poi base ic2 in
                      let c_auth =
                        Util.authorized_age conf base c1 &&
                          Util.authorized_age conf base c2
                      in
                      begin
                        if c_auth then
                          match
                            (Date.get_birth_death_date c1,
                             Date.get_birth_death_date c2)
                          with
                          | ((Some (Dgreg (({prec = Sure} as dmy_c1), _)), _, _) ,
                             (Some (Dgreg (({prec = Sure} as dmy_c2), _)), _, _)) ->
                              let a = Date.time_elapsed dmy_c1 dmy_c2 in
                              begin
                                let v = a.month + 12 * a.year in
                                let r =
                                  { s_year = dmy_c1.year - (dmy_c1.year mod periode);
                                    s_sex = Neuter;
                                    s_value = v; }
                                in
                                try
                                  let l = Hashtbl.find ht_diff_age_child r.s_year in
                                  Hashtbl.replace ht_diff_age_child r.s_year (r :: l)
                                with Not_found -> Hashtbl.add ht_diff_age_child r.s_year [r]
                              end
                          | _ -> ()
                        else ()
                      end;
                      loop (ic2 :: l)
                in
                loop childrenl
              end;

              (* age moyen à la naissance *)
              begin
                List.iter
                  (fun ic ->
                    let c = poi base ic in
                    let c_auth = Util.authorized_age conf base c in
                    if c_auth then
                      match Date.get_birth_death_date c with
                      | (Some (Dgreg (({prec = Sure} as dmy_c), _)), _, _) ->
                          begin
                            match Date.get_birth_death_date p with
                            | (Some (Dgreg (({prec = Sure} as dmy_p), _)), _, _) ->
                                let a = Date.time_elapsed dmy_p dmy_c in
                                let v = a.year in
                                begin
                                  let r1 =
                                    { s_year = dmy_c.year - (dmy_c.year mod periode);
                                      s_sex = get_sex p;
                                      s_value = v; }
                                  in
                                  try
                                    let l = Hashtbl.find ht_moy_age_birth r1.s_year in
                                    Hashtbl.replace ht_moy_age_birth r1.s_year (r1 :: l)
                                  with Not_found -> Hashtbl.add ht_moy_age_birth r1.s_year [r1]
                                end;
                            | _ -> ()
                          end;
                          begin
                            match Date.get_birth_death_date sp with
                            | (Some (Dgreg (({prec = Sure} as dmy_p), _)), _, _) ->
                                let a = Date.time_elapsed dmy_p dmy_c in
                                let v = a.year in
                                begin
                                  let r2 =
                                    { s_year = dmy_c.year - (dmy_c.year mod periode);
                                      s_sex = get_sex p;
                                      s_value = v; }
                                  in
                                  try
                                    let l = Hashtbl.find ht_moy_age_birth r2.s_year in
                                    Hashtbl.replace ht_moy_age_birth r2.s_year (r2 :: l)
                                  with Not_found -> Hashtbl.add ht_moy_age_birth r2.s_year [r2]
                                end;
                            | _ -> ()
                          end;
                      | _ -> ()
                    else ())
                  childrenl
              end;

              (* age moyen à la première naissance *)
              begin
                match childrenl with
                | [] -> ()
                | ic :: _ ->
                    let c = poi base ic in
                    let c_auth = Util.authorized_age conf base c in
                    if c_auth then
                      match Date.get_birth_death_date c with
                      | (Some (Dgreg (({prec = Sure} as dmy_c), _)), _, _) ->
                          begin
                            match Date.get_birth_death_date p with
                            | (Some (Dgreg (({prec = Sure} as dmy_p), _)), _, _) ->
                                let a = Date.time_elapsed dmy_p dmy_c in
                                let v = a.year in
                                begin
                                  let r1 =
                                    { s_year = dmy_c.year - (dmy_c.year mod periode);
                                      s_sex = get_sex p;
                                      s_value = v; }
                                  in
                                  try
                                    let l = Hashtbl.find ht_moy_age_first_birth r1.s_year in
                                    Hashtbl.replace ht_moy_age_first_birth r1.s_year (r1 :: l)
                                  with Not_found -> Hashtbl.add ht_moy_age_first_birth r1.s_year [r1]
                                end;
                            | _ -> ()
                          end;
                          begin
                            match Date.get_birth_death_date sp with
                            | (Some (Dgreg (({prec = Sure} as dmy_p), _)), _, _) ->
                                let a = Date.time_elapsed dmy_p dmy_c in
                                let v = a.year in
                                begin
                                  let r2 =
                                    { s_year = dmy_c.year - (dmy_c.year mod periode);
                                      s_sex = get_sex sp;
                                      s_value = v; }
                                  in
                                  try
                                    let l = Hashtbl.find ht_moy_age_first_birth r2.s_year in
                                    Hashtbl.replace ht_moy_age_first_birth r2.s_year (r2 :: l)
                                  with Not_found -> Hashtbl.add ht_moy_age_first_birth r2.s_year [r2]
                                end;
                            | _ -> ()
                          end;
                      | _ -> ()
                    else ()
              end;

              (* age moyen à la dernière naissance *)
              begin
                match List.rev childrenl with
                | [] -> ()
                | ic :: _ ->
                    let c = poi base ic in
                    let c_auth = Util.authorized_age conf base c in
                    if c_auth then
                      match Date.get_birth_death_date c with
                      | (Some (Dgreg (({prec = Sure} as dmy_c), _)), _, _) ->
                          begin
                            match Date.get_birth_death_date p with
                            | (Some (Dgreg (({prec = Sure} as dmy_p), _)), _, _) ->
                                let a = Date.time_elapsed dmy_p dmy_c in
                                let v = a.year in
                                begin
                                  let r1 =
                                    { s_year = dmy_c.year - (dmy_c.year mod periode);
                                      s_sex = get_sex p;
                                      s_value = v; }
                                  in
                                  try
                                    let l = Hashtbl.find ht_moy_age_last_birth r1.s_year in
                                    Hashtbl.replace ht_moy_age_last_birth r1.s_year (r1 :: l)
                                  with Not_found -> Hashtbl.add ht_moy_age_last_birth r1.s_year [r1]
                                end;
                            | _ -> ()
                          end;
                          begin
                            match Date.get_birth_death_date sp with
                            | (Some (Dgreg (({prec = Sure} as dmy_p), _)), _, _) ->
                                let a = Date.time_elapsed dmy_p dmy_c in
                                let v = a.year in
                                begin
                                  let r2 =
                                    { s_year = dmy_c.year - (dmy_c.year mod periode);
                                      s_sex = get_sex sp;
                                      s_value = v; }
                                  in
                                  try
                                    let l = Hashtbl.find ht_moy_age_last_birth r2.s_year in
                                    Hashtbl.replace ht_moy_age_last_birth r2.s_year (r2 :: l)
                                  with Not_found -> Hashtbl.add ht_moy_age_last_birth r2.s_year [r2]
                                end;
                            | _ -> ()
                          end;
                      | _ -> ()
                    else ()
              end;

              (* différence age premier et dernier enfant *)
              begin
                let children = childrenl in
                match (children, List.rev children) with
                | (ic1 :: _, ic2 :: _) ->
                    begin
                      if ic1 = ic2 then ()
                      else
                        let c1 = poi base ic1 in
                        let c2 = poi base ic2 in
                        let c_auth =
                          Util.authorized_age conf base c1 &&
                            Util.authorized_age conf base c2
                        in
                        if c_auth then
                          match
                            (Date.get_birth_death_date c1, Date.get_birth_death_date c2)
                          with
                          | ((Some (Dgreg (({prec = Sure} as dmy1), _)), _, _),
                             (Some (Dgreg (({prec = Sure} as dmy2), _)), _, _)) ->
                                let a = Date.time_elapsed dmy1 dmy2 in
                                let v = a.month + 12 * a.year in
                                begin
                                  let r =
                                    { s_year = dmy2.year - (dmy2.year mod periode);
                                      s_sex = Neuter;
                                      s_value = v; }
                                  in
                                  try
                                    let l = Hashtbl.find ht_diff_age_extr_child r.s_year in
                                    Hashtbl.replace ht_diff_age_extr_child r.s_year (r :: l)
                                  with Not_found -> Hashtbl.add ht_diff_age_extr_child r.s_year [r]
                                end;
                          | _ -> ()
                        else ()
                    end
                | _ -> ()
              end;

              (* plus grand nombre d'enfants *)
              begin
                Hashtbl.add ht_max_child ifam (Array.length (get_children fam))
              end;

            end
        | _ -> ()
      end
        (Gwdb.families base);

  (* Nom le plus long *)
  let _longuest_name =
    let () = load_strings_array base in
    let (list, _) = Alln.select_names conf base true "" true in
    let list =
      List.sort
        (fun (k1, _, _) (k2, _, _) ->
          let len1 = String.length k1 in
          let len2 = String.length k2 in
          if len1 > len2 then -1
          else 1)
        list
    in
    List.map (fun (s, _, _) -> s) (Util.reduce_list 10 list)
  in

  (* Nom les plus fréquents *)
  let () =
    let () = load_strings_array base in
    let (list, _) = Alln.select_names conf base true "" true in
    let list =
      List.sort
        (fun (k1, _, _) (k2, _, _) ->
          let len1 = String.length k1 in
          let len2 = String.length k2 in
          if len1 > len2 then -1
          else 1)
        list
    in
    List.iter
      (fun (s, _, len) ->
        let k = Name.lower s in
        if k = "?" || k = "" then ()
        else
          try
            let (s, n) = Hashtbl.find ht_surname k in
            Hashtbl.replace ht_surname k (s, (n + len))
          with Not_found -> Hashtbl.add ht_surname k (s, len))
      list;
    (*
    let res = ref [] in
    Hashtbl.iter (fun k (s, n) -> res := (s, n) :: !res) ht;
    let l =
      List.sort
        (fun (_, n1) (_, n2) -> if n1 > n2 then -1 else 1)
        !res
    in
    List.map (fun (s, _) -> s) (Util.reduce_list 10 l)
    *)
  in

  (* Prénom les plus fréquents *)
  let () =
    let () = load_strings_array base in
    let (list, _) = Alln.select_names conf base false "" true in
    let list =
      List.sort
        (fun (k1, _, _) (k2, _, _) ->
          let len1 = String.length k1 in
          let len2 = String.length k2 in
          if len1 > len2 then -1
          else 1)
        list
    in
    List.iter
      (fun (s, _, len) ->
         List.iter
           (fun s ->
             let s = Name.strip_c s '"' in
             let k = Name.lower s in
             if k = "?" || k = "" then ()
             else
               try
                 let (s, n) = Hashtbl.find ht_first_name k in
                 Hashtbl.replace ht_first_name k (s, (n + len))
               with Not_found -> Hashtbl.add ht_first_name k (s, len))
           (nsplit s ' '))
      list;
    (*
    let res = ref [] in
    Hashtbl.iter (fun s n -> res := (s, n) :: !res) ht;
    let l =
      List.sort
        (fun (_, n1) (_, n2) -> if n1 > n2 then -1 else 1)
        !res
    in
    List.map (fun (s, _) -> s) (Util.reduce_list 10 l)
    *)
  in

  (* plus grand nombre d'enfants suite et fin *)
  let _max_child =
    let res = ref [] in
    Hashtbl.iter (fun ifam n -> res := (ifam, n) :: !res) ht_max_child;
    let l = List.sort (fun (_, n1) (_, n2) -> if n1 > n2 then -1 else 1) !res in
    Util.reduce_list 10 l
  in

  (* plus jeune pere *)
  let _youngest_dad =
    let res = ref [] in
    Hashtbl.iter (fun ip n -> res := (ip, n) :: !res) ht_young_dad;
    let l = List.sort (fun (_, n1) (_, n2) -> compare n1 n2) !res in
    Util.reduce_list 10 l
  in

  (* plus jeune mere *)
  let _youngest_mom =
    let res = ref [] in
    Hashtbl.iter (fun ip n -> res := (ip, n) :: !res) ht_young_mom;
    let l = List.sort (fun (_, n1) (_, n2) -> compare n1 n2) !res in
    Util.reduce_list 10 l
  in

  (*
  let longuest = format_res ht_longuest in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      print_endline
        (string_of_int k ^ "&" ^ string_of_int nb_m ^ "&" ^
           (if nb_m <> 0 then string_of_int (am / nb_m) else "0") ^
           "&" ^ string_of_int nb_f ^ "&" ^
           (if nb_f <> 0 then string_of_int (af / nb_f) else "0")))
    longuest;
  *)
  (*
  let ad = [| "dimanche"; "lundi"; "mardi"; "mercredi"; "jeudi"; "vendredi"; "samedi" |] in
  let day_birth = format_res2 ht_day_birth in
  List.iter
    (fun (k, (wd, nb_m, nb_f))  ->
      print_endline
        (string_of_int k ^ "&" ^ ad.(wd) ^ "&" ^ string_of_int nb_m ^ "&" ^
           string_of_int nb_f))
    day_birth;
  *)
  (*
  let ad =
    [| "janvier"; "février"; "mars"; "avril"; "mai"; "juin";
       "juillet"; "août"; "septembre"; "octobre"; "novembre"; "décembre" |]
  in
  let month_birth = format_res2 ht_month_birth in
  List.iter
    (fun (k, (md, nb_m, nb_f))  ->
      print_endline
        (string_of_int k ^ "&" ^ ad.(md - 1) ^ "&" ^ string_of_int nb_m ^ "&" ^
           string_of_int nb_f))
    month_birth;
  *)
  (*
  let ad =
    [| "janvier"; "février"; "mars"; "avril"; "mai"; "juin";
       "juillet"; "août"; "septembre"; "octobre"; "novembre"; "décembre" |]
  in
  let month_death = format_res2 ht_month_death in
  List.iter
    (fun (k, (md, nb_m, nb_f))  ->
      print_endline
        (string_of_int k ^ "&" ^ ad.(md - 1) ^ "&" ^ string_of_int nb_m ^ "&" ^
           string_of_int nb_f))
    month_death;
  *)
  (*
  let ad = [| "dimanche"; "lundi"; "mardi"; "mercredi"; "jeudi"; "vendredi"; "samedi" |] in
  let day_marr = format_res2 ht_day_marr in
  List.iter
    (fun (k, (wd, nb_m, nb_f))  ->
      print_endline
        (string_of_int k ^ "&" ^ ad.(wd) ^ "&" ^ string_of_int nb_m ^ "&" ^
           string_of_int nb_f))
    day_marr;
  let ad =
    [| "janvier"; "février"; "mars"; "avril"; "mai"; "juin";
       "juillet"; "août"; "septembre"; "octobre"; "novembre"; "décembre" |]
  in
  let _ = print_endline "\n\n\n" in
  let month_marr = format_res2 ht_month_marr in
  List.iter
    (fun (k, (md, nb_m, nb_f))  ->
      print_endline
        (string_of_int k ^ "&" ^ ad.(md - 1) ^ "&" ^ string_of_int nb_m ^ "&" ^
           string_of_int nb_f))
    month_marr;
  *)
  (*
  let male_female = format_res3 ht_male_female in
  List.iter
    (fun (k, (nb_m, nb_f)) ->
      Printf.fprintf stdout "%d;%d;%d\n" k
        (int_of_float ((float_of_int nb_m /. (float_of_int nb_m +. float_of_int nb_f)) *. 100.))
        (int_of_float ((float_of_int nb_f /. (float_of_int nb_m +. float_of_int nb_f)) *. 100.)))
    male_female;
  *)
  (*
  let moon_arr =
    [| "Entre deux phases"; "Nouvelle lune"; "Premier quartier"; "Pleine lune"; "Dernier quartier"; |]
  in
  let moon = format_res2 ht_moon in
  List.iter
    (fun (k, (moon, nb_m, nb_f))  ->
      print_endline
        (string_of_int k ^ "&" ^ moon_arr.(moon) ^ "&" ^ string_of_int nb_m ^ "&" ^
           string_of_int nb_f))
    moon;
  *)
  (*
  let age_marr = format_res ht_marr_age in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      print_endline
        (string_of_int k ^ "&" ^ string_of_int nb_m ^ "&" ^
           (if nb_m <> 0 then string_of_int (am / nb_m) else "0") ^
           "&" ^ string_of_int nb_f ^ "&" ^
           (if nb_f <> 0 then string_of_int (af / nb_f) else "0")))
    age_marr;
  *)
  (*
  let diff_age_marr = format_res ht_marr_diff_age_cpl in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      print_endline
        (string_of_int k ^ "&" ^ string_of_int nb_m ^ "&" ^
           (if nb_m <> 0 then string_of_int (am / nb_m) else "0") ^
           "&" ^ string_of_int nb_f ^ "&" ^
           (if nb_f <> 0 then string_of_int (af / nb_f) else "0")))
    diff_age_marr;
  *)
  (*
  let moy_marr = format_res ht_moy_marr in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      Printf.fprintf stdout "%d;%d;%.2f;%d;%.2f\n" k
        nb_m (if nb_m <> 0 then (float_of_int am /. float_of_int nb_m) else 0.)
        nb_f (if nb_f <> 0 then (float_of_int af /. float_of_int nb_f) else 0.))
    moy_marr;
  *)
  (*
  let time_marr = format_res ht_marr_time in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      Printf.fprintf stdout "%d;%d;%.2f;%d;%.2f\n" k
        nb_m (if nb_m <> 0 then (float_of_int am /. float_of_int nb_m) else 0.)
        nb_f (if nb_f <> 0 then (float_of_int af /. float_of_int nb_f) else 0.))
    time_marr;
  *)
  (*
  let nb_child_marr = format_res ht_fecondite in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      Printf.fprintf stdout "%d;%d;%.2f;%d;%.2f\n" k
        nb_m (if nb_m <> 0 then (float_of_int am /. float_of_int nb_m) else 0.)
        nb_f (if nb_f <> 0 then (float_of_int af /. float_of_int nb_f) else 0.))
    nb_child_marr;
  *)
  (*
  let diff_age_child = format_res ht_diff_age_child in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      Printf.fprintf stdout "%d;%d;%.2f;%d;%.2f\n" k
        nb_m (if nb_m <> 0 then (float_of_int am /. float_of_int nb_m) else 0.)
        nb_f (if nb_f <> 0 then (float_of_int af /. float_of_int nb_f) else 0.))
    diff_age_child;
  *)
  (*
  let moy_age_birth = format_res ht_moy_age_birth in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      Printf.fprintf stdout "%d;%d;%.2f;%d;%.2f\n" k
        nb_m (if nb_m <> 0 then (float_of_int am /. float_of_int nb_m) else 0.)
        nb_f (if nb_f <> 0 then (float_of_int af /. float_of_int nb_f) else 0.))
    moy_age_birth;
  *)
  (*
  let moy_age_first_birth = format_res ht_moy_age_first_birth in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      Printf.fprintf stdout "%d;%d;%.2f;%d;%.2f\n" k
        nb_m (if nb_m <> 0 then (float_of_int am /. float_of_int nb_m) else 0.)
        nb_f (if nb_f <> 0 then (float_of_int af /. float_of_int nb_f) else 0.))
    moy_age_first_birth;
  *)
  (*
  let moy_age_last_birth = format_res ht_moy_age_last_birth in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      Printf.fprintf stdout "%d;%d;%.2f;%d;%.2f\n" k
        nb_m (if nb_m <> 0 then (float_of_int am /. float_of_int nb_m) else 0.)
        nb_f (if nb_f <> 0 then (float_of_int af /. float_of_int nb_f) else 0.))
    moy_age_last_birth;
  *)
  (*
  let diff_age_extr = format_res ht_diff_age_extr_child in
  List.iter
    (fun (k, ((nb_m, am), (nb_f, af))) ->
      Printf.fprintf stdout "%d;%d;%.2f;%d;%.2f\n" k
        nb_m (if nb_m <> 0 then (float_of_int am /. float_of_int nb_m) else 0.)
        nb_f (if nb_f <> 0 then (float_of_int af /. float_of_int nb_f) else 0.))
    diff_age_extr;
  *)
  (*
  let zodiac =
    [| "Bélier"; "Taureau"; "Gémeaux"; "Cancer"; "Lion";
       "Vierge"; "Balance"; "Scorpion"; "Sagittaire";
       "Capricorne"; "Verseau"; "Poissons"; |]
  in
  let astro = format_res2 ht_astro in
  List.iter
    (fun (k, (wd, nb_m, nb_f))  ->
      print_endline
        (string_of_int k ^ "&" ^ zodiac.(wd) ^ "&" ^ string_of_int nb_m ^ "&" ^
           string_of_int nb_f))
    astro;
  *)
  (*List.iter print_endline longuest_name*)
  (*
  let popu_occu =
    let res = ref [] in
    Hashtbl.iter
      (fun s n -> res := (s, n) :: !res)
      ht_occupation;
    let l =
      List.sort (fun (_, n1) (_, n2) -> if n1 > n2 then -1 else 1) !res
    in
    List.map (fun (s, _) -> s) (Util.reduce_list 10 l)
  in
  List.iter print_endline popu_occu
  *)
  (*List.iter print_endline most_firstnames*)
  (*
  List.iter
    (fun (ifam, n) ->
      let fam = foi base ifam in
      let father = poi base (get_father fam) in
      let mother = poi base (get_mother fam) in
      print_endline (Gutil.designation base father ^ " & " ^
                       Gutil.designation base mother ^ " => " ^ string_of_int n))
    max_child
  *)
  (*
  List.iter
    (fun (ip, n) ->
      let p = poi base ip in
      print_endline (Gutil.designation base p ^ " => " ^ string_of_int n))
    youngest_dad
  *)
  (*
  List.iter
    (fun (ip, n) ->
      let p = poi base ip in
      print_endline (Gutil.designation base p ^ " => " ^ string_of_int n))
    youngest_mom
  *)

  let all_stats = [] in
  let series_m_f = [`serie_male; `serie_female] in
  let series_days =
    [`serie_day_1; `serie_day_2; `serie_day_3; `serie_day_4;
     `serie_day_5; `serie_day_6; `serie_day_7;]
  in
  let series_months =
    [`serie_month_1; `serie_month_2; `serie_month_3; `serie_month_4;
     `serie_month_5; `serie_month_6; `serie_month_7; `serie_month_8;
     `serie_month_9; `serie_month_10; `serie_month_11; `serie_month_12]
  in
  let series_parents_age_first_child =
    [`serie_male_age_first_child; `serie_female_age_first_child]
  in
  let series_parents_age_last_child =
    [`serie_male_age_last_child; `serie_female_age_last_child]
  in
  let series_all = [`serie_all] in

  let stats =
    format_stats_m_f (format_res ht_longuest) `st_ind_longevity series_m_f
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_m_f2 (format_res ht_moy_age_first_birth)
      (format_res ht_moy_age_last_birth) `st_ind_parent_age
      series_parents_age_first_child series_parents_age_last_child
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_dmy (format_res2 ht_month_birth)
      `st_ind_birth_month series_months
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_dmy (format_res2 ht_day_marr) `st_fam_marr_day series_days
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_dmy (format_res2 ht_month_marr)
      `st_fam_marr_month series_months
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_m_f (format_res ht_marr_age)
      `st_fam_first_marr_parent_age series_m_f
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_all (format_res ht_diff_age_extr_child)
      `st_fam_diff_age_btw_children series_all
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_all (format_res ht_diff_age_child)
      `st_fam_int_btw_children series_all
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_all (format_res ht_fecondite)
      `st_fam_avg_nb_children series_all
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_all (format_res ht_marr_time)
      `st_fam_avg_marr_duration series_all
  in
  let all_stats = stats :: all_stats in

  let stats = format_top_stats ht_occupation `st_ind_occupation in
  let all_stats = stats :: all_stats in

  let stats = format_top_stats ht_surname `st_ind_lastname in
  let all_stats = stats :: all_stats in

  let stats = format_top_stats ht_first_name `st_ind_firstname in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_all (format_res ht_marr_diff_age_cpl)
      `st_fam_diff_age_btw_cpl series_all
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_astro (format_res2 ht_astro)
      `st_ind_astro
  in
  let all_stats = stats :: all_stats in

  let stats =
    format_stats_moon (format_res2 ht_moon)
      `st_ind_moon
  in
  let all_stats = stats :: all_stats in

  let stats =
    Mstats.Stats.({
      stats = all_stats;
    })
  in

  let data = Mext_stats.gen_stats stats in
  print_result conf data


let print_stats conf base =
  let params = get_params conf Mext_stats.parse_stats_params in
  if params.Mstats.Stats_params.i <> None
  then print_ind_stats conf base
  else print_all_stats conf base

#endif
