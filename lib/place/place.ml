open Def
open Place_types

let string_exists string filter =
  let len = String.length string in
  let rec loop i =
    if i = len then false else filter (String.unsafe_get string i) || loop (i + 1)
  in loop 0

let string_forall string filter =
  let len = String.length string in
  let rec loop i =
    if i = len then true else filter (String.unsafe_get string i) && loop (i + 1)
  in loop 0

let is_num = function '0'..'9' -> true | _ -> false
let is_alphanum = function '0'..'9'|'A'..'Z'|'a'..'z'|'('|')' -> true | _ -> false

let country = Place_match.country
let region = Place_match.region

let subregion c s =
  let open Place_match in
  match c with
  | France ->
    begin
      if String.length s < 2 then raise Not_found ;
      if (String.unsafe_get s 0 = '2' && match String.unsafe_get s 1 with 'A'|'B'|'0'..'9' -> false | _ -> true)
      || string_forall s is_num
      then
        try subregion_France (String.sub s 0 2)
        with Not_found when String.length s > 2 -> subregion_France (String.sub s 0 3)
      else subregion_France s
    end
  | Belgium ->
    if String.length s = 4 && string_forall s is_num
    then match String.unsafe_get s 0 with
      | '1' -> begin match String.unsafe_get s 1 with
          | '0'..'2' -> BE_Bruxelles_Capitale
          | '3' | '4' -> BE_Brabant_wallon
          | '5'..'9' -> BE_Brabant_flamand
          | _ -> raise Not_found
        end
      | '2' -> BE_Anvers
      | '3' -> begin match String.unsafe_get s 1 with
          | '0'..'4' -> BE_Brabant_flamand
          | '5'..'9' -> BE_Limbourg
          | _ -> raise Not_found
        end
      | '4' -> BE_Liege
      | '5' -> BE_Namur
      | '6' -> begin match String.unsafe_get s 1 with
          | '0'..'5' -> BE_Hainaut
          | '6'..'9' -> BE_Luxembourg
          | _ -> raise Not_found
        end
      | '7' -> BE_Hainaut
      | '8' -> BE_Flandre_Occidentale
      | '9' -> BE_Flandre_Orientale
      | _ -> raise Not_found
    else subregion_Belgium s
  | _ -> subregion c s

let finalize p = p

let empty_place place_raw =
  { Def.place_street = ""
  ; place_city = ""
  ; place_subregion = ""
  ; place_region = ""
  ; place_country = ""
  ; place_lieu_dit = ""
  ; place_raw
  }

(* TODO: start with num + alphanumspace uniquement: street *)
let split_place str =
  let len = String.length str in
  let pick i j =
    if i <= j - 1 then String.trim @@ String.sub str i (j - i + 1) else ""
  in
  let add i j comment acc =
    match pick i j with "" -> if comment = "" then acc else (comment, "") :: acc | x -> (x, comment) :: acc
  in
  let rec loop comment acc i j =
    if i < 0 then add 0 j comment acc
    else match String.unsafe_get str i with
      | ']' when let rec loop k =
                   if k >= j then false
                   else match String.unsafe_get str k with
                     | ' ' -> loop (k + 1)
                     | '-' -> true
                     | _ -> false
          in loop (i + 1) ->
        begin match String.rindex_from_opt str (i - 1) '[' with
          | Some k -> loop "" (add k j comment acc) (k - 1) (k - 1)
          | None -> loop comment acc (i - 1) j
        end
      | ')' | ']' as c ->
        begin match String.rindex_from_opt str (i - 1) (if c = ')' then '(' else '[') with
          | Some i' ->
            loop (pick (i' + 1) (i - 1)) (add (i + 1) j comment acc) (i' - 1) (i' - 1)
          | None -> loop comment acc (i - 1) j
        end
      | ',' | ';' -> loop "" (add (i + 1) j comment acc) (i - 1) (i - 1)
      | _ -> loop comment acc (i - 1) j
  in
  loop "" [] (len - 1) (len - 1)

let guess_place _conf str =
  let list = split_place str in
  let place_country, list =
    match List.rev list with
    | [] -> "", []
    | (main, comment) :: tl ->
      try show_country @@ country main, List.rev tl
      with Not_found ->
        if comment = ""
        then show_country France, list
        else
          try show_country (country comment), List.rev @@ (main, "") :: tl
          with Not_found -> show_country France, list
  in
  let p = { (empty_place str) with Def.place_country } in
  let rec loop i p = function
    | [] ->
      if p.place_street <> ""
      && string_forall p.place_street is_num
      && p.place_city = "" && p.place_region = ""
      then
        try finalize
              { p with place_street = ""
                     ; place_subregion = show_subregion @@ subregion (country p.place_country) p.place_street }
        with Not_found -> finalize p
      else finalize p
    | (main, comment) :: tl ->
      loop (i + 1)
        begin
          let place_city p s =
            let place_lieu_dit, place_city  =
              if String.unsafe_get s 0 = '['
              then match String.index_opt s ']' with
                | Some i ->
                  let rec loop j =
                    if j = String.length s then "", s
                    else match String.unsafe_get s j with
                      | ' ' | '-' -> loop (j + 1)
                      | _ -> String.sub s 1 (i - 1), String.sub s j (String.length s - j)
                  in loop (i + 1)
                | None -> "", s
              else "", s
            in
            { p with place_city ; place_lieu_dit }
          in
          if i = 0 && string_exists main is_num && string_forall main is_alphanum
          then { p with place_street = main ^ if comment = "" then "" else "(" ^ comment ^ ")" }
          else
            let aux s =
              try `Subregion (show_subregion @@ subregion (country p.place_country) s)
              with Not_found ->
              try `Region (show_region @@ region (country p.place_country) s)
              with Not_found -> `None s
            in
            match aux main, aux comment with

            | (`Region a, `Region b) ->
              assert (a = b) ;
              if p.place_region = "" then { p with place_region = a }
              else if key p.place_region <> key a then { p with place_region = "" } (* FIXME *)
              else p

            | (`Subregion b, `Subregion b') ->
              if key b <> key b'
              then print_endline @@ Printf.sprintf "%s: %s <> %s (%s)" __LOC__ b b' str ;
              if p.place_subregion <> "" && key p.place_subregion <> key b
              then print_endline @@ Printf.sprintf "%s: %s <> %s (%s)" __LOC__ p.place_subregion b str ;
              { p with place_subregion = b }

            | (`None s, `Subregion b) | (`Subregion b, `None s) ->
              if s = "" then (if p.place_subregion <> "" && key p.place_subregion <> key b
                              then print_endline @@ Printf.sprintf "%s: %s <> %s (%s)" __LOC__ p.place_subregion b str ;
                              { p with place_subregion = b } )
              else if p.place_city = "" then { (place_city p s) with place_subregion = b }
              else p (* !!! *)

            | (`Region a, `Subregion b) | (`Subregion b, `Region a) ->
              { p with place_region = a ; place_subregion = b }

            | (`Region a, `None s) | (`None s, `Region a) ->
              if p.place_city = "" && s <> "" then { (place_city p s) with place_region = a }
              else if s = "" then { p with place_region = a }
              else p (* !!! *)

            | (`None s1, `None s2) ->
              if string_exists p.place_street is_num && string_forall p.place_street is_alphanum then
                { p with place_street = p.place_street ^ main }
              else if p.place_street = ""
                   && is_num (String.unsafe_get main 0)
                   && match String.index_opt main ' ' with Some i -> String.index_from_opt main (i + 1) ' ' <> None | _ -> false
              then
                { p with place_street = main }
              else if p.place_city = "" then place_city p (s1 ^ s2) (* !!! *)
              else if p.place_street = "" && p.place_region = ""
              then { p with place_street = p.place_city ; place_city = main }
              else p
        end
        tl
  in loop 0 p list

let place_of_string conf place =
  guess_place conf place
