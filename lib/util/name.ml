(* $Id: name.ml,v 5.12 2018-09-27 10:34:14 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* La liste des caractères interdits *)
let forbidden_char = [':'; '@'; '#'; '='; '$']

(* Name.lower *)

(* TODO: replace with Unidecode.nbc
   when version constraint [= 0.2.0] will be removed *)
let nbc c =
  if Char.code c < 0b10000000 then 1
  else if Char.code c < 0b11000000 then invalid_arg "nbc"
  else if Char.code c < 0b11100000 then 2
  else if Char.code c < 0b11110000 then 3
  else if Char.code c < 0b11111000 then 4
  else if Char.code c < 0b11111100 then 5
  else if Char.code c < 0b11111110 then 6
  else invalid_arg "nbc"

let unaccent_utf_8 lower s i =
  let fns =
    if lower then fun n s -> String.lowercase_ascii s, n
    else fun n s -> s, n
  in
  let fnc =
    if lower then fun n c -> String.make 1 @@ Char.lowercase_ascii c, n
    else fun n c -> String.make 1 c, n
  in
  Unidecode.decode
    fns fnc (fun n -> String.sub s i (n - i), n) s i (String.length s)

let next_chars_if_equiv s i t j =
  if i >= String.length s || j >= String.length t then None
  else
    let (s1, i1) = unaccent_utf_8 true s i in
    let (t1, j1) = unaccent_utf_8 true t j in
    if s1 = t1 then Some (i1, j1) else None

let lower s =
  let rec copy special i len =
    if i = String.length s then Buff.get len
    else if Char.code s.[i] < 0x80 then match s.[i] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' as c ->
        let len = if special then Buff.store len ' ' else len in
        let c = Char.lowercase_ascii c in
        copy false (i + 1) (Buff.store len c)
      | _ -> copy (len <> 0) (i + 1) len
    else
      let len = if special then Buff.store len ' ' else len in
      let (t, j) = unaccent_utf_8 true s i in copy false j (Buff.mstore len t)
  in
  copy false 0 0

(* Name.abbrev *)

let abbrev_list =
  ["a", None; "af", None; "d", None; "de", None; "di", None; "ier", Some "i";
   "of", None; "saint", Some "st"; "sainte", Some "ste"; "van", None;
   "von", None; "zu", None; "zur", None]

let rec is_word s i p ip =
  if ip = String.length p then
    if i = String.length s then true else if s.[i] = ' ' then true else false
  else if i = String.length s then false
  else if s.[i] = p.[ip] then is_word s (i + 1) p (ip + 1)
  else false

let rec search_abbrev s i =
  function
    (w, a) :: pl ->
      if is_word s i w 0 then Some (String.length w, a)
      else search_abbrev s i pl
  | [] -> None

let abbrev s =
  let rec copy can_start_abbrev i len =
    if i >= String.length s then Buff.get len
    else
      match s.[i] with
        ' ' -> copy true (i + 1) (Buff.store len ' ')
      | c ->
          if can_start_abbrev then
            match search_abbrev s i abbrev_list with
              None -> copy false (i + 1) (Buff.store len c)
            | Some (n, Some a) -> copy false (i + n) (Buff.mstore len a)
            | Some (n, None) -> copy true (i + n + 1) len
          else copy false (i + 1) (Buff.store len c)
  in
  copy true 0 0

(* Name.strip *)

let strip_c s c =
  let rec copy i len =
    if i = String.length s then Buff.get len
    else if s.[i] = c then copy (i + 1) len
    else copy (i + 1) (Buff.store len s.[i])
  in
  copy 0 0

let strip s = strip_c s ' '


(* ******************************************************************** *)
(*  [Fonc] purge : string -> string                                     *)
(** [Description] : Supprime tous les caractères interdits (défini par
                    forbidden_char) présents dans la chaine passée en
                    argument.
    [Args] :
      - s : string que l'on veut purger
    [Retour] :
      - string : retourne la chaîne délestée des caractères interdits
    [Rem] : Exporté en clair hors de ce module.                         *)
(* ******************************************************************** *)
let purge s = List.fold_left (fun s c -> strip_c s c) s forbidden_char


(* Name.crush *)

let roman_number s i =
  let rec loop i =
    if i = String.length s then Some i
    else if s.[i] = ' ' then Some i
    else
      match s.[i] with
        'i' | 'v' | 'x' | 'l' -> loop (i + 1)
      | _ -> None
  in
  if i = 0 || s.[i-1] = ' ' then loop i else None

let crush s =
  let rec copy i len first_vowel =
    if i = String.length s then Buff.get len
    else if s.[i] = ' ' then copy (i + 1) len true
    else
      match roman_number s i with
        Some j ->
          let rec loop i len =
            if i = j then copy j len true
            else loop (i + 1) (Buff.store len s.[i])
          in
          loop i len
      | _ ->
          match s.[i] with
            'a' | 'e' | 'i' | 'o' | 'u' | 'y' ->
              let len = if first_vowel then Buff.store len 'e' else len in
              copy (i + 1) len false
          | 'h' ->
              let len =
                if i > 0 && s.[i-1] = 'p' then Buff.store (len - 1) 'f'
                else len
              in
              copy (i + 1) len first_vowel
          | 's' | 'z'
            when (i = String.length s - 1 || s.[i+1] = ' ') ->
              let len =
                let rec loop i len =
                  if i > 0 && len > 0 && s.[i] = Bytes.get !(Buff.buff) len &&
                     (s.[i] = 's' || s.[i] = 'z')
                  then
                    loop (i - 1) (len - 1)
                  else len + 1
                in
                loop (i - 1) (len - 1)
              in
              copy (i + 1) len false
          | 's' when i = String.length s - 1 || s.[i+1] = ' ' ->
              copy (i + 1) len false
          | c ->
              if i > 0 && s.[i-1] = c then copy (i + 1) len false
              else
                let c =
                  match c with
                    'k' | 'q' -> 'c'
                  | 'z' -> 's'
                  | c -> c
                in
                copy (i + 1) (Buff.store len c) false
  in
  copy 0 0 true

(* strip_lower *)

let strip_lower s = strip (lower s)

(* crush_lower *)

let crush_lower s = crush (abbrev (lower s))
