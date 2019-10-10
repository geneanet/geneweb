(* Copyright (c) 1998-2007 INRIA *)

open Gwdb
open Util

let suburb_aux sub nosub s =
  let len = String.length s in
  if len = 0 then nosub ""
  else begin
    if String.unsafe_get s 0 = '[' then begin
      match String.index_opt s ']' with
      | None -> nosub s
      | Some i ->
        match
          let rec loop b i =
            if i = len then None
            else match String.unsafe_get s i with
              | ' ' -> loop b (i + 1)
              | '-' when not b -> loop true (i + 1)
              | _ -> if b then Some i else None
          in loop false (i + 1)
        with
        | None -> nosub s
        | Some j -> sub s len i j
    end else nosub s
  end

(** [split_suburb "[foo-bar] - boobar (baz)"] is [9"foo-bar", "boobar (baz)")] *)
let split_suburb =
  suburb_aux
    begin fun s len i j -> String.sub s 1 (i - 1), String.sub s j (len - j) end
    begin fun s -> "", s end

(** [only_suburb "[foo-bar] - boobar (baz)"] is ["foo-bar"]
    [only_suburb "boobar (baz)"] is [""] *)
let only_suburb =
  suburb_aux
    begin fun s _len i _j -> String.sub s 1 (i - 1) end
    begin fun _ -> "" end

(** [without_suburb "[foo-bar] - boobar (baz)"] is ["boobar (baz)"]
    [without_suburb "boobar (baz)"] is ["boobar (baz)"] *)
let without_suburb =
  suburb_aux
    begin fun s len _i j -> String.sub s j (len - j) end
    begin fun s -> s end

(** Transform ["[foo-bar] - boobar (baz)"] into ["foo-bar, boobar (baz)"] *)
let normalize =
  suburb_aux
    begin fun s len i j ->
      let b = Bytes.create (len - j + i + 1) in
      Bytes.blit_string s 1 b 0 (i - 1) ;
      Bytes.unsafe_set b (i - 1) ',' ;
      Bytes.unsafe_set b i ' ' ;
      Bytes.blit_string s j b (i + 1) (len - j) ;
      Bytes.unsafe_to_string b
    end
    begin fun s -> s end

let compare_places s1 s2 =
  let ss1, s1 = split_suburb s1 in
  let ss2, s2 = split_suburb s2 in
  match
    Mutil.list_compare
      Gutil.alphabetic_order
      (String.split_on_char ',' s1)
      (String.split_on_char ',' s2)
  with
  | 0 -> Gutil.alphabetic_order ss1 ss2
  | x -> x

(* [String.length s > 0] is always true because we already tested [is_empty_string].
   If it is not true, then the base should be cleaned. *)
let fold_place_long inverted s =
  let len = String.length s in
  (* Trimm spaces after ',' and build reverse String.split_on_char ',' *)
  let rec loop iend list i ibeg =
    if i = iend
    then if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
    else
      let (list, ibeg) =
        match String.unsafe_get s i with
        | ',' ->
          let list =
            if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
          in
          list, i + 1
        | ' ' when i = ibeg -> (list, i + 1)
        | _ -> list, ibeg
      in
      loop iend list (i + 1) ibeg
  in
  let list =
    if String.unsafe_get s (len - 1) = ')'
    then match String.rindex_opt s '(' with
      | Some i when i < len - 2 ->
        let j =
          let rec loop i =
            if i >= 0 && String.unsafe_get s i = ' ' then loop (i - 1) else i + 1
          in
          loop (i - 1)
        in
        String.sub s (i + 1) (len - i - 2) :: loop j [] 0 0
      | _ -> loop len [] 0 0
    else loop len [] 0 0
  in
  if inverted then List.rev list else list

let fold_place_short inverted s =
  if inverted
  then match String.index_opt s ',' with
    | Some i -> String.sub s 0 i
    | None -> s
  else begin
    let len = String.length s in
    let default () =
      let i =
        match String.rindex_opt s ',' with
        | Some i ->
          let rec l i = if i < len && String.unsafe_get s i = ' ' then l (i + 1) else i in l (i + 1)
        | None -> 0
      in
      let i = if i = len then 0 else i in
      String.sub s i (len - i)
    in
    if String.unsafe_get s (len - 1) = ')'
    then match String.rindex_opt s '(' with
      | Some i when i < len - 2 ->
        String.sub s (i + 1) (len - i - 2)
      | _ -> default ()
    else default ()
  end

exception List_too_long

let get_all =
  fun conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage
    (dummy_key : 'a)
    (dummy_value : 'c)
    (fold_place : string -> 'a)
    (filter : 'a -> bool)
    (mk_value : 'b option -> person -> 'b)
    (fn : 'b -> 'c)
    (max_length : int) :
    ('a * 'c) array ->
  let ht_size = 2048 in (* FIXME: find the good heuristic *)
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
  let ht_add istr p =
    let key : 'a = sou base istr |> normalize |> fold_place in
    if filter key then begin
      begin match Hashtbl.find_opt ht key with
      | Some _ as prev -> Hashtbl.replace ht key (mk_value prev p)
      | None ->
        Hashtbl.add ht key (mk_value None p) ;
        if Hashtbl.length ht > max_length then raise List_too_long
      end
    end
  in
  if add_birth || add_death || add_baptism || add_burial then begin
    let aux b fn p =
      if b then let x = fn p in if not (is_empty_string x) then ht_add x p
    in
    Gwdb.Collection.iter (fun i ->
        let p = pget conf base i in
        if authorized_age conf base p then begin
          aux add_birth get_birth_place p ;
          aux add_baptism get_baptism_place p ;
          aux add_death get_death_place p ;
          aux add_burial get_burial_place p ;
        end)
    (Gwdb.ipers base) ;
  end ;
  if add_marriage then begin
    Gwdb.Collection.iter (fun i ->
        let fam = foi base i in
          let pl_ma = get_marriage_place fam in
          if not (is_empty_string pl_ma) then
            let fath = pget conf base (get_father fam) in
            let moth = pget conf base (get_mother fam) in
            if authorized_age conf base fath
            && authorized_age conf base moth
            then begin
              ht_add pl_ma fath ;
              ht_add pl_ma moth
        end)
      (Gwdb.ifams base) ;
  end ;
  let len = Hashtbl.length ht in
  let array = Array.make len (dummy_key, dummy_value) in
  let i = ref 0 in
  Hashtbl.iter
    (fun k v ->
       Array.unsafe_set array !i (k, fn v) ;
       incr i)
    ht ;
  array
