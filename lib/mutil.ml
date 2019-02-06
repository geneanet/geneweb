(* $Id: mutil.ml,v 5.19 2007-08-04 07:22:30 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

let int_size = 4
let verbose = ref true

let list_iter_first f = function
  | [] -> ()
  | hd :: tl -> f true hd ; List.iter (f false) tl

(* [decline] has been deprecated since version 5.00
   compatibility code: *)
let colon_to_at_word s ibeg iend =
  let iendroot =
    let rec loop i =
      if i + 3 >= iend then iend
      else if s.[i] = ':' && s.[i+2] = ':' then i
      else loop (i + 1)
    in
    loop ibeg
  in
  if iendroot = iend then String.sub s ibeg (iend - ibeg)
  else
    let (listdecl, maxd) =
      let rec loop list maxd i =
        if i >= iend then list, maxd
        else
          let inext =
            let rec loop i =
              if i + 3 >= iend then iend
              else if s.[i] = ':' && s.[i+2] = ':' then i
              else loop (i + 1)
            in
            loop (i + 3)
          in
          let (e, d) =
            let i = i + 3 in
            let j = inext in
            if i < j && s.[i] = '+' then String.sub s (i + 1) (j - i - 1), 0
            else if i < j && s.[i] = '-' then
              let rec loop n i =
                if i < j && s.[i] = '-' then loop (n + 1) (i + 1)
                else String.sub s i (j - i), n
              in
              loop 1 (i + 1)
            else String.sub s i (j - i), iendroot - ibeg
          in
          loop ((s.[i+1], e) :: list) (max d maxd) inext
      in
      loop [] 0 iendroot
    in
    let len = max 0 (iendroot - ibeg - maxd) in
    let root = String.sub s ibeg len in
    let s =
      List.fold_left
        (fun t (c, e) ->
           Printf.sprintf "%c?%s%s" c e (if t = "" then "" else ":" ^ t))
        (String.sub s (ibeg + len) (iendroot - ibeg - len)) listdecl
    in
    root ^ "@(" ^ s ^ ")"

let colon_to_at s =
  let rec loop ibeg i =
    if i = String.length s then
      if i = ibeg then "" else colon_to_at_word s ibeg i
    else
      match s.[i] with
        ' ' | '<' | '/' as sep ->
          colon_to_at_word s ibeg i ^ String.make 1 sep ^ loop (i + 1) (i + 1)
      | '>' -> String.sub s ibeg (i + 1 - ibeg) ^ loop (i + 1) (i + 1)
      | _ -> loop ibeg (i + 1)
  in
  loop 0 0

let decline case s =
  Printf.sprintf "@(@(%c)%s)" case
    (if not (String.contains s ':') then s else colon_to_at s)
(* end compatibility code *)

let nominative s =
  match String.rindex_opt s ':' with
    Some _ -> decline 'n' s
  | _ -> s

let remove_file f = try Sys.remove f with Sys_error _ -> ()

let mkdir_p x =
  let rec loop x =
    let y = Filename.dirname x in
    if y <> x && String.length y < String.length x then loop y;
    try Unix.mkdir x 0o755 with Unix.Unix_error (_, _, _) -> ()
  in
  loop x

let rec remove_dir d =
  begin try
    let files = Sys.readdir d in
    for i = 0 to Array.length files - 1 do
      remove_dir (Filename.concat d files.(i));
      remove_file (Filename.concat d files.(i))
    done
  with Sys_error _ -> ()
  end;
  try Unix.rmdir d with Unix.Unix_error (_, _, _) -> ()

let lock_file bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then
      Filename.chop_suffix bname ".gwb"
    else bname
  in
  bname ^ ".lck"

let output_value_no_sharing oc v =
  Marshal.to_channel oc v [Marshal.No_sharing]

let initial n =
  let rec loop i =
    if i = String.length n then 0
    else
      match n.[i] with
        'A'..'Z' | '\192'..'\221' -> i
      | _ -> loop (succ i)
  in
  loop 0

let name_key s =
  let i = initial s in
  let s =
    if i = 0 then s
    else String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i
  in
  Name.lower s

let input_particles fname =
  try
    let ic = open_in fname in
    let rec loop list len =
      match input_char ic with
      | '_' -> loop list (Buff.store len ' ')
      | '\n' -> loop (if len = 0 then list else Buff.get len :: list) 0
      | '\r' -> loop list len
      | c -> loop list (Buff.store len c)
      | exception End_of_file ->
        close_in ic;
        List.rev (if len = 0 then list else Buff.get len :: list)
    in
    loop [] 0
  with Sys_error _ -> []

let saints = ["saint"; "sainte"]

let surnames_pieces surname =
  let surname = Name.lower surname in
  let flush i0 i1 =
    if i1 > i0 then [String.sub surname i0 (i1 - i0)] else []
  in
  let rec loop i0 iw i =
    if i = String.length surname then
      if i0 = 0 then [] else if i > i0 + 3 then flush i0 i else []
    else if surname.[i] = ' ' then
      if i > iw + 3 then
        let w = String.sub surname iw (i - iw) in
        if List.mem w saints then loop i0 (i + 1) (i + 1)
        else flush i0 i @ loop (i + 1) (i + 1) (i + 1)
      else loop i0 (i + 1) (i + 1)
    else loop i0 iw (i + 1)
  in
  loop 0 0 0

let tr c1 c2 s =
  match String.rindex_opt s c1 with
  | Some _ ->
    String.init
      (String.length s)
      (fun i -> let c = String.unsafe_get s i in if c = c1 then c2 else c)
  | None -> s

let utf_8_of_iso_8859_1 str =
  let rec loop i len =
    if i = String.length str then Buff.get len
    else
      let c = str.[i] in
      if Char.code c < 0x80 then loop (i + 1) (Buff.store len c)
      else if Char.code c < 0xC0 then
        let len = Buff.store len (Char.chr 0xC2) in
        loop (i + 1) (Buff.store len c)
      else
        let len = Buff.store len (Char.chr 0xC3) in
        loop (i + 1) (Buff.store len (Char.chr (Char.code c - 0x40)))
  in
  loop 0 0

let iso_8859_1_of_utf_8 s =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else
      let c = s.[i] in
      match Char.code c with
        0xC2 when i + 1 < String.length s ->
          loop (i + 2) (Buff.store len s.[i+1])
      | 0xC3 when i + 1 < String.length s ->
          loop (i + 2) (Buff.store len (Char.chr (Char.code s.[i+1] + 0x40)))
      | _ -> loop (i + 1) (Buff.store len c)
  in
  loop 0 0

let strip_all_trailing_spaces s =
  let b = Buffer.create (String.length s) in
  let len =
    let rec loop i =
      if i < 0 then 0
      else
        match s.[i] with
          ' ' | '\t' | '\r' | '\n' -> loop (i - 1)
        | _ -> i + 1
    in
    loop (String.length s - 1)
  in
  let rec loop i =
    if i = len then Buffer.contents b
    else
      match s.[i] with
        '\r' -> loop (i + 1)
      | ' ' | '\t' ->
          let rec loop0 j =
            if j = len then Buffer.contents b
            else
              match s.[j] with
                ' ' | '\t' | '\r' -> loop0 (j + 1)
              | '\n' -> loop j
              | _ -> Buffer.add_char b s.[i]; loop (i + 1)
          in
          loop0 (i + 1)
      | c -> Buffer.add_char b c; loop (i + 1)
  in
  loop 0

let output_array_no_sharing oc arr_get arr_len =
  let header_pos = Iovalue.create_output_value_header oc in
  Iovalue.output_block_header oc 0 arr_len;
  for i = 0 to arr_len - 1 do Iovalue.output oc (arr_get i) done;
  let pos_end = Iovalue.patch_output_value_header oc header_pos in
  seek_out oc pos_end

let roman_of_arabian n =
  let build one five ten =
    function
      0 -> ""
    | 1 -> one
    | 2 -> one ^ one
    | 3 -> one ^ one ^ one
    | 4 -> one ^ five
    | 5 -> five
    | 6 -> five ^ one
    | 7 -> five ^ one ^ one
    | 8 -> five ^ one ^ one ^ one
    | _ -> one ^ ten
  in
  build "M" "M" "M" (n / 1000 mod 10) ^ build "C" "D" "M" (n / 100 mod 10) ^
  build "X" "L" "C" (n / 10 mod 10) ^ build "I" "V" "X" (n mod 10)

let arabian_of_roman s =
  let decode_digit one five ten r =
    let rec loop cnt i =
      if i >= String.length s then 10 * r + cnt, i
      else if s.[i] = one then loop (cnt + 1) (i + 1)
      else if s.[i] = five then
        if cnt = 0 then loop 5 (i + 1) else 10 * r + 5 - cnt, i + 1
      else if s.[i] = ten then 10 * r + 10 - cnt, i + 1
      else 10 * r + cnt, i
    in
    loop 0
  in
  let (r, i) = decode_digit 'M' 'M' 'M' 0 0 in
  let (r, i) = decode_digit 'C' 'D' 'M' r i in
  let (r, i) = decode_digit 'X' 'L' 'C' r i in
  let (r, i) = decode_digit 'I' 'V' 'X' r i in
  if i = String.length s then r else raise Not_found

module StrSet = Set.Make (struct type t = string let compare = compare end)

let start_with ?(wildcard = false) ini i s =
  let inilen = String.length ini in
  let strlen = String.length s in
  if i < 0 || i > strlen then raise (Invalid_argument "start_with") ;
  let rec loop i1 i2 =
    if i1 = inilen then true
    else if i2 = strlen
    then
      if wildcard && String.unsafe_get ini i1 = '_'
      then loop (i1 + 1) i2 else false
    else if String.unsafe_get s i2 = String.unsafe_get ini i1
         || (wildcard && String.unsafe_get s i2 = ' ' && String.unsafe_get ini i1 = '_')
    then loop (i1 + 1) (i2 + 1)
    else false
  in
  loop 0 i

let contains ?(wildcard = false) str sub =
  let strlen = String.length str in
  let sublen = String.length sub in
  if not wildcard
  then
    let rec loop i =
      if i + sublen <= strlen
      then start_with ~wildcard sub i str || loop (i + 1)
      else false
    in loop 0
  else
    let rec loop i =
      i <= strlen && (start_with ~wildcard sub i str || loop (i + 1))
    in loop 0

let get_particle list s =
  let rec loop = function
    | hd :: _ when start_with hd 0 s -> hd
    | _ :: tl -> loop tl
    | [] -> ""
  in
  loop list

let compare_after_particle particles s1 s2 =
  let p1 = get_particle particles s1 in
  let p2 = get_particle particles s2 in
  let rec loop i1 i2 =
    if i1 = String.length s1 && i2 = String.length s2 then compare p1 p2
    else if i1 = String.length s1 then -1
    else if i2 = String.length s2 then 1
    else
      let c1 = String.unsafe_get s1 i1 in
      let c2 = String.unsafe_get s2 i2 in
      if c1 < c2 then -1 else if c1 > c2 then 1 else loop (i1 + 1) (i2 + 1)
  in
  loop (String.length p1) (String.length p2)

let input_lexicon lang ht open_fname =
  try
    let ic = open_fname () in
    let lang =
      match String.index_opt lang '.' with
        Some i -> String.sub lang 0 i
      | None -> lang
    in
    let derived_lang =
      match String.index_opt lang '-' with
        Some i -> String.sub lang 0 i
      | None ->
          match String.index_opt lang '_' with
            Some i -> String.sub lang 0 i
          | None -> ""
    in
    try
      begin try
        while true do
          let k =
            let rec find_key line =
              if String.length line < 4 then find_key (input_line ic)
              else if String.sub line 0 4 <> "    " then
                find_key (input_line ic)
              else line
            in
            find_key (input_line ic)
          in
          let k = String.sub k 4 (String.length k - 4) in
          let rec loop line =
            match String.index_opt line ':' with
              Some i ->
                let line_lang = String.sub line 0 i in
                if line_lang = lang ||
                   line_lang = derived_lang && not (Hashtbl.mem ht k)
                then
                  begin let v =
                    if i + 1 = String.length line then ""
                    else String.sub line (i + 2) (String.length line - i - 2)
                  in
                    Hashtbl.add ht k v
                  end;
                loop (input_line ic)
            | None -> ()
          in
          loop (input_line ic)
        done
      with End_of_file -> ()
      end;
      close_in ic
    with e -> close_in ic; raise e
  with Sys_error _ -> ()

let array_to_list_map fn a =
  let rec loop acc i =
    if i < 0 then acc
    else loop (fn (Array.unsafe_get a i) :: acc) (i - 1)
  in
  loop [] (Array.length a - 1)
