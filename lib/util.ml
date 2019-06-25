(* $Id: util.ml,v 5.130 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

let rm fname =
  if Sys.file_exists fname then Sys.remove fname

let is_hide_names conf p =
  if conf.hide_names || get_access p = Private then true else false

let sharelib =
  List.fold_right Filename.concat [Gwlib.prefix; "share"] "geneweb"

let add_lang_path = Secure.add_lang_path
let set_base_dir = Secure.set_base_dir

let _ = add_lang_path sharelib
let _ = add_lang_path Filename.current_dir_name

let cnt_dir = ref Filename.current_dir_name

let search_in_path p s =
  let rec loop =
    function
      d :: dl ->
        let f = Filename.concat d s in
        if Sys.file_exists f then f else loop dl
    | [] -> s
  in
  loop (p ())

let search_in_lang_path = search_in_path Secure.lang_path

(* Internationalization *)

let start_with_vowel s =
  if String.length s > 0 then
    let (s, _) = Name.unaccent_utf_8 true s 0 in
    match s.[0] with
      'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false
  else false

let start_with_hi_i s =
  if String.length s > 0 then
    match Char.lowercase_ascii s.[0] with
      'i' -> true
    | 'h' -> String.length s > 1 && s.[1] = 'i'
    | _ -> false
  else false
let match_begin s t =
  let rec loop i j =
    if i >= String.length s || j >= String.length t then true
    else if s.[i] = t.[j] then loop (i + 1) (j + 1)
    else false
  in
  loop 0 0

let amp_capitalize capitale s =
  if String.length s <= 1 then s
  else if match_begin s "&iexcl;" then
    "&iexcl;" ^ capitale (String.sub s 7 (String.length s - 7))
  else if match_begin s "&aelig;" then
    "&AElig;" ^ String.sub s 7 (String.length s - 7)
  else
    match s.[1] with
      'a'..'z' ->
        "&" ^
        String.make 1
          (Char.chr (Char.code s.[1] - Char.code 'a' + Char.code 'A')) ^
        String.sub s 2 (String.length s - 2)
    | _ -> s


(* Copied from Ocaml < 4.03. To be removed with proper utf8 support *)
let uppercase c =
  if (c >= 'a' && c <= 'z')
  || (c >= '\224' && c <= '\246')
  || (c >= '\248' && c <= '\254')
  then Char.unsafe_chr(Char.code c - 32)
  else c

let rec capitale_utf_8 s =
  if String.length s = 0 then ""
  else
    let c = s.[0] in
    if c = '&' then amp_capitalize capitale_utf_8 s
    else if c = '<' then
      let rec loop i =
        if i = String.length s then s
        else if s.[i] = '>' then
          let s1 = String.sub s (i + 1) (String.length s - i - 1) in
          String.sub s 0 (i + 1) ^ capitale_utf_8 s1
        else loop (i + 1)
      in
      loop 1
    else if Char.code c < 0b10000000 then String.capitalize_ascii s
    else if String.length s = 1 then s
    else
      match Char.code c with
        0xC3 when Char.code s.[1] <> 0xBF ->
        let c1 = uppercase (Char.chr (Char.code s.[1] + 0x40)) in
        let c1 = Char.chr (Char.code c1 - 0x40) in
          Printf.sprintf "%c%c%s" c c1 (String.sub s 2 (String.length s - 2))
      | 0xC3 when Char.code s.[1] = 0xBF ->
          (* ÿ *)
          let c = Char.chr 0xC5 in
          let c1 = Char.chr 0xB8 in
          Printf.sprintf "%c%c%s" c c1 (String.sub s 2 (String.length s - 2))
      | 0xC4 | 0xC5 | 0xC6 | 0xC7 ->
          let c1 = Char.chr (Char.code s.[1] - 1) in
          Printf.sprintf "%c%c%s" c c1 (String.sub s 2 (String.length s - 2))
      | 0xD0 when Char.code s.[1] >= 0xB0 ->
          (* cyrillic lowercase *)
          let c1 = Char.chr (Char.code s.[1] - 0xB0 + 0x90) in
          Printf.sprintf "%c%c%s" c c1 (String.sub s 2 (String.length s - 2))
      | 0xD1 when Char.code s.[1] < 0x90 ->
          (* cyrillic lowercase again *)
          let c1 = Char.chr (Char.code s.[1] - 0x80 + 0xA0) in
          Printf.sprintf "%c%c%s" (Char.chr 0xD0) c1
            (String.sub s 2 (String.length s - 2))
      | _ -> s

let index_of_next_char s i =
  min (String.length s) (i + max 1 (Name.nbc s.[i]))

let capitale s = capitale_utf_8 s

type ('a, 'b) format2 = ('a, unit, string, 'b) format4

let fcapitale (a : ('a, 'b, 'c, 'd) format4) : ('a, 'b, 'c, 'd) format4 =
  Scanf.format_from_string (capitale (string_of_format a)) a

let nth_field_abs w n =
  let rec start i n =
    if n = 0 then i
    else if i < String.length w then
      match w.[i] with
        '<' -> start (i + 2) n
      | '/' -> start (i + 1) (n - 1)
      | _ -> start (i + 1) n
    else i
  in
  let rec stop i =
    if i < String.length w then
      match w.[i] with
        '<' -> stop (i + 2)
      | '/' -> i
      | _ -> stop (i + 1)
    else i
  in
  let i1 = start 0 n in let i2 = stop i1 in i1, i2

let nth_field w n =
  let (i1, i2) = nth_field_abs w n in
  let (i1, i2) = if i2 = i1 then nth_field_abs w 0 else i1, i2 in
  String.sub w i1 (i2 - i1)

let tnf s = "[" ^ s ^ "]"

let transl conf w = try Hashtbl.find conf.lexicon w with Not_found -> tnf w

let transl_nth conf w n =
  try nth_field (Hashtbl.find conf.lexicon w) n with
    Not_found -> tnf (nth_field w n)

let plus_decl s =
  match String.rindex_opt s '+' with
    Some i ->
      if i > 0 && s.[i-1] = ' ' then
        let start = String.sub s 0 (i - 1) in
        let decl = String.sub s (i - 1) (String.length s - (i - 1)) in
        Some (start, decl)
      else None
  | None -> None

let gen_decline_basic wt s =
  let s1 = if s = "" then "" else if wt = "" then s else " " ^ s in
  let len = String.length wt in
  if String.rindex_opt wt '/' <> None then
    if String.rindex_opt wt '/' <> None then
        (* special case for Spanish *)
        if String.length s > 0 && start_with_hi_i s then
          nth_field wt 1 ^ Mutil.decline 'n' s
        else nth_field wt 0 ^ Mutil.decline 'n' s1
    else wt ^ Mutil.decline 'n' s1
  else if len >= 3 && wt.[len-3] = ':' && wt.[len-1] = ':' then
    let start = String.sub wt 0 (len - 3) in
    start ^ Mutil.decline wt.[len-2] s
  else
    match plus_decl wt with
      Some (start, " +before") ->
        if s = "" then start else Mutil.decline 'n' s ^ " " ^ start
    | _ -> wt ^ Mutil.decline 'n' s1

let transl_decline conf w s = Translate.eval (gen_decline_basic (transl conf w) s)

let gen_decline wt s1 s2 s2_raw =
  let string_of =
    function
      '1' -> Some s1
    | '2' -> Some s2
    | _ -> None
  in
  let len = String.length wt in
  let rec loop i =
    if i = len then ""
    else
      let (s, i) =
        match wt.[i] with
          '%' when i + 1 < len ->
            begin match string_of wt.[i+1] with
              Some s -> s, i + 1
            | None -> "%", i
            end
        | ':' when i + 4 < len && wt.[i+2] = ':' && wt.[i+3] = '%' ->
            let c = wt.[i+1] in
            begin match string_of wt.[i+4] with
              Some s -> Mutil.decline c s, i + 4
            | None -> ":", i
            end
        | '[' ->
            begin try
              let j = String.index_from wt i ']' in
              let k = String.index_from wt i '|' in
              if k < j && j + 2 < len && wt.[j + 1] = '%' then
                match string_of wt.[j+2] with
                  Some s ->
                    let s =
                      if start_with_vowel s2_raw then String.sub wt (k+1) (j-k-1) ^ s (* [aa|bb]  *)
                      else String.sub wt (i + 1) (k-i-1) ^ s      (* i  k  j  *)
                    in
                    s, j + 2
                | None -> raise Not_found
              else raise Not_found
            with Not_found -> "[", i
            end
        | c -> String.make 1 c, i
      in
      s ^ loop (i + 1)
  in
  loop 0

let transl_a_of_b conf x y1 y2 =
  gen_decline (transl_nth conf "%1 of %2" 0) x y1 y2
let transl_a_of_gr_eq_gen_lev conf x y1 y2 =
  gen_decline (transl_nth conf "%1 of %2" 1) x y1 y2

let check_format ini_fmt (r : string) =
  let s = string_of_format ini_fmt in
  let rec loop i j =
    if i < String.length s - 1 && j < String.length r - 1 then
      match s.[i], s.[i+1], r.[j], r.[j+1] with
        '%', x, '%', y -> if x = y then loop (i + 2) (j + 2) else None
      | '%', _, _, _ -> loop i (j + 1)
      | _, _, '%', _ -> loop (i + 1) j
      | _ -> loop (i + 1) (j + 1)
    else if i < String.length s - 1 then
      if s.[i] = '%' then None else loop (i + 1) j
    else if j < String.length r - 1 then
      if r.[j] = '%' then None else loop i (j + 1)
    else Some (Scanf.format_from_string r ini_fmt)
  in
  loop 0 0

let valid_format ini_fmt r =
  match check_format ini_fmt r with
    Some fmt -> fmt
  | None -> Scanf.format_from_string (tnf r) ini_fmt

let cftransl conf fmt =
  let fmt = transl conf fmt in
  let rec loop i =
    function
      [] -> String.sub fmt i (String.length fmt - i)
    | a :: al as gal ->
        if i + 4 < String.length fmt && fmt.[i] = ':' && fmt.[i+2] = ':' &&
           fmt.[i+3] = '%' && fmt.[i+4] = 's'
        then
          Mutil.decline fmt.[i+1] a ^ loop (i + 5) al
        else if
          i + 1 < String.length fmt && fmt.[i] = '%' && fmt.[i+1] = 's'
        then
          Mutil.nominative a ^ loop (i + 2) al
        else if i < String.length fmt then
          String.make 1 fmt.[i] ^ loop (i + 1) gal
        else ""
  in
  loop 0

let ftransl conf s = valid_format s (transl conf (string_of_format s))

let ftransl_nth conf s p =
  valid_format s (transl_nth conf (string_of_format s) p)

let fdecline w s = valid_format w (gen_decline_basic (string_of_format w) s)

let translate_eval s = Translate.eval (Mutil.nominative s)

(* *)

let escape_char src dst str =
  let rec loop i len =
    if i = String.length str then Buff.get len
    else if str.[i] = src then loop (i + 1) (Buff.mstore len dst)
    else loop (i + 1) (Buff.store len str.[i])
  in
  loop 0 0

let escape_amp = escape_char '&' "&#38;"

let get_referer conf =
  let referer = Wserver.extract_param "referer: " '\n' conf.request in
  escape_amp referer

let begin_centered conf =
  Wserver.printf
    "<table border=\"%d\" width=\"100%%\"><tr><td align=\"center\">\n"
    conf.border
let end_centered _conf = Wserver.printf "</td></tr></table>\n"

let html_br conf = Wserver.printf "<br%s>\n" conf.xhs

let html_p _conf = Wserver.printf "<p>"; Wserver.printf "\n"

let html_li _conf = Wserver.printf "<li>"; Wserver.printf "\n"

let nl () = Wserver.printf "\013\010"

let week_day_txt =
  let txt = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  fun i -> let i = if i < 0 || i >= Array.length txt then 0 else i in txt.(i)
let month_txt =
  let txt =
    [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct";
       "Nov"; "Dec" |]
  in
  fun i -> let i = if i < 0 || i >= Array.length txt then 0 else i in txt.(i)

(* Returns true if sub is a substring of str or false otherwise. *)
let string_exists str sub =
  let re = Str.regexp_string sub in
  try ignore (Str.search_forward re str 0); true with Not_found -> false

let string_of_ctime conf =
  let lt = Unix.gmtime conf.ctime in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d GMT" (week_day_txt lt.Unix.tm_wday)
    lt.Unix.tm_mday (month_txt lt.Unix.tm_mon) (1900 + lt.Unix.tm_year)
    lt.Unix.tm_hour lt.Unix.tm_min lt.Unix.tm_sec

let html ?content_type conf =
  let content_type =
    match content_type with
      Some x -> x
    | None -> if conf.pure_xhtml then "application/xhtml+xml" else "text/html"
  in
  let charset = if conf.charset = "" then "utf-8" else conf.charset in
  if not !(Wserver.cgi) then Wserver.header "Server: GeneWeb/%s" Version.txt;
  Wserver.header "Date: %s" (string_of_ctime conf);
  Wserver.header "Connection: close";
  Wserver.header "Content-type: %s; charset=%s" content_type charset

let unauthorized conf auth_type =
  Wserver.http HttpStatus.Unauthorized;
  if not !(Wserver.cgi) then
    Wserver.header "WWW-Authenticate: Basic realm=\"%s\"" auth_type;
  Wserver.header "Content-type: text/html; charset=%s" conf.charset;
  Wserver.printf "<head><title>Access failed</title></head>\n";
  Wserver.printf "<body><h1>Access failed</h1>\n";
  Wserver.printf "<ul><li>%s</ul>\n" auth_type;
  Wserver.printf "</body>\n"

let commd conf =
  let c = conf.command ^ "?" in
  List.fold_left (fun c (k, v) ->
    if ( (k = "oc" || k = "ocz") && v = "0" ) || v = "" then c
    else c ^ k ^ "=" ^ v ^ "&") c (conf.henv @ conf.senv)

let commd_2 conf =
  let c = conf.command ^ "?" in
  List.fold_left (fun c (k, v) ->
    if ( k = "oc" || k = "ocz" && v = "" || v = "0" ) || v = "" then
      c else c ^ "&" ^ k ^ "=" ^ v ) c (conf.henv @ conf.senv)


let prefix_base conf =
  if conf.b_arg_for_basename then conf.command ^ "?b=" ^ conf.bname ^ "&"
  else conf.command ^ "?"

let prefix_base_2 conf =
  if conf.b_arg_for_basename then
    conf.command ^ "?b=" ^ conf.bname
  else
    conf.command ^ "?"

let prefix_base_password conf =
  if conf.b_arg_for_basename then
    if conf.cgi_passwd = "" then
      conf.command ^ "?b=" ^ conf.bname ^ "&"
    else
      conf.command ^ "?b=" ^ conf.bname ^ "_" ^ conf.cgi_passwd ^ "&"
  else
    conf.command ^ "?"


let prefix_base_password_2 conf =
  if conf.b_arg_for_basename then
    if conf.cgi_passwd = "" then
      conf.command ^ "?b=" ^ conf.bname
    else
      conf.command ^ "?b=" ^ conf.bname ^ "_" ^ conf.cgi_passwd
  else
    conf.command ^ "?"


let code_varenv = Wserver.encode
let decode_varenv = Wserver.decode

let safe_html_allowd_tags =
  [ ("http://www.w3.org/1999/xhtml", "a")
  ; ("http://www.w3.org/1999/xhtml", "area")
  ; ("http://www.w3.org/1999/xhtml", "b")
  ; ("http://www.w3.org/1999/xhtml", "blockquote")
  ; ("http://www.w3.org/1999/xhtml", "br")
  ; ("http://www.w3.org/1999/xhtml", "center")
  ; ("http://www.w3.org/1999/xhtml", "cite")
  ; ("http://www.w3.org/1999/xhtml", "dd")
  ; ("http://www.w3.org/1999/xhtml", "dir")
  ; ("http://www.w3.org/1999/xhtml", "div")
  ; ("http://www.w3.org/1999/xhtml", "dl")
  ; ("http://www.w3.org/1999/xhtml", "dt")
  ; ("http://www.w3.org/1999/xhtml", "em")
  ; ("http://www.w3.org/1999/xhtml", "embed")
  ; ("http://www.w3.org/1999/xhtml", "font")
  ; ("http://www.w3.org/1999/xhtml", "h1")
  ; ("http://www.w3.org/1999/xhtml", "h2")
  ; ("http://www.w3.org/1999/xhtml", "h3")
  ; ("http://www.w3.org/1999/xhtml", "h4")
  ; ("http://www.w3.org/1999/xhtml", "h5")
  ; ("http://www.w3.org/1999/xhtml", "h6")
  ; ("http://www.w3.org/1999/xhtml", "hr")
  ; ("http://www.w3.org/1999/xhtml", "i")
  ; ("http://www.w3.org/1999/xhtml", "img")
  ; ("http://www.w3.org/1999/xhtml", "li")
  ; ("http://www.w3.org/1999/xhtml", "map")
  ; ("http://www.w3.org/1999/xhtml", "object")
  ; ("http://www.w3.org/1999/xhtml", "ol")
  ; ("http://www.w3.org/1999/xhtml", "ol")
  ; ("http://www.w3.org/1999/xhtml", "p")
  ; ("http://www.w3.org/1999/xhtml", "param")
  ; ("http://www.w3.org/1999/xhtml", "pre")
  ; ("http://www.w3.org/1999/xhtml", "s")
  ; ("http://www.w3.org/1999/xhtml", "small")
  ; ("http://www.w3.org/1999/xhtml", "span")
  ; ("http://www.w3.org/1999/xhtml", "strike")
  ; ("http://www.w3.org/1999/xhtml", "strong")
  ; ("http://www.w3.org/1999/xhtml", "sub")
  ; ("http://www.w3.org/1999/xhtml", "sup")
  ; ("http://www.w3.org/1999/xhtml", "table")
  ; ("http://www.w3.org/1999/xhtml", "tbody")
  ; ("http://www.w3.org/1999/xhtml", "td")
  ; ("http://www.w3.org/1999/xhtml", "tfoot")
  ; ("http://www.w3.org/1999/xhtml", "th")
  ; ("http://www.w3.org/1999/xhtml", "thead")
  ; ("http://www.w3.org/1999/xhtml", "tr")
  ; ("http://www.w3.org/1999/xhtml", "tt")
  ; ("http://www.w3.org/1999/xhtml", "u")
  ; ("http://www.w3.org/1999/xhtml", "ul")
  ; ("http://www.w3.org/1999/xhtml", "nav")
  ; ("http://www.w3.org/1999/xhtml", "section")
  ]

(** [escape_html str] replaces '&', '"', '<' and '>'
    with their corresponding character entities (using entity number) *)
let escape_html str =
  let strlen = String.length str in
  let rec loop acc i =
    if i < strlen then
      match String.unsafe_get str i with
      | '&' | '"' | '\'' | '<' | '>' -> loop (acc + 5) (i + 1) (* "&#xx;" *)
      | _ -> loop (acc + 1) (i + 1)
    else if acc = strlen then str
    else
      let buf = Bytes.create acc in
      let rec loop istr ibuf =
        if istr = strlen then Bytes.unsafe_to_string buf
        else match String.unsafe_get str istr with
          | '&' -> Bytes.blit_string "&#38;" 0 buf ibuf 5 ; loop (istr + 1) (ibuf + 5)
          | '"' -> Bytes.blit_string "&#34;" 0 buf ibuf 5 ; loop (istr + 1) (ibuf + 5)
          | '\'' -> Bytes.blit_string "&#39;" 0 buf ibuf 5 ; loop (istr + 1) (ibuf + 5)
          | '<' -> Bytes.blit_string "&#60;" 0 buf ibuf 5 ; loop (istr + 1) (ibuf + 5)
          | '>' -> Bytes.blit_string "&#62;" 0 buf ibuf 5 ; loop (istr + 1) (ibuf + 5)
          | c -> Bytes.unsafe_set buf ibuf c ; loop (istr + 1) (ibuf + 1)
      in loop 0 0
  in
  loop 0 0

(** [escape_attribute str] only escapes double quote and ampersand.
    Since we will return normalized HTML, ['"'] should be the only
    dangerous character here. *)
let escape_attribute str =
  let strlen = String.length str in
  let rec loop acc i =
    if i < strlen then
      match String.unsafe_get str i with
      | '&' | '"' -> loop (acc + 5) (i + 1) (* "&#xx;" *)
      | _ -> loop (acc + 1) (i + 1)
    else if acc = strlen then str
    else
      let buf = Bytes.create acc in
      let rec loop istr ibuf =
        if istr = strlen then Bytes.unsafe_to_string buf
        else match String.unsafe_get str istr with
          | '&' -> Bytes.blit_string "&#38;" 0 buf ibuf 5 ; loop (istr + 1) (ibuf + 5)
          | '"' -> Bytes.blit_string "&#34;" 0 buf ibuf 5 ; loop (istr + 1) (ibuf + 5)
          | c -> Bytes.unsafe_set buf ibuf c ; loop (istr + 1) (ibuf + 1)
      in loop 0 0
  in
  loop 0 0

(* Few notes:

   According to https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Data_URIs,
   > Data URLs are treated as unique opaque origins by modern browsers,
   > rather than inheriting the origin of the settings object responsible for the navigation.
   We do not need to filter this attribute.

   We filter out all attributes starting with ["on"] (prevent javascript from being executed).

   Remove any attribute when the value start with ["javascript"].

   Text is escaped using [escape_html].

   Replace tags not authorized with empty comments.

   Markup.ml automatically return tags names in lowercase.
 *)
let safe_html s =
  let open Markup in
  let stack = ref [] in
  let make_safe = function
    | `Start_element (name, attrs) ->
      if not @@ List.mem name safe_html_allowd_tags then begin
        stack := `KO :: !stack ;
        `Comment ""
      end else begin
        let attrs =
          List.filter (function ((_, k), v) ->
              (String.length k <= 2
               || (String.get k 0 <> 'o' || String.get k 1 <> 'n') )
              && not (Mutil.contains (String.lowercase_ascii v) "javascript") )
            attrs
        in
        stack := `OK :: !stack ;
        `Start_element (name, attrs)
      end
    | `End_element ->
      begin match !stack with
        | `KO :: tl -> stack := tl ; `Comment ""
        | `OK :: tl -> stack := tl ; `End_element
        | _ -> failwith __LOC__
      end
    | e -> e
  in
  string s
  |> parse_html ~context:(`Fragment "body")
  |> signals
  |> map make_safe
  |> write_html ~escape_text:escape_html ~escape_attribute
  |> to_string

let no_html_tags s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
        '<' | '>' -> true
      | _ -> need_code (i + 1)
    else false
  in
  if need_code 0 then
    let rec loop i len =
      if i = String.length s then Buff.get len
      else
        let (len, next_i) =
          match s.[i] with
            '<' -> Buff.mstore len "&lt;", i + 1
          | '>' -> Buff.mstore len "&gt;", i + 1
          | c -> Buff.store len c, i + 1
        in
        loop next_i len
    in
    loop 0 0
  else s

(* Version 1 => moche *)
let clean_html_tags s l =
  List.fold_left
    (fun s html_tag -> Str.global_replace (Str.regexp html_tag) "&nbsp;" s) s
    l

let hidden_env conf =
  List.iter
    (fun (k, v) ->
       Wserver.printf "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n" k
         (escape_html (decode_varenv v)) conf.xhs)
    (conf.henv @ conf.senv)

let p_getenv env label =
  Opt.map decode_varenv (List.assoc_opt (decode_varenv label) env)

let p_getint env label =
  try Opt.map (fun s -> int_of_string (String.trim s)) (p_getenv env label)
  with Failure _ -> None

let nobtit conf base p =
  Gwdb.nobtit base conf.allowed_titles conf.denied_titles p

let strictly_after_private_years conf a =
  if a.year > conf.private_years then true
  else if a.year < conf.private_years then false
  else a.month > 0 || a.day > 0

let is_old_person conf p =
  match
    Adef.od_of_cdate p.birth, Adef.od_of_cdate p.baptism, p.death,
    CheckItem.date_of_death p.death
  with
    _, _, NotDead, _ when conf.private_years > 0 -> false
  | Some (Dgreg (d, _)), _, _, _ ->
      let a = CheckItem.time_elapsed d conf.today in
      strictly_after_private_years conf a
  | _, Some (Dgreg (d, _)), _, _ ->
      let a = CheckItem.time_elapsed d conf.today in
      strictly_after_private_years conf a
  | _, _, _, Some (Dgreg (d, _)) ->
      let a = CheckItem.time_elapsed d conf.today in
      strictly_after_private_years conf a
  | None, None, DontKnowIfDead, None ->
      p.access <> Private && conf.public_if_no_date
  | _ -> false

let fast_auth_age conf p =
  if conf.friend || conf.wizard || get_access p = Public then true
  else if
    conf.public_if_titles && get_access p = IfTitles && get_titles p <> []
  then
    true
  else is_old_person conf (gen_person_of_person p)

(* ********************************************************************** *)
(*  [Fonc] authorized_age : config -> base -> person -> bool              *)
(** [Description] : Calcul les droits de visualisation d'une personne en
      fonction de son age.
      Renvoie (dans l'ordre des tests) :
        - Vrai si : magicien ou ami ou la personne est public
        - Vrai si : la personne est en si_titre, si elle a au moins un
                    titre et que public_if_title = yes dans le fichier gwf
        - Faux si : la personne n'est pas décédée et private_years > 0
        - Vrai si : la personne est plus agée (en fonction de la date de
                    naissance ou de la date de baptème) que privates_years
        - Faux si : la personne est plus jeune (en fonction de la date de
                    naissance ou de la date de baptème) que privates_years
        - Vrai si : la personne est décédée depuis plus de privates_years
        - Faux si : la personne est décédée depuis moins de privates_years
        - Vrai si : la personne a entre 80 et 120 ans et qu'elle n'est pas
                    privée et public_if_no_date = yes
        - Vrai si : la personne s'est mariée depuis plus de private_years
        - Faux dans tous les autres cas
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : Vrai si on a les droits, Faux sinon.
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let authorized_age conf base p =
  conf.wizard
  || conf.friend
  || get_access p = Public
  || (conf.public_if_titles
      && get_access p = IfTitles
      && nobtit conf base p <> [])
  || begin
    let death = get_death p in
    if death = NotDead then conf.private_years < 1
    else
      let check_date none = function
        | Some (Dgreg (d, _)) ->
          strictly_after_private_years conf (CheckItem.time_elapsed d conf.today)
        | _ -> none ()
      in
      check_date
        (fun () ->
           check_date
             (fun () ->
                check_date
                  (fun () ->
                     (death = DontKnowIfDead && get_access p <> Private && conf.public_if_no_date)
                     || begin
                       let families = get_family p in
                       let len = Array.length families in
                       let rec loop i =
                         i < len
                         && check_date
                           (fun () -> loop (i + 1))
                           (Adef.od_of_cdate (get_marriage @@ foi base (Array.get families i)))
                       in
                       loop 0
                     end)
                  (CheckItem.date_of_death (get_death p)) )
             (Adef.od_of_cdate (get_baptism p)) )
        (Adef.od_of_cdate (get_birth p))
  end

let is_restricted (conf : config) base ip =
  let fct p =
    not (is_quest_string (get_surname p)) &&
    not (is_quest_string (get_first_name p)) &&
    not (authorized_age conf base p)
  in
  if conf.use_restrict then base_visible_get base fct (Adef.int_of_iper ip)
  else false

let pget (conf : config) base ip =
  if is_restricted conf base ip then Gwdb.empty_person base ip
  else poi base ip

let string_gen_person base p = Futil.map_person_ps (fun p -> p) (sou base) p

let string_gen_family base fam =
  Futil.map_family_ps (fun p -> p) (sou base) fam

let is_hidden p = is_empty_string (get_surname p)

let is_empty_name p =
  (Gwdb.is_empty_string (Gwdb.get_surname p) ||
   Gwdb.is_quest_string (Gwdb.get_surname p)) &&
  (Gwdb.is_empty_string (Gwdb.get_first_name p) ||
   Gwdb.is_quest_string (Gwdb.get_first_name p))

let is_public conf base p =
  get_access p = Public ||
  conf.public_if_titles && get_access p = IfTitles &&
  nobtit conf base p <> [] ||
  is_old_person conf (gen_person_of_person p)


(* ********************************************************************** *)
(*  [Fonc] accessible_by_key :
             config -> base -> person -> string -> string -> bool         *)
(** [Description] : Vrai si la personne est accessible par sa clé,
                    Faux sinon.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
      - fn   : prénom de la personne
      - sn   : patronyme de la personne
    [Retour] :
      - bool : Vrai si la personne est accessible par sa clé, faux sinon.
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let accessible_by_key conf base p fn sn =
  conf.access_by_key && not (fn = "?" || sn = "?") &&
  (not (is_hide_names conf p) || is_public conf base p || conf.friend ||
   conf.wizard)


(* ********************************************************************** *)
(*  [Fonc] acces_n : config -> base -> string -> person -> string         *)
(** [Description] : Renvoie les paramètres URL pour l'accès à la nième
                    personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - n    : la nième personne (e.g. : calcul de parenté entre p1 et p2)
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let acces_n conf base n x =
  let first_name = p_first_name base x in
  let surname = p_surname base x in
  if surname = "" then ""
  else if accessible_by_key conf base x first_name surname then
    "p" ^ n ^ "=" ^ code_varenv (Name.lower first_name) ^ "&n" ^ n ^ "=" ^
    code_varenv (Name.lower surname) ^
    (if get_occ x <> 0 then "&oc" ^ n ^ "=" ^ string_of_int (get_occ x)
     else "")
  else
    "i" ^ n ^ "=" ^ string_of_int (Adef.int_of_iper (get_key_index x)) ^
    (if conf.wizard && get_occ x <> 0 then
       "&oc" ^ n ^ "=" ^ string_of_int (get_occ x)
     else "")


(* ********************************************************************** *)
(*  [Fonc] acces : config -> base -> person -> string                     *)
(** [Description] : Renvoie les paramètres URL pour l'accès à la personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let acces conf base x = acces_n conf base "" x

type p_access = (base -> person -> string) * (base -> person -> string)
let std_access = p_first_name, p_surname
let raw_access =
  (fun base p -> sou base (get_first_name p)),
  (fun base p -> sou base (get_surname p))


(**/**)
(* Fonctions d'écriture du nom et prénom d'un individu en fonction de : *)
(*   - son/ses titre de noblesse                                        *)
(*   - son/ses nom public                                               *)
(*   - son/ses sobriquets                                               *)


let restricted_txt = "....."


(* ************************************************************************** *)
(*  [Fonc] gen_person_text : fun -> fun -> config -> base -> person -> string *)
(** [Description] : Renvoie le prénom et nom d'un individu en fonction
                    de son nom public et sobriquet.
    [Args] :
      - p_first_name : renvoie le prénom d'un individu (string)
      - p_surname    : renvoie le nom d'un individu (string)
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let gen_person_text (p_first_name, p_surname) conf base p =
  if is_hidden p then restricted_txt
  else if is_hide_names conf p && not (authorized_age conf base p) then "x x"
  else
    let beg =
      match sou base (get_public_name p), get_qualifiers p with
        "", nn :: _ -> p_first_name base p ^ " <em>" ^ sou base nn ^ "</em>"
      | "", [] -> p_first_name base p
      | n, nn :: _ -> n ^ " <em>" ^ sou base nn ^ "</em>"
      | n, [] -> n
    in
    let sn = p_surname base p in if sn = "" then beg else beg ^ " " ^ sn


(* ************************************************************************** *)
(*  [Fonc] gen_person_text_no_html :
             fun -> fun -> config -> base -> person -> string                 *)
(** [Description] : Renvoie le prénom et nom d'un individu en fonction
                    de son nom public et sobriquet (sans balise html <em>).
    [Args] :
      - p_first_name : renvoie le prénom d'un individu (string)
      - p_surname    : renvoie le nom d'un individu (string)
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let gen_person_text_no_html (p_first_name, p_surname) conf base p =
  if is_hidden p then restricted_txt
  else if is_hide_names conf p && not (authorized_age conf base p) then "x x"
  else
    let beg =
      match sou base (get_public_name p), get_qualifiers p with
        "", nn :: _ -> p_first_name base p ^ " " ^ sou base nn
      | "", [] -> p_first_name base p
      | n, nn :: _ -> n ^ " " ^ sou base nn
      | n, [] -> n
    in
    beg ^ " " ^ p_surname base p


(* ************************************************************************** *)
(*  [Fonc] gen_person_text_without_surname :
             fun -> fun -> config -> base -> person -> string                 *)
(** [Description] : Renvoie le prénom d'un individu en fonction de son
                    nom public et sobriquet.
    [Args] :
      - p_first_name : renvoie le prénom d'un individu (string)
      - p_surname    : renvoie le nom d'un individu (string)
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let gen_person_text_without_surname check_acc (p_first_name, _p_surname) conf
    base p =
  if is_hidden p then restricted_txt
  else if
    check_acc && is_hide_names conf p && not (authorized_age conf base p)
  then
    "x x"
  else
    match sou base (get_public_name p), get_qualifiers p with
      n, nn :: _ when n <> "" -> n ^ " <em>" ^ sou base nn ^ "</em>"
    | n, [] when n <> "" -> n
    | _, nn :: _ -> p_first_name base p ^ " <em>" ^ sou base nn ^ "</em>"
    | _, [] -> p_first_name base p

let person_text = gen_person_text std_access
let person_text_no_html = gen_person_text_no_html std_access
let person_text_without_surname =
  gen_person_text_without_surname true std_access
let person_text_no_surn_no_acc_chk =
  gen_person_text_without_surname false std_access


(* *********************************************************************** *)
(*  [Fonc] main_title : config -> base -> person -> title option           *)
(** [Description] : Renvoie le titre principal d'une personne. Si aucun
                    titre principal n'est trouvé mais que la personne a
                    plusieurs titre, alors on renvoie le premier titre.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : title option
    [Rem] : Exporté en clair hors de ce module.                            *)
(* *********************************************************************** *)
let main_title conf base p =
  (* Fonction de recherche du titre principal. *)
  let rec find_main =
    function
      [] -> None
    | x :: l -> if x.t_name = Tmain then Some x else find_main l
  in
  match find_main (nobtit conf base p) with
    None ->
      (* Aucun titre trouvé, on renvoie le premier (s'il existe). *)
      begin match nobtit conf base p with
        x :: _ -> Some x
      | _ -> None
      end
  | x -> x


(* *********************************************************************** *)
(*  [Fonc] titled_person_text : config -> base -> person -> istr gen_title *)
(** [Description] : Renvoie la chaîne de caractère de la personne en
                    fonction de son titre.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
      - t    : gen_title
    [Retour] : string
    [Rem] : Non exporté en clair hors de ce module.                        *)
(* *********************************************************************** *)
let titled_person_text conf base p t =
  if p_getenv conf.base_env "print_advanced_title" = Some "yes" then
    let estate = sou base t.t_place in
    let surname = p_surname base p in
    let elen = String.length estate in
    let slen = String.length surname in
    (* Si le nom de l'individu est le même que son domaine, on renvoie : *)
    (*   - le nom du titre                                               *)
    (*   - le nom du titre et le premier sobriquet                       *)
    (*   - le nom de la personne (donné par son nom de domaine) en       *)
    (*     fonction du nom public et sobriquet                           *)
    if Name.strip_lower estate = Name.strip_lower surname then
      match t.t_name, get_qualifiers p with
        Tname n, [] -> sou base n
      | Tname n, nn :: _ -> sou base n ^ " <em>" ^ sou base nn ^ "</em>"
      | _ -> person_text_without_surname conf base p
    else if elen < slen && String.sub surname (slen - elen) elen = estate then
      match t.t_name, get_qualifiers p with
        Tname n, [] -> sou base n
      | Tname n, nn :: _ -> sou base n ^ " <em>" ^ sou base nn ^ "</em>"
      | _ ->
          let trunc_surname _ _ =
            String.trim (String.sub surname 0 (slen - elen))
          in
          let trunc_access = p_first_name, trunc_surname in
          gen_person_text trunc_access conf base p
    else
      match t.t_name with
        Tname s ->
          let s = sou base s in
          begin match get_qualifiers p with
            [] -> s
          | nn :: _ -> s ^ " <em>" ^ sou base nn ^ "</em>"
          end
      | _ -> person_text conf base p
  else person_text conf base p


(* *********************************************************************** *)
(*  [Fonc] one_title_text : base -> istr gen_title     *)
(** [Description] : Renvoie la chaîne de caractère du titre ainsi que le
                    domaine.
    [Args] :
      - base : base de donnée
      - t    : le titre de noblesse que l'on veut afficher
    [Retour] : string
    [Rem] : Non exporté en clair hors de ce module.                        *)
(* *********************************************************************** *)
let one_title_text base t =
  let place = sou base t.t_place in
  let s = sou base t.t_ident in
  let s = if place = "" then s else s ^ " " ^ place in ", <em>" ^ s ^ "</em>"

let geneweb_link conf href s =
  if conf.cancel_links then s
  else "<a href=\"" ^ commd conf ^ href ^ "\">" ^ s ^ "</a>"

let wprint_geneweb_link conf href s =
  Wserver.printf "%s" (geneweb_link conf href s)

let reference conf base p s =
  let iper = get_key_index p in
  if conf.cancel_links || is_hidden p then s
  else
    String.concat ""
      ["<a href=\""; commd conf; acces conf base p; "\" id=\"i";
       string_of_int (Adef.int_of_iper iper); "\">"; s; "</a>"]


(* ************************************************************************* *)
(*  [Fonc] update_family_loop : config -> base -> person -> string -> string *)
(** [Description] : Essaie de déterminer dans quelle famille il peut y avoir
                    une boucle. Si il n'y a pas d'ambiguité, alors on renvoie
                    un lien vers la famille à modifier, sinon, on renvoie un
                    lien vers le menu général de mise à jour.
    [Args] :
      - conf : configuration
      - base : base
      - p    : person
      - s    : la clé de la personne sous forme de string
    [Retour] :
      - string : retourne un lien de mise à jour soit vers la famille
                 contenant la boucle, soit vers le menu de mise à jour.
    [Rem] : Exporté en clair hors de ce module.                              *)
(* ************************************************************************* *)
let update_family_loop conf base p s =
  if conf.cancel_links || is_hidden p then s
  else
    let iper = get_key_index p in
    let list = get_family p in
    let list = Array.map (fun ifam -> ifam, get_children (foi base ifam)) list in
    let res =
      Array.fold_left
        (fun acc (ifam, children) -> if Array.mem iper children then ifam :: acc else acc)
        [] list
    in
    if conf.wizard then
      if List.length res = 1 then
        let iper = string_of_int (Adef.int_of_iper iper) in
        let ifam = string_of_int (Adef.int_of_ifam (List.hd res)) in
        "<a href=\"" ^ commd conf ^ "m=MOD_FAM&i=" ^ ifam ^ "&ip=" ^ iper ^
        "\">" ^ s ^ "</a>"
      else
        let iper = string_of_int (Adef.int_of_iper iper) in
        "<a href=\"" ^ commd conf ^ "m=U&i=" ^ iper ^ "\">" ^ s ^ "</a>"
    else s

let no_reference _conf _base _p s = s

let gen_person_title_text reference p_access conf base p =
  if authorized_age conf base p then
    match main_title conf base p with
      Some t ->
        reference conf base p (titled_person_text conf base p t) ^
          ", " ^ one_title_text base t
    | None -> reference conf base p (gen_person_text p_access conf base p)
  else reference conf base p (gen_person_text p_access conf base p)

let referenced_person_title_text = gen_person_title_text reference std_access

let person_title_text = gen_person_title_text no_reference std_access

let referenced_person_text conf base p =
  reference conf base p (person_text conf base p)

let referenced_person_text_without_surname conf base p =
  reference conf base p (person_text_without_surname conf base p)

let gen_person_text_without_title p_access conf base p =
  match main_title conf base p with
    Some t ->
      if eq_istr t.t_place (get_surname p) then
        gen_person_text_without_surname true p_access conf base p
      else
        begin match t.t_name, get_qualifiers p with
          Tname s, nn :: _ -> sou base s ^ " <em>" ^ sou base nn ^ "</em>"
        | Tname s, _ -> sou base s
        | _ -> gen_person_text p_access conf base p
        end
  | None -> gen_person_text p_access conf base p

let person_text_without_title = gen_person_text_without_title std_access

let person_title conf base p =
  if authorized_age conf base p then
    match main_title conf base p with
      Some t -> one_title_text base t
    | None -> ""
  else ""

let name_key base s =
  let part = Mutil.get_particle (Gwdb.base_particles base) s in
  if part = "" then s
  else
    let i = String.length part in
    String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i

let surname_particle base s =
  let part = Mutil.get_particle (Gwdb.base_particles base) s in
  let len = String.length part in
  if len = 0 then ""
  else if part.[len-1] = ' ' then " (" ^ String.sub part 0 (len - 1) ^ ")"
  else " (" ^ part ^ ")"

let surname_without_particle base s =
  let part_len = String.length (Mutil.get_particle (Gwdb.base_particles base) s) in
  String.sub s part_len (String.length s - part_len)

let rec skip_spaces s i =
  if i < String.length s && s.[i] = ' ' then skip_spaces s (i + 1) else i

let create_env s =
  let rec get_assoc beg i =
    if i = String.length s then
      if i = beg then [] else [String.sub s beg (i - beg)]
    else if s.[i] = ';' || s.[i] = '&' then
      let next_i = skip_spaces s (succ i) in
      String.sub s beg (i - beg) :: get_assoc next_i next_i
    else get_assoc beg (succ i)
  in
  let rec separate i s =
    if i = String.length s then s, ""
    else if s.[i] = '=' then
      String.sub s 0 i, String.sub s (succ i) (String.length s - succ i)
    else separate (succ i) s
  in
  List.map (separate 0) (get_assoc 0 0)

let std_color conf s =
  "<span style=\"color:" ^ conf.highlight ^ "\">" ^ s ^ "</span>"

let index_of_sex =
  function
    Male -> 0
  | Female -> 1
  | Neuter -> 2

let string_of_pevent_name conf base epers_name =
  match epers_name with
    Epers_Birth -> transl conf "birth"
  | Epers_Baptism -> transl conf "baptism"
  | Epers_Death -> transl conf "death"
  | Epers_Burial -> transl conf "burial"
  | Epers_Cremation -> transl conf "cremation"
  | Epers_Accomplishment -> transl conf "accomplishment"
  | Epers_Acquisition -> transl conf "acquisition"
  | Epers_Adhesion -> transl conf "adhesion"
  | Epers_BaptismLDS -> transl conf "baptismLDS"
  | Epers_BarMitzvah -> transl conf "bar mitzvah"
  | Epers_BatMitzvah -> transl conf "bat mitzvah"
  | Epers_Benediction -> transl conf "benediction"
  | Epers_ChangeName -> transl conf "change name"
  | Epers_Circumcision -> transl conf "circumcision"
  | Epers_Confirmation -> transl conf "confirmation"
  | Epers_ConfirmationLDS -> transl conf "confirmation LDS"
  | Epers_Decoration -> transl conf "decoration"
  | Epers_DemobilisationMilitaire -> transl conf "demobilisationMilitaire"
  | Epers_Diploma -> transl conf "diploma"
  | Epers_Distinction -> transl conf "distinction"
  | Epers_Dotation -> transl conf "dotation"
  | Epers_DotationLDS -> transl conf "dotationLDS"
  | Epers_Education -> transl conf "education"
  | Epers_Election -> transl conf "election"
  | Epers_Emigration -> transl conf "emigration"
  | Epers_Excommunication -> transl conf "excommunication"
  | Epers_FamilyLinkLDS -> transl conf "familyLinkLDS"
  | Epers_FirstCommunion -> transl conf "firstCommunion"
  | Epers_Funeral -> transl conf "funeral"
  | Epers_Graduate -> transl conf "graduate"
  | Epers_Hospitalisation -> transl conf "hospitalisation"
  | Epers_Illness -> transl conf "illness"
  | Epers_Immigration -> transl conf "immigration"
  | Epers_ListePassenger -> transl conf "listePassenger"
  | Epers_MilitaryDistinction -> transl conf "militaryDistinction"
  | Epers_MilitaryPromotion -> transl conf "militaryPromotion"
  | Epers_MilitaryService -> transl conf "militaryService"
  | Epers_MobilisationMilitaire -> transl conf "mobilisationMilitaire"
  | Epers_Naturalisation -> transl conf "naturalisation"
  | Epers_Occupation -> transl_nth conf "occupation/occupations" 0
  | Epers_Ordination -> transl conf "ordination"
  | Epers_Property -> transl conf "property"
  | Epers_Recensement -> transl conf "recensement"
  | Epers_Residence -> transl conf "residence"
  | Epers_Retired -> transl conf "retired"
  | Epers_ScellentChildLDS -> transl conf "scellentChildLDS"
  | Epers_ScellentParentLDS -> transl conf "scellentParentLDS"
  | Epers_ScellentSpouseLDS -> transl conf "scellentSpouseLDS"
  | Epers_VenteBien -> transl conf "venteBien"
  | Epers_Will -> transl conf "will"
  | Epers_Name n -> sou base n

let string_of_fevent_name conf base = function
  | Efam_Marriage -> transl conf "marriage event"
  | Efam_NoMarriage -> transl conf "no marriage event"
  | Efam_NoMention -> transl conf "no mention"
  | Efam_Engage -> transl conf "engage event"
  | Efam_Divorce -> transl conf "divorce event"
  | Efam_Separated -> transl conf "separate event"
  | Efam_Annulation -> transl conf "annulation"
  | Efam_MarriageBann -> transl conf "marriage bann"
  | Efam_MarriageContract -> transl conf "marriage contract"
  | Efam_MarriageLicense -> transl conf "marriage licence"
  | Efam_PACS -> transl conf "PACS"
  | Efam_Residence -> transl conf "residence"
  | Efam_Name n -> sou base n

let string_of_fevent conf base = function
  | Efam_NoMention -> transl conf "no mention"
  | x -> string_of_fevent_name conf base x

let string_of_witness_kind conf sex witness_kind =
  match witness_kind with
    Witness -> transl_nth conf "witness/witness/witnesses" 0
  | Witness_GodParent ->
      let n = index_of_sex sex in
      transl_nth conf "godfather/godmother/godparents" n

let base_path pref bname =
  let pref = Secure.base_dir () :: pref in
  let bfile = List.fold_right Filename.concat pref bname in
  if Sys.unix then
    if Sys.file_exists bfile then bfile
    else if String.length bname >= 6 then
      let dirs = pref @ [String.make 1 bname.[0]; String.make 1 bname.[1]] in
      List.fold_right Filename.concat dirs bname
    else bfile
  else bfile

(* ************************************************************************ *)
(*  [Fonc] etc_file_name : config -> string -> string                       *)
(** [Description] : Renvoie le chemin vers le fichier de template passé
                    en paramètre.
    [Args] :
      - conf  : configuration de la base
      - fname : le fichier de template
    [Retour] :
      - string : le chemin vers le fichier de template
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let etc_file_name conf fname =
  (* On recherche si dans le nom du fichier, on a specifié son *)
  (* répertoire, i.e. si fname est écrit comme ceci : dir/file *)
  let fname = List.fold_left Filename.concat "" (String.split_on_char '/' fname) in
  (* On cherche le fichier dans cet ordre :
     - dans la base (bases/etc/base_name/name.txt)
     - dans la base (bases/etc/templx/name.txt)
     - dans le répertoire des programmes (gw/etc/templx/name.txt) *)
  let file_exist dir =
    let base_name_tpl_dir =
      Filename.concat (base_path ["etc"] conf.bname) (fname ^ ".txt")
    in
    let base_tpl_dir =
      Filename.concat (base_path ["etc"] (Filename.basename dir))
        (fname ^ ".txt")
    in
    let etc_tpl_dir =
      Filename.concat (search_in_lang_path "etc")
        (Filename.concat dir (fname ^ ".txt"))
    in
    if Sys.file_exists base_name_tpl_dir then base_name_tpl_dir
    else if Sys.file_exists base_tpl_dir then base_tpl_dir
    else if Sys.file_exists etc_tpl_dir then etc_tpl_dir
    else ""
  in
  (* Recherche le template par défaut en fonction de la variable gwf *)
  (* template = templ1,templ2,*                                      *)
  let rec default_templ config_templ std_fname =
    match config_templ with
      [] | ["*"] -> std_fname
    | x :: l ->
        match file_exist x with
          "" -> default_templ l std_fname
        | s -> s
  in
  let config_templ =
    try
      let s = List.assoc "template" conf.base_env in
      let rec loop list i len =
        if i = String.length s then List.rev (Buff.get len :: list)
        else if s.[i] = ',' then loop (Buff.get len :: list) (i + 1) 0
        else loop list (i + 1) (Buff.store len s.[i])
      in
      loop [] 0 0
    with Not_found -> [conf.bname; "*"]
  in
  let dir =
    match p_getenv conf.env "templ" with
      Some x when List.mem "*" config_templ -> x
    | Some x when List.mem x config_templ -> x
    | Some _ | None ->
        match config_templ with
          [] | ["*"] -> ""
        | x :: _ -> x
  in
  (* template par défaut *)
  let std_fname =
    search_in_lang_path (Filename.concat "etc" (fname ^ ".txt"))
  in
  (* On cherche le template dans l'ordre de file_exist.         *)
  (* Si on ne trouve rien, alors on cherche le premier template *)
  (* par défaut tel que défini par la variable template du gwf  *)
  match file_exist dir with
    "" -> default_templ config_templ std_fname
  | s -> s

let open_etc_file fname =
  let fname1 = base_path ["etc"] (Filename.basename fname ^ ".txt") in
  let fname2 =
    search_in_lang_path
      (Filename.concat "etc" (Filename.basename fname ^ ".txt"))
  in
  try Some (Secure.open_in fname1) with
    Sys_error _ -> try Some (Secure.open_in fname2) with Sys_error _ -> None

let open_hed_trl conf fname =
  try Some (Secure.open_in (etc_file_name conf fname)) with
    Sys_error _ -> None

let open_templ_fname conf fname =
  try
    let fname = etc_file_name conf fname in
    Some (Secure.open_in fname, fname) with
    Sys_error _ ->
      let std_fname =
        search_in_lang_path (Filename.concat "etc" (fname ^ ".txt"))
      in
      try Some (Secure.open_in std_fname, std_fname) with Sys_error _ -> None

let open_templ conf fname = Opt.map fst (open_templ_fname conf fname)

let image_prefix conf = conf.image_prefix

(*
   On cherche le fichier dans cet ordre :
    - dans la base (bases/etc/name.txt)
    - dans le répertoire des programmes (gw/etc/name.txt)
*)
let find_misc_file name =
  let base_tpl_dir = Filename.concat (base_path ["etc"] "") name in
  let etc_tpl_dir = Filename.concat (search_in_lang_path "etc") name in
  if Sys.file_exists base_tpl_dir then base_tpl_dir
  else if Sys.file_exists etc_tpl_dir then etc_tpl_dir
  else ""

(* Code mort. Géré par le css
value default_background conf =
  Printf.sprintf "background:url('%s/gwback.jpg')" (image_prefix conf)
;

value default_body_prop conf =
  let style =
    match p_getenv conf.env "size" with
    [ Some v -> "font-size:" ^ v ^ "&"
    | None -> "" ]
  in
  let style = Printf.sprintf "%s%s" style (default_background conf) in
  " style=\"" ^ style ^ "\""
;
   Code mort. Géré par le css *)

let body_prop conf =
  try
    match List.assoc "body_prop" conf.base_env with
      "" -> ""
    | s -> " " ^ s
  with Not_found -> ""

let get_server_string request =
  if not !(Wserver.cgi) then Wserver.extract_param "host: " '\r' request
  else
    let server_name = try Sys.getenv "SERVER_NAME" with Not_found -> "" in
    let server_port =
      try Sys.getenv "SERVER_PORT" with Not_found | Failure _ -> "80"
    in
    if server_port = "80" then server_name
    else server_name ^ ":" ^ server_port

let get_request_string request =
  if not !(Wserver.cgi) then Wserver.extract_param "GET " ' ' request
  else
    let script_name = try Sys.getenv "SCRIPT_NAME" with Not_found -> "" in
    let query_string = try Sys.getenv "QUERY_STRING" with Not_found -> "" in
    script_name ^ "?" ^ query_string

let url_no_index conf base =
  let scratch s = code_varenv (Name.lower (sou base s)) in
  let get_a_person v =
    try
      let i = int_of_string v in
      if i >= 0 && i < nb_of_persons base then
        let p = pget conf base (Adef.iper_of_int i) in
        if is_hide_names conf p && not (authorized_age conf base p) ||
           is_hidden p
        then
          None
        else
          let f = scratch (get_first_name p) in
          let s = scratch (get_surname p) in
          let oc = string_of_int (get_occ p) in Some (f, s, oc)
      else None
    with Failure _ -> None
  in
  let get_a_family v =
    try
      let i = int_of_string v in
        if i >= 0 && i < nb_of_families base then
          let fam = foi base (Adef.ifam_of_int i) in
          if is_deleted_family fam then None
          else
            let p = pget conf base (get_father fam) in
            let f = scratch (get_first_name p) in
            let s = scratch (get_surname p) in
            if f = "" || s = "" then None
            else
              let oc = string_of_int (get_occ p) in
              let u = pget conf base (get_father fam) in
              let n =
                let rec loop k =
                  if (get_family u).(k) = Adef.ifam_of_int i then
                    string_of_int k
                  else loop (k + 1)
                in
                loop 0
              in
              Some (f, s, oc, n)
        else None
    with Failure _ -> None
  in
  let env =
    let rec loop =
      function
        [] -> []
      | ("opt", "no_index") :: l -> loop l
      | (("dsrc" | "escache" | "oc" | "templ"), _) :: l -> loop l
      | ("i", v) :: l -> new_env "i" v (fun x -> x) l
      | ("ei", v) :: l -> new_env "ei" v (fun x -> "e" ^ x) l
      | (k, v) :: l when String.length k = 2 && k.[0] = 'i' ->
          let c = String.make 1 k.[1] in new_env k v (fun x -> x ^ c) l
      | (k, v) :: l when String.length k > 2 && k.[0] = 'e' && k.[1] = 'f' ->
          new_fam_env k v (fun x -> x ^ k) l
      | kv :: l -> kv :: loop l
    and new_env k v c l =
      match get_a_person v with
        Some (f, s, oc) ->
          if oc = "0" then (c "p", f) :: (c "n", s) :: loop l
          else (c "p", f) :: (c "n", s) :: (c "oc", oc) :: loop l
      | None -> (k, v) :: loop l
    and new_fam_env k v c l =
      match get_a_family v with
        Some (f, s, oc, n) ->
          let l = loop l in
          let l = if n = "0" then l else (c "f", n) :: l in
          if oc = "0" then (c "p", f) :: (c "n", s) :: l
          else (c "p", f) :: (c "n", s) :: (c "oc", oc) :: l
      | None -> (k, v) :: loop l
    in
    loop conf.env
  in
  let addr =
    let pref =
      let s = get_request_string conf.request in
      match String.rindex_opt s '?' with
        Some i -> String.sub s 0 i
      | None -> s
    in
    get_server_string conf.request ^ pref
  in
  let suff =
    List.fold_right
      (fun (x, v) s ->
         let sep = if s = "" then "" else "&" in x ^ "=" ^ v ^ sep ^ s)
      (("lang", conf.lang) :: env) ""
  in
  if conf.b_arg_for_basename then addr ^ "?b=" ^ conf.bname ^ "&" ^ suff
  else addr ^ "?" ^ suff

let message_to_wizard conf =
  if conf.wizard || conf.just_friend_wizard then
    let print_file fname =
      let fname = base_path ["etc"; conf.bname] (fname ^ ".txt") in
      try
        let ic = Secure.open_in fname in
        try while true do Wserver.printf "%c" (input_char ic) done
        with End_of_file -> close_in ic
      with Sys_error _ -> ()
    in
    print_file "mess_wizard";
    if conf.user <> "" then print_file ("mess_wizard_" ^ conf.user)

let doctype conf =
  match p_getenv conf.base_env "doctype" with
    Some "html-5" -> "<!DOCTYPE html>"
  | Some "html-4.01-trans" ->
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n\
       \"http://www.w3.org/TR/html4/loose.dtd\">"
  | Some "html-4.01" ->
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n\
       \"http://www.w3.org/TR/html4/strict.dtd\">"
  | Some "xhtml-1.0-trans" ->
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n\
       \"http://www.w3.org/TR/xhtml10/DTD/loose.dtd\">"
  | _ ->
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"\n\
       \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"

let http_string s i =
  let start_with s i p =
    i + String.length p <= String.length s &&
    String.lowercase_ascii (String.sub s i (String.length p)) = p
  in
  let http = "http://" in
  let https = "https://" in
  let (http, start_with_http) =
    if start_with s i http then http, true else https, start_with s i https
  in
  if start_with_http then
    let (j, par) =
      let rec loop j par =
        if j < String.length s then
          match s.[j] with
            'a'..'z' | 'A'..'Z' | '\128'..'\255' | '0'..'9' | '!' | '#' |
            '$' | '%' | '&' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' |
            ':' | ';' | '=' | '?' | '@' | '\\' | '_' | '~' ->
              if s.[j] = '(' then loop (j + 1) (par + 1)
              else if s.[j] = ')' then loop (j + 1) (par - 1)
              else loop (j + 1) par
          | '[' | '^' | '{' | '|' -> j + 1, par
          | ']' | '}' -> j, par
          | _ -> j, par
        else j, par
      in
      loop (i + String.length http) 0
    in
    let j =
      let rec loop j =
        match s.[j-1] with
          ')' | ',' | '.' | ':' | ';' ->
            if s.[j-1] = ')' && par = 0 then j
            else if s.[j-1] = ')' && par < 0 then j - 1
            else loop (j - 1)
        | _ -> j
      in
      loop j
    in
    let s = String.sub s i (j - i) in Some (s, j)
  else None

let rec followed_by_ident_semi s i =
  if i = String.length s then false
  else
    match s.[i] with
      'a'..'z' | 'A'..'Z' -> followed_by_ident_semi s (i + 1)
    | '#' | '0'..'9' -> followed_by_ident_semi s (i + 1)
    | ';' -> true
    | _ -> false

let expand_ampersand buff s =
  let rec loop i =
    if i = String.length s then ()
    else
      begin
        if s.[i] = '&' then Buffer.add_string buff "&amp;"
        else Buffer.add_char buff s.[i];
        loop (i + 1)
      end
  in
  loop 0

let email_addr s i =
  let rec before_at empty i =
    if i = String.length s then None
    else
      match s.[i] with
        'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' ->
          before_at false (i + 1)
      | '@' -> if empty then None else after_at true (i + 1)
      | _ -> None
  and after_at empty i =
    if i = String.length s then None
    else
      match s.[i] with
        'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> after_at false (i + 1)
      | '.' -> if empty then None else after_dot 0 (i + 1)
      | _ -> None
  and after_dot len i =
    if i = String.length s then Some (len, i)
    else
      match s.[i] with
        'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' ->
          after_dot (len + 1) (i + 1)
      | _ -> Some (len, i)
  in
  match before_at true i with
    Some (len, i) ->
      let (len, i) =
        if len > 0 && s.[i-1] = '.' then len - 1, i - 1 else len, i
      in
      if len = 0 then None else Some i
  | None -> None

let tag_id s i =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else
      match s.[i] with
        'a'..'z' | 'A'..'Z' | '0'..'9' | '!' | '-' ->
          loop (i + 1) (Buff.store len (Char.lowercase_ascii s.[i]))
      | _ -> if len = 0 then loop (i + 1) 0 else Buff.get len
  in
  loop i 0

let default_good_tag_list =
  "!--" :: List.map snd safe_html_allowd_tags

let allowed_tags_file = ref ""

let good_tag_list_fun () =
  if !allowed_tags_file <> "" then
    try
      let ic = open_in !allowed_tags_file in
      let rec loop tags =
        match input_line ic with
        | tg -> loop (String.lowercase_ascii tg :: tags)
        | exception End_of_file ->
          close_in ic;
          tags
      in
      loop []
    with Sys_error _ -> default_good_tag_list
  else default_good_tag_list

let good_tags_list = Lazy.from_fun good_tag_list_fun
let good_tag s i = List.mem (tag_id s i) (Lazy.force good_tags_list)

module Lbuff = Buff.Make (struct  end)

let filter_html_tags s =
  let rec loop len i =
    if i < String.length s then
      if s.[i] = '<' && not (good_tag s (i + 1)) then
        loop (Lbuff.mstore len "&lt;") (i + 1)
      else loop (Lbuff.store len s.[i]) (i + 1)
    else Lbuff.get len
  in
  loop 0 0

let get_variable s i =
  let rec loop len i =
    if i = String.length s then Buff.get len, [], i
    else
      match s.[i] with
        'a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c ->
          loop (Buff.store len c) (i + 1)
      | ':' ->
          let v = Buff.get len in
          let rec loop vl len i =
            if i = String.length s then v, List.rev (Buff.get len :: vl), i
            else
              match s.[i] with
                ':' -> loop (Buff.get len :: vl) 0 (i + 1)
              | ';' -> v, List.rev (Buff.get len :: vl), i + 1
              | c -> loop vl (Buff.store len c) (i + 1)
          in
          loop [] 0 (i + 1)
      | ';' -> Buff.get len, [], i + 1
      | _ -> Buff.get len, [], i
  in
  loop 0 i

type tag_type = In_a_href | In_norm | Out

let expand_env =
  let buff = Buffer.create 30 in
  fun conf s ->
    match p_getenv conf.base_env "expand_env" with
      Some "yes" ->
        let _ = (Buffer.clear buff : unit) in
        let rec loop i =
          if i = String.length s then Buffer.contents buff
          else if i + 1 < String.length s && s.[i] = '$' && s.[i+1] = '{' then
            try
              let j = String.index_from s (i + 1) '}' in
              let v = Sys.getenv (String.sub s (i + 2) (j - i - 2)) in
              Buffer.add_string buff v; loop (j + 1)
            with Not_found -> Buffer.add_char buff s.[i]; loop (i + 1)
          else begin Buffer.add_char buff s.[i]; loop (i + 1) end
        in
        loop 0
    | _ -> s

let string_with_macros conf env s =
  let start_with s i p =
    i + String.length p <= String.length s &&
    String.lowercase_ascii (String.sub s i (String.length p)) = p
  in
  let buff = Buffer.create 1000 in
  let rec loop tt i =
    if i < String.length s then
      if i + 1 < String.length s && s.[i] = '%' then
        let i =
          try Buffer.add_string buff (List.assoc s.[i+1] env ()); i + 2 with
            Not_found ->
              match s.[i+1] with
                's' -> Buffer.add_string buff (commd conf); i + 2
              | 'v' ->
                  let (k, vl, j) = get_variable s (i + 2) in
                  let (v, i) =
                    let v =
                      try
                        let v = List.assoc ("var_" ^ k) conf.base_env in
                        Some (expand_env conf v)
                      with Not_found -> None
                    in
                    match v with
                      Some s ->
                        let s =
                          let rec loop vl len i =
                            if i = String.length s then Buff.get len
                            else if
                              i + 1 < String.length s && s.[i] = '%' &&
                              s.[i+1] = 's'
                            then
                              match vl with
                                v :: vl -> loop vl (Buff.mstore len v) (i + 2)
                              | [] ->
                                  Buff.get len ^
                                  String.sub s i (String.length s - i)
                            else loop vl (Buff.store len s.[i]) (i + 1)
                          in
                          loop vl 0 0
                        in
                        s, j
                    | None -> "%", i + 1
                  in
                  Buffer.add_string buff v; i
              | '%' -> Buffer.add_string buff "%"; i + 2
              | _ -> Buffer.add_string buff "%"; i + 1
        in
        loop tt i
      else
        match tt with
          In_a_href ->
            let tt = if start_with s i "</a>" then Out else In_a_href in
            Buffer.add_char buff s.[i]; loop tt (i + 1)
        | In_norm ->
            let tt = if s.[i] = '>' then Out else In_norm in
            Buffer.add_char buff s.[i]; loop tt (i + 1)
        | Out ->
            match http_string s i with
              Some (x, j) ->
                Printf.bprintf buff "<a href=\"%s\">" x;
                expand_ampersand buff x;
                Printf.bprintf buff "</a>";
                loop Out j
            | None ->
                match email_addr s i with
                  Some j ->
                    let x = String.sub s i (j - i) in
                    Printf.bprintf buff "<a href=\"mailto:%s\">%s</a>" x x;
                    loop Out j
                | None ->
                    let tt =
                      if start_with s i "<a href=" ||
                         start_with s i "<a\nhref="
                      then
                        In_a_href
                      else if s.[i] = '<' then In_norm
                      else Out
                    in
                    if s.[i] = '&' && not (followed_by_ident_semi s (i + 1))
                    then
                      Buffer.add_string buff "&amp;"
                    else Buffer.add_char buff s.[i];
                    loop tt (i + 1)
    else filter_html_tags (Buffer.contents buff)
  in
  loop Out 0

let place_of_string conf place =
  match p_getenv conf.base_env "place" with
  | Some gwf_place ->
      let list = String.split_on_char ',' gwf_place in
      let list = List.map String.trim list in
      let list_p = String.split_on_char ',' place in
      let list_p = List.map String.trim list_p in
      let place =
        {other = ""; town = ""; township = ""; canton = ""; district = "";
         county = ""; region = ""; country = ""}
      in
      let place =
        let rec loop list list_p place =
          match list_p with
            [] -> place
          | x :: list_p ->
              match list with
                [] ->
                  let other = String.concat ", " (x :: list_p) in
                  let other = place.other ^ " " ^ other in
                  {place with other = other}
              | t :: list ->
                  let place =
                    match t with
                      "town" -> {place with town = x}
                    | "township" -> {place with township = x}
                    | "canton" -> {place with canton = x}
                    | "district" -> {place with district = x}
                    | "county" -> {place with county = x}
                    | "region" -> {place with region = x}
                    | "country" -> {place with country = x}
                    | _ ->
                        let other = place.other ^ " " ^ x in
                        {place with other = other}
                  in
                  loop list list_p place
        in
        loop list list_p place
      in
      Some place
  | None -> None

(* ********************************************************************** *)
(*  [Fonc] string_of_place : config -> string -> string                   *)
(** [Description] : Astuce temporaire pour supprimer les crochets dans
                    un lieu-dit. À l'avenir, il faudra revoir comment sont
                    implémentés les lieux.
    [Args] :
      - conf  : configuration de la base
      - place : lieu dont on veut supprimer les crochets
    [Retour] :
      - string : lieu sans les crochets du lieu-dit.
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let string_of_place conf place =
  List.fold_left (fun s c -> Name.strip_c s c)
    (string_with_macros conf [] place) ['['; ']']


type xhtml_tag =
    Btag of string * string
  | Etag of string
  | Atag of string

let tag_params_ok s =
  let rec loop i =
    if i = String.length s then true
    else
      match s.[i] with
        ' ' | '\n' -> loop (i + 1)
      | 'a'..'z' ->
          let rec loop_id i =
            if i = String.length s then false
            else
              match s.[i] with
                'a'..'z' -> loop_id (i + 1)
              | '=' ->
                  let i = i + 1 in
                  if i = String.length s then false
                  else if s.[i] = '"' then
                    let rec loop_str i =
                      if i = String.length s then false
                      else if s.[i] = '"' then loop (i + 1)
                      else loop_str (i + 1)
                    in
                    loop_str (i + 1)
                  else false
              | _ -> false
          in
          loop_id (i + 1)
      | _ -> false
  in
  loop 0

let xhtml_tag s i =
  if s.[i] = '<' then
    if i = String.length s - 1 then None
    else if s.[i+1] = '!' then None
    else
      let k =
        try String.index_from s i '>' with Not_found -> String.length s
      in
      let j =
        let rec loop i =
          if i = k then k
          else
            match s.[i] with
              ' ' | '\n' -> i
            | _ -> loop (i + 1)
        in
        loop i
      in
      if i + 1 = String.length s then None
      else
        let next_i = min (k + 1) (String.length s) in
        if s.[i+1] = '/' then
          let t = String.sub s (i + 2) (k - i - 2) in
          if j = k then Some (Etag t, next_i) else None
        else if s.[k-1] = '/' then
          let t = String.sub s (i + 1) (k - i - 2) in Some (Atag t, next_i)
        else
          let t = String.sub s (i + 1) (j - i - 1) in
          let a = String.sub s j (k - j) in
          if tag_params_ok a then Some (Btag (t, a), next_i) else None
  else None

let check_ampersand s i =
  if i = String.length s then Some ("&amp;", i)
  else
    match s.[i] with
      'a'..'z' ->
        let rec loop_id j =
          if j = String.length s then
            let a = Printf.sprintf "&amp;%s" (String.sub s i (j - i)) in Some (a, j)
          else
            match s.[j] with
              'a'..'z' -> loop_id (j + 1)
            | ';' -> None
            | _ ->
                let a = Printf.sprintf "&amp;%s" (String.sub s i (j - i)) in
                Some (a, j)
        in
        loop_id i
    | _ -> Some ("&amp;", i)

let bad col s = Printf.sprintf "<span style=\"color:%s\">%s</span>" col s

let check_ampersands s =
  let b = Buffer.create (String.length s) in
  let rec loop error i =
    if i = String.length s then
      if error then Some (Buffer.contents b) else None
    else
      match s.[i] with
        '&' ->
          begin match check_ampersand s (i + 1) with
            Some (txt, j) -> Buffer.add_string b (bad "red" txt); loop true j
          | None -> Buffer.add_char b '&'; loop error (i + 1)
          end
      | c -> Buffer.add_char b c; loop error (i + 1)
  in
  loop false 0

let check_xhtml s =
  let b = Buffer.create (String.length s) in
  let rec loop tag_stack i =
    if i = String.length s then
      begin
        List.iter
          (fun (pos, txt, _t) ->
             let s = Buffer.contents b in
             let s_bef = String.sub s 0 pos in
             let pos_aft = pos + String.length txt + 2 in
             let s_aft = String.sub s pos_aft (String.length s - pos_aft) in
             Buffer.clear b;
             Buffer.add_string b s_bef;
             Buffer.add_string b (bad "red" (Printf.sprintf "&lt;%s&gt;" txt));
             Buffer.add_string b s_aft)
          tag_stack;
        Buffer.contents b
      end
    else
      match xhtml_tag s i with
        Some (Btag (t, a), i) ->
          if t = "br" && a = "" then
            begin
              (* frequent error *)
              Buffer.add_string b (Printf.sprintf "<%s/>" t);
              loop tag_stack i
            end
          else
            begin match check_ampersands a with
              Some a ->
                Buffer.add_string b (Printf.sprintf "&lt;%s%s&gt;" t a);
                loop tag_stack i
            | None ->
                let pos = Buffer.length b in
                let txt = Printf.sprintf "%s%s" t a in
                Buffer.add_string b (Printf.sprintf "<%s>" txt);
                loop ((pos, txt, t) :: tag_stack) i
            end
      | Some (Etag t, i) ->
          begin match tag_stack with
            (_, _, bt) :: rest when t = bt ->
              Buffer.add_string b (Printf.sprintf "</%s>" t); loop rest i
          | _ ->
              Buffer.add_string b (bad "red" (Printf.sprintf "&lt;/%s&gt;" t));
              loop tag_stack i
          end
      | Some (Atag t, i) ->
          Buffer.add_string b (Printf.sprintf "<%s/>" t); loop tag_stack i
      | None ->
          if s.[i] = '&' then
            match check_ampersand s (i + 1) with
              Some (txt, j) ->
                Buffer.add_string b (bad "red" txt); loop tag_stack j
            | None -> Buffer.add_char b '&'; loop tag_stack (i + 1)
          else
            begin
              if s.[i] = '<' && (i + 1 = String.length s || s.[i+1] <> '!')
              then
                Buffer.add_string b (bad "red" "&lt;")
              else Buffer.add_char b s.[i];
              loop tag_stack (i + 1)
            end
  in
  loop [] 0

let menu_threshold = 20

let is_number t =
  match t.[0] with
    '1'..'9' -> true
  | _ -> false

let hexa_string s =
  let s' = Bytes.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    Bytes.set s' (2 * i) "0123456789ABCDEF".[Char.code s.[i] / 16];
    Bytes.set s' (2 * i + 1) "0123456789ABCDEF".[Char.code s.[i] mod 16]
  done;
  Bytes.unsafe_to_string s'

let print_alphab_list crit print_elem liste =
  let len = List.length liste in
  if len > menu_threshold then
    begin
      Wserver.printf "<p>\n";
      begin let _ =
        List.fold_left
          (fun last e ->
             let t = crit e in
             let same_than_last =
               match last with
                 Some t1 -> t = t1
               | _ -> false
             in
             if not same_than_last then
               Wserver.printf "<a href=\"#ai%s\">%s</a>\n" (hexa_string t) t;
             Some t)
          None liste
      in
        ()
      end;
      Wserver.printf "</p>\n"
    end;
  Wserver.printf "<ul>\n";
  begin let _ =
    List.fold_left
      (fun last e ->
         let t = crit e in
         let same_than_last =
           match last with
             Some t1 -> t = t1
           | _ -> false
         in
         if len > menu_threshold || is_number t then
           begin
             begin match last with
               Some _ ->
                 if not same_than_last then Wserver.printf "</ul>\n</li>\n"
             | _ -> ()
             end;
             if not same_than_last then
               begin
                 Wserver.printf "<li>\n";
                 Wserver.printf "<a id=\"ai%s\">%s</a>\n" (hexa_string t) t;
                 Wserver.printf "<ul>\n"
               end
           end;
         Wserver.printf "<li>\n  ";
         print_elem e;
         Wserver.printf "</li>\n";
         Some t)
      None liste
  in
    ()
  end;
  if len > menu_threshold then Wserver.printf "</ul>\n</li>\n";
  Wserver.printf "</ul>\n"


let relation_txt conf sex fam =
  let is = index_of_sex sex in
  match get_relation fam with
  | NotMarried
  | NoSexesCheckNotMarried ->
    ftransl_nth conf "relationship%t to" is
  | MarriageContract  ->
    ftransl_nth conf "marriage contract%t with" is
  | MarriageLicense
  | Married
  | NoSexesCheckMarried ->
    ftransl_nth conf "married%t to" is
  | Engaged ->
    ftransl_nth conf "engaged%t to" is
  | MarriageBann ->
    ftransl_nth conf "marriage banns%t to" is
  | Pacs ->
    ftransl_nth conf "pacsed%t to" is
  | Residence ->
    ftransl_nth conf "residence%t to" is
  | NoMention ->
    "%t" ^^ ftransl conf "with"

let relation_date conf fam =
  match Adef.od_of_cdate (get_marriage fam) with
    Some d ->
      begin match d with
        Dgreg (dmy, _) -> " " ^ ( transl conf "in (year)" ) ^ " " ^
          ( string_of_int dmy.year )
     | _ -> ""
      end
  | _ -> ""

(* ************************************************************************** *)
(*  [Fonc] child_of_parent : config -> base -> person -> unit                 *)
(** [Description] : Traduction selon l'existence des parents :
                      * fils/fille de Jean (et) Jeanne
    [Args] :
      - conf : configuration
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let child_of_parent conf base p =
  (* Si le père a un nom de famille différent de la personne *)
  (* alors on l'affiche, sinon on n'affiche que le prénom.   *)
  let print_father fath =
    if not (eq_istr (get_surname p) (get_surname fath)) then
      person_text conf base fath
    else gen_person_text (p_first_name, (fun _ _ -> "")) conf base fath
  in
  let a = pget conf base (get_key_index p) in
  let ifam =
    match get_parents a with
      Some ifam ->
        let cpl = foi base ifam in
        let fath =
          let fath = pget conf base (get_father cpl) in
          if p_first_name base fath = "?" then None else Some fath
        in
        let moth =
          let moth = pget conf base (get_mother cpl) in
          if p_first_name base moth = "?" then None else Some moth
        in
        Some (fath, moth)
    | None -> None
  in
  match ifam with
    Some (None, None) | None -> ""
  | Some (fath, moth) ->
      let s =
        match fath, moth with
          Some fath, None -> print_father fath
        | None, Some moth -> person_text conf base moth
        | Some fath, Some moth ->
            print_father fath ^ " " ^ transl_nth conf "and" 0 ^ " " ^
            person_text conf base moth
        | _ -> ""
      in
      let is = index_of_sex (get_sex p) in
      translate_eval
        (transl_a_of_gr_eq_gen_lev conf
           (transl_nth conf "son/daughter/child" is) s s)


(* ************************************************************************** *)
(*  [Fonc] husband_wife : config -> base -> person -> bool -> string                    *)
(** [Description] : Traduction selon l'existence du premier conjoint
                    différent de ?? :
                      * époux/épouse de Jean/Jeanne
    [Args] :
      - conf : configuration
      - base : base de donnée
      - p    : person
      - all  : if true, list all spouses
    [Retour] : string
    [Rem]    : Exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let husband_wife conf base p all =
  let relation =
    let rec loop i =
      if i < Array.length (get_family p) then
        let fam = foi base (get_family p).(i) in
        let conjoint = Gutil.spouse (get_key_index p) fam in
        let conjoint = pget conf base conjoint in
        if not @@ is_empty_name conjoint
        then
          translate_eval (Printf.sprintf (relation_txt conf (get_sex p) fam) (fun () -> ""))
        else loop (i + 1)
      else ""
    in
    loop 0
  in
  let res =
    let rec loop i res =
      if i < Array.length (get_family p) then
        let fam = foi base (get_family p).(i) in
        let conjoint = Gutil.spouse (get_key_index p) fam in
        let conjoint = pget conf base conjoint in
        if not @@ is_empty_name conjoint
        then
          if all then
            loop (i + 1) (res ^ translate_eval (" " ^
              (person_text conf base conjoint) ^ (relation_date conf fam)) ^ ",")
          else
            res ^ translate_eval (" " ^
              (person_text conf base conjoint) ^ (relation_date conf fam)) ^ ","
        else loop (i + 1) res
      else res
    in
    loop 0 relation
  in
  let res = if String.length res > 1
    then (String.sub res 0 (String.length res -1)) else res
  in
  res

(* ************************************************************************** *)
(*  [Fonc] first_child : config -> base -> person -> unit                     *)
(** [Description] : Traduction selon l'existence du premier enfants :
                      * père/mère de Jean
    [Args] :
      - conf : configuration
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let first_child conf base p =
  let is = index_of_sex (get_sex p) in
  let rec loop i =
    if i < Array.length (get_family p) then
      let fam = foi base (get_family p).(i) in
      let ct = get_children fam in
      if Array.length ct > 0 then
        let enfant = pget conf base ct.(0) in
        let child =
          if is_hide_names conf enfant &&
             not (authorized_age conf base enfant)
          then
            "xx"
          else if not (eq_istr (get_surname p) (get_surname enfant)) then
            person_text conf base enfant
          else
            gen_person_text (p_first_name, (fun _ _ -> "")) conf base enfant
        in
        translate_eval
          (transl_a_of_b conf (transl_nth conf "father/mother" is) child child)
      else loop (i + 1)
    else ""
  in
  loop 0


(* ************************************************************************** *)
(*  [Fonc] specify_homonymous : config -> base -> person -> bool -> string    *)
(** [Description] : Permet d'afficher des informations supplémentaires sur la
      personne en cas d'homonymes (par exemple sur la recherche par ordre
      alphabétique).
      L'affichage se fait de façon similaire à gen_person_text, i.e. en
      fonction du nom publique et sobriquet si on valorise le paramètre
      specify_public_name à True :
        * Louis VI le gros (nom publique sobriquet)
        * Louis le gros    (prénom sobriquet)
        * Louis VI         (nom publique)
        * Louis Capétiens, fils de Philippe et Berthe, marié avec Adèlaïde,
            père de Philippe
    [Args] :
      - conf : configuration
      - base : base de donnée
      - p    : person
      - specify_public_name : en fonction des affichages, on peut déjà avoir
          affiché le nom public de la personne, et dans ce cas, on ne veut pas
          l'afficher de nouveau.
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let specify_homonymous conf base p specify_public_name =
  match get_public_name p, get_qualifiers p with
    n, nn :: _ when sou base n <> "" && specify_public_name ->
      Wserver.printf " %s <em>%s</em>" (sou base n) (sou base nn)
  | _, nn :: _ when specify_public_name ->
      Wserver.printf " %s <em>%s</em>" (p_first_name base p) (sou base nn)
  | n, [] when sou base n <> "" && specify_public_name ->
      Wserver.printf " %s" (sou base n)
  | _, _ ->
      (* Le nom public et le qualificatif ne permettent pas de distinguer *)
      (* la personne, donc on affiche les informations sur les parents,   *)
      (* le mariage et/ou le premier enfant.                              *)
      let cop = child_of_parent conf base p in
      let hw = husband_wife conf base p true in
      let fc = first_child conf base p in
      let s =
        (if cop = "" then "" else ", " ^ cop) ^
        (if hw = "" then if fc = "" then "" else ", " ^ fc else ", " ^ hw) ^ "."
      in
      Wserver.printf "%s" s


(* ************************************************************************** *)
(*  [Fonc] get_approx_date_place :
             cdate -> string -> cdate -> string -> (cdate, string)         *)
(** [Description] : Renvoi la date et le lieu le mieux correspondant.
    [Args] :
      - d1   : date (naissance/décès)
      - p1   : lieu (naissance/décès)
      - d2   : date (baptème/inhumation)
      - p2   : lieu (baptème/inhumation)
    [Retour] : (cdate, string)
    [Rem] : None exporté en clair hors de ce module.                          *)
(* ************************************************************************** *)
let get_approx_date_place d1 p1 d2 p2 =
  match d1, p1, d2, p2 with
    Some d, "", None, y -> Some d, y
  | Some d, "", Some x, y -> if y = "" then Some d, "" else Some x, y
  | Some d, p, _, _ -> Some d, p
  | None, "", None, p -> None, p
  | None, "", Some x, y -> Some x, y
  | None, p, None, _ -> None, p
  | None, p, Some x, y -> if y = "" then Some x, p else Some x, y

let get_approx_birth_date_place conf base p =
  let birth = Adef.od_of_cdate (get_birth p) in
  let birth_place = string_of_place conf (sou base (get_birth_place p)) in
  let baptism = Adef.od_of_cdate (get_baptism p) in
  let baptism_place = string_of_place conf (sou base (get_baptism_place p)) in
  get_approx_date_place birth birth_place baptism baptism_place

let get_approx_death_date_place conf base p =
  let death = CheckItem.date_of_death (get_death p) in
  let death_place = string_of_place conf (sou base (get_death_place p)) in
  let buri =
    match get_burial p with
      Buried cd -> Adef.od_of_cdate cd
    | Cremated cd -> Adef.od_of_cdate cd
    | _ -> None
  in
  let buri_place = string_of_place conf (sou base (get_burial_place p)) in
  get_approx_date_place death death_place buri buri_place

let string_of_decimal_num conf f =
  let s = string_of_float f in
  let b = Buffer.create 20 in
  let rec loop i =
    if i = String.length s then Buffer.contents b
    else
      begin
        begin match s.[i] with
          '.' ->
            if i = String.length s - 1 then ()
            else Buffer.add_string b (transl conf "(decimal separator)")
        | x -> Buffer.add_char b x
        end;
        loop (i + 1)
      end
  in
  loop 0

let personal_image_file_name bname str =
  Filename.concat (base_path ["images"] bname) str

let source_image_file_name bname str =
  let fname1 =
    List.fold_right Filename.concat [base_path ["src"] bname; "images"] str
  in
  let fname2 =
    List.fold_right Filename.concat [Secure.base_dir (); "src"; "images"] str
  in
  if Sys.file_exists fname1 then fname1 else fname2

let image_file_name str =
  let fname1 =
    List.fold_right Filename.concat [Secure.base_dir (); "images"] str
  in
  if Sys.file_exists fname1 then fname1
  else search_in_lang_path (Filename.concat "images" str)

let png_image_size ic =
  let magic = really_input_string ic 4 in
  if magic = "\137PNG" then
    begin
      seek_in ic 16;
      let wid = input_binary_int ic in
      let hei = input_binary_int ic in Some (wid, hei)
    end
  else None

let gif_image_size ic =
  let magic = really_input_string ic 4 in
  if magic = "GIF8" then
    begin
      seek_in ic 6;
      let wid = let x = input_byte ic in input_byte ic * 256 + x in
      let hei = let x = input_byte ic in input_byte ic * 256 + x in
      Some (wid, hei)
    end
  else None

let jpeg_image_size ic =
  let magic = really_input_string ic 10 in
  if Char.code magic.[0] = 0xff && Char.code magic.[1] = 0xd8 &&
     (let m = String.sub magic 6 4 in m = "JFIF" || m = "Exif")
  then
    let exif_type = String.sub magic 6 4 = "Exif" in
    let rec loop found =
      while Char.code (input_char ic) <> 0xFF do () done;
      let ch =
        let rec loop ch =
          if Char.code ch = 0xFF then loop (input_char ic) else ch
        in
        loop (input_char ic)
      in
      if Char.code ch = 0xC0 || Char.code ch = 0xC3 then
        if exif_type && not found then loop true
        else
          begin
            for i = 1 to 3 do let _ = input_char ic in () done;
            let a = input_char ic in
            let b = input_char ic in
            let c = input_char ic in
            let d = input_char ic in
            let wid = Char.code c lsl 8 lor Char.code d in
            let hei = Char.code a lsl 8 lor Char.code b in Some (wid, hei)
          end
      else
        let a = input_char ic in
        let b = input_char ic in
        let len = Char.code a lsl 8 lor Char.code b in
        let len = if len >= 32768 then 0 else len in
        for i = 1 to len - 2 do let _ = input_char ic in () done;
        if Char.code ch <> 0xDA then loop found else None
    in
    loop false
  else None

let image_size fname =
  try
    let ic = Secure.open_in_bin fname in
    let r =
      try
        let sz = jpeg_image_size ic in
        let sz =
          if sz = None then begin seek_in ic 0; png_image_size ic end
          else sz
        in
        if sz = None then begin seek_in ic 0; gif_image_size ic end else sz
      with End_of_file -> None
    in
    close_in ic; r
  with Sys_error _ -> None

let limited_image_size max_wid max_hei fname size =
  match if fname = "" then size else image_size fname with
    Some (wid, hei) ->
      let (wid, hei) =
        if hei > max_hei then
          let wid = wid * max_hei / hei in let hei = max_hei in wid, hei
        else wid, hei
      in
      let (wid, hei) =
        if wid > max_wid then
          let hei = hei * max_wid / wid in let wid = max_wid in wid, hei
        else wid, hei
      in
      Some (wid, hei)
  | None -> None

let find_person_in_env conf base suff =
  match p_getint conf.env ("i" ^ suff) with
    Some i ->
      if i >= 0 && i < nb_of_persons base then
        let p = pget conf base (Adef.iper_of_int i) in
        if is_hidden p then None else Some p
      else None
  | None ->
      match
        p_getenv conf.env ("p" ^ suff), p_getenv conf.env ("n" ^ suff)
      with
        Some p, Some n ->
          let occ =
            match p_getint conf.env ("oc" ^ suff) with
              Some oc -> oc
            | None -> 0
          in
          begin match person_of_key base p n occ with
            Some ip ->
              let p = pget conf base ip in
              if is_hidden p then None
              else if
                not (is_hide_names conf p) || authorized_age conf base p
              then
                Some p
              else None
          | None -> None
          end
      | _ -> None

let person_exists conf base (fn, sn, oc) =
  match p_getenv conf.base_env "red_if_not_exist" with
    Some "off" -> true
  | Some _ | None ->
      match person_of_key base fn sn oc with
        Some ip -> authorized_age conf base (pget conf base ip)
      | None -> false

let default_sosa_ref conf base =
  match p_getenv conf.base_env "default_sosa_ref" with
    Some n ->
      if n = "" then None
      else
        begin match Gutil.person_ht_find_all base n with
          [ip] ->
            let p = pget conf base ip in if is_hidden p then None else Some p
        | _ -> None
        end
  | None -> None

let find_sosa_ref conf base =
  match find_person_in_env conf base "z" with
    Some p -> Some p
  | None -> default_sosa_ref conf base

let write_default_sosa conf key =
  let gwf = List.remove_assoc "default_sosa_ref" conf.base_env in
  let gwf = List.rev (("default_sosa_ref", key) :: gwf) in
  let fname = base_path [] (conf.bname ^ ".gwf") in
  let tmp_fname = fname ^ "2" in
  let oc =
    try Stdlib.open_out tmp_fname with
      Sys_error _ -> failwith "the gwf database is not writable"
  in
  List.iter (fun (k, v) -> Stdlib.output_string oc (k ^ "=" ^ v ^ "\n"))
    gwf;
  close_out oc;
  rm (fname ^ "~") ;
  Sys.rename fname (fname ^ "~") ;
  try Sys.rename tmp_fname fname with Sys_error _ -> ()

let update_gwf_sosa conf base (ip, (fn, sn, occ)) =
  let sosa_ref_key =
    match snd conf.default_sosa_ref with
      Some p ->
        p_first_name base p ^ "." ^ string_of_int (get_occ p) ^ " " ^
        p_surname base p
    | None -> ""
  in
  let new_key = fn ^ "." ^ string_of_int occ ^ " " ^ sn in
  if ip = fst conf.default_sosa_ref && new_key != sosa_ref_key then
    write_default_sosa conf new_key

let create_topological_sort conf base =
  match p_getenv conf.env "opt" with
    Some "no_tsfile" ->
      let () = load_ascends_array base in
      let () = load_couples_array base in
      Consang.topological_sort base (pget conf)
  | Some "no_tstab" -> Array.make (nb_of_persons base) 0
  | _ ->
      let bfile = base_path [] (conf.bname ^ ".gwb") in
      Lock.control (Mutil.lock_file bfile) false
        ~onerror:(fun () ->
            let () = load_ascends_array base in
            let () = load_couples_array base in
            Consang.topological_sort base (pget conf) )
        (fun () ->
           let tstab_file =
             if conf.use_restrict && not conf.wizard && not conf.friend then
               Filename.concat bfile "tstab_visitor"
             else Filename.concat bfile "tstab"
           in
           let r =
             try
               let ic = Secure.open_in_bin tstab_file in
               let r =
                 try Some (Marshal.from_channel ic)
                 with End_of_file | Failure _ -> None
               in
               close_in ic; r
             with Sys_error _ -> None
           in
           match r with
             Some tstab -> tstab
           | None ->
             let () = load_ascends_array base in
             let () = load_couples_array base in
             let tstab = Consang.topological_sort base (pget conf) in
             if conf.use_restrict && not conf.wizard && not conf.friend
             then
               base_visible_write base;
             begin
               try
                 let oc = Secure.open_out_bin tstab_file in
                 Marshal.to_channel oc tstab [Marshal.No_sharing];
                 close_out oc
               with Sys_error _ -> ()
             end;
             tstab)

let p_of_sosa conf base sosa p0 =
  let path = Sosa.branches sosa in
  let rec aux acc = function
    | [] -> Some acc
    | hd :: tl ->
      match get_parents acc with
      | Some ifam ->
        let cpl = foi base ifam in
        if hd = 0
        then aux (pget conf base (get_father cpl)) tl
        else aux (pget conf base (get_mother cpl)) tl
      | None -> None
  in
  aux p0 path

let branch_of_sosa conf base sosa p =
  if Sosa.eq sosa Sosa.zero then invalid_arg "branch_of_sosa";
  let rec expand bl sosa =
    if Sosa.eq sosa Sosa.one then bl
    else expand (Sosa.even sosa :: bl) (Sosa.half sosa)
  in
  let rec loop pl p = function
    | [] -> Some (p :: pl)
    | male :: tl ->
      match get_parents p with
      | Some ifam ->
        let cpl = foi base ifam in
        if male then loop (p :: pl) (pget conf base @@ get_father cpl) tl
        else loop (p :: pl) (pget conf base @@ get_mother cpl) tl
      | _ -> None
  in
  loop [] p (expand [] sosa)

let sosa_of_branch ipl =
  if ipl = [] then failwith "sosa_of_branch";
  let ipl = List.tl (List.rev ipl) in
  List.fold_left
    (fun b p ->
       let b = Sosa.twice b in
       match get_sex p with
       | Male -> b
       | Female -> Sosa.inc b 1
       | Neuter -> assert false)
    Sosa.one ipl

(* FIXME: remove this and use sosa_of_branch only *)
let old_sosa_of_branch conf base (ipl : (iper * sex) list) =
  sosa_of_branch (List.map (fun (ip, _) -> pget conf base ip) ipl)

(* FIXME: remove this and use branch_of_sosa only *)
let old_branch_of_sosa conf base ip sosa =
  branch_of_sosa conf base sosa (pget conf base ip)
  |> Opt.map @@ List.map (fun p -> get_key_index p, get_sex p)

let space_to_unders = Mutil.tr ' ' '_'


(* ************************************************************************** *)
(*  [Fonc] default_image_name_of_key : string -> string -> int -> string      *)
(** [Description] : Renvoie à partir de la clé d'une personne, le nom par
                    défaut de son image (portrait).
                    Par exemple, Jean Claude DUPOND 3 => jean_claude.3.dupond
    [Args] :
      - fnam : first name
      - snam : surname
      - occ  : occ
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                               *)
(* ************************************************************************** *)
let default_image_name_of_key fnam surn occ =
  let f = space_to_unders (Name.lower fnam) in
  let s = space_to_unders (Name.lower surn) in
  f ^ "." ^ string_of_int occ ^ "." ^ s


(* *********************************************************************** *)
(*  [Fonc] default_image_name : base -> person -> string                   *)
(** [Description] : Renvoie à partir d'une personne, le nom par défaut de
                    son image (portrait) => voir default_image_name_of_key.
    [Args] :
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                            *)
(* *********************************************************************** *)
let default_image_name base p =
  default_image_name_of_key (p_first_name base p) (p_surname base p)
    (get_occ p)

let auto_image_file conf base p =
  let s = default_image_name base p in
  let f = Filename.concat (base_path ["images"] conf.bname) s in
  if Sys.file_exists (f ^ ".gif") then Some (f ^ ".gif")
  else if Sys.file_exists (f ^ ".jpg") then Some (f ^ ".jpg")
  else if Sys.file_exists (f ^ ".png") then Some (f ^ ".png")
  else None

(* ********************************************************************** *)
(*  [Fonc] image_and_size : config -> base -> person -> image_size        *)
(** [Description] : Renvoie la source de l'image ainsi que sa taille.
    [Args] :
      - conf : configuration de la base
      - base : base de données
      - p    : personne
      [Retour] :
        - is_filename : indique si la source de l'image est un nom de
                        fichier ou une URL.
        - source
        - image_size
    [Rem] : Exporté en clair hors de ce module.                            *)
(* *********************************************************************** *)
let image_and_size conf base p image_size =
  if not conf.no_image && authorized_age conf base p then
    match sou base (get_image p) with
      "" ->
        begin match auto_image_file conf base p with
          Some f -> Some (true, f, image_size f None)
        | None -> None
        end
    | s ->
        let (s, size) =
          let l = String.length s - 1 in
          if s.[l] = ')' then
            try
              let pos1 = String.index s '(' in
              let pos2 = String.index_from s pos1 'x' in
              let wid = String.sub s (pos1 + 1) (pos2 - pos1 - 1) in
              let hei = String.sub s (pos2 + 1) (l - pos2 - 1) in
              let size = Some (int_of_string wid, int_of_string hei) in
              String.sub s 0 pos1, image_size "" size
            with Not_found | Failure _ -> s, None
          else s, None
        in
        let http = "http://" in
        let https = "https://" in
        if String.length s > String.length http &&
           String.sub s 0 (String.length http) = http ||
           String.length s > String.length https &&
           String.sub s 0 (String.length https) = https
        then
          Some (false, s, size)
        else if Filename.is_implicit s then
          match
            try Some (List.assoc "images_path" conf.base_env) with
              Not_found -> None
          with
            Some p when p <> "" -> Some (false, p ^ s, size)
          | _ ->
              let fname = personal_image_file_name conf.bname s in
              if Sys.file_exists fname then
                Some (true, fname, image_size fname None)
              else None
        else None
  else None

(* ********************************************************************** *)
(*  [Fonc] has_image : config -> base -> person -> bool                   *)
(** [Description] : Renvoie Vrai si la personne a une photo et qu'on a les
                    droits pour la voir, Faux sinon.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : Vrai si la personne a une image, Faux sinon.
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let has_image conf base p =
  if not conf.no_image && authorized_age conf base p then
    not (is_empty_string (get_image p)) &&
    (conf.wizard || conf.friend ||
     not (string_exists (sou base (get_image p)) "/private/")) ||
    auto_image_file conf base p <> None
  else false

let gen_only_printable or_nl s =
  let s' =
    let conv_char i =
      if Char.code s.[i] > 127 then s.[i]
      else
        match s.[i] with
          ' '..'~' | '\160'..'\255' -> s.[i]
        | '\n' -> if or_nl then '\n' else ' '
        | _ -> ' '
    in
    String.init (String.length s) conv_char
  in
  String.trim s'

let only_printable_or_nl = gen_only_printable true
let only_printable = gen_only_printable false

let relation_type_text conf t n =
  match t with
    Adoption ->
      transl_nth conf "adoptive father/adoptive mother/adoptive parents" n
  | Recognition ->
      transl_nth conf
        "recognizing father/recognizing mother/recognizing parents" n
  | CandidateParent ->
      transl_nth conf "candidate father/candidate mother/candidate parents" n
  | GodParent -> transl_nth conf "godfather/godmother/godparents" n
  | FosterParent ->
      transl_nth conf "foster father/foster mother/foster parents" n

let rchild_type_text conf t n =
  match t with
    Adoption ->
      transl_nth conf "adoptive son/adoptive daughter/adoptive child" n
  | Recognition ->
      transl_nth conf "recognized son/recognized daughter/recognized child" n
  | CandidateParent ->
      transl_nth conf "candidate son/candidate daughter/candidate child" n
  | GodParent -> transl_nth conf "godson/goddaughter/godchild" n
  | FosterParent ->
      transl_nth conf "foster son/foster daughter/foster child" n

let wprint_hidden conf pref name valu =
  Wserver.printf "<input type=\"hidden\" name=\"%s%s\" value=\"%s\"%s>\n" pref
    name (escape_html valu) conf.xhs

let wprint_hidden_person conf base pref p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if accessible_by_key conf base p first_name surname then
    begin
      wprint_hidden conf pref "p" (Name.lower first_name);
      wprint_hidden conf pref "n" (Name.lower surname);
      if get_occ p > 0 then
        wprint_hidden conf pref "oc" (string_of_int (get_occ p))
    end
  else
    wprint_hidden conf pref "i"
      (string_of_int (Adef.int_of_iper (get_key_index p)))

exception Ok

let has_nephews_or_nieces conf base p =
  try
    let a = p in
    match get_parents a with
      Some ifam ->
        let fam = foi base ifam in
        Array.iter
          (fun ip ->
             if ip = get_key_index p then ()
             else
               Array.iter
                 (fun ifam ->
                    if Array.length (get_children (foi base ifam)) > 0 then
                      raise Ok)
                 (get_family (pget conf base ip)))
          (get_children fam);
        false
    | _ -> false
  with Ok -> true

let h s = Digest.to_hex (Digest.string s)

let is_that_user_and_password auth_scheme user passwd =
  match auth_scheme with
    NoAuth -> false
  | TokenAuth ts -> user = ts.ts_user && passwd = ts.ts_pass
  | HttpAuth (Basic bs) -> user = bs.bs_user && passwd = bs.bs_pass
  | HttpAuth (Digest ds) ->
      if user <> ds.ds_username then false
      else
        let that_response_would_be =
          let a1 = Printf.sprintf "%s:%s:%s" user ds.ds_realm passwd in
          let a2 = Printf.sprintf "%s:%s" ds.ds_meth ds.ds_uri in
          if ds.ds_qop = "auth" || ds.ds_qop = "auth-int" then
            h
              (h a1 ^ ":" ^ ds.ds_nonce ^ ":" ^ ds.ds_nc ^ ":" ^
               ds.ds_cnonce ^ ":" ^ ds.ds_qop ^ ":" ^ h a2)
          else h (h a1 ^ ":" ^ ds.ds_nonce ^ ":" ^ h a2)
        in
        that_response_would_be = ds.ds_response

let browser_doesnt_have_tables conf =
  let user_agent = Wserver.extract_param "user-agent: " '/' conf.request in
  String.lowercase_ascii user_agent = "lynx"

(* Printing for browsers without tables *)

let pre_text_size txt =
  let rec normal len i =
    if i = String.length txt then len
    else if txt.[i] = '<' then in_tag len (i + 1)
    else if txt.[i] = '&' then in_char (len + 1) (i + 1)
    else normal (len + 1) (i + 1)
  and in_tag len i =
    if i = String.length txt then len
    else if txt.[i] = '>' then normal len (i + 1)
    else in_tag len (i + 1)
  and in_char len i =
    if i = String.length txt then len
    else if txt.[i] = ';' then normal len (i + 1)
    else in_char len (i + 1)
  in
  normal 0 0

let print_pre_center sz txt =
  for i = 1 to (sz - pre_text_size txt) / 2 do Wserver.printf " " done;
  Wserver.printf "%s\n" txt

let print_pre_left sz txt =
  let tsz = pre_text_size txt in
  if tsz < sz / 2 - 1 then
    for i = 2 to (sz / 2 - 1 - tsz) / 2 do Wserver.printf " " done;
  Wserver.printf " %s\n" txt

let print_pre_right sz txt =
  let tsz = pre_text_size txt in
  if tsz < sz / 2 - 1 then
    begin
      for i = 1 to sz / 2 do Wserver.printf " " done;
      for i = 1 to (sz / 2 - 1 - tsz) / 2 do Wserver.printf " " done;
      ()
    end
  else for i = 1 to sz - pre_text_size txt - 1 do Wserver.printf " " done;
  Wserver.printf " %s\n" txt

let of_course_died conf p =
  match Adef.od_of_cdate (get_birth p) with
    Some (Dgreg (d, _)) -> conf.today.year - d.year > 120
  | _ -> false


(* Hashtbl pour avoir le plus de flexibilité. *)
(* TODO : il faudrait que ce soit intégrer dans la base directement. *)
type cache_info_t = (string, string) Hashtbl.t

(* valeur dans le cache. *)
let cache_nb_base_persons = "cache_nb_persons"

(* Hashtbl utilisée tant qu'on a pas commit le patch. *)
let (ht_cache_info : cache_info_t) = Hashtbl.create 1


(* ************************************************************************ *)
(*  [Fonc] cache_info : config -> string                                    *)
(** [Description] : Renvoie le chemin du fichier de cache d'info de la base.
    [Args] :
      - config : configuration de la base
    [Retour] : unit
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let cache_info conf =
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  Filename.concat (base_path [] bname) "cache_info"


(* ************************************************************************ *)
(*  [Fonc] read_cache_info : config -> cache_info_t                         *)
(** [Description] : Lit le fichier de cache pour les infos d'une base.
    [Args] :
      - conf : configuration de la base
    [Retour] : Hashtbl cache_info_t
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let read_cache_info conf =
  let fname = cache_info conf in
  try
    let ic = Secure.open_in_bin fname in
    let ht : cache_info_t = input_value ic in close_in ic; ht
  with Sys_error _ -> ht_cache_info


(* ************************************************************************ *)
(*  [Fonc] write_cache_info : config -> cache_info_t -> unit                *)
(** [Description] : Met à jour le fichier de cache des infos d'une base.
    [Args] :
      - conf : configuration de la base
      - ht   : Hashtbl cache_info_t
    [Retour] : unit
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let write_cache_info conf =
  let ht_cache = read_cache_info conf in
  (* On met à jour la table en mémoire, avec toutes les valeurs qui *)
  (* sont dans le cache fichier mais pas dans celle en mémoire.     *)
  let () =
    Hashtbl.iter
      (fun k v ->
         if not (Hashtbl.mem ht_cache_info k) then
           Hashtbl.add ht_cache_info k v)
      ht_cache
  in
  let fname = cache_info conf in
  try
    let oc = Secure.open_out_bin fname in
    output_value oc ht_cache_info;
    close_out oc
  with Sys_error _ -> ()


(* ************************************************************************ *)
(*  [Fonc] patch_cache_info :
             config -> ((string * string) -> (string * string)) -> unit     *)
(** [Description] : Met à jour le fichier de cache.
    [Args] :
      - conf : configuration de la base
      - (key, value) : la clé et valeur de cache à mettre à jour
    [Retour] : unit
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let patch_cache_info conf k f =
  (* On met à jour le cache avec la nouvelle valeur. *)
  (* Si la clé n'existe pas dans la liste, c'est pas grave, elle sera *)
  (* ajouté lors de la création du cache.                             *)
  try
    let v = Hashtbl.find ht_cache_info k in
    Hashtbl.replace ht_cache_info k (f v)
  with Not_found ->
    try
      let cache_info = read_cache_info conf in
      let v = Hashtbl.find cache_info k in
      Hashtbl.replace ht_cache_info k (f v)
    with Not_found -> ()


let escache_value base =
  let t = Gwdb.date_of_last_change base in
  let v = int_of_float (mod_float t (float_of_int max_int)) in string_of_int v

let adm_file f = List.fold_right Filename.concat [!cnt_dir; "cnt"] f

let std_date conf =
  let (hour, min, sec) = conf.time in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" conf.today.year conf.today.month
    conf.today.day hour min sec

let read_wf_trace fname =
  try
    let ic = Secure.open_in fname in
    let rec loop acc =
      match input_line ic with
      | line -> loop (line :: acc)
      | exception End_of_file -> close_in ic ; List.rev acc
    in loop []
  with Sys_error _ -> []

let write_wf_trace fname wt =
  let oc = Secure.open_out fname in
  List.iter (fun (dt, u) -> Printf.fprintf oc "%s %s\n" dt u) wt; close_out oc

let update_wf_trace conf fname =
  let dt = std_date conf in
  let wt =
    let r = read_wf_trace fname in
    let dtlen = String.length dt in
    let rec loop found r =
      function
        x :: l ->
          if String.length x > dtlen + 2 then
            let u = String.sub x (dtlen + 1) (String.length x - dtlen - 1) in
            if u = conf.user then loop true ((dt, u) :: r) l
            else loop found ((String.sub x 0 dtlen, u) :: r) l
          else loop found r l
      | [] -> if found then r else (dt, conf.user) :: r
    in
    loop false [] r
  in
  write_wf_trace fname (List.sort (fun x y -> compare y x) wt)

let commit_patches conf base =
  Gwdb.commit_patches base;
  write_cache_info conf;
  conf.henv <-
    List.map
      (fun (k, v) -> if k = "escache" then k, escache_value base else k, v)
      conf.henv;
  if conf.user <> "" then
    let wpf =
      try List.assoc "wizard_passwd_file" conf.base_env with Not_found -> ""
    in
    if wpf <> "" then
      let fname = adm_file (conf.bname ^ "_u.txt") in
      update_wf_trace conf fname

let short_f_month m =
  match m with
    1 -> "VD"
  | 2 -> "BR"
  | 3 -> "FM"
  | 4 -> "NI"
  | 5 -> "PL"
  | 6 -> "VT"
  | 7 -> "GE"
  | 8 -> "FL"
  | 9 -> "PR"
  | 10 -> "ME"
  | 11 -> "TH"
  | 12 -> "FT"
  | 13 -> "JC"
  | _ -> ""

(* reading password file *)

type auth_user = { au_user : string; au_passwd : string; au_info : string }

let read_gen_auth_file fname =
  let fname = base_path [] fname in
  try
    let ic = Secure.open_in fname in
    let rec loop data =
      match input_line ic with
      | line ->
        let len = String.length line in
        let data =
          match String.index_opt line ':' with
          | Some i ->
            let user = String.sub line 0 i in
            let j = try String.index_from line (i + 1) ':' with Not_found -> len in
            let passwd = String.sub line (i + 1) (j - i - 1) in
            let rest =
              if j = len then ""
              else String.sub line (j + 1) (len - j - 1)
            in
            let au = {au_user = user; au_passwd = passwd; au_info = rest} in
            au :: data
          | None -> data
        in
        loop data
      | exception End_of_file ->
        close_in ic ; List.rev data
    in
    loop []
  with Sys_error _ -> []

let start_equiv_with case_sens s m i =
  let rec test i j =
    if j = String.length s then Some i
    else if i = String.length m then None
    else if case_sens then
      if m.[i] = s.[j] then test (i + 1) (j + 1) else None
    else
      match Name.next_chars_if_equiv m i s j with
        Some (i, j) -> test i j
      | None -> None
  in
  if case_sens then if m.[i] = s.[0] then test (i + 1) 1 else None
  else
    match Name.next_chars_if_equiv m i s 0 with
      Some (i, j) -> test i j
    | None -> None

let rec in_text case_sens s m =
  let rec loop in_tag i =
    if i = String.length m then false
    else if in_tag then loop (m.[i] <> '>') (i + 1)
    else if m.[i] = '<' then loop true (i + 1)
    else if m.[i] = '[' && i + 1 < String.length m && m.[i+1] = '[' then
      match NotesLinks.misc_notes_link m i with
        NotesLinks.WLpage (j, _, _, _, text) |
        NotesLinks.WLperson (j, _, text, _) |
        NotesLinks.WLwizard (j, _, text) ->
          if in_text case_sens s text then true else loop false j
      | NotesLinks.WLnone -> loop false (i + 1)
    else
      match start_equiv_with case_sens s m i with
        Some _ -> true
      | None -> loop false (i + 1)
  in
  loop false 0

let html_highlight case_sens h s =
  let ht i j =
    "<span class=\"found\">" ^ String.sub s i (j - i) ^ "</span>"
  in
  let rec loop in_tag i len =
    if i = String.length s then Buff.get len
    else if in_tag then loop (s.[i] <> '>') (i + 1) (Buff.store len s.[i])
    else if s.[i] = '<' then loop true (i + 1) (Buff.store len s.[i])
    else
      match start_equiv_with case_sens h s i with
        Some j -> loop false j (Buff.mstore len (ht i j))
      | None -> loop false (i + 1) (Buff.store len s.[i])
  in
  loop false 0 0

(* Wrapper to pretty print the produced XHTML (see Wserver.wrap_string) *)

let b = Buffer.create 80
let ind_bol = ref 0
let ind_curr = ref 0
let bol = ref false
let after_less = ref false
let after_slash = ref false
let curr_tag = ref None
let stack_in_error = ref false
let tag_stack = ref []
let check_tag_stack c =
  if !stack_in_error then ()
  else
    match !curr_tag with
      Some (tag1, topen, tag_found) ->
        begin match c with
          ' ' -> curr_tag := Some (tag1, topen, true)
        | '>' ->
            if topen then
              begin tag_stack := tag1 :: !tag_stack; curr_tag := None end
            else
              begin match !tag_stack with
                tag2 :: rest ->
                  if tag1 = tag2 then
                    begin tag_stack := rest; curr_tag := None end
                  else
                    begin
                      Printf.eprintf "Tag <%s> ended by </%s>\n" tag2 tag1;
                      stack_in_error := true
                    end
              | [] ->
                  Printf.eprintf "Ending tag not opened </%s>\n" tag1;
                  stack_in_error := true
              end
        | c ->
            if tag_found then ()
            else curr_tag := Some (tag1 ^ String.make 1 c, topen, false)
        end
    | None -> ()
let xml_pretty_print s =
  let rec loop i =
    if i = String.length s then ""
    else if !bol then
      if s.[i] = ' ' || s.[i] = '\n' then loop (i + 1)
      else begin bol := false; loop i end
    else
      match s.[i] with
        '\n' ->
          check_tag_stack ' ';
          let ind = min !ind_bol !ind_curr in
          let line = Buffer.contents b in
          bol := true;
          ind_bol := !ind_curr;
          Buffer.clear b;
          String.make (max 0 ind) ' ' ^ line ^ "\n" ^ loop (i + 1)
      | c ->
          let after_less_v = !after_less in
          let after_slash_v = !after_slash in
          after_less := false;
          after_slash := false;
          begin match c with
            '<' -> after_less := true; curr_tag := Some ("", true, false)
          | '/' ->
              if after_less_v then
                begin
                  ind_curr := !ind_curr - 2;
                  curr_tag := Some ("", false, false)
                end
              else after_slash := true
          | '!' -> curr_tag := None
          | '>' ->
              if after_slash_v then
                begin ind_curr := !ind_curr - 2; curr_tag := None end
              else check_tag_stack c
          | c ->
              check_tag_stack c;
              if after_less_v then ind_curr := !ind_curr + 2
          end;
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0

(* Print list in columns with Gutil.alphabetic order *)

type elem_kind = HeadElem | ContElem | Elem
let kind_size =
  function
    HeadElem | ContElem -> 4
  | Elem -> 1

let dispatch_in_columns ncol list order =
  let rlist =
    List.fold_left
      (fun rlist elem ->
         let ord = order elem in
         let kind =
           match rlist with
             (_, prev_ord, _prev_elem) :: _ ->
               if ord = prev_ord ||
                  ord <> "" && prev_ord <> "" && ord.[0] = prev_ord.[0]
               then
                 Elem
               else HeadElem
           | [] -> HeadElem
         in
         (ref kind, ord, elem) :: rlist)
      [] list
  in
  let (ini_list, ini_len) =
    List.fold_left
      (fun (list, len) (kind, _, _ as elem) ->
         elem :: list, len + kind_size !kind)
      ([], 0) rlist
  in
  let len_list =
    let rec loop rlen_list cnt col accu len list =
      if col > ncol then List.rev rlen_list
      else
        let (list, kind, is_last) =
          match list with
            (kind, _, _) :: list -> list, kind, false
          | [] -> [], ref Elem, true
        in
        let accu = accu + ncol * kind_size !kind in
        let cnt = cnt + 1 in
        if accu > len && not is_last && !kind = Elem then
          begin
            (* put a new size and restart from zero *)
            kind := ContElem;
            loop [] 0 1 0 (len + kind_size ContElem - 1) ini_list
          end
        else
          let (rlen_list, cnt, col, accu) =
            if accu > len && cnt > 1 then
              cnt - 1 :: rlen_list, 1, col + 1, accu - len
            else rlen_list, cnt, col, accu
          in
          loop rlen_list cnt col accu len list
    in
    loop [] 0 1 0 ini_len ini_list
  in
  len_list, ini_list

let print_in_columns conf ncols len_list list wprint_elem =
  begin_centered conf;
  Wserver.printf "<table width=\"95%%\" border=\"%d\">\n" conf.border;
  Wserver.printf "<tr align=\"%s\" valign=\"top\">\n" conf.left;
  begin let _ =
    List.fold_left
      (fun (list, _first) len ->
         let rec loop n list =
           if n = 0 then
             begin Wserver.printf "</ul>\n</td>\n"; list, false end
           else
             match list with
               (kind, ord, elem) :: list ->
                 if n = len then
                   Wserver.printf "<td width=\"%d\">\n" (100 / ncols)
                 else if !kind <> Elem then Wserver.printf "</ul>\n";
                 if !kind <> Elem then
                   begin
                     Wserver.printf "<h3 class=\"subtitle\">%s%s</h3>\n"
                       (if ord = "" then "..." else String.make 1 ord.[0])
                       (if !kind = HeadElem then ""
                        else " (" ^ transl conf "continued" ^ ")");
                     Wserver.printf "<ul>\n"
                   end;
                 Wserver.printf "<li>";
                 wprint_elem elem;
                 Wserver.printf "</li>\n";
                 loop (n - 1) list
             | [] -> [], false
         in
         loop len list)
      (list, true) len_list
  in
    ()
  end;
  Wserver.printf "</tr>\n";
  Wserver.printf "</table>\n";
  end_centered ()

let wprint_in_columns conf order wprint_elem list =
  let ncols =
    match p_getint conf.env "ncols" with
      Some n -> max 1 n
    | None ->
        let len_list = List.length list in
        if len_list < 10 then 1
        else if len_list < 100 then 2
        else if len_list < 200 then 3
        else 4
  in
  let (len_list, list) = dispatch_in_columns ncols list order in
  print_in_columns conf ncols len_list list wprint_elem


(* ********************************************************************** *)
(*  [Fonc] reduce_list : int -> list 'a -> list 'a                        *)
(** [Description] : Retourne la sous liste de taille size composée des
                    éléments 0 à (size - 1)
    [Args] :
      - size : la taille de la nouvelle liste
      - list : la liste originiale
    [Retour] :
      - list : la nouvelle liste de taille size
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let reduce_list size list =
  let rec loop size cnt reduced_list list =
    if cnt >= size then reduced_list
    else
      match list with
        [] -> reduced_list
      | x :: l -> loop size (cnt + 1) (x :: reduced_list) l
  in
  let sublist = loop size 0 [] list in List.rev sublist


(* ********************************************************************** *)
(*  [Fonc] print_reference : config -> string -> int -> string -> unit    *)
(** [Description] : Affiche la référence d'une personne
    [Args] :
      - conf : configuration de la base
      - fn   : first name
      - occ  : occ
      - sn   : surname
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let print_reference conf fn occ sn =
  Wserver.printf "<span class=\"reference\">";
  Wserver.printf " (%s %s.%d %s)" (transl conf "reference key")
    (Name.lower fn) occ (Name.lower sn);
  Wserver.printf "</span>"


(* ********************************************************************** *)
(*  [Fonc] gen_print_tips : conf -> string -> unit                        *)
(** [Description] : Affiche un tips.
    [Args] :
      - conf : configuration de la base
      - s    : le contenu du tips
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
let gen_print_tips conf s =
  Wserver.printf "<div class=\"tips\">\n";
  Wserver.printf "<table>\n";
  Wserver.printf "<tr>\n";
  Wserver.printf "<td>\n";
  Wserver.printf "%s" s;
  Wserver.printf "</td>\n";
  Wserver.printf "</tr>\n";
  Wserver.printf "</table>\n";
  Wserver.printf "</div>\n";
  Wserver.printf "<br%s>\n" conf.xhs

(* ********************************************************************** *)
(*  [Fonc] print_tips_relationship : conf -> unit                         *)
(** [Description] : Lors d'un calcul de parenté, il n'est pas évident de
      savoir qu'il faut cliquer sur la personne pour lancer le calcul.
      On affiche donc une petite aide pour l'utilisateur.
    [Args] :
      - conf : configuration de la base
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let print_tips_relationship conf =
  if p_getenv conf.env "em" = Some "R" || p_getenv conf.env "m" = Some "C"
  then
    let s = capitale (transl conf "select person to compute relationship") in
    gen_print_tips conf s


(* ********************************************************************** *)
(*  [Fonc] print_image_sex : conf -> person -> int -> unit                *)
(** [Description] : Affiche l'image du sexe correspondant à la personne.
    [Args] :
      - conf : configuration de la base
      - p    : person
      - size : taille de l'image
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let print_image_sex conf p size =
  let (image, alt) =
    match get_sex p with
      Male -> "male.png", "M"
    | Female -> "female.png", "F"
    | Neuter -> "sexunknown.png", "?"
  in
  Wserver.printf
    "<img src=\"%s/%s\" alt=\"%s\" title=\"sex\" width=\"%d\" heigth=\"%d\"%s>\n"
    (image_prefix conf) image alt size size conf.xhs


(* ********************************************************************** *)
(*  [Fonc] display_options : config -> string                             *)
(** [Description] : Recherche dans l'URL les options d'affichage qui sont
                    données et renvoie la concaténation de ces options.
    [Args] :
      - conf : configuration de la base
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
let display_options conf =
  let s =
    if p_getenv conf.env "image" = Some "off" then "&image=off"
    else ""
  in
  let s =
    if p_getenv conf.env "marriage" = Some "on" then s ^ "&marriage=on"
    else s
  in
  let s =
    match p_getenv conf.env "bd" with
      Some i -> s ^ "&bd=" ^ i
    | None -> s
  in
  match p_getenv conf.env "color" with
    Some c -> s ^ "&color=" ^ c
  | None -> s

(* Hashtbl qui associe un user à la liste des dernières personnes visitées. *)
(* On en profite aussi pour stocker la date de la dernière visite.          *)
type cache_visited_t = (string, (iper * string) list) Hashtbl.t


(* ************************************************************************ *)
(*  [Fonc] cache_visited : config -> string                                 *)
(** [Description] : Renvoie le chemin du fichier de cache.
    [Args] :
      - config : configuration de la base
    [Retour] : unit
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let cache_visited conf =
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  Filename.concat (base_path [] bname) "cache_visited"


(* ************************************************************************ *)
(*  [Fonc] read_visited : string -> cache_visited_t                         *)
(** [Description] : List le fichier de cache des dernières fiches visités.
    [Args] :
      - fname : le fichier de cache (qui se trouve dans base.gwb)
    [Retour] : Hashtbl des user => dernières visites
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let read_visited conf =
  let fname = cache_visited conf in
  try
    let ic = Secure.open_in_bin fname in
    let ht : cache_visited_t = input_value ic in
    close_in ic;
    ht
  with Sys_error _ -> Hashtbl.create 1


(* ************************************************************************ *)
(*  [Fonc] write_visited : string -> Hashtbl.t string (list iper) -> unit   *)
(** [Description] : Met à jour le fichier de cache des visites.
    [Args] :
      - fname : le fichier de cache (qui se trouve dans base.gwb)
      - ht    : le compteur de visite
    [Retour] : unit
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let write_visited conf ht =
  let fname = cache_visited conf in
  try
    let oc = Secure.open_out_bin fname in
    output_value oc ht;
    close_out oc
  with Sys_error _ -> ()


(* ************************************************************************ *)
(*  [Fonc] record_visited : config -> iper -> unit                          *)
(** [Description] : Vérifie si le user est ami ou magicien et met à jour
                    le fichier de cache.
    [Args] :
      - conf : configuration de la base
      - ip   : iper
    [Retour] : unit
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let record_visited conf ip =
  if conf.friend || conf.wizard then
    let ht = read_visited conf in
    let (hh, mm, ss) = conf.time in
    let time =
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" conf.today.year
        conf.today.month conf.today.day hh mm ss
    in
    let () =
      try
        let vl = Hashtbl.find ht conf.user in
        let vl = (ip, time) :: vl in
        (* On rend la liste unique sur les ip. *)
        let uniq =
          function
            [_] | [] as l -> l
          | (ip, _ as x) :: l ->
              let rec loop rl x =
                function
                  (ip2, _ as y) :: l ->
                    if ip = ip2 then loop rl x l else loop (x :: rl) y l
                | [] -> List.rev (x :: rl)
              in
              loop [] x l
        in
        let vl = uniq vl in
        let vl = reduce_list 10 vl in Hashtbl.replace ht conf.user vl
      with Not_found -> Hashtbl.add ht conf.user [ip, time]
    in
    write_visited conf ht


(**/**)


(* ************************************************************************ *)
(*  [Fonc] init_cache_info : config -> unit                                 *)
(** [Description] : Créé le fichier de cache.
    [Args] :
      - conf : configuration de la base
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let init_cache_info conf base =
  let nb_ind = Gwdb.nb_of_persons base in
  (* Reset le nombre réel de personnes d'une base. *)
  let nb_real_persons = ref 0 in
  for i = 0 to nb_ind - 1 do
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    if not @@ is_empty_name p then incr nb_real_persons else ()
  done;
  let () =
    Hashtbl.add ht_cache_info cache_nb_base_persons
      (string_of_int !nb_real_persons)
  in
  write_cache_info conf


(* ************************************************************************ *)
(*  [Fonc] real_nb_of_persons conf : config -> int                          *)
(** [Description] : Renvoie le nombre de personnes réelles (sans ? ?) d'une
                    base, à partir du fichier de cache.
    [Args] :
      - conf : configuration de la base
    [Retour] : nombre de personne sans ? ?
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let real_nb_of_persons conf base =
  let real_nb_person () =
    let ht = read_cache_info conf in
    let nb = Hashtbl.find ht cache_nb_base_persons in int_of_string nb
  in
  try real_nb_person () with
    Not_found ->
      try init_cache_info conf base; real_nb_person () with
        _ -> Gwdb.nb_of_persons base

let array_mem_witn conf base x a =
  let rec loop i =
    if i = Array.length a then (false, "")
    else if x = fst a.(i)
    then (true, string_of_witness_kind conf (get_sex @@ poi base x) (snd a.(i)))
    else loop (i + 1)
  in
  loop 0

let fprintf_date oc tm =
  Printf.fprintf oc "%4d-%02d-%02d %02d:%02d:%02d" (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let nb_char_occ c s =
  let cnt = ref 0 in
  String.iter (fun x -> if x = c then incr cnt) s ;
  !cnt

let rec filter_map fn = function
  | [] -> []
  | hd :: tl ->
    match fn hd with
    | Some x -> x :: filter_map fn tl
    | None -> filter_map fn tl

let rec rev_iter fn = function
  | [] -> ()
  | hd :: tl -> let () = rev_iter fn tl in fn hd

let groupby ~key ~value list =
  let h = Hashtbl.create (List.length list) in
  List.iter
    (fun x ->
       let k = key x in
       let v = value x in
       if Hashtbl.mem h k then Hashtbl.replace h k (v :: Hashtbl.find h k)
       else Hashtbl.add h k [v])
    list ;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []

let str_nth_pos str nth =
  let strlen = String.length str in
  let rec loop n i =
    if n = nth then i
    else if i >= strlen then raise (Invalid_argument "str_sub")
    else loop (n + 1) (index_of_next_char str i)
  in
  loop 0 0

let str_sub ?pad str start len =
  let strlen = String.length str in
  let n, i =
    let rec loop n i =
      if n = len || strlen = i then (n, i)
      else loop (n + 1) (index_of_next_char str i)
    in
    loop 0 start
  in
  if n = len then String.sub str start (i - start)
  else match pad with
    | None -> raise (Invalid_argument "str_sub")
    | Some pad ->
      let bytes = Bytes.make (i - start + len - n) pad in
      Bytes.blit (Bytes.unsafe_of_string str) start bytes 0 (String.length str) ;
      Bytes.unsafe_to_string bytes

let ls_r dirs =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
      Sys.readdir f
      |> Array.to_list
      |> List.rev_map (Filename.concat f)
      |> List.rev_append fs
      |> loop (f :: result)
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] dirs

let rm_rf dir =
  let (directories, files) = ls_r [dir] |> List.partition Sys.is_directory in
  List.iter Unix.unlink files ;
  List.iter Unix.rmdir directories

let init_cache_info bname base =
  match Gwdb.ascends_array base with
  | (_, _, _, None) ->
    begin
      (* Reset le nombre réel de personnes d'une base. *)
      let nb_real_persons = ref 0 in
      let nb_ind = Gwdb.nb_of_persons base in
      for i = 0 to nb_ind - 1 do
        let ip = Adef.iper_of_int i in
        let p = Gwdb.poi base ip in
        if is_empty_name p then () else incr nb_real_persons
      done;
      let ht = Hashtbl.create 1 in
      let () =
        Hashtbl.add ht cache_nb_base_persons (string_of_int !nb_real_persons)
      in
      let bdir =
        if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
      in
      let fname = Filename.concat bdir "cache_info" in
      match try Some (Secure.open_out_bin fname) with Sys_error _ -> None with
        Some oc -> output_value oc ht; close_out oc
      | None -> ()
    end
  | _ -> ()
