(* $Id: forum.ml,v 5.22 2008-01-07 13:29:47 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open TemplAst
open Util

type message =
  { m_time : string;
    m_date : date;
    m_hour : string;
    m_waiting : bool;
    m_from : string;
    m_ident : string;
    m_wizard : string;
    m_friend : string;
    m_email : string;
    m_access : string;
    m_subject : string;
    m_wiki : string;
    m_text : string }

module type MF =
  sig
    type in_chan
    type filename
    type pos
    val filename_of_string : string -> filename
    val open_in : filename -> in_chan
    val last_pos : in_chan -> pos
    val not_a_pos : pos
    val prev_pos : pos -> pos
    val next_pos : pos -> pos
    val string_of_pos : pos -> string
    val pos_of_string : string -> pos
    val input_char : in_chan -> char
    val input_line : in_chan -> string
    val rpos_in : in_chan -> pos
    val rseek_in : in_chan -> pos -> unit
    val close_in : in_chan -> unit
    val extend : filename -> (out_channel -> unit) -> unit
    val patch : filename -> pos -> string -> unit
  end

module MF : MF =
  struct
    type in_chan =
      { ic_fname : string;
        mutable ic_chan : in_channel;
        mutable ic_ext : int }
    type filename = string
    type pos =
      { mutable p_ord : bool; mutable p_ext : int; mutable p_pos : int }
    let filename_of_string x = x
    let last_pos ic =
      {p_ord = true; p_ext = 0; p_pos = in_channel_length ic.ic_chan}
    let not_a_pos = {p_ord = false; p_ext = 0; p_pos = -1}
    let prev_pos pos = {pos with p_pos = pos.p_pos - 1}
    let next_pos pos = {pos with p_pos = pos.p_pos + 1}
    let string_of_pos pos =
      if pos = not_a_pos then ""
      else if pos.p_ext = 0 then string_of_int pos.p_pos
      else string_of_int pos.p_ext ^ "-" ^ string_of_int pos.p_pos
    let pos_of_string s =
      try
        let pos = int_of_string s in
        if pos < 0 then not_a_pos else {p_ord = true; p_ext = 0; p_pos = pos}
      with Failure _ ->
        try
          Scanf.sscanf s "%d-%d"
            (fun a b -> {p_ord = a = 0; p_ext = a; p_pos = b})
        with Scanf.Scan_failure _ -> not_a_pos
    let extend fname f =
      let tmp = fname ^ "~" in
      let oc = open_out tmp in
      (try f oc with e -> close_out oc; raise e);
      begin match (try Some (open_in fname) with Sys_error _ -> None) with
        Some ic ->
          begin try while true do output_char oc (input_char ic) done with
            End_of_file -> ()
          end;
          close_in ic
      | None -> ()
      end;
      close_out oc;
      Util.rm fname ;
      Sys.rename tmp fname
    let patch fname pos str =
      let fname =
        if pos.p_ext = 0 then fname else fname ^ "." ^ string_of_int pos.p_ext
      in
      match try Some (open_in fname) with Sys_error _ -> None with
        Some ic ->
          let tmp_fname = fname ^ "~" in
          let oc = open_out tmp_fname in
          let ic_len = in_channel_length ic in
          begin let rec loop i =
            if i = ic_len then ()
            else
              let c = input_char ic in
              if i < ic_len - pos.p_pos ||
                 i >= ic_len - pos.p_pos + String.length str
              then
                output_char oc c
              else output_char oc str.[i - ic_len + pos.p_pos];
              loop (i + 1)
          in
            loop 0
          end;
          close_in ic;
          close_out oc;
          Util.rm fname ;
          Sys.rename tmp_fname fname
      | None -> ()
    let open_in fname =
      {ic_fname = fname; ic_chan = open_in_bin fname; ic_ext = 0}
    let input_char ic = input_char ic.ic_chan
    let rec input_line ic =
      try Pervasives.input_line ic.ic_chan with
        End_of_file ->
          let ext = ic.ic_ext + 1 in
          let fn = ic.ic_fname ^ "." ^ string_of_int ext in
          let ic2 =
            try open_in_bin fn with Sys_error _ -> raise End_of_file
          in
          close_in ic.ic_chan;
          ic.ic_chan <- ic2;
          ic.ic_ext <- ext;
          input_line ic
    let rpos_in ic =
      let pos = in_channel_length ic.ic_chan - pos_in ic.ic_chan in
      {p_ord = ic.ic_ext = 0; p_ext = ic.ic_ext; p_pos = pos}
    let rec rseek_in ic pos =
      if ic.ic_ext = pos.p_ext then
        let len = in_channel_length ic.ic_chan in
        if pos.p_pos > len then
          if pos.p_ext >= 1 then
            let ext = ic.ic_ext - 1 in
            pos.p_ord <- ext = 0;
            pos.p_ext <- ext;
            pos.p_pos <- pos.p_pos - len;
            rseek_in ic pos
          else invalid_arg "rseek_in"
        else seek_in ic.ic_chan (len - pos.p_pos)
      else
        let fn =
          if pos.p_ext = 0 then ic.ic_fname
          else ic.ic_fname ^ "." ^ string_of_int pos.p_ext
        in
        let ic2 =
          try open_in_bin fn with Sys_error _ -> failwith "rseek_in"
        in
        close_in ic.ic_chan;
        ic.ic_chan <- ic2;
        ic.ic_ext <- pos.p_ext;
        rseek_in ic pos
    let close_in ic = close_in ic.ic_chan
  end

let forum_file conf =
  let fn = Filename.concat (base_path [] (conf.bname ^ ".gwb")) "forum" in
  MF.filename_of_string fn

(* Black list *)

let match_strings regexp s =
  let rec loop i j =
    if i = String.length regexp && j = String.length s then true
    else if i = String.length regexp then false
    else if j = String.length s then false
    else if regexp.[i] = s.[j] then loop (i + 1) (j + 1)
    else if regexp.[i] = '*' then
      if i + 1 = String.length regexp then true
      else if regexp.[i+1] = s.[j] then loop (i + 2) (j + 1)
      else loop i (j + 1)
    else false
  in
  loop 0 0

let can_post conf =
  try
    let fname = List.assoc "forum_exclude_file" conf.base_env in
    let fname = Util.base_path [] fname in
    let ic = open_in fname in
    let rec loop () =
      match try Some (input_line ic) with End_of_file -> None with
        Some line ->
          if match_strings line conf.from then begin close_in ic; false end
          else loop ()
      | None -> close_in ic; true
    in
    loop ()
  with Not_found | Sys_error _ -> true

(* Print headers *)

let get_var ic lab s =
  let len = String.length lab in
  if String.length s >= len && String.sub s 0 len = lab then
    let start =
      if String.length s > len && s.[len] = ' ' then len + 1 else len
    in
    String.sub s start (String.length s - start), MF.input_line ic
  else "", s

let size_of_char s i = max 1 (Name.nbc s.[i])

let string_length s i =
  let rec loop i =
    if i >= String.length s then 0
    else let size = size_of_char s i in size + loop (i + size)
  in
  loop i

let sp2nbsp lim s =
  let trunc_signature = "..." in
  let signature_length = string_length trunc_signature 0 in
  let rec loop i len lim =
    if i >= String.length s || s.[i] = '\n' then Buff.get len
    else if lim <= 0 && string_length s i > signature_length then
      Buff.get len ^ trunc_signature
    else
      let size = size_of_char s i in
      let len =
        match s.[i] with
          ' ' -> Buff.mstore len "&nbsp;"
        | '&' -> Buff.mstore len "&amp;"
        | _ -> Buff.mstore len (String.sub s i size)
      in
      loop (i + size) len (lim - 1)
  in
  loop 0 0 lim

(* Print a message *)

let read_message conf ic =
  try
    let s = MF.input_line ic in
    let (time, s) = get_var ic "Time:" s in
    let ((time, s), deleted) =
      if time = "" then get_var ic "****:" s, true else (time, s), false
    in
    let (date, hour) =
      try
        let i = String.index time ' ' in
        String.sub time 0 i,
        String.sub time (i + 1) (String.length time - i - 1)
      with Not_found -> "", time
    in
    let date =
      try
        let y = int_of_string (String.sub date 0 4) in
        let m = int_of_string (String.sub date 5 2) in
        let d = int_of_string (String.sub date 8 2) in
        Dgreg
          ({year = y; month = m; day = d; prec = Sure; delta = 0}, Dgregorian)
      with Failure _ | Invalid_argument _ -> Dtext date
    in
    let (moderator, s) = get_var ic "Moderator:" s in
    let (from, s) = get_var ic "From:" s in
    let (ident, s) = get_var ic "Ident:" s in
    let (wizard, s) = get_var ic "Wizard:" s in
    let (friend, s) = get_var ic "Friend:" s in
    let (email, s) = get_var ic "Email:" s in
    let (access, s) = get_var ic "Access:" s in
    let (subject, s) = get_var ic "Subject:" s in
    let (wiki, s) = get_var ic "Wiki:" s in
    let (_, s) = get_var ic "Text:" s in
    let mess =
      let rec get_mess len s =
        if String.length s >= 2 && s.[0] = ' ' && s.[1] = ' ' then
          let s = String.sub s 2 (String.length s - 2) in
          let len = if len = 0 then len else Buff.store len '\n' in
          get_mess (Buff.mstore len s) (MF.input_line ic)
        else Buff.get len
      in
      get_mess 0 s
    in
    let waiting = String.length moderator > 0 && moderator.[0] = '.' in
    let mess =
      {m_time = time; m_waiting = waiting; m_from = from; m_date = date;
       m_hour = hour; m_ident = ident; m_wizard = wizard; m_friend = friend;
       m_email = email; m_access = access; m_subject = subject; m_wiki = wiki;
       m_text = mess}
    in
    let accessible =
      if deleted then false
      else if access <> "publ" && not conf.wizard && not conf.friend then
        false
      else true
    in
    Some (mess, accessible)
  with End_of_file -> None

let get_message conf pos =
  let fname = forum_file conf in
  match try Some (MF.open_in fname) with Sys_error _ -> None with
    Some ic ->
      MF.rseek_in ic pos;
      let r =
        match read_message conf ic with
          Some (m, accessible) -> Some (accessible, m, pos, MF.rpos_in ic)
        | None -> None
      in
      MF.close_in ic; r
  | None -> None

let backward_pos conf pos =
  let fname = forum_file conf in
  match try Some (MF.open_in fname) with Sys_error _ -> None with
    Some ic ->
      let sync_txt = "\nTime: " in
      let sync_txt_last = String.length sync_txt - 1 in
      let last_pos = MF.last_pos ic in
      let new_pos =
        let rec loop new_pos i =
          let new_pos = MF.next_pos new_pos in
          if new_pos = last_pos && i = 1 then new_pos
          else if new_pos < last_pos then
            begin
              MF.rseek_in ic new_pos;
              let c = MF.input_char ic in
              if c = sync_txt.[i] then
                if i = 0 then MF.prev_pos new_pos else loop new_pos (i - 1)
              else loop new_pos sync_txt_last
            end
          else pos
        in
        loop pos sync_txt_last
      in
      MF.close_in ic; new_pos
  | None -> pos

let passwd_in_file conf kind =
  match p_getenv conf.base_env (kind ^ "_passwd_file") with
    Some "" | None -> false
  | Some _ -> true

type 'a env =
    Vmess of message * message option * MF.pos * MF.pos * string option
  | Vpos of MF.pos ref
  | Vother of 'a
  | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let moderators conf =
  match p_getenv conf.base_env "moderator_file" with
    None | Some "" -> []
  | Some fname ->
      let fname = Util.base_path [] fname in
      match try Some (Secure.open_in fname) with Sys_error _ -> None with
        Some ic ->
          let list =
            let rec loop list =
              match try Some (input_line ic) with End_of_file -> None with
                Some line -> loop (line :: list)
              | None -> List.rev list
            in
            loop []
          in
          close_in ic; list
      | None -> []

let is_moderator conf = conf.wizard && List.mem conf.user (moderators conf)

let is_visible conf mess =
  not mess.m_waiting || is_moderator conf ||
  conf.wizard && mess.m_wizard <> "" && mess.m_wizard = conf.user

let rec eval_var conf base env _xx _loc =
  function
    ["can_post"] -> VVbool (can_post conf)
  | ["is_moderated_forum"] -> VVbool (moderators conf <> [])
  | ["is_moderator"] -> VVbool (is_moderator conf)
  | "message" :: sl -> eval_message_var conf base env sl
  | ["pos"] ->
      begin match get_env "pos" env with
        Vpos r -> VVstring (MF.string_of_pos !r)
      | _ -> raise Not_found
      end
  | _ -> raise Not_found
and eval_message_var conf base env =
  function
    ["access"] ->
      begin match get_env "mess" env with
        Vmess (mess, _, _, _, _) -> VVstring mess.m_access
      | _ -> raise Not_found
      end
  | "date" :: sl ->
      begin match get_env "mess" env with
        Vmess (mess, _, _, _, _) -> eval_date_var conf mess.m_date sl
      | _ -> raise Not_found
      end
  | "email" :: sl ->
      begin match get_env "mess" env with
        Vmess (mess, _, _, _, so) ->
          eval_message_string_var conf mess.m_email so sl
      | _ -> raise Not_found
      end
  | ["friend"] ->
      if passwd_in_file conf "friend" then
        match get_env "mess" env with
          Vmess (mess, _, _, _, _) -> VVstring mess.m_friend
        | _ -> raise Not_found
      else VVstring ""
  | ["from"] ->
      begin match get_env "mess" env with
        Vmess (mess, _, _, _, _) -> VVstring mess.m_from
      | _ -> raise Not_found
      end
  | ["hour"] ->
      begin match get_env "mess" env with
        Vmess (mess, _, _, _, _) -> VVstring mess.m_hour
      | _ -> raise Not_found
      end
  | "ident" :: sl ->
      begin match get_env "mess" env with
        Vmess (mess, _, _, _, so) ->
          eval_message_string_var conf mess.m_ident so sl
      | _ -> raise Not_found
      end
  | ["is_waiting"] ->
      begin match get_env "mess" env with
        Vmess (mess, _, _, _, _) -> VVbool mess.m_waiting
      | _ -> raise Not_found
      end
  | ["next_pos"] ->
      begin match get_env "mess" env with
        Vmess (_, _, pos, _, _) ->
          let rec loop pos =
            let back_pos = backward_pos conf pos in
            match get_message conf back_pos with
              Some (acc, mess, _, _) ->
                if back_pos = pos then VVstring ""
                else if acc && is_visible conf mess then
                  VVstring (MF.string_of_pos back_pos)
                else loop back_pos
            | None -> VVstring ""
          in
          loop pos
      | _ -> raise Not_found
      end
  | ["pos"] ->
      begin match get_env "mess" env with
        Vmess (_, _, pos, _, _) -> VVstring (MF.string_of_pos pos)
      | _ -> raise Not_found
      end
  | "prev_date" :: sl ->
      begin match get_env "mess" env with
        Vmess (_, prev_mess, _, _, _) ->
          begin match prev_mess with
            Some mess -> eval_date_var conf mess.m_date sl
          | None -> VVstring ""
          end
      | _ -> raise Not_found
      end
  | ["prev_pos"] ->
      begin match get_env "mess" env with
        Vmess (_, _, _, next_pos, _) ->
          let rec loop next_pos =
            match get_message conf next_pos with
              Some (acc, mess, next_pos, next_next_pos) ->
                if acc && is_visible conf mess then
                  VVstring (MF.string_of_pos next_pos)
                else loop next_next_pos
            | None -> VVstring ""
          in
          loop next_pos
      | _ -> raise Not_found
      end
  | "subject" :: sl ->
      begin match get_env "mess" env with
        Vmess (m, _, _, _, so) ->
          eval_message_string_var conf m.m_subject so sl
      | _ -> raise Not_found
      end
  | "text" :: sl ->
      begin match get_env "mess" env with
        Vmess (m, _, _, _, so) ->
          eval_message_text_var conf base m.m_text so sl
      | _ -> raise Not_found
      end
  | "time" :: sl ->
      begin match get_env "mess" env with
        Vmess (m, _, _, _, so) ->
          eval_message_text_var conf base m.m_time so sl
      | _ -> raise Not_found
      end
  | ["wiki"] ->
      begin match get_env "mess" env with
        Vmess (mess, _, _, _, _) -> VVstring mess.m_wiki
      | _ -> raise Not_found
      end
  | ["wizard"] ->
      if passwd_in_file conf "wizard" then
        match get_env "mess" env with
          Vmess (mess, _, _, _, _) -> VVstring mess.m_wizard
        | _ -> raise Not_found
      else VVstring ""
  | _ -> raise Not_found
and eval_date_var conf date =
  function
    ["month"] ->
      begin match date with
        Dgreg (d, _) -> VVstring (string_of_int d.month)
      | _ -> VVstring ""
      end
  | [] -> VVstring (Util.translate_eval (Date.string_of_date conf date))
  | _ -> raise Not_found
and eval_message_text_var conf base str so =
  function
    ["wiki"] ->
      let s = string_with_macros conf [] str in
      let lines = Wiki.html_of_tlsw conf s in
      let s = String.concat "\n" lines in
      let s =
        let wi =
          {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
           Wiki.wi_file_path = Notes.file_path conf base;
           Wiki.wi_person_exists = person_exists conf base;
           Wiki.wi_always_show_link = conf.wizard || conf.friend}
        in
        Wiki.syntax_links conf wi s
      in
      let s =
        match so with
          Some h ->
            let case_sens = p_getenv conf.env "c" = Some "on" in
            html_highlight case_sens h s
        | None -> s
      in
      VVstring s
  | ["nowiki"] ->
      let s = string_with_macros conf [] str in
      let s =
        match so with
          Some h ->
            let case_sens = p_getenv conf.env "c" = Some "on" in
            html_highlight case_sens h s
        | None -> s
      in
      VVstring s
  | ["raw"] -> VVstring (quote_escaped str)
  | sl -> eval_message_string_var conf str so sl
and eval_message_string_var conf str so =
  function
    ["cut"; s] ->
      begin try VVstring (no_html_tags (sp2nbsp (int_of_string s) str)) with
        Failure _ -> raise Not_found
      end
  | ["v"] -> VVstring (quote_escaped str)
  | [] ->
      let s = quote_escaped str in
      let s =
        match so with
          Some h ->
            let case_sens = p_getenv conf.env "c" = Some "on" in
            html_highlight case_sens h s
        | None -> s
      in
      VVstring s
  | _ -> raise Not_found

let print_foreach conf _base print_ast eval_expr =
  let eval_int_expr env e =
    let s = eval_expr env () e in
    try int_of_string s with Failure _ -> raise Not_found
  in
  let rec print_foreach env _xx _loc s sl el al =
    match s :: sl with
      ["message"] -> print_foreach_message env el al
    | _ -> raise Not_found
  and print_foreach_message env el al =
    let eval_pos_expr env e = MF.pos_of_string (eval_expr env () e) in
    let (to_pos, max_mess) =
      match el with
        [[e1]; [e2]] -> eval_pos_expr env e1, eval_int_expr env e2
      | _ -> raise Not_found
    in
    let fname = forum_file conf in
    match try Some (MF.open_in fname) with Sys_error _ -> None with
      Some ic ->
        let rec loop prev_mess i =
          if i >= max_mess then MF.rpos_in ic
          else
            let pos = MF.rpos_in ic in
            match read_message conf ic with
              Some (mess, accessible) ->
                if accessible && is_visible conf mess then
                  let next_pos = MF.rpos_in ic in
                  let vmess = Vmess (mess, prev_mess, pos, next_pos, None) in
                  let env = ("mess", vmess) :: env in
                  List.iter (print_ast env ()) al; loop (Some mess) (i + 1)
                else loop prev_mess i
            | None -> MF.not_a_pos
        in
        if to_pos = MF.not_a_pos then ()
        else (try MF.rseek_in ic to_pos with Sys_error _ -> ());
        let pos = loop None 0 in
        begin match get_env "pos" env with
          Vpos r -> r := pos
        | _ -> ()
        end;
        MF.close_in ic
    | None -> ()
  in
  print_foreach

let print_forum_message conf base r so =
  let env =
    match r with
      Some (acc, mess, pos, next_pos) ->
        if acc && is_visible conf mess then
          ["mess", Vmess (mess, None, pos, next_pos, so);
           "pos", Vpos (ref pos)]
        else ["pos", Vpos (ref MF.not_a_pos)]
    | None -> ["pos", Vpos (ref MF.not_a_pos)]
  in
  Hutil.interp conf "forum"
    {Templ.eval_var = eval_var conf base;
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = print_foreach conf base}
    env ()

let print conf base =
  let r =
    match p_getenv conf.env "p" with
      Some pos -> get_message conf (MF.pos_of_string pos)
    | None -> None
  in
  print_forum_message conf base r None

let print_forum_headers conf base =
  let env = ["pos", Vpos (ref MF.not_a_pos)] in
  Hutil.interp conf "forum"
    {Templ.eval_var = eval_var conf base;
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = print_foreach conf base}
    env ()

(* Send a message *)

let print_add conf base = print conf base

let get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get1 conf key =
  only_printable_or_nl (Mutil.strip_all_trailing_spaces (get conf key))

let forum_add conf _base moderated mess =
  let access =
    if conf.wizard || conf.friend then
      match p_getenv conf.env "priv_acc" with
        Some _ -> "priv"
      | None -> "publ"
    else "publ"
  in
  if mess.m_ident <> "" && mess.m_text <> "" then
    MF.extend (forum_file conf)
      (fun oc ->
         let (hh, mm, ss) = conf.time in
         Printf.fprintf oc "Time: %04d-%02d-%02d %02d:%02d:%02d\n" conf.today.year
           conf.today.month conf.today.day hh mm ss;
         if moderated then Printf.fprintf oc "Moderator: ....................\n";
         Printf.fprintf oc "From: %s\n" conf.from;
         Printf.fprintf oc "Ident: %s\n" mess.m_ident;
         if (conf.wizard || conf.just_friend_wizard) && conf.user <> "" then
           Printf.fprintf oc "Wizard: %s\n" conf.user;
         if conf.friend && not conf.just_friend_wizard && conf.user <> "" then
           Printf.fprintf oc "Friend: %s\n" conf.user;
         if mess.m_email <> "" then Printf.fprintf oc "Email: %s\n" mess.m_email;
         Printf.fprintf oc "Access: %s\n" access;
         let subject = if mess.m_subject = "" then "-" else mess.m_subject in
         Printf.fprintf oc "Subject: %s\n" subject;
         Printf.fprintf oc "Wiki: on\n";
         Printf.fprintf oc "Text:\n";
         let txt = mess.m_text in
         let rec loop i bol =
           if i = String.length txt then ()
           else
             begin
               if bol then Printf.fprintf oc "  ";
               if txt.[i] <> '\r' then output_char oc txt.[i];
               loop (i + 1) (txt.[i] = '\n')
             end
         in
         loop 0 true; Printf.fprintf oc "\n\n")

let visualize conf base mess =
  let vmess = Vmess (mess, None, MF.not_a_pos, MF.not_a_pos, None) in
  let env = ["mess", vmess] in
  Hutil.interp conf "forum"
    {Templ.eval_var = eval_var conf base;
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = print_foreach conf base}
    env ()

let print_add_ok conf base =
  let mess =
    let time =
      let (hh, mm, ss) = conf.time in
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" conf.today.year conf.today.month
        conf.today.day hh mm ss
    in
    let ident = String.trim (get conf "Ident") in
    let email = String.trim (get conf "Email") in
    let subject = String.trim (get conf "Subject") in
    let text = Gutil.trim_trailing_spaces (get1 conf "Text") in
    {m_time = time; m_date = Dtext ""; m_hour = ""; m_waiting = false;
     m_from = ""; m_ident = ident; m_wizard = ""; m_friend = "";
     m_email = email; m_access = ""; m_subject = subject; m_wiki = "";
     m_text = text}
  in
  if not (can_post conf) then Hutil.incorrect_request conf
  else if
    match p_getenv conf.env "visu" with
      Some _ -> true
    | None -> false
  then
    visualize conf base mess
  else if mess.m_ident = "" || mess.m_text = "" then print conf base
  else
    let title _ =
      Wserver.printf "%s" (capitale (transl conf "message added"))
    in
    try
      let mods = moderators conf in
      forum_add conf base (mods <> []) mess;
      Hutil.header conf title;
      Hutil.print_link_to_welcome conf true;
      if mods <> [] then
        Wserver.printf "<p>%s. %s.</p>"
          (capitale (transl conf "this forum is moderated"))
          (capitale (transl conf "your message is waiting for validation"));
      Wserver.printf "<a href=\"%sm=FORUM\">%s</a>\n" (commd conf)
        (capitale (transl conf "database forum"));
      Hutil.trailer conf
    with Update.ModErr -> ()

(* Deleting a message *)

let message_txt conf n =
  transl_nth conf "message/previous message/previous messages/next message" n

let forum_del conf pos =
  let fname = forum_file conf in MF.patch fname pos "****"

let print_del_ok conf next_pos =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "message deleted"))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  begin match next_pos with
    Some pos ->
      Wserver.printf "<a href=\"%sm=FORUM&p=%s\">%s</a>\n" (commd conf)
        (MF.string_of_pos pos) (capitale (message_txt conf 3))
  | None ->
      Wserver.printf "<a href=\"%sm=FORUM\">%s</a>\n" (commd conf)
        (capitale (transl conf "database forum"))
  end;
  Hutil.trailer conf

let find_next_pos conf =
  let rec loop pos =
    let back_pos = backward_pos conf pos in
    match get_message conf back_pos with
      Some (acc, _, _, _) ->
        if back_pos = pos then None
        else if acc then Some back_pos
        else loop back_pos
    | None -> None
  in
  loop

let delete_forum_message conf base pos =
  match get_message conf pos with
    Some (a, m, _, _) ->
      if a && conf.wizard && conf.user <> "" && m.m_wizard = conf.user &&
         passwd_in_file conf "wizard" ||
         conf.manitou || conf.supervisor
      then
        try
          forum_del conf pos;
          print_del_ok conf (find_next_pos conf pos)
        with Update.ModErr -> ()
      else print_forum_headers conf base
  | None -> print_forum_headers conf base

let print_del conf base =
  match p_getenv conf.env "p" with
    Some pos -> delete_forum_message conf base (MF.pos_of_string pos)
  | None -> print_forum_headers conf base

(* validate *)

let set_validator conf pos =
  let fname = forum_file conf in
  match try Some (MF.open_in fname) with Sys_error _ -> None with
    Some ic ->
      MF.rseek_in ic pos;
      let _ = MF.input_line ic in
      let pos = MF.rpos_in ic in
      let s = MF.input_line ic in
      let (moderator, _) = get_var ic "Moderator:" s in
      MF.close_in ic;
      if moderator <> "" && moderator.[0] = '.' then
        let m =
          let len = String.length moderator in
          if String.length conf.user < len - 1 then conf.user
          else String.sub conf.user 0 (len - 1)
        in
        MF.patch fname pos (Printf.sprintf "Moderator: /%s" m); true
      else false
  | None -> false

let message_txt conf n =
  transl_nth conf "message/previous message/previous messages/next message" n

let print_valid_ok conf pos del =
  let mess =
    if del then transl conf "message deleted" else transl conf "message added"
  in
  let title _ = Wserver.printf "%s" (capitale mess) in
  let next_pos = find_next_pos conf pos in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  begin match next_pos with
    Some pos ->
      Wserver.printf "<a href=\"%sm=FORUM&p=%s\">%s</a>\n" (commd conf)
        (MF.string_of_pos pos) (capitale (message_txt conf 3))
  | None ->
      Wserver.printf "<a href=\"%sm=FORUM\">%s</a>\n" (commd conf)
        (capitale (transl conf "database forum"))
  end;
  Hutil.trailer conf

let valid_forum_message conf base pos =
  match get_message conf pos with
    Some (a, _,  _, _) ->
      if a && conf.wizard && List.mem conf.user (moderators conf) then
        let del =
          match p_getenv conf.env "d" with
            Some "" | None -> false
          | Some _ -> true
        in
        if set_validator conf pos then
          begin
            if del then forum_del conf pos;
            print_valid_ok conf pos del
          end
        else print_forum_headers conf base
      else print_forum_headers conf base
  | None -> print_forum_headers conf base

let print_valid conf base =
  match p_getenv conf.env "p" with
    Some pos -> valid_forum_message conf base (MF.pos_of_string pos)
  | None -> print_forum_headers conf base

(* access switch *)

let set_access conf pos =
  let rec get_access ic =
    let pos = MF.rpos_in ic in
    let s = MF.input_line ic in
    let (access, _) = get_var ic "Access:" s in
    if access = "" then get_access ic else access, pos
  in
  let fname = forum_file conf in
  match try Some (MF.open_in fname) with Sys_error _ -> None with
    Some ic ->
      MF.rseek_in ic pos;
      let (access, pos) = get_access ic in
      MF.close_in ic;
      if access = "publ" || access = "priv" then
        let new_access =
          match access with
            "publ" -> "priv"
          | _ -> "publ"
        in
        MF.patch fname pos (Printf.sprintf "Access: %s" new_access); true
      else false
  | None -> false

let access_switch_forum_message conf base pos =
  match get_message conf pos with
    Some (a, m, _, _) ->
      if (a && conf.wizard && conf.user <> "" && m.m_wizard = conf.user &&
          passwd_in_file conf "wizard" ||
          conf.manitou || conf.supervisor) &&
         set_access conf pos
      then
        print_forum_message conf base (get_message conf pos) None
      else print_forum_headers conf base
  | None -> print_forum_headers conf base

let print_access_switch conf base =
  match p_getenv conf.env "p" with
    Some pos -> access_switch_forum_message conf base (MF.pos_of_string pos)
  | None -> print_forum_headers conf base

(* searching *)

let search_text conf base s =
  let s = if s = "" then " " else s in
  let fname = forum_file conf in
  match try Some (MF.open_in fname) with Sys_error _ -> None with
    Some ic ->
      let case_sens = p_getenv conf.env "c" = Some "on" in
      let rec loop () =
        let pos = MF.rpos_in ic in
        match read_message conf ic with
          Some (m, accessible) ->
            if accessible &&
               List.exists (in_text case_sens s)
                 [m.m_ident; m.m_subject; m.m_time; m.m_text]
            then
              Some (m, pos)
            else loop ()
        | None -> None
      in
      begin match p_getenv conf.env "p" with
        Some pos ->
          let pos = MF.pos_of_string pos in
          (try MF.rseek_in ic pos with Sys_error _ -> ());
          let _ = read_message conf ic in ()
      | None -> ()
      end;
      let messo = loop () in
      let next_pos = MF.rpos_in ic in
      MF.close_in ic;
      begin match messo with
        Some (mess, pos) ->
          let r = Some (true, mess, pos, next_pos) in
          print_forum_message conf base r (Some s)
      | None -> print_forum_headers conf base
      end
  | None -> print_forum_headers conf base

let print_search conf base =
  match try Some (List.assoc "s" conf.env) with Not_found -> None with
    Some s -> search_text conf base (Wserver.gen_decode false s)
  | None -> print_forum_headers conf base
