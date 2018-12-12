(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Dbdisk

let gwtp_tmp = ref (Filename.concat ".." "gwtp_tmp")
let gwtp_dst = ref (Filename.concat ".." "gwtp_dst")
let gwtp_log = ref ""
let gwtp_etc = ref ""
let gw_site = ref ""
let no_upload = ref false
let token_tmout = ref 900.0

let filename_basename str =
  let rec loop i =
    if i < 0 then str
    else
      match str.[i] with
        'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '~' | '.' -> loop (i - 1)
      | _ -> String.sub str (i + 1) (String.length str - i - 1)
  in
  loop (String.length str - 1)

(* Get CGI contents *)

let read_input len =
  if len >= 0 then really_input_string stdin len
  else
    let buff = Buffer.create 0 in
    begin try
      while true do let l = input_line stdin in Buffer.add_string buff l done
    with End_of_file -> ()
    end;
    Buffer.contents buff

let cgi_content_type () = try Sys.getenv "CONTENT_TYPE" with Not_found -> ""

let cgi_script_name () =
  try filename_basename (Sys.getenv "SCRIPT_NAME") with Not_found -> "gwtp"

let cgi_content () =
  let is_post =
    try Sys.getenv "REQUEST_METHOD" = "POST" with Not_found -> false
  in
  if is_post then
    let len =
      try int_of_string (Sys.getenv "CONTENT_LENGTH") with Not_found -> -1
    in
    set_binary_mode_in stdin true; read_input len
  else try Sys.getenv "QUERY_STRING" with Not_found -> ""

let cgi_from () =
  try Sys.getenv "REMOTE_HOST" with
    Not_found -> try Sys.getenv "REMOTE_ADDR" with Not_found -> ""

(* Utilitaires *)

let crlf () =
  flush stdout;
  let _ = (Unix.write_substring Unix.stdout "\013\n" 0 2 : int) in ()

let lowercase_start_with s s_ini =
  let len = String.length s_ini in
  String.length s >= len &&
  String.lowercase_ascii (String.sub s 0 len) = s_ini

let log_open () =
  let fname = Filename.concat !gwtp_log "gwtp.log" in
  open_out_gen [Open_wronly; Open_creat; Open_append] 0o644 fname

type env_val =
    Val of string
  | Fun of (unit -> unit)

let macro env c =
  match try Some (List.assoc c env) with Not_found -> None with
    Some (Val s) -> s
  | _ -> "%" ^ String.make 1 c

let get_variable ic =
  let rec loop len =
    match input_char ic with
      ';' -> Buff.get len
    | c -> loop (Buff.store len c)
  in
  loop 0

let get_binding ic =
  let rec loop len =
    match input_char ic with
      '=' -> let k = Buff.get len in k, get_variable ic
    | c -> loop (Buff.store len c)
  in
  loop 0

let template_fname fname =
  List.fold_right Filename.concat [!gwtp_etc; "lang"] (fname ^ ".txt")

let lindex s c =
  let rec pos i =
    if i = String.length s then None
    else if s.[i] = c then Some i
    else pos (i + 1)
  in
  pos 0

let input_lexicon lang =
  let ht = Hashtbl.create 501 in
  Mutil.input_lexicon lang ht
    (fun () ->
       open_in
         (List.fold_right Filename.concat [!gwtp_etc; "lang"] "lexicon.txt"));
  ht

let unfreeze_lexicon =
  let lexicon = ref None in
  fun lang ->
    match !lexicon with
      Some lex -> lex
    | None -> let lex = input_lexicon lang in lexicon := Some lex; lex

let transl lang w =
  let lexicon = unfreeze_lexicon lang in
  try Hashtbl.find lexicon w with Not_found -> "[" ^ w ^ "]"

let copy_template genv (varenv, filenv) env if_env fname =
  let lang =
    match HttpEnv.getenv genv "lang" with
      Some x -> x
    | _ -> "en"
  in
  let echo = ref true in
  let (push_echo, pop_echo) =
    let stack = ref [] in
    (fun x -> stack := !echo :: !stack; echo := x),
    (fun () ->
       match !stack with
         x :: l -> stack := l; echo := x
       | [] -> echo := true)
  in
  let ic = open_in (template_fname fname) in
  let rec if_expr =
    function
      'N' -> not (if_expr (input_char ic))
    | c ->
        try List.assoc c if_env with
          Not_found -> Printf.printf "!!!!!%c!!!!!" c; true
  in
  begin try
    while true do
      match input_char ic with
        '%' ->
          begin match input_char ic with
            'I' -> push_echo (!echo && if_expr (input_char ic))
          | 'E' -> pop_echo ()
          | _ when not !echo -> ()
          | 's' -> print_string (cgi_script_name ())
          | 'c' | 'e' as x ->
              let (v, k) = get_binding ic in
              begin try
                if k = List.assoc v varenv then
                  print_string (if x = 'c' then " checked" else " selected")
              with Not_found -> ()
              end
          | 'v' ->
              let v = get_variable ic in
              begin try
                print_string (Util.escape_html (List.assoc v varenv))
              with Not_found -> ()
              end
          | 'f' ->
              let v = get_variable ic in
              begin try
                print_string (Util.escape_html (List.assoc v filenv))
              with Not_found -> ()
              end
          | 'l' -> print_string lang
          | 'L' ->
              let v = get_variable ic in
              let lang_def = transl lang " !languages" in
              print_string (Translate.language_name v lang_def)
          | c ->
              match try Some (List.assoc c env) with Not_found -> None with
                Some (Val s) -> print_string s
              | Some (Fun f) -> f ()
              | None -> print_char '%'; print_char c
          end
      | '[' ->
          let s =
            let c = input_char ic in
            let s =
              let rec loop len c =
                if c = ']' then Buff.get len
                else loop (Buff.store len c) (input_char ic)
              in
              loop 0 (if c = '\n' then input_char ic else c)
            in
            if c = '\n' then
              let (s, alt) = Translate.inline lang '%' (macro env) s in
              if alt then "[" ^ s ^ "]" else s
            else transl lang s
          in
          if !echo then print_string s
      | c -> if !echo then print_char c
    done
  with End_of_file -> ()
  end;
  close_in ic

let variables () =
  let ic = open_in (template_fname "conf") in
  let vlist = ref [] in
  let flist = ref [] in
  begin try
    while true do
      match input_char ic with
        '%' ->
          begin match input_char ic with
            'e' | 'c' ->
              let (v, _) = get_binding ic in
              if not (List.mem v !vlist) then vlist := v :: !vlist
          | 'v' ->
              let v = get_variable ic in
              if not (List.mem v !vlist) then vlist := v :: !vlist
          | 'f' ->
              let v = get_variable ic in
              if not (List.mem v !vlist) then flist := v :: !flist
          | _ -> ()
          end
      | _ -> ()
    done
  with End_of_file -> ()
  end;
  close_in ic;
  !vlist, !flist

let sys_copy src dst =
  let ic = open_in src in
  let oc = open_out dst in
  begin try while true do let c = input_char ic in output_char oc c done with
    End_of_file -> ()
  end;
  close_out oc;
  close_in ic

let remove_dir_contents dir =
  let dh = Unix.opendir dir in
  try
    while true do
      match Unix.readdir dh with
        "." | ".." -> ()
      | f -> Unix.unlink (Filename.concat dir f)
    done
  with End_of_file -> Unix.closedir dh

(*
value html_escaped s =
  let s = String.escaped s in
  let rec loop i len =
    if i = String.length s then Buff.get len
    else
      let len =
        match s.[i] with
        [ '<' -> Buff.mstore len "&lt;"
        | '>' -> Buff.mstore len "&gt;"
        | x -> Buff.store len x ]
      in
      loop (i + 1) len
  in
  loop 0 0
;
*)

let gwtp_error txt =
  Printf.printf "content-type: text/html";
  crlf ();
  crlf ();
  Printf.printf "<head><title>Error</title></head>\n\
          <body>\n\
          <h1><font color=red>Error</font></h1>\n\
          %s\n\
          </body>"
    (String.capitalize_ascii txt)

let gwtp_invalid_request () = gwtp_error "Invalid request"

let random_self_init () =
  let seed = int_of_float (mod_float (Unix.time ()) (float max_int)) in
  let seed = seed + 256 * Unix.getpid () in Random.init seed

let _ = random_self_init ()

let mk_passwd size =
  let rec loop len =
    if len = size then Buff.get len
    else
      let r = Random.int (26 + 26 + 10) in
      let v =
        if r < 26 then Char.code 'a' + r
        else if r < 52 then Char.code 'A' + r - 26
        else Char.code '0' + r - 52
      in
      loop (Buff.store len (Char.chr v))
  in
  loop 0

(* Base configuration *)

let get_base_conf b =
  let fname = Filename.concat !gwtp_dst (b ^ ".gwf") in
  match try Some (open_in fname) with Sys_error _ -> None with
    Some ic ->
      let (variables, files) = variables () in
      let varenv =
        let varenv = ref [] in
        let rec record line =
          function
            v :: l ->
              if lowercase_start_with line (v ^ "=") then
                let len = String.length v + 1 in
                let x = String.sub line len (String.length line - len) in
                varenv := (v, x) :: !varenv
              else record line l
          | [] -> ()
        in
        begin try
          while true do
            let line =
              let line = input_line ic in
              if String.length line > 0 &&
                 line.[String.length line - 1] = '\r'
              then
                String.sub line 0 (String.length line - 1)
              else line
            in
            record line variables
          done
        with End_of_file -> close_in ic
        end;
        !varenv
      in
      let filenv =
        List.map
          (fun fsuff ->
             let fname =
               List.fold_right Filename.concat [!gwtp_dst; "lang"]
                 (b ^ "." ^ fsuff)
             in
             match try Some (open_in fname) with Sys_error _ -> None with
               Some ic ->
                 let len = ref 0 in
                 begin try
                   while true do len := Buff.store !len (input_char ic) done
                 with End_of_file -> close_in ic
                 end;
                 fsuff, Buff.get !len
             | None -> fsuff, "")
          files
      in
      varenv, filenv
  | None -> ["friend_passwd", mk_passwd 9; "wizard_passwd", mk_passwd 9], []

let set_base_conf b varenv =
  let fname = Filename.concat !gwtp_dst (b ^ ".gwf") in
  let fname_out = Filename.concat !gwtp_dst (b ^ "1.gwf") in
  let fname_saved = fname ^ "~" in
  let varenv = List.map (fun (k, v) -> k, v, ref false) varenv in
  let rec extract line =
    function
      (k, v, is_set) :: varenv ->
        if lowercase_start_with line (k ^ "=") then
          begin is_set := true; k ^ "=" ^ v end
        else extract line varenv
    | [] -> line
  in
  let oc = open_out fname_out in
  let ic_opt =
    try Some (open_in fname) with
      Sys_error _ ->
        let fname = Filename.concat !gwtp_etc "default.gwf" in
        try Some (open_in fname) with Sys_error _ -> None
  in
  begin match ic_opt with
    Some ic ->
      begin try
        while true do
          let line = input_line ic in
          let line_out = extract line varenv in Printf.fprintf oc "%s\n" line_out
        done
      with End_of_file -> close_in ic
      end
  | None -> ()
  end;
  List.iter
    (fun (k, v, is_set) ->
       if not !is_set && v <> "" then Printf.fprintf oc "%s=%s\n" k v)
    varenv;
  close_out oc;
  (try Sys.remove fname_saved with Sys_error _ -> ());
  let bdir = Filename.concat !gwtp_dst (b ^ ".gwb") in
  let b_ex = Sys.file_exists bdir in
  if b_ex then Sys.rename bdir (bdir ^ "~");
  if Sys.file_exists fname then Sys.rename fname fname_saved;
  Sys.rename fname_out fname;
  if b_ex then Sys.rename (bdir ^ "~") bdir

let set_base_files b filenv =
  List.iter
    (fun (k, v) ->
       let fname =
         List.fold_right Filename.concat [!gwtp_dst; "lang"] (b ^ "." ^ k)
       in
       if v = "" then try Sys.remove fname with Sys_error _ -> ()
       else let oc = open_out fname in output_string oc v; close_out oc)
    filenv

(* Login and tokens *)

let tokens_file_name () = Filename.concat !gwtp_tmp "token"

let read_tokens fname =
  match try Some (open_in fname) with Sys_error _ -> None with
    Some ic ->
      let rec loop list =
        match try Some (input_line ic) with End_of_file -> None with
          Some line ->
            let i = String.index line ' ' in
            let j = String.index_from line (i + 1) ' ' in
            let tm = float_of_string (String.sub line 0 i) in
            let b = String.sub line (i + 1) (j - i - 1) in
            let tok = String.sub line (j + 1) (String.length line - j - 1) in
            loop ((tm, b, tok) :: list)
        | None -> close_in ic; List.rev list
      in
      loop []
  | None -> []

let write_tokens fname tokens =
  let oc = open_out fname in
  List.iter (fun (tm, from_b, tok) -> Printf.fprintf oc "%.0f %s %s\n" tm from_b tok)
    tokens;
  close_out oc

let check_login b p =
  let line1 = b ^ ":" ^ p in
  let ic = open_in (Filename.concat !gwtp_etc "passwd") in
  let login_ok =
    let rec loop () =
      match try Some (input_line ic) with End_of_file -> None with
        Some line ->
          let line =
            if String.length line > 0 && line.[String.length line - 1] = '\r'
            then
              String.sub line 0 (String.length line - 1)
            else line
          in
          if line = line1 then true else loop ()
      | None -> false
    in
    loop ()
  in
  close_in ic;
  if login_ok then Some (mk_passwd 12) else begin Unix.sleep 5; None end

let check_token fname from b tok =
  let tokens = read_tokens fname in
  let from_b = from ^ "/" ^ b in
  let tm = Unix.time () in
  let rec loop =
    function
      (tm0, from_b0, tok0) :: tokens ->
        if tm < tm0 || tm -. tm0 > !token_tmout then loop tokens
        else if from_b = from_b0 && tok = tok0 then true
        else loop tokens
    | [] -> false
  in
  loop tokens

let update_tokens fname from b tok =
  let tokens = read_tokens fname in
  let from_b = from ^ "/" ^ b in
  let tm = Unix.time () in
  let tokens =
    List.fold_right
      (fun (tm0, from_b0, tok0 as token) tokens ->
         if tm < tm0 || tm -. tm0 > !token_tmout then tokens
         else if from_b = from_b0 && tok = tok0 then tokens
         else token :: tokens)
      tokens []
  in
  let tokens = (tm, from_b, tok) :: tokens in write_tokens fname tokens

let set_token from b tok =
  let fname = tokens_file_name () in
  let from_b = from ^ "/" ^ b in
  let tokens =
    let tokens = read_tokens fname in
    let tm = Unix.time () in
    List.fold_right
      (fun (tm0, from_b0, tok0) tokens ->
         if from_b = from_b0 || tm < tm0 || tm -. tm0 > !token_tmout then
           tokens
         else (tm0, from_b0, tok0) :: tokens)
      tokens [tm, from_b, tok]
  in
  write_tokens fname tokens

(* Requests *)

let insert_file env bdir name =
  let fname = HttpEnv.decode (List.assoc (name ^ "_name") env) in
  let fname = filename_basename fname in
  if fname = "" then ()
  else if fname <> name then
    Printf.printf "You selected <b>%s</b> instead of <b>%s</b> -&gt; ignored.\n"
      fname name
  else
    begin let contents = List.assoc name env in
      let i =
        if lowercase_start_with contents "content-type: " then
          String.index contents '\n'
        else 0
      in
      let j = String.index_from contents (i + 1) '\n' in
      let len = String.length contents - j - 3 in
      if len > 0 then
        let oc = open_out (Filename.concat bdir name) in
        output_substring oc contents (j + 1) len;
        flush oc;
        Printf.printf "File \"%s\" transferred.\n" name;
        close_out oc
    end;
  flush stdout

let make_temp env b =
  let bdir = Filename.concat !gwtp_tmp (b ^ ".gwb") in
  if Sys.file_exists bdir then remove_dir_contents bdir
  else Unix.mkdir bdir 0o777;
  insert_file env bdir "base";
  insert_file env bdir "notes";
  insert_file env bdir "patches";
  insert_file env bdir "particles.txt";
  flush stdout;
  let base = Iolight.input bdir in
  Printf.printf "\n";
  Printf.printf "persons: %d\n" base.data.persons.len;
  Printf.printf "families: %d\n\n" base.data.families.len;
  flush stdout;
  Secure.set_base_dir (Filename.dirname bdir);
  Outbase.output bdir base;
  flush stdout

let copy_temp b =
  let bdir = Filename.concat !gwtp_tmp (b ^ ".gwb") in
  let dir_old = Filename.concat !gwtp_dst "old" in
  let dir_old_gwb = Filename.concat dir_old (b ^ ".gwb") in
  let dir_gwb = Filename.concat !gwtp_dst (b ^ ".gwb") in
  if Sys.file_exists dir_gwb then
    begin
      if not (Sys.file_exists dir_old) then Unix.mkdir dir_old 0o777;
      if Sys.file_exists dir_old_gwb then
        begin remove_dir_contents dir_old_gwb; Unix.rmdir dir_old_gwb end;
      Sys.rename dir_gwb dir_old_gwb;
      let old_forum = Filename.concat dir_old_gwb "forum" in
      if Sys.file_exists old_forum then
        sys_copy old_forum (Filename.concat bdir "forum");
      let old_wiznotes = Filename.concat dir_old_gwb "wiznotes" in
      if Sys.file_exists old_wiznotes then
        Sys.rename old_wiznotes (Filename.concat bdir "wiznotes")
    end;
  Sys.rename bdir dir_gwb

let printf_link_to_main env b tok =
  let lang =
    match HttpEnv.getenv env "lang" with
      Some x -> x
    | _ -> "en"
  in
  Printf.printf "<p><hr /><div align=right>\n";
  Printf.printf "<a href=\"%s?m=MAIN&b=%s&t=%s&lang=%s\">%s</a></div>\n"
    (cgi_script_name ()) b tok lang (transl lang "main page")

(* Upload from GEDCOM *)

let make_gedcom_file env b =
  let fname = Filename.concat !gwtp_tmp (b ^ ".ged") in
  let oc = open_out fname in
  let contents = List.assoc "gedcom" env in
  let i =
    if lowercase_start_with contents "content-type: " then
      String.index contents '\n'
    else 0
  in
  let j = String.index_from contents (i + 1) '\n' in
  let len = String.length contents - j - 3 in
  output_string oc (String.sub contents (j + 1) len); close_out oc

let ged2gwb b =
  let comm =
    Filename.concat !gwtp_etc "ged2gwb" ^ " " ^
    Filename.concat !gwtp_tmp (b ^ ".ged") ^ " -f -o " ^
    Filename.concat !gwtp_tmp b ^ " > " ^
    Filename.concat !gwtp_tmp (b ^ ".log")
  in
  let _ = Sys.command comm in ()

let move_gedcom_to_old b =
  let dir = Filename.concat !gwtp_tmp "ged" in
  (try Unix.mkdir dir 0o775 with Unix.Unix_error (_, _, _) -> ());
  let fname_old = Filename.concat dir (b ^ ".ged") in
  (try Sys.remove fname_old with Sys_error _ -> ());
  let fname = Filename.concat !gwtp_tmp (b ^ ".ged") in
  Sys.rename fname fname_old

let send_gedcom_file env b tok fname =
  let fname = filename_basename fname in
  if Filename.check_suffix fname ".ged" || Filename.check_suffix fname ".GED"
  then
    begin
      Printf.printf "content-type: text/html";
      crlf ();
      crlf ();
      Printf.printf "<head><title>Gwtp...</title></head>\n\
              <body>\n\
              <h1 align=center>Gwtp...</h1>\n\
              <pre>\n";
      flush stdout;
      make_gedcom_file env b;
      Printf.printf "\nGedcom file transferred.\n";
      flush stdout;
      ged2gwb b;
      Printf.printf "New database created.\n";
      flush stdout;
      copy_temp b;
      Printf.printf "Database \"%s\" updated.\n" b;
      Printf.printf "<a href=\"%s?m=LOG&b=%s&t=%s\">Command output</a>\n"
        (cgi_script_name ()) b tok;
      flush stdout;
      move_gedcom_to_old b;
      Printf.printf "</pre>\n";
      printf_link_to_main env b tok;
      Printf.printf "</body>\n";
      flush stdout
    end
  else gwtp_error "This is not a gedcom file (not ending with .GED)"

let gwtp_send_gedcom env b t =
  match HttpEnv.getenv env "gedcom", HttpEnv.getenv env "gedcom_name" with
    Some _, Some fname ->
      send_gedcom_file env b t (HttpEnv.decode fname)
  | Some _, None ->
      gwtp_error "Sorry, your browser seems not be able to send files."
  | _ -> gwtp_invalid_request ()

let gwtp_upload_gedcom env b tok =
  let bcnf = Filename.concat !gwtp_dst (b ^ ".gwf") in
  if not (Sys.file_exists bcnf) then gwtp_error "no configuration file"
  else
    begin
      Printf.printf "content-type: text/html";
      crlf ();
      crlf ();
      copy_template env ([], []) ['b', Val b; 't', Val tok] [] "send_gedcom"
    end

let gwtp_print_log env b tok =
  Printf.printf "content-type: text/html";
  crlf ();
  crlf ();
  Printf.printf "<head><title>Gwtp - %s</title></head>\n\
          <body>\n\
          <h1 align=center>Gwtp - %s</h1>\n"
    b b;
  let fname = Filename.concat !gwtp_tmp (b ^ ".log") in
  let ic = open_in fname in
  Printf.printf "<pre>\n";
  begin try while true do output_char stdout (input_char ic) done with
    End_of_file -> ()
  end;
  Printf.printf "</pre>\n";
  close_in ic;
  printf_link_to_main env b tok;
  Printf.printf "</body>\n"

let gwtp_print_accesses of_wizards env b tok =
  Printf.printf "content-type: text/html";
  crlf ();
  crlf ();
  Printf.printf "<head><title>Gwtp - %s</title></head>\n\
          <body>\n\
          <h1 align=center>Gwtp - %s</h1>\n"
    b b;
  let (varenv, _) = get_base_conf b in
  let fname =
    try
      List.assoc
        (if of_wizards then "wizard_passwd_file" else "friend_passwd_file")
        varenv
    with Not_found -> ""
  in
  let fname =
    if fname <> "" then ""
    else
      List.fold_right Filename.concat [!gwtp_dst; "cnt"]
        (b ^ (if of_wizards then "_w.txt" else "_f.txt"))
  in
  Printf.printf "<pre>\n";
  if fname = "" then Printf.printf "[no password file]\n"
  else
    begin try
      let ic = open_in fname in
      begin try while true do output_char stdout (input_char ic) done with
        End_of_file -> ()
      end;
      close_in ic
    with Sys_error _ -> Printf.printf "[nothing]\n"
    end;
  Printf.printf "</pre>\n";
  printf_link_to_main env b tok;
  Printf.printf "</body>\n"

(* Actions *)

let send_file env b tok fname =
  let fname = filename_basename fname in
  let lockf = Filename.concat !gwtp_tmp (b ^ ".lck") in
  if fname = "base" then
    begin
      Printf.printf "content-type: text/html";
      crlf ();
      crlf ();
      Printf.printf "<head><title>Gwtp...</title></head>\n\
              <body>\n\
              <h1 align=center>Gwtp...</h1>\n\
              <pre>\n";
      flush stdout;
      Lock.control lockf false
        ~onerror:(fun () ->
            Printf.printf "Database is already being transferred.<br>\n";
            Printf.printf "Please try again later.\n")
        (fun () ->
           make_temp env b;
           Printf.printf "\nTemporary database created.\n";
             flush stdout;
           copy_temp b;
           Printf.printf "Database \"%s\" updated.\n" b);
      flush stdout;
      Printf.printf "</pre>\n";
      printf_link_to_main env b tok;
      Printf.printf "</body>\n"
    end
  else
    begin
      Printf.printf "content-type: text/html";
      crlf ();
      crlf ();
      Printf.printf "<head><title>Error</title></head>\n\
              <body>\n\
              <h1><font color=red>Error</font></h1>\n";
      if fname = "" then
        Printf.printf "You must select at least the <b>base</b> file\n"
      else
        Printf.printf "You selected the file <b>%s</b> instead of <b>base</b>\n"
          fname;
      Printf.printf "</body>\n";
      printf_link_to_main env b tok
    end

let gwtp_send env b t =
  match HttpEnv.getenv env "base", HttpEnv.getenv env "base_name" with
    Some _, Some fname -> send_file env b t (HttpEnv.decode fname)
  | Some _, None ->
      gwtp_error "Sorry, your browser seems not be able to send files."
  | _ -> gwtp_invalid_request ()

let gwtp_receive env b _ =
  match HttpEnv.getenv env "f" with
    Some fname ->
      let fname = filename_basename fname in
      let bdir = Filename.concat !gwtp_dst (b ^ ".gwb") in
      Printf.printf "content-type: bin/geneweb";
      crlf ();
      Printf.printf "content-disposition: attachment; filename=%s" fname;
      crlf ();
      crlf ();
      let ic = open_in (Filename.concat bdir fname) in
      begin try
        while true do let c = input_char ic in output_char stdout c done
      with End_of_file -> ()
      end;
      close_in ic
  | _ -> gwtp_invalid_request ()

let acceptable_tags =
  ["!--"; "a"; "b"; "br"; "em"; "font"; "hr"; "i"; "img"; "li"; "ol"; "p";
   "table"; "td"; "tr"; "ul"]

let secure_html s =
  let rec loop len i =
    if i = String.length s then Buff.get len
    else
      match s.[i] with
        '<' ->
          let i = i + 1 in
          let (slash, i) =
            if i = String.length s then false, i
            else if s.[i] = '/' then true, i + 1
            else false, i
          in
          let (tag, i) =
            let rec loop tag i =
              if i = String.length s then "", i
              else
                match s.[i] with
                  'a'..'z' | 'A'..'Z' | '!' | '-' ->
                    loop (tag ^ String.make 1 s.[i]) (i + 1)
                | _ -> tag, i
            in
            loop "" i
          in
          let len =
            if List.mem (String.lowercase_ascii tag) acceptable_tags then
              Buff.store len '<'
            else Buff.mstore len "&lt;"
          in
          let len = if slash then Buff.store len '/' else len in
          loop (Buff.mstore len tag) i
      | c -> loop (Buff.store len c) (i + 1)
  in
  loop 0 0

let gwtp_setconf env b tok =
  let (variables, files) = variables () in
  let varenv =
    List.fold_right
      (fun k varenv ->
         match HttpEnv.getenv env k with
           Some v -> (k, v) :: varenv
         | None -> varenv)
      variables []
  in
  let filenv =
    List.fold_right
      (fun k filenv ->
         match HttpEnv.getenv env k with
           Some v -> (k, secure_html v) :: filenv
         | None -> filenv)
      files []
  in
  Printf.printf "content-type: text/html";
  crlf ();
  crlf ();
  Printf.printf "<head><title>Gwtp - configuration %s</title></head>\n\
          <body>\n\
          <h1 align=center>Gwtp - configuration %s</h1>\n"
    b b;
  set_base_conf b varenv;
  set_base_files b filenv;
  Printf.printf "Configuration changed\n";
  printf_link_to_main env b tok;
  Printf.printf "</body>\n"

let gwtp_upload env b tok =
  let bcnf = Filename.concat !gwtp_dst (b ^ ".gwf") in
  if not (Sys.file_exists bcnf) then gwtp_error "no configuration file"
  else
    begin
      Printf.printf "content-type: text/html";
      crlf ();
      crlf ();
      copy_template env ([], []) ['b', Val b; 't', Val tok] [] "send"
    end

let gwtp_download env b tok =
  let bcnf = Filename.concat !gwtp_dst (b ^ ".gwf") in
  let bdir = Filename.concat !gwtp_dst (b ^ ".gwb") in
  if not (Sys.file_exists bcnf) then gwtp_error "no configuration file"
  else
    begin
      Printf.printf "content-type: text/html";
      crlf ();
      crlf ();
      if Sys.file_exists bdir then
        let print_directory () =
          let dh = Unix.opendir bdir in
          Printf.printf "<ul>\n";
          begin try
            while true do
              let f = Unix.readdir dh in
              let st = Unix.stat (Filename.concat bdir f) in
              if st.Unix.st_kind = Unix.S_REG &&
                 f.[String.length f - 1] <> '~'
              then
                begin
                  Printf.printf "<li><tt>";
                  Printf.printf "<a href=\"%s?m=RECV&b=%s&t=%s&f=/%s\">%s</a>"
                    (cgi_script_name ()) b tok f f;
                  let sz = string_of_int st.Unix.st_size in
                  Printf.printf "%t%s bytes"
                    (fun oc ->
                       for i = 1 to 25 - String.length sz - String.length f do
                         Printf.fprintf oc "&nbsp;"
                       done)
                    sz;
                  Printf.printf "</tt>\n"
                end
            done
          with End_of_file -> Unix.closedir dh
          end;
          Printf.printf "</ul>\n"
        in
        copy_template env ([], [])
          ['b', Val b; 't', Val tok; 'd', Fun print_directory] [] "recv"
      else
        begin
          Printf.printf "<head><title>Gwtp - download %s</title></head>\n\
                  <body>\n\
                  <h1 align=center>Gwtp - download %s</h1>\n\
                  <p>Your database does not exist or is empty.\n"
            b b;
          printf_link_to_main env b tok;
          Printf.printf "</body>\n"
        end
    end

let gwtp_config env b tok =
  let (varenv, filenv) = get_base_conf b in
  Printf.printf "content-type: text/html";
  crlf ();
  crlf ();
  copy_template env (varenv, filenv) ['b', Val b; 't', Val tok] [] "conf"

let gwtp_main env b tok =
  Printf.printf "content-type: text/html";
  crlf ();
  crlf ();
  copy_template env ([], []) ['b', Val b; 't', Val tok; 'w', Val !gw_site]
    ['c', Sys.file_exists (Filename.concat !gwtp_dst (b ^ ".gwf"));
     'g', Sys.file_exists (Filename.concat !gwtp_etc "ged2gwb");
     'w', !gw_site <> ""]
    "main"

let gwtp_login () =
  Printf.printf "content-type: text/html";
  crlf ();
  crlf ();
  Printf.printf "<head><title>Gwtp</title></head>\n\
          <body>\n\
          <h1>Gwtp</h1>\n\
          <form method=POST action=%s>\n\
          <input type=hidden name=m value=LOGIN>\n\
          Database: <input name=b><br>\n\
          Password: <input name=p type=password><br>\n\
          <input type=submit value=Login>\n\
          </form>\n\
          </body>\n"
    (cgi_script_name ())

(* Wrappers *)

let gwtp_check_login from env gwtp_fun =
  match HttpEnv.getenv env "b", HttpEnv.getenv env "p" with
    Some b, Some p ->
      begin match check_login b p with
        Some tok -> set_token from b tok; gwtp_fun env b tok
      | None -> gwtp_error "Invalid login"
      end
  | _ -> gwtp_invalid_request ()

let gwtp_logged from env gwtp_fun =
  match HttpEnv.getenv env "b", HttpEnv.getenv env "t" with
    Some b, Some t ->
      let fname = tokens_file_name () in
      if check_token fname from b t then
        begin
          begin try gwtp_fun env b t with
            e -> update_tokens fname from b t; raise e
          end;
          update_tokens fname from b t
        end
      else gwtp_error "Login expired"
  | _ -> gwtp_invalid_request ()

(* Main *)

let log oc_log str =
  let tm = Unix.localtime (Unix.time ()) in
  let user_agent = try Sys.getenv "HTTP_USER_AGENT" with Not_found -> "" in
  let referer = try Sys.getenv "HTTP_REFERER" with Not_found -> "" in
  let from =
    try Sys.getenv "REMOTE_HOST" with
      Not_found -> try Sys.getenv "REMOTE_ADDR" with Not_found -> ""
  in
  Printf.fprintf oc_log "%4d-%02d-%02d %02d:%02d:%02d" (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec;
  Printf.fprintf oc_log " %s?%s\n" (cgi_script_name ()) str;
  if from <> "" then Printf.fprintf oc_log "  From: %s\n" from;
  if user_agent <> "" then Printf.fprintf oc_log "  Agent: %s\n" user_agent;
  if referer <> "" then Printf.fprintf oc_log "  Referer: %s\n" referer

let gwtp () =
  let content_type = cgi_content_type () in
  let content = cgi_content () in
  let from = cgi_from () in
  let (str, env) = HttpEnv.make content_type content in
  let oc_log = log_open () in
  log oc_log str;
  flush oc_log;
  Unix.dup2 (Unix.descr_of_out_channel oc_log) Unix.stderr;
  begin match HttpEnv.getenv env "m" with
    Some "LOGIN" -> gwtp_check_login from env gwtp_main
  | Some "MAIN" -> gwtp_logged from env gwtp_main
  | Some "DNL" -> gwtp_logged from env gwtp_download
  | Some "CNF" -> gwtp_logged from env gwtp_config
  | Some "RECV" -> gwtp_logged from env gwtp_receive
  | Some "SCNF" -> gwtp_logged from env gwtp_setconf
  | Some "LOG" -> gwtp_logged from env gwtp_print_log
  | Some "ACCW" -> gwtp_logged from env (gwtp_print_accesses true)
  | Some "ACCF" -> gwtp_logged from env (gwtp_print_accesses false)
  | Some "UPL" when not !no_upload -> gwtp_logged from env gwtp_upload
  | Some "UPG" when not !no_upload -> gwtp_logged from env gwtp_upload_gedcom
  | Some "SEND" when not !no_upload -> gwtp_logged from env gwtp_send
  | Some "SEND_GEDCOM" when not !no_upload -> gwtp_logged from env gwtp_send_gedcom
  | Some _ -> gwtp_invalid_request ()
  | None -> gwtp_login ()
  end;
  flush stdout;
  flush oc_log;
  close_out oc_log

let usage_msg = "Usage: gwtp"
let speclist =
  ["-tmp", Arg.String (fun x -> gwtp_tmp := x),
   "<dir>: directory for gwtp stuff; default: " ^ !gwtp_tmp;
   "-dst", Arg.String (fun x -> gwtp_dst := x),
   "<dir>: directory for databases; default: " ^ !gwtp_dst;
   "-log", Arg.String (fun x -> gwtp_log := x),
   "<log>: directory for log file; default: " ^ !gwtp_tmp;
   "-etc", Arg.String (fun x -> gwtp_etc := x),
   "<etc>: directory for passwd, default.gwf and lang files; default: " ^
   !gwtp_tmp;
   "-site", Arg.String (fun x -> gw_site := x),
   "<url>: site (if any) where databases are accommodated";
   "-noup", Arg.Set no_upload, "no upload";
   "-tmout", Arg.Float (fun x -> token_tmout := x),
   "<sec>: tokens time out; default = " ^ string_of_float !token_tmout ^
   " sec"]
let anonfun _ = Arg.usage speclist usage_msg; exit 2

let main () =
  Arg.parse speclist anonfun usage_msg;
  if !gwtp_log = "" then gwtp_log := !gwtp_tmp;
  if !gwtp_etc = "" then gwtp_etc := !gwtp_tmp;
  gwtp ()

let _ =
  try main () with
    exc ->
      Printf.eprintf "Exception raised: %s\n" (Printexc.to_string exc); flush stderr
