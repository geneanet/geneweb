(* Copyright (c) 1998-2007 INRIA *)

type httpStatus =
  | OK (* 200 *)
  | Moved_Temporarily (* 302 *)
  | Bad_Request (* 400 *)
  | Unauthorized (* 401 *)
  | Forbidden (* 403 *)
  | Not_Found (* 404 *)

let connection_closed = ref false

let eprintf = Printf.eprintf

let sock_in = ref "wserver.sin"
let sock_out = ref "wserver.sou"
let stop_server = ref "STOP_SERVER"
let noproc = ref false
let cgi = ref false

let wserver_sock = ref Unix.stdout
let wsocket () = !wserver_sock

let wserver_oc = ref stdout
let woc () = !wserver_oc

let printnl fmt =
  Printf.ksprintf
    (fun s ->
       output_string !wserver_oc s; output_string !wserver_oc "\013\010")
    fmt

type printing_state = Nothing | Status | Contents

let printing_state = ref Nothing

let http status =
  if !printing_state <> Nothing then failwith "HTTP Status already sent";
  printing_state := Status;
  if status <> OK || not !cgi then
    let answer = match status with
      | OK -> "200 OK"
      | Moved_Temporarily -> "302 Moved Temporarily"
      | Bad_Request -> "400 Bad Request"
      | Unauthorized -> "401 Unauthorized"
      | Forbidden -> "403 Forbidden"
      | Not_Found -> "404 Not Found"
    in
    if !cgi then printnl "Status: %s" answer else printnl "HTTP/1.0 %s" answer

let header fmt =
  if !printing_state <> Status then
    if !printing_state = Nothing then http OK
    else failwith "Cannot write HTTP headers: page contents already started";
  printnl fmt

let printf fmt =
  if !printing_state <> Contents then
    begin
      if !printing_state = Nothing then http OK;
      printnl "";
      printing_state := Contents
    end;
  Printf.ksprintf (fun s -> output_string !wserver_oc s) fmt
let wflush () = flush !wserver_oc

let print_string ?(unsafe = false) s =
  if not unsafe then
    if !printing_state <> Contents then
      begin
        if !printing_state = Nothing then http OK;
        printnl "";
        printing_state := Contents
      end
  ;
  output_string !wserver_oc s

let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)

let hexa_val conf =
  match conf with
    '0'..'9' -> Char.code conf - Char.code '0'
  | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
  | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
  | _ -> 0

let gen_decode strip_spaces s =
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with
        '%' | '+' -> true
      | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          '%' when i + 2 < String.length s -> i + 3
        | _ -> succ i
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          '%' when i + 2 < String.length s ->
            let v = hexa_val s.[i+1] * 16 + hexa_val s.[i+2] in
            Bytes.set s1 i1 (Char.chr v); i + 3
        | '+' -> Bytes.set s1 i1 ' '; succ i
        | x -> Bytes.set s1 i1 x; succ i
      in
      copy_decode_in s1 i (succ i1)
    else Bytes.unsafe_to_string s1
  in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] = ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] = ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else s
    else s
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    let s = copy_decode_in s1 0 0 in
    if strip_spaces then strip_heading_and_trailing_spaces s else s
  else s

let decode = gen_decode true

let special =
  function
    '\000'..'\031' | '\127'..'\255' | '<' | '>' | '"' | '#' | '%' | '{' |
    '}' | '|' | '\\' | '^' | '~' | '[' | ']' | '`' | ';' | '/' | '?' | ':' |
    '@' | '=' | '&' | '+' ->
      true
  | _ -> false

let encode s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
        ' ' -> true
      | x -> if special x then true else need_code (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
          ' ' -> Bytes.set s1 i1 '+'; succ i1
        | c ->
            if special c then
              begin
                Bytes.set s1 i1 '%';
                Bytes.set s1 (i1 + 1) (hexa_digit (Char.code c / 16));
                Bytes.set s1 (i1 + 2) (hexa_digit (Char.code c mod 16));
                i1 + 3
              end
            else begin Bytes.set s1 i1 c; succ i1 end
      in
      copy_code_in s1 (succ i) i1
    else Bytes.unsafe_to_string s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (Bytes.create len) 0 0
  else s

let http_redirect_temporarily url =
  http Moved_Temporarily; printnl "Location: %s" url

let print_exc exc =
  let () =
    match exc with
      Unix.Unix_error (err, fun_name, arg) ->
        prerr_string "\"";
        prerr_string fun_name;
        prerr_string "\" failed";
        if String.length arg > 0 then
          begin
            prerr_string " on \"";
            prerr_string arg;
            prerr_string "\""
          end;
        prerr_string ": ";
        prerr_endline (Unix.error_message err)
    | Out_of_memory -> prerr_string "Out of memory\n"
    | Match_failure (file, first_char, last_char) ->
        prerr_string "Pattern matching failed, file ";
        prerr_string file;
        prerr_string ", chars ";
        prerr_int first_char;
        prerr_char '-';
        prerr_int last_char;
        prerr_char '\n'
    | Assert_failure (file, first_char, last_char) ->
        prerr_string "Assertion failed, file ";
        prerr_string file;
        prerr_string ", chars ";
        prerr_int first_char;
        prerr_char '-';
        prerr_int last_char;
        prerr_char '\n'
    | x ->
        prerr_string "Wserver: uncaught exception: ";
        prerr_string (Obj.magic (Obj.field (Obj.field (Obj.repr x) 0) 0));
        if Obj.size (Obj.repr x) > 1 then
          begin
            prerr_char '(';
            for i = 1 to Obj.size (Obj.repr x) - 1 do
              if i > 1 then prerr_string ", ";
              let arg = Obj.field (Obj.repr x) i in
              if not (Obj.is_block arg) then prerr_int (Obj.magic arg : int)
              else if Obj.tag arg = 252 then
                begin
                  prerr_char '"';
                  prerr_string (Obj.magic arg : string);
                  prerr_char '"'
                end
              else prerr_char '_'
            done;
            prerr_char ')'
          end;
        prerr_char '\n'
  in
  let () =
    if Printexc.backtrace_status () then
      begin prerr_string "Backtrace:\n"; Printexc.print_backtrace stderr end
  in
  ()

let print_err_exc exc = print_exc exc; flush stderr

let case_unsensitive_eq s1 s2 =
  String.lowercase_ascii s1 = String.lowercase_ascii s2

let rec extract_param name stop_char =
  function
    x :: l ->
      if String.length x >= String.length name &&
         case_unsensitive_eq (String.sub x 0 (String.length name)) name
      then
        let i =
          let rec loop i =
            if i = String.length x then i
            else if x.[i] = stop_char then i
            else loop (i + 1)
          in
          loop (String.length name)
        in
        String.sub x (String.length name) (i - String.length name)
      else extract_param name stop_char l
  | [] -> ""

let buff = ref (Bytes.create 80)
let store len x =
  if len >= Bytes.length !buff then
    buff := Bytes.extend !buff 0 (Bytes.length !buff);
  Bytes.set !buff len x;
  succ len
let get_buff len = Bytes.sub_string !buff 0 len

let get_request strm =
  let rec loop len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '\010' ->
        Stream.junk strm__;
        let s = strm__ in
        if len = 0 then [] else let str = get_buff len in str :: loop 0 s
    | Some '\013' -> Stream.junk strm__; loop len strm__
    | Some c -> Stream.junk strm__; loop (store len c) strm__
    | _ -> if len = 0 then [] else [get_buff len]
  in
  loop 0 strm

let timeout tmout spid _ =
  Unix.kill spid Sys.sigkill;
  Unix.kill spid Sys.sigterm;
  let pid = Unix.fork () in
  if pid = 0 then
    if Unix.fork () = 0 then
      begin
        http OK;
        printnl "Content-type: text/html; charset=iso-8859-1";
        printnl "";
        printf "<head><title>Time out</title></head>\n";
        printf "<body><h1>Time out</h1>\n";
        printf "Computation time > %d second(s)\n" tmout;
        printf "</body>\n";
        wflush ();
        exit 0
      end
    else exit 0;
  let _ = Unix.waitpid [] pid in (); exit 2

let get_request_and_content strm =
  let request = get_request strm in
  let content =
    match extract_param "content-length: " ' ' request with
      "" -> ""
    | x ->
        let get_next_char _ =
          let (strm__ : _ Stream.t) = strm in
          match Stream.peek strm__ with
            Some x -> Stream.junk strm__; x
          | _ -> ' '
        in
        String.init (int_of_string x) get_next_char
  in
  request, content

let string_of_sockaddr =
  function
    Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a
let sockaddr_of_string s = Unix.ADDR_UNIX s

let treat_connection tmout callback addr fd =
  if Sys.unix then
    if tmout > 0 then
      begin let spid = Unix.fork () in
        if spid > 0 then
          begin
            ignore @@ Sys.signal Sys.sigalrm (Sys.Signal_handle (timeout tmout spid)) ;
            ignore @@ Unix.alarm tmout ;
            ignore @@ Unix.waitpid [] spid ;
            ignore @@ Sys.signal Sys.sigalrm Sys.Signal_default ;
            exit 0
          end
      end;
  let (request, script_name, contents) =
    let (request, contents) =
      let strm =
        let c = Bytes.create 1 in
        Stream.from
          (fun _ ->
             if Unix.read fd c 0 1 = 1 then Some (Bytes.get c 0) else None)
      in
      get_request_and_content strm
    in
    let (script_name, contents) =
      match extract_param "GET /" ' ' request with
        "" -> extract_param "POST /" ' ' request, contents
      | str ->
          try
            let i = String.index str '?' in
            String.sub str 0 i,
            String.sub str (i + 1) (String.length str - i - 1)
          with Not_found -> str, ""
    in
    request, script_name, contents
  in
  begin try callback (addr, request) script_name contents with
    Unix.Unix_error (Unix.EPIPE, "write", _) -> ()
  | Sys_error msg when msg = "Broken pipe" -> ()
  | exc -> print_err_exc exc
  end;
  (try wflush () with _ -> ());
  try flush stderr with _ -> ()

let buff = Bytes.create 1024

let copy_what_necessary t oc =
  let strm =
    let len = ref 0 in
    let i = ref 0 in
    Stream.from
      (fun _ ->
         if !i >= !len then
           begin
             len := Unix.read t buff 0 (Bytes.length buff);
             i := 0;
             if !len > 0 then output oc buff 0 !len
           end;
         if !len = 0 then None
         else begin incr i; Some (Bytes.get buff (!i - 1)) end)
  in
  let _ = get_request_and_content strm in ()

let rec list_remove x =
  function
    [] -> failwith "list_remove"
  | y :: l -> if x = y then l else y :: list_remove x l

let pids = ref []

let cleanup_verbose = ref true

let cleanup_sons () =
  List.iter
    (fun p ->
       let pid =
         try fst (Unix.waitpid [Unix.WNOHANG] p) with
           Unix.Unix_error (_, _, _) as exc ->
             if !cleanup_verbose then
               begin
                 eprintf "*** Why error on waitpid %d?\n" p;
                 flush stderr;
                 print_exc exc;
                 eprintf "[";
                 List.iter (fun p -> eprintf " %d" p) !pids;
                 eprintf "]\n";
                 flush stderr;
                 cleanup_verbose := false
               end;
             p
       in
       if pid = 0 then () else pids := list_remove pid !pids)
    !pids

let wait_available max_clients s =
  match max_clients with
    Some m ->
      if List.length !pids >= m then
        (let (pid, _) = Unix.wait () in pids := list_remove pid !pids);
      if !pids <> [] then cleanup_sons ();
      let stop_verbose = ref false in
      while !pids <> [] && Unix.select [s] [] [] 15.0 = ([], [], []) do
        cleanup_sons ();
        if !pids <> [] && not !stop_verbose then
          begin
            stop_verbose := true;
            let tm = Unix.localtime (Unix.time ()) in
            eprintf
              "*** %02d/%02d/%4d %02d:%02d:%02d %d process(es) remaining after cleanup (%d)\n"
              tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year)
              tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
              (List.length !pids) (List.hd !pids);
            flush stderr;
            ()
          end
      done
  | None -> ()

let wait_and_compact s =
  if Unix.select [s] [] [] 15.0 = ([], [], []) then
    begin
      eprintf "Compacting... ";
      flush stderr;
      Gc.compact ();
      eprintf "Ok\n";
      flush stderr
    end

let skip_possible_remaining_chars fd =
  if not !connection_closed then begin
    let b = Bytes.create 3 in
    try
      let rec loop () =
        match Unix.select [fd] [] [] 5.0 with
          [_], [], [] ->
          let len = Unix.read fd b 0 (Bytes.length b) in
          if len = Bytes.length b then loop ()
        | _ -> ()
      in
      loop ()
    with Unix.Unix_error (Unix.ECONNRESET, _, _) -> ()
  end

let check_stopping () =
  if Sys.file_exists !stop_server then
    begin
      flush stdout;
      eprintf "\nServer stopped by presence of file %s.\n" !stop_server;
      eprintf "Remove that file to allow servers to run again.\n";
      flush stderr;
      exit 0
    end

let accept_connection tmout max_clients callback s =
  if !noproc then wait_and_compact s else wait_available max_clients s;
  let (t, addr) = Unix.accept s in
  check_stopping ();
  Unix.setsockopt t Unix.SO_KEEPALIVE true;
  if Sys.unix then
    match try Some (Unix.fork ()) with _ -> None with
      Some 0 ->
        begin try
          if max_clients = None && Unix.fork () <> 0 then exit 0;
          Unix.close s;
          wserver_sock := t;
          wserver_oc := Unix.out_channel_of_descr t;
          treat_connection tmout callback addr t
        with
          Unix.Unix_error (Unix.ECONNRESET, "read", _) -> ()
        | exc -> try print_err_exc exc; flush stderr with _ -> ()
        end;
        (try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ());
        (try Unix.shutdown Unix.stdout Unix.SHUTDOWN_SEND with _ -> ());
        skip_possible_remaining_chars t;
        (try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with _ -> ());
        (try Unix.shutdown Unix.stdin Unix.SHUTDOWN_RECEIVE with _ -> ());
        exit 0
    | Some id ->
        Unix.close t;
        if max_clients = None then let _ = Unix.waitpid [] id in ()
        else pids := id :: !pids
    | None -> Unix.close t; eprintf "Fork failed\n"; flush stderr
  else
    let oc = open_out_bin !sock_in in
    let cleanup () = try close_out oc with _ -> () in
    begin try copy_what_necessary t oc with
      Unix.Unix_error (_, _, _) -> ()
    | exc -> cleanup (); raise exc
    end;
    cleanup ();
    if !noproc then
      let fd = Unix.openfile !sock_in [Unix.O_RDONLY] 0 in
      let oc = open_out_bin !sock_out in
      wserver_oc := oc;
      treat_connection tmout callback addr fd;
      flush oc;
      close_out oc;
      Unix.close fd
    else
      begin let pid =
        let env =
          Array.append (Unix.environment ())
            [| "WSERVER=" ^ string_of_sockaddr addr |]
        in
        let args = Sys.argv in
        Unix.create_process_env Sys.argv.(0) args env Unix.stdin Unix.stdout
          Unix.stderr
      in
        let _ = Unix.waitpid [] pid in
        let ic = open_in_bin !sock_in in close_in ic
      end;
    let cleanup () =
      (try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ());
      skip_possible_remaining_chars t;
      (try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with _ -> ());
      try Unix.close t with _ -> ()
    in
    begin try
      begin let ic = open_in_bin !sock_out in
        let cleanup () = try close_in ic with _ -> () in
        begin try
          begin let rec loop () =
            let len = input ic buff 0 (Bytes.length buff) in
            if len = 0 then ()
            else
              begin
                begin let rec loop_write i =
                  let olen = Unix.write t buff i (len - i) in
                  if i + olen < len then loop_write (i + olen)
                in
                  loop_write 0
                end;
                loop ()
              end
          in
            loop ()
          end
        with
          Unix.Unix_error (_, _, _) -> ()
        | exc -> cleanup (); raise exc
        end;
        cleanup ()
      end
    with
      Unix.Unix_error (_, _, _) -> ()
    | exc -> cleanup (); raise exc
    end;
    cleanup ()

let close_connection () =
  print_endline __LOC__ ;
  (try Unix.close !wserver_sock with _ -> ()) ;
  (try close_out !wserver_oc with _ -> ());
  connection_closed := true

let f addr_opt port tmout max_clients g =
  match
    if Sys.unix then None
    else try Some (Sys.getenv "WSERVER") with Not_found -> None
  with
  | Some s ->
    let addr = sockaddr_of_string s in
    let fd = Unix.openfile !sock_in [Unix.O_RDONLY] 0 in
    let oc = open_out_bin !sock_out in
    wserver_oc := oc; ignore (treat_connection tmout g addr fd); exit 0
  | None ->
      check_stopping ();
      let addr =
        match addr_opt with
          Some addr ->
            begin try Unix.inet_addr_of_string addr with
              Failure _ -> (Unix.gethostbyname addr).Unix.h_addr_list.(0)
            end
        | None -> Unix.inet_addr_any
      in
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt s Unix.SO_REUSEADDR true;
      Unix.bind s (Unix.ADDR_INET (addr, port));
      Unix.listen s 4;
      if Sys.unix then (let _ = Unix.nice 1 in ());
      let tm = Unix.localtime (Unix.time ()) in
      eprintf "Ready %4d-%02d-%02d %02d:%02d port %d...\n"
        (1900 + tm.Unix.tm_year)
        (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
        port ;
      flush stderr;
      while true do
        begin try accept_connection tmout max_clients g s with
          Unix.Unix_error (Unix.ECONNRESET, "accept", _) -> ()
        | Unix.Unix_error ((Unix.EBADF | Unix.ENOTSOCK), "accept", _) as x ->
            (* oops! *)            raise x
        | Sys_error msg when msg = "Broken pipe" -> ()
        | exc -> print_err_exc exc
        end;
        (try wflush () with Sys_error _ -> ());
        (try flush stdout with Sys_error _ -> ());
        flush stderr
      done
