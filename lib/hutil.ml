(* $Id: hutil.ml,v 5.11 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config

let up_fname _conf = "up.png"

let commd_no_params conf =
  conf.command ^ "?" ^
  List.fold_left
    (fun c (k, v) ->
       c ^ (if c = "" then "" else "&") ^ k ^
       (if v = "" then "" else "=" ^ v))
    "" conf.henv

let link_to_referer conf =
  let referer = Util.get_referer conf in
  let back = Util.capitale (Util.transl conf "back") in
  if referer <> "" then
    Printf.sprintf "<a href=\"%s\">\
         <span class=\"fa fa-arrow-left fa-lg\" title=\"%s\"></span>\
       </a>\n"
      referer back
  else ""

let gen_print_link_to_welcome f conf right_aligned =
  if not conf.cancel_links then begin
    let fname = up_fname conf in
    let wid_hei =
      match Util.image_size (Util.image_file_name fname) with
      | Some (wid, hei) ->
        " width=\"" ^ string_of_int wid ^ "\" height=\"" ^ string_of_int hei ^ "\""
      | None -> ""
    in
    if right_aligned then
      Wserver.printf "<div style=\"float:%s\">\n" conf.right
    else Wserver.printf "<p>\n";
    f ();
    let str = link_to_referer conf in
    if str = "" then () else Wserver.printf "%s" str;
    Wserver.printf "<a href=\"%s\">\
                    <img src=\"%s/%s\"%s alt=\"^^\" title=\"^^\"%s>\
                    </a>\n"
      (commd_no_params conf) (Util.image_prefix conf) fname wid_hei conf.xhs;
    if right_aligned then Wserver.printf "</div>\n"
    else Wserver.printf "</p>\n"
  end

let print_link_to_welcome = gen_print_link_to_welcome (fun () -> ())

let header_without_http conf title =
  Wserver.printf "<!DOCTYPE html>\n";
  Wserver.printf "<head>\n";
  Wserver.printf "  <title>";
  title true;
  Wserver.printf "</title>\n";
  Wserver.printf "  <meta name=\"robots\" content=\"none\">\n";
  Wserver.printf "  <meta charset=\"%s\">\n" conf.charset;
  Wserver.printf
    "  <link rel=\"shortcut icon\" href=\"%s/favicon_gwd.png\">\n"
    (Util.image_prefix conf);
  Wserver.printf
    "  <link rel=\"apple-touch-icon\" href=\"%s/favicon_gwd.png\">\n"
    (Util.image_prefix conf);
  Wserver.printf "  <meta name=\"viewport\" content=\"width=device-width, \
                    initial-scale=1, shrink-to-fit=no\">\n";
  begin match Util.open_templ conf "css" with
    Some ic -> Templ.copy_from_templ conf [] ic
  | None -> ()
  end;
  Templ.include_hed_trl conf "hed";
  Wserver.printf "\n</head>\n";
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon " !dir" ^ "\"" with
      Not_found -> ""
  in
  let s = s ^ Util.body_prop conf in
  Wserver.printf "<body%s>\n" s;
  Util.message_to_wizard conf

let header_without_page_title conf title =
  Util.html conf;
  header_without_http conf title

let header_link_welcome conf title =
  header_without_page_title conf title;
  print_link_to_welcome conf true;
  Wserver.printf "<h1>";
  title false;
  Wserver.printf "</h1>\n"

let header_no_page_title conf title =
  header_without_page_title conf title;
  match Util.p_getenv conf.env "title" with
    None | Some "" -> ()
  | Some x -> Wserver.printf "<h1>%s</h1>\n" x

let header conf title =
  header_without_page_title conf title;
  Wserver.printf "\n<h1>";
  title false;
  Wserver.printf "</h1>\n"

let header_fluid conf title =
  header_without_http conf title;
  Wserver.printf "\n<h1>";
  title false;
  Wserver.printf "</h1>\n"

let rheader conf title =
  header_without_page_title conf title;
  Wserver.printf "<h1 class=\"error\">";
  title false;
  Wserver.printf "</h1>\n"

let gen_trailer with_logo conf =
  let conf = {conf with is_printed_by_template = false} in
  Templ.include_hed_trl conf "trl";
  if with_logo then Templ.print_copyright_with_logo conf
  else Templ.print_copyright conf;
  Wserver.printf "</div>\n";
  (* balances header_without_http *)
  begin match Util.open_templ conf "js" with
    Some ic -> Templ.copy_from_templ conf [] ic
  | None -> ()
  end;
  Wserver.printf "</body>\n</html>\n"

let trailer = gen_trailer true

let incorrect_request conf =
  let title _ =
    Wserver.printf "%s" (Util.capitale (Util.transl conf "incorrect request"))
  in
  Wserver.http HttpStatus.Bad_Request;
  header conf title;
  Wserver.printf "<p>\n";
  print_link_to_welcome conf false;
  Wserver.printf "</p>\n";
  trailer conf

let error_cannot_access conf fname =
  let title _ = Wserver.printf "Error" in
  header conf title;
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>\n";
  Wserver.printf "Cannot access file \"%s.txt\".\n" fname;
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n";
  trailer conf

let gen_interp header conf fname ifun env ep =
  let v = !(Templ.template_file) in
  Templ.template_file := fname;
  begin try
    match Templ.input_templ conf fname with
      Some astl ->
        if header then Util.html conf; Templ.interp_ast conf ifun env ep astl
    | None -> error_cannot_access conf fname
  with e -> Templ.template_file := v; raise e
  end;
  Templ.template_file := v

let interp_no_header conf fname ifun env ep =
  gen_interp false conf fname ifun env ep

let interp conf fname ifun env ep = gen_interp true conf fname ifun env ep

(* Calendar request *)

let eval_julian_day conf =
  let open Adef in
  let getint v =
    match Util.p_getint conf.env v with
      Some x -> x
    | _ -> 0
  in
  List.fold_left
    (fun d (var, cal, conv, max_month) ->
       let yy =
         match Util.p_getenv conf.env ("y" ^ var) with
           Some v ->
             begin try
               let len = String.length v in
               if cal = Djulian && len > 2 && v.[len-2] = '/' then
                 int_of_string (String.sub v 0 (len - 2)) + 1
               else int_of_string v
             with Failure _ -> 0
             end
         | None -> 0
       in
       let mm = getint ("m" ^ var) in
       let dd = getint ("d" ^ var) in
       let dt = {day = dd; month = mm; year = yy; prec = Sure; delta = 0} in
       match Util.p_getenv conf.env ("t" ^ var) with
         Some _ -> conv dt
       | None ->
           match
             Util.p_getenv conf.env ("y" ^ var ^ "1"),
             Util.p_getenv conf.env ("y" ^ var ^ "2"),
             Util.p_getenv conf.env ("m" ^ var ^ "1"),
             Util.p_getenv conf.env ("m" ^ var ^ "2"),
             Util.p_getenv conf.env ("d" ^ var ^ "1"),
             Util.p_getenv conf.env ("d" ^ var ^ "2")
           with
             Some _, _, _, _, _, _ -> conv {dt with year = yy - 1}
           | _, Some _, _, _, _, _ -> conv {dt with year = yy + 1}
           | _, _, Some _, _, _, _ ->
               let (yy, mm) =
                 if mm = 1 then yy - 1, max_month else yy, mm - 1
               in
               conv {dt with year = yy; month = mm}
           | _, _, _, Some _, _, _ ->
               let (yy, mm) =
                 if mm = max_month then yy + 1, 1 else yy, mm + 1
               in
               let r = conv {dt with year = yy; month = mm} in
               if r = conv dt then
                 let (yy, mm) =
                   if mm = max_month then yy + 1, 1 else yy, mm + 1
                 in
                 conv {dt with year = yy; month = mm}
               else r
           | _, _, _, _, Some _, _ -> conv {dt with day = dd - 1}
           | _, _, _, _, _, Some _ -> conv {dt with day = dd + 1}
           | _ -> d)
    (Calendar.sdn_of_gregorian conf.today)
    ["g", Dgregorian, Calendar.sdn_of_gregorian, 12;
     "j", Djulian, Calendar.sdn_of_julian, 12;
     "f", Dfrench, Calendar.sdn_of_french, 13;
     "h", Dhebrew, Calendar.sdn_of_hebrew, 13]

(* *)

type 'a env =
    Vint of int
  | Vother of 'a
  | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let eval_var conf env jd _loc =
  let open TemplAst in
  function
    ["integer"] ->
      begin match get_env "integer" env with
        Vint i -> VVstring (string_of_int i)
      | _ -> raise Not_found
      end
  | "date" :: sl -> TemplDate.eval_date_var conf jd sl
  | "today" :: sl ->
      TemplDate.eval_date_var conf (Calendar.sdn_of_gregorian conf.today) sl
  | _ -> raise Not_found

let print_foreach print_ast eval_expr =
  let eval_int_expr env jd e =
    let s = eval_expr env jd e in
    try int_of_string s with Failure _ -> raise Not_found
  in
  let rec print_foreach env jd _loc s sl el al =
    match s, sl with
      "integer_range", [] -> print_integer_range env jd el al
    | _ -> raise Not_found
  and print_integer_range env jd el al =
    let (i1, i2) =
      match el with
        [[e1]; [e2]] -> eval_int_expr env jd e1, eval_int_expr env jd e2
      | _ -> raise Not_found
    in
    for i = i1 to i2 do
      let env = ("integer", Vint i) :: env in List.iter (print_ast env jd) al
    done
  in
  print_foreach

let print_calendar conf =
  interp conf "calendar"
    {Templ.eval_var = eval_var conf;
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = print_foreach }
    [] (eval_julian_day conf)
