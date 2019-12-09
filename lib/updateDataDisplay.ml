open Config
open Gwdb
open TemplAst
open Util
open UpdateData

let translate_title conf =
  let title =
    match p_getenv conf.env "data" with
    | Some "occu" -> transl_nth conf "occupation/occupations" 1
    | Some "place" -> transl conf "places"
    | Some "src" -> transl_nth conf "source/sources" 1
    | Some "fn" -> transl_nth conf "first name/first names" 1
    | Some "sn" -> transl_nth conf "surname/surnames" 1
    | _ -> ""
  in
  Printf.sprintf (ftransl conf "book of %s") title, title

(* ******************************************************************** *)
(*  [Fonc] print_mod_ok : config -> base -> unit                        *)
(** [Description] : Met à jour toutes les personnes en relation avec
                    la donnée que l'on veut modifié.
    [Args] :
    - conf : configuration
    - base : base
      [Retour] :
    - unit
      [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_mod_ok conf base =
  let data = Opt.default "" (p_getenv conf.env "data") in
  let ini = Opt.default "" (p_getenv conf.env "s") in
  let new_input = Opt.map_default "" only_printable (p_getenv conf.env "nx_input") in
  let list = get_person_from_data conf base in
  let list = List.map (fun (istr, perl) -> sou base istr, perl) list in
  let nb_pers =
    List.fold_left (fun accu (_, perl) -> accu + List.length perl) 0 list
  in
  let data_modified = List.for_all (fun (old, _) -> new_input <> old) list in
  (* Indication : 1000 fiches prend environ 1 seconde de traitement. *)
  (* Attention à ne pas mettre une limite trop grande (d'où le test) *)
  (* pour ne pas dépasser le time out du serveur.                    *)
  let max_updates =
    match p_getint conf.base_env "max_nb_update" with
      Some n -> if n > 50000 then 5000 else n
    | _ -> 5000
  in
  if nb_pers <> 0 && data_modified then
    begin
      update_person_list conf base new_input list nb_pers max_updates;
      let title _ =
        Wserver.printf "%s" (Utf8.capitalize (transl conf "modification successful"))
      in
      Hutil.header conf title;
      Hutil.print_link_to_welcome conf true;
      Wserver.printf "<p>\n";
      (* En attendant mieux ... *)
      Wserver.printf "%s%s %d "
        (Utf8.capitalize (transl conf "modification successful"))
        (Util.transl conf ":")
        (min nb_pers max_updates);
      if p_getenv conf.base_env "history" = Some "yes" then
        begin
          Wserver.printf "<a href=\"%sm=HIST&k=20\">" (commd conf);
          Wserver.printf "%s."
            (transl_nth conf "modification/modifications"
               (if nb_pers > 1 then 1 else 0));
          Wserver.printf "</a>"
        end
      else
        Wserver.printf "%s."
          (transl_nth conf "modification/modifications"
             (if nb_pers > 1 then 1 else 0));
      Wserver.printf "</p>\n";
      if nb_pers > max_updates then begin
        Wserver.printf {|<form method="post" action="%s"><p>|} conf.command ;
        Util.hidden_env conf;
        Wserver.printf {|<input type="hidden" name="key" value="%s">|} (List.assoc "key" conf.env) ;
        Wserver.printf {|<input type="hidden" name="m" value="MOD_DATA_OK">|} ;
        Wserver.printf {|<input type="hidden" name="data" value="%s">|} data ;
        Wserver.printf {|<input type="hidden" name="s" value="%s">|} ini ;
        Wserver.printf {|<input type="hidden" name="nx_input" size="80" maxlength="200" value="%s" id="data">|}
          (Util.escape_html (only_printable new_input)) ;
        Wserver.printf "%s" (Utf8.capitalize (transl conf "continue correcting"));
        Wserver.printf {|<button type="submit" class="btn btn-secondary btn-lg">|};
        Wserver.printf "%s" (Utf8.capitalize (transl_nth conf "validate/delete" 0));
        Wserver.printf "</button></p></form>" ;
      end ;
      Wserver.printf {|<p><a href="%sm=MOD_DATA&data=%s&s=%s" id="reference">%s</a></p>|}
        (commd conf) data ini  (Utf8.capitalize (transl conf "new modification")) ;
      Hutil.trailer conf
    end
  else
    let title _ = Wserver.printf "%s" (Utf8.capitalize (transl conf "no modification")) in
    Hutil.header conf title;
    Hutil.print_link_to_welcome conf true;
    Wserver.printf {|<p><a href="%sm=MOD_DATA&data=%s&s=%s" id="reference">%s</a></p>|}
      (commd conf) data ini (Utf8.capitalize (transl conf "new modification")) ;
    Hutil.trailer conf


type 'a env =
    Vlist_data of (istr * string) list
  | Vlist_ini of string list
  | Vlist_value of (istr * string) list
  | Vint of int
  | Vstring of string
  | Vother of 'a
  | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x
let bool_val x = VVbool x
let str_val x = VVstring x


let rec eval_var conf base env xx _loc sl =
  try eval_simple_var conf base env xx sl with
    Not_found -> eval_compound_var conf base env xx sl
and eval_simple_var conf base env xx =
  function
    [s] ->
    begin try bool_val (eval_simple_bool_var conf base env xx s) with
        Not_found -> str_val (eval_simple_str_var conf base env xx s)
    end
  | _ -> raise Not_found
and eval_simple_bool_var _conf _base env _xx = function
  | "is_modified" -> begin match get_env "key" env with
      | Vstring _ as x -> get_env "entry_key" env = x
      | _ -> false
    end
  | _ -> raise Not_found
and eval_simple_str_var conf _base env _xx =
  function
    "entry_ini" -> eval_string_env "entry_ini" env
  | "entry_value" -> eval_string_env "entry_value" env
  | "entry_key" -> eval_string_env "entry_key" env
  | "ini" -> eval_string_env "ini" env
  | "nb_results" ->
    begin match get_env "list" env with
        Vlist_data l -> string_of_int (List.length l)
      | _ -> "0"
    end
  | "title" ->
    let len =
      match get_env "list" env with
        Vlist_data l -> List.length l
      | _ -> 0
    in
    let ini = Opt.default "" (p_getenv conf.env "s") in
    let (book_of, title) = translate_title conf in
    let result =
      if ini = "" then Printf.sprintf " (%d %s)" len title
      else
        " - " ^
        Printf.sprintf (ftransl conf "%d %s starting with %s") len title ini
    in
    Utf8.capitalize book_of ^ result
  | _ -> raise Not_found
and eval_compound_var conf base env xx sl =
  let rec loop =
    function
      [s] -> eval_simple_str_var conf base env xx s
    | ["evar"; s] -> Opt.default "" (p_getenv conf.env s)
    | "encode" :: sl -> code_varenv (loop sl)
    | "escape" :: sl -> Util.escape_html (loop sl)
    | "html_encode" :: sl -> no_html_tags (loop sl)
    | "printable" :: sl -> only_printable (loop sl)
    | _ -> raise Not_found
  in
  str_val (loop sl)
and eval_string_env s env =
  match get_env s env with
    Vstring s -> s
  | _ -> raise Not_found
and eval_int_env s env =
  match get_env s env with
    Vint i -> string_of_int i
  | _ -> raise Not_found

let print_foreach conf print_ast _eval_expr =
  let rec print_foreach env xx _loc s sl el al =
    match s :: sl with
      ["initial"] -> print_foreach_initial env xx al
    | ["entry"] -> print_foreach_entry env xx el al
    | ["value"] -> print_foreach_value env xx al
    | _ -> raise Not_found
  and print_foreach_entry env xx _el al =
    let list =
      match get_env "list" env with
        Vlist_data l -> l
      | _ -> []
    in
    let list = build_list_long conf list in
    let k = Opt.to_string @@ p_getenv conf.env "key" in
    let rec loop =
      function
        (ini_k, (list_v : (istr * string) list)) :: l ->
        let env =
          ("key", Vstring k)
          :: ("entry_ini", Vstring ini_k)
          :: ("list_value", Vlist_value list_v)
          :: env
        in
        List.iter (print_ast env xx) al; loop l
      | [] -> ()
    in
    loop list
  and print_foreach_value env xx al =
    let list =
      match get_env "list_value" env with
        Vlist_value l ->
        List.sort
          (fun (_, s1) (_, s2) ->
             let rss1 = Place.without_suburb s1 in
             let rss2 = Place.without_suburb s2 in
             if rss1 = rss2 then Gutil.alphabetic_order s1 s2
             else Gutil.alphabetic_order rss1 rss2)
          l
      | _ -> []
    in
    let rec loop =
      function
        (i, s) :: l ->
        let env =
          ("entry_value", Vstring s)
          :: ("entry_key", Vstring (string_of_istr i))
          :: env
        in
        List.iter (print_ast env xx) al; loop l
      | [] -> ()
    in
    loop list
  and print_foreach_initial env xx al =
    let list =
      match get_env "list" env with
        Vlist_data l -> l
      | _ -> []
    in
    let ini_list = build_list_short conf list in
    let rec loop =
      function
        ini :: l ->
        let env = ("ini", Vstring ini) :: env in
        List.iter (print_ast env xx) al; loop l
      | [] -> ()
    in
    loop ini_list
  in
  print_foreach

let print_mod conf base =
  match p_getenv conf.env "data" with
  | Some ("place" | "src" | "occu" | "fn" | "sn") ->
    let list = build_list conf base in
    let env = ["list", Vlist_data list] in
    Hutil.interp conf "upddata"
      {Templ.eval_var = eval_var conf base;
       Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
       Templ.eval_predefined_apply = (fun _ -> raise Not_found);
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach = print_foreach conf}
      env ()
  | _ ->
    Hutil.interp conf "upddatamenu"
      {Templ.eval_var = (fun _ -> raise Not_found);
       Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
       Templ.eval_predefined_apply = (fun _ -> raise Not_found);
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach = fun _ -> raise Not_found}
      [] ()
