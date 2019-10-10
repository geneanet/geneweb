open Geneweb

let bench ?(t=1) name fn arg =
  if match Sys.getenv_opt "BENCH_FN" with None -> true | Some x -> x = name
  then ignore @@ Benchmark.throughput1 ~name t (List.map fn) arg

let list =
  [1;2;10;100;1000;10000;100000;1000000;10000000;100000000;1000000000]

let sosa_list =
  List.map Sosa.of_int list

let () =
  bench "Sosa.gen" (List.map Sosa.gen) [ sosa_list ]
; bench "Sosa.to_string_sep" (List.map @@ Sosa.to_string_sep ",") [ sosa_list ]
; bench "Sosa.to_string" (List.map Sosa.to_string) [ sosa_list ]
; bench "Sosa.of_string" (List.map Sosa.of_string) [ List.map string_of_int list ]
; bench "Sosa.branches" (List.map Sosa.branches) [ sosa_list ]
; bench "Place.normalize" Geneweb.Place.normalize
    [ "[foo-bar] - boobar (baz)" ; "[foo-bar] – boobar (baz)" ; "[foo-bar] — boobar (baz)" ]
; bench "Mutil.unsafe_tr" (fun s -> Mutil.unsafe_tr 'a' 'b' @@ "a" ^ s)
    [ "aaaaaaaaaa" ; "bbbbbbbbbb" ; "abbbbbbbb" ; "bbbbbbbbba" ; "ababababab" ]
; bench "Mutil.tr" (fun s -> Mutil.tr 'a' 'b' @@ "a" ^ s)
    [ "aaaaaaaaaa" ; "bbbbbbbbbb" ; "abbbbbbbb" ; "bbbbbbbbba" ; "ababababab" ]
; bench "Mutil.contains" (Mutil.contains "foobarbaz")
    [ "foo" ; "bar" ; "baz" ; "foobarbaz!" ]
; bench "Place.compare_places" (Place.compare_places "[foo-bar] - baz, boobar")
    [ "[foo-bar] - baz, boobar"
    ; "[foo-bar] - baz, boobar, barboo"
    ; "baz, boobar"
    ]

; begin match Sys.getenv "BENCH_BASE" with
  | exception Not_found -> ()
  | bname when bname <> "" ->
    let conf =
      {Config.from="";manitou=false;supervisor=false;wizard=false;
#ifdef API
       api_host="";api_port=0;
#endif
       is_printed_by_template=false;friend=false;
       just_friend_wizard=false;user="";username="";
       auth_scheme=Config.NoAuth;pure_xhtml=false;command="";
       indep_command="";highlight="";lang="";default_lang="";
       default_sosa_ref=Gwdb.dummy_iper, None;multi_parents=false;
       can_send_image=false;authorized_wizards_notes=false;
       public_if_titles=false;public_if_no_date=false;
       cancel_links=false;setup_link=false;access_by_key=false;
       private_years=0;hide_names=false;use_restrict=false;
       no_image=false;no_note=false;bname="";cgi_passwd="";
       env=[];senv=[];henv=[];base_env=[];allowed_titles=lazy[];
       denied_titles=lazy[];xhs="";request=[];
       lexicon=Hashtbl.create 16;charset="";is_rtl=false;left="";
       right="";auth_file="";border=0;n_connect=None;
       today={Def.day=0;month=0;year=0;delta=0;prec=Def.Sure};
       today_wd=0;time=0,0,0;ctime=0.;image_prefix="";
       b_arg_for_basename=false}
    in
    bench
      "UpdateData.get_all_data"
      begin fun conf ->
            let base = Gwdb.open_base bname in
            let data = UpdateData.get_all_data conf base in
            Gwdb.close_base base ;
            data
      end
      [ { conf with Config.env = ["data","place"] } ]
  | _ -> ()
end
