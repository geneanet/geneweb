open Geneweb

let bench name n fn arg =
  ignore @@ Benchmark.latency1 ~name n (List.map fn) arg

let list =
  [1;2;10;100;1000;10000;100000;1000000;10000000;100000000;1000000000]

let sosa_list =
  List.map Sosa.of_int list

let () =
  bench "Sosa.gen" 1000000L (List.map Sosa.gen) [ sosa_list ]
; bench "Sosa.to_string_sep" 1000000L (List.map @@ Sosa.to_string_sep ",") [ sosa_list ]
; bench "Sosa.to_string" 1000000L (List.map Sosa.to_string) [ sosa_list ]
; bench "Sosa.of_string" 1000000L (List.map Sosa.of_string) [ List.map string_of_int list ]
; bench "Sosa.branches" 1000000L (List.map Sosa.branches) [ sosa_list ]
; bench "Place.normalize" 5000000L Geneweb.Place.normalize
    [ "[foo-bar] - boobar (baz)" ; "[foo-bar] – boobar (baz)" ; "[foo-bar] — boobar (baz)" ]
; bench "Mutil.unsafe_tr" 10000000L (fun s -> Mutil.unsafe_tr 'a' 'b' @@ "a" ^ s)
    [ "aaaaaaaaaa" ; "bbbbbbbbbb" ; "abbbbbbbb" ; "bbbbbbbbba" ; "ababababab" ]
; bench "Mutil.tr" 10000000L (fun s -> Mutil.tr 'a' 'b' @@ "a" ^ s)
    [ "aaaaaaaaaa" ; "bbbbbbbbbb" ; "abbbbbbbb" ; "bbbbbbbbba" ; "ababababab" ]
; bench "Mutil.split_fname" 10000000L Mutil.split_fname
    [ "Jean-Baptiste Emmanuel" ; "Jean Baptiste Emmanuel" ]
; bench "Name.concat" 100000000L (fun (a, b) -> Name.concat a b)
    [ ("foo", "bar")
    ; ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      , "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb") ]

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
      "UpdateData.get_all_data" 500L
      begin fun conf ->
            let base = Gwdb.open_base bname in
            let data = UpdateData.get_all_data conf base in
            Gwdb.close_base base ;
            data
      end
      [ { conf with Config.env = ["data","place"] } ]
  | _ -> ()
end
