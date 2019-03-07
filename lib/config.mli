(* $Id: config.mli,v 5.19 2007-07-25 14:19:55 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def

type auth_scheme_kind =
    NoAuth
  | TokenAuth of token_auth_scheme
  | HttpAuth of http_auth_scheme
and token_auth_scheme = { ts_user : string; ts_pass : string }
and http_auth_scheme =
    Basic of basic_auth_scheme
  | Digest of digest_auth_scheme
and basic_auth_scheme =
  { bs_realm : string; bs_user : string; bs_pass : string }
and digest_auth_scheme =
  { ds_username : string;
    ds_realm : string;
    ds_nonce : string;
    ds_meth : string;
    ds_uri : string;
    ds_qop : string;
    ds_nc : string;
    ds_cnonce : string;
    ds_response : string }

type config =
  { from : string;
#ifdef API
    api_host : string;
    api_port : int;
#endif
    manitou : bool;
    supervisor : bool;
    wizard : bool;
    is_printed_by_template : bool;
    friend : bool;
    just_friend_wizard : bool;
    user : string;
    username : string;
    auth_scheme : auth_scheme_kind;
    pure_xhtml : bool;
    command : string;
    indep_command : string;
    highlight : string;
    lang : string;
    default_lang : string;
    default_sosa_ref : iper * Gwdb.person option;
    multi_parents : bool;
    can_send_image : bool;
    authorized_wizards_notes : bool;
    public_if_titles : bool;
    public_if_no_date : bool;
    mutable cancel_links : bool;
    mutable setup_link : bool;
    access_by_key : bool;
    private_years : int;
    hide_names : bool;
    use_restrict : bool;
    no_image : bool;
    no_note : bool;
    bname : string;
#ifdef CGI
    cgi_passwd : string;
#endif
    env : (string * string) list;
    mutable senv : (string * string) list;
    mutable henv : (string * string) list;
    base_env : (string * string) list;
    allowed_titles : string list Lazy.t;
    denied_titles : string list Lazy.t;
    xhs : string;
    request : string list;
    lexicon : (string, string) Hashtbl.t;
    mutable charset : string;
    is_rtl : bool;
    left : string;
    right : string;
    auth_file : string;
    border : int;
    mutable n_connect : (int * int * int * (string * float) list) option;
    today : dmy;
    today_wd : int;
    time : int * int * int;
    ctime : float;

    (* prefix for image urls:
       the value of argument -images_url if specified, otherwise
       command ^ "?m=IM&v=" in CGI mode
       "images" otherwise *)
    image_prefix : string;
}
