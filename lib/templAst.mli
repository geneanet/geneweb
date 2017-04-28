(* $Id: templAst.mli,v 5.4 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type ast =
  | Atext of loc * string
  | Avar of loc * string * string list
  | Atransl of loc * bool * string * string
  | Aconcat of loc * ast list
  | Awid_hei of string
  | Aif of ast * ast list * ast list
  | Aforeach of (loc * string * string list) * (ast list) list * ast list
  | Afor of string * ast * ast * ast list
  | Adefine of string * string list * ast list * ast list
  | Aapply of loc * string * (ast list) list
  | Alet of string * ast list * ast list
  | Aop1 of loc * string * ast
  | Aop2 of loc * string * ast * ast
  | Aint of loc * string
  | Ainclude of string * ast list
and loc = (int * int)


type 'a expr_val =
  | VVbool of bool
  | VVstring of string
  | VVother of (string list -> 'a expr_val)

