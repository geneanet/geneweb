(* Copyright (c) 1998-2007 INRIA *)

module type MakeIn = sig
  val handler : RequestHandler.handler
end

module type MakeOut = sig
  val treat_request_on_base : Geneweb.Config.config -> unit
  val treat_request_on_nobase : Geneweb.Config.config -> unit
end

module Make (H : MakeIn) : MakeOut
