(* Copyright (c) 1998-2007 INRIA *)

open Gwdb

let rec _clear_descend_consang base consang mark ifam =
  let des = foi base ifam in
  Array.iter
    (fun ip ->
       if not (Gwdb.Marker.get mark ip) then
         begin
           Gwdb.Marker.set consang ip Adef.no_consang;
           Gwdb.Marker.set mark ip true ;
           let u = poi base ip in
           Array.iter (_clear_descend_consang base consang mark) (get_family u)
         end)
    (get_children des)

let _relationship base tab ip1 ip2 =
  fst (Consang.relationship_and_links base tab false ip1 ip2)

let _trace verbosity cnt max_cnt =
  if verbosity >= 2 then
    begin
      Printf.eprintf "%7d\008\008\008\008\008\008\008" cnt;
      flush stderr
    end
  else if verbosity >= 1 then
    ProgrBar.run (max_cnt - cnt) max_cnt

let compute ?(verbosity = 2) _base _tlim _from_scratch =
  let _ = verbosity in
  assert false
  (* let () = load_ascends_array base in
   * let () = load_couples_array base in
   * begin try
   *   let tab =
   *     let ts = Consang.topological_sort base poi in
   *     Consang.make_relationship_info base ts
   *   in
   *   let persons = Gwdb.ipers base in
   *   let families = Gwdb.ifams base in
   *   let consang_tab = Gwdb.ifam_marker families Adef.no_consang in
   *   let consang = Gwdb.iper_marker persons Adef.no_consang in
   *   let cnt = ref 0 in
   *   if not from_scratch then
   *     begin
   *       let mark = Gwdb.iper_marker persons false in
   *       List.iter
   *         (fun ip ->
   *            let u = poi base ip in
   *            Array.iter (clear_descend_consang base consang mark) (get_family u))
   *         (patched_ascends base)
   *     end;
   *   Gwdb.Collection.iter (fun i ->
   *        if from_scratch then begin cset i Adef.no_consang; incr cnt end
   *       else
   *         let cg = cget i in
   *         begin match fget i with
   *             Some ifam -> Gwdb.Marker.set consang_tab ifam cg
   *           | None -> ()
   *       end;
   *       if cg = Adef.no_consang then incr cnt
   *     ) persons ;
   *   let max_cnt = !cnt in
   *   let most = ref None in
   *   if verbosity >= 1 then Printf.eprintf "To do: %d persons\n" max_cnt;
   *   if max_cnt <> 0 then
   *     if verbosity >= 2 then begin
   *       Printf.eprintf "Computing consanguinity...";
   *       flush stderr
   *     end
   *     else if verbosity >= 1 then ProgrBar.start () ;
   *   let running = ref true in
   *   let end_time = Unix.time () +. float tlim in
   *   while !running && (tlim < 0 || Unix.time () < end_time) do
   *     running := false ;
   *     Gwdb.Collection.iter (fun i ->
   *         if cget i = Adef.no_consang then
   *           match fget i with
   *           Some ifam ->
   *           let pconsang = Gwdb.Marker.get consang_tab ifam in
   *           if pconsang = Adef.no_consang then
   *             let cpl = foi base ifam in
   *             let ifath = get_father cpl in
   *             let imoth = get_mother cpl in
   *             if cget ifath != Adef.no_consang
   *             && cget imoth != Adef.no_consang
   *             then
   *               let consang = relationship base tab ifath imoth in
   *               trace verbosity !cnt max_cnt;
   *               decr cnt;
   *               let cg = Adef.fix_of_float consang in
   *               cset i cg;
   *               Gwdb.Marker.set consang_tab ifam cg;
   *               (if verbosity >= 2 then
   *                  if match !most with Some m -> cg > cget m | None -> true then
   *                    begin
   *                      Printf.eprintf "\nMax consanguinity %g for %s... "
   *                        consang
   *                        (Gutil.designation base (poi base i));
   *                      flush stderr;
   *                      most := Some i
   *                    end)
   *             else running := true
   *           else
   *             begin trace verbosity !cnt max_cnt; decr cnt; cset i pconsang end
   *         | None ->
   *           trace verbosity !cnt max_cnt;
   *           decr cnt;
   *           cset i (Adef.fix_of_float 0.0)
   *       ) persons ;
   *   done;
   *   if max_cnt <> 0 then
   *     if verbosity >= 2 then begin
   *       Printf.eprintf " done   \n";
   *       flush stderr
   *     end
   *   else if verbosity >= 1 then ProgrBar.finish ()
   * with Sys.Break when verbosity > 0 -> Printf.eprintf "\n"; flush stderr; ()
   * end;
   * carray *)
