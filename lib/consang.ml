(* Copyright (c) 1998-2007 INRIA *)

(* Algorithm relationship and links from Didier Remy *)

open Def
open Gwdb

type anc_stat = MaybeAnc | IsAnc

(* relationship:
   - elim_ancestor
        to prune displayed relationships
   - anc_stat1, anc_stat2
        optimization to answer faster when ancestors list is exhausted
   - lens1, lens2:
        the third parameter (list iper) of the list has been added to
        be able to reconstitute the branch in case of the sex of the
        persons in the branch is important to display the relationship
        text
*)

type relationship =
  { mutable weight1 : float;
    mutable weight2 : float;
    mutable relationship : float;
    mutable lens1 : (int * int * iper list) list;
    mutable lens2 : (int * int * iper list) list;
    mutable inserted : int;
    mutable elim_ancestors : bool;
    mutable anc_stat1 : anc_stat;
    mutable anc_stat2 : anc_stat }

type relationship_info =
  { tstab : (Gwdb.iper, int) Gwdb.Marker.t
  ; reltab : (Gwdb.iper, relationship) Gwdb.Marker.t
  ; mutable queue : Gwdb.iper list array
  }

let half x = x *. 0.5

type visit = NotVisited | BeingVisited | Visited

let rec noloop_aux base error tab i =
  match Gwdb.Marker.get tab i with
  | NotVisited ->
    begin match get_parents (poi base i) with
      | Some ifam ->
        let fam = foi base ifam in
        let fath = get_father fam in
        let moth = get_mother fam in
        Gwdb.Marker.set tab i BeingVisited ;
        noloop_aux base error tab fath ;
        noloop_aux base error tab moth
      | None -> ()
    end ;
    Gwdb.Marker.set tab i Visited
  | BeingVisited -> error (OwnAncestor (poi base i))
  | Visited -> ()

let check_noloop base error =
  let tab = Marker.make (nb_of_persons base) NotVisited in
  Gwdb.Collection.iter (noloop_aux base error tab) (Gwdb.ipers base)

let check_noloop_for_person_list base error list =
  let tab = Marker.make (nb_of_persons base) NotVisited in
  List.iter (noloop_aux base error tab) list

exception TopologicalSortError of person

(* Return tab such as: i is an ancestor of j => tab.(i) > tab.(j) *)
(* This complicated topological sort has the important following properties:
   - only "ascends" has to be loaded; no need to load "union" and "descend"
     which use much memory space.
   - the value of tab is minimum; it is important for the optimization of
     relationship computation (stopping the computation when the ancestor
     list of one of the person is exhausted).
*)

let topological_sort base poi =
  let persons = Gwdb.ipers base in
  let tab = Marker.make (nb_of_persons base) 0 in
  let cnt = ref 0 in
  Gwdb.Collection.iter (fun i ->
      let a = poi base i in
      match get_parents a with
        Some ifam ->
        let cpl = foi base ifam in
        let ifath = get_father cpl in
        let imoth = get_mother cpl in
        Gwdb.Marker.set tab ifath (Gwdb.Marker.get tab ifath + 1) ;
        Gwdb.Marker.set tab imoth (Gwdb.Marker.get tab imoth + 1) ;
      | _ -> ()
    )
    persons ;
  let todo =
    Gwdb.Collection.fold (fun acc i ->
        if Gwdb.Marker.get tab i = 0 then i :: acc else acc
      ) [] persons
  in
  let rec loop tval list =
    if list = [] then ()
    else
      let new_list =
        List.fold_left
          (fun new_list i ->
             let a = poi base i in
             Gwdb.Marker.set tab i tval ;
             incr cnt;
             match get_parents a with
               Some ifam ->
                 let cpl = foi base ifam in
                 let ifath = get_father cpl in
                 let imoth = get_mother cpl in
                 Gwdb.Marker.set tab ifath (Gwdb.Marker.get tab ifath - 1) ;
                 Gwdb.Marker.set tab imoth (Gwdb.Marker.get tab imoth - 1) ;
                 let new_list =
                   if Gwdb.Marker.get tab ifath = 0 then ifath :: new_list else new_list
                 in
                 if Gwdb.Marker.get tab imoth = 0 then imoth :: new_list else new_list
             | _ -> new_list)
          [] list
      in
      loop (tval + 1) new_list
  in
  loop 0 todo;
  if !cnt <> nb_of_persons base then
    check_noloop base
      (function
         OwnAncestor p -> raise (TopologicalSortError p)
       | _ -> assert false);
  tab

let phony_rel =
  {weight1 = 0.0; weight2 = 0.0; relationship = 0.0; lens1 = []; lens2 = [];
   inserted = 0; elim_ancestors = false; anc_stat1 = MaybeAnc;
   anc_stat2 = MaybeAnc}

let make_relationship_info base tstab =
  let tab = Marker.make (nb_of_persons base) phony_rel in
  {tstab = tstab; reltab = tab; queue = [| |]}

let rec insert_branch_len_rec (len, n, ip as x) =
  function
    [] -> [len, n, [ip]]
  | (len1, n1, ipl1 as y) :: lens ->
      if len = len1 then
        let n2 = n + n1 in
        let n2 = if n < 0 || n1 < 0 || n2 < 0 then -1 else n2 in
        (len1, n2, ip :: ipl1) :: lens
      else y :: insert_branch_len_rec x lens

let insert_branch_len ip lens (len, n, _ipl) =
  insert_branch_len_rec (succ len, n, ip) lens

let consang_of p =
  if get_consang p = Adef.no_consang then 0.0
  else Adef.float_of_fix (get_consang p)

let mark = ref 0
let new_mark () = incr mark; !mark

let relationship_and_links base ri b ip1 ip2 =
  let i1 = ip1 in
  let i2 = ip2 in
  if i1 = i2 then 1.0, []
  else
    let reltab = ri.reltab in
    let tstab = ri.tstab in
    let yes_inserted = new_mark () in
    let reset u =
      let tu = Gwdb.Marker.get reltab u in
      if tu == phony_rel then
        Gwdb.Marker.set reltab u
          {weight1 = 0.0; weight2 = 0.0; relationship = 0.0; lens1 = [];
           lens2 = []; inserted = yes_inserted; elim_ancestors = false;
           anc_stat1 = MaybeAnc; anc_stat2 = MaybeAnc}
      else
        begin
          tu.weight1 <- 0.0;
          tu.weight2 <- 0.0;
          tu.relationship <- 0.0;
          tu.lens1 <- [];
          tu.lens2 <- [];
          tu.inserted <- yes_inserted;
          tu.elim_ancestors <- false;
          tu.anc_stat1 <- MaybeAnc;
          tu.anc_stat2 <- MaybeAnc
        end
    in
    let qi = ref (min (Gwdb.Marker.get tstab i1) (Gwdb.Marker.get tstab i2)) in
    let qmax = ref (-1) in
    let insert u =
      let v = Gwdb.Marker.get tstab u in
      reset u;
      if v >= Array.length ri.queue then
        begin let len = Array.length ri.queue in
          ri.queue <- Array.append ri.queue (Array.make (v + 1 - len) [])
        end;
      if !qmax < 0 then
        begin
          for i = !qi to v - 1 do ri.queue.(i) <- [] done;
          qmax := v;
          ri.queue.(v) <- [u]
        end
      else
        begin
          if v > !qmax then
            begin
              for i = !qmax + 1 to v do ri.queue.(i) <- [] done;
              qmax := v
            end;
          ri.queue.(v) <- u :: ri.queue.(v)
        end
    in
    let relationship = ref 0.0 in
    let nb_anc1 = ref 1 in
    let nb_anc2 = ref 1 in
    let tops = ref [] in
    let treat_parent ip_from u y =
      if (Gwdb.Marker.get reltab y).inserted <> yes_inserted then insert y;
      let ty = Gwdb.Marker.get reltab y in
      let p1 = half u.weight1 in
      let p2 = half u.weight2 in
      if u.anc_stat1 = IsAnc && ty.anc_stat1 <> IsAnc then
        begin ty.anc_stat1 <- IsAnc; incr nb_anc1 end;
      if u.anc_stat2 = IsAnc && ty.anc_stat2 <> IsAnc then
        begin ty.anc_stat2 <- IsAnc; incr nb_anc2 end;
      ty.weight1 <- ty.weight1 +. p1;
      ty.weight2 <- ty.weight2 +. p2;
      ty.relationship <- ty.relationship +. p1 *. p2;
      if u.elim_ancestors then ty.elim_ancestors <- true;
      if b && not ty.elim_ancestors then
        begin
          ty.lens1 <-
            List.fold_left (insert_branch_len ip_from) ty.lens1 u.lens1;
          ty.lens2 <-
            List.fold_left (insert_branch_len ip_from) ty.lens2 u.lens2
        end
    in
    let treat_ancestor u =
      let tu = Gwdb.Marker.get reltab u in
      let a = poi base u in
      let contribution =
        tu.weight1 *. tu.weight2 -. tu.relationship *. (1.0 +. consang_of a)
      in
      if tu.anc_stat1 = IsAnc then decr nb_anc1;
      if tu.anc_stat2 = IsAnc then decr nb_anc2;
      relationship := !relationship +. contribution;
      if b && contribution <> 0.0 && not tu.elim_ancestors then
        begin tops := u :: !tops; tu.elim_ancestors <- true end;
      match get_parents a with
        Some ifam ->
          let cpl = foi base ifam in
          treat_parent u tu (get_father cpl);
          treat_parent u tu (get_mother cpl)
      | _ -> ()
    in
    insert i1;
    insert i2;
    (Gwdb.Marker.get reltab i1).weight1 <- 1.0;
    (Gwdb.Marker.get reltab i2).weight2 <- 1.0;
    (Gwdb.Marker.get reltab i1).lens1 <- [0, 1, []];
    (Gwdb.Marker.get reltab i2).lens2 <- [0, 1, []];
    (Gwdb.Marker.get reltab i1).anc_stat1 <- IsAnc;
    (Gwdb.Marker.get reltab i2).anc_stat2 <- IsAnc;
    while !qi <= !qmax && !nb_anc1 > 0 && !nb_anc2 > 0 do
      List.iter treat_ancestor ri.queue.(!qi);
      incr qi
    done;
    half !relationship, !tops
