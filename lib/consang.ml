(* $Id: consang.ml,v 5.13 2007-02-21 18:14:01 ddr Exp $ *)
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
  { tstab : int array;
    reltab : relationship array;
    mutable queue : int list array }

let half x = x *. 0.5

let mark = ref 0
let new_mark () = incr mark; !mark

type visit = NotVisited | BeingVisited | Visited

let check_noloop base error =
  let tab = Array.make (nb_of_persons base) NotVisited in
  let rec noloop i =
    match tab.(i) with
      NotVisited ->
        begin match get_parents (poi base (Adef.iper_of_int i)) with
          Some ifam ->
            let fam = foi base ifam in
            let fath = get_father fam in
            let moth = get_mother fam in
            tab.(i) <- BeingVisited;
            noloop (Adef.int_of_iper fath);
            noloop (Adef.int_of_iper moth)
        | None -> ()
        end;
        tab.(i) <- Visited
    | BeingVisited -> error (OwnAncestor (poi base (Adef.iper_of_int i)))
    | Visited -> ()
  in
  for i = 0 to nb_of_persons base - 1 do
    match tab.(i) with
      NotVisited -> noloop i
    | BeingVisited -> failwith "check_noloop algorithm error"
    | Visited -> ()
  done

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
  let tab = Array.make (nb_of_persons base) 0 in
  let cnt = ref 0 in
  for i = 0 to nb_of_persons base - 1 do
    let a = poi base (Adef.iper_of_int i) in
    match get_parents a with
      Some ifam ->
        let cpl = foi base ifam in
        let ifath = Adef.int_of_iper (get_father cpl) in
        let imoth = Adef.int_of_iper (get_mother cpl) in
        tab.(ifath) <- tab.(ifath) + 1; tab.(imoth) <- tab.(imoth) + 1
    | _ -> ()
  done;
  let todo =
    let stop = nb_of_persons base in
    let rec loop i acc =
      if i >= stop then acc else
      loop (i + 1) (if tab.(i) = 0 then i :: acc else acc)
    in
    loop 0 []
  in
  let rec loop tval list =
    if list = [] then ()
    else
      let new_list =
        List.fold_left
          (fun new_list i ->
             let a = poi base (Adef.iper_of_int i) in
             tab.(i) <- tval;
             incr cnt;
             match get_parents a with
               Some ifam ->
                 let cpl = foi base ifam in
                 let ifath = Adef.int_of_iper (get_father cpl) in
                 let imoth = Adef.int_of_iper (get_mother cpl) in
                 tab.(ifath) <- tab.(ifath) - 1;
                 tab.(imoth) <- tab.(imoth) - 1;
                 let new_list =
                   if tab.(ifath) = 0 then ifath :: new_list else new_list
                 in
                 if tab.(imoth) = 0 then imoth :: new_list else new_list
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

let check_noloop_for_person_list base error ipl =
  let tab = Array.make (nb_of_persons base) NotVisited in
  let rec noloop ip =
    let i = Adef.int_of_iper ip in
    match tab.(i) with
      NotVisited ->
        begin match get_parents (poi base ip) with
          Some ifam ->
            let cpl = foi base ifam in
            tab.(i) <- BeingVisited; Array.iter noloop (get_parent_array cpl)
        | None -> ()
        end;
        tab.(i) <- Visited
    | BeingVisited -> error (OwnAncestor (poi base ip))
    | Visited -> ()
  in
  List.iter noloop ipl

let phony_rel =
  {weight1 = 0.0; weight2 = 0.0; relationship = 0.0; lens1 = []; lens2 = [];
   inserted = 0; elim_ancestors = false; anc_stat1 = MaybeAnc;
   anc_stat2 = MaybeAnc}

let make_relationship_info base tstab =
  let tab = Array.make (nb_of_persons base) phony_rel in
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

let relationship_and_links base ri b ip1 ip2 =
  let i1 = Adef.int_of_iper ip1 in
  let i2 = Adef.int_of_iper ip2 in
  if i1 = i2 then 1.0, []
  else
    let reltab = ri.reltab in
    let tstab = ri.tstab in
    let yes_inserted = new_mark () in
    let reset u =
      let tu = reltab.(u) in
      if tu == phony_rel then
        reltab.(u) <-
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
    let qi = ref (min tstab.(i1) tstab.(i2)) in
    let qmax = ref (-1) in
    let insert u =
      let v = tstab.(u) in
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
      if reltab.(y).inserted <> yes_inserted then insert y;
      let ty = reltab.(y) in
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
      let tu = reltab.(u) in
      let a = poi base (Adef.iper_of_int u) in
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
          treat_parent (Adef.iper_of_int u) tu
            (Adef.int_of_iper (get_father cpl));
          treat_parent (Adef.iper_of_int u) tu
            (Adef.int_of_iper (get_mother cpl))
      | _ -> ()
    in
    insert i1;
    insert i2;
    reltab.(i1).weight1 <- 1.0;
    reltab.(i2).weight2 <- 1.0;
    reltab.(i1).lens1 <- [0, 1, []];
    reltab.(i2).lens2 <- [0, 1, []];
    reltab.(i1).anc_stat1 <- IsAnc;
    reltab.(i2).anc_stat2 <- IsAnc;
    while !qi <= !qmax && !nb_anc1 > 0 && !nb_anc2 > 0 do
      List.iter treat_ancestor ri.queue.(!qi);
      incr qi
    done;
    half !relationship, !tops
