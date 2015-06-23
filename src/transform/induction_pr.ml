(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2015   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Ident
open Ty
open Term
open Decl
open Theory
open Task

let lab_ind = create_label "induction"
let lab_inv = create_label "inversion"

type context = {
 c_node : context_node;
 c_label: Slab.t;
 c_loc:   Loc.position option }

and context_node =
 | Hole
 | Clet of vsymbol * term * context
   (* triggers are forgotten on purpose *)
 | Cforall of vsymbol list * context
 | Cimplies of term * context


exception Ind_not_found

let make_context node term =
 {c_node = node ; c_label = term.t_label; c_loc = term.t_loc }


let make_context_ctx node context =
 {c_node = node ; c_label = context.c_label; c_loc = context.c_loc }

let locate kn label t =
  let rec locate_inductive find_any t = match t.t_node with
    | Tlet (t1, tb) ->
      let vs,t2 = t_open_bound tb in
      let ctx, ind, goal = locate_inductive find_any t2 in
      make_context (Clet (vs, t1, ctx)) t, ind, goal
    | Tquant(Tforall, tq) ->
      let vsl, _, t1 = t_open_quant tq in
      let ctx, ind, goal = locate_inductive find_any t1 in
      make_context ( Cforall (vsl, ctx)) t, ind, goal
    | Tbinop (Timplies, lhs, rhs) ->
      let locate_rhs find_any =
        let ctx, ind, goal = locate_inductive find_any rhs in
        make_context (Cimplies (lhs, ctx)) t, ind, goal in
      let slab () = Slab.mem label lhs.t_label
      in if find_any || (slab ())
        then
          match lhs.t_node with
            | Tapp (ls, argl) ->
              begin
                match (Mid.find ls.ls_name kn).d_node with
                | Dind (Ind, dl) ->
                  let cl = List.assq ls dl in
                  if find_any && not (slab ()) then
                    (* take first labeled inductive in rhs if any.
                       Otherwise, take lhs*)
                    try
                      locate_rhs false
                    with Ind_not_found ->
                      make_context Hole t, (ls, argl, cl), rhs
                  else
                    (*here a label has been found*)
                    make_context Hole t, (ls, argl, cl), rhs
                | Dind _ | Dlogic _ | Dparam _ | Ddata _ -> locate_rhs find_any
                | Dtype _ | Dprop _ -> assert false
              end
            | _  -> locate_rhs find_any
        else locate_rhs find_any
    | _  -> raise Ind_not_found
  in locate_inductive true t

let partition ctx vsi =
  let rec aux ctx vsi_acc cindep cdep = match ctx.c_node with
    | Hole -> cindep, cdep
    | Cimplies (t, ctx2) ->
      let add c = make_context_ctx (Cimplies (t, c)) ctx in
      let cindep, cdep =
        let fvs = t_vars t in
        if Mvs.is_empty (Mvs.set_inter fvs vsi_acc)
        then add cindep, cdep
        else cindep, add cdep in
      aux ctx2 vsi_acc cindep cdep
    | Cforall (vsl, ctx2) ->
      let add c = function
        | []  -> c
        | vl  -> make_context_ctx (Cforall (vl, c)) ctx in
      let vsl = List.filter (fun v -> not (Mvs.mem v vsi)) vsl in
      let vdep, vindep = List.partition (fun v -> Mvs.mem v vsi_acc) vsl in
      aux ctx2 vsi_acc (add cindep vindep) (add cdep vdep)
    | Clet (vs, t, ctx2) ->
      if Mvs.mem vs vsi
      then
        let t = t_equ (t_var vs) t in
        let cdep = make_context_ctx (Cimplies (t, cdep)) ctx in
        aux ctx2 vsi_acc cindep cdep
      else
        let add c = make_context_ctx (Clet (vs, t, c)) ctx in
        let fvs = t_vars t in
        if Mvs.is_empty (Mvs.set_inter fvs vsi_acc)
        then aux ctx2 vsi_acc (add cindep) cdep
        else aux ctx2 (Mvs.add vs 1 vsi_acc) cindep (add cdep)
  in
  let hole = make_context_ctx Hole ctx in
  aux ctx vsi hole hole



let introduce_equalities vsi paraml argl goal =
  let goal =
    List.fold_left2 (fun g p a -> t_implies (t_equ a p) g) goal paraml argl in
  t_forall_close (Mvs.keys vsi) [] goal

let rec zip ctx goal = match ctx.c_node with
  | Hole -> goal
  | Cimplies (t, ctx2)  ->
    zip ctx2 (t_label ?loc:ctx.c_loc ctx.c_label (t_implies t goal))
  | Cforall (vsl, ctx2) ->
    zip ctx2 (t_label ?loc:ctx.c_loc ctx.c_label (t_forall_close vsl [] goal))
  | Clet (vs, t, ctx2)  ->
    zip ctx2 (t_label ?loc:ctx.c_loc ctx.c_label (t_let_close vs t goal))



let substitute_clause induct vsi ls argl goal c =
  let sigma = ls_arg_inst ls argl in
  let c = t_ty_subst sigma Mvs.empty c in
  let rec subst keepi t = match t.t_node with
    | Tapp (ls', paraml) when ls_equal ls ls' ->
      let t2 () = introduce_equalities vsi paraml argl goal in
        if keepi then
          if induct && List.for_all2
            (fun a b -> Opt.equal ty_equal a.t_ty b.t_ty) argl paraml
          then t_and t (t2 ())
          (* in case of polymorphic recursion we do not generate IHs *)
          else t
        else t2 ()
    | _  -> t_map (subst keepi) t in
  let rec aux t = match t.t_node with
    | Tlet (t1, tb) ->
      let vs, t2, cb = t_open_bound_cb tb in
      t_label_copy t (t_let t1 (cb vs (aux t2)))
    | Tquant(Tforall, tq) ->
      let vsl, tr, t1, cb = t_open_quant_cb tq in
      t_label_copy t (t_forall (cb vsl tr (aux t1)))
    | Tbinop (Timplies, lhs, rhs) ->
      t_label_copy t (t_implies (subst true lhs) (aux rhs))
    | _ -> subst false t
  in aux c



let induction_l label induct kn t =
  let (ctx, (ls, argl, cl), goal) = locate kn label t in
  let vsi = t_vars (t_app_infer ls argl) in
  let cindep, cdep = partition ctx vsi in
  let goal = zip cdep goal in
  List.map (fun (_,c) ->
    zip cindep (substitute_clause induct vsi ls argl goal c)) cl


let induction_l label induct task = match task with
  | Some { task_decl ={ td_node = Decl { d_node = Dprop (Pgoal, pr, f) } };
	   task_prev = prev;
	   task_known = kn } ->
    begin try List.map (add_prop_decl prev Pgoal pr) (induction_l label induct kn f)
    with Ind_not_found -> [task] end
  | _ -> assert false



let () =
  Trans.register_transform_l "induction_pr" (Trans.store (induction_l lab_ind true))
    ~desc:"Generate@ induction@ hypotheses@ for@ goals@ over@ inductive@ predicates."

let () =
  Trans.register_transform_l "inversion_pr" (Trans.store (induction_l lab_inv false))
    ~desc:"Invert@ inductive@ predicate."


(*
Local Variables:
compile-command: "unset LANG; make -C ../.. bin/why3.byte"
End:
*)