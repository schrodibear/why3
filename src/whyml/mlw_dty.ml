(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-2012                                               *)
(*    François Bobot                                                      *)
(*    Jean-Christophe Filliâtre                                           *)
(*    Claude Marché                                                       *)
(*    Guillaume Melquiond                                                 *)
(*    Andrei Paskevich                                                    *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* destructive types for program type inference *)

open Why3
open Util
open Ident
open Ty
open Term
open Ptree
open Mlw_ty
open Mlw_ty.T
open Mlw_expr
open Mlw_module

let create_user_tv =
  let hs = Hashtbl.create 17 in
  fun s -> try Hashtbl.find hs s with Not_found ->
  let tv = create_tvsymbol (id_fresh s) in
  Hashtbl.add hs s tv;
  tv

type dity =
  | Dvar  of dvar ref
  | Duvar of tvsymbol
  | Dits  of itysymbol * dity list * dreg list
  | Dts   of tysymbol  * dity list

and dvar =
  | Dtvs  of tvsymbol
  | Dval  of dity

and dreg =
  | Rreg  of region * dity
  | Rureg of tvsymbol * dity * region Lazy.t
  | Rvar  of rvar ref

and rvar =
  | Rtvs  of tvsymbol * dity * region Lazy.t
  | Rval  of dreg

let rec ity_of_dity = function
  | Dvar { contents = Dtvs tv } -> ity_var tv
  | Dvar { contents = Dval dty } -> ity_of_dity dty
  | Duvar tv -> ity_var tv
  | Dits (its,dl,rl) ->
      ity_app its (List.map ity_of_dity dl) (List.map reg_of_dreg rl)
  | Dts (ts,dl) ->
      ity_pur ts (List.map ity_of_dity dl)

and reg_of_dreg = function
  | Rreg (r,_) -> r
  | Rureg (_,_,r)
  | Rvar { contents = Rtvs (_,_,r) } -> Lazy.force r
  | Rvar { contents = Rval dreg } -> reg_of_dreg dreg

let create_user_type_variable x =
  Duvar (create_user_tv x.id)

let create_type_variable () =
  Dvar (ref (Dtvs (create_tvsymbol (id_fresh "a"))))

let create_dreg ~user dity =
  let id = id_fresh (if user then "urho" else "rho") in
  let tv = create_tvsymbol id in
  let reg = lazy (create_region id (ity_of_dity dity)) in
  if user then Rureg (tv,dity,reg) else Rvar (ref (Rtvs (tv,dity,reg)))

let ts_app_real ts dl = Dts (ts, dl)

let its_app_real its dl rl = Dits (its, dl, rl)

let rec ity_inst_fresh ~user mv mr ity = match ity.ity_node with
  | Ityvar v ->
      mr, Mtv.find v mv
  | Itypur (s,tl) ->
      let mr,tl = Util.map_fold_left (ity_inst_fresh ~user mv) mr tl in
      mr, ts_app_real s tl
  | Ityapp (s,tl,rl) ->
      let mr,tl = Util.map_fold_left (ity_inst_fresh ~user mv) mr tl in
      let mr,rl = Util.map_fold_left (reg_refresh ~user mv) mr rl in
      mr, its_app_real s tl rl

and reg_refresh ~user mv mr r = match Mreg.find_opt r mr with
  | Some r ->
      mr, r
  | None ->
      let mr,ity = ity_inst_fresh ~user mv mr r.reg_ity in
      let reg = create_dreg ~user ity in
      Mreg.add r reg mr, reg

let its_app ~user s tl =
  let add m v t = Mtv.add v t m in
  let mv = try List.fold_left2 add Mtv.empty s.its_args tl
    with Invalid_argument _ ->
      raise (BadItyArity (s, List.length s.its_args, List.length tl))
  in
  match s.its_def with
  | Some ity ->
      snd (ity_inst_fresh ~user mv Mreg.empty ity)
  | None ->
      let _, rl =
        Util.map_fold_left (reg_refresh ~user mv) Mreg.empty s.its_regs in
      its_app_real s tl rl

let ts_app ts dl = match ts.ts_def with
  | Some ty ->
      let add m v t = Mtv.add v t m in
      let mv = try List.fold_left2 add Mtv.empty ts.ts_args dl
        with Invalid_argument _ ->
          raise (BadTypeArity (ts, List.length ts.ts_args, List.length dl)) in
      snd (ity_inst_fresh ~user:true mv Mreg.empty (ity_of_ty ty))
  | None ->
      ts_app_real ts dl

(* unification *)

let rec occur_check tv = function
  | Dvar { contents = Dval d } -> occur_check tv d
  | Dvar { contents = Dtvs tv' } -> if tv_equal tv tv' then raise Exit
  | Dits (_,dl,_) | Dts (_,dl) -> List.iter (occur_check tv) dl
  | Duvar _ -> ()

let rec unify d1 d2 = match d1,d2 with
  | Dvar { contents = Dval d1 }, d2
  | d1, Dvar { contents = Dval d2 } ->
      unify d1 d2
  | Dvar ({ contents = Dtvs tv } as r), d
  | d, Dvar ({ contents = Dtvs tv } as r) ->
      occur_check tv d; r := Dval d
  | Duvar tv1, Duvar tv2 when tv_equal tv1 tv2 -> ()
  | Dits (its1, dl1, rl1), Dits (its2, dl2, rl2) when its_equal its1 its2 ->
      assert (List.length rl1 = List.length rl2);
      assert (List.length dl1 = List.length dl2);
      List.iter2 unify dl1 dl2;
      List.iter2 unify_reg rl1 rl2
  | Dts (ts1, dl1), Dts (ts2, dl2) when ts_equal ts1 ts2 ->
      assert (List.length dl1 = List.length dl2);
      List.iter2 unify dl1 dl2
  | _ -> raise Exit

and unify_reg r1 r2 =
  let rec dity_of_reg = function
    | Rvar { contents = Rval r } -> dity_of_reg r
    | Rvar { contents = Rtvs (_,dity,_) }
    | Rureg (_,dity,_) | Rreg (_,dity) -> dity
  in
  match r1,r2 with
    | Rvar { contents = Rval r1 }, r2
    | r1, Rvar { contents = Rval r2 } -> unify_reg r1 r2
    | Rvar r, d | d, Rvar r ->
        unify (dity_of_reg r1) (dity_of_reg r2);
        r := Rval d
    | Rureg (tv1,_,_), Rureg (tv2,_,_) when tv_equal tv1 tv2 -> ()
    | Rreg (reg1,_), Rreg (reg2,_) when reg_equal reg1 reg2 -> ()
    | _ -> raise Exit

let unify d1 d2 =
  try unify d1 d2
  with Exit -> raise (TypeMismatch (ity_of_dity d1, ity_of_dity d2))

let ts_arrow =
  let v = List.map (fun s -> create_tvsymbol (Ident.id_fresh s)) ["a"; "b"] in
  Ty.create_tysymbol (Ident.id_fresh "arrow") v None

let rec vty_of_dity = function
  | Dts (ts, [d1; d2]) when ts_equal ts ts_arrow ->
      VTarrow (vty_arrow (vty_value (ity_of_dity d1)) (vty_of_dity d2))
  | dity ->
      VTvalue (vty_value (ity_of_dity dity))

type tvars = Stv.t (* a set of type variables *)
let empty_tvars = Stv.empty

let rec add_tvars tvs = function
  | Dvar { contents = Dval d } -> add_tvars tvs d
  | Dvar { contents = Dtvs tv } | Duvar tv -> Stv.add tv tvs
  | Dits (_,dl,rl) ->
      List.fold_left add_tvars_reg (List.fold_left add_tvars tvs dl) rl
  | Dts (_, dl) ->
      List.fold_left add_tvars tvs dl

and add_tvars_reg tvs = function
  | Rvar { contents = Rval r } -> add_tvars_reg tvs r
  | Rvar { contents = Rtvs (tv,dity,_) }
  | Rureg (tv,dity,_) -> add_tvars (Stv.add tv tvs) dity
  | Rreg _ -> tvs

let specialize_scheme tvs dity =
  let htvs = Htv.create 17 in
  let hreg = Htv.create 17 in
  let rec specialize = function
    | Dvar { contents = Dval d } -> specialize d
    | Dvar { contents = Dtvs tv } | Duvar tv as d ->
        if Stv.mem tv tvs then d else
        begin try Htv.find htvs tv with Not_found ->
          let v = create_type_variable () in
          Htv.add htvs tv v; v
        end
    | Dits (its, dl, rl) ->
        its_app_real its (List.map specialize dl) (List.map spec_reg rl)
    | Dts (ts, dl) ->
        ts_app_real ts (List.map specialize dl)
  and spec_reg = function
    | Rvar { contents = Rval r } -> spec_reg r
    | Rvar { contents = Rtvs (tv,dity,_) }
    | Rureg (tv,dity,_) as r ->
        if Stv.mem tv tvs then r else
        begin try Htv.find hreg tv with Not_found ->
          let v = create_dreg ~user:false (specialize dity) in
          Htv.add hreg tv v; v
        end
    | Rreg _ as r -> r
  in
  specialize dity

(* Specialization of symbols *)

let rec dity_of_ity ~user htv hreg ity = match ity.ity_node with
  | Ityvar tv ->
      assert (not user);
      begin try Htv.find htv tv with Not_found ->
        let dtv = create_type_variable () in
        Htv.add htv tv dtv;
        dtv
      end
  | Itypur (ts, ityl) ->
      ts_app_real ts (List.map (dity_of_ity ~user htv hreg) ityl)
  | Ityapp (its, ityl, rl) ->
      its_app_real its (List.map (dity_of_ity ~user htv hreg) ityl)
        (List.map (dreg_of_reg ~user htv hreg) rl)

and dreg_of_reg ~user htv hreg r =
  try Hreg.find hreg r with Not_found ->
  let dreg = create_dreg ~user (dity_of_ity ~user htv hreg r.reg_ity) in
  Hreg.add hreg r dreg;
  dreg

let dity_of_vtv ~user htv hreg v = dity_of_ity ~user htv hreg v.vtv_ity

let specialize_vtvalue ~user vtv =
  let htv = Htv.create 17 in
  let hreg = Hreg.create 17 in
  dity_of_vtv ~user htv hreg vtv

let specialize_pvsymbol pv =
  specialize_vtvalue ~user:true pv.pv_vtv

let make_arrow_type tyl ty =
  let arrow ty1 ty2 = ts_app_real ts_arrow [ty1;ty2] in
  List.fold_right arrow tyl ty

let specialize_vtarrow vta =
  let htv = Htv.create 17 in
  let hreg = Hreg.create 17 in
  let rec specialize a =
    let arg = dity_of_vtv ~user:false htv hreg a.vta_arg in
    let res = match a.vta_result with
      | VTvalue v -> dity_of_vtv ~user:false htv hreg v
      | VTarrow a1 -> specialize a1
    in
    make_arrow_type [arg] res
  in
  specialize vta

let specialize_psymbol ps = specialize_vtarrow ps.ps_vta

let specialize_plsymbol pls =
  let htv = Htv.create 17 in
  let hreg = Hreg.create 17 in
  let args = List.map (dity_of_vtv ~user:false htv hreg) pls.pl_args in
  make_arrow_type args (dity_of_vtv ~user:false htv hreg pls.pl_value)

let dity_of_ty ~user htv hreg ty = dity_of_ity ~user htv hreg (ity_of_ty ty)

let specialize_lsymbol ls =
  let htv = Htv.create 17 in
  let hreg = Hreg.create 17 in
  let ty = match ls.ls_value with
    | None -> dity_of_ity ~user:false htv hreg ity_bool
    | Some ty -> dity_of_ty ~user:false htv hreg ty
  in
  let args = List.map (dity_of_ty ~user:false htv hreg) ls.ls_args in
  make_arrow_type args ty
