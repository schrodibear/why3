(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2013   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Stdlib
open Ident
open Ty
open Term

(** Types *)

type dty =
  | Dvar of dvar ref
  | Dapp of tysymbol * dty list
  | Duty of ty

and dvar =
  | Dind of int
  | Dval of dty

let dty_fresh = let i = ref 0 in fun () -> Dvar (ref (Dind (incr i; !i)))

let dty_of_ty ty = Duty ty

let rec ty_of_dty ~strict = function
  | Dvar { contents = Dval (Duty ty) } ->
      ty
  | Dvar ({ contents = Dval dty } as r) ->
      let ty = ty_of_dty ~strict dty in
      r := Dval (Duty ty); ty
  | Dvar r ->
      if strict then Loc.errorm "undefined type variable";
      let ty = ty_var (create_tvsymbol (id_fresh "xi")) in
      r := Dval (Duty ty); ty
  | Dapp (ts,dl) ->
      ty_app ts (List.map (ty_of_dty ~strict) dl)
  | Duty ty -> ty

let rec occur_check i = function
  | Dvar { contents = Dval d } -> occur_check i d
  | Dvar { contents = Dind j } -> if i = j then raise Exit
  | Dapp (_,dl) -> List.iter (occur_check i) dl
  | Duty _ -> ()

let rec dty_match dty ty = match dty, ty.ty_node with
  | Dvar { contents = Dval dty }, _ -> dty_match dty ty
  | Dvar r, _ -> r := Dval (Duty ty)
  | Duty ty1, _ when ty_equal ty1 ty -> ()
  | Dapp (ts1,dl), Tyapp (ts2,tl) when ts_equal ts1 ts2 ->
      List.iter2 dty_match dl tl
  | _ -> raise Exit

let rec dty_unify dty1 dty2 = match dty1,dty2 with
  | Dvar { contents = Dval dty1 }, dty2
  | dty1, Dvar { contents = Dval dty2 } -> dty_unify dty1 dty2
  | Dvar { contents = Dind i },
    Dvar { contents = Dind j } when i = j -> ()
  | Dvar ({ contents = Dind i } as r), dty
  | dty, Dvar ({ contents = Dind i } as r) ->
      occur_check i dty; r := Dval dty
  | dty, Duty ty | Duty ty, dty -> dty_match dty ty
  | Dapp (ts1,dl1), Dapp (ts2,dl2) when ts_equal ts1 ts2 ->
      List.iter2 dty_unify dl1 dl2
  | _ -> raise Exit

let dty_int = Duty ty_int
let dty_real = Duty ty_real
let dty_bool = Duty ty_bool

let rec print_dty ht inn fmt = function
  | Dvar { contents = Dval dty } ->
      print_dty ht inn fmt dty
  | Dvar { contents = Dind i } ->
      let tv = try Hint.find ht i with Not_found ->
        let tv = create_tvsymbol (id_fresh "xi") in
        Hint.replace ht i tv; tv in
      Pretty.print_tv fmt tv
  | Dapp (ts,dl) when is_ts_tuple ts ->
      Format.fprintf fmt "(%a)"
        (Pp.print_list Pp.comma (print_dty ht false)) dl
  | Dapp (ts,[]) ->
      Pretty.print_ts fmt ts
  | Dapp (ts,dl) when inn ->
      Format.fprintf fmt "(%a@ %a)"
        Pretty.print_ts ts (Pp.print_list Pp.space (print_dty ht true)) dl
  | Dapp (ts,dl) ->
      Format.fprintf fmt "%a@ %a"
        Pretty.print_ts ts (Pp.print_list Pp.space (print_dty ht true)) dl
  | Duty ({ty_node = Tyapp (ts,(_::_))} as ty)
    when inn && not (is_ts_tuple ts) ->
      Format.fprintf fmt "(%a)" Pretty.print_ty ty
  | Duty ty ->
      Pretty.print_ty fmt ty

let print_dty = let ht = Hint.create 3 in fun fmt dty ->
  print_dty ht false fmt dty

(** Symbols *)

let specialize_ls ls =
  let htv = Htv.create 3 in
  let find_tv tv = try Htv.find htv tv with Not_found ->
    let dtv = dty_fresh () in Htv.add htv tv dtv; dtv in
  let rec spec ty = match ty.ty_node with
    | Tyapp (ts, tyl) -> Dapp (ts, List.map spec tyl)
    | Tyvar tv -> find_tv tv in
  let spec ty = if ty_closed ty then Duty ty else spec ty in
  List.map spec ls.ls_args, Opt.map spec ls.ls_value

let specialize_fs ls =
  let dtyl, dty = specialize_ls ls in
  match dty with
  | Some dty -> dtyl, dty
  | None -> raise (FunctionSymbolExpected ls)

let specialize_cs ls =
  if ls.ls_constr = 0 then raise (ConstructorExpected ls);
  let dtyl, dty = specialize_ls ls in
  dtyl, Opt.get dty

let specialize_ps ls =
  let dtyl, dty = specialize_ls ls in
  match dty with
  | Some _ -> raise (PredicateSymbolExpected ls)
  | None -> dtyl

(** Patterns, terms, and formulas *)

type dpattern = {
  dp_node : dpattern_node;
  dp_dty  : dty;
  dp_loc  : Loc.position option;
}

and dpattern_node =
  | DPwild
  | DPvar of preid
  | DPapp of lsymbol * dpattern list
  | DPor of dpattern * dpattern
  | DPas of dpattern * preid

type dterm = {
  dt_node  : dterm_node;
  dt_dty   : dty option;
  dt_loc   : Loc.position option;
}

and dterm_node =
  | DTvar of string * dty
  | DTgvar of vsymbol
  | DTconst of Number.constant
  | DTapp of lsymbol * dterm list
  | DTif of dterm * dterm * dterm
  | DTlet of dterm * preid * dterm
  | DTcase of dterm * (dpattern * dterm) list
  | DTeps of preid * dty * dterm
  | DTquant of quant * (preid * dty) list * dterm list list * dterm
  | DTbinop of binop * dterm * dterm
  | DTnot of dterm
  | DTtrue
  | DTfalse
  | DTcast of dterm * ty
  | DTuloc of dterm * Loc.position
  | DTlabel of dterm * Slab.t

(** Environment *)

type denv = dterm_node Mstr.t

exception TermExpected
exception FmlaExpected
exception DuplicateVar of string
exception UnboundVar of string

let denv_get denv n = Mstr.find_exn (UnboundVar n) n denv

let denv_get_opt denv n = Mstr.find_opt n denv

let dty_of_dterm dt = match dt.dt_dty with
  | None -> Loc.error ?loc:dt.dt_loc TermExpected
  | Some dty -> dty

let denv_empty = Mstr.empty

let denv_add_var denv id dty =
  let n = preid_name id in
  Mstr.add n (DTvar (n, dty)) denv

let denv_add_let denv dt id =
  let n = preid_name id in
  Mstr.add n (DTvar (n, dty_of_dterm dt)) denv

let denv_add_quant denv vl =
  let add acc (id,dty) = let n = preid_name id in
    Mstr.add_new (DuplicateVar n) n (DTvar (n, dty)) acc in
  let s = List.fold_left add Mstr.empty vl in
  Mstr.set_union s denv

let denv_add_pat denv dp =
  let rec get dp = match dp.dp_node with
    | DPwild -> Mstr.empty
    | DPvar id ->
        let n = preid_name id in
        Mstr.singleton n (DTvar (n, dp.dp_dty))
    | DPapp (_,dpl) ->
        let join n _ _ = raise (DuplicateVar n) in
        let add acc dp = Mstr.union join acc (get dp) in
        List.fold_left add Mstr.empty dpl
    | DPor (dp1,dp2) ->
        let join _ dtn1 dtn2 = match dtn1, dtn2 with
          | DTvar (_,dty1), DTvar (_,dty2) -> dty_unify dty1 dty2; Some dtn1
          | _ -> assert false in
        Mstr.union join (get dp1) (get dp2)
    | DPas (dp,id) ->
        let n = preid_name id in
        Mstr.add_new (DuplicateVar n) n (DTvar (n, dp.dp_dty)) (get dp)
  in
  Mstr.set_union (get dp) denv

(** Unification tools *)

let dty_unify_app ls unify l1 l2 =
  try List.iter2 unify l1 l2 with Invalid_argument _ ->
    raise (BadArity (ls, List.length l2))

let dpat_expected_type dp dty =
  try dty_unify dp.dp_dty dty with Exit -> Loc.errorm ?loc:dp.dp_loc
    "This pattern has type %a,@ but is expected to have type %a"
    print_dty dp.dp_dty print_dty dty

let darg_expected_type ?loc dt_dty dty =
  try dty_unify dt_dty dty with Exit -> Loc.errorm ?loc
    "This term has type %a,@ but is expected to have type %a"
    print_dty dt_dty print_dty dty

let dterm_expected_type dt dty = match dt.dt_dty with
  | Some dt_dty -> darg_expected_type ?loc:dt.dt_loc dt_dty dty
  | None -> begin try dty_unify dty_bool dty with Exit ->
      Loc.error ?loc:dt.dt_loc TermExpected end

let dfmla_expected_type dt = match dt.dt_dty with
  | Some dt_dty -> begin try dty_unify dt_dty dty_bool with Exit ->
      Loc.error ?loc:dt.dt_loc FmlaExpected end
  | None -> ()

let dexpr_expected_type dt dty = match dty with
  | Some dty -> dterm_expected_type dt dty
  | None -> dfmla_expected_type dt

(** Constructors *)

let dpattern ?loc node =
  let get_dty = function
    | DPwild -> dty_fresh ()
    | DPvar _ -> dty_fresh ()
    | DPapp (ls,dpl) ->
        let dtyl, dty = specialize_cs ls in
        dty_unify_app ls dpat_expected_type dpl dtyl;
        dty
    | DPor (dp1,dp2) ->
        dpat_expected_type dp2 dp1.dp_dty;
        dp1.dp_dty
    | DPas (dp,_) ->
        dp.dp_dty in
  let dty = Loc.try1 ?loc get_dty node in
  { dp_node = node; dp_dty = dty; dp_loc = loc }

let dterm ?loc node =
  let get_dty = function
    | DTvar (_,dty) ->
        Some dty
    | DTgvar vs ->
        Some (dty_of_ty vs.vs_ty)
    | DTconst (Number.ConstInt _) ->
        Some dty_int
    | DTconst (Number.ConstReal _) ->
        Some dty_real
    | DTapp (ls,dtl) ->
        let dtyl, dty = specialize_ls ls in
        dty_unify_app ls dterm_expected_type dtl dtyl;
        dty
    | DTif (df,dt1,dt2) ->
        dfmla_expected_type df;
        dexpr_expected_type dt2 dt1.dt_dty;
        if dt2.dt_dty = None then None else dt1.dt_dty
    | DTlet (dt,_,df) ->
        ignore (dty_of_dterm dt);
        df.dt_dty
    | DTcase (_,[]) ->
        raise EmptyCase
    | DTcase (dt,(dp1,df1)::bl) ->
        dterm_expected_type dt dp1.dp_dty;
        let check (dp,df) =
          dpat_expected_type dp dp1.dp_dty;
          dexpr_expected_type df df1.dt_dty in
        List.iter check bl;
        let is_fmla (_,df) = df.dt_dty = None in
        if List.exists is_fmla bl then None else df1.dt_dty
    | DTeps (_,dty,df) ->
        dfmla_expected_type df;
        Some dty
    | DTquant (_,_,_,df) ->
        dfmla_expected_type df;
        None
    | DTbinop (_,df1,df2) ->
        dfmla_expected_type df1;
        dfmla_expected_type df2;
        None
    | DTnot df ->
        dfmla_expected_type df;
        None
    | DTtrue | DTfalse ->
        (* we put here [Some dty_bool] instead of [None] because we can
           always replace [true] by [True] and [false] by [False], so that
           there is no need to count these constructs as "formulas" which
           require explicit if-then-else conversion to bool *)
        Some dty_bool
    | DTcast (dt,ty) ->
        let dty = dty_of_ty ty in
        dterm_expected_type dt dty;
        Some dty
    | DTuloc (dt,_)
    | DTlabel (dt,_) ->
        dt.dt_dty in
  let dty = Loc.try1 ?loc get_dty node in
  { dt_node = node; dt_dty = dty; dt_loc = loc }

(** Final stage *)

let pattern ~strict env dp =
  let acc = ref Mstr.empty in
  let find_var id ty =
    let n = preid_name id in
    try Mstr.find n !acc with Not_found ->
      let vs = create_vsymbol id ty in
      acc := Mstr.add n vs !acc; vs in
  let rec get dp =
    Loc.try2 ?loc:dp.dp_loc try_get dp.dp_dty dp.dp_node
  and try_get dty = function
    | DPwild ->
        pat_wild (ty_of_dty ~strict dty)
    | DPvar id ->
        pat_var (find_var id (ty_of_dty ~strict dty))
    | DPapp (ls,dpl) ->
        pat_app ls (List.map get dpl) (ty_of_dty ~strict dty)
    | DPor (dp1,dp2) ->
        pat_or (get dp1) (get dp2)
    | DPas (dp,id) ->
        let pat = get dp in
        pat_as pat (find_var id pat.pat_ty) in
  let pat = get dp in
  Mstr.set_union !acc env, pat

let quant_vars ~strict env vl =
  let add acc (id,dty) =
    let n = preid_name id in
    let ty = ty_of_dty ~strict dty in
    let vs = create_vsymbol id ty in
    Mstr.add_new (DuplicateVar n) n vs acc, vs in
  let acc, vl = Lists.map_fold_left add Mstr.empty vl in
  Mstr.set_union acc env, vl

let check_used_var vars vs =
  if not (Mvs.mem vs vars) then
  let s = vs.vs_name.id_string in
  if not (String.length s > 0 && s.[0] = '_') then
  Warning.emit ?loc:vs.vs_name.id_loc "unused variable %s" s

let check_exists_implies q f = match q, f.t_node with
  | Texists, Tbinop (Timplies,_,_) -> Warning.emit ?loc:f.t_loc
      "form \"exists x. P -> Q\" is likely an error (use \"not P \\/ Q\" if not)"
  | _ -> ()

let term ~strict ~keep_loc env prop dt =
  let t_label loc labs t =
    if loc = None && Slab.is_empty labs
    then t else t_label ?loc labs t in

  let rec strip uloc labs dt = match dt.dt_node with
    | DTcast (dt,_) -> strip uloc labs dt
    | DTuloc (dt,loc) -> strip (Some loc) labs dt
    | DTlabel (dt,s) -> strip uloc (Slab.union labs s) dt
    | _ -> uloc, labs, dt in

  let rec get uloc env prop dt =
    let uloc, labs, dt = strip uloc Slab.empty dt in
    let tloc = if keep_loc then dt.dt_loc else None in
    let tloc = if uloc <> None then uloc else tloc in
    let t = t_label tloc labs (Loc.try5 ?loc:dt.dt_loc
      try_get uloc env prop dt.dt_dty dt.dt_node) in
    match t.t_ty with
    | Some _ when prop -> t_label tloc Slab.empty
        (Loc.try2 ?loc:dt.dt_loc t_equ t t_bool_true)
    | None when not prop -> t_label tloc Slab.empty
        (t_if t t_bool_true t_bool_false)
    | _ -> t

  and try_get uloc env prop dty = function
    | DTvar (n,_) ->
        t_var (Mstr.find_exn (UnboundVar n) n env)
    | DTgvar vs ->
        t_var vs
    | DTconst c ->
        t_const c
    | DTapp (ls,[]) when ls_equal ls fs_bool_true ->
        if prop then t_true else t_bool_true
    | DTapp (ls,[]) when ls_equal ls fs_bool_false ->
        if prop then t_false else t_bool_false
    | DTapp (ls,[dt1;dt2]) when ls_equal ls ps_equ ->
      (* avoid putting formulas into a term context *)
        if dt1.dt_dty = None || dt2.dt_dty = None
        then t_iff (get uloc env true dt1) (get uloc env true dt2)
        else t_equ (get uloc env false dt1) (get uloc env false dt2)
    | DTapp (ls,dtl) ->
        t_app ls (List.map (get uloc env false) dtl)
          (Opt.map (ty_of_dty ~strict) dty)
    | DTlet (dt,id,df) ->
        let prop = prop || dty = None in
        let t = get uloc env false dt in
        let v = create_vsymbol id (t_type t) in
        let env = Mstr.add (preid_name id) v env in
        let f = get uloc env prop df in
        check_used_var f.t_vars v;
        t_let_close v t f
    | DTif (df,dt1,dt2) ->
        let prop = prop || dty = None in
        t_if (get uloc env true df)
          (get uloc env prop dt1) (get uloc env prop dt2)
    | DTcase (dt,bl) ->
        let prop = prop || dty = None in
        let branch (dp,df) =
          let env, p = pattern ~strict env dp in
          let f = get uloc env prop df in
          Svs.iter (check_used_var f.t_vars) p.pat_vars;
          t_close_branch p f in
        t_case (get uloc env false dt) (List.map branch bl)
    | DTeps (id,dty,df) ->
        let v = create_vsymbol id (ty_of_dty ~strict dty) in
        let env = Mstr.add (preid_name id) v env in
        let f = get uloc env true df in
        check_used_var f.t_vars v;
        t_eps_close v f
    | DTquant (q,vl,dll,df) ->
        let env, vl = quant_vars ~strict env vl in
        let tr_get dt = get uloc env (dt.dt_dty = None) dt in
        let trl = List.map (List.map tr_get) dll in
        let f = get uloc env true df in
        List.iter (check_used_var f.t_vars) vl;
        check_exists_implies q f;
        t_quant_close q vl trl f
    | DTbinop (op,df1,df2) ->
        t_binary op (get uloc env true df1) (get uloc env true df2)
    | DTnot df ->
        t_not (get uloc env true df)
    | DTtrue ->
        if prop then t_true else t_bool_true
    | DTfalse ->
        if prop then t_false else t_bool_false
    | DTcast _ | DTuloc _ | DTlabel _ ->
        assert false (* already stripped *)
  in
  get None env prop dt

let fmla ~strict ~keep_loc env dt = term ~strict ~keep_loc env true dt
let term ~strict ~keep_loc env dt = term ~strict ~keep_loc env false dt

(** Exception printer *)

let () = Exn_printer.register (fun fmt e -> match e with
  | TermExpected ->
      Format.fprintf fmt "syntax error: term expected"
  | FmlaExpected ->
      Format.fprintf fmt "syntax error: formula expected"
  | DuplicateVar s ->
      Format.fprintf fmt "duplicate variable %s" s
  | UnboundVar s ->
      Format.fprintf fmt "unbound variable %s" s
  | _ -> raise e)
