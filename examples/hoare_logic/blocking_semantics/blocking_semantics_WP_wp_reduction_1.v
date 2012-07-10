(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.

(* Why3 assumption *)
Definition implb(x:bool) (y:bool): bool := match (x,
  y) with
  | (true, false) => false
  | (_, _) => true
  end.

(* Why3 assumption *)
Inductive datatype  :=
  | TYunit : datatype 
  | TYint : datatype 
  | TYbool : datatype .

(* Why3 assumption *)
Inductive value  :=
  | Vvoid : value 
  | Vint : Z -> value 
  | Vbool : bool -> value .

(* Why3 assumption *)
Inductive operator  :=
  | Oplus : operator 
  | Ominus : operator 
  | Omult : operator 
  | Ole : operator .

Parameter ident : Type.

Parameter result: ident.

(* Why3 assumption *)
Inductive term  :=
  | Tvalue : value -> term 
  | Tvar : ident -> term 
  | Tderef : ident -> term 
  | Tbin : term -> operator -> term -> term .

(* Why3 assumption *)
Inductive fmla  :=
  | Fterm : term -> fmla 
  | Fand : fmla -> fmla -> fmla 
  | Fnot : fmla -> fmla 
  | Fimplies : fmla -> fmla -> fmla 
  | Flet : ident -> term -> fmla -> fmla 
  | Fforall : ident -> datatype -> fmla -> fmla .

Parameter map : forall (a:Type) (b:Type), Type.

Parameter get: forall (a:Type) (b:Type), (map a b) -> a -> b.
Implicit Arguments get.

Parameter set: forall (a:Type) (b:Type), (map a b) -> a -> b -> (map a b).
Implicit Arguments set.

Axiom Select_eq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) -> ((get (set m a1 b1)
  a2) = b1).

Axiom Select_neq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1)
  a2) = (get m a2)).

Parameter const: forall (a:Type) (b:Type), b -> (map a b).
Set Contextual Implicit.
Implicit Arguments const.
Unset Contextual Implicit.

Axiom Const : forall (a:Type) (b:Type), forall (b1:b) (a1:a),
  ((get (const b1:(map a b)) a1) = b1).

(* Why3 assumption *)
Definition env  := (map ident value).

(* Why3 assumption *)
Inductive list (a:Type) :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Set Contextual Implicit.
Implicit Arguments Nil.
Unset Contextual Implicit.
Implicit Arguments Cons.

(* Why3 assumption *)
Definition stack  := (list (ident* value)%type).

Parameter get_stack: ident -> (list (ident* value)%type) -> value.

Axiom get_stack_def : forall (i:ident) (pi:(list (ident* value)%type)),
  match pi with
  | Nil => ((get_stack i pi) = Vvoid)
  | (Cons (x, v) r) => ((x = i) -> ((get_stack i pi) = v)) /\ ((~ (x = i)) ->
      ((get_stack i pi) = (get_stack i r)))
  end.

Axiom get_stack_eq : forall (x:ident) (v:value) (r:(list (ident*
  value)%type)), ((get_stack x (Cons (x, v) r)) = v).

Axiom get_stack_neq : forall (x:ident) (i:ident) (v:value) (r:(list (ident*
  value)%type)), (~ (x = i)) -> ((get_stack i (Cons (x, v) r)) = (get_stack i
  r)).

Parameter eval_bin: value -> operator -> value -> value.

Axiom eval_bin_def : forall (x:value) (op:operator) (y:value), match (x,
  y) with
  | ((Vint x1), (Vint y1)) =>
      match op with
      | Oplus => ((eval_bin x op y) = (Vint (x1 + y1)%Z))
      | Ominus => ((eval_bin x op y) = (Vint (x1 - y1)%Z))
      | Omult => ((eval_bin x op y) = (Vint (x1 * y1)%Z))
      | Ole => ((x1 <= y1)%Z -> ((eval_bin x op y) = (Vbool true))) /\
          ((~ (x1 <= y1)%Z) -> ((eval_bin x op y) = (Vbool false)))
      end
  | (_, _) => ((eval_bin x op y) = Vvoid)
  end.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint eval_term(sigma:(map ident value)) (pi:(list (ident* value)%type))
  (t:term) {struct t}: value :=
  match t with
  | (Tvalue v) => v
  | (Tvar id) => (get_stack id pi)
  | (Tderef id) => (get sigma id)
  | (Tbin t1 op t2) => (eval_bin (eval_term sigma pi t1) op (eval_term sigma
      pi t2))
  end.
Unset Implicit Arguments.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint eval_fmla(sigma:(map ident value)) (pi:(list (ident* value)%type))
  (f:fmla) {struct f}: Prop :=
  match f with
  | (Fterm t) => ((eval_term sigma pi t) = (Vbool true))
  | (Fand f1 f2) => (eval_fmla sigma pi f1) /\ (eval_fmla sigma pi f2)
  | (Fnot f1) => ~ (eval_fmla sigma pi f1)
  | (Fimplies f1 f2) => (eval_fmla sigma pi f1) -> (eval_fmla sigma pi f2)
  | (Flet x t f1) => (eval_fmla sigma (Cons (x, (eval_term sigma pi t)) pi)
      f1)
  | (Fforall x TYint f1) => forall (n:Z), (eval_fmla sigma (Cons (x,
      (Vint n)) pi) f1)
  | (Fforall x TYbool f1) => forall (b:bool), (eval_fmla sigma (Cons (x,
      (Vbool b)) pi) f1)
  | (Fforall x TYunit f1) => (eval_fmla sigma (Cons (x, Vvoid) pi) f1)
  end.
Unset Implicit Arguments.

Parameter subst_term: term -> ident -> ident -> term.

Axiom subst_term_def : forall (e:term) (r:ident) (v:ident),
  match e with
  | ((Tvalue _)|(Tvar _)) => ((subst_term e r v) = e)
  | (Tderef x) => ((r = x) -> ((subst_term e r v) = (Tvar v))) /\
      ((~ (r = x)) -> ((subst_term e r v) = e))
  | (Tbin e1 op e2) => ((subst_term e r v) = (Tbin (subst_term e1 r v) op
      (subst_term e2 r v)))
  end.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint fresh_in_term(id:ident) (t:term) {struct t}: Prop :=
  match t with
  | (Tvalue _) => True
  | (Tvar v) => ~ (id = v)
  | (Tderef _) => True
  | (Tbin t1 _ t2) => (fresh_in_term id t1) /\ (fresh_in_term id t2)
  end.
Unset Implicit Arguments.

Axiom eval_subst_term : forall (sigma:(map ident value)) (pi:(list (ident*
  value)%type)) (e:term) (x:ident) (v:ident), (fresh_in_term v e) ->
  ((eval_term sigma pi (subst_term e x v)) = (eval_term (set sigma x
  (get_stack v pi)) pi e)).

Axiom eval_term_change_free : forall (t:term) (sigma:(map ident value))
  (pi:(list (ident* value)%type)) (id:ident) (v:value), (fresh_in_term id
  t) -> ((eval_term sigma (Cons (id, v) pi) t) = (eval_term sigma pi t)).

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint fresh_in_fmla(id:ident) (f:fmla) {struct f}: Prop :=
  match f with
  | (Fterm e) => (fresh_in_term id e)
  | ((Fand f1 f2)|(Fimplies f1 f2)) => (fresh_in_fmla id f1) /\
      (fresh_in_fmla id f2)
  | (Fnot f1) => (fresh_in_fmla id f1)
  | (Flet y t f1) => (~ (id = y)) /\ ((fresh_in_term id t) /\
      (fresh_in_fmla id f1))
  | (Fforall y ty f1) => (~ (id = y)) /\ (fresh_in_fmla id f1)
  end.
Unset Implicit Arguments.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint subst(f:fmla) (x:ident) (v:ident) {struct f}: fmla :=
  match f with
  | (Fterm e) => (Fterm (subst_term e x v))
  | (Fand f1 f2) => (Fand (subst f1 x v) (subst f2 x v))
  | (Fnot f1) => (Fnot (subst f1 x v))
  | (Fimplies f1 f2) => (Fimplies (subst f1 x v) (subst f2 x v))
  | (Flet y t f1) => (Flet y (subst_term t x v) (subst f1 x v))
  | (Fforall y ty f1) => (Fforall y ty (subst f1 x v))
  end.
Unset Implicit Arguments.

Axiom subst_fresh : forall (f:fmla) (x:ident) (v:ident), (fresh_in_fmla x
  f) -> ((subst f x v) = f).

Axiom let_subst : forall (t:term) (f:fmla) (x:ident) (id:ident) (idqt:ident),
  ((subst (Flet x t f) id idqt) = (Flet x (subst_term t id idqt) (subst f id
  idqt))).

Axiom eval_subst : forall (f:fmla) (sigma:(map ident value)) (pi:(list
  (ident* value)%type)) (x:ident) (v:ident), (fresh_in_fmla v f) ->
  ((eval_fmla sigma pi (subst f x v)) <-> (eval_fmla (set sigma x
  (get_stack v pi)) pi f)).

Axiom eval_swap : forall (f:fmla) (sigma:(map ident value)) (pi:(list (ident*
  value)%type)) (id1:ident) (id2:ident) (v1:value) (v2:value),
  (~ (id1 = id2)) -> ((eval_fmla sigma (Cons (id1, v1) (Cons (id2, v2) pi))
  f) <-> (eval_fmla sigma (Cons (id2, v2) (Cons (id1, v1) pi)) f)).

Axiom eval_change_free : forall (f:fmla) (sigma:(map ident value)) (pi:(list
  (ident* value)%type)) (id:ident) (v:value), (fresh_in_fmla id f) ->
  ((eval_fmla sigma (Cons (id, v) pi) f) <-> (eval_fmla sigma pi f)).

(* Why3 assumption *)
Definition valid_fmla(p:fmla): Prop := forall (sigma:(map ident value))
  (pi:(list (ident* value)%type)), (eval_fmla sigma pi p).

Axiom let_equiv : forall (id:ident) (idqt:ident) (t:term) (f:fmla),
  forall (sigma:(map ident value)) (pi:(list (ident* value)%type)),
  (eval_fmla sigma pi (Flet idqt t (subst f id idqt))) -> (eval_fmla sigma pi
  (Flet id t f)).

(* Why3 assumption *)
Inductive expr  :=
  | Evalue : value -> expr 
  | Ebin : expr -> operator -> expr -> expr 
  | Evar : ident -> expr 
  | Ederef : ident -> expr 
  | Eassign : ident -> expr -> expr 
  | Eseq : expr -> expr -> expr 
  | Elet : ident -> expr -> expr -> expr 
  | Eif : expr -> expr -> expr -> expr 
  | Eassert : fmla -> expr 
  | Ewhile : expr -> fmla -> expr -> expr .

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint fresh_in_expr(id:ident) (e:expr) {struct e}: Prop :=
  match e with
  | (Evalue _) => True
  | ((Eseq e1 e2)|(Ebin e1 _ e2)) => (fresh_in_expr id e1) /\
      (fresh_in_expr id e2)
  | (Evar v) => ~ (id = v)
  | (Ederef _) => True
  | (Eassign _ e1) => (fresh_in_expr id e1)
  | (Elet v e1 e2) => (~ (id = v)) /\ ((fresh_in_expr id e1) /\
      (fresh_in_expr id e2))
  | (Eif e1 e2 e3) => (fresh_in_expr id e1) /\ ((fresh_in_expr id e2) /\
      (fresh_in_expr id e3))
  | (Eassert f) => (fresh_in_fmla id f)
  | (Ewhile cond inv body) => (fresh_in_expr id cond) /\ ((fresh_in_fmla id
      inv) /\ (fresh_in_expr id body))
  end.
Unset Implicit Arguments.

(* Why3 assumption *)
Inductive one_step : (map ident value) -> (list (ident* value)%type) -> expr
  -> (map ident value) -> (list (ident* value)%type) -> expr -> Prop :=
  | one_step_var : forall (sigma:(map ident value)) (pi:(list (ident*
      value)%type)) (v:ident), (one_step sigma pi (Evar v) sigma pi
      (Evalue (get_stack v pi)))
  | one_step_deref : forall (sigma:(map ident value)) (pi:(list (ident*
      value)%type)) (v:ident), (one_step sigma pi (Ederef v) sigma pi
      (Evalue (get sigma v)))
  | one_step_bin_ctxt1 : forall (sigma:(map ident value)) (sigmaqt:(map ident
      value)) (pi:(list (ident* value)%type)) (piqt:(list (ident*
      value)%type)) (op:operator) (e1:expr) (e1qt:expr) (e2:expr),
      (one_step sigma pi e1 sigmaqt piqt e1qt) -> (one_step sigma pi (Ebin e1
      op e2) sigmaqt piqt (Ebin e1qt op e2))
  | one_step_bin_ctxt2 : forall (sigma:(map ident value)) (sigmaqt:(map ident
      value)) (pi:(list (ident* value)%type)) (piqt:(list (ident*
      value)%type)) (op:operator) (v1:value) (e2:expr) (e2qt:expr),
      (one_step sigma pi e2 sigmaqt piqt e2qt) -> (one_step sigma pi
      (Ebin (Evalue v1) op e2) sigmaqt piqt (Ebin (Evalue v1) op e2qt))
  | one_step_bin_value : forall (sigma:(map ident value)) (sigmaqt:(map ident
      value)) (pi:(list (ident* value)%type)) (piqt:(list (ident*
      value)%type)) (op:operator) (v1:value) (v2:value), (one_step sigma pi
      (Ebin (Evalue v1) op (Evalue v2)) sigmaqt piqt (Evalue (eval_bin v1 op
      v2)))
  | one_step_assign_ctxt : forall (sigma:(map ident value)) (sigmaqt:(map
      ident value)) (pi:(list (ident* value)%type)) (piqt:(list (ident*
      value)%type)) (x:ident) (e:expr) (eqt:expr), (one_step sigma pi e
      sigmaqt piqt eqt) -> (one_step sigma pi (Eassign x e) sigmaqt piqt
      (Eassign x eqt))
  | one_step_assign_value : forall (sigma:(map ident value)) (pi:(list
      (ident* value)%type)) (x:ident) (v:value), (one_step sigma pi
      (Eassign x (Evalue v)) (set sigma x v) pi (Evalue Vvoid))
  | one_step_seq_ctxt : forall (sigma:(map ident value)) (sigmaqt:(map ident
      value)) (pi:(list (ident* value)%type)) (piqt:(list (ident*
      value)%type)) (e1:expr) (e1qt:expr) (e2:expr), (one_step sigma pi e1
      sigmaqt piqt e1qt) -> (one_step sigma pi (Eseq e1 e2) sigmaqt piqt
      (Eseq e1qt e2))
  | one_step_seq_value : forall (sigma:(map ident value)) (pi:(list (ident*
      value)%type)) (e:expr), (one_step sigma pi (Eseq (Evalue Vvoid) e)
      sigma pi e)
  | one_step_let_ctxt : forall (sigma:(map ident value)) (sigmaqt:(map ident
      value)) (pi:(list (ident* value)%type)) (piqt:(list (ident*
      value)%type)) (id:ident) (e1:expr) (e1qt:expr) (e2:expr),
      (one_step sigma pi e1 sigmaqt piqt e1qt) -> (one_step sigma pi (Elet id
      e1 e2) sigmaqt piqt (Elet id e1qt e2))
  | one_step_let_value : forall (sigma:(map ident value)) (pi:(list (ident*
      value)%type)) (id:ident) (v:value) (e:expr), (one_step sigma pi
      (Elet id (Evalue v) e) sigma (Cons (id, v) pi) e)
  | one_step_if_ctxt : forall (sigma:(map ident value)) (sigmaqt:(map ident
      value)) (pi:(list (ident* value)%type)) (piqt:(list (ident*
      value)%type)) (e1:expr) (e1qt:expr) (e2:expr) (e3:expr),
      (one_step sigma pi e1 sigmaqt piqt e1qt) -> (one_step sigma pi (Eif e1
      e2 e3) sigmaqt piqt (Eif e1qt e2 e3))
  | one_step_if_true : forall (sigma:(map ident value)) (pi:(list (ident*
      value)%type)) (e1:expr) (e2:expr), (one_step sigma pi
      (Eif (Evalue (Vbool true)) e1 e2) sigma pi e1)
  | one_step_if_false : forall (sigma:(map ident value)) (pi:(list (ident*
      value)%type)) (e1:expr) (e2:expr), (one_step sigma pi
      (Eif (Evalue (Vbool false)) e1 e2) sigma pi e2)
  | one_step_assert : forall (sigma:(map ident value)) (pi:(list (ident*
      value)%type)) (f:fmla), (eval_fmla sigma pi f) -> (one_step sigma pi
      (Eassert f) sigma pi (Evalue Vvoid))
  | one_step_while : forall (sigma:(map ident value)) (pi:(list (ident*
      value)%type)) (cond:expr) (inv:fmla) (body:expr), (eval_fmla sigma pi
      inv) -> (one_step sigma pi (Ewhile cond inv body) sigma pi (Eif cond
      (Eseq body (Ewhile cond inv body)) (Evalue Vvoid))).

(* Why3 assumption *)
Inductive many_steps : (map ident value) -> (list (ident* value)%type)
  -> expr -> (map ident value) -> (list (ident* value)%type) -> expr
  -> Z -> Prop :=
  | many_steps_refl : forall (sigma:(map ident value)) (pi:(list (ident*
      value)%type)) (e:expr), (many_steps sigma pi e sigma pi e 0%Z)
  | many_steps_trans : forall (sigma1:(map ident value)) (sigma2:(map ident
      value)) (sigma3:(map ident value)) (pi1:(list (ident* value)%type))
      (pi2:(list (ident* value)%type)) (pi3:(list (ident* value)%type))
      (e1:expr) (e2:expr) (e3:expr) (n:Z), (one_step sigma1 pi1 e1 sigma2 pi2
      e2) -> ((many_steps sigma2 pi2 e2 sigma3 pi3 e3 n) ->
      (many_steps sigma1 pi1 e1 sigma3 pi3 e3 (n + 1%Z)%Z)).

Axiom steps_non_neg : forall (sigma1:(map ident value)) (sigma2:(map ident
  value)) (pi1:(list (ident* value)%type)) (pi2:(list (ident* value)%type))
  (e1:expr) (e2:expr) (n:Z), (many_steps sigma1 pi1 e1 sigma2 pi2 e2 n) ->
  (0%Z <= n)%Z.

Axiom many_steps_seq : forall (sigma1:(map ident value)) (sigma3:(map ident
  value)) (pi1:(list (ident* value)%type)) (pi3:(list (ident* value)%type))
  (e1:expr) (e2:expr) (n:Z), (many_steps sigma1 pi1 (Eseq e1 e2) sigma3 pi3
  (Evalue Vvoid) n) -> exists sigma2:(map ident value), exists pi2:(list
  (ident* value)%type), exists n1:Z, exists n2:Z, (many_steps sigma1 pi1 e1
  sigma2 pi2 (Evalue Vvoid) n1) /\ ((many_steps sigma2 pi2 e2 sigma3 pi3
  (Evalue Vvoid) n2) /\ (n = ((1%Z + n1)%Z + n2)%Z)).

Axiom many_steps_let : forall (sigma1:(map ident value)) (sigma3:(map ident
  value)) (pi1:(list (ident* value)%type)) (pi3:(list (ident* value)%type))
  (id:ident) (e1:expr) (e2:expr) (v2:value) (n:Z), (many_steps sigma1 pi1
  (Elet id e1 e2) sigma3 pi3 (Evalue v2) n) -> exists sigma2:(map ident
  value), exists pi2:(list (ident* value)%type), exists v1:value,
  exists n1:Z, exists n2:Z, (many_steps sigma1 pi1 e1 sigma2 pi2 (Evalue v1)
  n1) /\ ((many_steps sigma2 (Cons (id, v1) pi2) e2 sigma3 pi3 (Evalue v2)
  n2) /\ (n = ((1%Z + n1)%Z + n2)%Z)).

Axiom one_step_change_free : forall (e:expr) (eqt:expr) (sigma:(map ident
  value)) (sigmaqt:(map ident value)) (pi:(list (ident* value)%type))
  (piqt:(list (ident* value)%type)) (id:ident) (v:value), (fresh_in_expr id
  e) -> ((one_step sigma (Cons (id, v) pi) e sigmaqt piqt eqt) ->
  (one_step sigma pi e sigmaqt piqt eqt)).

(* Why3 assumption *)
Definition valid_triple(p:fmla) (e:expr) (q:fmla): Prop := forall (sigma:(map
  ident value)) (pi:(list (ident* value)%type)), (eval_fmla sigma pi p) ->
  forall (sigmaqt:(map ident value)) (piqt:(list (ident* value)%type))
  (v:value) (n:Z), (many_steps sigma pi e sigmaqt piqt (Evalue v) n) ->
  (eval_fmla sigmaqt (Cons (result, v) piqt) q).

(* Why3 assumption *)
Definition total_valid_triple(p:fmla) (e:expr) (q:fmla): Prop :=
  forall (sigma:(map ident value)) (pi:(list (ident* value)%type)),
  (eval_fmla sigma pi p) -> exists sigmaqt:(map ident value),
  exists piqt:(list (ident* value)%type), exists v:value, exists n:Z,
  (many_steps sigma pi e sigmaqt piqt (Evalue v) n) /\ (eval_fmla sigmaqt
  (Cons (result, v) piqt) q).

Parameter set1 : forall (a:Type), Type.

Parameter mem: forall (a:Type), a -> (set1 a) -> Prop.
Implicit Arguments mem.

(* Why3 assumption *)
Definition infix_eqeq (a:Type)(s1:(set1 a)) (s2:(set1 a)): Prop :=
  forall (x:a), (mem x s1) <-> (mem x s2).
Implicit Arguments infix_eqeq.

Axiom extensionality : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (infix_eqeq s1 s2) -> (s1 = s2).

(* Why3 assumption *)
Definition subset (a:Type)(s1:(set1 a)) (s2:(set1 a)): Prop := forall (x:a),
  (mem x s1) -> (mem x s2).
Implicit Arguments subset.

Axiom subset_trans : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a))
  (s3:(set1 a)), (subset s1 s2) -> ((subset s2 s3) -> (subset s1 s3)).

Parameter empty: forall (a:Type), (set1 a).
Set Contextual Implicit.
Implicit Arguments empty.
Unset Contextual Implicit.

(* Why3 assumption *)
Definition is_empty (a:Type)(s:(set1 a)): Prop := forall (x:a), ~ (mem x s).
Implicit Arguments is_empty.

Axiom empty_def1 : forall (a:Type), (is_empty (empty :(set1 a))).

Parameter add: forall (a:Type), a -> (set1 a) -> (set1 a).
Implicit Arguments add.

Axiom add_def1 : forall (a:Type), forall (x:a) (y:a), forall (s:(set1 a)),
  (mem x (add y s)) <-> ((x = y) \/ (mem x s)).

Parameter remove: forall (a:Type), a -> (set1 a) -> (set1 a).
Implicit Arguments remove.

Axiom remove_def1 : forall (a:Type), forall (x:a) (y:a) (s:(set1 a)), (mem x
  (remove y s)) <-> ((~ (x = y)) /\ (mem x s)).

Axiom subset_remove : forall (a:Type), forall (x:a) (s:(set1 a)),
  (subset (remove x s) s).

Parameter union: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments union.

Axiom union_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (union s1 s2)) <-> ((mem x s1) \/ (mem x s2)).

Parameter inter: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments inter.

Axiom inter_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (inter s1 s2)) <-> ((mem x s1) /\ (mem x s2)).

Parameter diff: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments diff.

Axiom diff_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (diff s1 s2)) <-> ((mem x s1) /\ ~ (mem x s2)).

Axiom subset_diff : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (subset (diff s1 s2) s1).

Parameter choose: forall (a:Type), (set1 a) -> a.
Implicit Arguments choose.

Axiom choose_def : forall (a:Type), forall (s:(set1 a)), (~ (is_empty s)) ->
  (mem (choose s) s).

Parameter all: forall (a:Type), (set1 a).
Set Contextual Implicit.
Implicit Arguments all.
Unset Contextual Implicit.

Axiom all_def : forall (a:Type), forall (x:a), (mem x (all :(set1 a))).

(* Why3 assumption *)
Definition assigns(sigma:(map ident value)) (a:(set1 ident)) (sigmaqt:(map
  ident value)): Prop := forall (i:ident), (~ (mem i a)) -> ((get sigma
  i) = (get sigmaqt i)).

Axiom assigns_refl : forall (sigma:(map ident value)) (a:(set1 ident)),
  (assigns sigma a sigma).

Axiom assigns_trans : forall (sigma1:(map ident value)) (sigma2:(map ident
  value)) (sigma3:(map ident value)) (a:(set1 ident)), ((assigns sigma1 a
  sigma2) /\ (assigns sigma2 a sigma3)) -> (assigns sigma1 a sigma3).

Axiom assigns_union_left : forall (sigma:(map ident value)) (sigmaqt:(map
  ident value)) (s1:(set1 ident)) (s2:(set1 ident)), (assigns sigma s1
  sigmaqt) -> (assigns sigma (union s1 s2) sigmaqt).

Axiom assigns_union_right : forall (sigma:(map ident value)) (sigmaqt:(map
  ident value)) (s1:(set1 ident)) (s2:(set1 ident)), (assigns sigma s2
  sigmaqt) -> (assigns sigma (union s1 s2) sigmaqt).

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint expr_writes(e:expr) (w:(set1 ident)) {struct e}: Prop :=
  match e with
  | ((Evalue _)|((Evar _)|((Ederef _)|(Eassert _)))) => True
  | (Ebin e1 _ e2) => (expr_writes e1 w) /\ (expr_writes e2 w)
  | (Eassign id _) => (mem id w)
  | (Eseq e1 e2) => (expr_writes e1 w) /\ (expr_writes e2 w)
  | (Elet id e1 e2) => (expr_writes e1 w) /\ (expr_writes e2 w)
  | (Eif e1 e2 e3) => (expr_writes e1 w) /\ ((expr_writes e2 w) /\
      (expr_writes e3 w))
  | (Ewhile cond _ body) => (expr_writes cond w) /\ (expr_writes body w)
  end.
Unset Implicit Arguments.

Parameter fresh_from: fmla -> expr -> ident.

Axiom fresh_from_fmla : forall (e:expr) (f:fmla),
  (fresh_in_fmla (fresh_from f e) f).

Axiom fresh_from_expr : forall (e:expr) (f:fmla),
  (fresh_in_expr (fresh_from f e) e).

Parameter abstract_effects: expr -> fmla -> fmla.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint wp(e:expr) (q:fmla) {struct e}: fmla :=
  match e with
  | (Evalue v) => (Flet result (Tvalue v) q)
  | (Evar v) => (Flet result (Tvar v) q)
  | (Ederef x) => (Flet result (Tderef x) q)
  | (Eassert f) => (Fand f (Fimplies f q))
  | (Eseq e1 e2) => (wp e1 (wp e2 q))
  | (Elet id e1 e2) => (wp e1 (Flet id (Tvar result) (wp e2 q)))
  | (Ebin e1 op e2) => let t1 := (fresh_from q e) in let t2 :=
      (fresh_from (Fand (Fterm (Tvar t1)) q) e) in let qqt := (Flet result
      (Tbin (Tvar t1) op (Tvar t2)) q) in let f := (wp e2 (Flet t2
      (Tvar result) qqt)) in (wp e1 (Flet t1 (Tvar result) f))
  | (Eassign x e1) => let id := (fresh_from q e1) in let qqt := (Flet result
      (Tvalue Vvoid) q) in (wp e1 (Flet id (Tvar result) (subst qqt x id)))
  | (Eif e1 e2 e3) => let f := (Fand (Fimplies (Fterm (Tvar result)) (wp e2
      q)) (Fimplies (Fnot (Fterm (Tvar result))) (wp e3 q))) in (wp e1 f)
  | (Ewhile cond inv body) => (Fand inv (abstract_effects body (wp cond
      (Fand (Fimplies (Fand (Fterm (Tvar result)) inv) (wp body inv))
      (Fimplies (Fand (Fnot (Fterm (Tvar result))) inv) q)))))
  end.
Unset Implicit Arguments.

Axiom wp_subst : forall (e:expr) (q:fmla) (id:ident) (idqt:ident),
  (fresh_in_expr id e) -> ((subst (wp e q) id idqt) = (wp e (subst q id
  idqt))).

Axiom wp_implies : forall (p:fmla) (q:fmla), (forall (sigma:(map ident
  value)) (pi:(list (ident* value)%type)), (eval_fmla sigma pi p) ->
  (eval_fmla sigma pi q)) -> forall (sigma:(map ident value)) (pi:(list
  (ident* value)%type)) (e:expr), (eval_fmla sigma pi (wp e p)) ->
  (eval_fmla sigma pi (wp e q)).

Axiom wp_conj : forall (sigma:(map ident value)) (pi:(list (ident*
  value)%type)) (e:expr) (p:fmla) (q:fmla), (eval_fmla sigma pi (wp e (Fand p
  q))) <-> ((eval_fmla sigma pi (wp e p)) /\ (eval_fmla sigma pi (wp e q))).

Require Why3.
Ltac ae := why3 "alt-ergo" timelimit 2.
Ltac ae10 := why3 "alt-ergo" timelimit 10.
Ltac cvc10 := why3 "cvc3-2.4" timelimit 10.

(* Why3 goal *)
Theorem wp_reduction : forall (sigma:(map ident value)) (sigmaqt:(map ident
  value)) (pi:(list (ident* value)%type)) (piqt:(list (ident* value)%type))
  (e:expr) (eqt:expr), (one_step sigma pi e sigmaqt piqt eqt) ->
  forall (q:fmla), (eval_fmla sigma pi (wp e q)) -> (eval_fmla sigmaqt piqt
  (wp eqt q)).

induction 1.
(* case 1: var *)
auto.
(* case 2: deref *)
auto.
(* case 3: e_1 bin_op e_2 *)
intros q.
simpl in *.
pose (t1 := fresh_from q (Ebin e1 op e2)).
fold t1.
pose (t1' := fresh_from q (Ebin e1qt op e2)).
fold t1'.
pose (t2 := fresh_from (Fand (Fterm (Tvar t1)) q) (Ebin e1 op e2)).
fold t2.
pose (t2' := fresh_from (Fand (Fterm (Tvar t1')) q) (Ebin e1qt op e2)).
fold t2'.
intros h.
apply IHone_step.
apply wp_implies with (2:=h).
intros sigma' pi'.
intro.
apply let_equiv with (idqt:=t1).
rewrite wp_subst.
rewrite let_subst.
rewrite let_subst.
rewrite subst_fresh.
rewrite (subst_term_def (Tbin (Tvar t1') op (Tvar t2'))).
rewrite (subst_term_def (Tvar t1') t1' t1).
rewrite (subst_term_def (Tvar t2') t1' t1).
rewrite (subst_term_def (Tvar result) t1' t1).

admit. (* needs lemmas on fresh_from *)
admit.
admit.
(* case 4: v_1 bin_op e_2 *)
intros q h.

admit. 

(* case 5: v_1 bin_op v_2 *)
intros q h.
admit. 

(* case x := e -> x := e' *)
admit.

(* case x := v -> void *)
admit.

(* case Eseq e_1 e_2 *)
admit.

(* case Eseq void e_2 *)
admit.

(* case Elet id e_1 e_2 *)
admit.

(* case Elet id v_1 e_2 *)
admit.

(* case Eif e_1 e_2 e_3*)
admit.

(* case Eif true e_1 e_2 *)
admit.

(* case Eif false e_1 e_2 *)
admit.

(* case Eassert f *)
admit.

(* case Ewhile cond inv body *)
admit.

Qed.

