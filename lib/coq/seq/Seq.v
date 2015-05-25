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

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require HighOrd.
Require int.Int.

(* Why3 goal *)
Definition seq : forall (a:Type), Type.
intro a.
exact (list a).
Defined.

Global Instance seq_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (seq a).
Proof.
intros a a_WT.
split.
exact (@nil a).
destruct a_WT.
decide equality.
Qed.

(* Why3 goal *)
Definition length: forall {a:Type} {a_WT:WhyType a}, (seq a) -> Z.
intros a a_WT.
exact (fix len l := match l with
  | nil => 0 | cons _ t => 1 + len t end)%Z.
Defined.

Hint Unfold length.

(* Why3 goal *)
Lemma length_nonnegative : forall {a:Type} {a_WT:WhyType a}, forall (s:(seq
  a)), (0%Z <= (length s))%Z.
intros a a_WT.
induction s.
auto with *.
unfold length. fold length. omega.
Qed.

(* Why3 goal *)
Definition empty: forall {a:Type} {a_WT:WhyType a}, (seq a).
intros a a_WT.
exact nil.
Defined.

(* Why3 goal *)
Lemma empty_length : forall {a:Type} {a_WT:WhyType a}, ((length (empty : (seq
  a))) = 0%Z).
intros a a_WT.
auto with *.
Qed.

(* Why3 goal *)
Definition mixfix_lbrb: forall {a:Type} {a_WT:WhyType a}, (seq a) -> Z -> a.
intros a (default, _) s i.
exact ((fix nth n l := match l with
  | nil => default
  | cons h t => if Zeq_bool n 0%Z then h else nth (n - 1)%Z t end) i s).
Defined.

(* Why3 assumption *)
Definition infix_eqeq {a:Type} {a_WT:WhyType a} (s1:(seq a)) (s2:(seq
  a)): Prop := ((length s1) = (length s2)) /\ forall (i:Z), ((0%Z <= i)%Z /\
  (i < (length s1))%Z) -> ((mixfix_lbrb s1 i) = (mixfix_lbrb s2 i)).

Notation "x == y" := (infix_eqeq x y) (at level 70, no associativity).

Lemma length_nonneg:
  forall {a:Type} {a_WT:WhyType a} (s: seq a), (0 <= length s)%Z.
induction s.
auto with *.
unfold length. fold length. omega.
Qed.

(* Why3 goal *)
Lemma extensionality : forall {a:Type} {a_WT:WhyType a}, forall (s1:(seq a))
  (s2:(seq a)), (infix_eqeq s1 s2) -> (s1 = s2).
intros a a_WT.
induction s1.
inversion 1.
destruct s2; auto.
unfold length in H0. fold length in H0.
generalize (length_nonneg s2); omega.
destruct s2.
inversion 1.
unfold length in H0. fold length in H0.
generalize (length_nonneg s1); omega.
inversion 1.
apply f_equal2.
assert (h: (0 <= 0 < length (a0 :: s1)%list)%Z).
unfold length. fold length.
generalize (length_nonneg s1); omega.
generalize (H1 0%Z h); clear H1.
unfold mixfix_lbrb. destruct a_WT. auto.
apply IHs1.
split.
unfold length in H0. fold length in H0. omega.
intros i hi.
assert (h: (0 <= i+1 < length (a0 :: s1)%list)%Z).
unfold length. fold length. omega.
generalize (H1 (i+1)%Z h); clear H1.
unfold mixfix_lbrb. destruct a_WT.
assert (Zeq_bool (i+1) 0 = false).
generalize (Zeq_is_eq_bool (i+1) 0).
case (Zeq_bool (i+1) 0); intuition.
omega.
destruct (Zeq_bool (i+1) 0).
discriminate H1.
replace (i+1-1)%Z with i by omega.
auto.
Qed.

(* Why3 goal *)
Definition cons: forall {a:Type} {a_WT:WhyType a}, a -> (seq a) -> (seq a).
intros a aWT x l.
exact (cons x l).
Defined.

(* Why3 goal *)
Lemma cons_length : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (s:(seq
  a)), ((length (cons x s)) = (1%Z + (length s))%Z).
intros a a_WT x s.
unfold length, cons. fold length; auto.
Qed.

(* Why3 goal *)
Lemma cons_get : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (s:(seq a))
  (i:Z), ((0%Z <= i)%Z /\ (i <= (length s))%Z) -> (((i = 0%Z) ->
  ((mixfix_lbrb (cons x s) i) = x)) /\ ((~ (i = 0%Z)) ->
  ((mixfix_lbrb (cons x s) i) = (mixfix_lbrb s (i - 1%Z)%Z)))).
intros a (d,eq) x s i hi.
split.
intro; subst i; now auto.

destruct s.
simpl in hi. intro. absurd (i=0)%Z; omega.
intro h.
unfold mixfix_lbrb at 1.
simpl ((fix nth (n : int) (l : list a) {struct l} : a :=
   match l with
   | nil => d
   | (h0 :: t)%list => if Zeq_bool n 0 then h0 else nth (n - 1)%Z t
   end) i (cons x (a0 :: s)%list)).
destruct (Zeq_bool i 0) eqn:? in *.
generalize (Zeq_bool_eq _ _ Heqb). now intuition.
auto.
Qed.

(* Why3 goal *)
Definition snoc: forall {a:Type} {a_WT:WhyType a}, (seq a) -> a -> (seq a).
intros a aWT s x.
exact (s ++ List.cons x nil)%list.
Defined.

Open Scope list_scope.
Open Scope Z_scope.

Lemma length_append:
  forall {a:Type} {a_WT:WhyType a} s1 s2, length (s1 ++ s2) = length s1 + length s2.
induction s1.
auto.
intro s2. simpl ((a0 :: s1) ++ s2).
unfold length; fold length.
rewrite IHs1; omega.
Qed.

(* Why3 goal *)
Lemma snoc_length : forall {a:Type} {a_WT:WhyType a}, forall (s:(seq a))
  (x:a), ((length (snoc s x)) = (1%Z + (length s))%Z).
intros.
unfold snoc.
rewrite length_append.
simpl (length (x::nil)). omega.
Qed.

(* Why3 goal *)
Lemma snoc_get : forall {a:Type} {a_WT:WhyType a}, forall (s:(seq a)) (x:a)
  (i:Z), ((0%Z <= i)%Z /\ (i <= (length s))%Z) -> (((i < (length s))%Z ->
  ((mixfix_lbrb (snoc s x) i) = (mixfix_lbrb s i))) /\
  ((~ (i < (length s))%Z) -> ((mixfix_lbrb (snoc s x) i) = x))).
(*
intros a a_WT (l, d) x i (h1,h2).
split; intros.
unfold mixfix_lbrb.
unfold default, elts; simpl; auto.
unfold length in *; simpl in *.
apply List.app_nth1.
rewrite <- (Zabs2Nat.id (Datatypes.length l)).
apply Zabs_nat_lt. omega.
unfold mixfix_lbrb.
unfold default, elts; simpl; auto.
unfold length in *; simpl in *.
assert (i = Z.of_nat (Datatypes.length l)).
omega.
rewrite List.app_nth2.
replace (Z.abs_nat i - Datatypes.length l) with O.
auto.
subst i.
rewrite (Zabs2Nat.id (Datatypes.length l)). omega.
subst i.
rewrite (Zabs2Nat.id (Datatypes.length l)). omega.
Qed.
*)
Admitted. (* TODO *)

(* Why3 goal *)
Definition mixfix_lb_dtdt_rb: forall {a:Type} {a_WT:WhyType a}, (seq a) ->
  Z -> Z -> (seq a).

Admitted. (* TODO *)

(* Why3 goal *)
Lemma sub_length : forall {a:Type} {a_WT:WhyType a}, forall (s:(seq a)) (i:Z)
  (j:Z), ((0%Z <= i)%Z /\ ((i <= j)%Z /\ (j <= (length s))%Z)) ->
  ((length (mixfix_lb_dtdt_rb s i j)) = (j - i)%Z).
intros a a_WT s i j (h1,(h2,h3)).

Admitted. (* TODO *)

(* Why3 goal *)
Lemma sub_get : forall {a:Type} {a_WT:WhyType a}, forall (s:(seq a)) (i:Z)
  (j:Z), ((0%Z <= i)%Z /\ ((i <= j)%Z /\ (j <= (length s))%Z)) ->
  forall (k:Z), ((0%Z <= k)%Z /\ (k < (j - i)%Z)%Z) ->
  ((mixfix_lbrb (mixfix_lb_dtdt_rb s i j) k) = (mixfix_lbrb s (i + k)%Z)).
intros a a_WT s i j (h1,(h2,h3)) k (h4,h5).

Admitted. (* TODO *)

(* Why3 assumption *)
Definition mixfix_lb_dtdtrb {a:Type} {a_WT:WhyType a} (s:(seq a)) (i:Z): (seq
  a) := (mixfix_lb_dtdt_rb s i (length s)).

(* Why3 goal *)
Definition infix_plpl: forall {a:Type} {a_WT:WhyType a}, (seq a) -> (seq
  a) -> (seq a).
intros a aWT s1 s2.
exact (s1 ++ s2)%list.
Defined.

(* Why3 goal *)
Lemma concat_length : forall {a:Type} {a_WT:WhyType a}, forall (s1:(seq a))
  (s2:(seq a)), ((length (infix_plpl s1
  s2)) = ((length s1) + (length s2))%Z).
(*
intros a a_WT (l1,d1) (l2,d2).
unfold length; simpl.
rewrite List.app_length.
rewrite Nat2Z.inj_add. auto.
Qed.
*)
Admitted. (* TODO *)


(* Why3 goal *)
Lemma concat_get1 : forall {a:Type} {a_WT:WhyType a}, forall (s1:(seq a))
  (s2:(seq a)) (i:Z), ((0%Z <= i)%Z /\ (i < (length s1))%Z) ->
  ((mixfix_lbrb (infix_plpl s1 s2) i) = (mixfix_lbrb s1 i)).
intros a a_WT s1 s2 i (h1,h2).

Admitted. (* TODO *)

(* Why3 goal *)
Lemma concat_get2 : forall {a:Type} {a_WT:WhyType a}, forall (s1:(seq a))
  (s2:(seq a)) (i:Z), (((length s1) <= i)%Z /\
  (i < ((length s1) + (length s2))%Z)%Z) -> ((mixfix_lbrb (infix_plpl s1 s2)
  i) = (mixfix_lbrb s2 (i - (length s1))%Z)).
intros a a_WT s1 s2 i (h1,h2).

Admitted. (* TODO *)

Fixpoint enum {a:Type} (f: Z -> a) (start: Z) (n: nat) : seq a :=
  match n with
  | O => nil
  | S p => (f start :: enum f (start+1)%Z p)%list end.

(* Why3 goal *)
Definition create: forall {a:Type} {a_WT:WhyType a}, Z -> (Z -> a) -> (seq
  a).
intros a a_WT n f.
exact (if Zlt_bool n 0 then nil else enum f 0%Z (Zabs_nat n)).
Defined.

Lemma enum_length:
  forall {a:Type} {a_WT:WhyType a} (f: Z -> a) n start,
  length (enum f start n) = Z.of_nat n.
induction n; intros.
now auto.
unfold enum. fold (enum f).
unfold length. fold length.
rewrite IHn.
rewrite Nat2Z.inj_succ. auto with *.
Qed.

(* Why3 goal *)
Lemma create_length : forall {a:Type} {a_WT:WhyType a}, forall (len:Z)
  (f:(Z -> a)), (0%Z <= len)%Z -> ((length (create len f)) = len).
intros a a_WT len f h1.
unfold create.
generalize (Z.ltb_lt len 0).
destruct (Zlt_bool len 0) eqn:?.
intuition.
intros _. clear Heqb.
rewrite enum_length.
rewrite Zabs2Nat.abs_nat_nonneg.
apply Z2Nat.id; auto.
assumption.
Qed.

(* Why3 goal *)
Lemma create_get : forall {a:Type} {a_WT:WhyType a}, forall (len:Z) (f:(Z ->
  a)) (i:Z), ((0%Z <= i)%Z /\ (i < len)%Z) -> ((mixfix_lbrb (create len f)
  i) = (f i)).
intros a a_WT len f i (h1,h2).

Admitted. (* TODO *)

