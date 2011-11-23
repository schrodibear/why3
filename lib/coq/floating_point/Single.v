(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require Import Rbasic_fun.
Require int.Int.
Require real.Real.
Require real.Abs.
Require real.FromInt.
Require floating_point.Rounding.
Require Import floating_point.GenFloat.

Definition single : Type := t 24 128.

Definition round: floating_point.Rounding.mode -> R -> R := round 24 128.

Definition round_logic: floating_point.Rounding.mode -> R -> single := round_logic 24 128 (refl_equal true) (refl_equal true).

Definition value: single -> R := value 24 128.

Definition exact: single -> R := exact 24 128.

Definition model: single -> R := model 24 128.


Definition round_error(x:single): R := (Rabs ((value x) - (exact x))%R).

Definition total_error(x:single): R := (Rabs ((value x) - (model x))%R).

Definition no_overflow(m:floating_point.Rounding.mode) (x:R): Prop :=
  ((Rabs (round m x)) <= (33554430 * 10141204801825835211973625643008)%R)%R.

Lemma max_single_eq: (33554430 * 10141204801825835211973625643008 = max 24 128)%R.
unfold max, Fcore_defs.F2R; simpl.
ring.
Qed.


(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Bounded_real_no_overflow : forall (m:floating_point.Rounding.mode)
  (x:R), ((Rabs x) <= (33554430 * 10141204801825835211973625643008)%R)%R ->
  (no_overflow m x).
(* YOU MAY EDIT THE PROOF BELOW *)
intros m x Hx.
unfold no_overflow.
rewrite max_single_eq in *.
exact (Bounded_real_no_overflow 24 128 (refl_equal true) (refl_equal true) m x Hx).
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Round_monotonic : forall (m:floating_point.Rounding.mode) (x:R) (y:R),
  (x <= y)%R -> ((round m x) <= (round m y))%R.
(* YOU MAY EDIT THE PROOF BELOW *)
apply Round_monotonic.
easy.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Round_idempotent : forall (m1:floating_point.Rounding.mode)
  (m2:floating_point.Rounding.mode) (x:R), ((round m1 (round m2
  x)) = (round m2 x)).
(* YOU MAY EDIT THE PROOF BELOW *)
now apply Round_idempotent.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Round_value : forall (m:floating_point.Rounding.mode) (x:single),
  ((round m (value x)) = (value x)).
(* YOU MAY EDIT THE PROOF BELOW *)
now apply Round_value.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Bounded_value : forall (x:single),
  ((Rabs (value x)) <= (33554430 * 10141204801825835211973625643008)%R)%R.
(* YOU MAY EDIT THE PROOF BELOW *)
rewrite max_single_eq.
now apply Bounded_value.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Exact_rounding_for_integers : forall (m:floating_point.Rounding.mode)
  (i:Z), (((-16777216%Z)%Z <= i)%Z /\ (i <= 16777216%Z)%Z) -> ((round m
  (IZR i)) = (IZR i)).
(* YOU MAY EDIT THE PROOF BELOW *)
intros m i Hi.
now apply Exact_rounding_for_integers.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Round_down_le : forall (x:R), ((round floating_point.Rounding.Down
  x) <= x)%R.
(* YOU MAY EDIT THE PROOF BELOW *)
now apply Round_down_le.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Round_up_ge : forall (x:R), (x <= (round floating_point.Rounding.Up
  x))%R.
(* YOU MAY EDIT THE PROOF BELOW *)
now apply Round_up_ge.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Round_down_neg : forall (x:R), ((round floating_point.Rounding.Down
  (-x)%R) = (-(round floating_point.Rounding.Up x))%R).
(* YOU MAY EDIT THE PROOF BELOW *)
now apply Round_down_neg.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Round_up_neg : forall (x:R), ((round floating_point.Rounding.Up
  (-x)%R) = (-(round floating_point.Rounding.Down x))%R).
(* YOU MAY EDIT THE PROOF BELOW *)
now apply Round_up_neg.
Qed.
(* DO NOT EDIT BELOW *)
