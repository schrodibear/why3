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

Definition double : Type := t 53 1024.

Definition round: floating_point.Rounding.mode -> R -> R := round 53 1024.

Definition round_logic: floating_point.Rounding.mode -> R -> double := round_logic 53 1024 (refl_equal true) (refl_equal true).

Definition value: double -> R := value 53 1024.

Definition exact: double -> R := exact 53 1024.

Definition model: double -> R := model 53 1024.

Definition round_error(x:double): R := (Rabs ((value x) - (exact x))%R).

Definition total_error(x:double): R := (Rabs ((value x) - (model x))%R).

Definition no_overflow(m:floating_point.Rounding.mode) (x:R): Prop :=
  ((Rabs (round m
  x)) <= (9007199254740991 * 19958403095347198116563727130368385660674512604354575415025472424372118918689640657849579654926357010893424468441924952439724379883935936607391717982848314203200056729510856765175377214443629871826533567445439239933308104551208703888888552684480441575071209068757560416423584952303440099278848)%R)%R.

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Bounded_real_no_overflow : forall (m:floating_point.Rounding.mode)
  (x:R),
  ((Rabs x) <= (9007199254740991 * 19958403095347198116563727130368385660674512604354575415025472424372118918689640657849579654926357010893424468441924952439724379883935936607391717982848314203200056729510856765175377214443629871826533567445439239933308104551208703888888552684480441575071209068757560416423584952303440099278848)%R)%R ->
  (no_overflow m x).
(* YOU MAY EDIT THE PROOF BELOW *)
exact (Bounded_real_no_overflow 53 1024 (refl_equal true) (refl_equal true)).
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Round_monotonic : forall (m:floating_point.Rounding.mode) (x:R) (y:R),
  (x <= y)%R -> ((round m x) <= (round m y))%R.
(* YOU MAY EDIT THE PROOF BELOW *)
now apply Round_monotonic.
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

Lemma Round_value : forall (m:floating_point.Rounding.mode) (x:double),
  ((round m (value x)) = (value x)).
(* YOU MAY EDIT THE PROOF BELOW *)
now apply Round_value.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Bounded_value : forall (x:double),
  ((Rabs (value x)) <= (9007199254740991 * 19958403095347198116563727130368385660674512604354575415025472424372118918689640657849579654926357010893424468441924952439724379883935936607391717982848314203200056729510856765175377214443629871826533567445439239933308104551208703888888552684480441575071209068757560416423584952303440099278848)%R)%R.
(* YOU MAY EDIT THE PROOF BELOW *)
now apply Bounded_value.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Exact_rounding_for_integers : forall (m:floating_point.Rounding.mode)
  (i:Z), (((-9007199254740992%Z)%Z <= i)%Z /\ (i <= 9007199254740992%Z)%Z) ->
  ((round m (IZR i)) = (IZR i)).
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