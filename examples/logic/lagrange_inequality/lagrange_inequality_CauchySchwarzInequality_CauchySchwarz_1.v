(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require Reals.R_sqrt.
Require BuiltIn.
Require real.Real.
Require real.Square.

(* Why3 assumption *)
Definition dot (x1:R) (x2:R) (y1:R) (y2:R): R :=
  ((x1 * y1)%R + (x2 * y2)%R)%R.

(* Why3 assumption *)
Definition norm2 (x1:R) (x2:R): R :=
  ((Reals.RIneq.Rsqr x1) + (Reals.RIneq.Rsqr x2))%R.

Axiom norm2_pos : forall (x1:R) (x2:R), (0%R <= (norm2 x1 x2))%R.

Axiom Lagrange : forall (a1:R) (a2:R) (b1:R) (b2:R), (((norm2 a1
  a2) * (norm2 b1 b2))%R = ((Reals.RIneq.Rsqr (dot a1 a2 b1
  b2)) + (Reals.RIneq.Rsqr ((a1 * b2)%R - (a2 * b1)%R)%R))%R).

Axiom CauchySchwarz_aux : forall (x1:R) (x2:R) (y1:R) (y2:R),
  ((Reals.RIneq.Rsqr (dot x1 x2 y1 y2)) <= ((norm2 x1 x2) * (norm2 y1
  y2))%R)%R.

(* Why3 assumption *)
Definition norm (x1:R) (x2:R): R := (Reals.R_sqrt.sqrt (norm2 x1 x2)).

Axiom norm_pos : forall (x1:R) (x2:R), (0%R <= (norm x1 x2))%R.

Axiom sqr_le_sqrt : forall (x:R) (y:R), ((Reals.RIneq.Rsqr x) <= y)%R ->
  (x <= (Reals.R_sqrt.sqrt y))%R.

Require Import Why3.
Ltac ae := why3 "Alt-Ergo,0.99.1," timelimit 3; admit.

(* Why3 goal *)
Theorem CauchySchwarz : forall (x1:R) (x2:R) (y1:R) (y2:R), ((dot x1 x2 y1
  y2) <= ((norm x1 x2) * (norm y1 y2))%R)%R.
(* Why3 intros x1 x2 y1 y2. *)
intros x1 x2 y1 y2.
unfold norm.
rewrite <- R_sqrt.sqrt_mult.
apply sqr_le_sqrt.
ae.
ae.
ae.
Admitted.

