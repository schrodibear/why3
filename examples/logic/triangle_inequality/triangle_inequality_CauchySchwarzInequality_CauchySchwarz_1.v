(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require Import R_sqrt.
Require BuiltIn.
Require real.Real.
Require real.Square.

(* Why3 assumption *)
Definition dot (x1:R) (x2:R) (y1:R) (y2:R): R :=
  ((x1 * y1)%R + (x2 * y2)%R)%R.

(* Why3 assumption *)
Definition norm2 (x1:R) (x2:R): R := ((Rsqr x1) + (Rsqr x2))%R.

Axiom norm2_pos : forall (x1:R) (x2:R), (0%R <= (norm2 x1 x2))%R.

(* Why3 assumption *)
Definition p (x1:R) (x2:R) (y1:R) (y2:R) (t:R): R := (((norm2 x1
  x2) + ((2%R * t)%R * (dot x1 x2 y1 y2))%R)%R + ((Rsqr t) * (norm2 y1
  y2))%R)%R.

Axiom p_expr : forall (x1:R) (x2:R) (y1:R) (y2:R) (t:R), ((p x1 x2 y1 y2
  t) = (norm2 (x1 + (t * y1)%R)%R (x2 + (t * y2)%R)%R)).

Axiom p_pos : forall (x1:R) (x2:R) (y1:R) (y2:R) (t:R), (0%R <= (p x1 x2 y1
  y2 t))%R.

Axiom mul_div_simpl : forall (x:R) (y:R), (~ (y = 0%R)) ->
  (((Rdiv x y)%R * y)%R = x).

Axiom p_val_part : forall (x1:R) (x2:R) (y1:R) (y2:R), (0%R < (norm2 y1
  y2))%R -> ((p x1 x2 y1 y2 (-(Rdiv (dot x1 x2 y1 y2) (norm2 y1
  y2))%R)%R) = ((norm2 x1 x2) - (Rdiv (Rsqr (dot x1 x2 y1 y2)) (norm2 y1
  y2))%R)%R).

Axiom p_val_part_pos : forall (x1:R) (x2:R) (y1:R) (y2:R), (0%R < (norm2 y1
  y2))%R -> (0%R <= ((norm2 x1 x2) - (Rdiv (Rsqr (dot x1 x2 y1 y2)) (norm2 y1
  y2))%R)%R)%R.

Axiom p_val_part_pos_aux : forall (x1:R) (x2:R) (y1:R) (y2:R),
  (0%R < (norm2 y1 y2))%R -> (0%R <= ((norm2 y1 y2) * (p x1 x2 y1 y2
  (Rdiv (-(dot x1 x2 y1 y2))%R (norm2 y1 y2))%R))%R)%R.

Axiom CauchySchwarz_aux_non_null : forall (x1:R) (x2:R) (y1:R) (y2:R),
  (0%R < (norm2 y1 y2))%R -> ((Rsqr (dot x1 x2 y1 y2)) <= ((norm2 x1
  x2) * (norm2 y1 y2))%R)%R.

Axiom norm_null : forall (y1:R) (y2:R), ((norm2 y1 y2) = 0%R) ->
  ((y1 = 0%R) \/ (y2 = 0%R)).

Axiom CauchySchwarz_aux_null : forall (x1:R) (x2:R) (y1:R) (y2:R), ((norm2 y1
  y2) = 0%R) -> ((Rsqr (dot x1 x2 y1 y2)) <= ((norm2 x1 x2) * (norm2 y1
  y2))%R)%R.

Axiom CauchySchwarz_aux : forall (x1:R) (x2:R) (y1:R) (y2:R), ((Rsqr (dot x1
  x2 y1 y2)) <= ((norm2 x1 x2) * (norm2 y1 y2))%R)%R.

(* Why3 assumption *)
Definition norm (x1:R) (x2:R): R := (sqrt (norm2 x1 x2)).

Axiom norm_pos : forall (x1:R) (x2:R), (0%R <= (norm x1 x2))%R.

Axiom sqr_le_sqrt : forall (x:R) (y:R), ((0%R <= x)%R /\
  ((0%R <= (Rsqr x))%R /\ ((Rsqr x) <= y)%R)) -> (x <= (sqrt y))%R.

(* Why3 goal *)
Theorem CauchySchwarz : forall (x1:R) (x2:R) (y1:R) (y2:R), ((dot x1 x2 y1
  y2) <= ((norm x1 x2) * (norm y1 y2))%R)%R.
(* intros x1 x2 y1 y2. *)
intros x1 x2 y1 y2.
unfold norm.
rewrite <- sqrt_mult.
assert (Hdot: (0 <= dot x1 x2 y1 y2)%R \/ (0 > dot x1 x2 y1 y2)%R).
generalize (Rtotal_order 0 (dot x1 x2 y1 y2))%R; intuition.

destruct Hdot.
apply sqr_le_sqrt.
intuition.
apply CauchySchwarz_aux.

apply Rle_trans with 0%R.
intuition.

apply sqrt_pos.
apply norm2_pos.
apply norm2_pos.
Qed.


