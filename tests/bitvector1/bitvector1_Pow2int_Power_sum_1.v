(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.
Parameter pow2: Z -> Z.


Axiom Power_0 : ((pow2 0%Z) = 1%Z).

Axiom Power_s : forall (n:Z), (0%Z <= n)%Z ->
  ((pow2 (n + 1%Z)%Z) = (2%Z * (pow2 n))%Z).

Axiom Power_1 : ((pow2 1%Z) = 2%Z).

(* YOU MAY EDIT THE CONTEXT BELOW *)
Open Scope Z_scope.
(* DO NOT EDIT BELOW *)

Theorem Power_sum : forall (n:Z) (m:Z), ((0%Z <= n)%Z /\ (0%Z <= m)%Z) ->
  ((pow2 (n + m)%Z) = ((pow2 n) * (pow2 m))%Z).
(* YOU MAY EDIT THE PROOF BELOW *)
intros n m Hmn.
cut (0 <= m); auto with zarith.
apply Z_lt_induction with
  (P:= fun m => 
      0 <= m -> pow2 (n + m) = pow2 n * pow2 m);
  auto with zarith.
intros x Hind Hxpos.
assert (h:(x = 0 \/ x > 0)) by omega.
destruct h.
subst x.
rewrite Power_0.
replace (n+0) with n by omega.
replace (pow2 n * 1) with (pow2 n)  by omega.
auto.
replace (x) with ((x-1)+1) by omega.
rewrite Power_s;auto with zarith.
replace (n + (x-1+1)) with (n+(x-1)+1) by omega.
rewrite Power_s;auto with zarith.
rewrite Hind;auto with zarith.
rewrite Zmult_permute.
auto with zarith.
Qed.
(* DO NOT EDIT BELOW *)

