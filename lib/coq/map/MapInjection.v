(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require map.Map.

(* preliminaries *)

Definition into (n:nat) (f:nat -> nat) :=
  forall i:nat, i < n -> f i < n.

Definition injection (n:nat) (f:nat -> nat) :=
  forall i j:nat, i < n -> j < n ->
     f i = f j -> i = j.

Definition surjection (n:nat) (f:nat -> nat) :=
  forall i:nat, i < n ->
    exists j:nat, j < n /\ f j = i.

Require Omega.
Require Import Peano_dec.

Theorem injective_implies_surjective:
  forall n:nat,
  forall f:nat -> nat,
  into n f ->
  injection n f ->
  surjection n f.
induction n.
(* case n = 0 *)
unfold surjection; intros.
elimtype False; omega.
(* case n > 0 *)
intros f Hinto Hinj.
pose (k := f n).
assert (Hbound_k: k < S n) by (apply Hinto; omega).
(* transposition n <-> k *)
pose (trans i :=
  if eq_nat_dec i n then k else
    if eq_nat_dec i k then n else i).
pose (g i := trans (f i)).

(* first step: g maps [0;n[ to [0;n[ *)

assert (Ginto: into n g).
  unfold into, g, trans; intros.
  destruct (eq_nat_dec (f i) n).
  (* f i = n *)
  assert (h : k < n \/ k = n) by omega.
  destruct h; auto.
  elimtype False.
  clear g trans; subst.
  assert (f i = i) by (apply Hinj; auto with * ).
  omega.
  (* f i <> n *)
  destruct (eq_nat_dec (f i) k).
  (* f i = k *)
  elimtype False.
  assert (i = n) by (apply Hinj; auto with * ).
  omega.
  (* f i <> k *)
  assert (f i < S n) by (apply Hinto; omega).
  omega.

(* second step: trans is injective *)

assert (trans_inj: injection (S n) trans).
  unfold injection, trans; intros.
  destruct (eq_nat_dec i n).
  (* i = n *)
  destruct (eq_nat_dec j n); auto with *.
  (* i = n and j <> n *)
  destruct (eq_nat_dec j k); omega. 
  (* i <> n *)
  destruct (eq_nat_dec i k); auto with *.
  destruct (eq_nat_dec j n); auto with *.
  destruct (eq_nat_dec j k); omega. 
  (* i <> n and i <> k *)
  destruct (eq_nat_dec j n); auto with *.
  destruct (eq_nat_dec j k); omega.

(* third step: g is injective on [0;n[  *)
assert (Ginj: injection n g).
  red; intros.
  apply Hinj; auto with *.

(* fourth step: g is surjective (by induction hypothesis) *)

assert (Gsurj: surjection n g).
  apply IHn with (f:=g); auto.

(* fifth step: f = trans o g *)

assert (f_is_trans_o_g: forall i, f i = trans (g i)).
  intro i; unfold g, trans.
  destruct (eq_nat_dec (f i) n).
  destruct (eq_nat_dec k n); auto with *.
  destruct (eq_nat_dec k k); auto with *.
  destruct (eq_nat_dec (f i) k).
  destruct (eq_nat_dec n n); auto with *.
  destruct (eq_nat_dec (f i) n); auto with *.
  destruct (eq_nat_dec (f i) k); auto with *.

(* conclusion *)
red; intros.
assert (h: i = k \/ i <> k) by omega.
destruct h.
(* case i = k: the preimage is n *)
exists n; auto.
(* case i <> k *)
assert (h: i = n \/ i <> n) by omega.
destruct h.
(* case i = n: the preimage is the preimage of k by g *)
elim Gsurj with (i:=k).
intros x (h1,h2).
exists x.
split; auto.
rewrite f_is_trans_o_g.
rewrite h2.
unfold trans.
destruct (eq_nat_dec k n); auto with *.
destruct (eq_nat_dec k k); auto with *.
omega.
(* case i <> n and i <> k:
   the preimage is the preimage of i by g
 *)
elim Gsurj with (i:=i).
intros x (h1,h2).
exists x.
split; auto.
rewrite f_is_trans_o_g.
rewrite h2.
unfold trans.
destruct (eq_nat_dec i n); auto with *.
destruct (eq_nat_dec i k); auto with *.
omega.
Qed.



(* lifting the theorem to Z *)

Require Import ZArith.
Open Scope Z_scope.

Theorem lifting: 
  forall n:Z,
  forall f:Z -> Z,
    (forall x:Z, 0 <= x < n -> 0 <= f x < n) ->
    exists g:nat -> nat,
      forall i:nat, Z_of_nat i < n -> Z_of_nat (g i) = f (Z_of_nat i).
intros n f Hpos.
exists (fun n => Zabs_nat (f (Z_of_nat n))).
intros i Hi_inf_n.
rewrite inj_Zabs_nat.
rewrite Zabs_eq; auto.
generalize (Hpos (Z_of_nat i)); auto with *.
Qed.

Theorem Zinjective_implies_surjective:
  forall n:Z,
  forall f:Z -> Z,
  (forall i:Z, 0 <= i < n -> 0 <= f i < n) ->
  (forall i j:Z, 0 <= i < n -> 0 <= j < n -> f i = f j -> i = j) ->
  forall i:Z, 0 <= i < n -> exists k:Z, 0 <= k < n /\ f k = i.
intros n f Hinto.
elim (lifting n f Hinto).
intros g Heq_g_f Hinj i Hi_inf_n.
assert (n_pos: 0 <= n) by omega.
elim (Z_of_nat_complete_inf n n_pos).
intros m Heq_n_m.

(* g is into *)

assert (Hinto_g: into m g).
  red; intros i0 Hinter.
  assert (0 <= f (Z_of_nat i0) < n) by (apply Hinto; omega).
  apply inj_lt_rev; auto with *.
  rewrite Heq_g_f; auto with *.

(* g is injective *)

assert (Hinj_g: injection m g).
  red; intros i0 j0 Hinter_i Hinter_j Heq_gi_gj.
  apply inj_eq_rev.
  apply Hinj; auto with *.
  repeat rewrite <- Heq_g_f; auto with *.

(* conclusion *)
generalize (injective_implies_surjective m g Hinto_g Hinj_g).
intro Hsurj_g.
assert (i_pos: 0 <= i) by omega.
elim (Z_of_nat_complete_inf i i_pos).
intros j Heq_j_i.
elim (Hsurj_g j); auto with *.
intros x (inter_x, eq_x).
exists (Z_of_nat x).
split; auto with *.
rewrite <- Heq_g_f; omega. 
Qed.




(* Why3 assumption *)
Definition injective (a:(@map.Map.map Z _ Z _)) (n:Z): Prop := forall (i:Z)
  (j:Z), ((0%Z <= i)%Z /\ (i < n)%Z) -> (((0%Z <= j)%Z /\ (j < n)%Z) ->
  ((~ (i = j)) -> ~ ((map.Map.get a i) = (map.Map.get a j)))).

(* Why3 assumption *)
Definition surjective (a:(@map.Map.map Z _ Z _)) (n:Z): Prop := forall (i:Z),
  ((0%Z <= i)%Z /\ (i < n)%Z) -> exists j:Z, ((0%Z <= j)%Z /\ (j < n)%Z) /\
  ((map.Map.get a j) = i).

(* Why3 assumption *)
Definition range (a:(@map.Map.map Z _ Z _)) (n:Z): Prop := forall (i:Z),
  ((0%Z <= i)%Z /\ (i < n)%Z) -> ((0%Z <= (map.Map.get a i))%Z /\
  ((map.Map.get a i) < n)%Z).

(* Why3 goal *)
Lemma injective_surjective : forall (a:(@map.Map.map Z _ Z _)) (n:Z),
  (injective a n) -> ((range a n) -> (surjective a n)).
unfold injective, range, surjective.
intros a n h1 h2.
intros.
apply Zinjective_implies_surjective; auto.
intros.
assert (h: (i0 = j \/ i0 <> j)%Z) by omega.
destruct h; auto.
red in h1.
elimtype False; apply h1 with i0 j; clear h1; auto.
Qed.

