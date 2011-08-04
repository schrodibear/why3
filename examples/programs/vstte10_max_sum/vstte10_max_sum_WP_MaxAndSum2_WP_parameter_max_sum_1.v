(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Definition unit  := unit.

Parameter mark : Type.

Parameter at1: forall (a:Type), a -> mark  -> a.

Implicit Arguments at1.

Parameter old: forall (a:Type), a  -> a.

Implicit Arguments old.

Inductive ref (a:Type) :=
  | mk_ref : a -> ref a.
Implicit Arguments mk_ref.

Definition contents (a:Type)(u:(ref a)): a :=
  match u with
  | mk_ref contents1 => contents1
  end.
Implicit Arguments contents.

Parameter map : forall (a:Type) (b:Type), Type.

Parameter get: forall (a:Type) (b:Type), (map a b) -> a  -> b.

Implicit Arguments get.

Parameter set: forall (a:Type) (b:Type), (map a b) -> a -> b  -> (map a b).

Implicit Arguments set.

Axiom Select_eq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) -> ((get (set m a1 b1)
  a2) = b1).

Axiom Select_neq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1)
  a2) = (get m a2)).

Parameter const: forall (b:Type) (a:Type), b  -> (map a b).

Set Contextual Implicit.
Implicit Arguments const.
Unset Contextual Implicit.

Axiom Const : forall (b:Type) (a:Type), forall (b1:b) (a1:a), ((get (const(
  b1):(map a b)) a1) = b1).

Inductive array (a:Type) :=
  | mk_array : Z -> (map Z a) -> array a.
Implicit Arguments mk_array.

Definition elts (a:Type)(u:(array a)): (map Z a) :=
  match u with
  | mk_array _ elts1 => elts1
  end.
Implicit Arguments elts.

Definition length (a:Type)(u:(array a)): Z :=
  match u with
  | mk_array length1 _ => length1
  end.
Implicit Arguments length.

Definition get1 (a:Type)(a1:(array a)) (i:Z): a := (get (elts a1) i).
Implicit Arguments get1.

Definition set1 (a:Type)(a1:(array a)) (i:Z) (v:a): (array a) :=
  match a1 with
  | mk_array xcl0 _ => (mk_array xcl0 (set (elts a1) i v))
  end.
Implicit Arguments set1.

Definition container  := (map Z Z).

Parameter sum: (map Z Z) -> Z -> Z  -> Z.


Axiom Sum_def_empty : forall (c:(map Z Z)) (i:Z) (j:Z), (j <= i)%Z -> ((sum c
  i j) = 0%Z).

Axiom Sum_def_non_empty : forall (c:(map Z Z)) (i:Z) (j:Z), (i <  j)%Z ->
  ((sum c i j) = ((get c i) + (sum c (i + 1%Z)%Z j))%Z).

Axiom Sum_right_extension : forall (c:(map Z Z)) (i:Z) (j:Z), (i <  j)%Z ->
  ((sum c i j) = ((sum c i (j - 1%Z)%Z) + (get c (j - 1%Z)%Z))%Z).

Axiom Sum_transitivity : forall (c:(map Z Z)) (i:Z) (k:Z) (j:Z),
  ((i <= k)%Z /\ (k <= j)%Z) -> ((sum c i j) = ((sum c i k) + (sum c k
  j))%Z).

Axiom Sum_eq : forall (c1:(map Z Z)) (c2:(map Z Z)) (i:Z) (j:Z),
  (forall (k:Z), ((i <= k)%Z /\ (k <  j)%Z) -> ((get c1 k) = (get c2 k))) ->
  ((sum c1 i j) = (sum c2 i j)).

Definition sum1(a:(array Z)) (l:Z) (h:Z): Z := (sum (elts a) l h).

Definition is_max(a:(array Z)) (l:Z) (h:Z) (m:Z): Prop := (forall (k:Z),
  ((l <= k)%Z /\ (k <  h)%Z) -> ((get1 a k) <= m)%Z) /\ (((h <= l)%Z /\
  (m = 0%Z)) \/ ((l <  h)%Z /\ exists k:Z, ((l <= k)%Z /\ (k <  h)%Z) /\
  (m = (get1 a k)))).

Theorem WP_parameter_max_sum : forall (a:Z), forall (n:Z), forall (a1:(map Z
  Z)), (((0%Z <= n)%Z /\ (n = a)) /\ forall (i:Z), ((0%Z <= i)%Z /\
  (i <  n)%Z) -> (0%Z <= (get a1 i))%Z) -> ((0%Z <= (n - 1%Z)%Z)%Z ->
  forall (m:Z), forall (s:Z), forall (i:Z), ((0%Z <= i)%Z /\
  (i <= (n - 1%Z)%Z)%Z) -> (((s = (sum a1 0%Z i)) /\ ((is_max (mk_array a a1)
  0%Z i m) /\ (s <= (i * m)%Z)%Z)) -> (((0%Z <= i)%Z /\ (i <  a)%Z) ->
  ((m <  (get a1 i))%Z -> (((0%Z <= i)%Z /\ (i <  a)%Z) -> forall (m1:Z),
  (m1 = (get a1 i)) -> (((0%Z <= i)%Z /\ (i <  a)%Z) -> forall (s1:Z),
  (s1 = (s + (get a1 i))%Z) -> (s1 <= ((i + 1%Z)%Z * m1)%Z)%Z)))))).
(* YOU MAY EDIT THE PROOF BELOW *)
intuition.
ring_simplify.
subst.
apply Zplus_le_compat_r.
apply Zle_trans with (i * m)%Z; auto.
auto with *.
Qed.
(* DO NOT EDIT BELOW *)

