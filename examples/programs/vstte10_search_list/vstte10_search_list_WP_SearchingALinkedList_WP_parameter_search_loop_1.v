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

Inductive list (a:Type) :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Set Contextual Implicit.
Implicit Arguments Nil.
Unset Contextual Implicit.
Implicit Arguments Cons.

Parameter length: forall (a:Type), (list a)  -> Z.

Implicit Arguments length.

Axiom length_def : forall (a:Type), forall (l:(list a)),
  match l with
  | Nil  => ((length l) = 0%Z)
  | Cons _ r => ((length l) = (1%Z + (length r))%Z)
  end.

Axiom Length_nonnegative : forall (a:Type), forall (l:(list a)),
  (0%Z <= (length l))%Z.

Axiom Length_nil : forall (a:Type), forall (l:(list a)),
  ((length l) = 0%Z) <-> (l = (Nil:(list a))).

Inductive option (a:Type) :=
  | None : option a
  | Some : a -> option a.
Set Contextual Implicit.
Implicit Arguments None.
Unset Contextual Implicit.
Implicit Arguments Some.

Parameter nth: forall (a:Type), Z -> (list a)  -> (option a).

Implicit Arguments nth.

Axiom nth_def : forall (a:Type), forall (n:Z) (l:(list a)),
  match l with
  | Nil  => ((nth n l) = (None:(option a)))
  | Cons x r => ((n = 0%Z) -> ((nth n l) = (Some x))) /\ ((~ (n = 0%Z)) ->
      ((nth n l) = (nth (n - 1%Z)%Z r)))
  end.

Definition zero_at(l:(list Z)) (i:Z): Prop := ((nth i l) = (Some 0%Z)) /\
  forall (j:Z), ((0%Z <= j)%Z /\ (j <  i)%Z) -> ~ ((nth j l) = (Some 0%Z)).

Definition no_zero(l:(list Z)): Prop := forall (j:Z), ((0%Z <= j)%Z /\
  (j <  (length l))%Z) -> ~ ((nth j l) = (Some 0%Z)).

Inductive ref (a:Type) :=
  | mk_ref : a -> ref a.
Implicit Arguments mk_ref.

Definition contents (a:Type)(u:(ref a)): a :=
  match u with
  | mk_ref contents1 => contents1
  end.
Implicit Arguments contents.

Definition hd (a:Type)(l:(list a)): (option a) :=
  match l with
  | Nil  => (None:(option a))
  | Cons h _ => (Some h)
  end.
Implicit Arguments hd.

Definition tl (a:Type)(l:(list a)): (option (list a)) :=
  match l with
  | Nil  => (None:(option (list a)))
  | Cons _ t => (Some t)
  end.
Implicit Arguments tl.

Theorem WP_parameter_search_loop : forall (l:(list Z)), forall (s:(list Z)),
  forall (i:Z), ((0%Z <= i)%Z /\ (((i + (length s))%Z = (length l)) /\
  ((forall (j:Z), (0%Z <= j)%Z -> ((nth j s) = (nth (i + j)%Z l))) /\
  forall (j:Z), ((0%Z <= j)%Z /\ (j <  i)%Z) -> ~ ((nth j
  l) = (Some 0%Z))))) -> ((~ (s = (Nil:(list Z)))) -> ((~ (s = (Nil:(list
  Z)))) -> forall (result:Z),
  (match s with
  | Nil  => (None:(option Z))
  | Cons h _ => (Some h)
  end = (Some result)) -> ((result = 0%Z) -> ((((0%Z <= i)%Z /\
  (i <  (length l))%Z) /\ (zero_at l i)) \/ ((i = (length l)) /\
  (no_zero l)))))).
(* YOU MAY EDIT THE PROOF BELOW *)
intuition.
destruct s.
discriminate H4.
injection H4; intros; subst; clear H4.
clear H0 H1.
left.
split.
rewrite (length_def _ (Cons 0%Z s)) in H.
generalize (Length_nonnegative _ s).
omega.
red; intuition.
assert (H0: (0 <= 0)%Z) by omega.
generalize (H3 0%Z H0).
generalize (nth_def _ 0%Z (Cons 0%Z s)).
ring_simplify (i+0)%Z.
intuition.
rewrite H4 in H1.
auto.
Qed.
(* DO NOT EDIT BELOW *)


