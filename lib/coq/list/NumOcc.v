(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require list.List.
Require list.Length.
Require list.Mem.
Require list.Append.
Require list.Reverse.

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require list.List.
Require list.Length.
Require list.Mem.
Require list.Append.
Require list.Reverse.

(* Why3 goal *)
Definition num_occ: forall {a:Type} {a_WT:WhyType a}, a -> (list a) -> Z.
intros a a_WT x.
exact (fix num_occ (l : list a) : int :=
  match l with
  | nil => 0
  | cons y r => (if why_decidable_eq x y then 1 else 0) + num_occ r
  end)%Z.
Defined.

(* Why3 goal *)
Lemma num_occ_def : forall {a:Type} {a_WT:WhyType a}, forall (x:a)
  (l:(list a)),
  match l with
  | Init.Datatypes.nil => ((num_occ x l) = 0%Z)
  | (Init.Datatypes.cons y r) => ((x = y) -> ((num_occ x
      l) = (1%Z + (num_occ x r))%Z)) /\ ((~ (x = y)) -> ((num_occ x
      l) = (0%Z + (num_occ x r))%Z))
  end.
Proof.
intros a a_WT x [|y r].
easy.
split ; intros H.
change ((if why_decidable_eq x y then 1 else 0) + num_occ x r = 1 + num_occ x r)%Z.
now case why_decidable_eq.
change ((if why_decidable_eq x y then 1 else 0) + num_occ x r = 0 + num_occ x r)%Z.
now case why_decidable_eq.
Qed.

(* Why3 goal *)
Lemma Num_Occ_NonNeg : forall {a:Type} {a_WT:WhyType a}, forall (x:a)
  (l:(list a)), (0%Z <= (num_occ x l))%Z.
intros a a_WT x l.
induction l as [|lh lt IHl].
easy.
simpl.
case why_decidable_eq ; intros H.
omega.
easy.
Qed.

(* Why3 goal *)
Lemma Mem_Num_Occ : forall {a:Type} {a_WT:WhyType a}, forall (x:a)
  (l:(list a)), (list.Mem.mem x l) <-> (0%Z < (num_occ x l))%Z.
Proof.
intros a a_WT x l.
induction l as [|lh lt IHl].
now split.
simpl.
case why_decidable_eq ; intros H ; split.
intros _.
clear.
generalize (Num_Occ_NonNeg x lt).
omega.
now left.
intros [H'|H'] ; try easy.
now apply IHl.
right.
now apply IHl.
Qed.

(* Why3 goal *)
Lemma Append_Num_Occ : forall {a:Type} {a_WT:WhyType a}, forall (x:a)
  (l1:(list a)) (l2:(list a)), ((num_occ x
  (Init.Datatypes.app l1 l2)) = ((num_occ x l1) + (num_occ x l2))%Z).
Proof.
intros a a_WT x l1 l2.
induction l1 as [|l1h l1t IHl1].
easy.
simpl.
rewrite IHl1.
rewrite Zplus_assoc.
now case why_decidable_eq.
Qed.

(* Why3 goal *)
Lemma reverse_num_occ : forall {a:Type} {a_WT:WhyType a}, forall (x:a)
  (l:(list a)), ((num_occ x l) = (num_occ x (Lists.List.rev l))).
intros a a_WT x l.
induction l; simpl.
auto.
rewrite Append_Num_Occ.
rewrite <- IHl.
ring_simplify.
simpl (num_occ x (a0 :: nil))%list.
ring.
Qed.

