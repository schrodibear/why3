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
Require list.NumOcc.

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
Require list.NumOcc.

(* Why3 assumption *)
Definition permut {a:Type} {a_WT:WhyType a} (l1:(list a))
  (l2:(list a)): Prop := forall (x:a), ((list.NumOcc.num_occ x
  l1) = (list.NumOcc.num_occ x l2)).

(* Why3 goal *)
Lemma Permut_refl : forall {a:Type} {a_WT:WhyType a}, forall (l:(list a)),
  (permut l l).
Proof.
now intros a a_WT l.
Qed.

(* Why3 goal *)
Lemma Permut_sym : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)), (permut l1 l2) -> (permut l2 l1).
Proof.
now intros a a_WT l1 l2 h1.
Qed.

(* Why3 goal *)
Lemma Permut_trans : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)) (l3:(list a)), (permut l1 l2) -> ((permut l2 l3) -> (permut
  l1 l3)).
Proof.
intros a a_WT l1 l2 l3 h1 h2 x.
now rewrite h1.
Qed.

(* Why3 goal *)
Lemma Permut_cons : forall {a:Type} {a_WT:WhyType a}, forall (x:a)
  (l1:(list a)) (l2:(list a)), (permut l1 l2) -> (permut
  (Init.Datatypes.cons x l1) (Init.Datatypes.cons x l2)).
Proof.
intros a a_WT x l1 l2 h1 y.
simpl.
now rewrite h1.
Qed.

(* Why3 goal *)
Lemma Permut_swap : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (y:a)
  (l:(list a)), (permut (Init.Datatypes.cons x (Init.Datatypes.cons y l))
  (Init.Datatypes.cons y (Init.Datatypes.cons x l))).
Proof.
intros a a_WT x y l z.
simpl.
ring.
Qed.

(* Why3 goal *)
Lemma Permut_cons_append : forall {a:Type} {a_WT:WhyType a}, forall (x:a)
  (l1:(list a)) (l2:(list a)), (permut
  (Init.Datatypes.app (Init.Datatypes.cons x l1) l2)
  (Init.Datatypes.app l1 (Init.Datatypes.cons x l2))).
Proof.
intros a a_WT x l1 l2 y.
induction l1 as [|l1h l1t IHl1].
easy.
simpl in IHl1 |- *.
rewrite <- IHl1.
ring.
Qed.

(* Why3 goal *)
Lemma Permut_assoc : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)) (l3:(list a)), (permut
  (Init.Datatypes.app (Init.Datatypes.app l1 l2) l3)
  (Init.Datatypes.app l1 (Init.Datatypes.app l2 l3))).
Proof.
intros a a_WT l1 l2 l3 y.
now rewrite List.app_assoc.
Qed.

(* Why3 goal *)
Lemma Permut_append : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)) (k1:(list a)) (k2:(list a)), (permut l1 k1) -> ((permut l2
  k2) -> (permut (Init.Datatypes.app l1 l2) (Init.Datatypes.app k1 k2))).
Proof.
intros a a_WT l1 l2 k1 k2 h1 h2 y.
rewrite 2!NumOcc.Append_Num_Occ.
now apply f_equal2.
Qed.

(* Why3 goal *)
Lemma Permut_append_swap : forall {a:Type} {a_WT:WhyType a},
  forall (l1:(list a)) (l2:(list a)), (permut (Init.Datatypes.app l1 l2)
  (Init.Datatypes.app l2 l1)).
Proof.
intros a a_WT l1 l2 y.
rewrite 2!NumOcc.Append_Num_Occ.
apply Zplus_comm.
Qed.

(* Why3 goal *)
Lemma Permut_mem : forall {a:Type} {a_WT:WhyType a}, forall (x:a)
  (l1:(list a)) (l2:(list a)), (permut l1 l2) -> ((list.Mem.mem x l1) ->
  (list.Mem.mem x l2)).
Proof.
intros a a_WT x l1 l2 h1 h2.
apply NumOcc.Mem_Num_Occ.
rewrite <- h1.
now apply NumOcc.Mem_Num_Occ.
Qed.

(* Why3 goal *)
Lemma Permut_length : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)), (permut l1 l2) ->
  ((list.Length.length l1) = (list.Length.length l2)).
Proof.
intros a a_WT l1 l2 h1.
revert l2 h1.
induction l1 as [|l1h l1t IHl1].
- destruct l2 as [|l2h l2t].
  easy.
  intros H.
  specialize (H l2h).
  contradict H.
  simpl.
  case why_decidable_eq ; intros H.
  generalize (NumOcc.Num_Occ_NonNeg l2h l2t).
  omega.
  now elim H.
- intros l2 H.
  assert (H': Mem.mem l1h l2).
    apply NumOcc.Mem_Num_Occ.
    specialize (H l1h).
    simpl in H.
    destruct (why_decidable_eq l1h l1h) as [_|H'].
    2: now elim H'.
    generalize (NumOcc.Num_Occ_NonNeg l1h l1t).
    omega.
  destruct (Append.mem_decomp _ _ H') as [l2a [l2b Hl2]].
  rewrite Hl2.
  rewrite Append.Append_length.
  change (1 + Length.length l1t = Length.length l2a + (1 + Length.length l2b))%Z.
  rewrite (IHl1 (l2a ++ l2b)%list).
  rewrite Append.Append_length.
  ring.
  rewrite Hl2 in H.
  assert (H1 := Permut_cons_append l1h l2a l2b).
  apply Permut_sym in H1.
  generalize (Permut_trans _ _ _ H H1).
  change ((l1h :: l2a) ++ l2b)%list with (l1h :: (l2a ++ l2b))%list.
  generalize (l2a ++ l2b)%list.
  clear.
  intros l H y.
  specialize (H y).
  simpl in H.
  omega.
Qed.

