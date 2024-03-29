(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Definition unit  := unit.

Parameter qtmark : Type.

Parameter at1: forall (a:Type), a -> qtmark -> a.

Implicit Arguments at1.

Parameter old: forall (a:Type), a -> a.

Implicit Arguments old.

Definition implb(x:bool) (y:bool): bool := match (x,
  y) with
  | (true, false) => false
  | (_, _) => true
  end.

Definition key  := Z.

Definition value  := Z.

Inductive color  :=
  | Red : color 
  | Black : color .

Inductive tree  :=
  | Leaf : tree 
  | Node : color -> tree -> Z -> Z -> tree -> tree .

Set Implicit Arguments.
Fixpoint memt(t:tree) (k:Z) (v:Z) {struct t}: Prop :=
  match t with
  | Leaf => False
  | (Node _ l kqt vqt r) => ((k = kqt) /\ (v = vqt)) \/ ((memt l k v) \/
      (memt r k v))
  end.
Unset Implicit Arguments.

Axiom memt_color : forall (l:tree) (r:tree) (k:Z) (kqt:Z) (v:Z) (vqt:Z)
  (c:color) (cqt:color), (memt (Node c l k v r) kqt vqt) -> (memt (Node cqt l
  k v r) kqt vqt).

Definition lt_tree(x:Z) (t:tree): Prop := forall (k:Z), forall (v:Z), (memt t
  k v) -> (k <  x)%Z.

Definition gt_tree(x:Z) (t:tree): Prop := forall (k:Z), forall (v:Z), (memt t
  k v) -> (x <  k)%Z.

Axiom lt_leaf : forall (x:Z), (lt_tree x Leaf).

Axiom gt_leaf : forall (x:Z), (gt_tree x Leaf).

Axiom lt_tree_node : forall (x:Z) (y:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (lt_tree x l) -> ((lt_tree x r) -> ((y <  x)%Z -> (lt_tree x (Node c l y v
  r)))).

Axiom gt_tree_node : forall (x:Z) (y:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (gt_tree x l) -> ((gt_tree x r) -> ((x <  y)%Z -> (gt_tree x (Node c l y v
  r)))).

Axiom lt_node_lt : forall (x:Z) (y:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (lt_tree x (Node c l y v r)) -> (y <  x)%Z.

Axiom gt_node_gt : forall (x:Z) (y:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (gt_tree x (Node c l y v r)) -> (x <  y)%Z.

Axiom lt_left : forall (x:Z) (y:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (lt_tree x (Node c l y v r)) -> (lt_tree x l).

Axiom lt_right : forall (x:Z) (y:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (lt_tree x (Node c l y v r)) -> (lt_tree x r).

Axiom gt_left : forall (x:Z) (y:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (gt_tree x (Node c l y v r)) -> (gt_tree x l).

Axiom gt_right : forall (x:Z) (y:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (gt_tree x (Node c l y v r)) -> (gt_tree x r).

Axiom lt_tree_not_in : forall (x:Z) (t:tree), (lt_tree x t) -> forall (v:Z),
  ~ (memt t x v).

Axiom lt_tree_trans : forall (x:Z) (y:Z), (x <  y)%Z -> forall (t:tree),
  (lt_tree x t) -> (lt_tree y t).

Axiom gt_tree_not_in : forall (x:Z) (t:tree), (gt_tree x t) -> forall (v:Z),
  ~ (memt t x v).

Axiom gt_tree_trans : forall (x:Z) (y:Z), (y <  x)%Z -> forall (t:tree),
  (gt_tree x t) -> (gt_tree y t).

Set Implicit Arguments.
Fixpoint bst(t:tree) {struct t}: Prop :=
  match t with
  | Leaf => True
  | (Node _ l k v r) => (bst l) /\ ((bst r) /\ ((lt_tree k l) /\ (gt_tree k
      r)))
  end.
Unset Implicit Arguments.

Axiom bst_Leaf : (bst Leaf).

Axiom bst_left : forall (k:Z) (v:Z) (l:tree) (r:tree) (c:color), (bst (Node c
  l k v r)) -> (bst l).

Axiom bst_right : forall (k:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (bst (Node c l k v r)) -> (bst r).

Axiom bst_color : forall (c:color) (cqt:color) (k:Z) (v:Z) (l:tree) (r:tree),
  (bst (Node c l k v r)) -> (bst (Node cqt l k v r)).

Axiom rotate_left : forall (kx:Z) (ky:Z) (vx:Z) (vy:Z) (a:tree) (b:tree)
  (c:tree) (c1:color) (c2:color) (c3:color) (c4:color), (bst (Node c1 a kx vx
  (Node c2 b ky vy c))) -> (bst (Node c3 (Node c4 a kx vx b) ky vy c)).

Axiom rotate_right : forall (kx:Z) (ky:Z) (vx:Z) (vy:Z) (a:tree) (b:tree)
  (c:tree) (c1:color) (c2:color) (c3:color) (c4:color), (bst (Node c3
  (Node c4 a kx vx b) ky vy c)) -> (bst (Node c1 a kx vx (Node c2 b ky vy
  c))).

Definition is_not_red(t:tree): Prop :=
  match t with
  | (Node Red _ _ _ _) => False
  | (Leaf|(Node Black _ _ _ _)) => True
  end.

Set Implicit Arguments.
Fixpoint rbtree(n:Z) (t:tree) {struct t}: Prop :=
  match t with
  | Leaf => (n = 0%Z)
  | (Node Red l _ _ r) => (rbtree n l) /\ ((rbtree n r) /\ ((is_not_red l) /\
      (is_not_red r)))
  | (Node Black l _ _ r) => (rbtree (n - 1%Z)%Z l) /\ (rbtree (n - 1%Z)%Z r)
  end.
Unset Implicit Arguments.

Axiom rbtree_Leaf : (rbtree 0%Z Leaf).

Axiom rbtree_Node1 : forall (k:Z) (v:Z), (rbtree 0%Z (Node Red Leaf k v
  Leaf)).

Axiom rbtree_left : forall (x:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (exists n:Z, (rbtree n (Node c l x v r))) -> exists n:Z, (rbtree n l).

Axiom rbtree_right : forall (x:Z) (v:Z) (l:tree) (r:tree) (c:color),
  (exists n:Z, (rbtree n (Node c l x v r))) -> exists n:Z, (rbtree n r).

Definition almost_rbtree(n:Z) (t:tree): Prop :=
  match t with
  | Leaf => (n = 0%Z)
  | (Node Red l _ _ r) => (rbtree n l) /\ (rbtree n r)
  | (Node Black l _ _ r) => (rbtree (n - 1%Z)%Z l) /\ (rbtree (n - 1%Z)%Z r)
  end.

Axiom rbtree_almost_rbtree : forall (n:Z) (t:tree), (rbtree n t) ->
  (almost_rbtree n t).

Axiom rbtree_almost_rbtree_ex : forall (s:tree), (exists n:Z, (rbtree n
  s)) -> exists n:Z, (almost_rbtree n s).

Axiom almost_rbtree_rbtree_black : forall (x:Z) (v:Z) (l:tree) (r:tree)
  (n:Z), (almost_rbtree n (Node Black l x v r)) -> (rbtree n (Node Black l x
  v r)).

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Theorem WP_parameter_rbalance : forall (l:tree), forall (k:Z), forall (v:Z),
  forall (r:tree), ((lt_tree k l) /\ ((gt_tree k r) /\ ((bst l) /\
  (bst r)))) ->
  match r with
  | ((Node Red (Node Red b ky vy c) kz vz d)|(Node Red b ky vy (Node Red c kz
      vz d))) => forall (n:Z), (almost_rbtree n r) -> ((rbtree n l) ->
      (rbtree (n + 1%Z)%Z (Node Red (Node Black l k v b) ky vy (Node Black c
      kz vz d))))
  | _ => True
  end.
(* YOU MAY EDIT THE PROOF BELOW *)
intuition.

destruct r; intuition.
destruct c; intuition.
destruct r1; intuition.
destruct r2; intuition.
destruct c; intuition.
inversion H2.
inversion H5.
simpl in H6.
subst.
simpl rbtree; intuition.

destruct c; intuition.
inversion H2.
simpl in H5.
simpl rbtree.
replace (n+1-1)%Z with n by omega; intuition.

destruct r2; intuition.
destruct c; intuition.
inversion H2.
simpl in H5.
simpl in H6.
simpl rbtree.
replace (n+1-1)%Z with n by omega; intuition.
Qed.
(* DO NOT EDIT BELOW *)


