(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.

(* Why3 assumption *)
Definition unit  := unit.

(* Why3 assumption *)
Inductive list (a:Type) :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Set Contextual Implicit.
Implicit Arguments Nil.
Unset Contextual Implicit.
Implicit Arguments Cons.

(* Why3 assumption *)
Inductive option (a:Type) :=
  | None : option a
  | Some : a -> option a.
Set Contextual Implicit.
Implicit Arguments None.
Unset Contextual Implicit.
Implicit Arguments Some.

Parameter nth: forall (a:Type), Z -> (list a) -> (option a).
Implicit Arguments nth.

Axiom nth_def : forall (a:Type), forall (n:Z) (l:(list a)),
  match l with
  | Nil => ((nth n l) = (None :(option a)))
  | (Cons x r) => ((n = 0%Z) -> ((nth n l) = (Some x))) /\ ((~ (n = 0%Z)) ->
      ((nth n l) = (nth (n - 1%Z)%Z r)))
  end.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint length (a:Type)(l:(list a)) {struct l}: Z :=
  match l with
  | Nil => 0%Z
  | (Cons _ r) => (1%Z + (length r))%Z
  end.
Unset Implicit Arguments.

Axiom Length_nonnegative : forall (a:Type), forall (l:(list a)),
  (0%Z <= (length l))%Z.

Axiom Length_nil : forall (a:Type), forall (l:(list a)),
  ((length l) = 0%Z) <-> (l = (Nil :(list a))).

Axiom nth_none_1 : forall (a:Type), forall (l:(list a)) (i:Z), (i < 0%Z)%Z ->
  ((nth i l) = (None :(option a))).

Axiom nth_none_2 : forall (a:Type), forall (l:(list a)) (i:Z),
  ((length l) <= i)%Z -> ((nth i l) = (None :(option a))).

Axiom nth_none_3 : forall (a:Type), forall (l:(list a)) (i:Z), ((nth i
  l) = (None :(option a))) -> ((i < 0%Z)%Z \/ ((length l) <= i)%Z).

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint infix_plpl (a:Type)(l1:(list a)) (l2:(list a)) {struct l1}: (list
  a) :=
  match l1 with
  | Nil => l2
  | (Cons x1 r1) => (Cons x1 (infix_plpl r1 l2))
  end.
Unset Implicit Arguments.

Axiom Append_assoc : forall (a:Type), forall (l1:(list a)) (l2:(list a))
  (l3:(list a)), ((infix_plpl l1 (infix_plpl l2
  l3)) = (infix_plpl (infix_plpl l1 l2) l3)).

Axiom Append_l_nil : forall (a:Type), forall (l:(list a)), ((infix_plpl l
  (Nil :(list a))) = l).

Axiom Append_length : forall (a:Type), forall (l1:(list a)) (l2:(list a)),
  ((length (infix_plpl l1 l2)) = ((length l1) + (length l2))%Z).

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint mem (a:Type)(x:a) (l:(list a)) {struct l}: Prop :=
  match l with
  | Nil => False
  | (Cons y r) => (x = y) \/ (mem x r)
  end.
Unset Implicit Arguments.

Axiom mem_append : forall (a:Type), forall (x:a) (l1:(list a)) (l2:(list a)),
  (mem x (infix_plpl l1 l2)) <-> ((mem x l1) \/ (mem x l2)).

Axiom mem_decomp : forall (a:Type), forall (x:a) (l:(list a)), (mem x l) ->
  exists l1:(list a), exists l2:(list a), (l = (infix_plpl l1 (Cons x l2))).

Axiom nth_append_1 : forall (a:Type), forall (l1:(list a)) (l2:(list a))
  (i:Z), (i < (length l1))%Z -> ((nth i (infix_plpl l1 l2)) = (nth i l1)).

Axiom nth_append_2 : forall (a:Type), forall (l1:(list a)) (l2:(list a))
  (i:Z), ((length l1) <= i)%Z -> ((nth i (infix_plpl l1
  l2)) = (nth (i - (length l1))%Z l2)).

Parameter map : forall (a:Type) (b:Type), Type.

Parameter get: forall (a:Type) (b:Type), (map a b) -> a -> b.
Implicit Arguments get.

Parameter set: forall (a:Type) (b:Type), (map a b) -> a -> b -> (map a b).
Implicit Arguments set.

Axiom Select_eq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) -> ((get (set m a1 b1)
  a2) = b1).

Axiom Select_neq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1)
  a2) = (get m a2)).

Parameter const: forall (a:Type) (b:Type), b -> (map a b).
Set Contextual Implicit.
Implicit Arguments const.
Unset Contextual Implicit.

Axiom Const : forall (a:Type) (b:Type), forall (b1:b) (a1:a),
  ((get (const b1:(map a b)) a1) = b1).

(* Why3 assumption *)
Inductive array (a:Type) :=
  | mk_array : Z -> (map Z a) -> array a.
Implicit Arguments mk_array.

(* Why3 assumption *)
Definition elts (a:Type)(v:(array a)): (map Z a) :=
  match v with
  | (mk_array x x1) => x1
  end.
Implicit Arguments elts.

(* Why3 assumption *)
Definition length1 (a:Type)(v:(array a)): Z :=
  match v with
  | (mk_array x x1) => x
  end.
Implicit Arguments length1.

(* Why3 assumption *)
Definition get1 (a:Type)(a1:(array a)) (i:Z): a := (get (elts a1) i).
Implicit Arguments get1.

(* Why3 assumption *)
Definition set1 (a:Type)(a1:(array a)) (i:Z) (v:a): (array a) :=
  (mk_array (length1 a1) (set (elts a1) i v)).
Implicit Arguments set1.

(* Why3 assumption *)
Inductive buffer (a:Type) :=
  | mk_buffer : Z -> Z -> (array a) -> (list a) -> buffer a.
Implicit Arguments mk_buffer.

(* Why3 assumption *)
Definition sequence (a:Type)(v:(buffer a)): (list a) :=
  match v with
  | (mk_buffer x x1 x2 x3) => x3
  end.
Implicit Arguments sequence.

(* Why3 assumption *)
Definition data (a:Type)(v:(buffer a)): (array a) :=
  match v with
  | (mk_buffer x x1 x2 x3) => x2
  end.
Implicit Arguments data.

(* Why3 assumption *)
Definition len (a:Type)(v:(buffer a)): Z :=
  match v with
  | (mk_buffer x x1 x2 x3) => x1
  end.
Implicit Arguments len.

(* Why3 assumption *)
Definition first (a:Type)(v:(buffer a)): Z :=
  match v with
  | (mk_buffer x x1 x2 x3) => x
  end.
Implicit Arguments first.

(* Why3 assumption *)
Definition size (a:Type)(b:(buffer a)): Z := (length1 (data b)).
Implicit Arguments size.

(* Why3 assumption *)
Definition buffer_invariant (a:Type)(b:(buffer a)): Prop :=
  ((0%Z <= (first b))%Z /\ ((first b) < (size b))%Z) /\
  (((0%Z <= (len b))%Z /\ ((len b) <= (size b))%Z) /\
  (((len b) = (length (sequence b))) /\ forall (i:Z), ((0%Z <= i)%Z /\
  (i < (len b))%Z) -> (((((first b) + i)%Z < (size b))%Z -> ((nth i
  (sequence b)) = (Some (get1 (data b) ((first b) + i)%Z)))) /\
  ((0%Z <= (((first b) + i)%Z - (size b))%Z)%Z -> ((nth i
  (sequence b)) = (Some (get1 (data b)
  (((first b) + i)%Z - (size b))%Z))))))).
Implicit Arguments buffer_invariant.

Require Import Why3. Ltac ae := why3 "alt-ergo" timelimit 3.

(* Why3 goal *)
Theorem WP_parameter_pop : forall (a:Type), forall (b:Z), forall (rho:(list
  a)) (rho1:(map Z a)) (rho2:Z) (rho3:Z), ((0%Z < rho2)%Z /\
  (((0%Z <= (first (mk_buffer rho3 rho2 (mk_array b rho1) rho)))%Z /\
  ((first (mk_buffer rho3 rho2 (mk_array b rho1)
  rho)) < (size (mk_buffer rho3 rho2 (mk_array b rho1) rho)))%Z) /\
  (((0%Z <= (len (mk_buffer rho3 rho2 (mk_array b rho1) rho)))%Z /\
  ((len (mk_buffer rho3 rho2 (mk_array b rho1) rho)) <= (size (mk_buffer rho3
  rho2 (mk_array b rho1) rho)))%Z) /\ (((len (mk_buffer rho3 rho2 (mk_array b
  rho1) rho)) = (length (sequence (mk_buffer rho3 rho2 (mk_array b rho1)
  rho)))) /\ forall (i:Z), ((0%Z <= i)%Z /\ (i < (len (mk_buffer rho3 rho2
  (mk_array b rho1) rho)))%Z) -> (((((first (mk_buffer rho3 rho2 (mk_array b
  rho1) rho)) + i)%Z < (size (mk_buffer rho3 rho2 (mk_array b rho1)
  rho)))%Z -> ((nth i (sequence (mk_buffer rho3 rho2 (mk_array b rho1)
  rho))) = (Some (get1 (data (mk_buffer rho3 rho2 (mk_array b rho1) rho))
  ((first (mk_buffer rho3 rho2 (mk_array b rho1) rho)) + i)%Z)))) /\
  ((0%Z <= (((first (mk_buffer rho3 rho2 (mk_array b rho1)
  rho)) + i)%Z - (size (mk_buffer rho3 rho2 (mk_array b rho1) rho)))%Z)%Z ->
  ((nth i (sequence (mk_buffer rho3 rho2 (mk_array b rho1)
  rho))) = (Some (get1 (data (mk_buffer rho3 rho2 (mk_array b rho1) rho))
  (((first (mk_buffer rho3 rho2 (mk_array b rho1)
  rho)) + i)%Z - (size (mk_buffer rho3 rho2 (mk_array b rho1)
  rho)))%Z))))))))) ->
  match rho with
  | Nil => True
  | (Cons _ s) => forall (rho4:(list a)), (rho4 = s) -> ((((0%Z < rho3)%Z \/
      (0%Z = rho3)) /\ (rho3 < b)%Z) -> forall (rho5:Z),
      (rho5 = (rho2 + (-1%Z)%Z)%Z) -> forall (rho6:Z),
      (rho6 = (rho3 + 1%Z)%Z) -> ((~ (rho6 = b)) -> forall (i:Z),
      ((0%Z <= i)%Z /\ (i < (len (mk_buffer rho6 rho5 (mk_array b rho1)
      rho4)))%Z) -> ((((first (mk_buffer rho6 rho5 (mk_array b rho1)
      rho4)) + i)%Z < (size (mk_buffer rho6 rho5 (mk_array b rho1)
      rho4)))%Z -> ((nth i (sequence (mk_buffer rho6 rho5 (mk_array b rho1)
      rho4))) = (Some (get1 (data (mk_buffer rho6 rho5 (mk_array b rho1)
      rho4)) ((first (mk_buffer rho6 rho5 (mk_array b rho1)
      rho4)) + i)%Z))))))
  end.
unfold get1, size; simpl.
intros a b rho rho1 rho2 rho3 (h1,((h2,h3),((h4,h5),(h6,h7)))).
destruct rho; auto.
intros; subst.
generalize (h7 (i+1)%Z).
ae.
Qed.

