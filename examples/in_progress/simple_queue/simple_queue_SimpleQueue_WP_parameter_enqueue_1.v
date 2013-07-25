(* This file is generated by Why3's Coq 8.4 driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require map.Map.

(* Why3 assumption *)
Definition unit := unit.

(* Why3 assumption *)
Inductive array
  (a:Type) {a_WT:WhyType a} :=
  | mk_array : Z -> (@map.Map.map Z _ a a_WT) -> array a.
Axiom array_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (array a).
Existing Instance array_WhyType.
Implicit Arguments mk_array [[a] [a_WT]].

(* Why3 assumption *)
Definition elts {a:Type} {a_WT:WhyType a} (v:(@array a a_WT)): (@map.Map.map
  Z _ a a_WT) := match v with
  | (mk_array x x1) => x1
  end.

(* Why3 assumption *)
Definition length {a:Type} {a_WT:WhyType a} (v:(@array a a_WT)): Z :=
  match v with
  | (mk_array x x1) => x
  end.

(* Why3 assumption *)
Definition get {a:Type} {a_WT:WhyType a} (a1:(@array a a_WT)) (i:Z): a :=
  (map.Map.get (elts a1) i).

(* Why3 assumption *)
Definition set {a:Type} {a_WT:WhyType a} (a1:(@array a a_WT)) (i:Z)
  (v:a): (@array a a_WT) := (mk_array (length a1) (map.Map.set (elts a1) i
  v)).

(* Why3 assumption *)
Definition make {a:Type} {a_WT:WhyType a} (n:Z) (v:a): (@array a a_WT) :=
  (mk_array n (map.Map.const v:(@map.Map.map Z _ a a_WT))).

(* Why3 assumption *)
Inductive list
  (a:Type) {a_WT:WhyType a} :=
  | Nil : list a
  | Cons : a -> (@list a a_WT) -> list a.
Axiom list_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (list a).
Existing Instance list_WhyType.
Implicit Arguments Nil [[a] [a_WT]].
Implicit Arguments Cons [[a] [a_WT]].

Parameter to_list: forall {a:Type} {a_WT:WhyType a}, (@array a a_WT) -> Z
  -> Z -> (@list a a_WT).

Axiom to_list_nil : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (l:Z) (u:Z), (u <= l)%Z -> ((to_list a1 l u) = (Nil :(@list
  a a_WT))).

Axiom to_list_cons : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (l:Z) (u:Z), (l < u)%Z -> ((to_list a1 l u) = (Cons (get a1 l)
  (to_list a1 (l + 1%Z)%Z u))).

Axiom to_list_one : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (l:Z), ((to_list a1 l (l + 1%Z)%Z) = (Cons (get a1 l) (Nil :(@list
  a a_WT)))).

(* Why3 assumption *)
Fixpoint infix_plpl {a:Type} {a_WT:WhyType a} (l1:(@list a a_WT)) (l2:(@list
  a a_WT)) {struct l1}: (@list a a_WT) :=
  match l1 with
  | Nil => l2
  | (Cons x1 r1) => (Cons x1 (infix_plpl r1 l2))
  end.

Axiom Append_assoc : forall {a:Type} {a_WT:WhyType a}, forall (l1:(@list
  a a_WT)) (l2:(@list a a_WT)) (l3:(@list a a_WT)), ((infix_plpl l1
  (infix_plpl l2 l3)) = (infix_plpl (infix_plpl l1 l2) l3)).

Axiom Append_l_nil : forall {a:Type} {a_WT:WhyType a}, forall (l:(@list
  a a_WT)), ((infix_plpl l (Nil :(@list a a_WT))) = l).

(* Why3 assumption *)
Fixpoint length1 {a:Type} {a_WT:WhyType a} (l:(@list
  a a_WT)) {struct l}: Z :=
  match l with
  | Nil => 0%Z
  | (Cons _ r) => (1%Z + (length1 r))%Z
  end.

Axiom Length_nonnegative : forall {a:Type} {a_WT:WhyType a}, forall (l:(@list
  a a_WT)), (0%Z <= (length1 l))%Z.

Axiom Length_nil : forall {a:Type} {a_WT:WhyType a}, forall (l:(@list
  a a_WT)), ((length1 l) = 0%Z) <-> (l = (Nil :(@list a a_WT))).

Axiom Append_length : forall {a:Type} {a_WT:WhyType a}, forall (l1:(@list
  a a_WT)) (l2:(@list a a_WT)), ((length1 (infix_plpl l1
  l2)) = ((length1 l1) + (length1 l2))%Z).

(* Why3 assumption *)
Fixpoint mem {a:Type} {a_WT:WhyType a} (x:a) (l:(@list
  a a_WT)) {struct l}: Prop :=
  match l with
  | Nil => False
  | (Cons y r) => (x = y) \/ (mem x r)
  end.

Axiom mem_append : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l1:(@list
  a a_WT)) (l2:(@list a a_WT)), (mem x (infix_plpl l1 l2)) <-> ((mem x l1) \/
  (mem x l2)).

Axiom mem_decomp : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l:(@list
  a a_WT)), (mem x l) -> exists l1:(@list a a_WT), exists l2:(@list a a_WT),
  (l = (infix_plpl l1 (Cons x l2))).

Axiom to_list_append : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (l:Z) (m:Z) (u:Z), ((l <= m)%Z /\ (m <= u)%Z) -> ((to_list a1 l
  u) = (infix_plpl (to_list a1 l m) (to_list a1 m u))).

Axiom to_list_eq : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (a2:(@array a a_WT)) (l1:Z) (l2:Z) (u1:Z) (u2:Z) (len:Z),
  (forall (i:Z), ((0%Z <= i)%Z /\ (i < len)%Z) -> ((get a1
  (l1 + i)%Z) = (get a2 (l2 + i)%Z))) -> (((u1 - l1)%Z = len) ->
  (((u2 - l2)%Z = len) -> ((to_list a1 l1 u1) = (to_list a2 l2 u2)))).

Axiom to_list_frame : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (l:Z) (u:Z) (i:Z) (x:a), ((i < l)%Z \/ (u <= i)%Z) ->
  ((to_list (set a1 i x) l u) = (to_list a1 l u)).

(* Why3 assumption *)
Inductive t
  (a:Type) {a_WT:WhyType a} :=
  | mk_t : (@array a a_WT) -> Z -> Z -> (@list a a_WT) -> t a.
Axiom t_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (t a).
Existing Instance t_WhyType.
Implicit Arguments mk_t [[a] [a_WT]].

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a} (v:(@t a a_WT)): (@list
  a a_WT) := match v with
  | (mk_t x x1 x2 x3) => x3
  end.

(* Why3 assumption *)
Definition n {a:Type} {a_WT:WhyType a} (v:(@t a a_WT)): Z :=
  match v with
  | (mk_t x x1 x2 x3) => x2
  end.

(* Why3 assumption *)
Definition m {a:Type} {a_WT:WhyType a} (v:(@t a a_WT)): Z :=
  match v with
  | (mk_t x x1 x2 x3) => x1
  end.

(* Why3 assumption *)
Definition data {a:Type} {a_WT:WhyType a} (v:(@t a a_WT)): (@array a a_WT) :=
  match v with
  | (mk_t x x1 x2 x3) => x
  end.

Require Import Why3. Ltac ae := why3 "alt-ergo" timelimit 3.

(* Why3 goal *)
Theorem WP_parameter_enqueue : forall {a:Type} {a_WT:WhyType a},
  forall (x:a), forall (rho:(@list a a_WT)) (rho1:Z) (rho2:Z) (rho3:Z)
  (rho4:(@map.Map.map Z _ a a_WT)), (((((0%Z < rho3)%Z /\
  (((0%Z <= rho2)%Z /\ (rho2 <= rho1)%Z) /\ (rho1 <= rho3)%Z)) /\
  (rho = (to_list (mk_array rho3 rho4) rho2 rho1))) /\ (0%Z <= rho3)%Z) /\
  (rho1 < rho3)%Z) -> (((0%Z <= rho1)%Z /\ (rho1 < rho3)%Z) ->
  forall (o:(@map.Map.map Z _ a a_WT)), ((0%Z <= rho3)%Z /\
  (o = (map.Map.set rho4 rho1 x))) -> forall (rho5:Z),
  (rho5 = (rho1 + 1%Z)%Z) -> forall (rho6:(@list a a_WT)),
  (rho6 = (infix_plpl rho (Cons x (Nil :(@list a a_WT))))) ->
  (rho6 = (to_list (mk_array rho3 o) rho2 rho5))).
(* Why3 intros a a_WT x rho rho1 rho2 rho3 rho4
        ((((h1,((h2,h3),h4)),h5),h6),h7) (h8,h9) o (h10,h11) rho5 h12 rho6
        h13. *)
intros a a_WT x rho rho1 rho2 rho3 rho4 ((((h1,((h2,h3),h4)),h5),h6),h7)
(h8,h9) o (h10,h11) rho5 h12 rho6 h13.
subst rho6.
symmetry.
rename rho2 into m.
rename rho1 into n.
rewrite (to_list_append _ m n rho5).
apply f_equal2.
3: omega.

rewrite to_list_eq with (mk_array rho3 o) (mk_array rho3 rho4) m m n n (n-m)%Z.
ae.
ae.
ae.
ae.

rewrite to_list_cons.
apply f_equal2.
ae.

ae.
ae.


Qed.

