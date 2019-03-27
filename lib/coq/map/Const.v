(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require map.Map.

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require map.Map.

(* Why3 goal *)
Definition const: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  b -> (map.Map.map a b).
intros a a_WT b b_WT v.
constructor; intros i.
exact v.
Defined.

(* Why3 goal *)
Lemma Const : forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  forall (b1:b) (a1:a), ((map.Map.get (const b1: (map.Map.map a b))
  a1) = b1).
intros a a_WT b b_WT b1 a1.
unfold const.
auto.
Qed.

