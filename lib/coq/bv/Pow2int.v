(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2017   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.

(* Why3 goal *)
Definition pow2: Z -> Z.
  exact (two_p).
Defined.

(* Why3 goal *)
Lemma Power_0 : ((pow2 0%Z) = 1%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma Power_s : forall (n:Z), (0%Z <= n)%Z ->
  ((pow2 (n + 1%Z)%Z) = (2%Z * (pow2 n))%Z).
  apply two_p_S.
Qed.

(* Why3 goal *)
Lemma Power_1 : ((pow2 1%Z) = 2%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma Power_sum : forall (n:Z) (m:Z), ((0%Z <= n)%Z /\ (0%Z <= m)%Z) ->
  ((pow2 (n + m)%Z) = ((pow2 n) * (pow2 m))%Z).
  unfold pow2.
  intros n m [H1 H2].
  apply two_p_is_exp; easy.
Qed.

(* Why3 goal *)
Lemma pow2pos : forall (i:Z), (0%Z <= i)%Z -> (0%Z < (pow2 i))%Z.
  intros i h1.
  Require Import Zorder.
  apply Zgt_lt.
  auto using two_p_gt_ZERO.
Qed.

(* Why3 goal *)
Lemma pow2_0 : ((pow2 0%Z) = 1%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_1 : ((pow2 1%Z) = 2%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_2 : ((pow2 2%Z) = 4%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_3 : ((pow2 3%Z) = 8%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_4 : ((pow2 4%Z) = 16%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_5 : ((pow2 5%Z) = 32%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_6 : ((pow2 6%Z) = 64%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_7 : ((pow2 7%Z) = 128%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_8 : ((pow2 8%Z) = 256%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_9 : ((pow2 9%Z) = 512%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_10 : ((pow2 10%Z) = 1024%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_11 : ((pow2 11%Z) = 2048%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_12 : ((pow2 12%Z) = 4096%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_13 : ((pow2 13%Z) = 8192%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_14 : ((pow2 14%Z) = 16384%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_15 : ((pow2 15%Z) = 32768%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_16 : ((pow2 16%Z) = 65536%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_17 : ((pow2 17%Z) = 131072%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_18 : ((pow2 18%Z) = 262144%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_19 : ((pow2 19%Z) = 524288%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_20 : ((pow2 20%Z) = 1048576%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_21 : ((pow2 21%Z) = 2097152%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_22 : ((pow2 22%Z) = 4194304%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_23 : ((pow2 23%Z) = 8388608%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_24 : ((pow2 24%Z) = 16777216%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_25 : ((pow2 25%Z) = 33554432%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_26 : ((pow2 26%Z) = 67108864%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_27 : ((pow2 27%Z) = 134217728%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_28 : ((pow2 28%Z) = 268435456%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_29 : ((pow2 29%Z) = 536870912%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_30 : ((pow2 30%Z) = 1073741824%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_31 : ((pow2 31%Z) = 2147483648%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_32 : ((pow2 32%Z) = 4294967296%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_33 : ((pow2 33%Z) = 8589934592%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_34 : ((pow2 34%Z) = 17179869184%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_35 : ((pow2 35%Z) = 34359738368%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_36 : ((pow2 36%Z) = 68719476736%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_37 : ((pow2 37%Z) = 137438953472%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_38 : ((pow2 38%Z) = 274877906944%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_39 : ((pow2 39%Z) = 549755813888%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_40 : ((pow2 40%Z) = 1099511627776%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_41 : ((pow2 41%Z) = 2199023255552%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_42 : ((pow2 42%Z) = 4398046511104%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_43 : ((pow2 43%Z) = 8796093022208%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_44 : ((pow2 44%Z) = 17592186044416%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_45 : ((pow2 45%Z) = 35184372088832%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_46 : ((pow2 46%Z) = 70368744177664%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_47 : ((pow2 47%Z) = 140737488355328%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_48 : ((pow2 48%Z) = 281474976710656%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_49 : ((pow2 49%Z) = 562949953421312%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_50 : ((pow2 50%Z) = 1125899906842624%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_51 : ((pow2 51%Z) = 2251799813685248%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_52 : ((pow2 52%Z) = 4503599627370496%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_53 : ((pow2 53%Z) = 9007199254740992%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_54 : ((pow2 54%Z) = 18014398509481984%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_55 : ((pow2 55%Z) = 36028797018963968%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_56 : ((pow2 56%Z) = 72057594037927936%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_57 : ((pow2 57%Z) = 144115188075855872%Z).
  easy.

Qed.

(* Why3 goal *)
Lemma pow2_58 : ((pow2 58%Z) = 288230376151711744%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_59 : ((pow2 59%Z) = 576460752303423488%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_60 : ((pow2 60%Z) = 1152921504606846976%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_61 : ((pow2 61%Z) = 2305843009213693952%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_62 : ((pow2 62%Z) = 4611686018427387904%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_63 : ((pow2 63%Z) = 9223372036854775808%Z).
  easy.
Qed.

(* Why3 goal *)
Lemma pow2_64 : ((pow2 64%Z) = 18446744073709551616%Z).
  easy.
Qed.

