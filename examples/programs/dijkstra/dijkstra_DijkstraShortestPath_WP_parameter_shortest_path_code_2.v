(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.

(* Why3 assumption *)
Definition unit  := unit.

(* Why3 assumption *)
Inductive ref (a:Type) {a_WT:WhyType a} :=
  | mk_ref : a -> ref a.
Axiom ref_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (ref a).
Existing Instance ref_WhyType.
Implicit Arguments mk_ref [[a] [a_WT]].

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a}(v:(ref a)): a :=
  match v with
  | (mk_ref x) => x
  end.

Axiom set : forall (a:Type) {a_WT:WhyType a}, Type.
Parameter set_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (set a).
Existing Instance set_WhyType.

Parameter mem: forall {a:Type} {a_WT:WhyType a}, a -> (set a) -> Prop.

(* Why3 assumption *)
Definition infix_eqeq {a:Type} {a_WT:WhyType a}(s1:(set a)) (s2:(set
  a)): Prop := forall (x:a), (mem x s1) <-> (mem x s2).

Axiom extensionality : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), (infix_eqeq s1 s2) -> (s1 = s2).

(* Why3 assumption *)
Definition subset {a:Type} {a_WT:WhyType a}(s1:(set a)) (s2:(set a)): Prop :=
  forall (x:a), (mem x s1) -> (mem x s2).

Axiom subset_refl : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  (subset s s).

Axiom subset_trans : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)) (s3:(set a)), (subset s1 s2) -> ((subset s2 s3) -> (subset s1
  s3)).

Parameter empty: forall {a:Type} {a_WT:WhyType a}, (set a).

(* Why3 assumption *)
Definition is_empty {a:Type} {a_WT:WhyType a}(s:(set a)): Prop :=
  forall (x:a), ~ (mem x s).

Axiom empty_def1 : forall {a:Type} {a_WT:WhyType a}, (is_empty (empty :(set
  a))).

Axiom mem_empty : forall {a:Type} {a_WT:WhyType a}, forall (x:a), ~ (mem x
  (empty :(set a))).

Parameter add: forall {a:Type} {a_WT:WhyType a}, a -> (set a) -> (set a).

Axiom add_def1 : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (y:a),
  forall (s:(set a)), (mem x (add y s)) <-> ((x = y) \/ (mem x s)).

Parameter remove: forall {a:Type} {a_WT:WhyType a}, a -> (set a) -> (set a).

Axiom remove_def1 : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (y:a)
  (s:(set a)), (mem x (remove y s)) <-> ((~ (x = y)) /\ (mem x s)).

Axiom subset_remove : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (s:(set
  a)), (subset (remove x s) s).

Parameter union: forall {a:Type} {a_WT:WhyType a}, (set a) -> (set a) -> (set
  a).

Axiom union_def1 : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)) (x:a), (mem x (union s1 s2)) <-> ((mem x s1) \/ (mem x s2)).

Parameter inter: forall {a:Type} {a_WT:WhyType a}, (set a) -> (set a) -> (set
  a).

Axiom inter_def1 : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)) (x:a), (mem x (inter s1 s2)) <-> ((mem x s1) /\ (mem x s2)).

Parameter diff: forall {a:Type} {a_WT:WhyType a}, (set a) -> (set a) -> (set
  a).

Axiom diff_def1 : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)) (x:a), (mem x (diff s1 s2)) <-> ((mem x s1) /\ ~ (mem x s2)).

Axiom subset_diff : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), (subset (diff s1 s2) s1).

Parameter choose: forall {a:Type} {a_WT:WhyType a}, (set a) -> a.

Axiom choose_def : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  (~ (is_empty s)) -> (mem (choose s) s).

Parameter cardinal: forall {a:Type} {a_WT:WhyType a}, (set a) -> Z.

Axiom cardinal_nonneg : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  (0%Z <= (cardinal s))%Z.

Axiom cardinal_empty : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  ((cardinal s) = 0%Z) <-> (is_empty s).

Axiom cardinal_add : forall {a:Type} {a_WT:WhyType a}, forall (x:a),
  forall (s:(set a)), (~ (mem x s)) -> ((cardinal (add x
  s)) = (1%Z + (cardinal s))%Z).

Axiom cardinal_remove : forall {a:Type} {a_WT:WhyType a}, forall (x:a),
  forall (s:(set a)), (mem x s) -> ((cardinal s) = (1%Z + (cardinal (remove x
  s)))%Z).

Axiom cardinal_subset : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), (subset s1 s2) -> ((cardinal s1) <= (cardinal s2))%Z.

Axiom cardinal1 : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  ((cardinal s) = 1%Z) -> forall (x:a), (mem x s) -> (x = (choose s)).

Axiom map : forall (a:Type) {a_WT:WhyType a} (b:Type) {b_WT:WhyType b}, Type.
Parameter map_WhyType : forall (a:Type) {a_WT:WhyType a}
  (b:Type) {b_WT:WhyType b}, WhyType (map a b).
Existing Instance map_WhyType.

Parameter get: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  (map a b) -> a -> b.

Parameter set1: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  (map a b) -> a -> b -> (map a b).

Axiom Select_eq : forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  forall (m:(map a b)), forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) ->
  ((get (set1 m a1 b1) a2) = b1).

Axiom Select_neq : forall {a:Type} {a_WT:WhyType a}
  {b:Type} {b_WT:WhyType b}, forall (m:(map a b)), forall (a1:a) (a2:a),
  forall (b1:b), (~ (a1 = a2)) -> ((get (set1 m a1 b1) a2) = (get m a2)).

Parameter const: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  b -> (map a b).

Axiom Const : forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  forall (b1:b) (a1:a), ((get (const b1:(map a b)) a1) = b1).

Axiom vertex : Type.
Parameter vertex_WhyType : WhyType vertex.
Existing Instance vertex_WhyType.

Parameter v: (set vertex).

Parameter g_succ: vertex -> (set vertex).

Axiom G_succ_sound : forall (x:vertex), (subset (g_succ x) v).

Parameter weight: vertex -> vertex -> Z.

Axiom Weight_nonneg : forall (x:vertex) (y:vertex), (0%Z <= (weight x y))%Z.

(* Why3 assumption *)
Definition min(m:vertex) (q:(set vertex)) (d:(map vertex Z)): Prop := (mem m
  q) /\ forall (x:vertex), (mem x q) -> ((get d m) <= (get d x))%Z.

(* Why3 assumption *)
Inductive path : vertex -> vertex -> Z -> Prop :=
  | Path_nil : forall (x:vertex), (path x x 0%Z)
  | Path_cons : forall (x:vertex) (y:vertex) (z:vertex), forall (d:Z),
      (path x y d) -> ((mem z (g_succ y)) -> (path x z (d + (weight y z))%Z)).

Axiom Length_nonneg : forall (x:vertex) (y:vertex), forall (d:Z), (path x y
  d) -> (0%Z <= d)%Z.

(* Why3 assumption *)
Definition shortest_path(x:vertex) (y:vertex) (d:Z): Prop := (path x y d) /\
  forall (d':Z), (path x y d') -> (d <= d')%Z.

Axiom Path_inversion : forall (src:vertex) (v1:vertex), forall (d:Z),
  (path src v1 d) -> (((v1 = src) /\ (d = 0%Z)) \/ exists v':vertex,
  (path src v' (d - (weight v' v1))%Z) /\ (mem v1 (g_succ v'))).

Axiom Path_shortest_path : forall (src:vertex) (v1:vertex), forall (d:Z),
  (path src v1 d) -> exists d':Z, (shortest_path src v1 d') /\ (d' <= d)%Z.

Axiom Main_lemma : forall (src:vertex) (v1:vertex), forall (d:Z), (path src
  v1 d) -> ((~ (shortest_path src v1 d)) -> (((v1 = src) /\ (0%Z < d)%Z) \/
  exists v':vertex, exists d':Z, (shortest_path src v' d') /\ ((mem v1
  (g_succ v')) /\ ((d' + (weight v' v1))%Z < d)%Z))).

Axiom Completeness_lemma : forall (s:(set vertex)), (forall (v1:vertex),
  (mem v1 s) -> forall (w:vertex), (mem w (g_succ v1)) -> (mem w s)) ->
  forall (src:vertex), (mem src s) -> forall (dst:vertex), forall (d:Z),
  (path src dst d) -> (mem dst s).

(* Why3 assumption *)
Definition inv_src(src:vertex) (s:(set vertex)) (q:(set vertex)): Prop :=
  (mem src s) \/ (mem src q).

(* Why3 assumption *)
Definition inv(src:vertex) (s:(set vertex)) (q:(set vertex)) (d:(map vertex
  Z)): Prop := (inv_src src s q) /\ (((get d src) = 0%Z) /\ ((subset s v) /\
  ((subset q v) /\ ((forall (v1:vertex), (mem v1 q) -> ~ (mem v1 s)) /\
  ((forall (v1:vertex), (mem v1 s) -> (shortest_path src v1 (get d v1))) /\
  forall (v1:vertex), (mem v1 q) -> (path src v1 (get d v1))))))).

(* Why3 assumption *)
Definition inv_succ(src:vertex) (s:(set vertex)) (q:(set vertex)) (d:(map
  vertex Z)): Prop := forall (x:vertex), (mem x s) -> forall (y:vertex),
  (mem y (g_succ x)) -> (((mem y s) \/ (mem y q)) /\ ((get d y) <= ((get d
  x) + (weight x y))%Z)%Z).

(* Why3 assumption *)
Definition inv_succ2(src:vertex) (s:(set vertex)) (q:(set vertex)) (d:(map
  vertex Z)) (u:vertex) (su:(set vertex)): Prop := forall (x:vertex), (mem x
  s) -> forall (y:vertex), (mem y (g_succ x)) -> (((~ (x = u)) \/ ((x = u) /\
  ~ (mem y su))) -> (((mem y s) \/ (mem y q)) /\ ((get d y) <= ((get d
  x) + (weight x y))%Z)%Z)).

Require Import Why3. Ltac ae := why3 "alt-ergo" timelimit 3.
Ltac z := why3 "z3" timelimit 3.

(* Why3 goal *)
Theorem WP_parameter_shortest_path_code : forall (src:vertex) (dst:vertex),
  forall (d:(map vertex Z)), ((mem src v) /\ (mem dst v)) -> forall (q:(set
  vertex)) (d1:(map vertex Z)) (visited:(set vertex)), (((forall (x:vertex),
  ~ (mem x visited)) /\ (q = (add src (empty :(set vertex))))) /\
  (d1 = (set1 d src 0%Z))) -> forall (q1:(set vertex)) (d2:(map vertex Z))
  (visited1:(set vertex)), ((((inv_src src visited1 q1) /\ (((get d2
  src) = 0%Z) /\ ((subset visited1 v) /\ ((subset q1 v) /\
  ((forall (v1:vertex), (mem v1 q1) -> ~ (mem v1 visited1)) /\
  ((forall (v1:vertex), (mem v1 visited1) -> (shortest_path src v1 (get d2
  v1))) /\ forall (v1:vertex), (mem v1 q1) -> (path src v1 (get d2
  v1)))))))) /\ forall (x:vertex), (mem x visited1) -> forall (y:vertex),
  (mem y (g_succ x)) -> (((mem y visited1) \/ (mem y q1)) /\ ((get d2
  y) <= ((get d2 x) + (weight x y))%Z)%Z)) /\ forall (m:vertex), ((mem m
  q1) /\ forall (x:vertex), (mem x q1) -> ((get d2 m) <= (get d2 x))%Z) ->
  forall (x:vertex), forall (dx:Z), (path src x dx) -> ((dx < (get d2
  m))%Z -> (mem x visited1))) -> forall (o:bool), ((o = true) <->
  forall (x:vertex), ~ (mem x q1)) -> ((~ (o = true)) ->
  ((~ forall (x:vertex), ~ (mem x q1)) -> forall (q2:(set vertex)),
  forall (u:vertex), (((mem u q1) /\ forall (x:vertex), (mem x q1) ->
  ((get d2 u) <= (get d2 x))%Z) /\ (q2 = (remove u q1))) -> (((path src u
  (get d2 u)) /\ forall (d':Z), (path src u d') -> ((get d2 u) <= d')%Z) ->
  forall (visited2:(set vertex)), (visited2 = (add u visited1)) ->
  forall (su:(set vertex)) (q3:(set vertex)) (d3:(map vertex Z)),
  (((forall (x:vertex), (mem x su) -> (mem x (g_succ u))) /\ ((inv_src src
  visited2 q3) /\ (((get d3 src) = 0%Z) /\ ((subset visited2 v) /\
  ((subset q3 v) /\ ((forall (v1:vertex), (mem v1 q3) -> ~ (mem v1
  visited2)) /\ ((forall (v1:vertex), (mem v1 visited2) -> (shortest_path src
  v1 (get d3 v1))) /\ forall (v1:vertex), (mem v1 q3) -> (path src v1 (get d3
  v1))))))))) /\ forall (x:vertex), (mem x visited2) -> forall (y:vertex),
  (mem y (g_succ x)) -> (((~ (x = u)) \/ ((x = u) /\ ~ (mem y su))) ->
  (((mem y visited2) \/ (mem y q3)) /\ ((get d3 y) <= ((get d3 x) + (weight x
  y))%Z)%Z))) -> forall (result:bool), ((result = true) <->
  ~ forall (x:vertex), ~ (mem x su)) -> ((result = true) ->
  ((~ forall (x:vertex), ~ (mem x su)) -> forall (su1:(set vertex)),
  forall (v1:vertex), ((mem v1 su) /\ (su1 = (remove v1 su))) ->
  forall (q4:(set vertex)) (d4:(map vertex Z)), (((mem v1 visited2) /\
  ((q4 = q3) /\ (d4 = d3))) \/ (((mem v1 q4) /\ (((get d4 v1) <= ((get d4
  u) + (weight u v1))%Z)%Z /\ ((q4 = q3) /\ (d4 = d3)))) \/ (((mem v1 q4) /\
  ((((get d3 u) + (weight u v1))%Z < (get d3 v1))%Z /\ ((q4 = q3) /\
  (d4 = (set1 d3 v1 ((get d3 u) + (weight u v1))%Z))))) \/ ((~ (mem v1
  visited2)) /\ ((~ (mem v1 q3)) /\ ((q4 = (add v1 q3)) /\ (d4 = (set1 d3 v1
  ((get d3 u) + (weight u v1))%Z)))))))) -> ((((get d4 v1) < ((get d4
  u) + (weight u v1))%Z)%Z \/ ((get d4 v1) = ((get d4 u) + (weight u
  v1))%Z)) -> forall (v2:vertex), (mem v2 q4) -> (path src v2 (get d4
  v2)))))))).
(*
intros src dst d (h1,h2) q d1 visited ((h3,h4),h5) q1 d2 visited1
((h6,(h7,(h8,(h9,(h10,(h11,h12)))))),h14) o h15 h16 h17 q2 u
((h18,h19),h20) (h21,h22) visited2 h23 su q3 d3
((h24,(h25,(h26,(h27,(h28,(h29,(h30,(h31,h32)))))))),h33) result h34 h35 h36
su1 v1 (h37,h38) q4 d4 h39 v2 h40.
*)
intuition; try ae.

assert (case: (v2 = v1 \/ v2 <> v1)) by ae. destruct case.
subst v2 d4; rewrite Select_eq.
apply Path_cons.
z.
 ae.
trivial.
subst d4; rewrite Select_neq.
ae.
ae.

assert (case: (v2 = v1 \/ v2 <> v1)) by ae. destruct case.
subst v2 d4; rewrite Select_eq.
apply Path_cons.
z.
ae.
trivial.
ae.
Qed.


