Set Nested Proofs Allowed.

From LF Require Export Basics.

Theorem plus_n_0 : forall n: nat, n = n + 0.
Proof.
  intros n. induction n as [| n' IHn' ].
  - (* n = 0 *) reflexivity.
  - (* n = S n' *) simpl. rewrite <- IHn'. reflexivity.
Qed.


Theorem minus_diag: forall n, minus n n = 0.
Proof.
  intros n.
  induction n as [|n' IHn'].
  - (* n = 0 *) simpl. reflexivity.
  - (* n = S n' *) simpl. rewrite <- IHn'. reflexivity.
Qed.

Theorem mult_0_r: forall n: nat, n * 0 = 0.
Proof.
  intros n.
  induction n as [|n' IHn'].
  - simpl. reflexivity.
  - simpl. rewrite -> IHn'. reflexivity.
Qed.

Theorem plus_n_Sm: forall n m: nat, S (n + m) = n + (S m).
Proof.
  intros n m.
  destruct m eqn: Em.
  - induction n as [|n' IHn'].
    + simpl. reflexivity.
    + simpl. rewrite <- IHn'. reflexivity.
  - induction n as [|n' IHn'].
    + simpl. reflexivity.
    + simpl. rewrite <- IHn'. reflexivity.
Qed.

Theorem plus_comm: forall n m: nat, n + m = m + n.
Proof.
  intros n m.
  destruct m eqn: Em.
  - rewrite <- plus_n_0. rewrite -> plus_O_n. reflexivity.
  - induction n as [|n' IHn'].
    + rewrite <- plus_n_0. rewrite -> plus_O_n. reflexivity.
    + simpl. rewrite -> IHn'. rewrite plus_n_Sm. simpl. reflexivity.
Qed.

Theorem plus_assoc: forall n m p : nat, n + (m + p) = (n + m) + p.
Proof.
  intros n m p.
  destruct m eqn: Em.
  - simpl. rewrite <- plus_n_0. reflexivity.
  - induction n as [|n' IHn'].
    + simpl. reflexivity.
    + simpl. rewrite <- IHn'. simpl. reflexivity.
Qed.
      

Fixpoint double (n: nat) :=
  match n with
  | O => O
  | S n' => S ( S ( double n'))
  end.

Lemma double_plus : forall n, double n = n + n.
Proof.
  intros n.
  induction n as [|n' IHn'].
  - simpl. reflexivity.
  - simpl. rewrite -> IHn'. rewrite -> plus_n_Sm. reflexivity.
Qed.

Theorem evenb_S: forall n: nat, evenb (S n) = negb (evenb n).
Proof.
  intros n.
  induction n as [|n' IHn'].
  - simpl. reflexivity.
  - rewrite -> IHn'. simpl. rewrite -> negation_fn_applied_twice. simpl. reflexivity.
    induction x.
    + simpl. reflexivity.
    + simpl. reflexivity.
Qed.

(**
- destruct 策略一般用于证明有限种情况，除非结合反证。
- induction 则可用于证明无限的情况。
 *)

Theorem mult_0_plus': forall n m: nat, (0 + n) * m = n * m.
Proof.
  intros n m.
  assert (H: 0 + n = n).
  { reflexivity. }
  rewrite -> H.
  reflexivity.
Qed.


Theorem plus_rearrange : forall n m p q: nat, (n + m) + (p + q) = (m + n) + (p + q).
Proof.
  intros n m p q.
  assert (n + m = m + n) as H.
  { rewrite -> plus_comm. reflexivity. }
  rewrite -> H.
  reflexivity.
Qed.

(**

定理：对于任何自然数 n 和 m，有(n + m) = (m + n).
证明：对 n 使用归纳法：
  - 首先，设 n = 0, 我们必须证明 0 + m = m + 0，带入 m 到 0 + m = m 和 m + 0 = m 得 m = m 显然成立。
  - 其次，设 n = S n', 其中 (n' + m) = (m + n')。
    我们必须证明 (S n' + m) = (m + S n')。
    根据 + 的定义，该式可以写成 S (n' + m) = (m + Sn') 带入 plus_m_Sm 定理可得：
    S (n' + m) = S (m + n') 带入归纳条件得 S (m + n') = S (m + n') 显然成立。
证毕。 
 *)

(**
定理：对于任何自然数 n，均有 true = n =? n
证明：对 n 使用自然归纳法：
  - 首先，设 n = 0，则必须证明 0 =? 0 = true，该结论由 =? 的定义可得。
  - 其次，设 n = S n'，其中 (n' =? n')。
    我们必须证明 (S n') =? (S n') = true. 该结论由 =? 的定义可得。
证毕。
 *)

Theorem plus_swap: forall n m p : nat, n + (m + p) = m + (n + p).
Proof.
  intros n m p.
  assert (n + (m + p) = (n + m) + p) as H.
  { rewrite -> plus_assoc. reflexivity. }
  rewrite -> H.
  assert (n + m = m + n) as G.
  { rewrite -> plus_comm. reflexivity. }
  rewrite -> G.
  rewrite <- plus_assoc.
  reflexivity.
Qed.

Theorem mult_comm : forall m n: nat, m * n = n * m.
Proof.
  intros m n.
  induction m as [|m' IHm'].
  - simpl. rewrite -> mult_0_r. reflexivity.
  - simpl.
    assert (n + m' * n = n * (S m')) as H.
    {
      rewrite -> IHm'. simpl. rewrite <- mult_n_Sm. rewrite -> plus_comm. reflexivity.
    }
    rewrite -> H.
    reflexivity.
Qed.


Check leb.

Theorem leb_refl: forall n: nat, true = (n <=? n).
Proof.
  intros n.
  induction n as [|n' IHn'].
  - simpl. reflexivity.
  - simpl. rewrite -> IHn'. reflexivity.
Qed.


Theorem zero_nbeq_S: forall n: nat, 0 =? (S n) = false.
Proof.
  intros n.
  simpl. reflexivity.
Qed.

Theorem andb_false_r: forall b: bool, andb b false = false.
Proof.
  intros [].
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.

Theorem plus_ble_compat_1: forall n m p : nat,
    n <=? m = true -> (p + n) <=? (p + m) = true.
Proof.
  intros n m p G.
  induction p as [|p' IHp'].
  - simpl.
    rewrite -> G.
    reflexivity.
  - simpl.
    rewrite -> IHp'.
    reflexivity.
Qed.

Theorem a113_spec: forall b c: bool, orb (andb b c) (orb (negb b) (negb c)) = true.
Proof.
  simpl.
  intros [] [].
  - simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.

Theorem mult_plus_distr_r: forall n m p : nat, (n + m) * p = (n * p) + (m * p).
Proof.
  intros n m p.
  induction p as [|p' IHp'].
  - rewrite -> mult_0_r. rewrite -> mult_comm. simpl. rewrite -> mult_0_r. reflexivity.
  - simpl.
    rewrite <- mult_n_Sm. rewrite <- mult_n_Sm; rewrite <- mult_n_Sm.
    rewrite <- plus_assoc.
    assert (n * p' + (n + (m * p' + m)) = n * p' + m * p' + m + n) as G.
    {
      assert (n + (m * p' + m) = m * p' + m + n) as E.
      { rewrite -> plus_comm. reflexivity. }
      rewrite -> E.
      rewrite -> plus_assoc.
      assert (n * p' + (m * p' + m) = n * p' + m * p' + m) as F.
      { rewrite -> plus_assoc. reflexivity. }
      rewrite -> F.
      reflexivity.
    }
    rewrite -> G.
    rewrite <- IHp'.
    rewrite <- plus_assoc.
    assert (n + m = m + n) as J.
    { rewrite -> plus_comm. reflexivity. }
    rewrite -> J.
    reflexivity.
Qed.


Theorem mult_assoc: forall n m p: nat, n * (m * p) = (n * m) * p.
Proof.
  intros n m p.
  induction m as [|m' IHm'].
  - simpl. rewrite -> mult_0_r. simpl. reflexivity.
  - simpl.
    rewrite -> mult_comm.
    rewrite <- mult_n_Sm.
    rewrite -> mult_plus_distr_r. rewrite -> mult_plus_distr_r.
    rewrite -> plus_comm.
    rewrite <- IHm'.
    assert (m' * p * n = n * (m' * p)) as G.
    { rewrite -> mult_comm. reflexivity. }
    rewrite <- G.
    assert (n * p = p * n) as H.
    { rewrite -> mult_comm. reflexivity. }
    rewrite -> H.
    reflexivity.
Qed.


Theorem plus_swap': forall n m p: nat,
    n + (m + p) = m + (n + p).
Proof.
  intros n m p.
  rewrite -> plus_comm.
  replace (n + p) with (p + n).
  rewrite -> plus_assoc. reflexivity.
  rewrite -> plus_comm. reflexivity.
Qed.

Theorem bin_to_nat_pres_incr: forall b : bin, S (bin_to_nat b) = bin_to_nat( incr b ).
Proof.
  intros b.
  induction b.
  - simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. rewrite <- IHb. simpl.
    replace (bin_to_nat b + S (bin_to_nat b)) with (S(bin_to_nat b + bin_to_nat b)).
    simpl. reflexivity.
    rewrite <- plus_n_Sm.
    reflexivity.
Qed.

Fixpoint nat_to_bin (n: nat) : bin :=
  match n with
  | O => Z
  | S n' => incr (nat_to_bin(n'))
  end.

Theorem plus_1 : forall n m : nat,
    n = m -> S n = S m.
Proof.
  intros n m H. induction n.
  - rewrite <- H. reflexivity.
  - rewrite -> H. reflexivity.
Qed.

Theorem nat_bin_nat: forall n, bin_to_nat (nat_to_bin n) = n.
Proof.
  intros n.
  induction n.
  - simpl. reflexivity.
  - simpl. apply plus_1 in IHn. rewrite bin_to_nat_pres_incr in IHn. apply IHn. Qed.

