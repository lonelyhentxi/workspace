Set Nested Proofs Allowed.
From LF Require Export Induction.
Module NatList.

  Inductive natprod : Type :=
  | pair (n₁ n₂: nat).

  Check (pair 3 5): natprod.

  Definition fst (p: natprod): nat :=
    match p with
    | pair x y => x
    end.

  Definition snd (p: natprod): nat :=
    match p with
    | pair x y => y
    end.

  Compute (fst (pair 3 5)).

  Notation "( x , y )" := (pair x y).

  Compute (fst (3, 5)).

  Definition fst' (p: natprod): nat :=
    match p with
    | (x, y) => x
    end.

  Definition snd' (p: natprod): nat :=
    match p with
    | (x, y) => y
    end.

  Definition swap_pair (p: natprod) : natprod :=
    match p with
    | (x, y) => (y, x)
    end.

  Fixpoint minus'' (n m : nat) : nat :=
    match n, m with
    | O, _ => O
    | S _ , O => n
    | S n' , S m' => minus n' m'
    end.

  (* Can not match on a pair with multiple patterns *)
  (* Can not match on multiple values with pair patterns *)

  Theorem surjective_pairing' : forall (n m: nat),
      (n, m) = (fst (n, m), snd (n, m)).
  Proof.
    reflexivity.
  Qed.

  Theorem surjective_pairing : forall (p : natprod),
      p = (fst p, snd p).
  Proof.
    intros p.
    destruct p as [n m].
    simpl.
    reflexivity.
  Qed.

  Theorem snd_fst_is_swap : forall (p: natprod),
      (snd p, fst p) = swap_pair p.
  Proof.
    intros p.
    destruct p as [m n].
    simpl.
    reflexivity.
  Qed.

  Theorem fst_swap_is_snd : forall (p: natprod),
      fst (swap_pair p) = snd p.
  Proof.
    intros p.
    destruct p as [m n].
    simpl.
    reflexivity.
  Qed.


  Inductive natlist : Type :=
  | nil
  | cons (n : nat) (l : natlist).

  Definition mylist := cons 1 (cons 2 (cons 3 nil)).

  Notation "x :: l" := (cons x l ) (at level 60, right associativity).
  Notation "[ ]" := nil.
  Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

  Definition mylist1 := 1 :: (2 :: (3 :: nil)).
  Definition mylist2 := 1 :: 2 :: 3 :: nil.
  Definition mylist3 := [1; 2; 3].

  Fixpoint repeat (n count: nat): natlist :=
    match count with
    | 0 => nil
    | S c' => n :: (repeat n c')
    end.

  Fixpoint length (l: natlist): nat :=
    match l with
    | nil => O
    | h :: t => S (length t)
    end.

  Fixpoint app (l₁ l₂ : natlist): natlist :=
    match l₁ with
    | nil => l₂
    | h :: t => h :: (app t l₂)
    end.

  Notation "x ++ y" := (app x y) (right associativity, at level 60).

  Example text_app1: [1; 2; 3] ++ [4; 5] = [1; 2; 3; 4; 5].
  Proof. reflexivity. Qed.

  Example text_app2: nil ++ [4; 5] = [4; 5].
  Proof. reflexivity. Qed.

  Example test_app3: [1; 2; 3] ++ nil = [1; 2; 3].
  Proof. reflexivity. Qed.

  Definition hd (default: nat) (l: natlist) : nat :=
    match l with
    | nil => default
    | h :: t => h
    end.

  Definition tl (l: natlist): natlist :=
    match l with
    | nil => nil
    | h :: t => t
    end.

  Example test_hd₁ : hd 0 [1;2;3] = 1.
  Proof. reflexivity. Qed.

  Example test_hd₂ : hd 0 [] = 0.
  Proof. reflexivity. Qed.

  Example test_tl: tl [1;2;3] = [2; 3].
  Proof. reflexivity. Qed.

  Fixpoint nonzeros (l: natlist) : natlist :=
    match l with
    | nil => nil
    | O :: t => nonzeros t
    | h :: t => h :: ( nonzeros t )
    end.

  Example test_nonzeros: nonzeros [0;1;0;2;3;0;0] = [1;2;3].
  Proof. simpl. reflexivity. Qed.

  Fixpoint oddmembers (l: natlist) : natlist :=
    match l with
    | nil => nil
    | h :: t => match ( evenb h ) with
                | true => oddmembers t
                | false => h :: (oddmembers t)
                end
    end.

  Example test_oddmembers:
    oddmembers [0;1;0;2;3;0;0] = [1;3].
  Proof. simpl. reflexivity. Qed.

  Definition countoddmembers (l: natlist) : nat := length (oddmembers l).

  Example test_countoddmembers1: countoddmembers [1;0;3;1;4;5] = 4.
  Proof. simpl. reflexivity. Qed.

  Example test_countoddmembers2: countoddmembers [0;2;4] = 0.
  Proof. simpl. reflexivity. Qed.

  Example test_countoddmembers3: countoddmembers nil = 0.
  Proof. simpl. reflexivity. Qed.

  Inductive natlistprod : Type :=
  | natlistpair ( l₁ l₂ : natlist ).
  
  Fixpoint alternate (l₁ l₂: natlist) : natlist :=
    match (natlistpair l₁ l₂) with
    | natlistpair nil l => l
    | natlistpair l nil => l
    | natlistpair (h₁ :: t₁) (h₂ :: t₂) => h₁ :: h₂ :: ( alternate t₁ t₂ )
    end.

  Example test_alternate1: alternate [1;2;3] [4;5;6] = [1;4;2;5;3;6].
  Proof. simpl. reflexivity. Qed.

  Example test_alternate2: alternate [1] [4;5;6] = [1;4;5;6].
  Proof. reflexivity. Qed.

  Example test_alternate3: alternate [1;2;3] [4] = [1;4;2;3].
  Proof. simpl. reflexivity. Qed.

  Example test_alternate4: alternate [] [20;30] = [20;30].
  Proof. simpl. reflexivity. Qed.                       

  Definition bag := natlist.
  
  Fixpoint filter_members (l: natlist) (pred: nat -> bool) : natlist :=
    match l with
    | nil => nil
    | h :: t => match (pred h) with
                | true => h :: (filter_members t pred)
                | _ => filter_members t pred
                end
    end.

  Fixpoint eqn' (v₁ v₂: nat): bool :=
    match v₁, v₂ with
    | O, O => true
    | O, _ => false
    | _, O => false
    | S n₁, S n₂ => eqn' n₁ n₂
    end.
  
  Fixpoint count (v: nat) (s: bag) : nat :=
    match s with
    | [] => 0
    | h :: t => ( if eqn' h v then 1 else 0) + (count v t)
    end.

  Example test_count1: count 1 [1;2;3;1;4;1] = 3.
  Proof. simpl. reflexivity. Qed.

  Example test_count2: count 6 [1;2;3;1;4;1] = 0.
  Proof. reflexivity. Qed.
  
  
    Definition sum : bag -> bag -> bag := app.

  Example test_sum1: count 1 (sum [1;2;3] [1;4;1]) = 3.
  Proof. reflexivity. Qed.

  Definition add (v: nat) (s: bag) : bag := v::s.

  Example test_add1: count 1 (add 1 [1;4;1]) = 3.
  Proof. reflexivity. Qed.

  Example test_add2: count 5 (add 1 [1;4;1]) = 0.
  Proof. reflexivity. Qed.

  Definition member (v: nat) (s: bag) : bool := negb ( eqn' O ( count v s )).

  Example test_member1: member 1 [1;4;1] = true.
  Proof. reflexivity. Qed.

  Example test_member2: member 2 [1;4;1] = false.
  Proof. reflexivity. Qed.

  Definition nen' (v w: nat) : bool := negb (eqn' v w). 

  Fixpoint remove_one (v:nat) (s:bag) : bag :=
    match s with
    | nil => nil
    | h :: t => match (eqn' h v) with
                | true => t
                | false => h :: ( remove_one v t )
                end
    end.

  Example test_remove_one1: count 5 (remove_one 5 [2;1;5;4;1]) = 0.
  Proof. simpl. reflexivity. Qed.

  Example test_remove_one2: count 5 (remove_one 5 [2;1;4;1]) = 0.
  Proof. reflexivity. Qed.

  Example test_remove_one3: count 4 (remove_one 5 [2;1;4;5;1;4]) = 2.
  Proof. simpl. reflexivity. Qed.

  Example test_remove_one4: count 5 (remove_one 5 [2;1;5;4;5;1;4]) = 1.
  Proof. reflexivity. Qed.

  Fixpoint remove_all (v:nat) (s:bag) : bag := filter_members s (nen' v).

  Example test_remove_all1: count 5 (remove_all 5 [2;1;5;4;1]) = 0.
  Proof. reflexivity. Qed.

  Example test_remove_all2: count 5 (remove_all 5 [2;1;4;1]) = 0.
  Proof. reflexivity. Qed.
  
  Example test_remove_all3: count 4 (remove_all 5 [2;1;4;5;1;4]) = 2.
  Proof. reflexivity. Qed.
  
  Example test_remove_all4: count 5 (remove_all 5 [2;1;5;4;5;1;4;5;1;4]) = 0.
  Proof. reflexivity. Qed.

  Fixpoint subset (s₁: bag) (s₂: bag) : bool :=
    match s₁ with
    | nil => true
    | h :: t => match count h s₂ with
                | S _ => subset t ( remove_one h s₂ )
                | O => false
                end
    end.

  Example test_subset1 : subset [1;2] [2;1;4;1] = true.
  Proof. reflexivity. Qed.

  Example test_subset2 : subset [1;2;2] [2;1;4;1] = false.
  Proof. reflexivity. Qed.

  
  Theorem bag_theorem : forall (x: nat) (s: bag), length ( add x s ) = S (length s ). 
   Proof.
     simpl.
     reflexivity.
   Qed.
   
  
   Theorem nil_app : forall l: natlist,  [] ++ l = l.
   Proof. reflexivity. Qed.

   Theorem tl_length_pred : forall l: natlist,
     pred (length l) = length (tl l).
   Proof.
     intros l. destruct l as [|n l'].
     - reflexivity.
     - reflexivity.
   Qed.

   Theorem app_assoc : forall l₁ l₂ l₃ : natlist,
       (l₁ ++ l₂) ++ l₃ = l₁ ++ (l₂ ++ l₃).
   Proof.
     intros l₁ l₂ l₃. induction l₁ as [|n l₁' IHl₁'].
     - simpl.  reflexivity.
     - simpl. rewrite -> IHl₁'. reflexivity.
   Qed.

   Fixpoint rev (l: natlist) : natlist :=
     match l with
     | nil => nil
     | h :: t => rev t ++ [h]
     end.

   Example test_rev1: rev [1;2;3] = [3;2;1].
   Proof. reflexivity. Qed.

   Example test_rev2: rev nil = nil.
   Proof. reflexivity. Qed.

   Theorem app_length : forall l₁ l₂ : natlist, length (l₁ ++ l₂ ) = (length l₁ ) + (length l₂).
   Proof.
     intros l₁ l₂.
     induction l₁ as [|n l₁' IHl₁'].
     - simpl. reflexivity.
     - simpl. rewrite -> IHl₁'. reflexivity.
   Qed.
   
   
   Theorem rev_length_firsttry : forall l : natlist, length (rev l) = length l.
   Proof.
     intros l.
     induction l as [|n l IHl'].
     - simpl. reflexivity.
     - simpl. rewrite -> app_length. rewrite -> IHl'. simpl. rewrite -> plus_comm. simpl. reflexivity.
   Qed.

   Theorem app_nil_r : forall l : natlist, l ++ [] = l.
   Proof.
     intros l. induction l as [|n l' IHl'].
     - simpl. reflexivity.
     - induction n as [|n' IHn'].
       + simpl. rewrite -> IHl'. reflexivity.
       + simpl. rewrite -> IHl'. reflexivity.
   Qed.

   Theorem rev_app_distr : forall l₁ l₂ : natlist, rev (l₁ ++ l₂ ) = rev l₂ ++ rev l₁.
   Proof.
     intros l₁ l₂. induction l₁ as [|n l₁' IHl₁'].
     - simpl. rewrite -> app_nil_r. reflexivity.
     - simpl. rewrite -> IHl₁'. rewrite -> app_assoc. reflexivity.
   Qed.


   Theorem rev_involutive : forall l : natlist, rev (rev l) = l.
   Proof.
     induction l as [|n l' IHl'].
     - reflexivity.
     - simpl. rewrite -> rev_app_distr. simpl. rewrite -> IHl'. reflexivity.
   Qed.

   Theorem app_assoc4 : forall l₁ l₂ l₃ l₄ : natlist,
       l₁ ++ (l₂ ++ (l₃ ++ l₄)) = ((l₁ ++ l₂) ++ l₃) ++ l₄.
   Proof.
     intros l₁ l₂ l₃ l₄.
     replace (l₂ ++ (l₃ ++ l₄)) with (l₂ ++ l₃ ++ l₄).
     replace (((l₁ ++ l₂) ++ l₃) ++ l₄) with ((l₁ ++ l₂) ++ l₃ ++ l₄).
     replace ((l₁ ++ l₂) ++ l₃ ++ l₄) with (l₁ ++ l₂ ++ l₃ ++ l₄).
     reflexivity.
     rewrite -> app_assoc. reflexivity.
     rewrite <- app_assoc. reflexivity.
     reflexivity.
   Qed.

   Lemma nonzeros_app : forall l₁ l₂ : natlist, nonzeros (l₁ ++ l₂) = (nonzeros l₁) ++ (nonzeros l₂).
   Proof.
     intros l₁ l₂.
     induction l₁ as [|n l₁' IHl₁'].
     - simpl. reflexivity.
     - destruct n eqn: En.
       + simpl. rewrite -> IHl₁'. reflexivity.
       + simpl. rewrite -> IHl₁'. reflexivity.
   Qed.

   Fixpoint eqblist (l₁ l₂ : natlist ) : bool :=
     match natlistpair l₁ l₂  with
     | natlistpair nil nil => true
     | natlistpair (h₁::t₁) (h₂::t₂) => match eqn' h₁ h₂ with
                                  | true => eqblist t₁ t₂
                                  | false => false
                                  end
     | _ => false
     end.

   Example test_eqblist1 : (eqblist nil nil) = true.
   Proof. reflexivity. Qed.

   Example test_eqblist2: eqblist [1;2;3] [1;2;3] = true.
   Proof. reflexivity. Qed.

   Example test_eqblist3: eqblist [1;2;3] [1;2;4] = false.
   Proof. reflexivity. Qed.

   Theorem eqn'_refl : forall n: nat,  true = eqn' n n.
   Proof.
     intros n.
     induction n as [|n IHn'].
     - reflexivity.
     - simpl. rewrite <- IHn'. reflexivity.
   Qed.
   
   
   Theorem eqblist_refl: forall l: natlist,
       true = eqblist l l.
   Proof.
     intros l.
     induction l as [|n l' IHl'].
     - reflexivity.
     - simpl. rewrite <- eqn'_refl. rewrite -> IHl'. reflexivity.
   Qed.

   Theorem count_member_nonzero : forall (s: bag), 1 <=? (count 1 (1 :: s)) = true.
   Proof.
     intros s. induction s as [|n s' IHs'].
     - simpl. reflexivity.
     - simpl. reflexivity.
   Qed.
   
   Theorem leb_n_Sn : forall n,
  n <=? (S n) = true.
   Proof.
     intros n. induction n as [| n' IHn'].
     - simpl. reflexivity.
     - simpl. rewrite IHn'. reflexivity.
   Qed.
   
   Theorem remove_does_not_increase_count : forall (s: bag), (count 0 (remove_one 0 s)) <=? (count 0 s) = true.
   Proof.
     intros s. induction s as [|n s' IHs'].
     - simpl. reflexivity.
     - destruct n eqn: En.
       + rewrite -> leb_n_Sn. reflexivity.
       + replace (count 0 (remove_one 0 (S n0 :: s'))) with (count 0 (remove_one 0 s')).
         replace (count 0 (S n0 :: s')) with (count 0 s').
         rewrite -> IHs'. reflexivity.
         simpl. reflexivity.
         replace (remove_one 0 ( S n0 :: s')) with ([S n0] ++ ( remove_one 0 s')).
         simpl. reflexivity.
         simpl. induction s' as [|n' s'' IHs''].
         * simpl. reflexivity.
         * simpl. reflexivity.
   Qed.

   Theorem bag_count_sum: forall (n: nat) (s1 s2: bag), count n (sum s1 s2) = count n s1 + count n s2.
   Proof.
     intros n s1 s2.
     induction s1 as [|h t IHh].
     - simpl. reflexivity.
     - destruct h as [|h'].
       + simpl. rewrite -> IHh. rewrite -> plus_assoc . reflexivity.
       + simpl. rewrite -> IHh. rewrite -> plus_assoc . reflexivity.
   Qed.

   Theorem rev_injective: forall (l1 l2 : natlist), rev l1 = rev l2 -> l1 = l2.
   Proof.
     intros l1 l2 H.
     rewrite <- rev_involutive with l1.
     rewrite <- rev_involutive with l2.
     rewrite -> H.
     reflexivity.
   Qed.

   Inductive natoption : Type :=
   | Some (n: nat)
   | None.


   Fixpoint nth_error (l: natlist) (n:nat) : natoption :=
     match l with
     | nil => None
     | a :: l' => match n with
                 | O => Some a
                 | S n' => nth_error l' n'
                  end
     end.

   Example test_nth_error1: nth_error [4;5;6;7] 0 = Some 4.
   Proof. reflexivity. Qed.

   Example test_nth_error2: nth_error [4;5;6;7] 3 = Some 7.
   Proof. reflexivity. Qed.

   Example test_nth_error3: nth_error [4;5;6;7] 9  = None.
   Proof. reflexivity. Qed.

   Fixpoint nth_error' (l: natlist) (n: nat) : natoption :=
     match l with
     | nil => None
     | a :: l' => if n =? 0 then Some a else nth_error' l' (pred n)
     end.


   Definition option_elim (d: nat) (o: natoption) :nat :=
     match o with
     | Some n' => n'
     | None => d
     end.

   
   Definition hd_error (l: natlist) : natoption :=
     match l with
     | nil => None
     | h :: t => Some h
     end.

   Example test_hd_error1 : hd_error [] = None.
   Proof. reflexivity. Qed.

   Example test_hd_error2: hd_error [1] = Some 1.
   Proof. reflexivity. Qed.

   Example test_hd_error3: hd_error [5;6] = Some 5.
   Proof. reflexivity. Qed.

   Theorem option_elim_hd : forall (l: natlist) (default: nat),
       hd default l = option_elim default (hd_error l).
   Proof.
     intros l default.
     destruct l eqn: El.
     - simpl. reflexivity.
     - simpl. reflexivity.
   Qed.


   Inductive id : Type :=
   | Id (n: nat).

   Definition eqb_id (x1 x2 : id) :=
     match x1, x2 with
     | Id n1, Id n2 => n1 =? n2
     end.

   Theorem eqb_id_refl : forall x, true = eqb_id x x.
     
         
End NatList.

