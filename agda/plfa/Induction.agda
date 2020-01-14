module plfa.Induction where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym) 
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_ ;_∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _^_)

-- @practice: exercise `operators` start
--
-- 1. _+_ _*_: Unit (Zero), Associativity, Commutativity
-- operator _*_ distributes over operator _+_ from the left and right.
--
-- 2. x operator of matrices: Unit (Unit matrix),
-- Associativity (Associativity of the Linear transformation)
-- No Commutativity

-- @practice: exercise `operators` end

_ : (3 + 4) + 5 ≡ 3 + (4 + 5)
_ =
  begin
    (3 + 4) + 5
  ≡⟨⟩
    7 + 5
  ≡⟨⟩
    12
  ≡⟨⟩
    3 + 9
  ≡⟨⟩
    3 + (4 + 5)
  ∎

+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p =
  begin
    (zero + n) + p
  ≡⟨⟩
    n + p
  ≡⟨⟩
    zero + (n + p)
  ∎
+-assoc (suc m) n p =
  begin
    (suc m + n) + p
  ≡⟨⟩
    suc (m + n) + p
  ≡⟨⟩
    suc ((m + n) + p)
  ≡⟨ cong suc (+-assoc m n p) ⟩
    suc (m + (n + p))
  ≡⟨⟩
    suc m + (n + p)
  ∎

-- examples start

+-assoc-2 : ∀ (n p : ℕ) → (2 + n) + p ≡ 2 + (n + p)
+-assoc-2 n p =
  begin
    (2 + n) + p
  ≡⟨⟩
    suc (1 + n) + p
  ≡⟨⟩
    suc ((1 + n) + p)
  ≡⟨ cong suc (+-assoc-1 n p) ⟩
    suc (1 + (n + p))
  ≡⟨⟩
    2 + (n + p)
  ∎
  where
  +-assoc-1 : ∀ (n p : ℕ) → (1 + n) + p ≡ 1 + (n + p)
  +-assoc-1 n p =
    begin
      (1 + n) + p
    ≡⟨⟩
      suc (0 + n) + p
    ≡⟨⟩
      suc ((0 + n) + p)
    ≡⟨ cong suc (+-assoc-0 n p) ⟩
      suc (0 + (n + p))
    ≡⟨⟩
      1 + (n + p)
    ∎
    where
    +-assoc-0 : ∀ (n p : ℕ) → (0 + n) + p ≡ 0 + (n + p)
    +-assoc-0 n p =
      begin
        (0 + n) + p
      ≡⟨⟩
        n + p
      ≡⟨⟩
        0 + (n + p)
      ∎

-- examples end

+-identityʳ : ∀ ( m : ℕ ) → m + zero ≡ m
+-identityʳ zero =
  begin
    zero + zero
  ≡⟨⟩
    zero
  ∎
+-identityʳ (suc m) =
  begin
    suc m + zero
  ≡⟨⟩
    suc (m + zero)
  ≡⟨ cong suc (+-identityʳ m) ⟩
    suc m
  ∎


+-suc : ∀ ( m n : ℕ ) → m + suc n ≡ suc ( m + n )
+-suc zero n =
  begin
    zero + suc n
  ≡⟨⟩
    suc n
  ≡⟨⟩
    suc (zero + n)
  ∎
+-suc (suc m) n =
  begin
    suc m + suc n
  ≡⟨⟩
    suc ( m + suc n )
  ≡⟨ cong suc (+-suc m n)⟩
    suc ( suc ( m + n ))
  ≡⟨⟩
    suc ( suc m + n)
  ∎

+-comm : ∀ ( m n : ℕ ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
  ≡⟨ +-identityʳ m ⟩
    m
  ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
    m + suc n
  ≡⟨ +-suc m n ⟩
    suc (m + n)
  ≡⟨ cong suc ( +-comm m n )⟩
    suc (n + m)
  ≡⟨⟩
    suc n + m
  ∎

+-rearrange : ∀ (m n p q : ℕ) → (m + n) + (p + q) ≡ m + (n + p) + q
+-rearrange m n p q =
  begin
    (m + n) + (p + q)
  ≡⟨ +-assoc m n (p + q) ⟩
    m + (n + (p + q))
  ≡⟨ cong ( m +_ ) (sym (+-assoc n p q)) ⟩
    m + ((n + p) + q)
  ≡⟨ sym (+-assoc m (n + p) q)⟩
    (m + (n + p)) + q
  ∎

-- @stretch: exercise `finite-+-assoc` ignore

+-assoc' : ∀ ( m n p : ℕ ) → (m + n) + p ≡ m + (n + p)
+-assoc' zero n p = refl
+-assoc' (suc m) n p rewrite +-assoc' m n p  = refl

+-identity' : ∀ ( n : ℕ ) → n + zero ≡ n
+-identity' zero = refl
+-identity' (suc m) rewrite +-identity' m = refl

+-suc' : ∀ ( m n : ℕ ) → m + suc n ≡ suc (m + n)
+-suc' zero n = refl
+-suc' (suc m) n rewrite +-suc' m n  = refl

+-comm' : ∀ ( m n : ℕ ) → m + n ≡ n + m
+-comm' m zero rewrite +-identity' m = refl
+-comm' m (suc n) rewrite +-suc' m n | +-comm' m n = refl

-- with hole

+-assoc'' : ∀ (m n p : ℕ ) → ( m + n ) + p ≡ m + (n + p)
+-assoc'' zero n p = refl
+-assoc'' (suc m) n p rewrite +-assoc'' m n p = refl

-- @recommended: exercise `+-swap` start

+-swap : ∀ ( m n p : ℕ ) → m + (n + p) ≡ n + (m + p)
+-swap zero n p = refl
+-swap (suc m) n p rewrite +-swap m n p | +-suc n (m + p) = refl

-- @recommended: exercise `+-swap` end

-- @recommended: exercise `*-distrib-+` start

*-distrib-+ : ∀ ( m n p : ℕ ) → ( m + n ) * p ≡ m * p + n * p
*-distrib-+ zero n p = refl
*-distrib-+ (suc m) n p rewrite *-distrib-+ m n p | +-assoc p (m * p) (n * p) = refl

-- @recommended: exercise `*-distrib-+` end

-- @recommended: exercise `*-assoc` start

*-assoc : ∀ ( m n p : ℕ ) → (m * n) * p ≡ m * ( n * p)
*-assoc zero n p = refl
*-assoc (suc m) n p rewrite *-distrib-+ n (m * n) p | *-assoc m n p = refl

-- @recommended: exercise `*-assoc` end

-- @practice: exercise `*-comm` start

*-zeroʳ : ∀ ( m : ℕ ) → m * zero ≡ zero
*-zeroʳ zero = refl
*-zeroʳ ( suc m ) rewrite *-distrib-+ 1 m zero | *-zeroʳ m = refl

*-suc : ∀ ( n m : ℕ ) → n + n * m ≡ n * suc m
*-suc zero m = refl
*-suc (suc n) m
  rewrite
    sym (*-suc n m) |
    sym (+-assoc m n ( n * m )) | sym ( +-assoc n m ( n * m )) |
    +-comm m n = refl

*-comm : ∀ ( m n : ℕ ) → m * n ≡ n * m
*-comm zero n rewrite *-zeroʳ n = refl
*-comm (suc m) n rewrite *-distrib-+ 1 m n | +-identityʳ n | *-comm m n | *-suc n m =  refl

-- @practice: exercise `*-comm` end
-- @practice: exercise `0∸n≡0` start

0∸n≡0 : ∀ ( n : ℕ ) → 0 ∸ n ≡ 0
0∸n≡0 zero = refl
0∸n≡0 (suc n) = refl

-- @practice: exercise `0∸n≡0` end
-- @practice: exercise `∸-|-assoc` start

∸-|-assoc : ∀ ( m n p : ℕ ) → m ∸ n ∸ p ≡ m ∸ ( n + p )
∸-|-assoc zero n p rewrite 0∸n≡0 n | 0∸n≡0 p | 0∸n≡0 ( n + p )  = refl
∸-|-assoc (suc m) zero p = refl
∸-|-assoc (suc m) (suc n) p rewrite ∸-|-assoc m n p = refl

-- @practice: exercise `∸-|-assoc` end

-- @stretch: exercise `+*^` start

^-distribʳ-+-* : ∀ ( m n p : ℕ ) → m ^ ( n + p ) ≡ (m ^ n) * (m ^ p)
^-distribʳ-+-* m zero zero = refl
^-distribʳ-+-* m zero (suc p) rewrite +-identityʳ (m * (m ^ p)) = refl
^-distribʳ-+-* m (suc n) p =
  begin
    m ^ (suc n + p)
  ≡⟨⟩
    m ^ suc ( n + p )
  ≡⟨⟩
    m * ( m ^ ( n + p ) )
  ≡⟨ cong (_*_ m) ( ^-distribʳ-+-* m n p ) ⟩
    m * ( ( m ^ n ) * ( m ^ p ) )
  ≡⟨ sym (*-assoc m (m ^ n) (m ^ p)) ⟩
    m * ( m ^ n ) * (m ^ p)
  ≡⟨⟩
    (m ^ suc n ) * ( m ^ p )
  ∎

^-sucʳ : ∀ ( n p : ℕ ) → n ^ suc p ≡ n * n ^ p
^-sucʳ n p  = refl

^-distribˡ-* : ∀ ( m n p : ℕ ) → (m * n) ^ p ≡ ( m ^ p ) * ( n ^ p)
^-distribˡ-* m n zero = refl
^-distribˡ-* m n (suc p) =
  begin
    (m * n) ^ ( suc p )
  ≡⟨ ^-sucʳ (m * n) p ⟩
    (m * n) * ((m * n) ^ p)
  ≡⟨ cong (_*_ (m * n)) (^-distribˡ-* m n p) ⟩
    m * n  * ((m ^ p) * (n ^ p))
  ≡⟨ sym (*-assoc (m * n) (m ^ p) (n ^ p)) ⟩
    m * n * (m ^ p) * (n ^ p)
  ≡⟨ *-1234-[13][24] m n (m ^ p) (n ^ p) ⟩
    (m * m ^ p ) * (n * n ^ p)
  ≡⟨ cong (_*_ (m * m ^ p)) (sym (^-sucʳ n p))⟩
    (m * m ^ p ) * ( n ^ suc p)
  ≡⟨ cong (λ {x → x * (n ^ suc p)}) (^-sucʳ m p) ⟩
    (m ^ suc p) * (n ^ suc p)
  ∎
  where
  *-1234-[13][24] : ∀ ( a b c d : ℕ ) → a * b * c * d ≡ (a * c) * ( b * d )
  *-1234-[13][24] a b c d
    rewrite
      *-assoc a b c |
      *-comm b c |
      sym (*-assoc a c b) |
      *-assoc (a * c) b d = refl

^-oneˡ : ∀ ( p : ℕ ) → 1 ^ p ≡ 1
^-oneˡ zero = refl
^-oneˡ (suc p) rewrite ^-oneˡ p = refl

^-distribʳ-* :  ∀ ( m n p : ℕ ) → m ^ (n * p) ≡ (m ^ n) ^ p
^-distribʳ-* m zero p rewrite ^-oneˡ p = refl
^-distribʳ-* m n zero rewrite *-zeroʳ n = refl
^-distribʳ-* m (suc n) (suc p)
  rewrite
    ^-distribʳ-+-* m p ( n * suc p) |
    ^-distribˡ-* m (m ^ n) p |
    ^-distribʳ-* m n (suc p) =
    begin
      m * ((m ^ p) * ((m ^ n) * ((m ^ n) ^ p)))
    ≡⟨ *-1[2[34]]-13[24] m (m ^ p) (m ^ n) ((m ^ n) ^ p) ⟩
      m * (m ^ n) * ((m ^ p) * ((m ^ n) ^ p))
    ∎
    where
    *-1[2[34]]-13[24] : ∀ ( a b c d : ℕ ) → a * ( b * ( c * d ) ) ≡ a * c * ( b * d )
    *-1[2[34]]-13[24] a b c d
      rewrite
        sym (*-assoc b c d) |
        *-comm b c |
        *-assoc c b d |
        sym (*-assoc a c (b * d)) = refl
        
-- @stretch: exercise `+*^` end

-- @stretch: exercise `Bin-laws` start

data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc (m O) = m I
inc (m I) = (inc m) O
inc ⟨⟩ = ⟨⟩ I

to : ℕ → Bin
to zero = ⟨⟩ O
to (suc n) = inc (to n)

from : Bin -> ℕ
from (m O) = 2 * (from m)
from (m I) = 2 * (from m) + 1
from ⟨⟩ = 0

comm-+-from : ∀ ( b : Bin ) → from (inc b) ≡ suc (from b)
comm-+-from ⟨⟩ = refl
comm-+-from (b O)
  rewrite
    sym ( +-suc' (from b) (from b + zero)) |
    +-assoc' (from b) (from b + 0) 1 |
    +-identityʳ (from b) |
    +-comm' (from b) 1 = refl
comm-+-from (b I)
  rewrite
    +-identityʳ (from (inc b)) | +-identityʳ ( from b) |
    comm-+-from b |
    sym (+-comm' (from b) 1) |
    +-assoc' (from b) (from b) 1 = refl

identity-to-from : ∀ ( n : ℕ ) → from (to n) ≡ n
identity-to-from zero = refl
identity-to-from (suc n) rewrite comm-+-from (to n) | identity-to-from n  = refl

-- ⟨⟩ !== ⟨⟩ O

-- @stretch: exercise `Bin-laws` end
