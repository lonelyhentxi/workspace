module plfa.Relations where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open import Data.Nat using (ℕ ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm)

data _≤_ : ℕ → ℕ → Set where

  z≤n : ∀ { n : ℕ }
      ---------
      → zero ≤ n

  s≤s : ∀ { m n : ℕ }
    → m ≤ n
      ---------------
    → suc m ≤ suc n

infix 4 _≤_

_ : 2 ≤ 4
_ = s≤s (s≤s z≤n)

_ : 2 ≤ 4
_ = s≤s {1} {3} (s≤s {0} {2} (z≤n {2}))

_ : 2 ≤ 4
_ = s≤s {m = 1} {n = 3} (s≤s {m = 0} {n = 2} (z≤n {n = 2}))

_ : 2 ≤ 4
_ = s≤s {n = 3} (s≤s {n = 2} z≤n)

inv-s≤s : ∀ { m n : ℕ }
  → suc m ≤ suc n
    -------------
  → m ≤ n
inv-s≤s (s≤s m≤n) = m≤n

inv-z≤n : ∀ { m : ℕ }
  → m ≤ zero
    --------
  → m ≡ zero
inv-z≤n z≤n = refl

-- @practice: exercise `orderings` start

-- _⊆_ relations has partial order property but not total order
-- adjacency relation has preorder property but not partial order

-- @practice: exercise `orderings` end

-- 自反: reflexive - refl
-- 传递: transitive - trans
-- 反对称: anti-symmetric - antisym
-- 完全: total - total

≤-refl : ∀ { n : ℕ }
    -----
    → n ≤ n
≤-refl {zero} = z≤n
≤-refl {suc n} = s≤s ≤-refl


≤-trans : ∀ { m n p : ℕ }
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p
≤-trans z≤n    _ = z≤n
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)

