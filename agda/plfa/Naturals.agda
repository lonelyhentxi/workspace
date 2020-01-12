module plfa.Naturals where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

_+_ : ℕ → ℕ → ℕ
zero + n = n
suc m + n = suc (m + n)

-- @practice: exercise `seven` start
_ : 7 ≡ suc (suc (suc (suc (suc (suc (suc zero))))))
_ = refl
-- @practice: exercise `seven` end
  

_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
  ≡⟨⟩
    (suc (suc zero)) + (suc (suc (suc zero)))
  ≡⟨⟩
    suc ((suc zero) + (suc (suc (suc zero))))
  ≡⟨⟩
    suc (suc (zero + (suc (suc (suc zero)))))
  ≡⟨⟩
    suc (suc (suc (suc (suc zero))))
  ≡⟨⟩
    5
  ∎

-- @practice: exercise `+-example` start

_ : 3 + 4 ≡ 7
_ =
  begin
    3 + 4
  ≡⟨⟩
    suc (2 + 4)
  ≡⟨⟩
    suc ( suc ( 1 + 4 ))
  ≡⟨⟩
    suc ( suc ( suc ( 0 + 4 )))
  ≡⟨⟩
    suc ( suc ( suc 4 ))
  ≡⟨⟩
    7
  ∎

-- @practice: exercise `+-example` end

_*_ : ℕ → ℕ → ℕ
zero * ℕ = zero
(suc m) * n = n + ( m * n )

_ =
  begin
    2 * 3
  ≡⟨⟩
    3 + ( 1 * 3 )
  ≡⟨⟩
    3 + ( 3 + ( 0 * 3 ))
  ≡⟨⟩
    3 + (3 + 0)
  ≡⟨⟩
    6
  ∎

-- @practice: exercise `*-example` start
_ =
  begin
    3 * 4
  ≡⟨⟩
    4 + ( 2 * 4)
  ≡⟨⟩
    4 + ( 4 + (1 * 4))
  ≡⟨⟩
    4 + ( 4 + (4 + (0 * 4)))
  ≡⟨⟩
    4 + (4 + (4 + 0))
  ≡⟨⟩
    12
  ∎
-- @practice: exercise `*-example` end

-- @recommend: exercise `_^_` start
_^_ : ℕ → ℕ → ℕ
m ^ zero = 1
m ^ (suc n) = m * (m ^ n)

_ : 3 ^ 4 ≡ 81
_ = refl
-- @recommend: exercise `_^_` end
