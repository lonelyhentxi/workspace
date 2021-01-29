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

infixl 6 _+_

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

infixl 7 _*_

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

infixl 8 _^_

_ : 3 ^ 4 ≡ 81
_ = refl
-- @recommend: exercise `_^_` end


_∸_ : ℕ → ℕ → ℕ
m    ∸ zero = m
zero ∸ n = zero
suc m ∸ suc n = m ∸ n

infixl 6 _∸_

_ =
  begin
    3 ∸ 2
  ≡⟨⟩
    2 ∸ 1
  ≡⟨⟩
    1 ∸ 0
  ≡⟨⟩
    1
  ∎

_ =
  begin
    2 ∸ 3
  ≡⟨⟩
    1 ∸ 2
  ≡⟨⟩
    0 ∸ 1
  ≡⟨⟩
    0
  ∎

-- @recommend: exercise `∸-examples` start
_ =
  begin
    5 ∸ 3
  ≡⟨⟩
    4 ∸ 2
  ≡⟨⟩
    3 ∸ 1
  ≡⟨⟩
    2 ∸ 0
  ≡⟨⟩
    2
  ∎

_ =
  begin
    2 ∸ 5
  ≡⟨⟩
    1 ∸ 4
  ≡⟨⟩
    0 ∸ 3
  ≡⟨⟩
    0
  ∎
-- @recommend: exercise `∸-examples` end

-- infixl 6 _+_ _∸_
-- infixl 7 _⋆_
-- infixl indicates an operator associates to the left
-- infixr indicates an operator associates to the right
-- infix indicates that parentheses are always required

-- C-c + C-l convert `?` to `{}#`
-- C-c + C-f jump to next hole
-- C-c + C-c pattern variables to case
-- C-c + C-, the target type and the context
-- C-c + C-. the target type and the context and the inferred type
-- C-c + C-r recommendation
-- C-c + C-SPA fill up the hole
-- C-c + C-a auto fill up the holes

{-# BUILTIN NATPLUS _+_ #-}
{-# BUILTIN NATTIMES _*_ #-}
{-# BUILTIN NATMINUS _∸_ #-}

-- @stretch: exercise `Bin` start

data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc (m O) = m I
inc (m I) = (inc m) O
inc ⟨⟩ = ⟨⟩ I

-- inc unit test start

_ =
  begin
    inc (⟨⟩)
  ≡⟨⟩
    ⟨⟩ I
  ∎

_ =
  begin
    inc (⟨⟩ I)
  ≡⟨⟩
    ⟨⟩ I O
  ∎

_ =
  begin
    inc (⟨⟩ I O)
  ≡⟨⟩
    ⟨⟩ I I
  ∎

_ =
  begin
    inc (⟨⟩ I O I I)
  ≡⟨⟩
    (inc (⟨⟩ I O I)) O
  ≡⟨⟩
    ( (inc (⟨⟩ I O)) O ) O
  ≡⟨⟩
    ((⟨⟩ I I) O ) O
  ≡⟨⟩
    ⟨⟩ I I O O
  ∎

-- inc unit test end

to : ℕ → Bin
to zero = ⟨⟩ O
to (suc n) = inc (to n)

from : Bin -> ℕ
from (m O) = 2 * (from m)
from (m I) = 2 * (from m) + 1
from ⟨⟩ = 0

-- from & to unit test start

_ : (to 11) ≡ (⟨⟩ I O I I)
_ = refl

_ : (from (⟨⟩ I O I I)) ≡ 11
_ = refl

_ : (to 0) ≡ (⟨⟩ O)
_ = refl

_ : (to 1) ≡ (⟨⟩ I)
_ = refl

_ : (from (⟨⟩ I)) ≡ 1
_ = refl

_ : (from (⟨⟩ O)) ≡ 0
_ = refl

-- from & to unit test end

-- @stretch: exercise `Bin` end

-- import Data.Nat using (ℕ; zero; suc; _+_; _*_; _^_; _∸_)
