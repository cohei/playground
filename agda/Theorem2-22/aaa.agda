module aaa where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

data Exp : Set where
  nat : ℕ → Exp
  _+_ : Exp → Exp → Exp
  _*_ : Exp → Exp → Exp

infixl 60 _+_
infixl 70 _*_

data Plus : ℕ → ℕ → ℕ → Set where
  P-Zero : ∀ n → Plus zero n n
  P-Succ : ∀ n₁ n₂ n → Plus n₁ n₂ n → Plus (succ n₁) n₂ (succ n)

data Times : ℕ → ℕ → ℕ → Set where
  T-Zero : ∀ n → Times zero n zero
  T-Succ : ∀ n₁ n₂ n₃ n₄ → Times n₁ n₂ n₃ → Plus n₂ n₃ n₄ → Times (succ n₁) n₂ n₄

data _⟶_ : Exp → Exp → Set where
  R-Plus  : ∀ n₁ n₂ n₃ → Plus n₁ n₂ n₃ → nat n₁ + nat n₂ ⟶ nat n₃
  R-Times : ∀ n₁ n₂ n₃ → Times n₁ n₂ n₃ → nat n₁ * nat n₂ ⟶ nat n₃
  R-PlusL : ∀ e₁ e₁' e₂ → e₁ ⟶ e₁' → e₁ + e₂ ⟶ e₁' + e₂
  R-PlusR : ∀ e₁ e₂ e₂' → e₂ ⟶ e₂' → e₁ + e₂ ⟶ e₁ + e₂'
  R-TimesL : ∀ e₁ e₁' e₂ → e₁ ⟶ e₁' → e₁ * e₂ ⟶ e₁' * e₂
  R-TimesR : ∀ e₁ e₂ e₂' → e₂ ⟶ e₂' → e₁ * e₂ ⟶ e₁ * e₂'

infixl 50 _⟶_

data _∧_ (P : Set) (Q : Set) : Set where
  ∧-intro : P → Q → (P ∧ Q)

-- 自然数でないならば、という条件も必要らしい
-- それ以上の簡約がないため

-- confluence : ∀ e₁ e₂ e₃ → (e₁ ⟶ e₂) → (e₁ ⟶ e₃) → ∃ e₄. (e₂ ⟶ e₄) ∧ (e₃ ⟶ e₄)
-- confluence = {}
