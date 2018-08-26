module LearnYouAn where
  data ℕ : Set where
    zero : ℕ
    succ : ℕ → ℕ

  _+_ : ℕ → ℕ → ℕ
  zero + m = m
  (succ n) + m = succ (n + m)

  data _even : ℕ → Set where
    ZERO : zero even
    STEP : ∀ {n} → n even → (succ (succ n)) even

  proof₁ : succ (succ (succ (succ zero))) even
  proof₁ = STEP (STEP ZERO)

  proof₂' : {A : Set} -> A -> A
  proof₂' x = x

  proof₂ : ℕ → ℕ
  proof₂ = proof₂'
