module Nov14 where

open import Relation.Binary.PropositionalEquality
  using (_≡_; refl)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Product using (Σ; _,_)

data Expr : Set where
  true : Expr
  false : Expr
  if_then_else : Expr → Expr → Expr → Expr
  zero : Expr
  suc : Expr → Expr
  iszero : Expr → Expr
  pred : Expr → Expr

data NV : Expr → Set where
  isZ : NV zero
  isS : ∀ {e} → NV e → NV (suc e)

data Value : Expr → Set where
  isT : Value true
  isF : Value false
  isNV : ∀ {e} → NV e → Value e

data Type : Set where bool nat : Type

data ⊢_∶_ : Expr → Type → Set where
  T-True : ⊢ true ∶ bool
  T-False : ⊢ false ∶ bool
  T-Zero : ⊢ zero ∶ nat
  T-Succ : ∀{x} → ⊢ x ∶ nat → ⊢ suc x ∶ nat
  T-If : ∀ {cd th el} {t : Type} → ⊢ cd ∶ bool → ⊢ th ∶ t → ⊢ el ∶ t →
    ⊢ if cd then th else el ∶ t
  T-Pred : ∀{x} → ⊢ x ∶ nat → ⊢ pred x ∶ nat
  T-IsZero : ∀{x} → ⊢ x ∶ nat → ⊢ iszero x ∶ bool

data _⟶_ : Expr → Expr → Set where
  E-IfTrue : ∀ {th el} → if true then th else el ⟶ th
  E-IfFalse : ∀ {th el} → if false then th else el ⟶ el
  E-If : ∀ {co co' th el} → co ⟶ co' → if co then th else el ⟶ if co' then th else el
  E-Succ : ∀ {x y} → x ⟶ y → suc x ⟶ suc y
  E-PredZero : pred zero ⟶ zero
  E-PredSuc : ∀ {x} → NV x → pred (suc x) ⟶ x
  E-Pred : ∀ {x y} → x ⟶ y → pred x ⟶ pred y
  E-IsZeroZero : iszero zero ⟶ true
  E-IsZeroSucc : ∀ {x} → NV x → iszero (suc x) ⟶ false
  E-IsZero : ∀ {x y} → x ⟶ y → iszero x ⟶ iszero y

-- Inversion will be 'free'

-- Uniqueness
uniq : ∀ {t T T'} → ⊢ t ∶ T → ⊢ t ∶ T' → T ≡ T'
uniq d1 d2 = {!!}

-- progress
progress : ∀ {t T} → ⊢ t ∶ T → Value t ⊎ (Σ Expr (λ t' → t ⟶ t'))
progress typed = {!!}

-- preservation
pres : ∀ {t t' T} → ⊢ t ∶ T → t ⟶ t' → ⊢ t' ∶ T
pres typed step = {!!}

-- canonical forms
canBool : ∀ {v} → ⊢ v ∶ bool → Value v → v ≡ true ⊎ v ≡ false
canBool typed val = {!!}

canNat : ∀ {v} → ⊢ v ∶ nat → Value v → NV v
canNat typed val = {!!}
