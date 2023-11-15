module Nov14full where

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
  T-True    : ⊢ true ∶ bool
  T-False   : ⊢ false ∶ bool
  T-Zero    : ⊢ zero ∶ nat
  T-Succ    : ∀{x} → ⊢ x ∶ nat →
                     ---------
                     ⊢ suc x ∶ nat
  T-If : ∀ {cd th el} {t : Type} →
    ⊢ cd ∶ bool → ⊢ th ∶ t → ⊢ el ∶ t →
    ------------------------------------
    ⊢ if cd then th else el ∶ t
  T-Pred : ∀{x} → ⊢ x ∶ nat → ⊢ pred x ∶ nat
  T-IsZero : ∀{x} → ⊢ x ∶ nat → ⊢ iszero x ∶ bool

data _⟶_ : Expr → Expr → Set where
  E-IfTrue : ∀ {th el} → if true then th else el ⟶ th
  E-IfFalse : ∀ {th el} → if false then th else el ⟶ el
  E-If : ∀ {co co' th el} → co ⟶ co' →
        -----------------------------------------------
        if co then th else el ⟶ if co' then th else el
  E-Succ : ∀ {x y} → x ⟶ y → suc x ⟶ suc y
  E-PredZero : pred zero ⟶ zero
  E-PredSuc : ∀ {x} → NV x → pred (suc x) ⟶ x
  E-Pred : ∀ {x y} → x ⟶ y → pred x ⟶ pred y
  E-IsZeroZero : iszero zero ⟶ true
  E-IsZeroSucc : ∀ {x} → NV x → iszero (suc x) ⟶ false
  E-IsZero : ∀ {x y} → x ⟶ y → iszero x ⟶ iszero y

-- Inversion will be 'free'

-- Uniqueness
uniq : ∀ {t : Expr} {T T' : Type} → ⊢ t ∶ T → ⊢ t ∶ T' → T ≡ T'
uniq T-True T-True = refl
uniq T-False T-False = refl
uniq T-Zero T-Zero = refl
uniq (T-Succ d1) (T-Succ d2) = refl
uniq (T-If d1 d3 d4) (T-If d2 d5 d6) = uniq d3 d5
uniq (T-Pred d1) (T-Pred d2) = refl
uniq (T-IsZero d1) (T-IsZero d2) = refl

-- progress
progress : ∀ {t T} → ⊢ t ∶ T →
                       Value t ⊎ (Σ Expr (λ t' → t ⟶ t'))
progress T-True = inj₁ isT
progress T-False = inj₁ isF
progress T-Zero = inj₁ (isNV isZ)
progress (T-Succ {n} typed) with progress typed
... | inj₁ (isNV n-is-NV) = inj₁ (isNV (isS n-is-NV))
... | inj₂ (t' , n-steps) = inj₂ (suc t' , E-Succ n-steps)
progress (T-If {th = th} {el = el} typed typed₁ typed₂) with progress typed
... | inj₁ isT = inj₂ (th , E-IfTrue)
... | inj₁ isF = inj₂ (_ , E-IfFalse)
progress (T-If {th = _} {_} () typed₁ typed₂) | inj₁ (isNV isZ)
progress (T-If {th = _} {_} () typed₁ typed₂) | inj₁ (isNV (isS x))
... | inj₂ (t' , cd⟶t') = inj₂ (if t' then th else el , E-If cd⟶t')
progress (T-Pred typed) with progress typed
... | inj₁ (isNV isZ) = inj₂ (zero , E-PredZero)
... | inj₁ (isNV (isS n-is-NV)) = inj₂ (_ , E-PredSuc n-is-NV)
... | inj₂ (t' , x⟶t') = inj₂ (pred _ , E-Pred x⟶t')
progress (T-IsZero typed) with progress typed
... | inj₁ (isNV isZ) = inj₂ (true , E-IsZeroZero)
... | inj₁ (isNV (isS n)) = inj₂ (false , E-IsZeroSucc n)
... | inj₂ (t' , x⟶t') = inj₂ (iszero _ , E-IsZero x⟶t')

-- preservation
pres : ∀ {t t' T} → ⊢ t ∶ T → t ⟶ t' → ⊢ t' ∶ T
pres (T-If typed typed₁ typed₂) E-IfTrue = typed₁
pres (T-If typed typed₁ typed₂) E-IfFalse = typed₂
pres (T-If typed typed₁ typed₂) (E-If step) = T-If (pres typed step) typed₁ typed₂
pres (T-Succ typed) (E-Succ step) = T-Succ (pres typed step)
pres (T-Pred typed) E-PredZero = typed
pres (T-Pred (T-Succ typed)) (E-PredSuc x) = typed
pres (T-Pred typed) (E-Pred step) = T-Pred (pres typed step)
pres (T-IsZero typed) E-IsZeroZero = T-True
pres (T-IsZero typed) (E-IsZeroSucc x) = T-False
pres (T-IsZero typed) (E-IsZero step) = T-IsZero (pres typed step)

-- canonical forms
canBool : ∀ {v} → ⊢ v ∶ bool → Value v → v ≡ true ⊎ v ≡ false
canBool typed isT = inj₁ refl
canBool typed isF = inj₂ refl
canBool () (isNV isZ)
canBool () (isNV (isS x))

canNat : ∀ {v} → ⊢ v ∶ nat → Value v → NV v
canNat typed (isNV x) = x
