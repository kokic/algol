
import Algol.Class
import Algol.Generic.Power

structure Mat2x2 (α : Type u) where
  a₁₁ : α
  a₁₂ : α
  a₂₁ : α
  a₂₂ : α
deriving Repr

def Mat2x2.mul [Add α] [Mul α]
    : Mat2x2 α → Mat2x2 α → Mat2x2 α
  | A, B => Mat2x2.mk
    ((A.a₁₁ * B.a₁₁) + (A.a₁₂ * B.a₂₁))
    ((A.a₁₁ * B.a₁₂) + (A.a₁₂ * B.a₂₂))
    ((A.a₂₁ * B.a₁₁) + (A.a₂₂ * B.a₂₁))
    ((A.a₂₁ * B.a₁₂) + (A.a₂₂ * B.a₂₂))

instance [Add α] [Mul α] : Mul (Mat2x2 α) :=
  ⟨Mat2x2.mul⟩

def Mat2x2.id [HasNil α] [HasOne α]
    : Mat2x2 α := Mat2x2.mk
      HasOne.one HasNil.nil
      HasNil.nil HasOne.one

instance [HasNil α] [HasOne α] [Add α] [Mul α]
    : HPow (Mat2x2 α) Nat (Mat2x2 α) :=
  ⟨exponentBySquaring .mul .id⟩
