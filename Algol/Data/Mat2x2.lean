/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Class
import Algol.Generic.Power

structure Mat2x2 (α : Type u) where
  a₁₁ : α
  a₁₂ : α
  a₂₁ : α
  a₂₂ : α
deriving Repr

def Mat2x2.add [Add α] : Mat2x2 α → Mat2x2 α → Mat2x2 α
  | A, B => Mat2x2.mk
    (A.a₁₁ + B.a₁₁) (A.a₁₂ + B.a₁₂)
    (A.a₂₁ + B.a₂₁) (A.a₂₂ + B.a₂₂)

instance [Add α] : Add (Mat2x2 α) :=
  ⟨Mat2x2.add⟩

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

def Mat2x2.det [Add α] [Mul α] [Sub α] : Mat2x2 α → α
  | A => (A.a₁₁ * A.a₂₂) - (A.a₁₂ * A.a₂₁)

def Mat2x2.trace [Add α] : Mat2x2 α → α
  | A => A.a₁₁ + A.a₂₂

def Mat2x2.inv
    [Add α] [Mul α] [Sub α] [Div α] [Neg α] : Mat2x2 α → Mat2x2 α
  | A =>
    let det := A.det
    Mat2x2.mk
      (A.a₂₂ / det) ((-A.a₁₂) / det)
      ((-A.a₂₁) / det) (A.a₁₁ / det)

def Mat2x2.transpose [Add α] [Mul α] : Mat2x2 α → Mat2x2 α
  | A => Mat2x2.mk
    A.a₁₁ A.a₂₁
    A.a₁₂ A.a₂₂

def Mat2x2.conjugate
    [Add α] [Mul α] [Sub α] [Div α] [Neg α] : Mat2x2 α → Mat2x2 α
  | A => (A.inv).transpose
