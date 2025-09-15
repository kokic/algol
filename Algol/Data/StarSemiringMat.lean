/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Generic.Semiring
import Algol.Generic.StarSemiring
import Algol.Data.Mat

instance [Semiring α] [Inhabited α]
    : Semiring (Matrix α) where
  nil := .scalar Semiring.nil
  one := .scalar Semiring.one
  add := .add
  mul := .mul

partial def star [StarSemiring α] [Inhabited α]
    : Matrix α → Matrix α
  | .scalar x => .scalar (x∗)
  | .matrix [[x]] => .matrix [[x∗]]
  | M@(.matrix _) =>
    let (A, B, C, D) := Matrix.split! M
    let D' := star D
    let B' := B * D'
    let A' := star (A + B' * C)
    let C' := D' * C * A'
    Matrix.join (A', A' * B', C', D' + C' * B')

instance [StarSemiring α] [Inhabited α]
    : StarSemiring (Matrix α) where
  star := star

def inverse [StarSemiring α] [Neg α] [Inhabited α]
    (mat : Matrix α) :=
  StarSemiring.star (Semiring.one + mat.map Neg.neg)

def unit : Matrix (Compact) := Semiring.one
