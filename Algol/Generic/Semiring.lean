/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Class

class Semiring (a : Type) where
  nil : a
  one : a
  add : a → a → a
  mul : a → a → a

instance [Semiring α] : HasNil α := ⟨Semiring.nil⟩
instance [Semiring α] : HasOne α := ⟨Semiring.one⟩

instance {α} [Semiring α] : Add α := ⟨Semiring.add⟩
instance {α} [Semiring α] : Mul α := ⟨Semiring.mul⟩
