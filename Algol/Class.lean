/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic
-/
class HasNil (α : Type u) where
  nil : α

class HasOne (α : Type u) where
  one : α

instance : HasNil Nat := ⟨0⟩
instance : HasOne Nat := ⟨1⟩
