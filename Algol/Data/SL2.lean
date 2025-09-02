/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Data.Mat2x2

namespace SL2

/-- The translation matrix. -/
def T [HasNil α] [HasOne α] : Mat2x2 α :=
  Mat2x2.mk
    HasOne.one HasOne.one
    HasNil.nil HasOne.one

/-- The inversion matrix -/
def S
    [HasNil α] [HasOne α] [Neg α] : Mat2x2 α :=
  Mat2x2.mk
    HasOne.one (Neg.neg HasOne.one)
    HasOne.one HasNil.nil

end SL2
