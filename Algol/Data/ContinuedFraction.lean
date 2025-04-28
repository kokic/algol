
import Algol.Data.Mat2x2

namespace SimpleContinuedFraction

def T [HasNil α] [HasOne α] (a : α)
    : Mat2x2 α :=
  Mat2x2.mk
    a HasOne.one
    HasOne.one HasNil.nil

def fold [HasNil α] [HasOne α] [Add α] [Mul α]
    (data : List α) : Mat2x2 α :=
  (data.map fun a => T a).foldl (init := .id) .mul

end SimpleContinuedFraction
