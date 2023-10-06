
import Lean.Data.Rat

namespace Lean

namespace Rat

def ppow : Rat → Nat → Rat
  | _, 0 => 1
  | r, 1 => r
  | r, n + 1 => r * ppow r n

instance : Pow Rat Nat where
  pow := Rat.ppow

def toTeX (r : Rat) := s!"\\frac\{{r.num}}\{{r.den}}"

end Rat

end Lean
