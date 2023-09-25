
import Algol.Diophantus.ContinuedFrac

-- #check ContinuedFrac

-- Quadratic

namespace PellEquation


def quadraticContinuedFracFinite (d n : Nat) := Id.run do
  let mut xs : List Nat := []
  let mut approx := Float.sqrt d.toFloat
  for _ in [:n] do
    let cᵢ := approx.floor
    xs := xs ++ [cᵢ.toUInt32.toNat]
    approx := 1 / (approx - cᵢ)
  xs






structure PellPair :=
  p : Int
  q : Int
deriving Repr

def PellPair.addInt (pair : PellPair) (a : Int) :=
  PellPair.mk (pair.p + pair.q * a) pair.q

end PellEquation

def Mat2x2.toPair (A : Mat2x2 Nat) := PellEquation.PellPair.mk A.a₁₂ A.a₂₂


namespace PellSolution


instance [AddMul α] : Mul (Mat2x2 α) := ⟨.mul⟩

-- #check Mat2x2.mul
-- #check Mat2x2.powerNat


-- x² - 5y² = 1
-- #eval quadraticContinuedFracFinite 5 4
-- [2, 4, 4, 4]

def five (nth : Nat) := 
  let A := fracMatReg 4
  let B := A.powerNat 2 -- fracMatReg 4 |>.mul A
  A.mul (B.powerNat nth) |>.toPair |>.addInt 2

-- #eval five 0
-- #eval 9^2 - 4^2*5

-- #eval five 1
-- #eval 161^2 - 72^2*5




-- x² - 7y² = 1
-- #eval quadraticContinuedFracFinite 7 9
-- [2, 1, 1, 1, 4, 1, 1, 1, 4]

def seven (nth : Nat) := 
  let A := fracMatReg 1 |>.powerNat 3
  let B := fracMatReg 4 |>.mul A
  A.mul (B.powerNat nth) |>.toPair |>.addInt 2

-- #eval seven 0
-- #eval 8^2 - 3^2 * 7

-- #eval seven 1
-- #eval 127^2 - 48^2 * 7





