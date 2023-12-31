
import Algol.Common.QPoint

open Lean

namespace EllipticCurve


structure ECQPoint extends QPoint

instance : Repr ECQPoint where
  reprPrec p n := reprPrec p.x n

def ECQPoint.toTeX : ECQPoint → String := (·.toQPoint.toTeX)

structure WeierstrassForm :=
  A : Int
  B : Int

def prettyCoeff : Int → String
  | .ofNat n => s!" + {n}" -- Int.ofNat
  | .negSucc n => s!" - {n + 1}"  

def prettyTerm : Int → String → String
  | 0, _ => ""
  | 1, x => s!"{if x == "" then "1" else x}"
  | a, x => s!"{prettyCoeff a}{x}"


instance : Repr WeierstrassForm where
  reprPrec form _ := 
    let term₁ := prettyTerm form.A "x"
    let term₂ := prettyTerm form.B ""
    s!"y² = x³{term₁}{term₂}"


-- #eval WeierstrassForm.mk (-0) (-2)

namespace WeierstrassForm
-- y² = x³ + Ax + B

def doublePointXUnderly (A B : Int) (x : Rat) := 
  (x ^ 4 - 2 * A * x ^ 2 - 8 * B * x + (A ^ 2 : Int)) / 
  (4 * (x ^ 3 + A * x + B))

def doublePointUnderly (A : Int) (P : ECQPoint) : ECQPoint :=
  let (x, y) := (P.x, P.y)
  let k := (3 * x ^ 2 + A) / (2 * y)
  let x' := k ^ 2 - 2 * x
  let y' := - (k * (x' - x) + y)
  ⟨x', y'⟩

def doublePoint (form : WeierstrassForm) (P : ECQPoint) :=
  doublePointUnderly form.A P


end WeierstrassForm





def inverse (P : ECQPoint) : ECQPoint := ⟨P.x, -P.y⟩

-- assume P ≠ Q
def additionFormula (P Q : ECQPoint) : ECQPoint :=
  let (x₁, y₁, x₂, y₂) := (P.x, P.y, Q.x, Q.y)
  let k := (y₁ - y₂) / (x₁ - x₂)
  let x' := k ^ 2 - x₁ - x₂
  let y' := - (k * (x' - x₁) + y₁)
  ⟨x', y'⟩


instance : Add ECQPoint := ⟨additionFormula⟩
instance : Neg ECQPoint := ⟨inverse⟩

end EllipticCurve
