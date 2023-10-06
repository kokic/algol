
import Algol.Common.QPoint

open Lean

namespace EllipticCurve

namespace WeierstrassForm
-- y² = x³ + Ax + B

def doublePointX (A B : Int) (x : Rat) := 
  (x ^ 4 - 2 * A * x ^ 2 - 8 * B * x + (A ^ 2 : Int)) / 
  (4 * (x ^ 3 + A * x + B))

end WeierstrassForm


def additiveFormula (P Q : QPoint) :=
  let (x₁, y₁, x₂, y₂) := (P.x, P.y, Q.x, Q.y)
  let k := (y₁ - y₂) / (x₁ - x₂)
  k ^ 2 - x₁ - x₂

end EllipticCurve
