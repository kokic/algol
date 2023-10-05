

import Algol.Common.Rat

open Lean

-- x³ + y³ = a
def doublePointX (P : QPoint) :=
  let (x, y) := (P.x, P.y)
  x * (x ^ 3 + 2 * y ^ 3) / (x ^ 3 - y ^ 3)

def doublePointY (P : QPoint) := doublePointX (QPoint.mk P.y P.x)


