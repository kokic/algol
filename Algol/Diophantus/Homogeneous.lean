

import Algol.Common.QPoint

open Lean

namespace Plane

-- x² + y² = 1
def unitCirclePoint (t : Rat) :=
  let x := (1 - t ^ 2) / (1 + t ^ 2)
  let y := (2 * t) / (1 + t ^ 2)
  (x, y)

-- #eval unitCirclePoint (1/2)




-- x³ + y³ = a
def doublePointX (P : QPoint) :=
  let (x, y) := (P.x, P.y)
  x * (x ^ 3 + 2 * y ^ 3) / (x ^ 3 - y ^ 3)

def doublePointY (P : QPoint) := doublePointX (QPoint.mk P.y P.x)

def doublePoint (P : QPoint) := QPoint.mk (doublePointX P) (doublePointY P)

end Plane



-- x³ + y³ + z³ = 1
def unitCirclePoint𝕊3 (a : Rat) :=
  let (r3, a3) := ((3 : Rat), a ^ 3)
  let (c35, c36) := (r3 ^ 5, r3 ^ 6)
  let c35a := c35 * a3
  let s := (3 * a3) ^ 2 + r3 ^ 4 * a3 + c36
  let u := a * s
  let x := (a3 ^ 3 - c36) / u
  let y := (c36 - a3 ^ 3 + c35a) / u
  let z := (r3 ^ 3 * a3 ^ 2 + c35a) / u
  (x, y, z)

-- #eval unitCirclePoint𝕊3 (-3)



-- x³ + y³ + z³ = a
def threeRatExpress (a : Rat) :=
  let r3 := (3 : Rat)
  let (c35, c36) := (r3 ^ 5, r3 ^ 6)
  let c35a := c35 * a
  let s := (3 * a) ^ 2 + r3 ^ 4 * a + c36
  let x := (a ^ 3 - c36) / s
  let y := (c36 - a ^ 3 + c35a) / s
  let z := (r3 ^ 3 * a ^ 2 + c35a) / s
  (x, y, z)



-- #eval threeRatExpress 1
-- #eval threeRatExpress (-1)




#eval threeRatExpress 8
