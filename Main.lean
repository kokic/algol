-- import «Algol»

import Algol.Adic.Series
import Algol.Diophantus.Quadratic.PellEquation
import Algol.Diophantus.Homogeneous
import Algol.Curve.EllipticCurve

-- import Algol.Adic.Series

#eval AdicNumberSeries.fromNat 5 998244353
-- { adic := 5, data := [3, 0, 4, 4, 0, 3, 2, 2, 0, 1, 2, 0, 4] }

#eval println! AdicNumberSeries.sqrtByEnum 2 7 9 |>.toTeX
-- 3 + 1\cdot7 + 2\cdot7^{2} + 6\cdot7^{3} + 1\cdot7^{4} + 2\cdot7^{5}
--   + 1\cdot7^{6} + 2\cdot7^{7} + 4\cdot7^{8}



-- import Algol.Diophantus.Quadratic.PellEquation

#eval PellSolution.seven 0 -- { p := 8, q := 3 }
#eval PellSolution.seven 1 -- { p := 127, q := 48 }
#eval PellSolution.seven 2 -- { p := 2024, q := 765 }
#eval PellSolution.seven 3 -- { p := 32257, q := 12192 }



-- import Algol.Diophantus.Homogeneous

def theP := QPoint.mk 1 2
def the2P := Plane.doublePoint theP
def the4P := Plane.doublePoint the2P
def the8P := Plane.doublePoint the4P

#eval the2P -- { x := (-17 : Rat)/7, y := (20 : Rat)/7 }
#eval the4P -- { x := (188479 : Rat)/90391, y := (-36520 : Rat)/90391 }
#eval the8P -- { x := (1243617733990094836481 : Rat)/609623835676137297449, y := (487267171714352336560 : Rat)/609623835676137297449 }



-- import Algol.Curve.EllipticCurve

def curve := EllipticCurve.WeierstrassForm.mk (-7) 10
def P : EllipticCurve.ECQPoint := QPoint.mk 1 2 
def Q : EllipticCurve.ECQPoint := QPoint.mk 3 4

#eval curve -- y² = x³ - 7x + 10
#eval curve.doublePoint P -- { x := -1, y := -4 }
#eval curve.doublePoint Q -- { x := (1 : Rat)/4, y := (23 : Rat)/8 }
#eval EllipticCurve.additionFormula P Q -- { x := -3, y := 2 }
#eval P + Q -- { x := -3, y := 2 }
#eval let P2 := curve.doublePoint P
      P + P2 + Q -- { x := 13, y := 46 }
#eval (- curve.doublePoint (P + Q)) -- { x := 31, y := 172 }
#eval let largeIntegerP := QPoint.mk 31 172
      curve.doublePoint largeIntegerP -- { x := (58409 : Rat)/7396, y := (13451741 : Rat)/636056 }

def main : IO Unit :=
  IO.println s!"Hello, Algol!"
