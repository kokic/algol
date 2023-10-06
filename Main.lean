-- import «Algol»

import Algol.Adic.Series
import Algol.Diophantus.Quadratic.PellEquation
import Algol.Diophantus.Cubic.Homogeneous


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



-- import Algol.Diophantus.Cubic.Homogeneous

def theP := QPoint.mk 1 2
def the2P := Plane.doublePoint theP
def the4P := Plane.doublePoint the2P
def the8P := Plane.doublePoint the4P

#eval the2P -- { x := (-17 : Rat)/7, y := (20 : Rat)/7 }
#eval the4P -- { x := (188479 : Rat)/90391, y := (-36520 : Rat)/90391 }
#eval the8P -- { x := (1243617733990094836481 : Rat)/609623835676137297449, y := (487267171714352336560 : Rat)/609623835676137297449 }




def main : IO Unit :=
  IO.println s!"Hello, Algol!"
