
# Algol
(computer) algebraic algorithm in lean4


### Adic.Series

- $998244353 = 3 + 4\cdot5^2 + 4\cdot5^3 + 3\cdot5^5 + 2\cdot5^6 + 2\cdot5^7 + 1\cdot5^9 + 2\cdot5^{10} + 4\cdot5^{12} \in \mathbf{Z}_5$

  ```lean
  import Algol.Adic.Series

  #eval AdicNumberSeries.fromNat 5 998244353
  -- { adic := 5, data := [3, 0, 4, 4, 0, 3, 2, 2, 0, 1, 2, 0, 4] }
  ```

- $\sqrt{2} = 3 + 1\cdot7 + 2\cdot7^{2} + 6\cdot7^{3} + 1\cdot7^{4} + 2\cdot7^{5} + 1\cdot7^{6} + 2\cdot7^{7} + 4\cdot7^{8} + \cdots \in\mathbf{Q}_7$

  ```lean
  import Algol.Adic.Series

  #eval println! AdicNumberSeries.sqrtByEnum 2 7 9 |>.toTeX
  -- 3 + 1\cdot7 + 2\cdot7^{2} + 6\cdot7^{3} + 1\cdot7^{4} + 2\cdot7^{5} + 1\cdot7^{6} + 2\cdot7^{7} + 4\cdot7^{8}
  ```


### Diophantus.Quadratic.PellEquation

- $(x,y)\in\mathbf{Z}_{\ge1}$ s.t. $x^2-7y^2=1$

  ```lean
  import Algol.Diophantus.Quadratic.PellEquation

  #eval PellSolution.seven 0 -- { p := 8, q := 3 }
  #eval PellSolution.seven 1 -- { p := 127, q := 48 }
  #eval PellSolution.seven 2 -- { p := 2024, q := 765 }
  #eval PellSolution.seven 3 -- { p := 32257, q := 12192 }
  ```

### Diophantus.Cubic.Homogeneous

- a rational point $P=(1,2)$ in $C:x^3+y^3=9$
  - $2P=(\frac{-17}{7}, \frac{20}{7})$
  - $4P=(\frac{188479}{90391}, \frac{-36520}{90391})$
  - $8P=(\frac{1243617733990094836481}{609623835676137297449}, \frac{487267171714352336560}{609623835676137297449})$

  ```lean
  import Algol.Diophantus.Cubic.Homogeneous
  
  def theP := QPoint.mk 1 2
  def the2P := Plane.doublePoint theP
  def the4P := Plane.doublePoint the2P
  def the8P := Plane.doublePoint the4P
  
  #eval the2P -- { x := (-17 : Rat)/7, y := (20 : Rat)/7 }
  #eval the4P -- { x := (188479 : Rat)/90391, y := (-36520 : Rat)/90391 }
  #eval the8P -- { x := (1243617733990094836481 : Rat)/609623835676137297449, y := (487267171714352336560 : Rat)/609623835676137297449 }
  ```


