
# Algol
(computer) algebraic algorithm in lean4

- $(\tfrac43)^3 = (\tfrac67)^3 + (\tfrac{19}{21})^3$

- $(\tfrac89)^3 + (\tfrac{10}9)^3 + (\tfrac{24}{73})^3 = (\tfrac{30}{91})^3 + (\tfrac{487}{657})^3 + (\tfrac{971}{819})^3$




# Example

## Numerical

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

- $(x,y)\in\mathbf{Z}_ {\ge1}\times\mathbf{Z}_ {\ge1}$ and $x^2-7y^2=1$

  ```lean
  import Algol.Diophantus.Quadratic.PellEquation

  #eval PellSolution.seven 0 -- { p := 8, q := 3 }
  #eval PellSolution.seven 1 -- { p := 127, q := 48 }
  #eval PellSolution.seven 2 -- { p := 2024, q := 765 }
  #eval PellSolution.seven 3 -- { p := 32257, q := 12192 }
  ```

### Diophantus.Homogeneous

- a rational point $P=(1,2)$ in $C:x^3+y^3=9$
  - $2P=(\frac{-17}{7}, \frac{20}{7})$
  - $4P=(\frac{188479}{90391}, \frac{-36520}{90391})$
  - $8P=(\frac{1243617733990094836481}{609623835676137297449}, \frac{487267171714352336560}{609623835676137297449})$

  ```lean
  import Algol.Diophantus.Homogeneous
  
  def theP := QPoint.mk 1 2
  def the2P := Plane.doublePoint theP
  def the4P := Plane.doublePoint the2P
  def the8P := Plane.doublePoint the4P
  
  #eval the2P -- { x := (-17 : Rat)/7, y := (20 : Rat)/7 }
  #eval the4P -- { x := (188479 : Rat)/90391, y := (-36520 : Rat)/90391 }
  #eval the8P -- { x := (1243617733990094836481 : Rat)/609623835676137297449, y := (487267171714352336560 : Rat)/609623835676137297449 }
  ```

### Algol.Curve.EllipticCurve

- rational points $P=(1,2)$, $Q=(3, 4)$ in $C: y^2=x^3 - 7x + 10$
  - $2P=(-1,-4)$
  - $2Q=(\frac{1}{4}, \frac{23}{8})$
  - $P+Q=(-3,2)$
  - $3P+Q=(13,46)$
  - $-2(P+Q)=(31,172)$
  - $-4(P+Q)=(\frac{58409}{7396}, \frac{13451741}{636056})$

  ```lean
  import Algol.Curve.EllipticCurve

  def curve := EllipticCurve.WeierstrassForm.mk (-7) 10
  def P := QPoint.mk 1 2 
  def Q := QPoint.mk 3 4

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
  ```



## Algebraic

