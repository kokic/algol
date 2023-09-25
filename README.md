# Algol
computer algebraic algorithm in lean4



### Diophantus.Quadratic.PellEquation

- $(x,y)\in\mathbf{Z}_{\ge1}$ s.t. $x^2-7y^2=1$

  ```lean
  import Algol.Diophantus.Quadratic.PellEquation

  #eval PellSolution.seven 0 -- { p := 8, q := 3 }
  #eval PellSolution.seven 1 -- { p := 127, q := 48 }
  #eval PellSolution.seven 2 -- { p := 2024, q := 765 }
  #eval PellSolution.seven 3 -- { p := 32257, q := 12192 }
  ```


### Adic.Series

- $998244353 = 3 + 4\cdot5^2 + 4\cdot5^3 + 3\cdot5^5 + 2\cdot5^6 + 2\cdot5^7 + 1\cdot5^9 + 2\cdot5^{10} + 4\cdot5^{12} \in \mathbf{Z}_5$

  ```lean
  import Algol.Adic.Series

  #eval AdicNumberSeries.fromNat 5 998244353
  -- { adic := 5, data := [3, 0, 4, 4, 0, 3, 2, 2, 0, 1, 2, 0, 4] }
  ```

- $\sqrt{2} = 3 + 1\cdot7 + 2\cdot7^{2} + 6\cdot7^{3} + 1\cdot7^{4} + 2\cdot7^{5} + 1\cdot7^{6} + 2\cdot7^{7} + 4\cdot7^{8} + \cdots \in\mathbf{Q}_7$

  ```lean
  #eval println! AdicNumberSeries.sqrtByEnum 2 7 9 |>.toTeX
  -- 3 + 1\cdot7 + 2\cdot7^{2} + 6\cdot7^{3} + 1\cdot7^{4} + 2\cdot7^{5} + 1\cdot7^{6} + 2\cdot7^{7} + 4\cdot7^{8}
  ```


