-- import «Algol»

import Algol.Diophantus.Quadratic.PellEquation

#eval PellSolution.seven 0 -- { p := 8, q := 3 }
#eval PellSolution.seven 1 -- { p := 127, q := 48 }
#eval PellSolution.seven 2 -- { p := 2024, q := 765 }
#eval PellSolution.seven 3 -- { p := 32257, q := 12192 }



def main : IO Unit :=
  IO.println s!"Hello, Algol!"
