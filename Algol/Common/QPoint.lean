
import Algol.Common.Rat

open Lean

structure QPoint := 
  x : Rat
  y : Rat
deriving Repr

namespace QPoint

def toTeX (P : QPoint) :=
  s!"({P.x.toTeX}, {P.y.toTeX})"

end QPoint

