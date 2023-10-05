
import Lean.Data.Rat

open Lean

namespace Rat

def ppow : Rat → Nat → Rat
  | _, 0 => 1
  | r, 1 => r
  | r, n + 1 => r * ppow r n

instance : Pow Rat Nat where
  pow := Rat.ppow

end Rat


structure QPoint := 
  x : Rat
  y : Rat
deriving Repr

-- namespace QPoint

-- end QPoint
