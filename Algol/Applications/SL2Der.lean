/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Data.Mat2x2
import Algol.Polynomial.Basic
import Std.Internal.Rat

open Std.Internal

instance : HasNil Rat := ⟨0⟩
instance : HasOne Rat := ⟨1⟩

abbrev E := Int

instance : HasNil E := ⟨0⟩
instance : HasOne E := ⟨1⟩

def var_x := var "x"
def x := poly [monomial ((1 : Rat), [(var_x, (1 : E))])]

def ofRat (r : Rat) : Polynomial E Rat :=
  if r == 0 then .zero else poly [monomial (r, [(var_x, 0)])]

instance : HasOne (Polynomial E Rat) := ⟨ofRat 1⟩

instance : Neg (Monomial E Rat) := ⟨fun a => .mk a.vars a.terms (-a.coefficient)⟩
instance : Neg (Polynomial E Rat) := ⟨fun a => poly (a.terms.map fun m => -m)⟩
instance : Sub (Polynomial E Rat) := ⟨fun a b => a + (-b)⟩

/--
( x 1 )
( 0 x )
-/
def dx := Mat2x2.mk x (ofRat 1) (ofRat 0) x

instance : Inhabited (Monomial E Rat) := ⟨monomial ((0 : Rat), [])⟩


#eval dx * dx.inv
