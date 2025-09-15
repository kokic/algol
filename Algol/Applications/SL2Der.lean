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
def poly_x := poly [monomial ((1 : Rat), [(var_x, (1 : E))])]

def ofRat (r : Rat) : Polynomial E Rat :=
  if r == 0 then .zero else poly [monomial (r, [(var_x, 0)])]

instance : HasOne (Polynomial E Rat) := ⟨ofRat 1⟩

instance : Neg (Monomial E Rat) := ⟨fun a => .mk a.vars a.terms (-a.coefficient)⟩
instance : Neg (Polynomial E Rat) := ⟨fun a => poly (a.terms.map fun m => -m)⟩
instance : Sub (Polynomial E Rat) := ⟨fun a b => a + (-b)⟩

inductive Expression where
  | poly : Polynomial E Rat → Expression
  | neg : Expression → Expression
  | add : Expression → Expression → Expression
  | mul : Expression → Expression → Expression
  | div : Expression → Expression → Expression

def reprExpr : Expression → String
  | .poly p => reprStr p
  | .neg e => "-(" ++ reprExpr e ++ ")"
  | .add e1 e2 => "(" ++ reprExpr e1 ++ " + " ++ reprExpr e2 ++ ")"
  | .mul e1 e2 => "(" ++ reprExpr e1 ++ " ⬝ " ++ reprExpr e2 ++ ")"
  | .div e1 e2 => "(" ++ reprExpr e1 ++ " / " ++ reprExpr e2 ++ ")"

instance : Repr Expression where
  reprPrec e _ := reprExpr e

instance : Coe (Polynomial E Rat) Expression := ⟨.poly⟩

def zero := Expression.poly (ofRat 0)

instance : Add Expression := ⟨ fun
  | .poly p1, .poly p2 => .poly (p1 + p2)
  | e1, e2 =>
    if reprExpr e1 == "0" then e2
    else if reprExpr e2 == "0" then e1
    else .add e1 e2
⟩
instance : Sub Expression := ⟨ fun
  | .poly p1, .poly p2 => .poly (p1 - p2)
  | e1, e2 => .add e1 (.neg e2)
⟩
instance : Mul Expression := ⟨ fun
  | .poly p1, .poly p2 => .poly (p1 * p2)
  | e1, e2 =>
    if reprExpr e1 == "0" ||
       reprExpr e2 == "0" then zero
    else if reprExpr e1 == "1" then e2
    else if reprExpr e2 == "1" then e1
    else .mul e1 e2
⟩
instance : Div Expression := ⟨ fun
  | e1, e2 =>
    if reprExpr e1 == "0" then zero
    else if reprExpr e2 == "1" then e1
    else .div e1 e2
⟩

instance : Neg Expression := ⟨ fun
  | .poly p => .poly (-p)
  | e => .neg e
⟩

def x := Expression.poly poly_x

/--
( x 1 )
( 0 x )
-/
def dx := Mat2x2.mk x (ofRat 1) (ofRat 0) x

instance : Inhabited (Monomial E Rat) := ⟨monomial ((0 : Rat), [])⟩

#eval dx.inv * dx.inv
