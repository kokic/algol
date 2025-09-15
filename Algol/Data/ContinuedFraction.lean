/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Data.Mat2x2

import Algol.Infoview.RemoteKaTeX
import Algol.Polynomial.Basic

import Algol.Mac.TeX

namespace SimpleContinuedFraction

def T [HasNil α] [HasOne α] (a : α)
    : Mat2x2 α :=
  Mat2x2.mk
    a HasOne.one
    HasOne.one HasNil.nil

def fold [HasNil α] [HasOne α] [Add α] [Mul α]
    (data : List α) : Mat2x2 α :=
  (data.map fun a => T a).foldl (init := .id) .mul

def cfrac [Repr α] (data : List α) : String :=
  go (data.map fun x => reprStr x)
where
  go : List String → String
    | [x] => x
    | x :: xs => x ++ " + " ++ "cfrac".tex ["1", go xs]
    | _ => ""

def evalTo
    [HasNil α] [HasOne α] [Add α] [Mul α] [Repr α]
    (data : List α) : RenderWidgetProps :=
  let matrix := fold data
  let result := "cfrac".tex [reprStr matrix.a₁₁, reprStr matrix.a₂₁]
  (cfrac data ++ " \\quad=\\quad " ++ result).render

end SimpleContinuedFraction

def var_q := var "q"
def q := poly [monomial (1, [(var_q, 1)])]

instance : OfNat (Polynomial Nat Nat) n :=
  ⟨ite (n == 0) .zero (poly [monomial (n, [(var_q, 0)])])⟩

instance : HasOne (Polynomial Nat Nat) := ⟨1⟩

open SimpleContinuedFraction in
#show evalTo [q, 2*q^2, 3*q^3, 4*q^4]
