/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Generic.StarSemiring
import Std.Internal.Rat

open Std.Internal

inductive Compact where
  | real : Rat → Compact
  | inf : Compact
deriving Repr, Inhabited

instance : Semiring (Compact) where
  nil := .real 0
  one := .real 1
  add | .inf, _ => .inf
      | _, .inf => .inf
      | .real x, .real y => .real (x + y)
  mul | .real 0, _ => .real 0
      | _, .real 0 => .real 0
      | .inf, _ => .inf
      | _, .inf => .inf
      | .real x, .real y => .real (x * y)

instance : StarSemiring (Compact) where
  star | .real 1 => .inf
       | .real x => .real (1 / (1 - x))
       | .inf => .inf

instance : Neg (Compact) where
  neg | .real x => .real (-x)
      | _ => .inf
