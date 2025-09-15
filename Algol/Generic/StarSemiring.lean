/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Generic.Semiring

/--
`a : StarSemiring α` then `a∗` (i.e. `a\ast`) is the Kleene closure.

- `a∗ = 1 + a⁺`
- `a⁺ = a * a∗`
-/
class StarSemiring (a) extends Semiring a where
 star : a → a

declare_syntax_cat star_semiring

syntax ident"∗" : star_semiring
syntax star_semiring : term

macro_rules
  | `($x:ident∗) => `(StarSemiring.star $x)
