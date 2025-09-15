/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/

/-
Polynomial Division with Remainder
-/
import Algol.Polynomial.Basic

partial def polynomial_div
    [DecidableEq e] [HasNil e] [Neg e] [Add e]
    [Mul e] [Div e] [LT e] [DecidableLT e] [Sub e] [Repr e]
    [Repr c] [HasNil c] [HasOne c] [BEq c] [Add c] [Mul c] [Div c] [Neg c]
    (a b : Polynomial e c)
  : Option (Polynomial e c × Polynomial e c) :=
    if b.isZero then none else
      some (div_impl a b .zero)
where
  div_impl : Polynomial e c → Polynomial e c → Polynomial e c → (Polynomial e c × Polynomial e c)
    | r, d, q =>
      match r.terms with
        | [] => (q, r)
        | r₁ :: _ =>
          match d.terms with
            | [] => (q, r) -- unreachable
            | d₁ :: _ =>
              let m := monomial_div! r₁ d₁
              if m.isZero then
                (q, r)
              else
                let q' := q + poly [m]
                let r' := r - (poly [m] * d)
                div_impl r' d q'

def polynomial_div!
    [DecidableEq e] [HasNil e] [Neg e] [Add e]
    [Mul e] [Div e] [LT e] [DecidableLT e] [Sub e] [Repr e]
    [Repr c] [HasNil c] [HasOne c] [BEq c] [Add c] [Mul c] [Div c] [Neg c]
    (a b : Polynomial e c) : (Polynomial e c × Polynomial e c) :=
  (polynomial_div a b).getD (.zero, a)
