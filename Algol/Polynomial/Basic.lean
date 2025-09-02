/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Std.Data.HashSet
import Algol.Variable.Basic
import Algol.Monomial.Basic
import Algol.Abbrev
import Algol.Generic.Power

/-!
## Reference

- Zippel, Richard. Effective polynomial computation. Vol. 241. Springer Science & Business Media, 2012.
-/

open Std

/-- collect their variables from a series of monomials. -/
def vars (xs : List (Monomial e c))
               (capacity : Nat := 3)
    : HashSet Variable  :=
  xs.foldl (init := (.emptyWithCapacity capacity))
    fun s t => s.insertMany t.vars

/-- comparing the lexicographic order (`y < x`) and exponentiation of variables. -/
def monomial_lt
    [Repr e] [Repr c] [BEq e] [LT e] [DecidableLT e]
    (a b : Monomial e c) : Bool :=
  ite (a.vars.size < b.vars.size) true
      (ite (a.vars.size > b.vars.size) false
           (lt_impl
             a.sortedVars.toList
             b.sortedVars.toList))
where
  lt_impl : List Variable → List Variable → Bool
    | _, [] => false
    | [], _ => true
    | x :: xs, y :: ys =>
      dite (x.display ∈ a.terms ∧
            y.display ∈ b.terms)
        (fun h =>
          ite (var_exp_eq a b x y h)
              (lt_impl xs ys)
              (var_exp_order
                (x.gt y)
                (a.exp x h.left < b.exp y h.right)))
          (fun _ => false)
  var_exp_order : Bool → Bool → Bool
    | false, false => false
    | _, _ => true

/-- comparing the lexicographic order (`x > y`) and exponentiation of variables. -/
def monomial_gt
    [Repr e] [Repr c] [BEq e] [LT e] [DecidableLT e]
    (a b : Monomial e c) : Bool :=
  monomial_lt b a

structure Polynomial (e c : Type u) where
  vars : HashSet Variable
  terms : List (Monomial e c)

def poly (data : List (Monomial e c))
    : Polynomial e c :=
  let allVars := vars data
  .mk allVars data

def Polynomial.mul_id
    [BEq e] [HasNil e] [HasOne c]
    : Polynomial e c :=
  poly [.mul_id]

def Polynomial.zero : Polynomial e c := poly []

instance : HasNil (Polynomial e c) := ⟨.zero⟩

def polynomial_add
    [Repr e] [BEq e] [LT e] [DecidableLT e]
    [Repr c] [HasNil c] [BEq c] [Add c]
    (a b : Polynomial e c) : Polynomial e c :=
  let allVars := a.vars.insertMany b.vars
  .mk allVars (add_impl a.terms b.terms)
where
  add_impl : Bi (List (Monomial e c))
    | xs, [] => xs
    | [], ys => ys
    | x :: xs, y :: ys =>
      if monomial_gt x y then
        x :: add_impl xs (y :: ys) else
      if monomial_lt x y then
        y :: add_impl (x :: xs) ys else
      let coeff_sum := x.coefficient + y.coefficient
      ite (coeff_sum == HasNil.nil)
          (add_impl xs ys)
          ((.mk x.vars x.terms coeff_sum) :: add_impl xs ys)

instance
    [Repr e] [BEq e] [LT e] [DecidableLT e]
    [Repr c] [HasNil c] [BEq c] [Add c] [Mul c]
  : Add (Polynomial e c) := ⟨polynomial_add⟩

def polynoimal_mul
    [Repr e] [BEq e] [LT e] [DecidableLT e] [Add e]
    [Repr c] [HasNil c] [BEq c] [Add c] [Mul c]
    (a b : Polynomial e c) : Polynomial e c :=
  let allVars := a.vars.insertMany b.vars
  .mk allVars (mul_impl a.terms b.terms)
where
  mul_impl : Bi (List (Monomial e c))
    | [], _ => []
    | _, [] => []
    | x :: xs, ys => polynomial_add.add_impl
      (fixed x ys) (mul_impl xs ys)
  fixed : Monomial e c → List (Monomial e c) → List (Monomial e c)
    | _, [] => []
    | x, y :: ys =>
      (monomial_mul x y) :: fixed x ys

instance
    [Repr e] [BEq e] [LT e] [DecidableLT e] [Add e]
    [Repr c] [HasNil c] [BEq c] [Add c] [Mul c]
  : Mul (Polynomial e c) := ⟨polynoimal_mul⟩

instance
    [Repr e] [HasNil e] [BEq e] [LT e] [DecidableLT e] [Add e]
    [Repr c] [HasNil c] [HasOne c] [BEq c] [Add c] [Mul c]
    : HPow (Polynomial e c) Nat (Polynomial e c) :=
  ⟨exponentBySquaring polynoimal_mul .mul_id⟩

namespace Polynomial

def toString [Repr e] [Repr c]
    (p : Polynomial e c) :=
  match p.terms with
    | [] => "0"
    | [m] => m.toString
    | m :: ms => ms.foldl (init := m.toString)
      fun s t => s ++ " + " ++ Monomial.toString t

end Polynomial

instance [Repr e] [Repr c]
    : ToString (Polynomial e c) :=
  ⟨Polynomial.toString⟩

instance [Repr e] [Repr c]
    : Repr (Polynomial e c) :=
  ⟨fun p _ => p.toString⟩

instance
    [Repr e] [Repr c]
    : BEq (Polynomial e c) :=
  ⟨fun a b => a.toString == b.toString⟩

namespace Example

def _x₂ := poly [monomial (1, [(var "x", 2)])]
def _x₂z := poly [monomial (1, [(var "x", 2), (var "z", 1)])]
def _3xy₂ := poly [monomial (3, [(var "x", 1), (var "y", 2)])]

#eval [_x₂ + _x₂z, _3xy₂ + _x₂z, _x₂ + _x₂]
#eval (_x₂ + _x₂z) * (_3xy₂ + _x₂z + _x₂)

end Example
