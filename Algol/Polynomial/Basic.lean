
import Std.Data.HashSet
import Algol.Variable.Basic
import Algol.Monomial.Basic
import Algol.Abbrev

open Std

/-- collect their variables from a series of monomials. -/
def vars (xs : List (Monomial e c))
               (capacity : Nat := 3)
    : HashSet Variable  :=
  xs.foldl (init := (.emptyWithCapacity capacity))
    fun s t => s.insertMany t.vars

/-- comparing the lexicographic order (`y < x`) and exponentiation of variables. -/
def monomial_lt [Repr e] [Repr c] [LT e] [DecidableLT e]
    (a b : Monomial e c) : Bool :=
  ite (a.vars.size < b.vars.size) true
      (ite (a.vars.size > b.vars.size) false
           (lt_impl
             a.sortedVars.toList
             b.sortedVars.toList))
where
  lt_impl : List Variable → List Variable → Bool
    | [], _ => true
    | _, [] => false
    | x :: _, y :: _ =>
      dite (x.display ∈ a.terms ∧
            y.display ∈ b.terms)
        (fun h => lex_exp_order
          (x.gt y)
          (a.exp x h.left < b.exp y h.right))
        (fun _ => false)
  lex_exp_order : Bool → Bool → Bool
    | false, false => false
    | _, _ => true

/-- comparing the lexicographic order (`x > y`) and exponentiation of variables. -/
def monomial_gt [Repr e] [Repr c] [LT e] [DecidableLT e]
    (a b : Monomial e c) : Bool :=
  monomial_lt b a

structure Polynomial (e c : Type u) where
  vars : HashSet Variable
  terms : List (Monomial e c)

instance [Repr e] [Repr c]
    : ToString (Polynomial e c) :=
  ⟨fun p => match p.terms with
    | [] => ""
    | [m] => m.toString
    | m :: ms => ms.foldl (init := m.toString)
      fun s t => s!"{s} + {t.toString}"⟩

def poly (data : List (Monomial e c))
    : Polynomial e c :=
  let allVars := vars data
  .mk allVars data

def polynomial_add [Repr e] [Repr c] [LT e] [DecidableLT e][HasZero c] [BEq c] [Add c]
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
      ite (coeff_sum == HasZero.zero)
          (add_impl xs ys)
          ((.mk x.vars x.terms coeff_sum) :: add_impl xs ys)

instance [Repr e] [Repr c] [LT e] [DecidableLT e][HasZero c] [BEq c] [Add c]
    : Add (Polynomial e c) := ⟨polynomial_add⟩

namespace Polynomial

end Polynomial

def _x₂ := poly [monomial (1, [(var "x", 2)])]
def _x₂z := poly [monomial (1, [(var "x", 2), (var "z", 1)])]
def _3xy₂ := poly [monomial (3, [(var "x", 1), (var "y", 2)])]

#eval [_x₂ + _x₂z, _3xy₂ + _x₂z, _x₂ + _x₂]
