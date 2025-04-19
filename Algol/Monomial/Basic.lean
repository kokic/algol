
import Std.Data.HashMap
import Std.Data.HashSet
import Algol.Variable.Basic
import Algol.Class
import Algol.Pretty.Basic

open Std

structure Term (e : Type u) where
  exponent: e

abbrev Terms e := HashMap String (Term e)

structure Monomial (e c :  Type u) where
  vars : HashSet Variable
  terms : Terms e
  coefficient: c

def displayTerm [Repr e]
    (key : String) (terms : HashMap String (Term e)) :=
  match terms.get? key with
    | some term => prettyExp key (reprStr term.exponent)
    | none => ""

namespace Monomial

def toString [Repr e] [Repr c]
    (m : Monomial e c) : String :=
  let term := m.vars.toList
    |>.map (fun x => displayTerm x.display m.terms)
    |>.foldl (init := "") fun s t => s ++ t
  prettyCoeff term (reprStr m.coefficient)

end Monomial

instance [Repr e] [Repr c] : ToString (Monomial e c) :=
  ⟨fun m => m.toString⟩

instance [Repr e] [Repr c] : Repr (Monomial e c) :=
  ⟨fun m _ => ToString.toString m⟩

instance [HasOne e] [HasOne c] : Coe Variable (Monomial e c) :=
  ⟨fun v => .mk
    (.ofList [v])
      (.insert (.emptyWithCapacity 4) v.display
        (.mk HasOne.one))
    HasOne.one⟩

def monomial [HasZero e] [BEq e]
    (data : c × List (Variable × e)) : Monomial e c := Id.run do
  let (coeff, terms) := data
  let terms := terms.filter (·.snd == HasZero.zero)
  let capacity := terms.length
  let α := HashSet Variable × HashMap String (Term e)
  let init : α := (.emptyWithCapacity capacity, .emptyWithCapacity capacity)
  let data := terms.foldl (init := init)
    fun (vars, map) (var, exp) =>
      (vars.insert var, map.insert var.display (.mk exp))
  .mk data.fst data.snd coeff

-- (axᵐ)ⁿ → aⁿxᵐⁿ
def monomial_pow [HMul e Nat e] [HPow c Nat c]
    (m : Monomial e c)
    (n : Nat) : Monomial e c := Id.run do
  let vars := m.vars
  let mut terms := m.terms
  for v in vars do
    let key := v.display
    if h : key ∈ m.terms then
      let term := m.terms.get v.display h
      let term := Term.mk (term.exponent * n)
      terms := terms.insert key term
  .mk vars terms (m.coefficient ^ n)

-- axᵐ * byⁿ → abxᵐyⁿ
def monomial_mul [Add e] [Mul c]
    (a : Monomial e c)
    (b : Monomial e c) : Monomial e c := Id.run do
  let mut terms := a.terms
  for bv in b.vars do
    let key := bv.display
    if hb : key ∈ b.terms then
      let bTerm := b.terms.get key hb
      if ha : key ∈ a.terms then
        let aTerm := a.terms.get key ha
        let term := Term.mk
          (aTerm.exponent + bTerm.exponent)
        terms := terms.insert key term
      else
        terms := terms.insert key bTerm
  .mk (a.vars.insertMany b.vars) terms
      (a.coefficient * b.coefficient)

instance [HMul e Nat e] [HPow c Nat c] :
    HPow (Monomial e c) Nat (Monomial e c) :=
  ⟨monomial_pow⟩

instance [Add e] [Mul c] : Mul (Monomial e c) :=
  ⟨monomial_mul⟩

instance : HasZero Nat := ⟨0⟩
instance : HasOne Nat := ⟨1⟩

namespace Example

def x : Monomial Nat Nat := var "x"
def _3 := monomial (3, [(var "x", 0)])
def _3x₂ := monomial (3, [(var "x", 2)])

def y : Monomial Nat Nat := var "y"

#eval y * monomial (3, [])
#eval _3x₂^0
#eval x^3 * y^2
#eval x * x^2 * _3x₂

end Example
