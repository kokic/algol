
import Std.Data.HashMap
import Std.Data.HashSet
import Algol.Variable.Basic
import Algol.Class
import Algol.Pretty.Basic

open Std

abbrev Exponent (e : Type u) := e

abbrev Exponents e := HashMap String (Exponent e)

def displayTerm [Repr e]
    (key : String) (terms : Exponents e) :=
  match terms.get? key with
    | some term => prettyExp key (reprStr term)
    | none => ""

structure Monomial (e c :  Type u) where
  vars : HashSet Variable
  terms : Exponents e
  coefficient : c

namespace Monomial

def sortedVars (m : Monomial e c) :=
  m.vars.toArray.qsort Variable.lt

def toString [Repr e] [Repr c]
    (m : Monomial e c) : String :=
  prettyCoeff exponentTerm (reprStr m.coefficient)
where
  exponentTerm :=
    let arr := m.sortedVars.map
      fun x => displayTerm x.display m.terms
    arr.foldl (init := "") fun s t => s ++ t

def expLitEq [Repr e] [Repr c]
    (a b : Monomial e c) :=
  toString.exponentTerm a == toString.exponentTerm b

def exp (m : Monomial e c) (var : Variable) :=
  m.terms.get var.display

def exp? (m : Monomial e c) (var : Variable)
    : Option e :=
  m.terms.get? var.display

def exp_or_nil [HasNil e]
    (m : Monomial e c) (var : Variable) : e :=
  (m.exp? var).getD HasNil.nil

end Monomial

def var_exp_eq [BEq e]
    (a b : Monomial e c)
    (x y : Variable)
    (h : x.display ∈ a.terms ∧
         y.display ∈ b.terms) :=
  let aExp := a.exp x h.left
  let bExp := b.exp y h.right
  x == y && aExp == bExp

instance [Repr e] [Repr c]
    : ToString (Monomial e c) :=
  ⟨fun m => m.toString⟩

instance [Repr e] [Repr c]
    : Repr (Monomial e c) :=
  ⟨fun m _ => ToString.toString m⟩

instance [HasOne e] [HasOne c]
    : Coe Variable (Monomial e c) :=
  ⟨fun v => .mk
    (.ofList [v])
      (.insert (.emptyWithCapacity 4)
        v.display HasOne.one)
    HasOne.one⟩

def monomial [HasNil e] [BEq e]
    (data : c × List (Variable × e)) : Monomial e c := Id.run do
  let (coeff, terms) := data
  let terms := terms.filter (·.snd != HasNil.nil)
  let capacity := terms.length
  let α := HashSet Variable × HashMap String (Exponent e)
  let init : α := (.emptyWithCapacity capacity, .emptyWithCapacity capacity)
  let data := terms.foldl (init := init)
    fun (vars, map) (var, exp) =>
      (vars.insert var, map.insert
        var.display exp)
  .mk data.fst data.snd coeff

def Monomial.mul_id
    [BEq e] [HasNil e] [HasOne c]
    : Monomial e c :=
  monomial (HasOne.one, [(var "", HasNil.nil)])

-- (axᵐ)ⁿ → aⁿxᵐⁿ
def monomial_pow [HMul e Nat e] [HPow c Nat c]
    (m : Monomial e c)
    (n : Nat) : Monomial e c := Id.run do
  let vars := m.vars
  let mut terms := m.terms
  for v in vars do
    let key := v.display
    if h : key ∈ m.terms then
      let term := m.exp v h
      let term := term * n
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
        let term := aTerm + bTerm
        terms := terms.insert key term
      else
        terms := terms.insert key bTerm
  .mk (a.vars.insertMany b.vars) terms
      (a.coefficient * b.coefficient)

instance [HMul e Nat e] [HPow c Nat c]
    : HPow (Monomial e c) Nat (Monomial e c) :=
  ⟨monomial_pow⟩

instance [Add e] [Mul c]
    : Mul (Monomial e c) := ⟨monomial_mul⟩

namespace Example

def x : Monomial Nat Nat := var "x"
def _3 := monomial (3, [(var "x", 0)])
def _3x₂ := monomial (3, [(var "x", 2)])

def y : Monomial Nat Nat := var "y"

#eval y * monomial (3, [])
#eval (_3x₂, _3x₂^0)
#eval (x^3 * y^2, y^2 * x)
#eval x * x^2 * _3x₂

end Example
