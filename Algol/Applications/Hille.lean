/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic
-/
import Algol.Polynomial.Basic
import Algol.Derivative.DualNumber
import Algol.Adic.Binary
import Algol.Data.BinaryTreeCode

def var_q := var "q"
def q := poly [monomial (1, [(var_q, 1)])]

instance : OfNat (Polynomial Nat Nat) n :=
  ⟨poly [monomial (n, [(var_q, 0)])]⟩

instance : BEq (Polynomial Nat Nat) :=
  ⟨fun a b => a.toString == b.toString⟩

instance : HasNil (Polynomial Nat Nat) := ⟨0⟩
instance : HasOne (Polynomial Nat Nat) := ⟨1⟩

instance
    [OfNat R n] [HasNil R]
    : OfNat (DualNumber R) n :=
  ⟨(OfNat.ofNat n, HasNil.nil)⟩

instance
    : HMul Nat (Polynomial Nat Nat)
               (Polynomial Nat Nat)
  := ⟨fun n p => OfNat.ofNat n * p⟩

instance
    [HPow R Nat R] [HMul Nat R R] [Mul R]
    : HPow (DualNumber R) Nat (DualNumber R) :=
  ⟨fun ⟨u, u'⟩ n => ⟨u ^ n, n * u ^ (n - 1) * u'⟩⟩

instance
    [HMul Nat R R]
    : HMul Nat (DualNumber R) (DualNumber R)
  := ⟨fun n ⟨u, u'⟩ => ⟨n * u, n * u'⟩⟩

def dual_q := dual (poly [monomial (1, [(var_q, 1)])])

def lift (p : Polynomial Nat Nat)
         (v : Variable := var_q)
    : DualNumber (Polynomial Nat Nat) :=
  let terms := p.terms.map fun m =>
    let c : Nat := m.coefficient
    let n : Nat := m.exp_or_nil v
    c * dual_q ^ n
  terms.foldl (init := HasNil.nil) fun s t => s + t

def poly2 (s : String) := Id.run do
  let mut p := poly [monomial (0, [(var_q, 0)])]
  let mut n := s.length - 1
  for c in s.toList do
    if c != '0' then
      p := p + q ^ n
    n := n - 1
  p

def formula_dif (p : Polynomial Nat Nat)
                (v : Variable := var_q) :=
  (lift p v).snd

def ev (p : Polynomial Nat Nat) : Nat :=
  let data := p.terms.map
    fun m => (m.coefficient, m.exp_or_nil var_q)
  go data 0
where
  go : List (Nat × Nat) -> Nat → Nat
    | [], n => n
    | (c, e) :: xs, n => go xs (n + c * 2 ^ e)

def ev2 (p : Polynomial Nat Nat) : String :=
  minimizeCode (natToBinary (ev p))

def dif (p : Polynomial Nat Nat)
        (v : Variable := var_q) :=
  poly2 (ev2 (formula_dif p v))

partial def chain (p : Polynomial Nat Nat)
                  (v : Variable := var_q)
    : List (Polynomial Nat Nat) :=
  derived [p]
where
  derived (xs : List (Polynomial Nat Nat)) :=
    match xs.getLast? with
      | none => xs
      | some e => ite (terminal e.toString) xs
                      (derived (xs ++ [dif e v]))
  terminal (s : String) :=
    !s.contains 'q' || s == "q" || s == "1" || s == "0"

def chain2 (p : Polynomial Nat Nat)
           (v : Variable := var_q) :=
  (chain p v).map fun x => (x, ev2 x)

#eval println! chain2 (poly2 "1110001")

def trees4 := (allTrees 4).map (minimizeCode ∘ toCode)
def chain4 := trees4.map (chain2 ∘ poly2)

#eval println! chain4.foldl (init := "")
  fun s t => s ++ t.toString ++ "\n"
