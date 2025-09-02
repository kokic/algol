import Algol.Polynomial.Basic

def var_q := var "q"
def q := poly [monomial (1, [(var_q, 1)])]

instance : OfNat (Polynomial Nat Nat) n :=
  ⟨ite (n == 0) .zero (poly [monomial (n, [(var_q, 0)])])⟩

instance : HasOne (Polynomial Nat Nat) := ⟨1⟩
