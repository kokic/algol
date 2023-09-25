

structure Mat2x2 (α : Type u) :=
  a₁₁ : α 
  a₁₂ : α 
  a₂₁ : α 
  a₂₂ : α 
deriving Repr 



-- class ConcreteMat.Mul (Mat : Type) where
  -- mul : Mat → Mat → Mat

class AddMul (α : Type u) where
  add : α → α → α
  mul : α → α → α
-- export RingOperator (add mul)


instance : AddMul Nat := ⟨.add, .mul⟩
instance : AddMul Int := ⟨.add, .mul⟩

def Mat2x2.mul [AddMul α] : Mat2x2 α → Mat2x2 α → Mat2x2 α := 
  let (add, mul) := (AddMul.add, AddMul.mul)
  λ A B => Mat2x2.mk 
    (add (mul A.a₁₁ B.a₁₁) (mul A.a₁₂ B.a₂₁))
    (add (mul A.a₁₁ B.a₁₂) (mul A.a₁₂ B.a₂₂))
    (add (mul A.a₂₁ B.a₁₁) (mul A.a₂₂ B.a₂₁))
    (add (mul A.a₂₁ B.a₁₂) (mul A.a₂₂ B.a₂₂))


instance [AddMul α] : Mul (Mat2x2 α) := ⟨.mul⟩


def Mat2x2.power [AddMul α] : Mat2x2 α → α → α → Nat → Mat2x2 α
  | _, zero, one, 0 => Mat2x2.mk one zero zero one
  | A, _, _, 1 => A
  | A, _, _, 2 => A * A
  | A, zero, one, n + 1 => A.mul (A.power zero one n)


def Mat2x2.powerNat : Mat2x2 Nat → Nat → Mat2x2 Nat
  | A, n => power A 0 1 n


-- def ConcreteMat.mul (A B : Mat2x2 α) := 



-- instance : ConcreteMat.Mul (Mat2x2 Nat) := ⟨λ A B => Mat2x2.mul A B Nat.add Nat.mul⟩
-- instance : ConcreteMat.Mul (Mat2x2 Int) := ⟨λ A B => Mat2x2.mul A B Int.add Int.mul⟩




-- def A := Mat2x2.mk 1 2 3 4
-- #eval A.mul A


def fracMat (p q : Nat) := Mat2x2.mk 0 1 p q
def fracMatReg (a : Nat) := Mat2x2.mk 0 1 1 a




namespace ContinuedFrac





partial def coeffByEuclidAux (p q : Nat) (xs : List Nat) :=
  if p < q then coeffByEuclidAux q p (0 :: xs) else
  match p % q with
    | 0 => xs ++ [p]
    | r => let a := (p - r) / q
           coeffByEuclidAux q r (xs ++ [a])

def coeffByEuclid (numerator denominator : Nat) := 
  coeffByEuclidAux numerator denominator []

  
def coeffByEuclidFinite (numerator denominator n : Nat) 
    := Id.run do
  let mut xs : List Nat := []
  let mut (p, q) := (numerator, denominator)
  for _ in [:n] do
    if p < q then (p, q) := (q, p); xs := 0 :: xs else
    match p % q with
      | 0 => return xs ++ [p]
      | r => let a := (p - r) / q
             (xs, p, q) := (xs ++ [a], q, r)
  xs

end ContinuedFrac



def UInt32.ε := 1 / UInt32.size.toFloat

def floatApproxUInt32 (value : Float) (n : Nat) := Id.run do
  let mut xs : List Nat := []
  let mut approx := value
  for _ in [:n] do
    let cᵢ := approx.floor
    xs := xs ++ [cᵢ.toUInt32.toNat]
    let diff := approx - cᵢ
    if diff < UInt32.ε then return xs
    approx := 1 / diff
  xs

open ContinuedFrac


-- #eval coeffByEuclid 111 19
-- #eval floatApproxUInt32 (111 / 19) 5


-- #eval (1./0).floor

-- #eval coeffByEuclid 19 43

-- #eval coeffByEuclidFinite 43 19 5
-- #eval coeffByEuclidFinite 19 43 5



-- #check (Mat2x2 Nat).mul⟩

-- def regularContinuedFrac

