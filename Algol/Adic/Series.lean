

structure AdicNumberSeries :=
  adic : Nat
  data : List Nat
deriving Repr

namespace AdicNumberSeries

partial def fromNatAux (adic residue power : Nat) 
                       (data : List Nat) := 
  match residue with
    | 0 => data
    | r => 
      let base := adic ^ power
      let aᵢ := r % adic ^ (power + 1) / base
      let term := aᵢ * base
      fromNatAux adic (r - term) (power + 1) (data ++ [aᵢ])

def fromNat (adic n : Nat) : AdicNumberSeries := ⟨adic, fromNatAux adic n 0 []⟩


def toNatAux : Nat → List Nat → Nat → Nat
  | _, [], _ => 0
  | adic, x :: xs, n => 
    x * adic ^ n + toNatAux adic xs (n + 1)

def toNat' : Nat → List Nat → Nat 
  | adic, data => toNatAux adic data 0

def toNat (series : AdicNumberSeries) := 
  toNat' series.adic series.data


-- #eval toNat 
-- #eval toNat (fromNat 5 7)


-- #check List.range

-- √n i.e. x s.t. x² = a


-- none or x₀ s.t. xᵖ = a
def hasPowerSolution (a adic power : Nat) :=
  let rec aux : Nat → Option Nat 
    | 0 => none
    | dec + 1 => 
      let x := adic - dec
      if x ^ power % adic == a
      then some x else aux dec
  aux adic



-- #eval hasPowerSolution 2 7 2
-- #eval hasPowerSolution 4 5 3


def rootByEnum (a adic order n : Nat) := 
  let power_go (acc power : Nat) := 
    let rec aux : Nat → Option Nat
      | 0 => none -- no solution
      | dec + 1 =>
        let coeff := adic - dec
        let approx := acc + coeff * adic ^ power
        if approx ^ order % adic ^ (power + 1) == a 
        then some coeff else aux dec
    aux adic
  let rec coeff_go
    | 0 => []
    | n + 1 => 
      let xs := coeff_go n
      let acc := toNat' adic xs
      match power_go acc n with 
        | none => xs
        | some x => xs ++ [x]
  coeff_go n

-- #eval rootByEnum 2 7 2 5
-- #eval rootByEnum 5 7 2 5
-- #eval rootByEnum 4 5 3 5


--| a, adic, 1 => [power_go a adic 0 0]
-- def sqrtByEnum : Nat → Nat → Nat → List Nat
--   | _, _, 0 => []
--   | a, adic, n + 1 => 
--     let xs := sqrtByEnum a adic n
--     let acc := AdicNumberSeries.toNat' adic xs
--     xs ++ [power_go a adic acc n]
--   where 
--     power_go (a adic acc power : Nat) := 
--       let rec aux : Nat → Nat
--         | 0 => 0 -- no solution
--         | dec + 1 =>
--           let coeff := adic - dec
--           let approx := acc + coeff * adic ^ power
--           if approx ^ 2 % adic ^ (power + 1) == a 
--           then coeff else aux dec
--       aux adic


-- #eval sqrt 5 11 3





end AdicNumberSeries

