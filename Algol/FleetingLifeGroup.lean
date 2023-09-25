

partial def factorial : Nat → Nat
  | 0 | 1 => 1
  | n => n * factorial (n - 1)

-- #eval factorial 5


partial def calculateFValueAux (n m : Nat) := 
  if factorial m % n == 0 then m else calculateFValueAux n (m + 1)

-- the min m s.t. n | m!
-- n | m! i.e. m! % n == 0
def calculateFValue : Nat → Nat  
  | 0 | 1 => 0
  | n => calculateFValueAux n 2

-- #eval calculateFValue 8


