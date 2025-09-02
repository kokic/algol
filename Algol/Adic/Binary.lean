/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
partial def natToBinary (n : Nat) : String :=
  ite (n == 0) "0" (go n "")
where
  go (n : Nat) (acc : String) : String :=
    ite (n == 0) acc (go (n / 2)
        (ite (n % 2 == 0) "0" "1" ++ acc))

def binaryToNat (binary : String) : Option Nat :=
  binary.foldl (init := some 0) fun acc c =>
    match acc, c with
    | none, _ => none
    | some n, '0' => some (2 * n)
    | some n, '1' => some (2 * n + 1)
    | _, _ => none
