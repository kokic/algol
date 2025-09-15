/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Generic.StarSemiring
import Algol.Data.Vec

inductive Matrix (a : Type) where
  | matrix : List (List a) → Matrix a
  | scalar : a → Matrix a
deriving Repr

namespace Matrix

def uniformize : Matrix a → List (List a)
  | .scalar x => [[x]]
  | .matrix x => x

def BlockMatrix a :=
  Matrix a × Matrix a ×
  Matrix a × Matrix a

instance [Inhabited a] : Inhabited (BlockMatrix a) :=
  ⟨(.scalar default, .scalar default, .scalar default, .scalar default)⟩


def join : BlockMatrix a → Matrix a
  | (a, b, c, d) =>
    .matrix (hcat a.uniformize b.uniformize ++ hcat c.uniformize d.uniformize)
where
  hcat (xs ys : List (List a)) :=
    List.zipWith List.append xs ys

def map (f : α → β) : Matrix α → Matrix β
  | m => .matrix ((uniformize m).map (λ ys => ys.map f))

def split [Inhabited a] (matrix : Matrix a) : Option (BlockMatrix a) :=
  match matrix with
    | .matrix (row :: rows) =>
      let (fst, top) := (row.head!, row.tail!)
      let (left, rest) := (rows.map
        (λ | [] => ([], [])
           | (x :: xs) => ([x], xs))).unzip
      some (.matrix [[fst]], .matrix [top],
       .matrix left   , .matrix rest)
    | _ => none

/--
Usage:
```
matrix [[1, 2], [3, 4]] |>split! |>.join
```
-/
def split! [Inhabited a] [Inhabited (BlockMatrix a)] (matrix : Matrix a) : BlockMatrix a :=
  split matrix |>.get!

/--
Usage:
```
build (λ i j => if i == j then 1 else 0) 3 3
-- .matrix [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
```
-/
def build (f : Nat → Nat → α) (m n : Nat) : Matrix α :=
  .matrix data
where
  data :=
    let (rm, rn) := (List.range m, List.range n)
    rm.map (λ i => rn.map (λ j => f i j))

def column [Inhabited α] (xs : List (List α)) (i : Nat) :=
  xs.map fun row => row[i]!

partial def add [Semiring α] [Inhabited α]
    : Matrix α → Matrix α → Matrix α
  | a, b => case_mat_mat a.uniformize b.uniformize
where
  case_mat_mat (a b : List (List α)) :=
    .matrix (.zipWith (.zipWith Semiring.add) a b)

def mul [HasNil α] [Add α] [Mul α] [Inhabited α]
    : Matrix α → Matrix α → Matrix α
  | a, b =>
    let (a, b) := (a.uniformize, b.uniformize)
    let (m, n) := (a.length, b.head!.length)
    Matrix.build (λ i j => Vec.dot (a[i]!) (column b j)) m n
where
  case_scal_mat (a : α) (b : List (List α)) :=
    Matrix.matrix (b.map (λ xs => xs.map (λ x => a * x)))

end Matrix
