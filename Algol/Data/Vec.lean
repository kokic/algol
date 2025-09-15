/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic (@kokic)
-/
import Algol.Class

def Vec.dot [HasNil α] [Add α] [Mul α] (xs ys : List α) :=
  xs |>.zipWith Mul.mul ys
     |>.foldl Add.add HasNil.nil
