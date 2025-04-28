/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic
-/
import Algol.Mac.List

inductive Tree where
  | nil : Tree
  | node : Tree → Tree → Tree
deriving Repr, Inhabited

def pt := Tree.node .nil .nil

partial def allTrees : Nat → List Tree
  | 0 => [.nil]
  | n => [
      .node lchild rchild
      | for i ← List.range n
      , for lchild ← allTrees i
      , for rchild ← allTrees (n - 1 - i)
    ]

partial def allTreesLessEq : Nat → List Tree
  | 0 => [.nil]
  | n => [
    .node lchild rchild
      | for i ← List.range n
      , for j ← List.range n
      , for lchild ← allTrees i
      , for rchild ← allTrees j
    ]
