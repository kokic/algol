/-
Copyright (c) 2025 Algol Project. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: kokic
-/
import Algol.Data.BinaryTreeType

inductive Ins where
  | lchild
  | rchild
  | up
deriving Repr

def toCode (t : Tree) : String := Id.run do
  let mut stack : List (Tree × Ins) := [(t, Ins.lchild)]
  let mut result : String := ""
  repeat do
    let head := stack.head?
    if h : head.isSome then
      let (tree, action) := head.get h
      stack := stack.tail!
      match tree with
      | .nil => continue
      | .node l r =>
        match action with
        | .lchild => (result, stack) := (result ++ "1", update tree l r stack)
        | .rchild => (result, stack) := (result ++ "01", update tree l r stack)
        | .up => result := result ++ "0"
    else
      break
  result
where
  update (tree l r : Tree)
         (stack : List (Tree × Ins)) :=
    (l, Ins.lchild) :: (r, Ins.rchild)
                    :: (tree, Ins.up) :: stack

def minimizeCode (s : String) :=
  match s.revFind fun c => c != '0' with
  | some pos => s.extract 0 ⟨pos.byteIdx + 1⟩
  | _ => s
