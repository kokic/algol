
class HasNil (α : Type u) where
  nil : α

class HasOne (α : Type u) where
  one : α

instance : HasNil Nat := ⟨0⟩
instance : HasOne Nat := ⟨1⟩
