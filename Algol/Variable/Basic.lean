
structure Variable where
  display: String
deriving BEq, Inhabited, Hashable

instance : Repr Variable := ⟨fun v _ => v.display⟩

def var : String → Variable := Variable.mk
