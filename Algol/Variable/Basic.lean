
structure Variable where
  display: String
deriving DecidableEq, BEq, Inhabited, Hashable

instance : ToString Variable := ⟨fun v => v.display⟩
instance : Repr Variable := ⟨fun v _ => v.display⟩

def var : String → Variable := Variable.mk

namespace Variable

def lt : Variable → Variable → Bool :=
  fun x y => x.display < y.display

def le : Variable → Variable → Bool :=
  fun x y => x.lt y || x == y

def gt : Variable → Variable → Bool :=
  fun x y => lt y x

def ge : Variable → Variable → Bool :=
  fun x y => le y x

end Variable
