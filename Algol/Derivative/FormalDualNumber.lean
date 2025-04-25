
import Algol.Class

def DualNumber (R : Type u) := R × R

namespace DualNumber

def toString [Repr R] (a : DualNumber R) :=
  reprStr (a.fst, a.snd)

end DualNumber

instance [Repr R] : ToString (DualNumber R) :=
  ⟨DualNumber.toString⟩

instance [Repr R] : Repr (DualNumber R) :=
  ⟨fun a _ => DualNumber.toString a⟩

def dual [HasOne R] (a : R) : DualNumber R :=
  (a, HasOne.one)

instance [Neg R] : Neg (DualNumber R) :=
  ⟨fun ⟨a, b⟩ => ⟨-a, -b⟩⟩

instance [Add R] : Add (DualNumber R) :=
  ⟨fun ⟨a, b⟩ ⟨c, d⟩ => ⟨a + c, b + d⟩⟩

instance [Sub R] : Sub (DualNumber R) :=
  ⟨fun ⟨a, b⟩ ⟨c, d⟩ => ⟨a - c, b - d⟩⟩

instance [Mul R] [Add R]
    : Mul (DualNumber R) :=
  ⟨fun ⟨u, u'⟩ ⟨v, v'⟩ => ⟨u * v, u' * v + u * v'⟩⟩

instance
    [Div R] [Mul R] [Sub R] [Pow R R] [OfNat R 2]
    : Div (DualNumber R) :=
  ⟨fun ⟨a, b⟩ ⟨c, d⟩ =>
       ⟨a / c, (b * c - a * d) / (c * c)⟩⟩
