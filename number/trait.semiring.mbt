///|
pub trait Semiring: Add + Mul {
  zero() -> Self
  one() -> Self
  is_zero(self : Self) -> Bool
}
