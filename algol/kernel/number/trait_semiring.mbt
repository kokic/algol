///|
pub trait Semiring: Add + Mul {
  fn zero() -> Self
  fn one() -> Self
  fn is_zero(self : Self) -> Bool
}
