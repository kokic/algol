///|
pub trait Semiring: Add + Mul + Eq {
  fn zero() -> Self
  fn one() -> Self
  // fn is_zero(self : Self) -> Bool
}
