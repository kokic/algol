///|
pub trait Semiring: Add + Mul + Eq {
  fn zero() -> Self
  fn one() -> Self
}

///|
pub trait Ring: Semiring {
  fn neg(self : Self) -> Self
  fn sub(self : Self, other : Self) -> Self
}

///|
pub trait EuclideanDomain: Ring + Eq {
  fn abs(self : Self) -> Self
  fn signum(self : Self) -> Int
  fn compare(self : Self, other : Self) -> Int
  fn div_mod_euclid(self : Self, divisor : Self) -> (Self, Self)
  fn extended_gcd(a : Self, b : Self) -> (Self, Self, Self)
}

///|
pub trait Field: Ring {
  fn inv(self : Self) -> Self
}
