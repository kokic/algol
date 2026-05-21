///|
pub trait EuclideanDomain: Ring + Eq {
  abs(self : Self) -> Self
  signum(self : Self) -> Int
  compare(self : Self, other : Self) -> Int
  div_mod_euclid(self : Self, divisor : Self) -> (Self, Self)
  extended_gcd(a : Self, b : Self) -> (Self, Self, Self)
}
