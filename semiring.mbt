///|
pub trait Semiring: Add + Mul {
  zero() -> Self
  one() -> Self
  is_zero(self : Self) -> Bool
}

///|
pub trait Ring: Semiring {
  neg(self : Self) -> Self
  sub(self : Self, other : Self) -> Self
}

///|
pub trait Field: Ring {
  inv(self : Self) -> Self
}
