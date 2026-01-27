///|
pub trait Ring: Semiring {
  neg(self : Self) -> Self
  sub(self : Self, other : Self) -> Self
}
