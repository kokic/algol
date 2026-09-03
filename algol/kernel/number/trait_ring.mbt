///|
pub trait Ring: Semiring {
  fn neg(self : Self) -> Self
  fn sub(self : Self, other : Self) -> Self
}
