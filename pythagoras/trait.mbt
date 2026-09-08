///|
pub trait HasNil {
  fn nil() -> Self
}

///|
pub trait HasOne {
  fn one() -> Self
}

///|
pub trait Abs: HasNil + Neg + Compare {}

///|
pub trait Signum: HasNil + HasOne + Neg + Eq + Compare {}
