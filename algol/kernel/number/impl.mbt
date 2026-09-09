

///|
pub impl Semiring for @rational.BigRational with fn zero() {
  @pythagoras.rat_zero
}

///|
pub impl Semiring for @rational.BigRational with fn one() -> @rational.BigRational {
  @pythagoras.rat_one
}

///|
pub impl Ring for @rational.BigRational with fn neg(
  self : @rational.BigRational,
) {
  -self
}

///|
pub impl Ring for @rational.BigRational with fn sub(
  self : @rational.BigRational,
  other : @rational.BigRational,
) -> @rational.BigRational {
  self - other
}

///|
pub impl Field for @rational.BigRational with fn inv(
  self : @rational.BigRational,
) {
  @rational.new(1N, 1).unwrap() / self
}

///|
pub impl ToUnicode for @rational.BigRational with fn unicodify(self) {
  Show::to_string(self)
}
