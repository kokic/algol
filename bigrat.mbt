///|
pub let rat_zero : @rational.BigRational = @rational.new(0N, 1).unwrap()

///|
pub let rat_one : @rational.BigRational = @rational.new(1N, 1).unwrap()

///|
pub let rat_two : @rational.BigRational = @rational.new(2N, 1).unwrap()

///|
pub let rat_neg_one : @rational.BigRational = @rational.new(-1N, 1).unwrap()

///|
pub impl Semiring for @rational.BigRational with zero() {
  rat_zero
}

///|
pub impl Semiring for @rational.BigRational with one() -> @rational.BigRational {
  rat_one
}

///|
pub impl Semiring for @rational.BigRational with is_zero(
  self : @rational.BigRational,
) -> Bool {
  self == rat_zero
}

///|
pub impl Ring for @rational.BigRational with neg(self : @rational.BigRational) {
  -self
}

///|
pub impl Ring for @rational.BigRational with sub(
  self : @rational.BigRational,
  other : @rational.BigRational,
) -> @rational.BigRational {
  self - other
}

///|
pub impl Field for @rational.BigRational with inv(self : @rational.BigRational) {
  @rational.new(1N, 1).unwrap() / self
}

pub impl ToUnicode for @rational.BigRational with unicodify(self) {
  self.to_string()
}
