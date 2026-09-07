///|
pub fn[N : Compare] min(a : N, b : N) -> N {
  guard a < b else { b }
  a
}

///|
pub fn[N : Abs] abs(x : N) -> N {
  guard x < HasNil::nil() else { x }
  -x
}

///|
pub fn[N : Signum] signum(a : N) -> N {
  guard a != HasNil::nil() else { HasNil::nil() }
  guard a < HasNil::nil() else { HasOne::one() }
  Neg::neg(HasOne::one())
}

///|
pub fn[N : Signum + Mod] gcd(a : N, b : N) -> N {
  for a = a, b = b {
    if signum(b) == HasNil::nil() {
      break if a < HasNil::nil() { -a } else { a }
    }
    continue b, a % b
  }
}
