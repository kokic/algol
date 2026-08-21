///|
/// Also see: https://github.com/moonbitlang/x/blob/9cd244e9dc63d3fe253d767d9932a4d9bc27d10d/decimal/decimal.mbt#L452-L460
pub fn signum(a : BigInt) -> Int {
  if a.is_zero() {
    0
  } else if a > 0 {
    1
  } else {
    -1
  }
}

///|
/// Also see: https://github.com/moonbitlang/x/blob/9cd244e9dc63d3fe253d767d9932a4d9bc27d10d/rational/rational.mbt#L19-L26
pub fn gcd(a : BigInt, b : BigInt) -> BigInt {
  for a = a, b = b {
    if signum(b) == 0 {
      break if a < 0N { -a } else { a }
    }
    continue b, a % b
  }
}

///|
pub fn random(rand : @random.Rand, limit : BigInt) -> BigInt {
  // limit <= 1 means the range [0, limit) is empty or degenerate
  guard limit > 1 else { return 0 }
  let k = limit.bit_length()
  let two_pow_k = (1 : BigInt) << k
  let threshold = two_pow_k - two_pow_k % limit
  let mut x = rand.bigint(k)
  while x >= threshold {
    x = rand.bigint(k)
  }
  x % limit
}

///|
pub fn random_range(
  rand : @random.Rand,
  inclusive : BigInt,
  exclusive : BigInt,
) -> BigInt {
  inclusive + random(rand, exclusive - inclusive)
}

///|
pub impl Semiring for BigInt with fn zero() {
  0N
}

///|
pub impl Semiring for BigInt with fn one() {
  1N
}

///|
pub impl Semiring for BigInt with fn is_zero(self) {
  self.is_zero()
}

///|
pub impl Ring for BigInt with fn neg(self : BigInt) -> BigInt {
  -self
}

///|
pub impl Ring for BigInt with fn sub(self : BigInt, other : BigInt) -> BigInt {
  self - other
}

///|
pub impl EuclideanDomain for BigInt with fn abs(self : BigInt) -> BigInt {
  if self < 0N {
    -self
  } else {
    self
  }
}

///|
pub impl EuclideanDomain for BigInt with fn signum(self : BigInt) -> Int {
  signum(self)
}

///|
pub impl EuclideanDomain for BigInt with fn compare(
  self : BigInt,
  other : BigInt,
) -> Int {
  self.compare(other)
}

///|
pub impl EuclideanDomain for BigInt with fn div_mod_euclid(
  self : BigInt,
  divisor : BigInt,
) -> (BigInt, BigInt) {
  let b = if divisor < 0N { -divisor } else { divisor }
  let mut q = self / b
  let mut r = self % b
  if r < 0N {
    q = q - 1
    r = r + b
  }
  if divisor < 0N {
    q = -q
  }
  (q, r)
}

///|
pub impl EuclideanDomain for BigInt with fn extended_gcd(a : BigInt, b : BigInt) -> (
  BigInt,
  BigInt,
  BigInt,
) {
  let mut old_r = a
  let mut r = b
  let mut old_s = 1N
  let mut s = 0N
  let mut old_t = 0N
  let mut t = 1N
  while !r.is_zero() {
    let (q, next_r) = EuclideanDomain::div_mod_euclid(old_r, r)
    let next_s = old_s - q * s
    let next_t = old_t - q * t
    old_r = r
    r = next_r
    old_s = s
    s = next_s
    old_t = t
    t = next_t
  }
  if old_r < 0N {
    (-old_r, -old_s, -old_t)
  } else {
    (old_r, old_s, old_t)
  }
}

///|
pub impl ToUnicode for BigInt with fn unicodify(self) {
  self.to_string()
}
