///|
pub fn min(a : BigInt, b : BigInt) -> BigInt {
  if a < b {
    a
  } else {
    b
  }
}

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
      break a
    }
    continue b, a % b
  }
}

///|
pub fn random(rand : @random.Rand, limit : BigInt) -> BigInt {
  guard !(limit == 1) else { return 0 }
  let k = limit.bit_length()
  let two_pow_k = (1 : BigInt) << k
  let threshold = two_pow_k - two_pow_k % limit
  while true {
    let x = rand.bigint(k)
    if x < threshold {
      return x % limit
    }
  }
  0 // unreachable!
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
pub impl Semiring for BigInt with zero() {
  0N
}

///|
pub impl Semiring for BigInt with one() {
  1N
}

///|
pub impl Semiring for BigInt with is_zero(self) {
  self.is_zero()
}

///|
pub impl ToUnicode for BigInt with unicodify(self) {
  self.to_string()
}
