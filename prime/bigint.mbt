///|
#inline
pub fn is_even(n : BigInt) -> Bool {
  (
  // Fixed in Jan 9, 2026: 
  //   `n & 1` panic for n is large even (≥ 2^32)
  // Also see: https://github.com/moonbitlang/core/issues/3133 
    n & 1
  ) ==
  0
}

///|
#inline
pub fn is_odd(n : BigInt) -> Bool {
  !is_even(n)
}

///|
pub fn euclid_mod(a : BigInt, n : BigInt) -> BigInt {
  let r = a.mod(n)
  guard r < 0 else { r }
  r + n
}

///|
pub fn abs(x : BigInt) -> BigInt {
  guard x < 0 else { x }
  -x
}
