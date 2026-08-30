///|
/// Decompose $n$ to $(d, s)$ s.t. $n = d \cdot 2^s$ where $d$ is odd
fn decompose_ds(n : BigInt) -> (BigInt, Int) {
  let mut d = n
  let mut s = 0
  while is_even(d) {
    d = d / 2
    s = s + 1
  }
  (d, s)
}

///|
/// Decompose $n-1$ to $(d, s)$ s.t. $n - 1 = d \cdot 2^s$ where $d$ is odd
fn decompose_minus_one(n : BigInt) -> (BigInt, Int) {
  decompose_ds(n - 1)
}

///|
/// Decompose $n+1$ to $(d, s)$ s.t. $n + 1 = d \cdot 2^s$ where $d$ is odd
fn decompose_plus_one(n : BigInt) -> (BigInt, Int) {
  decompose_ds(n + 1)
}

///|
test "decompose (d, s)" {
  // 57 - 1 = 7 × 2^3
  @debug.assert_eq(decompose_minus_one(57), (7, 3))

  // 55 + 1 = 7 × 2^3
  @debug.assert_eq(decompose_plus_one(55), (7, 3))
}

///|
fn miller_rabin_witness(a : BigInt, n : BigInt) -> Bool {
  guard !a.mod(n).is_zero() else { false }

  // a ≠ 0 (mod n)
  let n_minus_one = n - 1
  let (d, s) = decompose_minus_one(n)
  let x_base = a.pow(d, modulus=n)
  guard !(x_base == 1 || x_base == n_minus_one) else { false }

  // `x_base` ≠ 0 && `x_base` ≠ n - 1
  let mut x = x_base
  for i = 1; i < s; i = i + 1 {
    x = x * x % n
    if x == n_minus_one {
      return false
    }
    if x == 1 {
      return true
    }
  }
  true
}

///|
fn miller_rabin(n : BigInt, bases : Array[Int]) -> Bool {
  guard n >= 2 else { false }
  guard !(n == 2 || n == 3) else { true }
  guard is_odd(n) else { false }
  for base in bases {
    if miller_rabin_witness(BigInt::from_int(base), n) {
      return false
    }
  }
  true
}

///|
test "Miller Rabin" {
  let miller_rabin = (n : BigInt) => miller_rabin(n, [2])
  assert_true(miller_rabin(2017))
  assert_true(miller_rabin(2027))
  assert_true(!miller_rabin(9999))
  assert_true(miller_rabin(1919802127))
  assert_true(miller_rabin(626290517014499))
  assert_true(miller_rabin(83318820587408930933))
}

///|
/// Also see: https://en.wikipedia.org/wiki/Jacobi_symbol#Primality_testing
pub fn jacobi(a_base : BigInt, n_base : BigInt) -> Int raise {
  assert_true(n_base > 0 && is_odd(n_base), msg="n > 0 && n is odd: \{n_base}")
  let mut a = euclid_mod(a_base, n_base)
  let mut n = n_base
  let mut t = 1
  while !a.is_zero() {
    while (a % 2).is_zero() {
      a = a / 2
      let n_mod_8 = n.mod(8)
      if n_mod_8 == 3 || n_mod_8 == 5 {
        t = -t
      }
    }

    // swap a, n
    let temp = a
    a = n
    n = temp

    // a = n = 3 (mod 4)
    if a.mod(4) == 3 && n.mod(4) == 3 {
      t = -t
    }
    a = a.mod(n)
  }
  guard n == 1 else { 0 }
  t
}

///|
test "Jacobi symbol" {
  assert_eq(jacobi(-1, 3), -1)
  assert_eq(jacobi(0, 9), 0)
  assert_eq(jacobi(2, 5), -1)
  assert_eq(jacobi(5, 11), 1)
  assert_eq(jacobi(5, 21), 1)
  assert_eq(jacobi(8, 21), -1)
  assert_eq(jacobi(19, 45), 1)
}

///|
fn half_mod(x : BigInt, n : BigInt) -> BigInt {
  guard is_even(x) else { ((x + n) / 2).mod(n) }
  (x / 2).mod(n)
}

///|
fn lucas_uvqk(
  p : BigInt,
  q : BigInt,
  d : BigInt,
  n : BigInt,
) -> (BigInt, BigInt, BigInt) {
  let mut u : BigInt = 0
  let mut v : BigInt = 2
  let mut qk : BigInt = 1
  let d_val = p * p - 4 * q
  let mut bit = d.bit_length() - 1
  while bit >= 0 {
    let u2 = euclid_mod(u * v, n)
    let v2 = euclid_mod(v * v - qk * 2, n)
    u = u2
    v = v2
    qk = euclid_mod(qk * qk, n)
    if !((d >> bit) & 1).is_zero() {
      let u_old = u
      let v_old = v
      u = half_mod(u_old + v_old, n)
      v = half_mod(v_old + d_val * u_old, n)
      qk = euclid_mod(qk * q, n)
    }
    bit = bit - 1
  }
  (u, v, qk)
}

///|
fn strong_lucas_selfridge(n : BigInt) -> Bool raise {
  // step 1: pick `jacobi_d`
  let mut jacobi_d : BigInt = 5
  let mut sign = 1
  while true {
    let j = jacobi(jacobi_d, n)
    if j == -1 {
      break
    }
    if j == 0 {
      return false
    }
    let abs_d = abs(jacobi_d) + 2
    sign = -sign
    jacobi_d = if sign == 1 { abs_d } else { -abs_d }
  }

  // Selfridge: p = 1, q = (1-d)/4
  let p : BigInt = 1
  let q = ((1 : BigInt) - jacobi_d) / 4

  // step 2: n + 1 = d \cdot 2^s
  let (d, s) = decompose_plus_one(n)

  // step 3: U_d, V_d, Q^d (mod n)
  let (u, v, qk) = lucas_uvqk(p, q, d, n)

  // step 4: strong condition
  guard euclid_mod(u, n) != 0 else { true }
  let mut v_mod = euclid_mod(v, n)
  guard v_mod != 0 else { true }
  let mut q_mod = euclid_mod(qk, n)
  for r = 1; r < s; r = r + 1 {
    v_mod = euclid_mod(v_mod * v_mod - q_mod * 2, n)
    q_mod = euclid_mod(q_mod * q_mod, n)
    if v_mod.is_zero() {
      return true
    }
  }
  false
}

///|
fn is_perfect_square(n : BigInt) -> Bool {
  guard n >= 0 else { false }
  guard n >= 2 else { true }
  let r = n.mod(16)
  guard r == 0 || r == 1 || r == 4 || r == 9 else { false }

  let mut x = (1 : BigInt) << ((n.bit_length() + 1) / 2)
  let mut y = (x + n / x) / 2
  while y < x {
    x = y
    y = (x + n / x) / 2
  }
  x * x == n
}

///|
/// Also see: @math.is_probable_prime
pub fn is_probable_prime_bpsw(n : BigInt) -> Bool {
  guard n >= 2 else { false }
  guard !(n == 2 || n == 3) else { true }
  guard is_odd(n) else { false }
  for p in trial_division_primes {
    let p_big = BigInt::from_int(p)
    if n == p_big {
      return true
    }
    if n.mod(p_big).is_zero() {
      return false
    }
  }
  guard !is_perfect_square(n) else { false }
  guard miller_rabin(n, [2]) else { false }
  try! strong_lucas_selfridge(n)
}

///|
test "BPSW: is probable prime" {
  assert_true(is_probable_prime_bpsw(115792125365684818504981760053))
}

///|
test "BPSW: rejects composites" {
  assert_true(!is_probable_prime_bpsw(1000006000009))
  assert_true(!is_probable_prime_bpsw(3215031751))
  assert_true(!is_probable_prime_bpsw(3825123056546413051))
}

///|
test "perfect square detection" {
  let square = BigInt::from_string("1000006000009")
  assert_true(is_perfect_square(square))
  assert_true(!is_perfect_square(square - 1))
  assert_true(!is_perfect_square(square + 1))
}

///|
let trial_division_primes : Array[Int] = [
  3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
]

///|
/// The function calls `is_small_prime` (n < 10000) or `is_probable_prime_bpsw` (otherwise)
/// 
/// ```mbt check
/// test "is_prime boundary regression" {
///   assert_true(!is_prime(-1))
///   assert_true(!is_prime(0))
///   assert_true(!is_prime(1))
///   assert_true(is_prime(2))
///   assert_true(is_prime(3))
///   assert_true(is_prime(5381))
///   assert_true(is_prime(5387))
///   assert_true(is_prime(9973))
///   assert_true(!is_prime(9999))
///   assert_true(!is_prime(10000))
///   assert_true(!is_prime(10001))
///   assert_true(is_prime(10007))
/// }
/// ```
pub fn is_prime(n : BigInt) -> Bool {
  guard n >= 2 else { false }
  let small_prime_limit = BigInt::from_int(SMALL_PRIME_LIMIT)
  guard n >= small_prime_limit else { is_small_prime(n.to_int()) }
  is_probable_prime_bpsw(n)
}
