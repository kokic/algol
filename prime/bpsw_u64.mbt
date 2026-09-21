///|
/// Native `UInt64` implementation of the Baillie–Pomerance–Selfridge (BPSW)
/// probable-prime test, used for `n < 2^63`.
/// 
/// The `BigInt` path is correct but allocates on every arithmetic operation,
/// which makes the strong Lucas–Selfridge step the dominant cost. Doing the
/// same test in machine words with Montgomery reduction removes those
/// allocations entirely; measured on the benchmark suite it is roughly one to
/// two orders of magnitude faster for the small/medium cases (the strong Lucas
/// step drops from ~130–220 µs to a few microseconds).
/// 
/// The modulus is always odd here — even candidates are rejected before any
/// arithmetic — so Montgomery reduction with `R = 2^64` applies directly.
#valtype
priv struct U64Mod {
  n : UInt64
  /// `-n^{-1} mod 2^64`
  ninv : UInt64
  /// `2^128 mod n`, the Montgomery representation of `1`
  r2 : UInt64
}

///|
/// The high 64 bits of the 128-bit product `a * b`; the low half is just
/// `a * b`. Returning the halves separately keeps the inner loop allocation-free
/// (a returned tuple would be boxed on the native backend).
#inline
fn umul_hi(a : UInt64, b : UInt64) -> UInt64 {
  let a_lo = a & 0xffffffffUL
  let a_hi = a >> 32
  let b_lo = b & 0xffffffffUL
  let b_hi = b >> 32
  let x = a_lo * b_lo
  let y = a_hi * b_lo + (x >> 32)
  let z = a_lo * b_hi + (y & 0xffffffffUL)
  a_hi * b_hi + (y >> 32) + (z >> 32)
}

///|
/// Precomputes the Montgomery constants for an odd modulus `n < 2^63`.
fn U64Mod::new(n : UInt64) -> U64Mod {
  // Newton–Raphson iteration for `n^{-1} mod 2^64`, starting from `n` (which is
  // correct modulo 8 for odd `n`) and doubling the number of correct bits each
  // step. Six steps carry 3 bits to 192, well past 64.
  let mut inv = n
  for k = 0; k < 6; k = k + 1 {
    inv = inv * (2UL - n * inv)
  }
  // `2^64 mod n` by repeated doubling; `n < 2^63` so the sums do not overflow.
  let mut r = 1UL % n
  for k = 0; k < 64; k = k + 1 {
    r = r + r
    if r >= n {
      r = r - n
    }
  }
  // `2^128 mod n = (2^64 mod n) * 2^64 mod n`.
  let mut r2 = r
  for k = 0; k < 64; k = k + 1 {
    r2 = r2 + r2
    if r2 >= n {
      r2 = r2 - n
    }
  }
  { n, ninv: 0UL - inv, r2, }
}

///|
/// Montgomery product `a * b * R^{-1} mod n`, for `a, b < n < 2^63`.
#inline
fn U64Mod::mont_mul(self : U64Mod, a : UInt64, b : UInt64) -> UInt64 {
  let lo = a * b
  let hi = umul_hi(a, b)
  let m = lo * self.ninv
  let mlo = m * self.n
  let mhi = umul_hi(m, self.n)
  let s = lo + mlo
  let c = if s < lo { 1UL } else { 0UL }
  let t = hi + mhi + c
  // `t < 2n`, so a single conditional subtraction reduces it.
  if t >= self.n {
    t - self.n
  } else {
    t
  }
}

///|
/// Modular product `a * b mod n`, for `a, b < n < 2^63`.
#inline
fn U64Mod::mul_mod(self : U64Mod, a : UInt64, b : UInt64) -> UInt64 {
  // `mont_mul(a, r2) = a * R`, then `mont_mul(a * R, b) = a * b`.
  self.mont_mul(self.mont_mul(a, self.r2), b)
}

///|
#inline
fn U64Mod::add_mod(self : U64Mod, a : UInt64, b : UInt64) -> UInt64 {
  let s = a + b
  if s >= self.n {
    s - self.n
  } else {
    s
  }
}

///|
#inline
fn U64Mod::sub_mod(self : U64Mod, a : UInt64, b : UInt64) -> UInt64 {
  if a >= b {
    a - b
  } else {
    a + self.n - b
  }
}

///|
/// Halves `x < n` modulo `n`, i.e. `x * 2^{-1} mod n`.
#inline
fn U64Mod::half_mod(self : U64Mod, x : UInt64) -> UInt64 {
  if (x & 1UL) == 0UL {
    x >> 1
  } else {
    (x + self.n) >> 1
  }
}

///|
/// `base^exp mod n`.
fn U64Mod::pow_mod(self : U64Mod, base : UInt64, exp : UInt64) -> UInt64 {
  let mut result = 1UL % self.n
  let mut b = base % self.n
  let mut e = exp
  while e > 0UL {
    if (e & 1UL) == 1UL {
      result = self.mul_mod(result, b)
    }
    e = e >> 1
    if e > 0UL {
      b = self.mul_mod(b, b)
    }
  }
  result
}

///|
/// Strong probable-prime test of `n` to `base` (Miller–Rabin). `n` is odd and
/// greater than 3.
fn U64Mod::is_strong_probable_prime(self : U64Mod, base : UInt64) -> Bool {
  let m = self.n - 1UL
  let s = m.ctz()
  let d = m >> s
  let mut x = self.pow_mod(base, d)
  if x == 1UL || x == m {
    return true
  }
  for k = 1; k < s; k = k + 1 {
    x = self.mul_mod(x, x)
    if x == m {
      return true
    }
    if x == 1UL {
      return false
    }
  }
  false
}

///|
/// Jacobi symbol `(a / n)` for odd `n > 0`.
fn U64Mod::jacobi(self : U64Mod, a_in : UInt64) -> Int {
  let mut a = a_in % self.n
  let mut n = self.n
  let mut t = 1
  while a != 0UL {
    while (a & 1UL) == 0UL {
      a = a >> 1
      let r = n & 7UL
      if r == 3UL || r == 5UL {
        t = -t
      }
    }
    let swap = a
    a = n
    n = swap
    if (a & 3UL) == 3UL && (n & 3UL) == 3UL {
      t = -t
    }
    a = a % n
  }
  if n == 1UL {
    t
  } else {
    0
  }
}

///|
/// Computes `(U_d, V_d, Q^d) mod n` for the Lucas sequence with parameters
/// `(P, Q) = (1, q_mod)` and index `d`, via the binary ladder.
fn U64Mod::lucas_uvqk(
  self : U64Mod,
  q_mod : UInt64,
  d_val_mod : UInt64,
  d : UInt64,
) -> (UInt64, UInt64, UInt64) {
  let mut u : UInt64 = 0
  let mut v : UInt64 = 2UL % self.n
  let mut qk : UInt64 = 1UL % self.n
  let mut bit = 63 - d.clz()
  while bit >= 0 {
    let u2 = self.mul_mod(u, v)
    let v2 = self.sub_mod(self.mul_mod(v, v), self.add_mod(qk, qk))
    u = u2
    v = v2
    qk = self.mul_mod(qk, qk)
    if ((d >> bit) & 1UL) == 1UL {
      let u_old = u
      let v_old = v
      u = self.half_mod(self.add_mod(u_old, v_old))
      v = self.half_mod(self.add_mod(v_old, self.mul_mod(d_val_mod, u_old)))
      qk = self.mul_mod(qk, q_mod)
    }
    bit = bit - 1
  }
  (u, v, qk)
}

///|
/// Strong Lucas–Selfridge test with Selfridge's parameter choice.
fn U64Mod::is_strong_lucas(self : U64Mod) -> Bool {
  // Selfridge: pick `D` in `5, -7, 9, -11, …` with `(D / n) = -1`.
  let mut d_signed : Int64 = 5L
  let mut sign = true
  let mut d_val_mod : UInt64 = 0
  let mut found = false
  while !found {
    let abs_d = d_signed.abs().reinterpret_as_uint64()
    let residue = abs_d % self.n
    d_val_mod = if sign { residue } else { self.sub_mod(0UL, residue) }
    let j = self.jacobi(d_val_mod)
    if j == -1 {
      found = true
    } else if j == 0 {
      return false
    } else {
      let next = d_signed.abs() + 2L
      sign = !sign
      d_signed = if sign { next } else { -next }
    }
  }
  // `P = 1`, `Q = (1 - D) / 4`.
  let q_signed = (1L - d_signed) / 4L
  let q_mod = if q_signed >= 0L {
    q_signed.reinterpret_as_uint64() % self.n
  } else {
    self.sub_mod(0UL, (-q_signed).reinterpret_as_uint64() % self.n)
  }
  // `n + 1 = d * 2^s` with `d` odd.
  let np1 = self.n + 1UL
  let s = np1.ctz()
  let d = np1 >> s
  let (u, v, qk) = self.lucas_uvqk(q_mod, d_val_mod, d)
  if u == 0UL {
    return true
  }
  if v == 0UL {
    return true
  }
  let mut vv = v
  let mut qk_cur = qk
  for k = 1; k < s; k = k + 1 {
    vv = self.sub_mod(self.mul_mod(vv, vv), self.add_mod(qk_cur, qk_cur))
    qk_cur = self.mul_mod(qk_cur, qk_cur)
    if vv == 0UL {
      return true
    }
  }
  false
}

///|
fn is_perfect_square_u64(n : UInt64) -> Bool {
  if n < 2UL {
    return true
  }
  let r = n & 15UL
  if !(r == 0UL || r == 1UL || r == 4UL || r == 9UL) {
    return false
  }
  let mut x = 1UL << ((64 - n.clz() + 1) / 2)
  let mut y = (x + n / x) >> 1
  while y < x {
    x = y
    y = (x + n / x) >> 1
  }
  x * x == n
}

///|
/// The first 64 primes (3 … 311). Trial division by these rejects almost every
/// composite outright — matching what `core/math` does — and in machine words it
/// costs only a few hundred nanoseconds.
let trial_division_primes_u64 : Array[UInt64] = [
  3UL, 5UL, 7UL, 11UL, 13UL, 17UL, 19UL, 23UL, 29UL, 31UL, 37UL, 41UL, 43UL, 47UL,
  53UL, 59UL, 61UL, 67UL, 71UL, 73UL, 79UL, 83UL, 89UL, 97UL, 101UL, 103UL, 107UL,
  109UL, 113UL, 127UL, 131UL, 137UL, 139UL, 149UL, 151UL, 157UL, 163UL, 167UL, 173UL,
  179UL, 181UL, 191UL, 193UL, 197UL, 199UL, 211UL, 223UL, 227UL, 229UL, 233UL, 239UL,
  241UL, 251UL, 257UL, 263UL, 269UL, 271UL, 277UL, 281UL, 283UL, 293UL, 307UL, 311UL,
]

///|
/// BPSW probable-prime test for `n < 2^63` using native 64-bit arithmetic.
fn is_probable_prime_bpsw_u64(n : UInt64) -> Bool {
  guard n >= 2UL else { return false }
  guard !(n == 2UL || n == 3UL) else { return true }
  guard (n & 1UL) == 1UL else { return false }
  for p in trial_division_primes_u64 {
    if n == p {
      return true
    }
    if n % p == 0UL {
      return false
    }
  }
  if is_perfect_square_u64(n) {
    return false
  }
  let ctx = U64Mod::new(n)
  guard ctx.is_strong_probable_prime(2UL) else { return false }
  ctx.is_strong_lucas()
}

///|
test "u64 BPSW agrees with BigInt BPSW" {
  for i = 2; i < 30000; i = i + 1 {
    let n = BigInt::from_int(i)
    if is_probable_prime_bpsw_u64(n.to_uint64()) !=
      is_probable_prime_bpsw_bigint(n) {
      fail("u64 vs BigInt mismatch at \{i}")
    }
  }
  let mut state : UInt64 = 0x243F6A8885A308D3UL
  for k = 0; k < 3000; k = k + 1 {
    state = state * 6364136223846793005UL + 1442695040888963407UL
    let v = state >> 1
    if v >= 2UL {
      let n = BigInt::from_string(v.to_string())
      if is_probable_prime_bpsw_u64(v) != is_probable_prime_bpsw_bigint(n) {
        fail("u64 vs BigInt mismatch at \{v}")
      }
    }
  }
}
