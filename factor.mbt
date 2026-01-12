///|
/// Pollard's Rho algorithm for integer factorization with Brent's cycle detection.
/// 
/// ### Parameters
/// * `n`: Composite number to be factored.
/// * `k`: Frequency of GCD computations.
/// * `c`: Constant used in the polynomial function.
/// * `f`: Polynomial function (default is f(t) = (t^2 + c) mod n).
/// * `rand`: Random number generator (default is a time-based seed).
/// * `retry`: Number of attempts to find a non-trivial factor.
pub fn pollard_rho_brent(
  n : BigInt,
  k? : Int = 128,
  c? : (@random.Rand) -> BigInt = rand => random_range(rand, 1, n - 1),
  f? : (BigInt, BigInt, BigInt) -> BigInt = (t, c, n) => (t * t + c).mod(n),
  rand? : @random.Rand = @random.Rand::chacha8(seed=generate_time_based_seed()),
  retry? : Int = 10,
) -> BigInt {
  for _ in 0..<retry {
    let d = pollard_rho_brent_loop(n, k~, c~, f~, rand~)
    if d > 1 && d < n {
      return d
    }
  }
  n
}

///|
/// ### Parameters
/// * `n`: Composite number to be factored.
/// * `k`: Frequency of GCD computations.
/// * `c`: Constant used in the polynomial function.
/// * `f`: Polynomial function (default is f(t) = (t^2 + c) mod n).
/// * `rand`: Random number generator (default is a time-based seed).
pub fn pollard_rho_brent_loop(
  n : BigInt,
  k? : Int = 128,
  c? : (@random.Rand) -> BigInt = rand => random_range(rand, 1, n - 1),
  f? : (BigInt, BigInt, BigInt) -> BigInt = (t, c, n) => (t * t + c).mod(n),
  rand? : @random.Rand = @random.Rand::chacha8(seed=generate_time_based_seed()),
) -> BigInt {
  guard n > 2 else { n }
  guard @prime.is_odd(n) else { n >> 1 }
  let c = c(rand) // fixed constant
  let mut t : BigInt = 2
  for goal = 1; ; goal = goal << 1 {
    let s = t
    let mut v : BigInt = 1
    for step in 1..=goal {
      t = f(t, c, n)
      v = v * @prime.abs(t - s) % n
      guard !v.is_zero() else { return n }
      if step % k == 0 {
        let d = gcd(v, n)
        guard !(d > 1) else { return d }
      }
    }
    let d = gcd(v, n)
    guard !(d > 1) else { return d }
  }
}

///|
/// n ← min(a, n/a) where a is a non-trivial factor of n found via Pollard's rho.
pub fn prime_factor_descent(n : BigInt) -> BigInt {
  guard !n.is_zero() else { 0 }
  guard n > 3 && !@prime.is_prime(n) else { n }
  let mut n = n
  while n > 1 {
    let a = pollard_rho_brent(n)
    guard !@prime.is_prime(a) else { return a }
    let q = n / a
    guard !@prime.is_prime(q) else { return q }
    n = min(a, q)
  }
  n
}

///|
test {
  // 1235589577 = 26479 × 46663
  let factor = prime_factor_descent(1235589577)
  assert_true(factor == 26479 || factor == 46663)
}

///|
pub fn factor(n : BigInt) -> @hashmap.HashMap[BigInt, Int] {
  let mut n = n
  let factors = @hashmap.HashMap::new()
  while n > 1 {
    let p = prime_factor_descent(n)
    let count = factors.get(p).unwrap_or(0) + 1
    factors[p] = count
    n = n / p
  }
  factors
}
