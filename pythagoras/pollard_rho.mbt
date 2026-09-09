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
  rand? : @random.Rand = random_time_based(),
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
  rand? : @random.Rand = random_time_based(),
) -> BigInt {
  guard n > 2 else { n }
  guard @prime.is_odd(n) else { n >> 1 }
  let c = c(rand) // fixed constant
  let mut t : BigInt = 2
  for goal = 1; ; goal = goal << 1 {
    let s = t
    let mut v : BigInt = 1
    for step in 1..<=goal {
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
