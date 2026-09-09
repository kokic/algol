///|
using @hashmap {type HashMap}

///|
/// This function assumes that `n` has no prime factors greater than 9973 (i.e., n ≤ 9973² = 99460729). 
/// The returned result does not contain 1. 
/// 
/// Time complexity: (O(π(√n)) = O(√n/log n)), assuming arithmetic operations are (O(1)). Given the fixed input bound (n ≤ 9973²), the actual running time is bounded by a constant.
pub fn factor9973(n : Int) -> HashMap[Int, Int] raise InvokeError {
  guard 0 < n && n <= 99460729 else {
    raise InvalidInput(
      msg="This factorization is only valid for 0 < n ≤ 99460729.",
    )
  }

  let mut phase = n
  let factors = HashMap([])

  for i = 0; i < @prime.SMALL_PRIMES_LENGTH; i = i + 1 {
    let prime = @prime.small_primes[i]
    guard prime * prime <= phase else { break }
    while phase % prime == 0 {
      factors[prime] = factors.get(prime).unwrap_or(0) + 1
      phase = phase / prime
    }
  }

  guard phase != 1 else { factors }
  factors[phase] = 1
  factors
}

///|
pub type Factorization[N, P] = (N) -> HashMap[N, P] raise InvokeError

///|
pub fn[E : Compare, P : Compare] sorted_factors(
  n : E,
  f : Factorization[E, P],
) -> Array[(E, P)] raise InvokeError {
  let factors = f(n).to_array()
  factors.sort()
  factors
}

///|
test "small factorize for n < 10007" {
  assert_true(sorted_factors(1, factor9973) == [])
  assert_true(sorted_factors(108, factor9973) == [(2, 2), (3, 3)])
  assert_true(sorted_factors(9973, factor9973) == [(9973, 1)])
  assert_true(sorted_factors(10006, factor9973) == [(2, 1), (5003, 1)])
  assert_true(sorted_factors(10007, factor9973) == [(10007, 1)])
  assert_true(sorted_factors(10008, factor9973) == [(2, 3), (3, 2), (139, 1)])
  assert_true(sorted_factors(9973 * 9973, factor9973) == [(9973, 2)])
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
    // q == 1 means pollard_rho_brent failed and returned n itself
    guard q > 1 else { return n }
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
pub fn factor(n : BigInt) -> @hashmap.HashMap[BigInt, Int] raise InvokeError {
  guard 0N < n else {
    raise InvalidInput(msg="This factorization is only valid for 0 < n.")
  }
  let mut phase = n
  let factors = @hashmap.HashMap([])
  while phase > 1 {
    let prime = prime_factor_descent(phase)
    if prime == phase {
      // n is prime, or Pollard's rho gave up on a composite n
      guard @prime.is_prime(phase) else { break }
      factors[phase] = factors.get(phase).unwrap_or(0) + 1
      break
    }
    let count = factors.get(prime).unwrap_or(0) + 1
    factors[prime] = count
    phase = phase / prime
  }
  factors
}

///|
test "factor handles small and repeated prime factors" {
  assert_eq(factor(1N).length(), 0)
  assert_eq(factor(2N)[2N], 1)
  assert_eq(factor(16N)[2N], 4)
  assert_eq(factor(52N)[2N], 2)
  assert_eq(factor(52N)[13N], 1)
}
