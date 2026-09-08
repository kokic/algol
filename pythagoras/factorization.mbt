///|
using @hashmap {type HashMap}

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
test "small factorize for n < 10007" {
  assert_true(sorted_factors(1, factor9973) == [])
  assert_true(sorted_factors(108, factor9973) == [(2, 2), (3, 3)])
  assert_true(sorted_factors(9973, factor9973) == [(9973, 1)])
  assert_true(sorted_factors(10006, factor9973) == [(2, 1), (5003, 1)])
  assert_true(sorted_factors(10007, factor9973) == [(10007, 1)])
  assert_true(sorted_factors(10008, factor9973) == [(2, 3), (3, 2), (139, 1)])
  assert_true(sorted_factors(9973 * 9973, factor9973) == [(9973, 2)])
}
