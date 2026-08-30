///|
const PIERRE_DUSART_BOUNDARY : Int = 5393

///|
const MAX_ERROR_OFFSET : Int = 16

///|
const SMALL_PRIME_LIMIT : Int = 10000

///|
fn chebyshev_approx(n : Int) -> Double {
  n.to_double() / @math.ln(n.to_double())
}

///|
fn fast_error(n : Int) -> Double {
  n.to_double() / 64 - 1
}

///|
/// ∀ n ≤ 5392, `effective_chebyshev_approx ≤ π(n)`. 
fn effective_chebyshev_approx(n : Int) -> Double {
  chebyshev_approx(n) + fast_error(n)
}

///|
/// ∀ n ≥ 5393, `pierre_dusart_approx ≤ π(n)`.
/// 
/// Also see: https://en.wikipedia.org/wiki/Prime-counting_function#Inequalities
fn pierre_dusart_approx(n : Int) -> Double {
  n.to_double() / (@math.ln(n.to_double()) - 1)
}

///|
fn small_approx(n : Int) -> Double {
  guard n < PIERRE_DUSART_BOUNDARY else { pierre_dusart_approx(n) }
  effective_chebyshev_approx(n)
}

///|
fn small_prime_window_contains(
  n : Int,
  min_index : Int,
  max_index : Int,
) -> Bool {
  guard 0 <= min_index &&
    min_index <= max_index &&
    max_index <= SMALL_PRIMES_LENGTH else {
    false
  }
  proof_assert small_prime_window_bounds(min_index, max_index)
  for i = min_index; i < max_index; i = i + 1 {
    proof_assert small_prime_loop_bounds(i, min_index, max_index)
    proof_assert 0 <= i
    proof_assert i < SMALL_PRIMES_LENGTH
    let prime = small_primes[i]
    if n == prime {
      return true
    }
  } where {
    proof_invariant: small_prime_loop_bounds(i, min_index, max_index),
  }
  false
}

///|
/// ```mbt check
/// test "is_small_prime boundary regression" {
///   assert_true(!is_small_prime(0))
///   assert_true(!is_small_prime(1))
///   assert_true(is_small_prime(2))
///   assert_true(is_small_prime(3))
///   assert_true(is_small_prime(5381))
///   assert_true(is_small_prime(5387))
///   assert_true(is_small_prime(5393))
///   assert_true(is_small_prime(9973))
///   assert_true(!is_small_prime(9999))
///   assert_true(!is_small_prime(10000))
/// }
/// ```
pub fn is_small_prime(n : Int) -> Bool {
  guard n >= 2 else { false }
  guard n != 2 && n != 3 else { true }
  guard n < SMALL_PRIME_LIMIT else { false }
  guard n % 2 != 0 else { false }
  let min_index = (small_approx(n).to_int() - 1).max(0)
  let max_index = (min_index + MAX_ERROR_OFFSET).min(SMALL_PRIMES_LENGTH)
  small_prime_window_contains(n, min_index, max_index)
}
