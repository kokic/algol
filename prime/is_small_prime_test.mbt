///|
fn trial_division_prime(n : Int) -> Bool {
  guard n >= 2 else { false }
  guard n != 2 else { true }
  guard n % 2 != 0 else { false }

  let mut i = 3
  while i * i <= n {
    if n % i == 0 {
      return false
    }
    i = i + 2
  }
  true
}

///|
test "is_small_prime exact for [0, 10000)" {
  for n = 0; n < 10000; n = n + 1 {
    assert_eq(is_small_prime(n), trial_division_prime(n))
  }
}
