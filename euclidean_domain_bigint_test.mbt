///|
test "BigInt Euclidean div_mod_euclid keeps non-negative remainder" {
  let cases = [(17N, 5N), (17N, -5N), (-17N, 5N), (-17N, -5N)]
  for _, pair in cases {
    let (a, b) = pair
    let (q, r) = EuclideanDomain::div_mod_euclid(a, b)
    let b_abs = EuclideanDomain::abs(b)
    assert_true(r >= 0N)
    assert_true(r < b_abs)
    assert_eq(a, b * q + r)
  }
}

///|
test "BigInt Euclidean extended_gcd satisfies Bezout identity" {
  let a = -120N
  let b = 23N
  let (g, s, t) = EuclideanDomain::extended_gcd(a, b)

  assert_true(g >= 0N)
  assert_eq(a * s + b * t, g)
  assert_eq(g, EuclideanDomain::abs(gcd(a, b)))
}
