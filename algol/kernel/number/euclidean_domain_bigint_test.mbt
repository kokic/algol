///|
test "BigInt Euclidean div_mod_euclid keeps non-negative remainder" {
  let cases = [(17N, 5N), (17N, -5N), (-17N, 5N), (-17N, -5N)]
  for _, pair in cases {
    let (a, b) = pair
    let (q, r) = EuclideanDomain::div_mod_euclid(a, b)
    let b_abs = EuclideanDomain::abs(b)
    assert_true(r >= 0N)
    assert_true(r < b_abs)
    @test.assert_eq(a, b * q + r)
  }
}

///|
test "BigInt Euclidean extended_gcd satisfies Bezout identity" {
  let a = -120N
  let b = 23N
  let (g, s, t) = EuclideanDomain::extended_gcd(a, b)

  assert_true(g >= 0N)
  @test.assert_eq(a * s + b * t, g)
  @test.assert_eq(g, EuclideanDomain::abs(gcd(a, b)))
}

///|
test "BigInt gcd is non-negative" {
  @test.assert_eq(gcd(-2N, 0N), 2N)
  @test.assert_eq(gcd(0N, -2N), 2N)
  @test.assert_eq(gcd(-12N, -18N), 6N)
}

///|
test "random handles degenerate limits without panicking" {
  let rand = @random.Rand::chacha8(seed=uint64_array_to_bytes([1, 2, 3, 4]))
  @test.assert_eq(random(rand, 0N), 0N)
  @test.assert_eq(random(rand, 1N), 0N)
  @test.assert_eq(random_range(rand, 5N, 5N), 5N)
  @test.assert_eq(random_range(rand, 7N, 3N), 7N)
}

///|
test "uint64_array_to_bytes handles non-four element arrays" {
  @test.assert_eq(uint64_array_to_bytes([1, 2]).length(), 16)
  @test.assert_eq(uint64_array_to_bytes([0x0102030405060708]).length(), 8)
}

///|
test "factor handles small and repeated prime factors" {
  @test.assert_eq(factor(1N).length(), 0)
  @test.assert_eq(factor(2N)[2N], 1)
  @test.assert_eq(factor(16N)[2N], 4)
  @test.assert_eq(factor(52N)[2N], 2)
  @test.assert_eq(factor(52N)[13N], 1)
}
