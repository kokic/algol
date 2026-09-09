///|
test "constant polynomials have no candidates" {
  for coeffs in [[5N], [-5N, 0, 0]] {
    @test.assert_eq(rational_root_candidates(coeffs), [])
  }
}

///|
test "candidate generation trims high-degree zeros without changing input" {
  let coeffs = [-1N, 1, 0, 0]
  let roots = rational_root_candidates(coeffs).filter(r => is_root(coeffs, r))
  @test.assert_eq(roots, [@rational.new(1N, 1).unwrap()])
  @test.assert_eq(coeffs, [-1N, 1, 0, 0])
}

///|
test "repeated zero roots are included once alongside nonzero roots" {
  let coeffs = [0N, 0, -2, 1, 1, 0]
  let roots = rational_root_candidates(coeffs).filter(r => is_root(coeffs, r))
  @test.assert_eq(roots, [
    @rational.new(-2N, 1).unwrap(),
    rat_zero,
    @rational.new(1N, 1).unwrap(),
  ])
  let monomial = Array::makei(10002, i => if i == 10000 { 7N } else { 0N })
  @test.assert_eq(rational_root_candidates(monomial), [rat_zero])
}

///|
test "zero polynomials are rejected with InvalidInput" {
  for coeffs in [[], [0N], [0N, 0, 0]] {
    let error = @test.expect_error(() => rational_root_candidates(coeffs))
    assert_true(error is InvalidInput(_))
  }
}

///|
test "Cauchy bound rejects invalid input with a catchable error" {
  for coeffs in [[], [5N], [0N], [-1N, 1, 0]] {
    let error = @test.expect_error(() => cauchy_bound(coeffs))
    assert_true(error is InvalidInput(_))
  }
}

///|
test "Cauchy bound rounds up and retains fractional roots" {
  @test.assert_eq(cauchy_bound([-5N, 2]), 4N)
  @test.assert_eq(cauchy_bound([5N, -2]), 4N)
  let coeffs = [-5N, 2]
  let roots = rational_root_candidates(coeffs).filter(r => is_root(coeffs, r))
  @test.assert_eq(roots, [@rational.new(5N, 2).unwrap()])
}

///|
test "factorization rejects a failed search before returning partial factors" {
  let error = @test.expect_error(() => {
    factor_with_descent(18N, n => if n == 18 { 2N } else { n })
  })
  assert_true(error is FactorizationFailed(remaining=9N))
  let error = @test.expect_error(() => factor_with_descent(45N, _ => 9N))
  assert_true(error is FactorizationFailed(remaining=45N))
}

///|
test "factorization accepts complete descent and divisors propagate failure" {
  let factors = factor_with_descent(18N, n => if n == 18 { 2N } else { 3N })
  @test.assert_eq(factors.length(), 2)
  @test.assert_eq(factors[2N], 1)
  @test.assert_eq(factors[3N], 2)
  let error = @test.expect_error(() => {
    divisors(18N, n => factor_with_descent(n, remaining => remaining))
  })
  assert_true(error is FactorizationFailed(remaining=18N))
}
