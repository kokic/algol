///|
test "Factor multivariate extracts variable-wise linear factors after square-free" {
  let vars = ["x", "y"]
  let x = Polynomial::from_term(vars, [1, 0], 1N)
  let y = Polynomial::from_term(vars, [0, 1], 1N)
  let one = Polynomial::from_constant(vars, 1N)
  let two = Polynomial::from_constant(vars, 2N)

  let x_minus_one = x - one
  let y_plus_two = y + two
  let x_plus_y_plus_one = x + y + one
  let poly = (x_minus_one * x_minus_one) * y_plus_two * x_plus_y_plus_one

  let (factors, remainder) = poly.factor_multivariate()
  let factor_strings = factors.map(f => f.unicodify())
  factor_strings.sort()
  assert_true(factor_strings.contains("x - 1"))

  let x_q = Polynomial::from_term(vars, [1, 0], rat_one)
  let y_q = Polynomial::from_term(vars, [0, 1], rat_one)
  let one_q = Polynomial::from_constant(vars, rat_one)
  let two_q = Polynomial::from_constant(vars, @rational.new(2N, 1N).unwrap())
  let poly_q = (x_q - one_q) * (x_q - one_q) * (y_q + two_q) * (x_q + y_q + one_q)

  let (_, rem_by_factor) = poly_q.div_rem(factors[0]).unwrap()
  assert_true(rem_by_factor.terms.is_empty())
  let (_, rem_by_remainder) = poly_q.div_rem(remainder).unwrap()
  assert_true(rem_by_remainder.terms.is_empty())
}

///|
test "Factor multivariate falls back to remainder for non-extractable factors" {
  let vars = ["x", "y"]
  let x = Polynomial::from_term(vars, [1, 0], 1N)
  let y = Polynomial::from_term(vars, [0, 1], 1N)
  let one = Polynomial::from_constant(vars, 1N)
  let poly = x + y + one

  let (factors, remainder) = poly.factor_multivariate()
  assert_eq(factors.length(), 0)
  assert_eq(remainder.coeff([1, 0]), rat_one)
  assert_eq(remainder.coeff([0, 1]), rat_one)
  assert_eq(remainder.coeff([0, 0]), rat_one)
}

///|
test "Factor multivariate works for univariate input" {
  let vars = ["x"]
  let x = Polynomial::from_term(vars, [1], 1N)
  let one = Polynomial::from_constant(vars, 1N)
  let two = Polynomial::from_constant(vars, 2N)
  let poly = (x - one) * (x - one) * (x + two)

  let (factors, remainder) = poly.factor_multivariate()
  let factor_strings = factors.map(f => f.unicodify())
  factor_strings.sort()
  inspect(factor_strings, content="[\"x + 2\", \"x - 1\"]")
  assert_eq(remainder.unicodify(), "1")
}

///|
test "Factor multivariate full iterates on remainder" {
  let vars = ["x", "y"]
  let x = Polynomial::from_term(vars, [1, 0], 1N)
  let y = Polynomial::from_term(vars, [0, 1], 1N)
  let one = Polynomial::from_constant(vars, 1N)
  let two = Polynomial::from_constant(vars, 2N)

  let poly = (x - one) * (x - one) * (y + two) * (x + y + one)
  let (factors, remainder) = poly.factor_multivariate_full()
  let factor_strings = factors.map(f => f.unicodify())
  factor_strings.sort()
  inspect(factor_strings, content="[\"x - 1\", \"y + 2\"]")

  assert_eq(remainder.coeff([1, 0]), rat_one)
  assert_eq(remainder.coeff([0, 1]), rat_one)
  assert_eq(remainder.coeff([0, 0]), rat_one)
}

///|
test "Factor multivariate full report matches wrapper and contains trace" {
  let vars = ["x", "y"]
  let x = Polynomial::from_term(vars, [1, 0], 1N)
  let y = Polynomial::from_term(vars, [0, 1], 1N)
  let one = Polynomial::from_constant(vars, 1N)
  let two = Polynomial::from_constant(vars, 2N)

  let poly = (x - one) * (x - one) * (y + two) * (x + y + one)
  let report = poly.factor_multivariate_full_report()
  let (factors, remainder) = poly.factor_multivariate_full()

  let report_factor_strings = report.factors.map(f => f.unicodify())
  report_factor_strings.sort()
  let wrapper_factor_strings = factors.map(f => f.unicodify())
  wrapper_factor_strings.sort()
  assert_eq(report_factor_strings, wrapper_factor_strings)

  assert_eq(report.remainder.unicodify(), remainder.unicodify())
  assert_eq(report.primitive_remainder.unicodify(), remainder.unicodify())
  assert_eq(report.content, rat_one)
  let original_q = (Polynomial::from_term(vars, [1, 0], rat_one) -
    Polynomial::from_constant(vars, rat_one)) *
    (Polynomial::from_term(vars, [1, 0], rat_one) -
      Polynomial::from_constant(vars, rat_one)) *
    (Polynomial::from_term(vars, [0, 1], rat_one) +
      Polynomial::from_constant(vars, @rational.new(2N, 1N).unwrap())) *
    (Polynomial::from_term(vars, [1, 0], rat_one) +
      Polynomial::from_term(vars, [0, 1], rat_one) +
      Polynomial::from_constant(vars, rat_one))
  let reconstructed = Polynomial::reconstruct_from_report(report)
  assert_eq(report.is_reconstruction_valid, (reconstructed - original_q).terms.is_empty())
  assert_true(report.rounds >= 1)
  let trace = report.trace.display(2)
  assert_true(trace.contains(": factor_multivariate_full_report"))
  assert_true(trace.contains("[content]: 1"))
  assert_true(trace.contains("[stop_reason]:"))
}

///|
test "Factor multivariate full report extracts non-unit content" {
  let vars = ["x"]
  let x = Polynomial::from_term(vars, [1], 1N)
  let one = Polynomial::from_constant(vars, 1N)
  let two = Polynomial::from_constant(vars, 2N)
  let six = Polynomial::from_constant(vars, 6N)
  let poly = six * (x - one) * (x - one) * (x + two)

  let report = poly.factor_multivariate_full_report()
  assert_eq(report.content, @rational.new(6N, 1N).unwrap())
  let original_q = Polynomial::from_constant(vars, @rational.new(6N, 1N).unwrap()) *
    (Polynomial::from_term(vars, [1], rat_one) -
      Polynomial::from_constant(vars, rat_one)) *
    (Polynomial::from_term(vars, [1], rat_one) -
      Polynomial::from_constant(vars, rat_one)) *
    (Polynomial::from_term(vars, [1], rat_one) +
      Polynomial::from_constant(vars, @rational.new(2N, 1N).unwrap()))
  let reconstructed = Polynomial::reconstruct_from_report(report)
  assert_eq(report.is_reconstruction_valid, (reconstructed - original_q).terms.is_empty())

  let factor_strings = report.factors.map(f => f.unicodify())
  factor_strings.sort()
  inspect(factor_strings, content="[\"x + 2\", \"x - 1\"]")
  assert_eq(report.primitive_remainder.unicodify(), "1")
  assert_eq(report.remainder.unicodify(), "1")
}

///|
test "Reconstruct from multivariate factor report" {
  let vars = ["x", "y"]
  let x = Polynomial::from_term(vars, [1, 0], 1N)
  let y = Polynomial::from_term(vars, [0, 1], 1N)
  let one = Polynomial::from_constant(vars, 1N)
  let three = Polynomial::from_constant(vars, 3N)
  let poly = three * (x - one) * (x + y + one)

  let report = poly.factor_multivariate_full_report()
  assert_true(report.is_reconstruction_valid)
  let reconstructed = Polynomial::reconstruct_from_report(report)
  let original = Polynomial::from_term(vars, [0, 0], @rational.new(3N, 1N).unwrap()) *
    (Polynomial::from_term(vars, [1, 0], rat_one) -
      Polynomial::from_constant(vars, rat_one)) *
    (Polynomial::from_term(vars, [1, 0], rat_one) +
      Polynomial::from_term(vars, [0, 1], rat_one) +
      Polynomial::from_constant(vars, rat_one))
  assert_true((reconstructed - original).terms.is_empty())
}
