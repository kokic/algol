///|
test "Univariate coeff extraction and rational evaluation" {
  let vars = ["x"]
  let poly = Polynomial::from_term(vars, [3], 3N) -
    Polynomial::from_term(vars, [1], 2N) +
    Polynomial::from_constant(vars, 5N)
  inspect(poly.to_univariate_coeffs(), content="[5, -2, 0, 3]")

  let three_half = @rational.new(3N, 2N).unwrap()
  let one_half = @rational.new(1N, 2N).unwrap()
  let poly_q = Polynomial::from_term(vars, [2], rat_one) +
    Polynomial::from_term(vars, [1], -three_half) +
    Polynomial::from_constant(vars, rat_one)
  assert_eq(poly_q.eval(one_half), one_half)
}

///|
test "Find rational roots from univariate polynomial" {
  let vars = ["x"]
  let poly = Polynomial::from_term(vars, [3], 1N) +
    Polynomial::from_term(vars, [2], 1N) -
    Polynomial::from_term(vars, [1], 2N)
  inspect(poly.find_rational_roots(), content="[-2, 0, 1]")
}

///|
test "Factor by rational roots with multiplicity" {
  let vars = ["x"]
  let poly = Polynomial::from_term(vars, [3], 1N) -
    Polynomial::from_term(vars, [2], 1N) -
    Polynomial::from_term(vars, [1], 1N) +
    Polynomial::from_constant(vars, 1N)
  let (roots, remainder) = poly.factor_by_rational_roots()
  inspect(roots, content="[-1, 1, 1]")
  assert_eq(remainder.unicodify(), "1")
}
