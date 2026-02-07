///|
test "Heuristic multivariate gcd finds common bivariate factor" {
  let vars = ["x", "y"]
  let x = Polynomial::from_term(vars, [1, 0], 1N)
  let y = Polynomial::from_term(vars, [0, 1], 1N)
  let one = Polynomial::from_constant(vars, 1N)
  let two = Polynomial::from_constant(vars, 2N)

  let f = (x + y) * (x + one)
  let g = (x + y) * (y + two)
  let d = f.gcd_multivariate(g)

  assert_eq(d.terms.length(), 2)
  assert_eq(d.coeff([1, 0]), rat_one)
  assert_eq(d.coeff([0, 1]), rat_one)
  assert_eq(d.coeff([0, 0]), rat_zero)
}

///|
test "Heuristic multivariate gcd falls back to one for coprime pair" {
  let vars = ["x", "y"]
  let f = Polynomial::from_term(vars, [1, 0], 1N) +
    Polynomial::from_term(vars, [0, 1], 1N)
  let g = Polynomial::from_term(vars, [1, 0], 1N) -
    Polynomial::from_term(vars, [0, 1], 1N)
  let d = f.gcd_multivariate(g)
  assert_eq(d.unicodify(), "1")
}

///|
test "Heuristic multivariate gcd supports different variable orders" {
  let vars_a = ["x", "y"]
  let x_a = Polynomial::from_term(vars_a, [1, 0], 1N)
  let y_a = Polynomial::from_term(vars_a, [0, 1], 1N)
  let one_a = Polynomial::from_constant(vars_a, 1N)
  let f = (x_a + y_a) * (x_a + one_a)

  let vars_b = ["y", "x"]
  let y_b = Polynomial::from_term(vars_b, [1, 0], 1N)
  let x_b = Polynomial::from_term(vars_b, [0, 1], 1N)
  let two_b = Polynomial::from_constant(vars_b, 2N)
  let g = (x_b + y_b) * (x_b + two_b)

  let d = f.gcd_multivariate(g)
  assert_eq(d.terms.length(), 2)
  assert_eq(d.coeff([1, 0]), rat_one)
  assert_eq(d.coeff([0, 1]), rat_one)
  assert_eq(d.coeff([0, 0]), rat_zero)
}
