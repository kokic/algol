///|
test "Univariate polynomial gcd over BigRat" {
  let vars = ["x"]
  let two = @rational.new(2N, 1N).unwrap()
  let three = @rational.new(3N, 1N).unwrap()
  let p = Polynomial::from_term(vars, [3], rat_one) -
    Polynomial::from_term(vars, [1], three) +
    Polynomial::from_constant(vars, two)
  let dp = Polynomial::from_term(vars, [2], three) -
    Polynomial::from_constant(vars, three)

  let g = p.gcd(dp)
  let one_factor = Polynomial::from_term(vars, [1], rat_one) +
    Polynomial::from_constant(vars, -rat_one)
  let (q, r) = g.div_rem(one_factor).unwrap()
  assert_true(r.terms.is_empty())
  assert_eq(q.unicodify(), "1")
  assert_eq(g.eval(rat_one), rat_zero)
}

///|
test "Square-free part removes repeated rational roots" {
  let vars = ["x"]
  let poly = Polynomial::from_term(vars, [3], 1N) -
    Polynomial::from_term(vars, [1], 3N) +
    Polynomial::from_constant(vars, 2N)
  let sf = poly.square_free_part()

  let one_factor = Polynomial::from_term(vars, [1], rat_one) +
    Polynomial::from_constant(vars, -rat_one)
  let minus_two = @rational.new(-2N, 1N).unwrap()
  let minus_two_factor = Polynomial::from_term(vars, [1], rat_one) +
    Polynomial::from_constant(vars, -minus_two)
  let (q1, r1) = sf.div_rem(one_factor).unwrap()
  assert_true(r1.terms.is_empty())
  let (q2, r2) = q1.div_rem(minus_two_factor).unwrap()
  assert_true(r2.terms.is_empty())
  assert_eq(q2.unicodify(), "1")

  assert_eq(sf.eval(rat_one), rat_zero)
  assert_eq(sf.eval(minus_two), rat_zero)
}
