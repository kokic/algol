///|
pub struct Term[E] {
  exps : Array[Int]
  coeff : E
} derive(Show)

///|
pub struct Polynomial[E] {
  vars : Array[String]
  terms : @hashmap.HashMap[String, Term[E]]
}

///|
impl[E : Neg] Neg for Polynomial[E] with neg(self) {
  Polynomial::{
    vars: self.vars,
    terms: self.terms.map((_, v) => Term::{ exps: v.exps, coeff: -v.coeff }),
  }
}

///|
fn monomial_key(exps : Array[Int]) -> String {
  join(exps, separator=",")
}

///|
fn assert_exp_arity(vars : Array[String], exps : Array[Int]) -> Unit raise {
  assert_true(
    vars.length() == exps.length(),
    msg="exponent arity mismatch: \{vars.length()} vars vs \{exps.length()} exps",
  )
}

///|
fn[E : Semiring] add_term_to_map(
  terms : @hashmap.HashMap[String, Term[E]],
  exps : Array[Int],
  coeff : E,
) -> Unit {
  guard !coeff.is_zero() else { return }
  let key = monomial_key(exps)
  match terms.get(key) {
    Some(term) => {
      let new_coeff = term.coeff.add(coeff)
      if new_coeff.is_zero() {
        terms.remove(key)
      } else {
        terms[key] = Term::{ exps: term.exps, coeff: new_coeff }
      }
    }
    None => terms[key] = Term::{ exps, coeff }
  }
}

///|
pub fn[E] Polynomial::zero(vars : Array[String]) -> Polynomial[E] {
  Polynomial::{ vars, terms: @hashmap.HashMap::new() }
}

///|
pub fn[E : Semiring] Polynomial::one(
  vars : Array[String],
) -> Polynomial[E] raise {
  Polynomial::from_term(vars, Array::makei(vars.length(), _ => 0), E::one())
}

///|
pub fn[E : Semiring] Polynomial::from_term(
  vars : Array[String],
  exps : Array[Int],
  coeff : E,
) -> Polynomial[E] raise {
  assert_exp_arity(vars, exps)
  let terms = @hashmap.HashMap::new()
  add_term_to_map(terms, exps, coeff)
  Polynomial::{ vars, terms }
}

///|
pub fn[E : Semiring] Polynomial::from_constant(
  vars : Array[String],
  coeff : E,
) -> Polynomial[E] raise {
  Polynomial::from_term(vars, Array::makei(vars.length(), _ => 0), coeff)
}

///|
pub fn[E : Semiring] Polynomial::coeff(
  poly : Polynomial[E],
  exps : Array[Int],
) -> E raise {
  assert_exp_arity(poly.vars, exps)
  let key = monomial_key(exps)
  match poly.terms.get(key) {
    Some(term) => term.coeff
    None => E::zero()
  }
}

///|
fn[E : Semiring] unsafe_polynomial_add(
  a : Polynomial[E],
  b : Polynomial[E],
) -> Polynomial[E] {
  // assert_same_vars(a.vars, b.vars)
  let terms = @hashmap.HashMap::new()
  for _, term in a.terms {
    add_term_to_map(terms, term.exps, term.coeff)
  }
  for _, term in b.terms {
    add_term_to_map(terms, term.exps, term.coeff)
  }
  Polynomial::{ vars: a.vars, terms }
}

///|
fn vars_equal(a : Array[String], b : Array[String]) -> Bool {
  guard a.length() == b.length() else { false }
  for i in 0..<a.length() {
    guard a[i] == b[i] else { return false }
  }
  true
}

///|
fn[E : Semiring] align_to_vars(
  poly : Polynomial[E],
  new_vars : Array[String],
  var_index : @hashmap.HashMap[String, Int],
) -> Polynomial[E] {
  let index_map = Array::makei(poly.vars.length(), fn(i) {
    var_index.get(poly.vars[i]).unwrap()
  })
  let terms = @hashmap.HashMap::new()
  for _, term in poly.terms {
    let exps = Array::makei(new_vars.length(), _ => 0)
    for i in 0..<index_map.length() {
      exps[index_map[i]] = term.exps[i]
    }
    add_term_to_map(terms, exps, term.coeff)
  }
  Polynomial::{ vars: new_vars, terms }
}

///|
fn[E : Semiring] mutual_align_vars(
  a : Polynomial[E],
  b : Polynomial[E],
) -> (Polynomial[E], Polynomial[E]) {
  let new_vars = []
  let var_index = @hashmap.HashMap::new()
  for i in 0..<a.vars.length() {
    let v = a.vars[i]
    new_vars.push(v)
    var_index[v] = i
  }
  let mut next_index = new_vars.length()
  for v in b.vars {
    guard !var_index.contains(v) else { continue }
    new_vars.push(v)
    var_index[v] = next_index
    next_index = next_index + 1
  }
  (align_to_vars(a, new_vars, var_index), align_to_vars(b, new_vars, var_index))
}

///|
pub impl[E : Semiring] Add for Polynomial[E] with add(a, b) {
  guard !vars_equal(a.vars, b.vars) else { unsafe_polynomial_add(a, b) }
  let (a_aligned, b_aligned) = mutual_align_vars(a, b)
  unsafe_polynomial_add(a_aligned, b_aligned)
}

///|
pub impl[E : Semiring + Neg] Sub for Polynomial[E] with sub(a, b) {
  a + -b
}

///|
pub fn[E : Semiring] unsafe_polynomial_mul(
  a : Polynomial[E],
  b : Polynomial[E],
) -> Polynomial[E] {
  // assert_same_vars(a.vars, b.vars)
  let terms = @hashmap.HashMap::new()
  for _, term_a in a.terms {
    guard !term_a.coeff.is_zero() else { continue }
    for _, term_b in b.terms {
      guard !term_b.coeff.is_zero() else { continue }
      let exps = Array::makei(a.vars.length(), fn(i) {
        term_a.exps[i] + term_b.exps[i]
      })
      let coeff = term_a.coeff.mul(term_b.coeff)
      add_term_to_map(terms, exps, coeff)
    }
  }
  Polynomial::{ vars: a.vars, terms }
}

///|
pub impl[E : Semiring] Mul for Polynomial[E] with mul(a, b) -> Polynomial[E] {
  guard !vars_equal(a.vars, b.vars) else { unsafe_polynomial_mul(a, b) }
  let (a_aligned, b_aligned) = mutual_align_vars(a, b)
  unsafe_polynomial_mul(a_aligned, b_aligned)
}

///|
fn[E : Semiring] dense_univariate_coeffs(poly : Polynomial[E]) -> Array[E] raise {
  assert_true(
    poly.vars.length() == 1,
    msg="expected univariate polynomial, got \{poly.vars.length()} vars",
  )
  let mut degree = 0
  for _, term in poly.terms {
    let exp = term.exps[0]
    assert_true(exp >= 0, msg="negative exponent [\{exp}] is not supported")
    if exp > degree {
      degree = exp
    }
  }
  let coeffs = Array::makei(degree + 1, _ => E::zero())
  for _, term in poly.terms {
    coeffs[term.exps[0]] = coeffs[term.exps[0]] + term.coeff
  }
  coeffs
}

///|
fn bigint_to_bigrat(value : BigInt) -> @rational.BigRational {
  @rational.new(value, 1N).unwrap()
}

///|
fn bigint_to_bigrat_polynomial(
  poly : Polynomial[BigInt],
) -> Polynomial[@rational.BigRational] {
  let terms = @hashmap.HashMap::new()
  for key, term in poly.terms {
    terms[key] = Term::{ exps: term.exps, coeff: bigint_to_bigrat(term.coeff) }
  }
  Polynomial::{ vars: poly.vars, terms }
}

///|
fn linear_factor_from_root(
  vars : Array[String],
  root : @rational.BigRational,
) -> Polynomial[@rational.BigRational] raise {
  let x = Polynomial::from_term(vars, [1], rat_one)
  let c = Polynomial::from_constant(vars, -root)
  x + c
}

///|
pub fn Polynomial::to_univariate_coeffs(
  poly : Polynomial[BigInt],
) -> Array[BigInt] raise {
  dense_univariate_coeffs(poly)
}

///|
pub fn Polynomial::eval(
  poly : Polynomial[@rational.BigRational],
  x : @rational.BigRational,
) -> @rational.BigRational raise {
  let coeffs = dense_univariate_coeffs(poly)
  let degree = coeffs.length() - 1
  let mut acc = coeffs[degree]
  for i in 0..<degree {
    let j = degree - 1 - i
    acc = acc * x + coeffs[j]
  }
  acc
}

///|
pub fn Polynomial::find_rational_roots(
  poly : Polynomial[BigInt],
) -> Array[@rational.BigRational] raise {
  let coeffs = poly.to_univariate_coeffs()
  guard coeffs.length() > 1 else { [] }
  rational_root_candidates(coeffs).filter(r => is_root(coeffs, r))
}

///|
pub fn Polynomial::factor_by_rational_roots(
  poly : Polynomial[BigInt],
) -> (Array[@rational.BigRational], Polynomial[@rational.BigRational]) raise {
  let roots = poly.find_rational_roots()
  let mut remainder = bigint_to_bigrat_polynomial(poly)
  let factors = []
  for root in roots {
    while true {
      let linear = linear_factor_from_root(remainder.vars, root)
      match remainder.div_rem(linear) {
        Some((quotient, rem)) => {
          if rem.terms.is_empty() {
            factors.push(root)
            remainder = quotient
          } else {
            break
          }
        }
        None => break
      }
    }
  }
  (factors, remainder)
}

///|
fn assert_univariate_vars(vars : Array[String]) -> Unit raise {
  assert_true(
    vars.length() == 1,
    msg="expected univariate polynomial, got \{vars.length()} vars",
  )
}

///|
fn scale_bigrat_polynomial(
  poly : Polynomial[@rational.BigRational],
  factor : @rational.BigRational,
) -> Polynomial[@rational.BigRational] {
  let terms = @hashmap.HashMap::new()
  for _, term in poly.terms {
    add_term_to_map(terms, term.exps, term.coeff * factor)
  }
  Polynomial::{ vars: poly.vars, terms }
}

///|
fn normalize_monic(
  poly : Polynomial[@rational.BigRational],
) -> Polynomial[@rational.BigRational] {
  guard !poly.terms.is_empty() else { poly }
  let lead = leading_term(poly).unwrap()
  scale_bigrat_polynomial(poly, Field::inv(lead.coeff))
}

///|
fn mul_bigint_by_nonnegative_int(value : BigInt, times : Int) -> BigInt {
  let mut acc = 0N
  for _ in 0..<times {
    acc = acc + value
  }
  acc
}

///|
fn derivative_univariate_bigint(poly : Polynomial[BigInt]) -> Polynomial[BigInt] raise {
  assert_univariate_vars(poly.vars)
  let terms = @hashmap.HashMap::new()
  for _, term in poly.terms {
    let exp = term.exps[0]
    assert_true(exp >= 0, msg="negative exponent [\{exp}] is not supported")
    guard exp > 0 else { continue }
    let coeff = mul_bigint_by_nonnegative_int(term.coeff, exp)
    add_term_to_map(terms, [exp - 1], coeff)
  }
  Polynomial::{ vars: poly.vars, terms }
}

///|
pub fn Polynomial::gcd(
  a : Polynomial[@rational.BigRational],
  b : Polynomial[@rational.BigRational],
) -> Polynomial[@rational.BigRational] raise {
  assert_univariate_vars(a.vars)
  assert_univariate_vars(b.vars)
  assert_true(
    vars_equal(a.vars, b.vars),
    msg="gcd requires the same variable order",
  )
  let mut x = normalize_monic(a)
  let mut y = normalize_monic(b)
  if x.terms.is_empty() {
    return y
  }
  if y.terms.is_empty() {
    return x
  }
  while !y.terms.is_empty() {
    let (_, r) = x.div_rem(y).unwrap()
    x = y
    y = normalize_monic(r)
  }
  normalize_monic(x)
}

///|
pub fn Polynomial::square_free_part(
  poly : Polynomial[BigInt],
) -> Polynomial[@rational.BigRational] raise {
  assert_univariate_vars(poly.vars)
  let p = bigint_to_bigrat_polynomial(poly)
  guard !p.terms.is_empty() else { p }
  let dp = bigint_to_bigrat_polynomial(derivative_univariate_bigint(poly))
  guard !dp.terms.is_empty() else { normalize_monic(p) }
  let g = p.gcd(dp)
  let (q, r) = p.div_rem(g).unwrap()
  assert_true(
    r.terms.is_empty(),
    msg="square_free_part division should have zero remainder",
  )
  normalize_monic(q)
}

///|
fn max_exp_in_poly(poly : Polynomial[BigInt]) -> Int raise {
  let mut max_exp = 0
  for _, term in poly.terms {
    for exp in term.exps {
      assert_true(exp >= 0, msg="negative exponent [\{exp}] is not supported")
      if exp > max_exp {
        max_exp = exp
      }
    }
  }
  max_exp
}

///|
fn kronecker_weights(arity : Int, base : Int) -> Array[Int] {
  let weights = []
  let mut weight = 1
  for _ in 0..<arity {
    weights.push(weight)
    weight = weight * base
  }
  weights
}

///|
fn encode_kronecker_exps(exps : Array[Int], weights : Array[Int]) -> Int {
  let mut code = 0
  for i in 0..<exps.length() {
    code = code + exps[i] * weights[i]
  }
  code
}

///|
fn try_decode_kronecker_exps(
  code : Int,
  arity : Int,
  base : Int,
) -> Array[Int]? {
  guard code >= 0 else { None }
  guard base >= 2 else { None }
  let exps = Array::makei(arity, _ => 0)
  let mut x = code
  for i in 0..<arity {
    exps[i] = x % base
    x = x / base
  }
  if x == 0 {
    Some(exps)
  } else {
    None
  }
}

///|
fn kronecker_embed_univariate(
  poly : Polynomial[BigInt],
  weights : Array[Int],
) -> Polynomial[@rational.BigRational] {
  let terms = @hashmap.HashMap::new()
  for _, term in poly.terms {
    let encoded_exp = encode_kronecker_exps(term.exps, weights)
    add_term_to_map(terms, [encoded_exp], bigint_to_bigrat(term.coeff))
  }
  Polynomial::{ vars: ["_k"], terms }
}

///|
fn kronecker_decode_univariate(
  poly : Polynomial[@rational.BigRational],
  vars : Array[String],
  base : Int,
) -> Polynomial[@rational.BigRational]? {
  let terms = @hashmap.HashMap::new()
  for _, term in poly.terms {
    guard term.exps.length() == 1 else { return None }
    let encoded_exp = term.exps[0]
    match try_decode_kronecker_exps(encoded_exp, vars.length(), base) {
      Some(exps) => add_term_to_map(terms, exps, term.coeff)
      None => return None
    }
  }
  Some(Polynomial::{ vars, terms })
}

///|
fn divides_exact(
  dividend : Polynomial[@rational.BigRational],
  divisor : Polynomial[@rational.BigRational],
) -> Bool {
  guard !divisor.terms.is_empty() else { false }
  match dividend.div_rem(divisor) {
    Some((_, remainder)) => remainder.terms.is_empty()
    None => false
  }
}

///|
pub fn Polynomial::gcd_multivariate(
  a : Polynomial[BigInt],
  b : Polynomial[BigInt],
) -> Polynomial[@rational.BigRational] raise {
  let (a, b) = if vars_equal(a.vars, b.vars) {
    (a, b)
  } else {
    mutual_align_vars(a, b)
  }

  let a_q = bigint_to_bigrat_polynomial(a)
  let b_q = bigint_to_bigrat_polynomial(b)
  if a_q.terms.is_empty() {
    return normalize_monic(b_q)
  }
  if b_q.terms.is_empty() {
    return normalize_monic(a_q)
  }

  if a.vars.length() == 1 {
    return a_q.gcd(b_q)
  }

  let max_exp_a = max_exp_in_poly(a)
  let max_exp_b = max_exp_in_poly(b)
  let mut base = if max_exp_a > max_exp_b { max_exp_a } else { max_exp_b }
  base = base + 1
  if base < 2 {
    base = 2
  }

  let weights = kronecker_weights(a.vars.length(), base)
  let a_u = kronecker_embed_univariate(a, weights)
  let b_u = kronecker_embed_univariate(b, weights)
  let g_u = a_u.gcd(b_u)

  match kronecker_decode_univariate(g_u, a.vars, base) {
    Some(candidate) => {
      let candidate = normalize_monic(candidate)
      if divides_exact(a_q, candidate) && divides_exact(b_q, candidate) {
        candidate
      } else {
        Polynomial::from_constant(a.vars, rat_one)
      }
    }
    None => Polynomial::from_constant(a.vars, rat_one)
  }
}

///|
fn derivative_multivariate_bigint(
  poly : Polynomial[BigInt],
  var_index : Int,
) -> Polynomial[BigInt] raise {
  assert_true(
    0 <= var_index && var_index < poly.vars.length(),
    msg="variable index out of range: \{var_index}",
  )
  let terms = @hashmap.HashMap::new()
  for _, term in poly.terms {
    let exp = term.exps[var_index]
    assert_true(exp >= 0, msg="negative exponent [\{exp}] is not supported")
    guard exp > 0 else { continue }
    let exps = Array::makei(term.exps.length(), i => term.exps[i])
    exps[var_index] = exp - 1
    let coeff = mul_bigint_by_nonnegative_int(term.coeff, exp)
    add_term_to_map(terms, exps, coeff)
  }
  Polynomial::{ vars: poly.vars, terms }
}

///|
fn square_free_part_multivariate_heuristic(
  poly : Polynomial[BigInt],
) -> Polynomial[@rational.BigRational] raise {
  guard !poly.terms.is_empty() else { bigint_to_bigrat_polynomial(poly) }
  guard poly.vars.length() > 1 else { poly.square_free_part() }

  let p = bigint_to_bigrat_polynomial(poly)
  let mut common_g : Polynomial[@rational.BigRational]? = None
  for var_index in 0..<poly.vars.length() {
    let dp = derivative_multivariate_bigint(poly, var_index)
    guard !dp.terms.is_empty() else { continue }
    let g = poly.gcd_multivariate(dp)
    match common_g {
      None => common_g = Some(g)
      Some(curr) => {
        match (try_bigrat_to_bigint_polynomial(curr), try_bigrat_to_bigint_polynomial(g)) {
          (Some(curr_i), Some(g_i)) => common_g = Some(curr_i.gcd_multivariate(g_i))
          _ => ()
        }
      }
    }
  }

  match common_g {
    None => normalize_monic(p)
    Some(g) => {
      let g = normalize_monic(g)
      let (q, r) = p.div_rem(g).unwrap()
      if r.terms.is_empty() {
        normalize_monic(q)
      } else {
        normalize_monic(p)
      }
    }
  }
}

///|
fn trim_trailing_zero_coeffs(coeffs : Array[BigInt]) -> Array[BigInt] {
  guard !coeffs.is_empty() else { [0N] }
  let mut last = coeffs.length() - 1
  while last > 0 && coeffs[last] == 0N {
    last = last - 1
  }
  Array::makei(last + 1, i => coeffs[i])
}

///|
fn specialized_univariate_coeffs_at_zero(
  poly : Polynomial[BigInt],
  target_var_index : Int,
) -> Array[BigInt] raise {
  assert_true(
    0 <= target_var_index && target_var_index < poly.vars.length(),
    msg="variable index out of range: \{target_var_index}",
  )

  let mut degree = 0
  let mut has_term = false
  for _, term in poly.terms {
    let mut survives = true
    for i in 0..<term.exps.length() {
      let exp = term.exps[i]
      assert_true(exp >= 0, msg="negative exponent [\{exp}] is not supported")
      if i != target_var_index && exp != 0 {
        survives = false
        break
      }
    }
    guard survives else { continue }
    has_term = true
    let exp = term.exps[target_var_index]
    if exp > degree {
      degree = exp
    }
  }

  guard has_term else { [0N] }
  let coeffs = Array::makei(degree + 1, _ => 0N)
  for _, term in poly.terms {
    let mut survives = true
    for i in 0..<term.exps.length() {
      if i != target_var_index && term.exps[i] != 0 {
        survives = false
        break
      }
    }
    guard survives else { continue }
    let exp = term.exps[target_var_index]
    coeffs[exp] = coeffs[exp] + term.coeff
  }
  trim_trailing_zero_coeffs(coeffs)
}

///|
fn specialized_rational_roots_at_zero(
  poly : Polynomial[BigInt],
  target_var_index : Int,
) -> Array[@rational.BigRational] raise {
  let coeffs = specialized_univariate_coeffs_at_zero(poly, target_var_index)
  guard coeffs.length() > 1 else { [] }
  rational_root_candidates(coeffs).filter(r => is_root(coeffs, r))
}

///|
fn try_bigrat_to_bigint_polynomial(
  poly : Polynomial[@rational.BigRational],
) -> Polynomial[BigInt]? {
  let terms = @hashmap.HashMap::new()
  for _, term in poly.terms {
    guard term.coeff.is_integer() else { return None }
    add_term_to_map(terms, term.exps, term.coeff.trunc())
  }
  Some(Polynomial::{ vars: poly.vars, terms })
}

///|
fn polynomial_content_bigint(poly : Polynomial[BigInt]) -> BigInt {
  guard !poly.terms.is_empty() else { 0N }
  let mut content = 0N
  for _, term in poly.terms {
    let coeff_abs = @prime.abs(term.coeff)
    content = if content == 0N { coeff_abs } else { gcd(content, coeff_abs) }
  }
  content
}

///|
fn divide_bigint_polynomial_by_scalar(
  poly : Polynomial[BigInt],
  scalar : BigInt,
) -> Polynomial[BigInt] raise {
  assert_true(scalar != 0N, msg="scalar must be non-zero")
  let terms = @hashmap.HashMap::new()
  for _, term in poly.terms {
    assert_true(
      term.coeff % scalar == 0N,
      msg="coefficient is not divisible by scalar",
    )
    add_term_to_map(terms, term.exps, term.coeff / scalar)
  }
  Polynomial::{ vars: poly.vars, terms }
}

///|
fn linear_factor_for_var(
  vars : Array[String],
  var_index : Int,
  root : @rational.BigRational,
) -> Polynomial[@rational.BigRational] raise {
  assert_true(0 <= var_index && var_index < vars.length())
  let exps = Array::makei(vars.length(), i => if i == var_index { 1 } else { 0 })
  let var_term = Polynomial::from_term(vars, exps, rat_one)
  let const_term = Polynomial::from_constant(vars, -root)
  var_term + const_term
}

///|
pub fn Polynomial::factor_multivariate(
  poly : Polynomial[BigInt],
) -> (Array[Polynomial[@rational.BigRational]], Polynomial[@rational.BigRational]) raise {
  guard !poly.terms.is_empty() else { ([], bigint_to_bigrat_polynomial(poly)) }

  let mut remainder = square_free_part_multivariate_heuristic(poly)
  let factors = []

  for var_index in 0..<poly.vars.length() {
    let roots = specialized_rational_roots_at_zero(poly, var_index)
    for root in roots {
      let factor = linear_factor_for_var(poly.vars, var_index, root)
      while true {
        match remainder.div_rem(factor) {
          Some((quotient, rem)) => {
            if rem.terms.is_empty() {
              factors.push(factor)
              remainder = normalize_monic(quotient)
            } else {
              break
            }
          }
          None => break
        }
      }
    }
  }

  (factors, normalize_monic(remainder))
}

///|
pub struct FactorMultivariateFullReport {
  content : @rational.BigRational
  factors : Array[Polynomial[@rational.BigRational]]
  primitive_remainder : Polynomial[@rational.BigRational]
  remainder : Polynomial[@rational.BigRational]
  rounds : Int
  stop_reason : String
  is_reconstruction_valid : Bool
  trace : Message
}

///|
fn polynomial_equal_bigrat(
  a : Polynomial[@rational.BigRational],
  b : Polynomial[@rational.BigRational],
) -> Bool {
  (a - b).terms.is_empty()
}

///|
fn reconstruct_from_factor_multivariate_full_report_parts(
  content : @rational.BigRational,
  factors : Array[Polynomial[@rational.BigRational]],
  primitive_remainder : Polynomial[@rational.BigRational],
) -> Polynomial[@rational.BigRational] {
  let mut reconstructed = primitive_remainder
  for factor in factors {
    reconstructed = reconstructed * factor
  }
  scale_bigrat_polynomial(reconstructed, content)
}

///|
pub fn Polynomial::reconstruct_from_report(
  report : FactorMultivariateFullReport,
) -> Polynomial[@rational.BigRational] {
  reconstruct_from_factor_multivariate_full_report_parts(
    report.content,
    report.factors,
    report.primitive_remainder,
  )
}

///|
pub fn Polynomial::factor_multivariate_full_report(
  poly : Polynomial[BigInt],
) -> FactorMultivariateFullReport raise {
  guard !poly.terms.is_empty() else {
    let remainder = bigint_to_bigrat_polynomial(poly)
    let trace = Child(
      Text(": factor_multivariate_full_report"),
      group([
        Label("content", "0"),
        Label("rounds", "0"),
        Label("stop_reason", "zero_polynomial"),
        Label("factors", "[]"),
        Label("primitive_remainder", remainder.unicodify()),
        Label("remainder", remainder.unicodify()),
      ]),
    )
    return FactorMultivariateFullReport::{
      content: rat_zero,
      factors: [],
      primitive_remainder: remainder,
      remainder,
      rounds: 0,
      stop_reason: "zero_polynomial",
      is_reconstruction_valid: true,
      trace,
    }
  }

  let content_bigint = polynomial_content_bigint(poly)
  let content = bigint_to_bigrat(content_bigint)
  let primitive = if content_bigint == 0N || content_bigint == 1N {
    poly
  } else {
    divide_bigint_polynomial_by_scalar(poly, content_bigint)
  }

  let factors = []
  let round_messages = []
  let mut remainder = bigint_to_bigrat_polynomial(primitive)
  let mut rounds = 0
  let mut stop_reason = "unknown"

  while true {
    match try_bigrat_to_bigint_polynomial(remainder) {
      None => {
        stop_reason = "non_integral_remainder"
        break
      }
      Some(step_input) => {
        let (step_factors, step_remainder) = step_input.factor_multivariate()
        rounds = rounds + 1
        round_messages.push(
          Child(
            Text("round \{rounds}"),
            group([
              Label("factors", step_factors.map(f => f.unicodify()).to_string()),
              Label("remainder", step_remainder.unicodify()),
            ]),
          ),
        )

        if step_factors.is_empty() {
          remainder = step_remainder
          stop_reason = "no_new_factors"
          break
        }
        for factor in step_factors {
          factors.push(factor)
        }
        remainder = step_remainder
      }
    }
  }

  let remainder = normalize_monic(remainder)
  let trace_data = [
    Label("content", ToUnicode::unicodify(content)),
    Label("rounds", "\{rounds}"),
    Label("stop_reason", stop_reason),
    Label("factors", factors.map(f => f.unicodify()).to_string()),
    Label("primitive_remainder", remainder.unicodify()),
    Label("remainder", remainder.unicodify()),
  ]
  for msg in round_messages {
    trace_data.push(msg)
  }
  let trace = Child(Text(": factor_multivariate_full_report"), group(trace_data))
  let reconstructed = reconstruct_from_factor_multivariate_full_report_parts(
    content,
    factors,
    remainder,
  )
  let original = bigint_to_bigrat_polynomial(poly)
  let is_reconstruction_valid = polynomial_equal_bigrat(reconstructed, original)

  FactorMultivariateFullReport::{
    content,
    factors,
    primitive_remainder: remainder,
    remainder,
    rounds,
    stop_reason,
    is_reconstruction_valid,
    trace,
  }
}

///|
pub fn Polynomial::factor_multivariate_full(
  poly : Polynomial[BigInt],
) -> (Array[Polynomial[@rational.BigRational]], Polynomial[@rational.BigRational]) raise {
  let report = poly.factor_multivariate_full_report()
  (report.factors, report.remainder)
}

///|
fn exps_divides(divisor : Array[Int], dividend : Array[Int]) -> Bool {
  for i in 0..<divisor.length() {
    if divisor[i] > dividend[i] {
      return false
    }
  }
  true
}

///|
fn[E] leading_term(poly : Polynomial[E]) -> Term[E]? {
  let mut best : Term[E]? = None
  for _, term in poly.terms {
    match best {
      None => best = Some(term)
      Some(best_term) =>
        if compare_exps_desc(term.exps, best_term.exps) < 0 {
          best = Some(term)
        }
    }
  }
  best
}

///|
fn[E] clone_terms(
  terms : @hashmap.HashMap[String, Term[E]],
) -> @hashmap.HashMap[String, Term[E]] {
  let copy = @hashmap.HashMap::new()
  for key, term in terms {
    copy[key] = Term::{ exps: term.exps, coeff: term.coeff }
  }
  copy
}

///|
pub fn[E : Field] Polynomial::div_rem(
  dividend : Polynomial[E],
  divisor : Polynomial[E],
) -> (Polynomial[E], Polynomial[E])? {
  guard !divisor.terms.is_empty() else { None }
  let (dividend, divisor) = if vars_equal(dividend.vars, divisor.vars) {
    (dividend, divisor)
  } else {
    mutual_align_vars(dividend, divisor)
  }
  let dividend = Polynomial::{
    vars: dividend.vars,
    terms: clone_terms(dividend.terms),
  }
  let vars = dividend.vars
  let quotient = Polynomial::zero(vars)
  let remainder = Polynomial::zero(vars)
  let lt_divisor = leading_term(divisor).unwrap()
  while !dividend.terms.is_empty() {
    let lt_dividend = leading_term(dividend).unwrap()
    if exps_divides(lt_divisor.exps, lt_dividend.exps) {
      let exp_q = Array::makei(vars.length(), fn(i) {
        lt_dividend.exps[i] - lt_divisor.exps[i]
      })
      let coeff_q = lt_dividend.coeff.mul(lt_divisor.coeff.inv())
      add_term_to_map(quotient.terms, exp_q, coeff_q)
      for _, term in divisor.terms {
        let new_exps = Array::makei(vars.length(), fn(i) {
          term.exps[i] + exp_q[i]
        })
        let new_coeff = term.coeff.mul(coeff_q).neg()
        add_term_to_map(dividend.terms, new_exps, new_coeff)
      }
    } else {
      add_term_to_map(remainder.terms, lt_dividend.exps, lt_dividend.coeff)
      let key = monomial_key(lt_dividend.exps)
      dividend.terms.remove(key)
    }
  }
  Some((quotient, remainder))
}

///|
fn exps_total_degree(exps : Array[Int]) -> Int {
  let mut sum = 0
  for exp in exps {
    sum = sum + exp
  }
  sum
}

///|
fn compare_exps_desc(a : Array[Int], b : Array[Int]) -> Int {
  let deg_a = exps_total_degree(a)
  let deg_b = exps_total_degree(b)
  if deg_a > deg_b {
    -1
  } else if deg_a < deg_b {
    1
  } else {
    for i in 0..<a.length() {
      if a[i] > b[i] {
        return -1
      }
      if a[i] < b[i] {
        return 1
      }
    }
    0
  }
}

///|
fn[E] sort_terms_desc(terms : Array[Term[E]]) -> Unit {
  for i in 1..<terms.length() {
    let current = terms[i]
    let mut j = i
    while j > 0 && compare_exps_desc(current.exps, terms[j - 1].exps) < 0 {
      terms[j] = terms[j - 1]
      j = j - 1
    }
    terms[j] = current
  }
}

///|
fn unicodify_multivariate_monomial(
  vars : Array[String],
  exps : Array[Int],
  coeff : String,
) -> String {
  guard !(coeff == "0") else { "0" }
  let var_parts = []
  for i in 0..<vars.length() {
    let exp = exps[i]
    guard exp != 0 else { continue }
    if exp == 1 {
      var_parts.push(vars[i])
    } else {
      var_parts.push(vars[i] + to_super_string("\{exp}"))
    }
  }
  let var_part = join(var_parts, separator="")
  guard !(var_part == "") else { coeff }
  guard !(coeff == "1") else { var_part }
  coeff + var_part
}

///|
pub impl[E : ToUnicode] ToUnicode for Polynomial[E] with unicodify(
  self : Polynomial[E],
) -> String {
  guard !self.terms.is_empty() else { "0" }
  let terms = []
  for _, term in self.terms {
    terms.push(term)
  }
  sort_terms_desc(terms)
  let monomials = terms
    .map(term => {
      let coeff = term.coeff.unicodify()
      unicodify_multivariate_monomial(self.vars, term.exps, coeff)
    })
    .filter(s => s != "0")
  guard !monomials.is_empty() else { "0" }
  monomials[1:].fold(init=monomials[0], (s, t) => {
    guard t.has_prefix("-") else { "\{s} + \{t}" }
    "\{s} - \{str_abs(t)}"
  })
}

///|
test "Polynomial addition and multiplication with BigInt" {
  let vars = ["x", "y"]
  let p = Polynomial::from_term(vars, [1, 0], 2N) -
    Polynomial::from_term(vars, [0, 1], 3N)
  assert_eq(p.unicodify(), "2x - 3y")
  let q = Polynomial::from_term(vars, [1, 0], 1N) +
    Polynomial::from_constant(vars, 4N)
  assert_eq(q.unicodify(), "x + 4")
  assert_eq((p * q).unicodify(), "2x² - 3xy + 8x - 12y")
}

///|
test "Polynomial division with BigRat" {
  let vars = ["x"]
  let u = Polynomial::from_term(vars, [2], rat_one) +
    Polynomial::from_constant(vars, rat_one)
  let v = Polynomial::from_term(vars, [1], rat_one) +
    Polynomial::from_constant(vars, rat_one)
  let (q, r) = u.div_rem(v).unwrap()
  assert_eq(
    "\{u.unicodify()} = (\{v.unicodify()})(\{q.unicodify()}) + \{r.unicodify()}",
    "x² + 1 = (x + 1)(x - 1) + 2",
  )
}
