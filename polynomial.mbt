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
test "BigInt: Polynomial add & mul" {
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
test "BigRat: Polynomial div" {
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
