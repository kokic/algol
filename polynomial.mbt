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
pub impl[E : Semiring] Add for Polynomial[E] with add(a, b) -> Polynomial[E] {
  guard !vars_equal(a.vars, b.vars) else { unsafe_polynomial_add(a, b) }
  let (a_aligned, b_aligned) = mutual_align_vars(a, b)
  unsafe_polynomial_add(a_aligned, b_aligned)
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
test "BigInt-Polynomial add & mul" {
  let vars = ["x", "y"]
  // 2x + 3y
  let p = Polynomial::from_term(vars, [1, 0], 2N) +
    Polynomial::from_term(vars, [0, 1], 3N)

  // x + 4
  let q = Polynomial::from_term(vars, [1, 0], 1N) +
    Polynomial::from_constant(vars, 4N)

  // 2x² + 3xy + 8x + 12y
  let r = p * q
  assert_eq(r.coeff([2, 0]), 2N)
  assert_eq(r.coeff([1, 1]), 3N)
  assert_eq(r.coeff([1, 0]), 8N)
  assert_eq(r.coeff([0, 1]), 12N)
}
