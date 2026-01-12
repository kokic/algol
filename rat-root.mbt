///|
/// All positive m such that m | n
pub fn generate_divisors(n : BigInt) -> Array[BigInt] {
  guard n != 0 else { [] }
  let factors = factor(n)
  let mut divisors = [1N]
  for p, exp in factors {
    let p_big = p
    let mut power = 1N
    // precompute powers [1, p, p^2, ..., p^exp]
    let powers = [power]
    for _ in 1..=exp {
      power = power * p_big
      powers.push(power)
    }
    let new_divisors = []
    for d in divisors {
      for pow in powers {
        new_divisors.push(d * pow)
      }
    }
    divisors = new_divisors
  }
  divisors
}

///|
/// ### Parameters
/// * `coeffs` : [a_0, a_1, ..., a_n]
pub fn cauchy_bound(coeffs : Array[BigInt]) -> BigInt raise {
  let n = coeffs.length() - 1
  assert_true(n >= 1, msg="polynomial degree [\{n}] must be at least 1")
  let a_n = coeffs[n]
  assert_true(a_n != 0, msg="leading coefficient [\{a_n}] must be non-zero")
  let mut max_ratio = 0N
  for i in 0..=n {
    let a_i_abs = @prime.abs(coeffs[i])
    let a_n_abs = @prime.abs(a_n)
    let ratio = if a_i_abs == 0 { 0N } else { a_i_abs / a_n_abs }
    if ratio > max_ratio {
      max_ratio = ratio
    }
  }
  1N + max_ratio
}

///|
pub fn generate_divisors_bounded(n : BigInt, bound : BigInt) -> Array[BigInt] {
  guard n != 0 else { [] }
  guard bound >= 1 else { [] }
  let factors = factor(@prime.abs(n))
  let mut divisors = [1N]
  for p, exp in factors {
    let p_big = p
    let mut power = 1N
    // precompute powers [1, p, p^2, ..., p^exp]
    let powers = [1N]
    for _ in 1..=exp {
      power = power * p_big
      if power > bound {
        break
      }
      powers.push(power)
    }
    let new_divisors = []
    for d in divisors {
      for pow in powers {
        let candidate = d * pow
        if candidate <= bound {
          new_divisors.push(candidate)
        }
        // Note: since powers are increasing and d ≥ 1,
        // once candidate > bound, later pow will be larger — but we can't break
        // because d varies. So no inner break.
      }
    }
    divisors = new_divisors
  }
  divisors
}

///|
test "Generate divisors" {
  assert_eq(generate_divisors(52), [1, 2, 4, 13, 26, 52])
}

///|
/// ### Parameters
/// * `coeffs` : [a_0, a_1, ..., a_n]
pub fn rational_root_candidates(
  coeffs : Array[BigInt],
) -> Array[@rational.BigRational] {
  let length = coeffs.length()
  guard length >= 1 else { [] }

  // degree >= 1
  let a_0 = coeffs[0]
  let a_n = coeffs[length - 1]
  guard !a_0.is_zero() else {
    let zero = @rational.new(0N, 1N).unwrap()
    [zero]
  }
  let bound = try! cauchy_bound(coeffs)
  let ps = generate_divisors(@prime.abs(a_0))
  let qs = generate_divisors(@prime.abs(a_n))
  // Should be replaced with @hashset.HashSet in the future
  let cases : Array[@rational.BigRational] = Array::new(
    capacity=2 * ps.length() * qs.length(),
  )
  for p in ps {
    for q in qs {
      // Prune using Cauchy bound: |p/q| <= bound iff p <= bound * q
      if p <= bound * q {
        cases.push(@rational.new(p, q).unwrap())
        cases.push(@rational.new(-p, q).unwrap())
      }
    }
  }
  cases.sort()
  cases.dedup()
  cases
}

///|
pub fn is_root(coeffs : Array[BigInt], r : @rational.BigRational) -> Bool {
  guard !coeffs.is_empty() else { false }
  let degree = coeffs.length() - 1
  let mut acc = @rational.new(coeffs[degree], 1).unwrap()
  for i in 0..<degree {
    let j = degree - 1 - i
    acc = acc * r + @rational.new(coeffs[j], 1).unwrap()
  }
  acc == big_rat_zero
}

///|
test "120x^4 - 166x^3 + x^2 + 74x - 24" {
  let coeffs = [120N, -166, 1, 74, -24].rev()
  let roots = rational_root_candidates(coeffs).filter(r => is_root(coeffs, r))
  inspect(roots, content="[-2/3, 1/2, 3/4, 4/5]")
}
