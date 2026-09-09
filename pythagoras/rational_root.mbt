///|
/// ### Parameters
/// * `coeffs` : [a_0, a_1, ..., a_n]
/// Raises `InvalidInput` for degree below one or a zero leading coefficient.
pub fn cauchy_bound(coeffs : Array[BigInt]) -> BigInt raise InvokeError {
  let n = coeffs.length() - 1
  guard n >= 1 else {
    raise InvalidInput(msg="polynomial degree [\{n}] must be at least 1")
  }
  let a_n = coeffs[n]
  guard a_n != 0 else {
    raise InvalidInput(msg="leading coefficient [\{a_n}] must be non-zero")
  }
  let a_n_abs = abs(a_n)
  let mut max_ratio = 0N
  // ceil(|a_i| / |a_n|): floor division would make the bound too tight
  for i in 0..<n {
    let ai_abs = abs(coeffs[i])
    let ratio = if ai_abs == 0 { 0N } else { (ai_abs + a_n_abs - 1) / a_n_abs }
    if ratio > max_ratio {
      max_ratio = ratio
    }
  }
  1N + max_ratio
}

///|
pub fn generate_divisors_bounded(
  n : BigInt,
  bound : BigInt,
) -> Array[BigInt] raise InvokeError {
  guard n != 0 else { [] }
  guard bound >= 1 else { [] }
  let factors = factor(abs(n))
  let mut divisors = [1N]
  for p, exp in factors {
    let p_big = p
    let mut power = 1N
    // precompute powers [1, p, p^2, ..., p^exp]
    let powers = [1N]
    for _ in 1..<=exp {
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
        // powers are increasing, so once candidate > bound, later pows are too
        guard candidate <= bound else { break }
        new_divisors.push(candidate)
      }
    }
    divisors = new_divisors
  }
  divisors
}

///|
/// ### Parameters
/// * `coeffs` : [a_0, a_1, ..., a_n]
/// High-degree zero coefficients are ignored. Nonzero constants have no
/// candidates. Empty or all-zero coefficients raise `InvalidInput`, since
/// the zero polynomial has infinitely many rational roots.
/// Factor search failure is propagated rather than returning incomplete candidates.
pub fn rational_root_candidates(
  coeffs : Array[BigInt],
) -> Array[@rational.BigRational] raise InvokeError {
  let mut end = coeffs.length()
  while end > 0 && coeffs[end - 1] == 0 {
    end -= 1
  }
  guard end > 0 else {
    raise InvalidInput(
      msg="the zero polynomial has infinitely many rational roots",
    )
  }
  let mut start = 0
  while start < end && coeffs[start] == 0 {
    start += 1
  }
  let cases : Array[@rational.BigRational] = [..if start > 0 { [rat_zero] }]
  guard end - start > 1 else { cases }
  let coeffs = Array::makei(end - start, i => coeffs[start + i])
  let a_0 = coeffs[0]
  let a_n = coeffs[coeffs.length() - 1]
  let bound = cauchy_bound(coeffs)
  let ps = divisors(abs(a_0), factor)
  let qs = divisors(abs(a_n), factor)
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
/// ### Parameters
/// * `coeffs` : [a_0, a_1, ..., a_n]
pub fn is_root(coeffs : Array[BigInt], r : @rational.BigRational) -> Bool {
  guard !coeffs.is_empty() else { false }
  let degree = coeffs.length() - 1
  let mut acc = @rational.new(coeffs[degree], 1).unwrap()
  for i in 0..<degree {
    let j = degree - 1 - i
    acc = acc * r + @rational.new(coeffs[j], 1).unwrap()
  }
  acc == rat_zero
}

///|
test "120x^4 - 166x^3 + x^2 + 74x - 24" {
  let coeffs = [120N, -166, 1, 74, -24].rev()
  let roots = rational_root_candidates(coeffs).filter(r => is_root(coeffs, r))
  debug_inspect(
    roots,
    content=(
      #|[-2/3, 1/2, 3/4, 4/5]
    ),
  )
}

///|
test "x^3 + x^2 - 2x has roots -2, 0, 1" {
  let coeffs = [0N, -2, 1, 1]
  let roots = rational_root_candidates(coeffs).filter(r => is_root(coeffs, r))
  debug_inspect(
    roots,
    content=(
      #|[-2, 0, 1]
    ),
  )
}

///|
test "-2x + 0 has root 0" {
  let coeffs = [0N, -2]
  let roots = rational_root_candidates(coeffs).filter(r => is_root(coeffs, r))
  debug_inspect(
    roots,
    content=(
      #|[0]
    ),
  )
}
