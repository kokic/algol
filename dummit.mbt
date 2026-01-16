///|
/// ### References
/// [1] Dummit, David S. "Solving solvable quintics." mathematics of computation 57.195 (1991): 387-401.
fn quintic_ab_coeff(a : BigInt, b : BigInt) -> Array[BigInt] {
  let a6 = (1 : BigInt)
  let (a5, a4) = (8N * a, 40N * a.pow(2))
  let (a3, a2) = (160N * a.pow(3), 400N * a.pow(4))
  let a1 = 512N * a.pow(5) - 3125 * b.pow(4)
  let a0 = 256N * a.pow(6) - 9375 * a * b.pow(4)
  [a6, a5, a4, a3, a2, a1, a0]
}

///|
/// Return quintic equation x⁵ + ax + b is solvable
pub fn is_quintic_ab_solvable(a : BigInt, b : BigInt) -> Bool {
  let mut msg : Message = Empty
  let coeffs = quintic_ab_coeff(a, b)
  msg += Label(
    "rsolvents",
    unicodify_univariate_polynomial(coeffs, descent=true),
  )
  let roots = rational_root_candidates(coeffs)
  let roots = roots.filter(r => is_root(coeffs, r))
  msg += Label("roots", roots.to_string())
  msg.exit("is_quintic_ab_solvable", !roots.is_empty())
}

///|
test {
  // println(is_quintic_ab_solvable(-1, 1))
  println(is_quintic_ab_solvable(-5, 12))
  println(tracing.msg.display(2))
}
