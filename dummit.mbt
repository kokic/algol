///|
/// Returns `[a6, a5, a4, a3, a2, a1, a0]`
/// ### References
/// [1] Dummit, David S. "Solving solvable quintics." mathematics of computation 57.195 (1991): 387-401.
fn quintic_ab_coeff(a : BigInt, b : BigInt) -> Array[BigInt] {
  let a6 = 1N
  let (a5, a4) = (8N * a, 40N * a.pow(2))
  let (a3, a2) = (160N * a.pow(3), 400N * a.pow(4))
  let a1 = 512N * a.pow(5) - 3125 * b.pow(4)
  let a0 = 256N * a.pow(6) - 9375 * a * b.pow(4)
  [a6, a5, a4, a3, a2, a1, a0]
}

///|
pub struct QuinticSolvabilityReport {
  check_name : String
  resolvent : String
  roots : Array[@rational.BigRational]
  solvable : Bool
  msg : Message
} derive(Show)

///|
fn build_quintic_solvability_report(
  check_name : String,
  descent_coeffs : Array[BigInt],
) -> QuinticSolvabilityReport {
  let ascent_coeffs = descent_coeffs.rev()
  let roots = rational_root_candidates(ascent_coeffs)
    .filter(r => is_root(ascent_coeffs, r))
  let resolvent = unicodify_univariate_polynomial(descent_coeffs)
  let proof_log = Child(
    Text(": \{check_name}"),
    group([Label("resolvent", resolvent), Label("roots", roots.to_string())]),
  )
  QuinticSolvabilityReport::{
    check_name,
    resolvent,
    roots,
    solvable: !roots.is_empty(),
    msg: proof_log,
  }
}

///|
pub fn check_quintic_ab_solvability(
  a : BigInt,
  b : BigInt,
) -> QuinticSolvabilityReport {
  build_quintic_solvability_report("is_quintic_ab_solvable", quintic_ab_coeff(a, b))
}

///|
/// Return whether quintic equation x^5 + ax + b is solvable.
pub fn is_quintic_ab_solvable(a : BigInt, b : BigInt) -> Bool {
  let report = check_quintic_ab_solvability(a, b)
  tracing.add(report.msg)
  report.solvable
}

///|
/// Returns `[a6, a5, a4, a3, a2, a1, a0]`
/// ### References
/// [1] Dummit, David S. "Solving solvable quintics." mathematics of computation 57.195 (1991): 387-401.
fn quintic_pqrs_coeff(
  p : BigInt,
  q : BigInt,
  r : BigInt,
  s : BigInt,
) -> Array[BigInt] {
  let a6 = 1N
  let a5 = 8N * r
  let a4 = 2N * p * q.pow(2) - 6 * p.pow(2) * r + 40 * r.pow(2) - 50 * q * s
  let a3 = 2N * q.pow(4) +
    21 * p * q.pow(2) * r -
    40 * p.pow(2) * r.pow(2) +
    160 * r.pow(3) -
    15 * p.pow(2) * q * s -
    400 * q * r * s +
    125 * p * s.pow(2)
  let a2 = p.pow(2) * q.pow(4) -
    6 * p.pow(3) * q.pow(2) * r -
    8 * q.pow(4) * r +
    9 * p.pow(4) * r.pow(2) +
    76 * p * q.pow(2) * r.pow(2) -
    136 * p.pow(2) * r.pow(3) +
    400 * r.pow(4) -
    50 * p * q.pow(3) * s +
    90 * p.pow(2) * q * r * s -
    1400 * q * r.pow(2) * s +
    625 * q.pow(2) * s.pow(2) +
    500 * p * r * s.pow(2)
  let a1 = -2N * p * q.pow(6) +
    19 * p.pow(2) * q.pow(4) * r -
    51 * p.pow(3) * q.pow(2) * r.pow(2) +
    3 * q.pow(4) * r.pow(2) +
    32 * p.pow(4) * r.pow(3) +
    76 * p * q.pow(2) * r.pow(3) -
    256 * p.pow(2) * r.pow(4) +
    512 * r.pow(5) -
    31 * p.pow(3) * q.pow(3) * s -
    58 * q.pow(5) * s +
    117 * p.pow(4) * q * r * s +
    105 * p * q.pow(3) * r * s +
    260 * p.pow(2) * q * r.pow(2) * s -
    2400 * q * r.pow(3) * s -
    108 * p.pow(5) * s.pow(2) -
    325 * p.pow(2) * q.pow(2) * s.pow(2) +
    525 * p.pow(3) * r * s.pow(2) +
    2750 * q.pow(2) * r * s.pow(2) -
    500 * p * r.pow(2) * s.pow(2) +
    625 * p * q * s.pow(3) -
    3125 * s.pow(4)
  let a0 = q.pow(8) -
    13 * p * q.pow(6) * r +
    p.pow(5) * q.pow(2) * r.pow(2) +
    65 * p.pow(2) * q.pow(4) * r.pow(2) -
    4 * p.pow(6) * r.pow(3) -
    128 * p.pow(3) * q.pow(2) * r.pow(3) +
    17 * q.pow(4) * r.pow(3) +
    48 * p.pow(4) * r.pow(4) -
    16 * p * q.pow(2) * r.pow(4) -
    192 * p.pow(2) * r.pow(5) +
    256 * r.pow(6) -
    4 * p.pow(5) * q.pow(3) * s -
    12 * p.pow(2) * q.pow(5) * s +
    18 * p.pow(6) * q * r * s +
    12 * p.pow(3) * q.pow(3) * r * s -
    124 * q.pow(5) * r * s +
    196 * p.pow(4) * q * r.pow(2) * s +
    590 * p * q.pow(3) * r.pow(2) * s -
    160 * p.pow(2) * q * r.pow(3) * s -
    1600 * q * r.pow(4) * s -
    27 * p.pow(7) * s.pow(2) -
    150 * p.pow(4) * q.pow(2) * s.pow(2) -
    125 * p * q.pow(4) * s.pow(2) -
    99 * p.pow(5) * r * s.pow(2) -
    725 * p.pow(2) * q.pow(2) * r * s.pow(2) +
    1200 * p.pow(3) * r.pow(2) * s.pow(2) +
    3250 * q.pow(2) * r.pow(2) * s.pow(2) -
    2000 * p * r.pow(3) * s.pow(2) -
    1250 * p * q * r * s.pow(3) +
    3125 * p.pow(2) * s.pow(4) -
    9375 * r * s.pow(4)
  [a6, a5, a4, a3, a2, a1, a0]
}

///|
pub fn check_quintic_pqrs_solvability(
  p : BigInt,
  q : BigInt,
  r : BigInt,
  s : BigInt,
) -> QuinticSolvabilityReport {
  build_quintic_solvability_report(
    "is_quintic_pqrs_solvable",
    quintic_pqrs_coeff(p, q, r, s),
  )
}

///|
/// Return whether quintic equation x^5 + px^3 + qx^2 + rx + s is solvable.
pub fn is_quintic_pqrs_solvable(
  p : BigInt,
  q : BigInt,
  r : BigInt,
  s : BigInt,
) -> Bool {
  let report = check_quintic_pqrs_solvability(p, q, r, s)
  tracing.add(report.msg)
  report.solvable
}

///|
test {
  let report = check_quintic_ab_solvability(-5, 12)
  assert_true(report.solvable)
  assert_eq(report.check_name, "is_quintic_ab_solvable")
  inspect(report.roots, content="[40]")
  let proof = report.msg.display(2)
  assert_true(proof.contains(": is_quintic_ab_solvable"))
  assert_true(proof.contains("[resolvent]: "))
  assert_true(proof.contains("[roots]: [40]"))
  assert_true(is_quintic_ab_solvable(-5, 12))
  assert_eq(tracing.msg.display(2), proof)
}
