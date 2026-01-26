pub(open) trait ToUnicode {
  unicodify(Self) -> String
}

///|
pub fn unicodify_monomial(term : String, coeff : String, power : String) -> String {
  guard !(coeff == "0") else { "0" }
  guard !(power == "0") else { coeff }
  guard !(power == "1") else { unicodify_monomial_case_power_one(term, coeff) }

  // for power > 1
  let sup = to_super_string(power)
  guard !(coeff == "1") else { term + sup }
  coeff + term + sup
}

///|
fn unicodify_monomial_case_power_one(term : String, coeff : String) -> String {
  guard !(coeff == "1") else { term }
  coeff + term
}

///|
fn str_abs(number : String) -> StringView {
  number.strip_prefix("-").unwrap_or(number)
}

///|
pub fn unicodify_univariate_polynomial(
  term? : String = "x",
  coeffs : Array[BigInt],
  descent? : Bool = false,
) -> String {
  guard !coeffs.is_empty() else { "0" }
  let monomials = coeffs
    .mapi((i, c) => unicodify_monomial(term, "\{c}", "\{i}"))
    .filter(s => s != "0")
  let monomials = if descent { monomials.rev() } else { monomials }
  monomials[1:].fold(init=monomials[0], (s, t) => {
    guard t.has_prefix("-") else { "\{s} + \{t}" }
    "\{s} - \{str_abs(t)}"
  })
}

///|
test {
  assert_eq(
    unicodify_univariate_polynomial([-1, 2, -3, -5, 7], descent=true),
    "7x⁴ - 5x³ - 3x² + 2x - 1",
  )
}
