///|
/// All positive m such that m | n
pub fn[E : Abs + HasOne + Mul, P : HasOne + Compare + Add] divisors(
  n : E,
  f : Factorization[E, P],
) -> Array[E] raise InvokeError {
  guard n != HasNil::nil() else { [] }

  let factors = f(abs(n))
  let mut divisors = [HasOne::one()]

  for p, exp in factors {
    let mut power = HasOne::one()
    let powers = [power] // precompute [1, p, p^2, ..., p^exp]
    for i = HasOne::one(); i <= exp; i = i + HasOne::one() {
      power = power * p
      powers.push(power)
    }
    divisors = divisors.map(d => powers.map(pow => d * pow)).flatten()
  }

  divisors
}

///|
test "divisors" {
  assert_eq(divisors(6, factor9973), [1, 3, 2, 6])
  assert_eq(divisors(109, factor9973), [1, 109])
}
