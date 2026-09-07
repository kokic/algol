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
    let p_big = p
    let mut power = HasOne::one()
    // precompute powers [1, p, p^2, ..., p^exp]
    let powers = [power]
    for i = HasOne::one(); i <= exp; i = i + HasOne::one() {
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
