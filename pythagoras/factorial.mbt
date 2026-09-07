///|
pub fn[N : HasOne + Compare + Add + Mul] factorial_naive(
  n : N,
) -> N raise InvokeError {
  guard n >= HasOne::one() else {
    raise InvalidInput(msg="Factorial is only defined for n >= 1.")
  }
  let mut result = HasOne::one()
  for i = HasOne::one(); i <= n; i = i + HasOne::one() {
    result = result * i
  }
  result
}
