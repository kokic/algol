///|
pub fn quintic_ab_coeff(
  a : BigInt,
  b : BigInt,
) -> (BigInt, BigInt, BigInt, BigInt, BigInt, BigInt, BigInt) {
  let a6 = (1 : BigInt)
  let (a5, a4) = (a * 8, a.pow(2) * 40)
  let (a3, a2) = (a.pow(3) * 160, a.pow(4) * 400)
  let a1 = a.pow(5) * 512 - 3125 * b.pow(4)
  let a0 = a.pow(6) * 256 - 9375 * a * b.pow(4)
  (a6, a5, a4, a3, a2, a1, a0)
}
