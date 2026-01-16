///|
const SUPER_MINUS = '⁻'

///|
const SUPER_DIGITS = "⁰¹²³⁴⁵⁶⁷⁸⁹"

///|
pub fn super_digit_char(c : Char) -> Char {
  match c {
    '0'..='9' => SUPER_DIGITS.get_char(c.to_int() - '0').unwrap()
    '-' => SUPER_MINUS
    _ => c
  }
}

///|
test {
  assert_eq(super_digit_char('-'), SUPER_MINUS)
}

///|
pub fn to_super_digits(s : String) -> Array[Char] {
  s.to_array().map(super_digit_char)
}

///|
pub fn to_super_string(s : String) -> String {
  String::from_array(to_super_digits(s))
}

///|
pub fn big_to_super_digits(n : BigInt) -> Array[Char] {
  to_super_digits(n.to_string())
}

///|
test {
  assert_eq(String::from_array(big_to_super_digits(-31382)), "⁻³¹³⁸²")
}
