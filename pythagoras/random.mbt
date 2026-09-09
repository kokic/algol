///|
pub fn random_time_based() -> @random.Rand {
  @random.Rand::chacha8(seed=generate_time_based_seed())
}

///|
pub fn generate_time_based_seed() -> Bytes {
  let timestamp = @env.now()
  let part1 = timestamp - 25
  let part2 = timestamp + 25
  let part3 = timestamp - 41
  let part4 = timestamp + 41
  uint64_array_to_bytes([part1, part2, part3, part4])
}

///|
pub fn uint64_array_to_bytes(values : Array[UInt64]) -> Bytes {
  Bytes::makei(values.length() * 8, fn(i) {
    let value = values[i / 8]
    let shift = i % 8 * 8
    (value >> shift).to_byte()
  })
}

///|
test "random handles degenerate limits without panicking" {
  let rand = @random.Rand::chacha8(seed=uint64_array_to_bytes([1, 2, 3, 4]))
  assert_eq(random(rand, 0N), 0N)
  assert_eq(random(rand, 1N), 0N)
  assert_eq(random_range(rand, 5N, 5N), 5N)
  assert_eq(random_range(rand, 7N, 3N), 7N)
}

///|
test "uint64_array_to_bytes handles non-four element arrays" {
  assert_eq(uint64_array_to_bytes([1, 2]).length(), 16)
  assert_eq(uint64_array_to_bytes([0x0102030405060708]).length(), 8)
}

///|
pub fn random(rand : @random.Rand, limit : BigInt) -> BigInt {
  // limit <= 1 means the range [0, limit) is empty or degenerate
  guard limit > 1 else { return 0 }
  let k = limit.bit_length()
  let two_pow_k = (1 : BigInt) << k
  let threshold = two_pow_k - two_pow_k % limit
  let mut x = rand.bigint(k)
  while x >= threshold {
    x = rand.bigint(k)
  }
  x % limit
}

///|
pub fn random_range(
  rand : @random.Rand,
  inclusive : BigInt,
  exclusive : BigInt,
) -> BigInt {
  inclusive + random(rand, exclusive - inclusive)
}
