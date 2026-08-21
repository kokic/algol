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
    let shift = (i % 8) * 8
    (value >> shift).to_byte()
  })
}
