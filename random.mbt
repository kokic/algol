///|
pub fn generate_time_based_seed() -> Bytes {
  let timestamp = @env.now()
  let splitmix_rng = @splitmix.new(seed=timestamp)
  let part1 = splitmix_rng.next_uint64()
  let part2 = splitmix_rng.next_uint64()
  let part3 = splitmix_rng.next_uint64()
  let part4 = splitmix_rng.next_uint64()
  uint64_array_to_bytes([part1, part2, part3, part4])
}

///|
pub fn uint64_array_to_bytes(values : Array[UInt64]) -> Bytes {
  Bytes::makei(32, fn(i) {
    let value_index = i / 8
    let byte_index = i % 8
    let value = values[value_index]
    match byte_index {
      0 => (value & 0xFFUL).to_byte()
      1 => ((value >> 8) & 0xFFUL).to_byte()
      2 => ((value >> 16) & 0xFFUL).to_byte()
      3 => ((value >> 24) & 0xFFUL).to_byte()
      4 => ((value >> 32) & 0xFFUL).to_byte()
      5 => ((value >> 40) & 0xFFUL).to_byte()
      6 => ((value >> 48) & 0xFFUL).to_byte()
      7 => ((value >> 56) & 0xFFUL).to_byte()
      _ => b'\x00' // unreachable!
    }
  })
}
