///|
priv struct PrimeBenchCase {
  name : String
  n : BigInt
  expected : Bool
}

///|
fn prime_bench_case(
  name : String,
  n : BigInt,
  expected : Bool,
) -> PrimeBenchCase {
  { name, n, expected }
}

///|
fn prime_bench_cases() -> Array[PrimeBenchCase] {
  [
    prime_bench_case("prime/first-above-small-limit", 10007, true),
    prime_bench_case("prime/31-bit", 1919802127, true),
    prime_bench_case("prime/50-bit", 626290517014499, true),
    prime_bench_case("prime/67-bit", 83318820587408930933, true),
    prime_bench_case(
      "probable-prime/90-bit", 115792125365684818504981760053, true,
    ),
    prime_bench_case("composite/perfect-square", 1000006000009, false),
    prime_bench_case("composite/base-2-strong-pseudoprime", 3215031751, false),
    prime_bench_case(
      "composite/base-2-strong-pseudoprime/large", 3825123056546413051, false,
    ),
  ]
}

///|
fn assert_prime_bench_cases(cases : Array[PrimeBenchCase]) -> Unit raise {
  for c in cases {
    assert_true(@prime.is_probable_prime_bpsw(c.n) == c.expected)
    let rand = @random.Rand::new()
    assert_true(@math.is_probable_prime(c.n, rand) == c.expected)
  }
}

///|
fn bench_prime_cases_bpsw(
  b : @bench.T,
  name : String,
  cases : Array[PrimeBenchCase],
) -> Unit {
  b.bench(name~, fn() {
    for c in cases {
      b.keep(@prime.is_probable_prime_bpsw(c.n))
    }
  })
}

///|
fn bench_prime_cases_core(
  b : @bench.T,
  name : String,
  cases : Array[PrimeBenchCase],
  iters : Int,
) -> Unit {
  let rand = @random.Rand::new()
  b.bench(name~, fn() {
    for c in cases {
      b.keep(@math.is_probable_prime(c.n, rand, iters~))
    }
  })
}

///|
test "is_probable_prime benchmark: aggregate" (b : @bench.T) {
  let cases = prime_bench_cases()
  assert_prime_bench_cases(cases)

  bench_prime_cases_bpsw(b, "kokic/prime BPSW aggregate", cases)
  bench_prime_cases_core(b, "core/math MR iters=1 aggregate", cases, 1)
  bench_prime_cases_core(b, "core/math MR iters=8 aggregate", cases, 8)
  bench_prime_cases_core(b, "core/math MR iters=64 aggregate", cases, 64)
}

///|
test "is_probable_prime benchmark: per case" (b : @bench.T) {
  let cases = prime_bench_cases()
  assert_prime_bench_cases(cases)

  for c in cases {
    b.bench(name="kokic/prime BPSW " + c.name, fn() {
      b.keep(@prime.is_probable_prime_bpsw(c.n))
    })

    let rand = @random.Rand::new()
    b.bench(name="core/math MR iters=64 " + c.name, fn() {
      b.keep(@math.is_probable_prime(c.n, rand, iters=64))
    })
  }
}
