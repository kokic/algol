# Prime

Efficient primality testing for MoonBit `BigInt`.

## Public API

- `is_prime(n)`:
  - Uses `is_small_prime` for `n < 10_000`.
  - Uses `is_probable_prime_bpsw` for `n >= 10_000`.
- `is_small_prime(n)`:
  - Exact primality check for `Int` values in `[0, 10_000)`.
- `is_probable_prime_bpsw(n)`:
  - Baillie-Pomerance-Selfridge-Wagstaff (BPSW) probable-prime test.
  - Fast in practice and has no known counterexample, but is still a probable-prime test.

## Notes

- The small-number path is optimized with a precomputed prime table and a narrow lookup window.
- `is_probable_prime_bpsw` does its arithmetic in machine words (`UInt64` with Montgomery
  reduction) whenever `n < 2^63`, avoiding `BigInt`'s per-operation allocation; the
  `BigInt` path (Miller-Rabin base 2 + strong Lucas-Selfridge) runs above that.
- Trial division covers the first 64 primes before the heavier tests.

## Benchmark

```
$ moon bench -p prime/benchmarks --target native --release

name                            time (mean ± σ)         range (min … max)
kokic/prime BPSW aggregate       527.34 µs ±   8.42 µs   518.28 µs … 543.90 µs  in 10 ×    192 runs
core/math MR iters=1 aggregate   187.14 µs ±   3.10 µs   183.43 µs … 192.00 µs  in 10 ×    531 runs
core/math MR iters=8 aggregate   493.30 µs ±  65.43 µs   409.35 µs … 597.68 µs  in 10 ×    249 runs
core/math MR iters=64 aggregate    2.41 ms ± 131.27 µs     2.21 ms …   2.62 ms  in 10 ×     40 runs
```

The benchmark also reports per-case timings for primes, probable primes, perfect-square composites, and base-2 strong pseudoprimes. `core/math` uses Miller-Rabin with a configurable iteration count; its default-equivalent comparison here is `iters=64`.
