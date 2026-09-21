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
kokic/prime BPSW aggregate       524.34 µs ±   2.55 µs   520.31 µs … 527.48 µs  in 10 ×    190 runs
core/math MR iters=1 aggregate   185.23 µs ±   1.28 µs   183.20 µs … 186.87 µs  in 10 ×    536 runs
core/math MR iters=8 aggregate   404.96 µs ±   5.88 µs   390.56 µs … 410.76 µs  in 10 ×    267 runs
core/math MR iters=64 aggregate    2.21 ms ± 141.51 µs     1.99 ms …   2.53 ms  in 10 ×     47 runs
```

The benchmark also reports per-case timings for primes, probable primes, perfect-square composites, and base-2 strong pseudoprimes. `core/math` uses Miller-Rabin with a configurable iteration count; its default-equivalent comparison here is `iters=64`.
