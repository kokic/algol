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
- The large-number path combines Miller-Rabin (base 2) and strong Lucas-Selfridge.

## Benchmark

```
$ moon bench -p kokic/prime/benchmarks --target native --release

name                            time (mean ± σ)         range (min … max)
kokic/prime BPSW aggregate       391.42 µs ±   2.54 µs   387.84 µs … 395.03 µs  in 10 ×    256 runs
core/math MR iters=1 aggregate   136.49 µs ± 968.35 ns   134.86 µs … 138.01 µs  in 10 ×    740 runs
core/math MR iters=8 aggregate   708.71 µs ±   6.47 µs   698.48 µs … 716.75 µs  in 10 ×    140 runs
core/math MR iters=64 aggregate    5.19 ms ±  45.90 µs     5.12 ms …   5.26 ms  in 10 ×     20 runs
```

The benchmark also reports per-case timings for primes, probable primes, perfect-square composites, and base-2 strong pseudoprimes. `core/math` uses Miller-Rabin with a configurable iteration count; its default-equivalent comparison here is `iters=64`.
