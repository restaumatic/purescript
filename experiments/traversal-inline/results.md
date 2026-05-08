# Results for traversal-inline

## Headline (verdict: win)

Combined two runs against c84101d8 baseline:

| Scenario   | Run 1 (load 4-7) | Run 2 (load 1.85) | Combined verdict           |
| ---------- | ---------------: | ----------------: | -------------------------- |
| full       | **-9.6%**        | **-8.9%**         | **win — robust ~-9%**      |
| nochange   | +8.4%            | -3.0%             | ~0 (small absolute, noise) |
| prelude    | -0.5%            | +1.6%             | ~0 (within noise)          |
| leaf       | -4.3%            | -1.5%             | small win or noise         |

**Hypothesis (-1% to -3% on full) was exceeded** — actual full-build win
is ~-9%, three to nine times larger than predicted.

The nochange swing between runs (+8.4% ↔ -3.0%) means the absolute
+54ms in Run 1 was machine-load noise on a 643ms wallclock — not a
real INLINABLE-induced startup penalty. Run 2 (under load 1.85)
agrees with the structural argument that nochange shouldn't be
affected by INLINABLE on traversal helpers (the typecheck-heavy hot
caller `replaceTypeClassDictionaries` doesn't run when there's no
work).

## Per-round detail

### Run 1 (load avg ~4-7, baseline absolute ~58s)

```
=== full ===
  round 2: base=57779 head=55492 ms (Δ -4.0%)
  round 3: base=58686 head=52761 ms (Δ -10.1%)
  round 4: base=58376 head=52344 ms (Δ -10.3%)
  round 5: base=60112 head=54911 ms (Δ -8.7%)
  median: base=58376 head=52761  Δ=-9.6%

=== nochange ===
  round 2: base=732 head=714 ms (Δ -2.5%)
  round 3: base=634 head=679 ms (Δ +7.1%)
  round 4: base=643 head=698 ms (Δ +8.6%)
  round 5: base=682 head=697 ms (Δ +2.2%)
  median: base=643 head=697  Δ=+8.4%   ← noisy

=== prelude ===
  median: base=4214 head=4192  Δ=-0.5%

=== leaf ===
  median: base=1794 head=1717  Δ=-4.3%
```

### Run 2 (load avg 1.85, head=b831b298)

```
=== full ===
  round 2: base=49607 head=44579 ms (Δ -10.1%)
  round 3: base=56306 head=47732 ms (Δ -15.2%)
  round 4: base=51864 head=46906 ms (Δ -9.6%)
  round 5: base=51134 head=46579 ms (Δ -8.9%)
  median: base=51134 head=46579  Δ=-8.9%

=== nochange ===
  round 2: base=614 head=617 ms (Δ +0.5%)
  round 3: base=602 head=602 ms (Δ  0.0%)
  round 4: base=604 head=586 ms (Δ -3.0%)
  round 5: base=615 head=585 ms (Δ -4.9%)
  median: base=604 head=586  Δ=-3.0%

=== prelude ===
  median: base=3767 head=3826  Δ=+1.6%

=== leaf ===
  round 2: base=1663 head=1535 ms (Δ -7.7%)
  round 3: base=1647 head=1672 ms (Δ +1.5%)
  round 4: base=1785 head=1648 ms (Δ -7.7%)
  round 5: base=1604 head=1623 ms (Δ +1.2%)
  median: base=1647 head=1623  Δ=-1.5%
```

## Why this works

The hot caller is
`Entailment.replaceTypeClassDictionaries`
in `src/Language/PureScript/TypeChecker/Entailment.hs:118-139`,
which uses
`m = WriterT (Any, [...]) (StateT InstanceContext TypeCheckM)` —
at least four monad-transformer layers. The traversal helper
`everywhereOnValuesTopDownM` was polymorphic over `Monad m =>`, so
without `INLINABLE` GHC could not specialise the helper at the call
site (GHC only specialises across module boundaries when the
unfolding is exposed in the interface file via `INLINABLE`).

Adding `{-# INLINABLE everywhereOnValuesTopDownM #-}` tells GHC to
emit the unfolding, letting the compile-time specialiser at the
call site produce a flat, monad-specialised loop. Per-node bind
overhead (which dominates a recursive descent over Expr trees)
collapses from "polymorphic dictionary lookup + four nested
binds" to "one direct call per node."

This is the canonical "polymorphic helper without INLINABLE"
pattern — same shape as PR #18's recovery of `compareType`
specialisation in the cache lookups.

## Cost / risk

- **Diff size: 2 lines** (two `INLINABLE` pragmas).
- **Binary size: +213 KB** (48,625,952 → 48,839,456 bytes; +0.4%).
  GHC emits a few specialised copies at heavy callers (Entailment
  TypeCheckM stack, Sugar passes). Negligible.
- **stack build time:** unaffected within noise (no extra cycles).
- **Correctness:** identical observable behaviour;
  `stack test --fast` passes 1340 examples.

## Source diff

```diff
+ {-# INLINABLE everywhereOnValuesTopDownM #-}
  everywhereOnValuesTopDownM f g h = (f' <=< f, g' <=< g, h' <=< h)
  ...
+ {-# INLINABLE everywhereOnValuesM #-}
  everywhereOnValuesM f g h = (f', g', h')
```

Branch `traversal-inline`, commit `b831b298`. 1 file changed, +2 lines.

## Recommended next move

**Ship.** The full-build improvement is large (~-9%), reproducible
across two runs, the diff is two lines, the binary-size cost is
negligible, and the structural reasoning matches the measured
outcome. Open a PR.

## Raw runs (recorded by `exp run`)

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-05-08 | full     | c84101d8     | c84101d8 |     58.4 |     52.8 |   -9.6% | run 1, load avg ~4-7, proper -O build |
| 2026-05-08 | nochange | c84101d8     | c84101d8 |      0.6 |      0.7 |   +8.4% | run 1, noise — see Run 2 |
| 2026-05-08 | prelude  | c84101d8     | c84101d8 |      4.2 |      4.2 |   -0.5% | run 1 |
| 2026-05-08 | leaf     | c84101d8     | c84101d8 |      1.8 |      1.7 |   -4.3% | run 1 |
| 2026-05-08 | full     | c84101d8     | b831b298 |     51.1 |     46.6 |   -8.9% | verification rerun, load avg 1.85 |
| 2026-05-08 | nochange | c84101d8     | b831b298 |      0.6 |      0.6 |   -3.0% | verification rerun, load avg 1.85 |
| 2026-05-08 | prelude  | c84101d8     | b831b298 |      3.8 |      3.8 |   +1.6% | verification rerun, load avg 1.85 |
| 2026-05-08 | leaf     | c84101d8     | b831b298 |      1.6 |      1.6 |   -1.5% | verification rerun, load avg 1.85 |
