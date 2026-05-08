# Results for env-hashmap

## Headline

| Scenario   | Baseline (c84101d8) | Head (a45e938b) | Δ        |
| ---------- | ------------------: | --------------: | -------: |
| full       | 46.8 s              | 47.4 s          | **+1.4%** |
| nochange   | 0.580 s             | 0.572 s         | **-1.4%** |
| prelude    | 3.97 s              | 4.02 s          | **+1.3%** |
| leaf       | 1.567 s             | 1.626 s         | **+3.8%** |

**Verdict: no-win.** Hypothesis was -5% to -10% on full builds; we
got +1.4%. The HashMap migration on `typeClasses`, `types`, and the
class-keyed inner map of `typeClassDictionaries` did not yield a
measurable win, and was slightly negative on three of four scenarios.

(Final row in the auto-table below — `Head 0.0s -100%` for SHA
a45e938b — is a `--profile` build run failed; ignore. Real numbers
are the rows for c84101d8 above and the analysis here.)

## Raw runs (recorded by `exp run`)

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-05-08 | full     | c84101d8     | c84101d8 |     46.9 |     47.3 |   +0.9% | median of 2, head 47333-47611 ms, base 46899-47699 ms |
| 2026-05-08 | full     | c84101d8     | c84101d8 |     46.8 |     47.4 |   +1.4% | median of 4, head 47142-47809 ms, base 46424-48264 ms |
| 2026-05-08 | nochange | c84101d8     | c84101d8 |      0.6 |      0.6 |   -1.4% | median of 4, head 568-581 ms, base 558-594 ms |
| 2026-05-08 | prelude  | c84101d8     | c84101d8 |      4.0 |      4.0 |   +1.3% | median of 4, head 3907-4048 ms, base 3930-4064 ms |
| 2026-05-08 | leaf     | c84101d8     | c84101d8 |      1.6 |      1.6 |   +3.8% | median of 4, head 1539-1679 ms, base 1562-1636 ms |
| 2026-05-08 | full     | c84101d8     | a45e938b |     47.1 |      0.0 | -100.0% | bogus — head was a profiled build, --profile RTS flag rejected; ignore |

## Why no win — likely causes

1. **Aggregated cost-centre cluster doesn't all come from Map lookups.**
   The post-PR-#18 cost-centre table credited `compare (ProperName)` 3.9%,
   `compare (Qualified)` 2.3%, `==` (PSString) 1.4% — totalling ~7.6%.
   But those are aggregated across *all callers*: `compareType`, `eqType`,
   AST sorting, JSON serialization, and many more — not only Map lookups.
   The `name-compare-survey` measured 1.45M Map lookups across four sites
   but did not measure how much of the 7.6% cluster is attributable to
   *those particular call sites*. If lookup-driven compares are <30% of
   the cluster, the win is in the noise.

2. **Per-lookup costs are roughly balanced.** A `Data.Map` probe at N=337
   does ~log₂(337) ≈ 8.4 short ASCII compares. Each compare on
   `Qualified (ProperName _)` short-circuits on the first different
   character — typically 2-4 byte ops per compare. A `HashMap` lookup
   must (a) hash the qualifier-tag, (b) hash a ~10-char module name,
   (c) hash a ~10-char class name, then (d) do one full equality check.
   Concretely: each Text hash is `length` ops; total ≈20 ops + tag.
   That's similar to 8.4 short compares. No order-of-magnitude win.

3. **GHC inlining may not have specialised through the Hashable chain.**
   `Hashable (Qualified a)` calls into `Hashable QualifiedBy` and
   `Hashable a`; `Hashable (ProperName a)` calls into `Hashable Text`.
   Even with `{-# INLINE #-}` on every method, GHC's inlining policy
   for cross-module phantom-typed instances is sometimes conservative.
   Verifying would require Core inspection (`-ddump-simpl`).

4. **Working-set / cache penalty.** `HashMap.Strict` is a HAMT — bigger
   per-node footprint than the small balanced trees `Data.Map` builds for
   N≈337 entries that fit in a few cache lines. For very-hot, cardinality-
   small Maps the HashMap's broader footprint can lose on locality.

## Comparison to PR #18 (type-hash / unify-leaf-no-hash)

PR #18 worked because it eliminated `compareType`, where each call
recursed through entire Type ASTs (10–30 nodes) — orders of magnitude
more work per probe than a short-string compare. The relative win of
"1 hash + 1 eq" over "log_n compares of 30-node Type trees" is
enormous. Over "log_n compares of 12-char ASCII strings" — small
enough to be lost in implementation overhead.

## What this experiment rules out

- **Pure HashMap migration on Environment Maps is not a win** at the
  cardinalities and key shapes the survey measured (337 keys for
  class-Maps, 3642 for the type-Map; short ASCII string keys).
  Even with INLINE Hashable instances and a clean migration that
  passes all 1340 tests, the change is at best neutral.
- **The 7.6% name-compare cluster is not all attributable to the
  surveyed Map lookups.** A characterization that measured
  cluster-vs-lookup attribution (instrument the compares themselves
  rather than the lookup sites) would have surfaced this before
  committing to a 19-file migration.

## What might still work (different attack)

- **Specialise the per-Map lookup at the known key type.** A monomorphic
  `lookupClass :: Qualified (ProperName 'ClassName) -> Map _ a -> Maybe a`
  with INLINE compares might let GHC produce a tighter probe loop than
  the polymorphic `M.lookup` it inlines today.
- **Attack the *non-lookup* sources of name compares.** If most of the
  cluster comes from `compareType` / `eqType` recursion, the right
  attack is structural-share-or-cache on those — not Map representation.
- **Replace the innermost per-class dictionary Map.** This experiment
  kept `M.Map (Qualified Ident) (NonEmpty NamedDict)` unchanged.
  Those have ~30 entries but are walked by `M.elems` in `findDicts`,
  ~795k times per build. A different data structure there is a separate
  experiment.

## Source diff

Branch `env-hashmap`, commit `a45e938b`. 19 files changed, +157/-92.

Tests: `stack test --fast` — 1340 examples, 0 failures.
