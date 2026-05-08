# Results for env-hashmap

## Headline

The migration was measured twice. **Both runs are too noisy to resolve
a sub-3% delta**, but **both rule out the hypothesised -5% to -10%
win** — an effect of that magnitude would survive the noise floor.

| Scenario   | Run 1 (load avg ~9) | Run 2 (load 2→14) |
| ---------- | ------------------: | ----------------: |
| full       | +1.4%               | -1.1%             |
| nochange   | -1.4%               | -0.5%             |
| prelude    | +1.3%               | +2.2%             |
| leaf       | +3.8%               | +1.5%             |

**Verdict: no-win.** The hypothesised -5% to -10% on full does not
appear in either measurement. The full-build sign flip between runs
(+1.4% ↔ -1.1%) and the wide per-run spreads (Run 2 baseline was 55-69
s, a 25% spread on a code-unchanged binary) confirm both runs were
CPU-contended and the true delta is somewhere in [-2%, +2%] —
indistinguishable from zero.

A run on a clean machine could resolve the sign of any small
remaining effect, but it can't change the conclusion that the
log-N → O(1) win we expected from the survey did not materialise.

## Raw runs (recorded by `exp run`)

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-05-08 | full     | c84101d8     | c84101d8 |     46.9 |     47.3 |   +0.9% | median of 2, head 47333-47611 ms, base 46899-47699 ms |
| 2026-05-08 | full     | c84101d8     | c84101d8 |     46.8 |     47.4 |   +1.4% | median of 4, head 47142-47809 ms, base 46424-48264 ms |
| 2026-05-08 | nochange | c84101d8     | c84101d8 |      0.6 |      0.6 |   -1.4% | median of 4, head 568-581 ms, base 558-594 ms |
| 2026-05-08 | prelude  | c84101d8     | c84101d8 |      4.0 |      4.0 |   +1.3% | median of 4, head 3907-4048 ms, base 3930-4064 ms |
| 2026-05-08 | leaf     | c84101d8     | c84101d8 |      1.6 |      1.6 |   +3.8% | median of 4, head 1539-1679 ms, base 1562-1636 ms |
| 2026-05-08 | full     | c84101d8     | a45e938b |     47.1 |      0.0 | -100.0% | bogus — head was a profiled build, --profile RTS flag rejected; ignore |

## Noise discipline note (added after rerun)

Both runs were on a contended machine. Run 1 (10:50, load avg 8.83/9.05/7.59
for 1/5/15 min averages — Chrome puppeteer at 111% × 2 plus four Rails
servers and a node test runner). Run 2 (13:53, load avg 2.04 at start
but climbing to 7.90 / 14.04 / 11.31 by completion — concurrent
`claude` sessions and a long-running storybook).

Per-run variance:

- **Run 1** (full scenario): base medians 46424, 46823, 46899, 47699
  → spread 1275 ms (2.7%); head medians 47142, 47432, 47593, 47809
  → spread 667 ms (1.4%). Tighter than Run 2 in absolute terms.
- **Run 2** (full scenario): base medians 55158, 55416, 60029, 68957
  → spread 13799 ms (25%) — clearly contaminated.

Run 1 was actually the more usable data. The "low-load rerun" was
not, in the end, lower-load: load climbed during execution.

Conclusion: any effect bigger than ±3% would have shown up in both runs.
None did. The hypothesis is falsified at that scale.

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
| 2026-05-08 | full     | c84101d8     | a45e938b |     55.4 |     54.8 |   -1.1% | median of 4, head 53906-59905 ms, base 55158-68957 ms; rerun under low load (load avg 2.04) |
| 2026-05-08 | nochange | c84101d8     | a45e938b |      0.6 |      0.6 |   -0.5% | median of 4, head 581-633 ms, base 584-622 ms; rerun under low load (load avg 2.04) |
| 2026-05-08 | prelude  | c84101d8     | a45e938b |      3.8 |      3.9 |   +2.2% | median of 4, head 3867-4152 ms, base 3739-4173 ms; rerun under low load (load avg 2.04) |
| 2026-05-08 | leaf     | c84101d8     | a45e938b |      1.6 |      1.6 |   +1.5% | median of 4, head 1560-1634 ms, base 1552-1635 ms; rerun under low load (load avg 2.04) |
