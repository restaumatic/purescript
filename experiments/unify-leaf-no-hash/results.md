# Results for unify-leaf-no-hash

Append-only. See experiments/SCHEMA.md for format.

| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ----- |
| 2026-04-29 | full     | 799e8208     | 799e8208 |     56.7 |     46.2 |  -18.6% | median of 6, 45595-46759 ms |
| 2026-04-29 | nochange | 799e8208     | 799e8208 |      0.6 |      0.6 |   +3.9% | median of 6, 563-623 ms |
| 2026-04-29 | prelude  | 799e8208     | 799e8208 |      3.9 |      3.8 |   -0.9% | median of 6, 3807-3978 ms |
| 2026-04-29 | leaf     | 799e8208     | 799e8208 |      1.5 |      1.5 |   -0.1% | median of 6, 1533-1599 ms |
| 2026-04-29 | full     | 5713e832     | 799e8208 |     48.2 |     51.5 |   +6.7% | median of 6, 46871-63417 ms |
| 2026-04-29 | nochange | 5713e832     | 799e8208 |      0.6 |      0.7 |   +1.7% | median of 6, 574-689 ms |
| 2026-04-29 | prelude  | 5713e832     | 799e8208 |      3.7 |      3.8 |   +2.2% | median of 6, 3752-5027 ms |
| 2026-04-29 | leaf     | 5713e832     | 799e8208 |      1.6 |      1.6 |   +3.7% | median of 6, 1533-8692 ms |
| 2026-04-29 | full     | 5713e832     | 799e8208 |     48.0 |     47.9 |   -0.4% | median of 6, 46127-51221 ms |
| 2026-04-29 | nochange | 5713e832     | 799e8208 |      0.6 |      0.6 |   -1.4% | median of 6, 539-595 ms |
| 2026-04-29 | prelude  | 5713e832     | 799e8208 |      4.0 |      4.0 |   +1.4% | median of 6, 4021-5426 ms |
| 2026-04-29 | leaf     | 5713e832     | 799e8208 |      1.6 |      1.6 |   +2.6% | median of 6, 1543-8816 ms |

(Mid-table rows 11–14 with `+6.7%` full are DISCARDED — head
runs were hit by load spike to 7.4 mid-run. The clean re-run on
rows 15–18 is the load-bearing comparison.)

## Headline

### vs 799e8208 (pre-type-hash, direct base)

| Scenario  | Baseline | Head     | Δ       |
| --------- | -------- | -------- | ------- |
| full      | 56.7 s   | 46.2 s   | **-18.6%** |
| nochange  | 0.56 s   | 0.58 s   | +3.9%      |
| prelude   | 3.86 s   | 3.83 s   | -0.9%      |
| leaf      | 1.54 s   | 1.54 s   | -0.1%      |

The headline figure: **-18.6% on full vs the pre-type-hash base**
— larger than the -15.4% type-hash shipped alone. So the leaf
fast-path + cache drop is a *better* full-build optimisation
than type-hash for the same starting point.

### vs 5713e832 (post-type-hash, current shipped tip)

| Scenario  | Baseline | Head     | Δ       |
| --------- | -------- | -------- | ------- |
| full      | 48.0 s   | 47.9 s   | **-0.4%** |
| nochange  | 0.58 s   | 0.57 s   | -1.4%     |
| prelude   | 3.98 s   | 4.04 s   | +1.4%     |
| leaf      | 1.56 s   | 1.60 s   | +2.6%     |

This is the load-bearing comparison: **the simplified codebase
matches the current shipped tip on every scenario within ±2.6%.**

## Interpretation

### What we replaced

| Component | Status |
|---|---|
| Per-node `tfHash` field on every `Type` constructor | Removed (pre-type-hash code) |
| Hash-combine in pattern-synonym smart constructors | Removed |
| `Hashable Type` instance + UNPACK / INLINE pragmas | Removed |
| `HashSet (Type, Type)` `unificationCache` | Dropped (was `S.Set (Type, Type)` here) |
| `unifyTypes''` cache wrapper | Dropped |

### What we added

5 top-level pattern clauses on `unifyTypes`:

```haskell
unifyTypes (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = pure ()
unifyTypes (TypeVar _ v1)         (TypeVar _ v2)         | v1 == v2 = pure ()
unifyTypes (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = pure ()
unifyTypes (TypeLevelInt _ n1)    (TypeLevelInt _ n2)    | n1 == n2 = pure ()
unifyTypes (Skolem _ _ _ s1 _)    (Skolem _ _ _ s2 _)    | s1 == s2 = pure ()
```

Net diff: removed substantially more code than added.

### Comparison to type-hash + cache

| Metric | type-hash + HashSet cache | leaf fast-path, no cache |
|---|---:|---:|
| Δ vs 799e8208 full | -15.4% | **-18.6%** |
| Δ vs 5713e832 full (self) | 0% | -0.4% |
| Binary size | 49.1 MB | 48.6 MB (−500 KB) |
| Code complexity | TypeFlags{Word8,Int} + UNPACK + Hashable + HashSet machinery | 5 pattern clauses |
| GHC-specific footguns documented in LESSONS | UNPACK on multi-field record (+107% if missed), INLINE on Hashable (+102% if missed) | none |

### Why this works

`unify-leaf-fast-path` Phase 1 (post-type-hash baseline): leaf
fast-path alone was neutral — the 86% of cache hits it eliminates
weren't on the critical path because the cache was already very
cheap (HashSet O(1) thanks to type-hash).

But on the pre-type-hash branch, the cache was `Set (Type, Type)`
with O(log n) `compareType` — type-hash's whole reason for being
was to make those cache lookups cheaper. **If you don't have a
cache at all, you don't need type-hash.** The leaf fast-path
catches the same recurrence pattern that the cache was catching,
without paying either Set's compareType cost or HashSet's
construction-time hash cost.

### Caveats

- nochange +3.9% vs 799e8208 — likely a real but small regression
  on the no-op path. Possibly because the per-module
  `unificationCache` (S.Set, never re-allocated when empty) was
  cheaper to no-op than the pattern dispatch in unifyTypes. Worth
  characterising if shipping.
- The `unificationCache` field still occupies `CheckState`
  un-read/un-written. Removal is mechanical follow-up cleanup.
- Reverting type-hash from the post-type-hash tip is the
  deployment path; this experiment is on the equivalent end-
  state via a fresh branch off pre-type-hash.

## Verdict

**Win.** Simplification at no perf cost. **Recommend shipping.**

Concrete deployment steps:

1. Revert type-hash machinery from the post-type-hash tip
   (Types.hs TypeFlags struct change, Hashable Type instance,
   pattern-synonym hash computations, UNPACK / INLINE pragmas).
2. Apply leaf fast-path (5 patterns on `unifyTypes`).
3. Drop `unificationCache` from `CheckState` and the cache
   wrapper from `Unify.hs`.
4. Rerun all 4 scenarios on the deployment branch to confirm
   the equivalent end-state holds.

Combined diff is a net code reduction.
