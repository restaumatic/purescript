# Handoff: unify-cache

## TL;DR

**Verdict: no-win.** Two findings:

1. **The cache is net positive.** Hit rate is 39.3% (412,727 / 1,049,412
   on a full pr-admin compile). Disabling it costs **+24%** on full builds
   (52 s → 64 s, clean measurement). The cache saves ~12 s for the
   ~10 s of hashing overhead it adds — net +12 s.
2. **Specialising the tuple Hashable doesn't help.** Wrapping the cache
   key in a `UnifyKey` newtype with a hand-written `Hashable` that
   reads `typeHash` directly is +8% slower than the generic tuple
   Hashable, not faster. GHC already specialises the generic instance
   well enough at this call site (likely via `INLINE` on `Hashable Type`
   that landed in type-hash).

The 19% Hashable-infrastructure cost in the profile is therefore mostly
the *unavoidable* per-call overhead of the cache. Reducing it further
would need either a structurally different cache (small LRU, IntSet of
pre-mixed hashes, bloom filter front-end) or different unification
discipline that obviates the cache. Both are bigger projects than this
experiment scoped.

## The big lesson

Adding ~50 lines of `NOINLINE`-marked top-level definitions to `Unify.hs`
(IORef counters, `dumpUnifyCacheStats`) caused a **+135%** apparent
regression on full builds — even though the new code was never called
from the hot path. Removing those definitions restored baseline timing.
This is a much larger inlining-sensitivity effect than the previously
documented Unify.hs cases (which were small `eqType` guards with ~30%
swings). See LESSONS.md.

The contaminated measurements (Phase 1 with hot-path instrumentation,
Phase 2 with cache disabled but instrumentation still present, Phase 3
attempt 1 with UnifyKey + instrumentation) all reported +120–160% on
full. They are now in `results.md` flagged as `CONTAMINATED`. The
clean three-line manual measurements are the real ones.

## Phase results (clean measurements)

| Phase | Description | Full (s) | Δ vs baseline 51.8 s |
|-------|-------------|---------:|---------------------:|
| Baseline | 43f6b613 (type-hash post-cleanup) | 51.8 | — |
| Phase 2 | Cache dropped (one-line: `unifyTypes'' = unifyTypes'`) | 64.3 | **+24%** |
| Phase 3 | Cache wrapped in `UnifyKey` newtype with hand-tuned Hashable | 55.9 | +8% |

## What's left in the worktree

The unify-cache branch currently has the Phase 3 code:
- `UnifyKey` newtype + Hashable instance in `Monad.hs`
- `typeHash` re-exported from `Types.hs`
- `unifyTypes''` in `Unify.hs` wraps the key in `UnifyKey`

This is a no-win, so the branch should be abandoned (verdict no-win)
and not merged. The Phase 1 instrumentation (`dumpUnifyCacheStats` etc.)
was reverted before the final clean measurement.

## Open follow-ups (separate experiments)

- **Smaller / cheaper cache structure.** With 39% hit rate and ~10 s
  of hashing overhead, a small-LRU or bloom-filter-fronted variant
  might win a few seconds. Speculative; not high priority.
- **Different unification discipline.** If we can structurally reduce
  the number of redundant `unifyTypes` calls (the cache catches 412 K
  of them on full pr-admin), the cache becomes unnecessary. Bigger
  scope; would interact with skip-redundant-entailment-unify and
  other entailment work.
