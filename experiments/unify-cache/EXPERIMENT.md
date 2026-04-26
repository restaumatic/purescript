---
id: unify-cache
status: abandoned
verdict: no-win
branch: unify-cache
worktree: /workspace/p/unify-cache
baseline_sha: 43f6b613
head_sha: 0938d3b3
hypothesis: >
  After skip-redundant-entailment-unify shipped, the unificationCache
  may catch too few duplicate (t1, t2) pairs to justify its cost. The
  type-hash profile shows ~19% of compile time in Hashable
  infrastructure (liftHashWithSalt, defaultHashWithSalt, hashInt, …) —
  almost entirely driven by HashSet membership/insert on the cache.
  Removing the cache (or replacing it with something cheaper) should
  erase most of that 19% if hit rate is low.
headline_delta: "no-win — cache is +24% net positive; UnifyKey wrap is a wash"
tags: [unification, caching, hashing, measurement]
started: 2026-04-26
closed: 2026-04-26
---

# unify-cache — does the unification cache still earn its keep?

## Hypothesis

`unificationCache :: HashSet (SourceType, SourceType)` was added back
when `compareType` dominated profiles to skip redundant unification of
already-seen pairs. Since then:

1. `skip-redundant-entailment-unify` shipped, dropping the
   identical-type cases on the entailment side before they reach
   `unifyTypes`.
2. `type-hash` shipped, switching the cache from `Set` (Ord-based,
   compareType cost) to `HashSet` (Hashable-based) — that removed
   `compareType` from the profile but introduced the hashing cost.

Post-type-hash profile (`/tmp/typehash-prof/purs-after.prof`,
2026-04-26, against pr-admin): ~19% of total time is now in the
Hashable infrastructure:

| %time | Cost centre |
|------:|-------------|
|   4.5 | liftHashWithSalt |
|   4.1 | defaultHashWithSalt |
|   2.7 | hashInt |
|   2.4 | hash |
|   2.0 | liftHashWithSalt.step |
|   1.9 | hashWithSalt (Class:385) |
|   1.6 | hashWithSalt (Class:357) |

Inserting `(SourceType, SourceType)` into a HashSet hashes the tuple
via `liftHashWithSalt`, then walks chains for collision resolution.
`Hashable Type` itself reads the cached hash in O(1) (via type-hash),
but the lifted/tuple/walk machinery still costs per-call.

If post-skip-redundant the cache hit rate is low (most lookups miss
and add a fresh entry), the cache is doing little useful work and
costs ~19% of compile time. Removing it could be a large win.

It's also possible the hit rate is high — in which case the cache is
genuinely earning its keep and the question becomes whether a cheaper
structure (e.g. an `IntSet` of `hashWithSalt h1 h2` pre-mixed keys
with an `Eq` fallback on collision) beats the current HashSet.

## Plan

**Phase 1 — measure (no behaviour change).**  Add IORef-based
counters for {hits, misses, inserts} on `unificationCache`. Run a
full pr-admin build, dump counts on type-checker shutdown. Decide
direction based on the numbers.

**Phase 2 — act.**
- If hit rate is < ~5%: drop the cache entirely. The membership check
  becomes `False`, the insert becomes a no-op. Measure all four
  scenarios.
- If hit rate is significant: try cheaper structures (IntSet keyed by
  pre-mixed hash; bloom filter; small-LRU instead of unbounded set).

## Scope

In:
- Cache instrumentation in `TypeChecker.Monad` / `TypeChecker.Unify`.
- One-line dump on type-checker shutdown (or per-module, so we can
  see the distribution).
- Phase-2 cache removal or simplification, depending on Phase-1 data.

Out (deliberately):
- Other Hashable-driven hot paths. If the cache itself isn't the
  source of most Hashable time, we'll find out in Phase 1 and adjust
  scope.
- Touching `Ord Type`, `Eq Type`, or anything in the type-hash bedrock.

## Risks / things to watch

- **The cache might be load-bearing for full builds.** It was added
  for a measurable reason; just because skip-redundant-entailment-unify
  filters earlier doesn't mean the residual hits are zero. Drop blindly
  → risk regressing full builds. Phase 1 is non-negotiable.
- **Counter overhead distorts measurement.** Use unboxed `IORef Int`
  counters and don't log per-call; aggregate, dump once.
- **Hit-rate may vary by phase.** What we care about is the time-
  weighted hit contribution — a low-rate cache that catches huge
  subtrees is still a win. Consider also instrumenting time saved if
  Phase-1 numbers are ambiguous.

## Links

- Worktree: /workspace/p/unify-cache
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Source profile: `/tmp/typehash-prof/purs-after.prof` (post-type-hash,
  not committed)
- Precedent: [type-hash](../type-hash/EXPERIMENT.md) — turned the
  cache from Set into HashSet; this experiment asks whether the cache
  itself is still worth it.
