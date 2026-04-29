---
id: unify-cache-anatomy
status: done (research)
verdict: tbd
branch: unify-cache-anatomy
worktree: /workspace/p/unify-cache-anatomy
baseline_sha: 5713e832
head_sha: 5713e832
hypothesis: >
  Three call-elimination experiments at the 3 hot funApp/abs/array
  sites in TypeChecker/Types.hs all regress prelude by ~7%
  (skip-redundant-funapp-unify, unify-pattern-survey Phase 2,
  funapp-pattern-match). Cost-centre data shows these calls are
  individually trivial. So the cache is doing something beyond
  caching outer calls — most plausibly: it's also intercepting
  recursive `unifyTypes'` descents through the TypeApp case
  (Unify.hs:147-149) where every TypeApp triggers two child
  unifyTypes that go back through the cache check.
  This survey decomposes cache behaviour by lookup depth, pair
  size, and per-module distribution to identify what algorithmic
  alternative could replace it (e.g., tag-equality fast-path,
  TypeFlags-driven structural eq, size-thresholded cache).
headline_delta: 86% of cache hits are on 1-2 node pairs (constructor-self recurrences); 97% ≤10 nodes; tail of >50-node pairs is 0.23% of hits
tags: [unification, caching, characterization, anatomy]
started: 2026-04-29
closed: null
---

# unify-cache-anatomy

## Why

Four prior experiments establish:

1. **Dropping the cache costs +24% on full** (`unify-cache`).
2. **Replacing it with hash-eq-no-cache costs +7% prelude**
   (`unify-pattern-survey` Phase 2).
3. **Skipping outer calls at the 3 hot sites — by `eqType` guard,
   nested constructor pattern, or any other means — costs ~+7%
   prelude** (3 separate experiments).
4. **The 3 hot sites' calls are individually near-zero-cost**
   (cost-centre profile: unifyTypes/substituteType/withErrorMessageHint
   all 0.0% inherited time).

The cache earns its keep but **not** by memoizing the trivial outer
calls — those are too cheap. The benefit must come from somewhere
else in the cache's interaction with the unification machinery.

Code reading shows: cache is per-module, never reset within a
module, and intercepted at `unifyTypes''` *before* dispatch into
`unifyTypes'`. Every recursive `t3 \`unifyTypes\` t5` from the
`TypeApp` case (Unify.hs:147-149) goes back through the cache
check. So the cache may be amortising not the outer calls but the
recursive descents triggered by larger structural unifications.

## Hypothesis

The cache's load-bearing behaviour is intercepting **recursive
descents** through `TypeApp`/`KindApp`/structural pairs, not the
outer calls from the 3 hot sites. Specifically: when a single
outer `unifyTypes T T'` traverses a tree of N nodes, the cache
dedupes within and across that traversal; without the cache, the
N traversals re-compute substituteType, take the hint, and walk
the structure even when the same `(t, t')` pair has already been
checked.

If true, an algorithmic alternative needs to handle the recursive-
descent case, not just the outer-call case. Candidates:

- **Tag-equality fast-path on `unifyTypes'`** — for trivially-equal
  pairs (`TypeConstructor c1 ~ TypeConstructor c2 | c1 == c2`,
  `TypeVar v1 ~ TypeVar v2 | v1 == v2`, etc.), bypass the cache
  insert and the structural dispatch. Cheap pairs handled outside
  the cache.
- **TypeFlags-driven structural eq** — extend the existing per-node
  `tfHash` so that `eqType t1 t2 = typeHash t1 == typeHash t2 &&
  shallow-tag-eq && children` short-circuits at every node, making
  recursive descent self-memoizing.
- **Size-thresholded cache** — only insert pairs whose total node
  count exceeds N. Most cache entries today are tiny; HashSet
  bookkeeping for them may not pay off.
- **Per-binding-group flush** — clear cache at `withFreshSubstitution`
  to test whether benefit is intra-group vs cross-group.

## Scope

**In.** A read-only survey: instrument the cache lookup/insert
points to record per-call depth, pair size, hit/miss, and per-
module cache size growth. Dump on shutdown.

**Out.** Implementing any of the algorithmic alternatives above —
those are follow-up experiments once we know which to pursue.
Modifying Unify.hs structure (per LESSONS Unify.hs sensitivity).

## Falsification criterion

If the data shows that >70% of cache hits are at recursive depth
≥1, the recursive-descent hypothesis is supported and the next
experiment should attack that path. If hits are mostly at depth 0
(outer calls), then the cost-elimination experiments should have
shown a win — and the regression mechanism is something else
entirely (e.g., GC pressure from the HashSet itself shaping
allocation patterns), which would need a different angle.

## Links

- Worktree: /workspace/p/unify-cache-anatomy
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
