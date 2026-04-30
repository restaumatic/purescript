---
id: unify-leaf-no-hash
status: shipped
verdict: win
branch: unify-leaf-no-hash
worktree: /workspace/p/unify-leaf-no-hash
baseline_sha: 5713e832
head_sha: e3425f4d
headline_delta: vs 5713e832 (current shipped tip) full -0.4%, nochange -1.4%, prelude +1.4%, leaf +2.6%; vs 799e8208 (pre-type-hash) full -18.6% (better than type-hash's -15.4% standalone). Simplification — removes type-hash machinery + cache for a 5-line leaf fast-path with equivalent perf
hypothesis: >
  type-hash shipped at -15.4% on full and lives to make the
  HashSet unificationCache cheap (O(1) hash + ~1 eqType per
  lookup vs Set's O(log n) compareType). unify-leaf-fast-path
  Phase 2 showed the leaf fast-path absorbs essentially all of
  the cache's full-build value (+24% naked drop → -0.3% with
  fast-path). If we drop both the cache AND type-hash starting
  from pre-type-hash baseline 799e8208, type-hash's -15.4% win
  evaporates because the thing it was making cheap doesn't
  exist anymore. We trade type-hash machinery (per-node hash
  computation at construction, Hashable instance, HashSet
  bookkeeping) for a 5-line leaf fast-path. Goal: match or
  beat 5713e832 (current shipped tip, type-hash applied) with
  a simpler codebase.
tags: [unification, fast-path, leaf, no-cache, type-hash, simplification]
started: 2026-04-29
closed: 2026-04-30
---

# unify-leaf-no-hash

## Hypothesis

type-hash's value is contingent on having a cache to make
cheap. Removing the cache (replacing it with a leaf fast-path
that catches 86% of its hits) eliminates type-hash's reason
for existing. Pre-type-hash + leaf fast-path + no cache
should:

- Save the per-node hash computation at construction time
  (extra Int operations per `Type` allocation across
  `TypeApp` / `KindApp` / `RCons` / `ConstrainedType` / etc.)
- Save the `Hashable` instance dispatch / inline complexity
- Save the cache bookkeeping
- Lose only the +3.4% prelude regression that
  `unify-leaf-fast-path` Phase 2 measured for cache-less
  operation on the post-type-hash branch — possibly less
  here since the pre-type-hash branch wasn't paying type-
  hash's construction-time tax to begin with.

If full + nochange + leaf are within ±2% of 5713e832 and
prelude is within +5%, this is a net codebase simplification
at no perf cost — ship.

## Scope

**In.** Apply leaf fast-path + cache drop on pre-type-hash
baseline 799e8208. The patterns are identical to the post-
type-hash variant; the cache type is `Set (Type, Type)`
instead of `HashSet (Type, Type)` but is removed in both.

**Out.** Reverting type-hash from the post-type-hash tip
(equivalent end state, but messier diff). Touching the
`unificationCache` field in `CheckState` (left unread/
unwritten for now — field removal is follow-up if this
ships).

## Falsification criterion

If full regresses by ≥2% vs 5713e832 (the current tip), or
prelude regresses by ≥5% vs 5713e832, type-hash is still
earning its keep on real workloads — close as no-win and
keep type-hash.

## Comparison plan

Two baseline comparisons:

1. **vs 799e8208** (pre-type-hash, S.Set cache) — the direct
   base of this branch. Measures what leaf fast-path + cache
   drop does *to* the pre-type-hash codebase.

2. **vs 5713e832** (post-type-hash, HashSet cache) — the
   current shipped tip. Measures whether the simplification
   matches the type-hash + cache combo we'd be replacing.

Comparison #2 is the load-bearing question. Comparison #1 is
a sanity check.

## Links

- Worktree: /workspace/p/unify-leaf-no-hash
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
