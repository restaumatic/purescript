---
id: type-hash
status: abandoned
verdict: abandoned
branch: type-hash
worktree: /workspace/p/type-hash
baseline_sha: 799e8208
head_sha: 43f6b613
hypothesis: >
  Cache a structural hash on every Type node (alongside the existing
  TypeFlags), computed at construction. Use it to provide a Hashable
  Type instance so hot Type-keyed containers — first the
  unificationCache — can switch from Set/Map to HashSet/HashMap,
  eliminating compareType from the hot path.
headline_delta: "-15.4% full, neutral incremental"
tags: [typechecker, hashing, type-flags, unification]
started: 2026-04-25
closed: 2026-04-30
---

# type-hash — precomputed structural hash on Type

> **Superseded by [`unify-leaf-no-hash`](../unify-leaf-no-hash/EXPERIMENT.md)
> (shipped via PR #18, 2026-04-30).** This experiment hit -15.4% on full
> by adding a per-`Type`-node hash field + Hashable instance + HashSet
> cache. `unify-leaf-no-hash` reached -18.6% on full from the same
> baseline (799e8208) by going the opposite direction — a 5-line leaf
> fast-path on `unifyTypes` that catches 86% of cache hits upstream,
> letting the cache *and* the hash machinery be removed entirely. Ranked
> against each other, the simpler approach was strictly better, so this
> branch was abandoned without merging. See `experiments/LESSONS.md`
> entry "type-hash's value is contingent on having a cache". The
> hypothesis and design notes below remain as a record of how the
> direction was reached.

## Hypothesis

After synonym-opt + skip-redundant-entailment-unify shipped, the new
top hotspot is `compareType` at 7.7% of full-build time on pr-admin
(see `experiments/measure-merges/results.md` and the post-merges
profile at `profiles/baseline.meta.md`). Most of that cost flows
through:

1. `unificationCache :: Set (Type, Type)` membership check in
   `Unify.hs:121–123` — every `unifyTypes` call does one `S.notMember`
   plus one `S.insert`, both O(log n × compareType).
2. Various `Map (Qualified _) X` lookups in `Environment` whose values
   are `Type`-bearing structures (lower priority — separate experiment).

Adding a precomputed hash to each Type node lets us:
- Build a `Hashable Type` instance with O(1) hashing (no traversal).
- Short-circuit `eqType` on hash mismatch (different hash ⇒ different
  types, no traversal needed).
- Convert `unificationCache` to `HashSet (Type, Type)`, dropping
  `compareType` from this hot path entirely.

The construction-side cost should be cheap because:
- `combineFlags` already runs ~479M times per pr-admin build with
  literally 0% individual time (it's a single `(.|.)` on `Word8`).
  Hash combine is one `Int` mul/xor — same cost class.
- Leaf hashes (TypeVar's `Text`, TypeConstructor's `Qualified`, etc.)
  are only computed when leaves are *first* constructed, mostly at
  parse / extern-load. Inner-node reconstructions during traversals
  reuse children's cached hashes.

## Scope

In:
- Extend `TypeFlags` to carry both bit-flags and a hash.
- Compute and combine hashes in pattern synonym builders alongside
  flags, mirroring the existing combineFlags discipline.
- Add `Hashable Type` instance using the cached hash.
- Add hash short-circuit to `eqType`.
- Convert `unificationCache` from `Set` to `HashSet`.
- Tests and verification.

Out (deliberately):
- **Don't change `Ord Type`.** Hash-based ordering changes observable
  iteration order of types in `Map`/`Set`, which can break
  deterministic output, error message formatting, snapshot tests.
  See `LESSONS.md` § "A suspiciously large speedup" — rust-interning
  Phase 2 changed Ord and broke iteration order. We keep `compareType`
  structural; the win comes from avoiding the `Ord` path entirely via
  HashSet/HashMap substitutions, not from making the `Ord` path faster.
- Switching other Type-keyed containers (Environment Maps, etc.) —
  separate follow-up experiment if this one ships.

## Risks / things to watch

- **Hash combine cost on construction.** If hash combining starts
  showing up in profile, the win evaporates. Benchmark step 1 alone
  (TypeFlags extended, no eqType/HashSet changes yet) before step 2.
- **Hash quality / collisions.** Bad distribution → frequent fallback
  to structural compare. Use `Hashable` library hashes for leaves and
  `hashWithSalt` chaining for inner nodes; verify distribution against
  the pr-admin Type corpus.
- **Memory overhead.** TypeFlags goes from `Word8` (1B) to roughly
  `Word8 + Int` (~16B with alignment). At ~480M Type allocations per
  build, GC pressure may rise. Watch peak RSS and minor-GC behaviour.
- **`Hashable Type` ↔ `Eq Type` consistency.** Required by HashSet/
  HashMap: `a == b ⇒ hash a == hash b`. Trivially satisfied since
  hash is structural and Eq is structural.
- **The unificationCache might be unnecessary now.** If post-merges
  hit rate is low (skip-redundant-entailment-unify already drops the
  identical-type cases earlier), dropping it outright could beat the
  HashSet conversion. We instrument before committing to either path.

## Links

- Worktree: /workspace/p/type-hash
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Source profile: [profiles/baseline.meta.md](profiles/baseline.meta.md)
- Precedent: [synonym-opt](../synonym-opt/EXPERIMENT.md) — same
  TypeFlags pattern, proven cheap at scale
