# Handoff: type-hash

## TL;DR

**Win, ready to ship.** Step 3 (HashSet for `unificationCache`)
delivers **-15.4% on full builds** of pr-admin against baseline
`799e8208`, neutral on the three incremental scenarios. Two
commits on the `type-hash` branch:

- `c4001ef4` — step 1, extend TypeFlags with hash (foundation)
- `0c4c614e` — step 3, HashSet for unificationCache + INLINE on
  Hashable Type (the headline win)

Step 2 (hash short-circuit `eqType`) was tried and reverted; it was
a small net loss because the hot eqType callers compare equal types.

## Final results

Median of 4, baseline `799e8208`, pr-admin (1758 modules):

| Scenario | Base (s) | Head (s) | Δ | Notes |
|----------|---------:|---------:|---|-------|
| full | 57.0 | 48.2 | **-15.4%** | tight, 47515-49155 ms |
| nochange | 0.6 | 0.6 | -1.9% | tight, 564-594 ms |
| prelude | 4.0 | 4.0 | -0.3% | within noise |
| leaf | 1.6 | 1.6 | -1.1% | very noisy (1550-8576 ms) |

See `results.md` for the full append log including the dead-end runs.

## What landed

### Step 1 — TypeFlags with hash

`data TypeFlags = TypeFlags { tfBits :: !Word8, tfHash :: !Int }`,
combined at construction by the pattern synonym builders via per-
constructor salts and golden-ratio mixing. Hashable instances added
on leaf payload types (PSString, Label, Names, etc.). `Hashable Type`
exposes the cached hash in O(1).

Critical detail: `{-# UNPACK #-}` on the `!TypeFlags` field of every
`Type` constructor. Without it, every Type allocation goes through an
extra boxed object → +107% regression on full builds. With it, neutral.

`typeHash` and `typeBits` direct accessors (pattern-match Type, return
unboxed Int/Word8) avoid the TypeFlags box that `typeFlags` reconstructs
on each call. Hot paths use these.

### Step 2 — REVERTED

Hash short-circuit in `eqType` was a small net loss (~+0.7% on full).
The hot eqType callers (e.g. the skip-redundant guard at
`Entailment.hs:295`) compare equal types most of the time, so the
hash check just adds work without saving traversal.

### Step 3 — HashSet for unificationCache

`unificationCache :: HashSet (SourceType, SourceType)` in `CheckState`
instead of `Set`. `S.notMember` / `S.insert` → `not . HS.member` /
`HS.insert`. Drops `compareType` from this hot path entirely.

**Critical detail**: `{-# INLINE #-}` on the `Hashable Type` instance
methods. Without them, GHC dispatches through the class dictionary at
each hash call and HashSet measures **worse** than Set (+102% on full).
With INLINE, GHC specializes the instance for `Type SourceAnn` and the
chain `hashWithSalt → typeHash → field read` collapses to direct memory
access.

## Open follow-ups

- The same Hashable + INLINE pattern could win on other Type-keyed
  containers (Environment Maps in particular — `compare (Qualified _)`
  is still 4.1% of profile time). Separate experiment.
- `typeFlags` has UNPACK boxing semantics that surprised me. Worth
  documenting as a lesson.

## Ready to ship checklist

- [x] All four scenarios neutral or win
- [x] `stack test --fast` passes (1340 examples, 0 failures)
- [x] No Ord Type changes (rust-interning trap avoided)
- [x] Commits cleanly on top of `restaumatic`
- [ ] PR opened (when user is ready)
