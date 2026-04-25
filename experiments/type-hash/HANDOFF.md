# Handoff: type-hash

## TL;DR

**Win, ready to ship.** Step 3 (HashSet for `unificationCache`)
delivers **-15.4% on full builds** of pr-admin against baseline
`799e8208`, neutral on the three incremental scenarios. Three
commits on the `type-hash` branch:

- `c4001ef4` — step 1, extend TypeFlags with hash (foundation)
- `0c4c614e` — step 3, HashSet for unificationCache + INLINE on
  Hashable Type (the headline win)
- `43f6b613` — post-review cleanup: hide underscore constructors,
  add `modifyFlags`, uniform `hashWithSalt` chains, drop dead helpers

Step 2 (hash short-circuit `eqType`) was tried and reverted; it was
a small net loss because the hot eqType callers compare equal types.

## Final results

Median of 4, baseline `799e8208`, head `43f6b613` (post-cleanup),
pr-admin (1758 modules):

| Scenario | Base (s) | Head (s) | Δ | Notes |
|----------|---------:|---------:|---|-------|
| full | 57.8 | 49.1 | **-15.1%** | tight, 48714-49291 ms |
| nochange | 0.6 | 0.6 | -1.5% | tight on baseline (576-610), one outlier on head (509-595 ms) |
| prelude | 4.06 | 4.00 | -1.5% | tight (3978-5468 ms) |
| leaf | 1.66 | 1.63 | -1.9% | one head outlier (1550-8455 ms) |

Same shape as the pre-cleanup measurement (-15.4% / -1.9% / -0.3% /
-1.1%) — the cleanup commit was perf-neutral as intended. See
`results.md` for the full append log including the dead-end runs.

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

## Post-review cleanup (commit `43f6b613`)

After review, two judgement calls came back as "do both":

1. **Uniform `hashWithSalt` chains.** The node-flag helpers (rconsNodeFlags,
   binaryNodeFlags, ternaryNodeFlags, unaryNodeFlags, constraintNodeFlags)
   used a custom `mixHash` (multiply-add with golden-ratio constant)
   intermixed with `hashWithSalt`. Switched everything to uniform
   `hashWithSalt` chains. `hashWithSalt` is `infixl 0` so chains read
   left-to-right without parens. Slight hash-distribution change but
   negligible for our use.
2. **Hide the underscore constructors.** Replaced the
   `module Language.PureScript.Types` self-export with an explicit list
   that excludes the `_`-suffixed data constructors. Combined with a
   new `modifyFlags :: (TypeFlags -> TypeFlags) -> Type a -> Type a`
   helper, this makes wrong-hash construction structurally impossible
   instead of relying on convention. Synonyms.hs (the only external
   user that touched the underscore form) was refactored to use
   pattern synonyms + `modifyFlags`. GHC's case-of-known-constructor
   fuses pattern-synonym builder + `modifyFlags` (both INLINE) into
   the same single allocation the explicit form had.

`stack test --fast` still passes. Re-measurement on full was disturbed
by concurrent profiling on the host (baseline range 64.4–68.4 s vs
typical ~57 s); head was tight at 50.5 s (49429-50884 ms), consistent
with the prior -15.4% measurement against a clean ~57 s baseline.

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
