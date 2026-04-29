# Results for unify-cache-anatomy

## Survey on full pr-admin compile (5713e832 baseline, single thread)

Run with `PURS_UNIFY_ANATOMY=1` on a from-scratch full build of pr-admin
(1758 modules). Survey hook records every cache lookup at
`Unify.hs:121-123` as `(hit?, combined node count of pair)` bucketed
into 1–2 / 3–10 / 11–50 / 51+ nodes. Histogram dumped on shutdown.

**Total cache lookups: 1,049,340**
**Total cache hits: 412,665 (39.33% hit rate)**

### Hits by combined pair size

| Bucket   | Hits     | % of hits |
|----------|---------:|----------:|
| 1–2      | 354,804  | **85.98%** |
| 3–10     |  47,111  | 11.42%    |
| 11–50    |   9,803  |  2.38%    |
| 51+      |     947  |  0.23%    |

### Misses by combined pair size

| Bucket   | Misses   | % of misses |
|----------|---------:|------------:|
| 1–2      | 225,264  | 35.38%      |
| 3–10     | 238,491  | 37.46%      |
| 11–50    | 117,282  | 18.42%      |
| 51+      |  55,638  |  8.74%      |

### Key findings

1. **86% of cache hits are on tiny pairs** (1–2 nodes total). A pair
   of size 2 is exactly two leaf nodes — almost always
   `(TypeConstructor c, TypeConstructor c)` or
   `(TypeVar v, TypeVar v)`. The cache's dominant job is **memoizing
   constructor-equal-itself recurrences** from recursive descent
   through `TypeApp`/`KindApp`/`RCons` etc.

2. **97% of hits are on small pairs** (≤10 nodes). The cache is **not**
   primarily memoizing big-tree work — only 0.23% of hits (947 out of
   412k) are on pairs >50 nodes.

3. **Misses skew bigger** — 27% of misses are on pairs ≥11 nodes
   vs 2.6% of hits. Big pairs are typically novel and need to actually
   be unified. The cache catches the small repeating ones.

4. **Confirms the recursive-descent hypothesis.** The 354k 1–2-node
   hits cannot all originate at the 3 hot funApp/abs/array sites
   (those produce ~214k outer calls per the lineage survey). The
   excess (~140k) must come from recursive `unifyTypes` calls inside
   `unifyTypes'` — particularly the TypeApp two-child case
   (Unify.hs:147-149) and the structural recursion in `unifyRows`.

### Implications for algorithmic alternatives

A. **Tag-equality fast-path at the wrapper.** What the cache mostly
   does: short-circuit `unifyTypes (TypeConstructor c) (TypeConstructor c)`.
   This is already a one-liner inside `unifyTypes'` (line 143-144).
   The cache adds value because it sits *before* `substituteType` and
   the hint stack push/pop. A pre-substitute fast-path on
   constructor-equality could eliminate ~86% of cache work without a
   HashSet:

   ```haskell
   unifyTypes (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = pure ()
   unifyTypes (TypeVar _ v1) (TypeVar _ v2) | v1 == v2 = pure ()
   unifyTypes t1 t2 = do
     sub <- gets checkSubstitution
     withErrorMessageHint (ErrorUnifyingTypes t1 t2) $
       unifyTypes'' (substituteType sub t1) (substituteType sub t2)
   ```

   Soundness: identical constructors / type vars unify to themselves
   trivially, no substitution change, no error possible.

   Open question: would this hit the same prelude regression?
   `skip-redundant-funapp-unify` and `funapp-pattern-match` both
   skip outer calls without going through `unifyTypes`, so they're
   not analogous. This *is* analogous but at a finer granularity —
   only triggers when both args are leaf-tag-equal, regardless of
   call site. Worth a dedicated experiment.

B. **Drop cache for small pairs, keep for big.** With 86% of hits on
   1-2-node pairs, those entries dominate the HashSet. If a tag-eq
   fast-path catches them upstream, the remaining cache could be
   tiny — possibly a small ring buffer instead of an unbounded
   HashSet. Less GC pressure, possibly less hash work.

C. **TypeFlags-driven structural eq at every node.** With the
   per-node `tfHash` already cached, `eqType` could short-circuit at
   each subtree on hash inequality. This makes recursive descent
   self-memoizing — every recursive `unifyTypes` on identical
   subtrees becomes O(1) hash compare without any cache. But this
   is roughly what `unify-pattern-survey` Phase 2 tested (-2.8%
   full, +7.1% prelude). Falsified for "no-cache + hash-eq"; might
   work as "keep-cache + hash-eq early bail" but that's basically
   option A.

D. **Per-binding-group cache flush.** Untested. Would tell us
   whether the cache's value is intra-group vs cross-group within
   a module. Cheap diagnostic but not algorithmic.

### Recommended next experiment

**`unify-leaf-fast-path`** — option A. Add a pre-substitute clause
to `unifyTypes` that returns immediately on
`(TypeConstructor c, TypeConstructor c)` and
`(TypeVar v, TypeVar v)` equal-tag pairs (and possibly
`TypeLevelString`/`TypeLevelInt`). Keep the HashSet cache for
everything else. Hypothesis: catches the 86% of cache hits without
HashSet bookkeeping; doesn't regress prelude because it's not
skipping the wrapper for non-leaf calls (which is what the prior
experiments did).

If that works → follow up with option B (shrink the cache).
If it regresses prelude → the regression mechanism is genuinely
something else (maybe GC residency of the HashSet itself, or
substituteType allocation patterns), and that's the next survey.

## Verdict

Survey complete. Recommend `unify-leaf-fast-path` as the next
experiment.
