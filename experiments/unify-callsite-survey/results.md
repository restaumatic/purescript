# Results for unify-callsite-survey

Append-only. See experiments/SCHEMA.md for format.

## Phase 1 — characterization (counts, no timing)

Run: pr-admin clean build, `PURS_UNIFY_CALLSITE_SURVEY=1`, head SHA
at the survey-instrumentation commit (counters + tags only). Single
run. Survey reads the live `unificationCache` membership at each
external call site, so "cache_hit" here is the **exact** predicate
the cache uses, not the hash-equality proxy from
`unify-pattern-survey`.

```
=== unify-callsite-survey ===
total external calls:       641,991
cache-hit calls:            280,931  ( 43.8%)

by call site (hits / total / hit% / share-of-all-hits):
  Types:funAppHead                 174,161 /   175,294   99.4%   62.0%
  Subsumption:default               49,384 /   179,947   27.4%   17.6%
  Types:checkAbsArrow               29,067 /    29,726   97.8%   10.3%
  Types:binderConstructor            9,977 /    28,647   34.8%    3.6%
  Types:checkArrayHead               9,009 /     9,283   97.0%    3.2%
  Entailment:fundepEnforce           3,253 /   136,042    2.4%    1.2%
  Subsumption:rowSubsume             1,915 /     7,781   24.6%    0.7%
  Entailment:substPairwise           1,825 /     2,665   68.5%    0.6%
  Types:checkUnknown                 1,584 /    62,347    2.5%    0.6%
  Types:arrayLiteralElement            266 /     1,201   22.1%    0.1%
  Types:binderString                   150 /       181   82.9%    0.1%
  Types:binderInt                      115 /       155   74.2%    0.0%
  Types:ifThenElse                      84 /       262   32.1%    0.0%
  Types:binderBool                      69 /        90   76.7%    0.0%
  Types:binderTyped                     44 /       209   21.1%    0.0%
  Types:binderChar                      16 /        18   88.9%    0.0%
  Types:checkPropertiesEnd               9 /       865    1.0%    0.0%
  Types:binderNumber                     3 /         5   60.0%    0.0%
  (sites with 0 hits: binderArray, binderObjectRecord,
   binderObjectRowTail, bindingGroupElement,
   checkPropertiesExtend, funAppUnknown, letBinding)
```

### Headline

**~75% of all cache hits come from three "extract-then-verify" sites
in `Types.hs`.** All three pull a type constructor out of a TypeApp
shape and then unify it with a known constant — the unify is an
assertion that the extracted constructor really is what the shape
implies, and 97–99% of the time it already is.

| Site | Code shape | Hits | Hit rate | Share |
|---|---|---:|---:|---:|
| `Types:funAppHead` | `unifyTypes tyFunction' tyFunction` (head of fn application) | 174,161 | 99.4% | 62.0% |
| `Types:checkAbsArrow` | `unifyTypes t tyFunction` (head of `Abs` against arrow type) | 29,067 | 97.8% | 10.3% |
| `Types:checkArrayHead` | `unifyTypes a tyArray` (head of array literal against `Array` type) | 9,009 | 97.0% | 3.2% |
| **subtotal** | | **212,237** | | **75.5%** |

The fourth-largest contributor, `Subsumption:default` (17.6% of
hits), is a different pattern — the catch-all `subsumes' mode ty1
ty2 = unifyTypes ty1 ty2`. Hit rate is moderate (27.4%) and call
volume is the highest of any site (179,947). It's not a "verify
constant" site; it's a real load-bearing unification.

### Interpretation

The three concentrated sites are **structural assertions**, not
unifications. The pattern in each case is:

```haskell
checkFunctionApplication' fn (TypeApp _ (TypeApp _ tyFunction' argTy) retTy) arg = do
  unifyTypes tyFunction' tyFunction   -- ← assertion
  ...
```

When the outer pattern matches, `tyFunction'` is whatever
constructor was at the head of the doubly-nested TypeApp. The unify
forces the assertion that it actually IS the function constructor.
For any well-typed program we've seen so far, it always is. The
0.6%–3% miss rate represents either malformed types (real type
errors that surface later) or some rare structural case we haven't
classified yet.

These calls go through the cache, hash both pairs, hit, and return
— all to verify "yes, the function constructor is the function
constructor." That's exactly the work the cache exists to skip,
and exactly the work an `eqType` short-circuit at the call site
could skip cheaper.

### Why upstream eqType beats the cache here

The cache pays:
- Compute `S.member (t1, t2) cache` — `compare` on a (Type, Type)
  pair → at minimum two `compareType` walks.
- On miss, `S.insert` rebuilds the spine.
- Membership is `O(log n · compareType)`, where n grows unbounded
  through a typecheck.

An `eqType` short-circuit at the call site pays:
- One `eqType` walk against a *constant* (`tyFunction` /
  `tyArray`) — those are 1-node `TypeConstructor`s, so the walk
  is a single pattern-match in 99%+ of cases.
- Skips both the unify and the cache-add.

Net: replace ~212K cache lookups with ~212K trivial pattern matches.

### Implications

The previous `unify-pattern-survey` concluded the cache is
essentially optimal for what it does (sound pair memoization).
This survey shows there's a much cheaper alternative *for the
sites that produce most of the cache hits* — eliminate the
upstream calls instead of making the cache faster. After that,
the remaining cache traffic (the ~25% spread across other sites)
might be small enough that a much smaller, simpler cache (or
none at all) is viable.

## Verdict

Phase 1 produces a clear **concentrated finding**: 75% of cache
hits originate at three "verify-this-extracted-constructor"
assertions in `Types.hs`. Those calls are essentially free to
skip with an `eqType` short-circuit against a known constant.

This unblocks a follow-up optimisation experiment
(`skip-redundant-funapp-unify` or similar) that:
1. Adds `unless (eqType tyFunction' tyFunction)` /
   `unless (eqType a tyArray)` short-circuits at the three sites.
2. Re-measures cache hit-rate to confirm ~75% reduction.
3. Drops the cache (or shrinks it) and benchmarks the four
   scenarios.

Closing this experiment as **win for characterization, follow-up
needed for shipping.**
