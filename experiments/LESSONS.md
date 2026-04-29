# Cross-experiment lessons

Distilled learnings from performance experiments on the PureScript
compiler. Read this before starting a new experiment — particularly
the "dead-end techniques" section, so you don't re-attempt something
that's already been shown not to work on our workload (pr-admin, 1758
modules).

Each entry names the experiment it came from so you can dig into the
details. Append new entries when closing an experiment.

## Workload baseline

All numbers on this page are measured against `pr-admin`
(`/workspace/restaumatic/apps/pr-admin`, 1758 modules) unless noted.
Baseline numbers on the `restaumatic` branch, optimised build:

| Scenario                              | Time    |
| ------------------------------------- | ------- |
| Full build                            | ~72–73s |
| No-change rebuild                     | ~1.1s   |
| Touch leaf (timestamp only)           | ~1.2s   |
| Comment change to Prelude (1342 deps) | ~2.3s   |

Headline deltas quoted elsewhere are relative to those numbers. A
"+1% full" penalty is easily inside noise on a busy machine; a real
regression is usually ≥5%.

## Correctness traps

### A suspiciously large speedup usually means you broke semantics
**From:** `rust-interning` (see
`/workspace/p/rust-interning/PHASE2-RESULTS.md` and
`/workspace/p/rust-interning/profile-results.log`)

The Phase 2 Label-interning work measured an 80.7% improvement (57s →
11s) with "all 1340 tests passing." The speedup was real but came from
an `Ord` instance that compared interning ids rather than the
underlying string bytes. That is wrong whenever label iteration order
is observable (row normalisation, error formatting, deterministic
output). Our test corpus didn't exercise those paths.

Once the `Ord` instance was corrected, the same interning work
measured **+29% slower** than baseline (second-order lookup cost
exceeded the O(1) compare saving). A subsequent "caching" variant
recovered to -3.4%. A later commit regressed to **+148% slower** —
the variance suggests an unstable optimisation.

**Takeaways:**
1. When a perf experiment shows >20% improvement on a well-trodden
   path, assume something is broken and look for it explicitly. 5–10%
   is the normal range for a correct single-axis optimisation.
2. Tests passing is not semantic confirmation — we need test cases
   that depend on iteration order (error-message snapshot tests,
   deterministic-output tests) before trusting any change to `Ord`.
3. The 80.7% headline in `PHASE2-RESULTS.md` should be treated as
   refuted; the experiment's actual state is unresolved.

## Dead-end techniques (do not re-attempt without new evidence)

### Caching cheap per-decl work across builds
**From:** `tc-queries` (see `p/tc-queries/HANDOFF.md`)

For cheap declaration kinds (`DataDeclaration`,
`TypeSynonymDeclaration`, `TypeClassDeclaration`, `ExternDeclaration`,
`ExternDataDeclaration`, `RoleDeclaration`, `TypeInstanceDeclaration`)
the typecheck work per decl runs in low tens of microseconds. Adding a
cache — fingerprint + env-delta compute + env-delta apply — costs
*more* than the work it skips. On pr-admin, caching these decl kinds
adds +4–6% to full builds and +13% to incremental edits, even with
100% cache hit rates. The overhead is per-decl, not per-miss.

**Takeaway:** don't cache work that costs less than a hash+map-diff.
The prize is value-groups, not cheap decls. Any future caching
experiment should bring a measurement that the work-per-item exceeds
the caching overhead before building the cache.

### Changing PSString internal representation (ShortByteString)
**From:** `rust-interning` / earlier `rows-optimization` attempt
(see `/workspace/p/rust-interning/perf.md`)

Replacing PSString's internal `[Word16]` with `ShortByteString` for
better cache locality measured **+3.5% slower** on pr-admin. The
conversion overhead at every call site (`toUTF16CodeUnits`,
`fromUTF16CodeUnits`, JSON serialisation round-trips) dwarfed the
comparison-speed gain — because, per the above, PSString comparison
isn't the bottleneck we thought it was.

**Takeaway:** if you're changing a core representation, count the
conversion sites first. If the existing API shape forces conversions
around the hot path, the representation change has to pay for those
conversions before it can even start winning.

### Serialising elaborated value declarations to disk
**From:** `tc-queries` (see `p/tc-queries/HANDOFF.md`)

The elaborated `Declaration` output of `typesOf` embeds fully-typed
expressions (`TypedValue` annotations, dictionary references, source
spans everywhere). CBOR-serialising these to disk produces megabytes
per decl, aggregating to ~2 GB on pr-admin's ~17k value decls. Disk
I/O and in-memory allocation at that scale dwarfs any typecheck saving
(full build goes 73s → 217s).

**Takeaway:** a within-module cache for value groups is valuable in
principle, but not with the elaborated representation. Any future
attempt needs either (a) a smaller cache shape — e.g., env-delta only,
with codegen refactored to operate on un-elaborated decls plus
separately-cached type info — or (b) aggressive compression. Just
serialising the current types will lose.

## Confirmed boundaries worth exploiting

### `withFreshSubstitution` marks natural query boundaries
**From:** `tc-queries`

Both `typesOf` (`src/Language/PureScript/TypeChecker/Types.hs:93`) and
`kindsOfAll` (`src/Language/PureScript/TypeChecker/Kinds.hs:957`) start
with a clean unification substitution and don't leak it. These are
self-contained computation units — good Rock-query boundaries in
principle. The infrastructure for dispatching through queries is
already wired up on the `tc-queries` branch, but caching was blocked
by the serialisation cost above.

### Per-module extern hash reuse is mandatory
**From:** `tc-queries`

If you want to fingerprint anything per-decl-within-module, you have
to reuse per-module extern hashes across all modules that depend on
them. Re-serialising dep externs for every module with any cached
decl was a 10s+ cost on pr-admin. See `externHashRef` on the
`tc-queries` branch.

### Per-node TypeFlags short-circuits hot traversals
**From:** `synonym-opt` (shipped — combined with skip-redundant-entailment-unify
gave -22.9% full build on pr-admin)

`replaceAllTypeSynonyms'`, `replaceTypeWildcards`, `introduceSkolemScope`,
and `substituteType` walk every node of every type tree at ~40 call sites
per typecheck. Most subtrees are already clean — synonym-free, wildcard-free,
scoped, unknown-free. Adding a small `Int` bitfield to every `Type`
constructor (`TypeFlags`) with auto-computation via pattern synonyms lets
each traversal short-circuit on already-clean subtrees. Crucially, the flag
is computed once at construction and combined from children's flags, so
there is no per-call recomputation cost.

The biggest single contributor is `tfHasUnknowns` for `substituteType`:
every `unifyTypes` call substitutes both sides before unifying, and most
types in flight contain no unknowns at all. Walking them was pure overhead.

**Takeaway:** when a hot traversal's body is "do nothing in 90% of subtrees",
caching the property that triggers the no-op on the constructor is cheap
and effective. Use pattern synonyms so callers don't have to thread the flag.

### Looking for redundant work, not just expensive work
**From:** `skip-redundant-entailment-unify` (shipped — -15.5% full build alone)

Original hypothesis was a within-module memo table for entailment results
(87% redundancy on wide-row HasField calls in pr-admin). While instrumenting,
we noticed the actual redundant work isn't repeated *solves* — it's individual
solves whose final unification step does no useful work because both sides
are structurally equal already. A two-line `unless (eqType inferredType t2)`
guard captures this without any cache, keying logic, or invalidation concerns.

**Takeaway:** when profiling shows "this hot path is expensive AND repeated",
the impulse is to memoize. But check first whether each call is producing
information at all — sometimes the cheaper fix is to short-circuit the
no-op cases. `eqType` walks lockstep with no allocation; `unifyTypes` on
identical wide rows does sort + allocate + merge-join. Same outcome, very
different cost.

### Per-node structural hash + HashSet for ordered-key caches
**From:** `type-hash` (shipped — -15.4% full build on pr-admin)

After synonym-opt + skip-redundant-entailment-unify shipped, `compareType`
became the new top hotspot at 7.7%, mostly from `Set (Type, Type)` lookups
in the `unificationCache` at `Unify.hs:121–123`. Caching a structural hash
on every Type node (alongside the existing `TypeFlags`, computed at
construction by combining children's hashes with a per-constructor salt)
gives a `Hashable Type` instance with O(1) hash. Switching the cache to
`HashSet (Type, Type)` then drops `compareType` from the hot path entirely:
HashSet does O(1) hash + ~1 `eqType` per op, vs Set's O(log n) `compareType`.

Two non-obvious implementation details are load-bearing — see the GHC
representation pitfalls section below.

**Takeaway:** when a `compare`-based container shows up high in profile
and you can compute a cheap, cached hash for the keys, the right move is
usually to switch the container to a HashMap/HashSet rather than try to
make the compare function itself faster. The win lives in the algorithmic
change (O(1) vs O(log n)), not in micro-optimising the comparison.

## Performance representation pitfalls (GHC-specific)

These bit us hard during `type-hash` and would bite again on any similar
work. Both look like "minor codegen hints" but have 2× swings on full
builds when missed.

### `{-# UNPACK #-}` on strict multi-field structural fields
**From:** `type-hash` (shipped)

Going from `newtype TypeFlags = TypeFlags Word8` to
`data TypeFlags = TypeFlags { tfBits :: !Word8, tfHash :: !Int }` measured
**+107% on full builds** without `{-# UNPACK #-}` on the `!TypeFlags`
field of every `Type` constructor. The newtype was zero-cost (the `Word8`
sat directly inside `Type`); the new data type is a separate boxed heap
object reached via pointer. Each `Type` allocation gained an extra heap
object and an extra indirection. Adding `{-# UNPACK #-} !TypeFlags`
unpacks the `Word8 + Int` pair into the parent constructor and the
regression collapses to ~+1.8%.

Note that `typeFlags :: Type a -> TypeFlags` then has to *reconstruct*
the box at every call — for hot paths that only need one of the two
fields, write a direct accessor (`typeHash`, `typeBits`) that pattern-
matches `Type` and returns the unboxed Int/Word8. The same boxing trap
that bit us on construction also bites on extraction.

**Takeaway:** when changing a strict structural field from a newtype
single-byte/word wrapper to a multi-field record, **always** UNPACK both
the outer field and the inner fields. And budget for boxing-on-read at
every accessor call — if a hot path only needs one field, don't go via
the typed wrapper.

### `{-# INLINE #-}` on `Hashable` instance methods
**From:** `type-hash` (shipped)

The first attempt at `HashSet (Type, Type)` for the `unificationCache`
measured **+102% on full builds**. The Hashable instance was

```haskell
instance Hashable (Type a) where
  hash = typeHash
  hashWithSalt s t = s `hashWithSalt` typeHash t
```

Without `{-# INLINE #-}` on `hash`/`hashWithSalt`, GHC dispatches through
the class dictionary at every call, which (a) prevents specialisation of
the `(Type, Type)` tuple Hashable through to `typeHash`, and (b) allocates
dictionary thunks. Adding INLINE on both methods turned the +102%
regression into a -15.4% win — same code, same cache, same data structure;
just a codegen hint.

**Takeaway:** `Hashable` instances on hot Map/Set keys must mark their
methods INLINE (or INLINABLE) so the dictionary collapses through to the
underlying field read at the call site. The generic tuple instance
`Hashable (a, b)` won't specialise through unless the inner instances
inline. Same lesson likely applies to any other class instance used in
HAMT-style containers.

## Dead-end techniques (do not re-attempt without new evidence)

### Hash-prefix short-circuit on `eqType`
**From:** `type-hash` (step 2, reverted)

Adding `eqType t1 t2 = typeHash t1 == typeHash t2 && eqTypeStructural t1 t2`
measured a small *net loss* (~+0.7% on full vs the no-shortcut baseline).
For unequal types the hash check is a clear win, but the hot eqType
callers in this codebase compare *equal* types most of the time — e.g.
the `unless (eqType inferredType t2)` guard in `Entailment.hs:295`
exists precisely to skip work when types are already equal. So the
hash check fires on a True case, falls through to structural anyway,
and only adds work.

**Takeaway:** before adding a fast path keyed on hash equality, profile
whether the hot callers are the equal-types or unequal-types case.
Short-circuiting only helps the side you're not already on.

## Instrumentation in Unify.hs contaminates measurement (`unify-cache`)

Adding ~50 lines of `NOINLINE`-marked top-level definitions to
`Unify.hs` — IORef counters, `unsafePerformIO` wrappers, a
`dumpUnifyCacheStats :: IO ()` callable from `Command.Compile` — caused
**+135% on full** even though the new definitions were never called from
the hot path. Binary shrank from 49.1 MB → 46.7 MB, signalling that GHC
made significantly different inlining decisions for the rest of the
module.

The hot path itself was unchanged. The only signal during iteration was
the absurd absolute timing (full builds inflating from ~50 s to ~120 s)
and the binary-size shrink. Once the instrumentation was pulled out,
timing returned to baseline immediately.

This is a much bigger effect than the previously documented Unify.hs
inlining sensitivity (the `eqType` guard story under
`entailment-memo`'s -31% / 2 MB-shrink result). The trigger here is the
*module-level* presence of `unsafePerformIO` IORef CAFs, not a hot-path
edit.

**Takeaway:**
- Never put instrumentation infrastructure in `Unify.hs`. Put IORefs,
  unsafePerformIO state, and any debug dumps in a *separate* module
  imported only when measurement is needed.
- Binary size is a primary signal. A >1 MB delta on `Unify.hs`-touching
  changes means GHC's inlining decisions changed; treat the timing as
  uninterpretable until the size delta is understood.
- Manual single-shot `time purs compile $(spago sources)` against the
  worktree binary is the fastest way to spot contamination — do this
  before kicking off a multi-scenario harness run.

The `unify-cache` experiment itself ended no-win (the cache is net
positive: dropping it costs +24% on full; specialised `Hashable
UnifyKey` is a wash), but the measurement-discipline lesson is the
load-bearing finding.

## The unification cache is a hash-equal memoizer (`unify-pattern-survey`)

Survey on pr-admin (1.05M lookups, 39.3% hit rate): **412,311 of
412,727 cache hits — 99.9% — are hash-equal pairs.** Only 416 hits
involved hash-distinct pairs (mostly with synonym differences).

This means the cache's job is essentially "skip a unification we
already decided is `t ~ t`." Any cheaper replacement has to either
walk the structure (`eqType`, regresses prelude +7%) or trust the
hash (sound only modulo 64-bit collisions, which makes the
compiler unsound in principle).

Phases tried, all on baseline 43f6b613:

| Scheme | full | nochange | prelude | leaf | Soundness |
|--------|-----:|---------:|--------:|-----:|-----------|
| HashSet (orig) | — | — | — | — | sound (Eq fallback) |
| `typeHash ==` + `eqType`, no cache | -2.8% | -2.0% | **+7.1%** | -0.3% | sound |
| IntSet of `mix(h1,h2)`, cache kept | +0.1% | +0.4% | +0.7% | -3.3% | unsound |
| `typeHash ==` only, no cache | -0.9% | -0.7% | -0.6% | -2.8% | unsound |

**Takeaway:** the cache earns its keep precisely by being a hash-equal
memoizer, and the original HashSet+Eq pair is essentially optimal
for this discipline. The ~19% Hashable cost is the unavoidable
price of sound pair memoization. Real wins here would need either
a fundamentally different structure (small-LRU, bloom-filter
front-end, per-module reset) or upstream reduction of redundant
unification calls (extending the work `skip-redundant-entailment-unify`
started). Don't re-attempt hash-only replacement of the cache —
the soundness loss isn't worth the marginal speed.

## Most cache hits are constructor assertions (`unify-callsite-survey`)

Follow-up call-site survey on pr-admin (641,991 external `unifyTypes`
calls, 43.8% cache-hit rate measured against the live cache):
**75.5% of all cache hits originate at three "extract a head
constructor from a TypeApp shape, then unify with a constant"
sites in `Types.hs`**, all with 97–99% hit rates.

| Site | Code shape | Hits | Hit% | Share |
|---|---|---:|---:|---:|
| `Types:funAppHead` | `unifyTypes tyFunction' tyFunction` (line 1015) | 174,161 | 99.4% | 62.0% |
| `Types:checkAbsArrow` | `unifyTypes t tyFunction` (line 841) | 29,067 | 97.8% | 10.3% |
| `Types:checkArrayHead` | `unifyTypes a tyArray` (line 835) | 9,009 | 97.0% | 3.2% |

Each of these is a structural assertion — once the outer pattern
matches `(TypeApp _ (TypeApp _ tc _) _)`, the inner `tc` is
practically always the function/array constructor already, and the
unify is a no-op the cache then short-circuits.

Each of these is a structural assertion — once the outer pattern
matches `(TypeApp _ (TypeApp _ tc _) _)`, the inner `tc` is
practically always the function/array constructor already, and the
unify is a no-op the cache then short-circuits.

**The follow-up `skip-redundant-funapp-unify` falsified the
implied win:** adding `unless (eqType x const)` guards at all three
sites was neutral on the S.Set baseline and **regressed prelude
+6.4% on the HashSet baseline** — same shape as
`unify-pattern-survey` Phase 2 (+7.1% prelude). The cache hits
characterised here recur across the 1,342-module prelude cascade
and benefit from HashSet amortisation that the upstream eqType
skip can't match. See the `skip-redundant-funapp-unify` lesson
below for the full mechanism.

**Survey vs ship is two different questions.** This survey
correctly identified where cache hits cluster, but "75% of cache
hits at 3 sites" does not imply "75% of cache cost at 3 sites" —
the dominant pairs at these sites are tiny constants on which the
cache is unusually cheap. Future surveys should weight by *cost*
(e.g. type size, hash walk length), not raw count, when projecting
optimisation potential.

## Cache-hit reduction ≠ speed win when hits are on tiny pairs (`skip-redundant-funapp-unify`)

The follow-up to `unify-callsite-survey`: added three
`unless (eqType x const) $ unifyTypes x const` guards at the
identified concentrated sites (funAppHead, checkAbsArrow,
checkArrayHead, 75.5% of all cache hits between them).

Measured on **both** baselines, because the call-site survey
characterised the cache on the post-type-hash branch (HashSet,
~19% Hashable cost) while the current shipped tip is pre-type-hash
(`S.Set`, ordered by structural compare):

| Baseline | full | nochange | prelude | leaf | Verdict |
|---|---:|---:|---:|---:|---|
| 799e8208 (S.Set, pre-type-hash) | -0.0% | -1.9% | +2.3% | -1.6% | neutral |
| 43f6b613 (HashSet, post-type-hash) | -0.2% | -1.2% | **+6.4%** | +2.5% | regresses prelude |

**Two distinct mechanisms.** On the S.Set baseline the cache
lookup on tiny constants is already O(1) (compare bottoms out at
the constructor tag). The eqType substitute is also one
constructor-tag comparison; we trade trivial work for trivial work
and gain nothing. On the HashSet baseline the cache is doing real
work (Hashable walks the type), but skipping the cache lookup
upstream regresses prelude — the same shape as
`unify-pattern-survey` Phase 2 (typeHash + eqType + no cache:
prelude +7.1%). Plausible mechanism: the prelude cascade rebuild
typechecks 1,342 modules against a long-lived substitution + cache,
and the same trivial pairs flow through repeatedly. The HashSet
hit catches them in one bucket walk; with the upstream eqType skip,
recursive calls (e.g. through `unifyTypes' (TypeApp …) (TypeApp …)`)
miss the cache, run unifyTypes', and re-insert — net more work.

**Combined takeaway:** the unification cache plus its Hashable
infrastructure is essentially optimal across all four scenarios on
the post-type-hash branch. The
upstream-skip pattern that worked at the single entailment-fundep
site (`skip-redundant-entailment-unify`, -15.5% full) does **not**
generalise to the funApp / array / abs sites — those fire
repeatedly across the prelude cascade and benefit from the cache's
amortisation. Don't repeat this experiment on the post-type-hash
baseline; the prelude regression is structural.

**Implication for future work:**
- Don't pursue more eqType-guard sites at hot recurring call points.
  The pattern only ships when the call site has *unique*
  redundancy that fires few times per dictionary/instance.
- The cache earns its keep on the post-type-hash branch precisely
  because the prelude cascade re-uses the same pairs. Cache-replacement
  schemes have to handle that re-use pattern or they regress prelude.
- Branch `skip-redundant-funapp-unify-th` (off 43f6b613, +3 guards,
  +6.4% prelude) is parked as the canonical falsifier of this
  hypothesis.

## 99.9% of unifyTypes calls at the 3 hot sites are trivially equal (`funapp-lineage-survey`)

Lineage histogram on a from-scratch full pr-admin compile (post-type-hash
baseline 5713e832, env-gated `PURS_FUNAPP_LINEAGE=1` recording
`(callsite, hash t1, hash t2)`):

| Callsite        | Calls    | Distinct (h1, h2) pairs | Top pair share |
|-----------------|---------:|------------------------:|---------------:|
| funAppHead      | 175,272  | 5                       | 99.998%        |
| checkAbsArrow   |  29,720  | 140                     | 99.5%          |
| checkArrayHead  |   9,276  | 63                      | 99.3%          |

**214,049 of 214,268 calls (99.9%) are `tyFunction ~ tyFunction` or
`tyArray ~ tyArray` — same constant on both sides.** The remaining
0.1% is calls where `t1` is a fresh `TUnknown` (each gets a distinct
hash; needs to actually be solved).

The pattern `TypeApp _ (TypeApp _ x argTy) retTy` at these 3 sites
matches *structurally* — it doesn't statically prove that `x` is
`tyFunction`/`tyArray`. So the existing code asserts the head with
`unifyTypes x tyFunction`. In 99.9% of compile-time calls, x already
**is** the constant. The cache catches these as hits, but the call is
still made — pure waste at the call-graph level that the cache
amortises but doesn't eliminate.

**Why this characterisation matters more than the earlier
`unify-callsite-survey` cache-hit count.** The earlier survey weighted
by *cache hits*; this one weights by raw structural redundancy and
shows that the dominant pairs aren't just frequent — they're a tiny
constant set (5 distinct pairs at funAppHead). That's the right
signal for an algorithmic fix that bypasses both the call and the
cache.

**Implication.** The follow-up `funapp-pattern-match` falsified the
code-gen hypothesis. Replacing the call with a nested constructor
pattern (`TypeApp _ (TypeApp _ (TypeConstructor _ C.Function) …) …`)
produced **+7.0% prelude** on the same baseline — same shape as
`unify-pattern-survey` Phase 2 (+7.1%) and `skip-redundant-funapp-unify`
(+6.4%). Three different implementations skip the call three different
ways, all regress prelude by the same ~7%. The mechanism is therefore
not how the skip is expressed in source — it's structural to skipping
the `unifyTypes` call at these 3 sites.

**Takeaway:** the unification cache plus its `unifyTypes` wrapper
(substituteType + hint stack + cache lookup) at funAppHead /
checkAbsArrow / checkArrayHead is doing something the prelude
cascade depends on, beyond just memoizing equal pairs. Skipping
the call — by any mechanism — costs ~7% on prelude. Don't pursue
further variants of "skip the call at these 3 sites" without first
characterising what the call is contributing on the cascade path
(beyond the cache hit it ultimately resolves to).

## 86% of unification cache hits are constructor-self pairs (`unify-cache-anatomy`)

Hooked the cache lookup site (Unify.hs:121-123) on a from-scratch
full pr-admin compile (5713e832 baseline) and bucketed the 412,665
hits by combined pair node count:

| Bucket   | Hits    | % of hits |
|----------|--------:|----------:|
| 1–2      | 354,804 | **85.98%** |
| 3–10     |  47,111 | 11.42%    |
| 11–50    |   9,803 |  2.38%    |
| 51+      |     947 |  0.23%    |

A pair of size 2 is two leaf nodes — almost always
`(TypeConstructor c, TypeConstructor c)` or `(TypeVar v, TypeVar v)`.
**The cache's load-bearing job is memoizing constructor-equal-itself
recurrences from recursive descent**, not memoizing big-tree work
(only 0.23% of hits are >50 nodes). 214k of these hits originate at
the 3 hot funApp/abs/array sites (per `funapp-lineage-survey`); the
remaining ~140k come from recursive `unifyTypes` calls inside
`unifyTypes'` — particularly the TypeApp two-child recursion at
Unify.hs:147-149.

Misses, by contrast, skew bigger: 27% of misses are pairs ≥11 nodes
vs 2.6% of hits. Big pairs are typically novel.

**Implications:**
- A pre-substitute leaf fast-path on `unifyTypes` for
  `(TypeConstructor c, TypeConstructor c)` and `(TypeVar v, TypeVar v)`
  could catch the 86% of cache hits without a HashSet. This is
  finer-grained than the call-site skips that all regressed prelude
  +7% — those bypassed the wrapper for whole call sites; this one
  only bypasses for tag-equal leaves regardless of call site.
- Soundness of leaf fast-path: trivial — identical constructors
  unify to themselves, no substitution change, no error possible.
- Untested whether this regresses prelude. The prior call-site
  skips all hit ~+7% on prelude — the leaf fast-path may or may not
  share that mechanism. Tracked as proposed `unify-leaf-fast-path`.

**Takeaway:** when a cache shows up high in profile, characterise
its contents by *cost* (here: pair size) before designing a
replacement. A HashSet earning 86% of its keep on 1-2-node pairs
is doing very different work than one earning its keep on
50+-node pairs — and the algorithmic alternatives are different.

## `stack test --fast` re-installs an unoptimised binary in-place (`funapp-pattern-match`)

`stack test --fast` rebuilds the library *with* `--fast` (no
optimisations) and re-installs the resulting `purs` binary on top
of the optimised one. Sequence that bit:

1. `rm -rf .stack-work && stack build` → 49.1 MB optimised binary.
2. `stack test --fast` → tests pass, but binary is now 46.7 MB
   (unoptimised).
3. `exp run … --skip-build` → benchmarks run against the
   unoptimised binary → catastrophic numbers
   (full +138%, prelude +166%, leaf +59%, nochange +40%).

**Diagnostic signal:** binary size dropped 49.1 → 46.7 MB
between the build and the benchmark, with `stack test --fast` in
between. Same shape as the documented Unify.hs / stale-incremental
"binary shrinks → perf collapses" signals.

**Takeaways:**
- Run benchmarks *before* `stack test --fast`, or run a clean
  `stack build` (no `--fast`) after testing and before measuring.
- Don't combine `stack test --fast` with `exp run … --skip-build`.
  Either omit `--skip-build` (the harness will rebuild the
  optimised binary) or `stack build` manually first.
- Binary size remains the cheapest contamination check across
  all variants of this trap. Always check it before trusting
  benchmark results.

## Stack incremental builds can produce slow binaries (`skip-redundant-funapp-unify`)

The first benchmark of skip-redundant-funapp-unify produced
catastrophic apparent regressions:

| Scenario | Δ (incremental build) | Δ (after stack clean) |
|---|---:|---:|
| full | +74.1% | -0.0% |
| prelude | +164.2% | +2.3% |
| leaf | +51.5% | -1.6% |
| nochange | +39.4% | -1.9% |

**Diagnostic signal:** the head binary was 46.6 MB vs baseline
48.6 MB — *2 MB smaller*. Same shape as the documented Unify.hs
inlining-cliff (binary shrinks, perf collapses), but here
triggered by stale incremental compilation under
`.stack-work/dist/`, not by a hot-path source edit. A
`stack clean` + full rebuild produced a 48.6 MB binary matching
baseline at neutral perf.

**Takeaways:**
- **Always `stack clean` before a benchmark when results look
  implausible.** Don't trust incremental builds for perf measurement.
- **Binary size is a primary signal.** A >1 MB delta without a
  proportional source change means GHC compiled differently —
  treat timing as uninterpretable until the size is explained.
- **A single manual `time purs compile $(spago sources)` on the
  worktree binary** is the fastest way to spot contamination,
  before kicking off a 5-run × 4-scenario harness invocation.
  (Same recipe as the Unify.hs lesson, but applies generally.)
