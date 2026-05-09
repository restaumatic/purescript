# Results for name-compare-lineage

Survey complete. Verdict: **research — found a concrete next experiment.**

## Headline

**~80% of the residual `compare Qualified` cluster — 4.1% of full
builds — lives in `replaceAllTypeSynonyms'.go`'s `M.lookup ctor syns`**,
where `syns :: SynonymMap = Map (Qualified (ProperName 'TypeName)) ...`.

The other 1.4% (`==` PSString) is row-label comparisons inside
`alignRowsWith` / `rowToSortedList` during row unification.

The prior `name-compare-survey` measured Environment Map lookup
volume (1.45M lookups across `typeClassDictionaries`/`typeClasses`/
`types`/`dataConstructors`); env-hashmap migrated all of those and
got no win. The reason it didn't pay is **those Maps weren't where
the cycles lived in the first place** — the cycles live in the
SynonymMap, which env-hashmap didn't touch (it's the Environment
field `typeSynonyms`, plus also threaded through
`replaceAllTypeSynonyms'`'s arguments).

## Method

Phase A: static call-site enumeration via codebase-analyzer fork.
~50 candidate sites identified across 5 buckets. Output:
[static-enumeration.md](static-enumeration.md).

Phase B: cost-attribution from existing prof data. The
`traversal-inline/profiles/head-full-20260509-080817.prof` profile
is at b831b298 — the same head as this experiment's baseline — so
re-running profiling wasn't needed. Built a Python analyzer
([analyze-prof.py](analyze-prof.py)) that walks the call tree and
aggregates inherited %time of compare/eq SCCs by the deepest
non-pattern-synonym, non-compare ancestor — distinguishing the
"real caller" from the AST-pattern-synonym matcher SCCs that GHC
materializes around every `case t of TypeConstructor ...`.

Phase B alternative attempted: `-fprof-late` build for post-spec
SCC binning. The resulting binary segfaulted on pr-admin
(exit 139, signal 11). Reverted; the existing prof's call-tree
gave enough data without it.

Phase C (targeted counters) skipped — Phase B unambiguous.

## Phase B findings — `compare Qualified` cluster

Aggregating the existing prof at three ancestor levels:

### Level 1 — direct non-matcher caller

| %inh | caller |
|---:|---|
| **4.10%** | `replaceAllTypeSynonyms'.go` (Synonyms.hs:90-102) |
| 0.70% | `applyExternsFileToEnvironment.applyDecl` (Externs.hs:179) |
| 0.20% | `typeCheckAll.go.\` (TypeChecker.hs:405) |
| 0.10% | `applyExternsFileToEnvironment.applyDecl.updateMap` (Externs.hs:194) |

Total: 5.10% inherited across the call tree (vs 3.6% individual in
the summary table — inherited is summed across all ancestor
contexts).

### Level 2 — one further up

| %inh | caller |
|---:|---|
| **4.10%** | `replaceAllTypeSynonyms'.trySyn` (Synonyms.hs:87) |
| 0.70% | `applyExternsFileToEnvironment` (Externs.hs:176-202) |
| 0.20% | `$mValueDecl.\` (AST/Declarations.hs:367) |

### Level 3 — module/file caller

| %inh | caller |
|---:|---|
| **4.10%** | `replaceAllTypeSynonyms'.walk` (Synonyms.hs:76-82) |
| 0.70% | `rebuildModuleWithIndex.env` (Make.hs:111) |
| 0.20% | `typeCheckAll.go` (TypeChecker.hs:259-424) |

The chain `walk → trySyn → go → M.lookup ctor syns → compare
Qualified` is the load-bearing path — same 4.10% accountable to
each level, confirming this is one tight call sequence not a
diffuse pattern.

## Phase B findings — companion compares

`compare QualifiedBy` 1.9% and `compare ModuleName` 1.2% are
recursive-descent children of `compare Qualified` (a Qualified is a
QualifiedBy + an inner name; QualifiedBy contains a ModuleName).
Their attribution mirrors Qualified's: 1.50% / 1.10% via the same
synonym walker. So these aren't *separate* hotspots — they're the
inner steps of the same compare chain. The headline number for
the synonym walker's name-compare attribution is therefore the
union of those rows, dominated by the 4.10%.

## Phase B findings — `==` PSString 1.4%

| %inh | caller | what |
|---:|---|---|
| 1.20% | `==` Label.hs:18 → `entails.solve.go.\` Entailment.hs:335-338 | `unless (eqType inferredType t2)` row-label compares inside `alignRowsWith`/`rowToSortedList` during entailment unification |
| 0.20% | `==` CoreImp/AST.hs:115 → `untilFixedPoint.go` CoreImp/Optimizer.hs | codegen optimizer (different domain) |

At level 3:
- 1.10% inherited via `withErrorMessageHint` (Monad.hs:188-194) —
  every typecheck error wrap. That's the wrapper around `unifyTypes`,
  so PSString `==` lives inside row unification's merge-join.

The PSString cost is structural to row unification — pr-admin is
record-heavy, every record unification sorts row labels. Hard to
optimise without changing row representation.

## Why the synonym walker is hot

`replaceAllTypeSynonyms'` (Synonyms.hs:41-151) does:

```haskell
walk :: SourceType -> Either MultipleErrors SourceType
walk t | hasFlag tfSynonymsFree (typeFlags t) = Right t
walk t@(TypeApp_ _ _ _ _) = trySyn t >>= walkChildren
walk t@(KindApp_ _ _ _ _) = trySyn t >>= walkChildren
walk t@(TypeConstructor_ _ _ _) = trySyn t >>= ...

trySyn t = fromMaybe t <$> go ...

go _ _ _ _ (TypeConstructor _ ctor)
  | Just (synArgs, body) <- M.lookup ctor syns
  , c == length synArgs
  , ...
```

Every `TypeConstructor`, `TypeApp`, and `KindApp` node in every
type tree that hasn't been pre-marked `tfSynonymsFree` triggers
`trySyn t` → `go` → `M.lookup ctor syns`. **Most of these
lookups miss** (synonyms are rare; most type constructors are
primitive types, type-class dictionaries, etc.). Each miss does
log₂(N) `compare Qualified` operations to determine the miss.

`tfSynonymsFree` already short-circuits trees that have been
fully expanded — but a 4.1% residual proves there's still
substantial walking on un-flagged trees.

The `M.null syns` check at line 47 short-circuits when there are
*no* synonyms in scope — but pr-admin has plenty of synonyms in
scope so this rarely fires.

## Recommended next experiment

### `synonym-fast-path`

Add a `HashSet (ProperName 'TypeName)` of **disqualified inner
names that ARE synonyms** alongside the existing `SynonymMap`.
Use it as a cheap miss-prefix check before `M.lookup ctor syns`:

```haskell
go ss c kargs args (TypeConstructor _ ctor)
  | not (HS.member (disqualify ctor) synProperNames) = return Nothing
  | Just (synArgs, body) <- M.lookup ctor syns ...
```

**Mechanism:**

- Most `TypeConstructor` lookups miss (synonyms are rare).
- A `HashSet (ProperName 'TypeName)` membership check on the
  disqualified inner name is O(1) hash + ~1 Text equality.
- Misses skip the full `M.lookup` and its log₂(N) `Qualified`
  compares.
- `HashSet` of `ProperName` is small (~50–500 entries on pr-admin),
  trivially constructed once when the SynonymMap is built.
- Same key type (`ProperName 'TypeName`, just disqualified) so no
  weird new Hashable instances.

**Hypothesis:** -1% to -3% on full builds. The 4.10% inherited
isn't all reducible — some lookups are hits, and the construction
of the HashSet has setup cost. But shaving 50-70% of the misses
should surface.

**Why this isn't `env-hashmap` redux:** env-hashmap migrated *the
storage* (Map → HashMap) on already-balanced workloads. This
experiment **adds a cheap miss-filter before** the existing Map,
keeping the storage. It's a Bloom-filter pattern, not a structural
migration. The per-call cost reduction is real (skip log₂(N)
compares for misses), and the construction cost is paid once per
typecheck, amortised across thousands of lookups.

**Diff size:** ~30 lines in `Synonyms.hs` (build the HashSet, thread
it through, gate the lookup). Plus possibly threading through
`KindMap` companion check at `lookupKindArgs`.

**Risk traps:**

- *env-hashmap risk:* the bloom-filter overhead might exceed the
  savings if hit rate is actually high. Need to measure hit/miss
  ratio before committing — if >50% of M.lookup calls hit, the
  HashSet check is wasted on most calls.
- *GHC inlining sensitivity:* per LESSONS.md, Hashable instances
  on hot paths need INLINE pragmas. ProperName already has Eq/Ord
  derived; need to add a hand-written INLINEd Hashable.
- *`tfSynonymsFree` interaction:* the flag already short-circuits
  walks; the fast-path only matters for un-flagged trees. Need to
  confirm that's where the 4.10% lives (high confidence — the call
  tree shows it, and `M.null syns` short-circuit isn't firing).

**Alt: `tfHasSynonyms` flag.** Add the *positive* of
`tfSynonymsFree` — pre-compute at construction whether any
descendant constructor is a synonym, short-circuit the whole walk
at top-level if false. More invasive (touches every Type
constructor's smart-constructor flag-combine logic) but eliminates
the walk entirely on whole subtrees.

Recommend the HashSet variant first — smaller diff, quicker to
falsify or ship.

## What this rules in / rules out

**Ruled in:**

- The "name-compare cluster" residual is concentrated in the
  synonym walker. Anything that reduces `M.lookup ctor syns` calls
  is on the critical path.
- The PSString 1.4% is row-label compares — separate target,
  different domain (would need row-representation change).

**Ruled out:**

- More general migrations of remaining Environment Maps (those
  weren't where the cycles lived).
- Adding hash-on-Qualified (analogue to type-hash on Type) — the
  cost isn't in compares being slow per se, it's in *too many
  compares being fired* by the synonym walker. Reduce volume,
  not per-compare cost.
- Touching the `eqType`/`compareType` hand-written instances at
  Types.hs:958-1006 — they're already optimal; the calls into them
  are the issue.

## Branches / artefacts

- Branch: `name-compare-lineage` (no source changes; cabal toggle
  reverted after `-fprof-late` segfault).
- Worktree: /workspace/p/name-compare-lineage
- Static enumeration: [static-enumeration.md](static-enumeration.md)
- Analyzer: [analyze-prof.py](analyze-prof.py)
- Profile data: re-used from
  `/workspace/purescript/experiments/traversal-inline/profiles/head-full-20260509-080817.prof`

## Raw runs

No runs (research-only). The recommended follow-up experiment
`synonym-fast-path` would do its own measurement.
