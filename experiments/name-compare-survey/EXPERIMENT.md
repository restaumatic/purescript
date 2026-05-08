---
id: name-compare-survey
status: done (research)
verdict: tbd
branch: name-compare-survey
worktree: /workspace/p/name-compare-survey
baseline_sha: c84101d8
head_sha: c84101d8
hypothesis: >
  Post-PR-#18 cost-centre table shows `compare (ProperName)` 3.9% +
  `compare (Qualified)` 2.3% + `==` (PSString) 1.4% = ~7.6% combined,
  all driven by `Map (Qualified (ProperName _)) v` Environment lookups.
  Before deciding between (a) full HashMap migration and (b) a small-set
  fast-path on the very hottest class names, instrument the four most
  likely-hot lookup sites and count call frequency by key. If a small
  set dominates, fast-path. If broadly distributed, HashMap.
headline_delta: 1.45M lookups across 4 sites — typeClassDictionaries (55%) and typeClasses (26%) dominate, both keyed on (Qualified ClassName). HasField is 15% of all class-Map lookups by itself. 337 distinct class names; top-25 cover 60–66%. Distribution is concentrated enough to motivate a HashMap migration on these two Maps, with a 5-class fast-path as additional sweetener.
tags: [name-compare, environment, lookups, characterization]
started: 2026-05-08
closed: 2026-05-08
---

# name-compare-survey

## Why

`entailment-decl-memo` (just abandoned) ruled out a global solve-result
cache as unsound. The remaining attack surface for the
~7.6% name-compare cluster is the underlying Map lookup machinery
itself. Two shapes of fix were on the table:

1. **HashMap migration** (precedent: `type-hash`, -15.4%) — switch
   `Map (Qualified ClassName) v` to `HashMap`, with INLINEd Hashable.
   Big diff, GHC-codegen-sensitive.
2. **Closed-set fast-path** — special-case the top-N class names
   in a hand-written constant-pattern dispatch before the Map lookup.
   Tiny diff, no codegen risk, but only wins if N is small.

The survey was designed to pick between them with a 1-hour
characterization rather than a 1-day implementation gamble.

## Method

Instrumented the four most likely-hot Environment Map lookup sites
in a separate-module hook (`NameCompareSurvey`), gated by
`PURS_NAME_COMPARE_SURVEY=1`:

- `typeClasses` lookup at `Entailment.solve.go:288` — fires once
  per top-level solve.
- `typeClassDictionaries` lookup inside `findDicts:103` — fires
  during instance candidate search.
- `types` lookup in `Kinds.go.TypeConstructor:169` and
  `.ConstrainedType:179` — type-checking type constructors.
- `dataConstructors` lookup in `Types.infer'.Constructor:504` —
  pattern matching.

Hook records `(mapName, showQualified key)` per call; dumps
per-map histograms on shutdown.

## Findings

See [results.md](results.md) for the full breakdown. Headlines:

- **Total lookups: 1,448,269** across 4 sites in one full pr-admin
  build.
- **typeClassDictionaries (findDicts) 794,706 = 55% of all hooked
  lookups, 337 distinct keys, top-25 cover 58.5%.**
- **typeClasses (Entailment.solve) 375,232 = 26%, 337 distinct
  keys, top-25 cover 66%.**
- The two class-Map sites share the same key type and broadly
  the same hot keys: HasField, IsSymbol, Cons, TestHasLabelRL,
  RowToList, Lacks dominate both. HasField alone is 121k of
  the 794k findDicts hits — 15.2% of all class-Map lookups
  through one key.
- **types (Kinds): 237k lookups, 3642 distinct keys** —
  much more diverse, top-25 only 58%. Fast-path is wrong shape
  here; structural change has to do the work.
- **dataConstructors: 35k lookups** — only 2% of the volume,
  not worth attacking on its own.

## Implications

1. **HashMap migration is well-motivated.** 1.17M lookups
   (typeClassDictionaries + typeClasses) on Maps of cardinality
   ~337 each, all keyed on `Qualified (ProperName 'ClassName)`.
   Each lookup currently does log₂(337) ≈ 8.4 `compareType`-style
   text compares. Replacing with hash+single eq is the same
   shape of move that PR #18 made for the unification cache.
2. **Fast-path alone is insufficient.** Top-25 covers 60–66%,
   leaving 33–40% of lookups still going through the full Map.
   A 25-arm pattern-match dispatch is also clunky to maintain.
3. **The `types` Map is a separate problem.** 3642 distinct
   keys means HashMap migration helps via O(1) vs O(log n), but
   no fast-path applies.
4. **PSString comparison cost (1.4%) is on the cluster's
   periphery** — most class-name keys are short ASCII (HasField,
   etc.), so the per-compare cost is small; the volume is what
   adds up.

## Next experiment

**`env-hashmap`** — migrate `typeClasses`, `typeClassDictionaries`,
and `types` Maps in `Environment` to `HashMap`. Add INLINEd
Hashable instances on `ProperName` (over Text), `Qualified`
(combining `QualifiedBy` + inner), and `PSString` (over the
existing word-list). Per the type-hash trap in LESSONS, every
Hashable method must be marked INLINE or we get a +100%
regression instead of a -15% win.

## Links

- Worktree: /workspace/p/name-compare-survey
- Survey output: [survey-output.txt](survey-output.txt)
- Results: [results.md](results.md)
- Predecessor: [../entailment-redundancy/results.md](../entailment-redundancy/results.md)
- Trap to avoid: LESSONS.md "INLINE on Hashable instance methods"
