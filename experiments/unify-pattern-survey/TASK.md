# Task: unify-pattern-survey

## Goal

Characterize the 412K cache hits the unification cache catches per full
pr-admin compile, by classifying each `(t1, t2)` lookup into structural
buckets at lookup time. Output a histogram so we can decide whether a
flag-based or hash-equality shortcut can replace the HashSet.

## Background

Post-`unify-cache` we know:
- 39.3% hit rate (412K / 1.05M lookups on full pr-admin)
- Cache is +24% net positive (dropping it = 51.8s → 64.3s)
- Hashable infrastructure costs ~19% of total compile time

We don't know what's actually being cached. The survey answers that
without committing to any cache redesign.

## Approach

1. **New module `UnifyPatternSurvey.hs`** containing:
   - `IORef Int` counters per bucket (NOINLINE CAFs)
   - `IORef Bool` env-var gate (NOINLINE CAF, seeded once)
   - `recordPair :: SourceType -> SourceType -> Bool -> IO ()`
     (Bool = was-this-a-cache-hit)
   - `dumpSurvey :: IO ()` — formats histogram, writes to stderr
   - Bucketing function classifying each pair via `TypeFlags` + hash

2. **Hot-path wiring in `Unify.hs`** — at the cache-check site:
   ```haskell
   let !_ = unsafePerformIO (recordPair t1 t2 inCache)
   ```
   One import, one call. Nothing else changes.

3. **Shutdown dump.** Add a single call to `dumpSurvey` from
   `Command.Compile` after the typecheck phase finishes (mirrors
   the pattern unify-cache used). This lives in `Command.Compile`,
   not in `Unify.hs`, so no module-level inlining contamination.

4. **Run.** `PURS_UNIFY_SURVEY=1 purs compile $(spago sources)` once
   on pr-admin. Capture stderr → `results.md`.

5. **Decide.**
   - If `hash_eq` dominates: drop cache, add hash-equal short-circuit
     in `unifyTypes` itself.
   - If `both_concrete_leaf` dominates: add a flag-keyed shortcut
     that doesn't require hashing.
   - If hits spread evenly: cache is irreducible; close as no-win.

## Bucketing taxonomy

For each `(t1, t2)` lookup, classify into the *first* bucket that
matches (mutually exclusive):

```
1. hash_eq          : typeHash t1 == typeHash t2
2. both_tunknown    : both are TUnknown (regardless of unknown id)
3. has_wildcard     : either has containsWildcards flag set
4. has_synonym      : either has containsTypeSynonyms flag set
5. has_unknown      : either has containsUnknowns flag set
6. both_concrete_leaf : both are leaves (TypeConstructor/TypeLevelString/etc.)
7. both_concrete_complex : both have no unknown/wildcard/synonym flags
8. other            : everything else
```

Track `(hits, misses)` per bucket.

## Key files

- `src/Language/PureScript/TypeChecker/UnifyPatternSurvey.hs` (new)
- `src/Language/PureScript/TypeChecker/Unify.hs` (1 import + 1 call)
- `src/Language/PureScript/Make.hs` or `Command/Compile.hs` (1 dump call)
- `purescript.cabal` (register new module)

## How to measure

Counts only, no timing. One run on pr-admin with the env var set.
Dump output to `results.md`. We do **not** run the 4-scenario harness
during the survey phase — the survey changes Unify.hs (1 line) and
that may flip inlining. If we later ship a structural shortcut, that
goes on a separate branch *without* the survey module imported, and
we re-measure properly there.

## Risks / things to watch

- **Unify.hs inlining sensitivity** (see LESSONS.md). Keep the Unify.hs
  diff to import + 1 call. All state lives in the survey module.
- **Bucket order matters.** First-match-wins, so the taxonomy is
  ordered by specificity (`hash_eq` first because it subsumes most
  others; then unknowns, etc.).
- **Sample pairs per bucket.** Useful for spot-checking; capture
  e.g. first 5 pairs of each bucket as `Show`-strings to a separate
  file. Cheap; bounded memory.

## Tests

`stack test --fast` must still pass. The survey is gated on env var,
so tests run with default behaviour (no recording).
