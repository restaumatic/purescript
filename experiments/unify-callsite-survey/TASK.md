# Task: unify-callsite-survey

## Goal

Build a counts-only histogram of external `unifyTypes` call sites
on a pr-admin compile, broken down by total calls and hash-equal
calls per site. Identify whether hash-equal traffic concentrates at
a few sites (→ upstream redundancy fix) or spreads thinly (→ cache
structure rework).

## Background

`unify-pattern-survey` proved 99.9% of unification-cache hits are
hash-equal pairs. The cache itself ships ~19% of compile time as
Hashable infrastructure (lifted-tuple hashing + HashSet membership).
We can't drop the cache without first eliminating the upstream calls
that would otherwise hit it.

`skip-redundant-entailment-unify` is the precedent: it identified one
redundant-by-construction `unifyTypes` call in entailment fundep
enforcement, removed it, shipped -15.5% on full builds. Same pattern
likely applies elsewhere — but we don't know where.

## Approach

Counts only, no timing.

1. **Survey module** at
   `src/Language/PureScript/TypeChecker/UnifyCallSiteSurvey.hs`:
   - `IORef (Map String (Int, Int))` mapping tag → (hash_eq, total).
   - Env-gated (`PURS_UNIFY_CALLSITE_SURVEY=1`) via top-level CAF.
   - `recordCallSite :: String -> SourceType -> SourceType -> ()`
     under `unsafePerformIO` + `NOINLINE`; bumps the (hash_eq, total)
     pair atomically.
   - `dumpSurvey :: IO ()` writes the histogram (sorted by hash_eq
     descending) to stderr.

2. **Tag every external `unifyTypes` call site.** Each site gets a
   stable string of the form `"<File>:<func>"` or `"<File>:<line>"`.
   Insertion is `let !_ = recordCallSite "..." t1 t2`.

3. **Wire dump** at the very end of `app/Command/Compile.hs` so it
   prints once per process.

4. **Run on pr-admin** clean build, capture histogram to
   `results.md`.

## Key files

| File | Action |
| --- | --- |
| `src/Language/PureScript/TypeChecker/UnifyCallSiteSurvey.hs` | new — counters + dump |
| `src/Language/PureScript/TypeChecker/Subsumption.hs` | tag 2 sites |
| `src/Language/PureScript/TypeChecker/Entailment.hs` | tag ~3 sites |
| `src/Language/PureScript/TypeChecker/Types.hs` | tag ~20 sites |
| `src/Language/PureScript/TypeChecker/Kinds.hs` | tag any sites |
| `src/Language/PureScript/TypeChecker/Roles.hs` | tag any sites |
| `app/Command/Compile.hs` | dumpSurvey on shutdown |
| `purescript.cabal` | register new module |

## How to measure

This experiment is counts-only. Skip `exp run` (timing harness);
just build and run a single pr-admin compile manually:

```bash
cd /workspace/p/unify-callsite-survey
stack build
# Use an isolated output dir to avoid stomping the regular build
PURS_UNIFY_CALLSITE_SURVEY=1 \
  stack exec -- purs compile \
    --output /tmp/unify-callsite-output \
    "$(cat /workspace/restaumatic/apps/pr-admin/.spago-sources)" \
    2> /tmp/unify-callsite-survey.stderr
grep -A 200 "unify-callsite-survey" /tmp/unify-callsite-survey.stderr
```

(See `experiments/unify-pattern-survey/` for the prior version of
this command — same shape.)

## Tests

```bash
stack test --fast    # 1340 examples; survey is read-only, tests must pass
```

## Risks / things to watch

- **Coverage gap.** `grep -rn "unifyTypes\b"` against `src/` to make
  sure every external call site is tagged. Sites in test fixtures
  are out of scope.
- **Recursion contamination.** Do **not** tag inside `Unify.hs`
  itself; recursive entries would dominate counts and obscure which
  external caller is responsible.
- **Bang-pattern force.** Confirm `let !_ = recordCallSite ...`
  produces non-zero counts; if zero, the unsafePerformIO wasn't
  forced.
- **Inlining sensitivity in `Unify.hs`.** Don't touch Unify.hs at
  all if avoidable. The instrumentation lives outside.
